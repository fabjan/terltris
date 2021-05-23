-module(gui).

-author('fabian@fmbb.se').

-include_lib("deps/esdl2/include/sdl_keycode.hrl").
-include_lib("deps/esdl2/include/sdl_scancode.hrl").

-export([run/2, start/0]).

-define(WIDTH, 480).
-define(HEIGHT, 640).

start() ->
    spawn(fun() -> run(10, 20) end).

run(BoardWidth, BoardHeight) ->
    %% setup game
    Game = game:new(BoardWidth, BoardHeight),

    %% setup SDL
    ok = sdl:start([video]),
    ok = sdl:stop_on_exit(),
    {ok, Window} = sdl_window:create(<<"terltris">>, 10, 10, ?WIDTH, ?HEIGHT, []),
    {ok, Renderer} = sdl_renderer:create(Window, -1, [accelerated, present_vsync]),

    %% load assets
    %% TODO replace with a PNG font, with transparency
    {ok, Font} = sdl_texture:create_from_file(Renderer, "fonts/mario3.bmp"),
    Sprites = load_shape_textures(Renderer, [g, i, j, l, o, s, t, z]),

    %% loop de loop
    loop(#{window => Window,
           renderer => Renderer,
           font => Font,
           sprites => Sprites,
           ticker => 0,
           game => Game}).

-define(LOOP_TICK, 16).
-define(MAX_TICKS, 20).
-define(LEVEL_SPEEDUP, 0.1).

loop(State) ->
    case update(State) of
        quit ->
            terminate();
        NextState ->
            render(NextState),
            timer:sleep(?LOOP_TICK), %% TODO game loop?
            loop(NextState)
    end.

update(State) ->
    NextState = events_loop(State),
    update_game(NextState).

update_game(State = #{ticker := 0, game := Game}) ->
    State#{ticker => tick_time(game:level(Game)), game => game:tick(Game)};
update_game(State = #{ticker := Tick}) ->
    State#{ticker => Tick - 1}.

%% this could be calculated once per level
tick_time(Level) ->
    Speedup = math:pow(1 - ?LEVEL_SPEEDUP, ?MAX_TICKS - Level),
    Time = round(?MAX_TICKS - ?MAX_TICKS * Speedup),
    max(Time, 0).

events_loop(State = #{window := Window}) ->
    case sdl_events:poll() of
        false ->
            State;
        #{type := quit} ->
            quit;
        #{type := key_down,
          state := pressed,
          sym := Sym} ->
            handle_keypressed(State, Sym);
        #{type := resize,
          w := W,
          h := H} ->
            sdl_window:set_size(Window, W, H),
            State;
        _ ->
            events_loop(State)
    end.

-define(KEY_LEFT, ?SDLK_LEFT).
-define(KEY_RIGHT, ?SDLK_RIGHT).
-define(KEY_ROTATE, ?SDLK_UP).
-define(KEY_CONTINUE, ?SDLK_DOWN).
-define(KEY_DROP, ?SDLK_SPACE).
-define(KEY_QUIT, ?SDLK_ESCAPE).

handle_keypressed(State = #{game := Game}, ?KEY_RIGHT) ->
    State#{game => game:move_right(Game)};
handle_keypressed(State = #{game := Game}, ?KEY_LEFT) ->
    State#{game => game:move_left(Game)};
handle_keypressed(State = #{game := Game}, ?KEY_ROTATE) ->
    State#{game => game:rotate(Game)};
handle_keypressed(State, ?KEY_CONTINUE) ->
    State#{ticker => 0};
handle_keypressed(State = #{game := Game}, ?KEY_DROP) ->
    State#{game => game:drop(Game)};
handle_keypressed(_, ?KEY_QUIT) ->
    quit.

%%
%% render helpers
%%

-define(TEXT_X, 352).
-define(TEXT_Y, 192).

ui_string(LineNo, S, Renderer, Font) ->
    blit_string(S, Font, ?TEXT_X, ?TEXT_Y + lines(LineNo), Renderer).

render(State =
           #{renderer := Renderer,
             font := Font,
             sprites := Sprites,
             game := Game}) ->
    ok = sdl_renderer:clear(Renderer),

    %% render the board
    Dst = #{x => 0,
            y => 0,
            w => 320,
            h => 640},
    ok = sdl_renderer:set_draw_color(Renderer, 10, 10, 10, 255),
    ok = sdl_renderer:fill_rect(Renderer, Dst),

    %% render game pieces
    [blit_block(Block, Renderer, Sprites) || Block <- game:blocks(Game)],

    %% render info aside
    %% TODO figure out what the hell is doing this formatting (erlang-ls via rebar?)
    NextPiece =
        piece:translate(
            game:next(Game), {6, -2}),
    NextShape = piece:shape(NextPiece),
    NextBlocks = piece:blocks(NextPiece),
    Preview = [{Coord, NextShape} || Coord <- NextBlocks],
    [blit_block(Block, Renderer, Sprites) || Block <- Preview],

    LevelInfo =
        case game:live(Game) of
            yes ->
                io_lib:format("LEVEL: ~B", [game:level(Game)]);
            _ ->
                "GAME OVER, MAN"
        end,
    ScoreInfo = io_lib:format("SCORE: ~B", [game:score(Game)]),
    ui_string(0, LevelInfo, Renderer, Font),
    ui_string(1, ScoreInfo, Renderer, Font),

    debug_render(State),
    ok = sdl_renderer:present(Renderer).

-ifdef(debugging).

debug_render(#{renderer := Renderer,
               font := Font,
               ticker := Ticker}) ->
    ui_string(-8, io_lib:format("TICKER ~B", [Ticker]), Renderer, Font),
    ui_string(-7, "DEBUGGING", Renderer, Font).

-else.

debug_render(_) ->
    %% maybe render fps always?
    ok.

-endif.

terminate() ->
    init:stop(),
    exit(normal).

%%
%% block helpers
%%
-define(BLOCK_SIZE, 32).

block_rect_at(X, Y) ->
    #{w => ?BLOCK_SIZE,
      h => ?BLOCK_SIZE,
      x => X * ?BLOCK_SIZE,
      y => Y * ?BLOCK_SIZE}.

load_shape_textures(Renderer, Shapes) ->
    [{Shape, load_block_texture(Renderer, Shape)} || Shape <- Shapes].

load_block_texture(Renderer, Shape) ->
    Path = "src/sprites/" ++ atom_to_list(Shape) ++ ".bmp",
    {ok, Texture} = sdl_texture:create_from_file(Renderer, Path),
    Texture.

blit_block({{X, Y}, Shape}, Renderer, Sprites) ->
    %% TODO maps are a thing in Erlang now!
    {value, {_, Sprite}} = lists:keysearch(Shape, 1, Sprites),
    Dst = block_rect_at(X, -Y),
    %%io:format("blit block @ ~p~n", [Dst]),
    ok = sdl_renderer:copy(Renderer, Sprite, undefined, Dst).

%%
%% bitmap font helpers
%%
-define(FONT_SIZE, 8).
-define(LETTER_SPACING, 1).
-define(X_ZERO, 66).
-define(X_A, 191).

lines(N) ->
    N * (?FONT_SIZE + ?LETTER_SPACING).

blit_string(S, Font, X, Y, Renderer) ->
    blit_string(S, Font, X, Y, Renderer, 0).

blit_string([], _, _, _, _, _) ->
    ok;
blit_string([C | S], Font, X, Y, Renderer, I) ->
    PaddedSize = ?FONT_SIZE + ?LETTER_SPACING,
    CharX = X + PaddedSize * I,
    if $0 =< C andalso C =< $9 ->
           blit_digit(C, Font, CharX, Y, Renderer);
       $A =< C andalso C =< $Z ->
           blit_letter(C, Font, CharX, Y, Renderer);
       true ->
           ok
    end,
    blit_string(S, Font, X, Y, Renderer, I + 1).

char_rect_at(X, Y) ->
    #{w => ?FONT_SIZE,
      h => ?FONT_SIZE,
      x => X,
      y => Y}.

blit_digit(C, Font, X, Y, Renderer) ->
    PaddedSize = ?FONT_SIZE + ?LETTER_SPACING,
    Src = char_rect_at(?X_ZERO + PaddedSize * (C - $0), 1),
    Dst = move(Src, X, Y),
    ok = sdl_renderer:copy(Renderer, Font, Src, Dst).

blit_letter(C, Font, X, Y, Renderer) ->
    PaddedSize = ?FONT_SIZE + ?LETTER_SPACING,
    Src = char_rect_at(?X_A + PaddedSize * (C - $A), 1),
    Dst = move(Src, X, Y),
    ok = sdl_renderer:copy(Renderer, Font, Src, Dst).

%%
%% rect helpers
%%

move(R, X, Y) ->
    R#{x => X, y => Y}.

%translate(R =   #{x := X, y := Y}, Dx, Dy) ->
%    R#{x => X + Dx, y => Y + Dy}.
