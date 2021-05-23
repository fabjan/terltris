-module(gui).

-author('fabian@fmbb.se').

-include_lib("deps/esdl2/include/sdl_keycode.hrl").
-include_lib("deps/esdl2/include/sdl_scancode.hrl").

-export([run/2, start/0]).

%% Render settings
-define(WIDTH, 480).
-define(HEIGHT, 640).
-define(BLOCK_SIZE, 32).
%% Game settings
-define(UI_TICK_MS, 16).
-define(MAX_UI_TICKS_PER_GAME_TICK, 60).
-define(LEVEL_SPEEDUP, 0.1).
%% Controls
-define(KEY_LEFT, ?SDLK_LEFT).
-define(KEY_RIGHT, ?SDLK_RIGHT).
-define(KEY_ROTATE, ?SDLK_UP).
-define(KEY_SOFT_DROP, ?SDLK_DOWN).
-define(KEY_HARD_DROP, ?SDLK_SPACE).
-define(KEY_QUIT, ?SDLK_ESCAPE).
%% UI settings
-define(TEXT_X, 352).
-define(TEXT_Y, 192).
%% Font settings
-define(FONT_SIZE, 8).
-define(LETTER_SPACING, 1).
-define(X_ZERO, 66).
-define(X_A, 191).

start() ->
    spawn(fun() -> run(10, 20) end).

terminate() ->
    init:stop(),
    exit(normal).

run(BoardWidth, BoardHeight) ->
    %% setup game
    Game = game:new(BoardWidth, BoardHeight),

    %% setup SDL
    ok = sdl:start([video]),
    ok = sdl:stop_on_exit(),
    {ok, Window} = sdl_window:create(<<"terltris">>, 10, 10, ?WIDTH, ?HEIGHT, [resizable]),
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

loop(State) ->
    case update(State) of
        quit ->
            terminate();
        NextState ->
            render(NextState),
            timer:sleep(?UI_TICK_MS), %% TODO game loop?
            loop(NextState)
    end.

update(State) ->
    NextState = drain_events(State),
    update_game(NextState).

update_game(State = #{ticker := 0, game := Game}) ->
    State#{ticker => tick_time(game:level(Game)), game => game:tick(Game)};
update_game(State = #{ticker := Tick}) ->
    State#{ticker => Tick - 1}.

%% this could be calculated once per level
%% TODO implement proper level ups,
%%      move ticking into game module
tick_time(Level) ->
    MaxTicks = ?MAX_UI_TICKS_PER_GAME_TICK,
    Speedup = math:pow(1 - ?LEVEL_SPEEDUP, MaxTicks - Level),
    Time = round(MaxTicks - MaxTicks * Speedup),
    max(Time, 0).

%% TODO lift cases out to function clauses
drain_events(State = #{window := Window}) ->
    case sdl_events:poll() of
        false ->
            State;
        #{type := quit} ->
            quit;
        #{type := key_down,
          state := pressed,
          sym := Sym} ->
            NewState = handle_keypressed(State, Sym),
            drain_events(NewState);
        #{type := resize,
          w := W,
          h := H} ->
            sdl_window:set_size(Window, W, H),
            drain_events(State);
        _ ->
            drain_events(State)
    end.

handle_keypressed(State = #{game := Game}, ?KEY_RIGHT) ->
    State#{game => game:move_right(Game)};
handle_keypressed(State = #{game := Game}, ?KEY_LEFT) ->
    State#{game => game:move_left(Game)};
handle_keypressed(State = #{game := Game}, ?KEY_ROTATE) ->
    State#{game => game:rotate(Game)};
handle_keypressed(State = #{game := Game}, ?KEY_SOFT_DROP) ->
    State#{ticker => 0, game => game:soft_drop(Game)};
handle_keypressed(State = #{game := Game}, ?KEY_HARD_DROP) ->
    State#{game => game:hard_drop(Game)};
handle_keypressed(_, ?KEY_QUIT) ->
    quit.

%%
%% render helpers
%%

render(State =
           #{renderer := Renderer,
             window := Window,
             font := Font,
             sprites := Sprites,
             game := Game}) ->
    {W, H} = sdl_window:get_size(Window),
    Backdrop =
        #{x => 0,
          y => 0,
          w => W,
          h => H},
    ok = sdl_renderer:set_draw_color(Renderer, 50, 50, 50, 255),
    ok = sdl_renderer:fill_rect(Renderer, Backdrop),

    %% render the board
    {Wg, Hg} = game:get_size(Game),
    Well =
        #{x => 0,
          y => 0,
          w => Wg * ?BLOCK_SIZE,
          h => Hg * ?BLOCK_SIZE},
    ok = sdl_renderer:set_draw_color(Renderer, 10, 10, 10, 255),
    ok = sdl_renderer:fill_rect(Renderer, Well),

    %% render game pieces
    [blit_block(Block, Renderer, Sprites) || Block <- game:blocks(Game)],

    %% render info aside
    %% TODO figure out what the hell is doing this formatting (erlang-ls via rebar?)
    NextPiece =
        piece:translate(
            game:next(Game), {Wg div 2 + 2, -2}),
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
    ui_string(11, io_lib:format("TICKER ~B", [Ticker]), Renderer, Font),
    ui_string(10, "DEBUGGING", Renderer, Font).

-else.

debug_render(_) ->
    %% maybe render fps always?
    ok.

-endif.

ui_string(LineNo, S, Renderer, Font) ->
    blit_string(S, Font, ?TEXT_X, ?TEXT_Y + lines(LineNo), Renderer).

%%
%% block helpers
%%

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
           blit_char(digit_src(C), Font, CharX, Y, Renderer);
       $A =< C andalso C =< $Z ->
           blit_char(letter_src(C), Font, CharX, Y, Renderer);
       true ->
           ok
    end,
    blit_string(S, Font, X, Y, Renderer, I + 1).

char_src(Offset, I) ->
    #{w => ?FONT_SIZE,
      h => ?FONT_SIZE,
      x => Offset + I * (?FONT_SIZE + ?LETTER_SPACING),
      y => 1}.

digit_src(C) ->
    char_src(?X_ZERO, C - $0).

letter_src(C) ->
    char_src(?X_A, C - $A).

blit_char(Src, Font, X, Y, Renderer) ->
    Dst = Src#{x => X, y => Y},
    ok = sdl_renderer:copy(Renderer, Font, Src, Dst).
