-module(gui).

-author('fabian@fmbb.se').

-export([run/0, start/0]).

-define(WIDTH, 480).
-define(HEIGHT, 640).

run() ->
    init().

start() ->
    spawn(fun init/0).

init() ->
    %% setup game
    Game = game:new(?WIDTH, ?HEIGHT),

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
           game => Game}).

loop(State) ->
    events_loop(),
    render(State),
    loop(State).

events_loop() ->
    case sdl_events:poll() of
        false ->
            ok;
        #{type := quit} ->
            terminate();
        _ ->
            events_loop()
    end.

render(#{renderer := Renderer,
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
    LevelInfo = io_lib:format("LEVEL: ~B", [game:level(Game)]),
    ScoreInfo = io_lib:format("SCORE: ~B", [game:score(Game)]),
    NextPiece =
        piece:translate(
            game:next(Game), {6, -2}),
    NextShape = piece:shape(NextPiece),
    NextBlocks = piece:blocks(NextPiece),
    Preview = [{Coord, NextShape} || Coord <- NextBlocks],
    [blit_block(Block, Renderer, Sprites) || Block <- Preview],

    TextX = 352,
    TextY = 192,
    ok = blit_string(LevelInfo, Font, TextX, TextY, Renderer),
    ok = blit_string(ScoreInfo, Font, TextX, TextY + lines(1), Renderer),
    case game:live(Game) of
        no ->
            ok = blit_string("GAME OVER MAN", Font, TextX, TextY + lines(2), Renderer);
        _ ->
            ok
    end,

    ok = blit_string("TERLTRIS", Font, 100, 400, Renderer),
    ok = sdl_renderer:present(Renderer).

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
      y => -Y * ?BLOCK_SIZE}.

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
    sdl_renderer:copy(Renderer, Sprite, undefined, Dst).

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
    sdl_renderer:copy(Renderer, Font, Src, Dst).

blit_letter(C, Font, X, Y, Renderer) ->
    PaddedSize = ?FONT_SIZE + ?LETTER_SPACING,
    Src = char_rect_at(?X_A + PaddedSize * (C - $A), 1),
    Dst = move(Src, X, Y),
    sdl_renderer:copy(Renderer, Font, Src, Dst).

%%
%% rect helpers
%%

move(R, X, Y) ->
    R#{x => X, y => Y}.

%translate(R =   #{x := X, y := Y}, Dx, Dy) ->
%    R#{x => X + Dx, y => Y + Dy}.
