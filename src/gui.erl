-module(gui).
-author('sempetmer@gmail.com').

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("gl.hrl").

-compile(export_all).

-record(state, {window, game, sprites, delay0 = 10, delay = 0}).

-define(WIDTH, 400).
-define(HEIGHT, 800).
-define(BPP, 24).

init(Width, Height) ->
    case init_video() of
        error ->
            sdl:quit(),
            error;
        Window ->
            sdl_video:wm_setCaption("terltris", 0),
            {A, B, C} = now(), random:seed(A, B, C),
            Game = game:new(Width, Height),
            Sprites = load_sprites([g,i,j,l,o,s,t,z]),
            loop(#state{window = Window, game = Game, sprites = Sprites}),
            sdl:quit()
    end.

init_video() ->
    sdl:init(?SDL_INIT_VIDEO),
    resize_window(?WIDTH, ?HEIGHT).

resize_window(Width, Height) ->
    case sdl_video:setVideoMode(Width, Height, ?BPP,
                                ?SDL_HWSURFACE bor
                                ?SDL_RESIZABLE bor
                                ?SDL_DOUBLEBUF) of
        error ->
            io:format("Can't set video mode~n", []),
            error;
        Surface ->
            sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),
            Surface
    end.

loop(State) ->
    render(State),
    NewState = handle_event(State),
    case NewState of
        quit ->
            ok;
        #state{delay0 = Delay, delay = 0, game = NewGame} ->
            loop(NewState#state{delay = Delay, game = game:tick(NewGame)});
        #state{delay = Delay} ->
            loop(NewState#state{delay = Delay - 1})
    end.

render(#state{window = Window, game = Game, sprites = Sprites}) ->
    sdl_video:fillRect(Window, null, sdl_video:mapRGB(Window, 0, 0, 0)),
    lists:foreach(fun (Block) -> draw_block(Block, Window, Sprites) end,
                  game:blocks(Game)),
    sdl_video:flip(Window).

origin(Width, Height) ->
    Ratio = Width/Height,
    Scale = 40,
    {-Width/Scale, Height/(Scale/Ratio), -32}.

draw_block(_Block = {{X, Y}, Shape}, Window, Sprites) ->
    {value, {_Shape, Sprite}} = lists:keysearch(Shape, 1, Sprites),
    Source = #sdl_rect{x = 0, y = 0, w = 32, h = 32},
    #sdl_surface{w = WWidth, h = WHeight} = sdl_video:getSurface(Window),
    %%Width = WWidth/10, Height = WHeight/20,
    Width = Height = 32,
    Dest = #sdl_rect{x = round(X*Width), y = round(-Y*Width),
                     w = round(Width), h = round(Height)},
    sdl_video:blitSurface(Sprite, Source, Window, Dest).

load_sprites(L) ->
    [{Shape, sdl_video:loadBMP("src/sprites/" ++ atom_to_list(Shape) ++ ".bmp")}
     || Shape <- L].

handle_event(State = #state{game = Game}) ->
    case sdl_events:pollEvent() of 
        #quit{} -> 
            quit;
        #resize{w = W, h = H} ->
            resize_window(W, H),
            State;
        #keyboard{sym = ?SDLK_ESCAPE} ->
            quit;
        #keyboard{state = ?SDL_PRESSED, sym = Sym} ->
            case Sym of
                ?SDLK_RIGHT ->
                    State#state{game = game:move_right(Game)};
                ?SDLK_LEFT ->
                    State#state{game = game:move_left(Game)};
                ?SDLK_UP ->
                    State#state{game = game:rotate(Game)};
                ?SDLK_DOWN ->
                    State#state{delay = 0};
                _ ->
                    State
            end;
        _ ->
            timer:sleep(10),
            State
    end.
