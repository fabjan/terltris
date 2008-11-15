-module(gui).
-author('sempetmer@gmail.com').

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("gl.hrl").

-compile(export_all).

-record(state, {width, height, game, delay0 = 10, delay = 0}).

init() ->
    Width  = 400,
    Height = 800,
    Vst = init_video(Width, Height),
    if
        Vst == error ->
            sdl:quit(),
            error;
        true ->
            sdl_video:wm_setCaption("terltris", 0),
            init_gl(),
            {A, B, C} = now(), random:seed(A, B, C),
            Game = game:new(10, 20),
            loop(#state{width = Width, height = Height, game = Game}),
            sdl:quit()
    end.

init_video(W, H) ->
    sdl:init(?SDL_INIT_VIDEO bor
             ?SDL_INIT_ERLDRIVER bor
             ?SDL_INIT_NOPARACHUTE),
    Bpp = 16,
    Surface = sdl_video:setVideoMode(W, H, Bpp, 
                                     ?SDL_OPENGL bor
                                     ?SDL_RESIZABLE bor 
                                     ?SDL_GL_DOUBLEBUFFER),   
    if 
        Surface == error ->
            io:format("Can't set video mode~n", []),
            error;
        true ->
            sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),
            resize_window(W, H)
    end.

init_gl() ->
    gl:shadeModel(?GL_SMOOTH),
    gl:clearColor(0.0, 0.0, 0.0, 0.0),
    gl:clearDepth(1.0),
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LEQUAL),
    gl:hint(?GL_PERSPECTIVE_CORRECTION_HINT, ?GL_NICEST).

resize_window(Width, Height) ->
    Bpp = 16,
    Surface = sdl_video:setVideoMode(Width, Height, Bpp, 
                                     ?SDL_OPENGL bor ?SDL_RESIZABLE bor 
                                     ?SDL_GL_DOUBLEBUFFER),   
    if 
        Surface == error ->
            io:format("Can't set video mode~n", []),
            error;
       true ->
            sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),
            Ratio = Width / Height,    
            gl:viewport(0, 0, Width, Height),
            gl:matrixMode(?GL_PROJECTION),
            gl:loadIdentity(),
            glu:perspective(45.0, Ratio, 0.1, 100.0),
            gl:matrixMode(?GL_MODELVIEW),
            gl:loadIdentity()
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

render(#state{game = Game, width = Width, height = Height}) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    Origin = origin(Width, Height),
    lists:foreach(fun (Block) -> draw_block(Block, Origin) end,
                  game:blocks(Game)),
    T = gl:swapBuffers().

origin(Width, Height) ->
    Ratio = Width/Height,
    Scale = 40,
    {-Width/Scale, Height/(Scale/Ratio), -32}.

draw_block(Block = {{X, Y}, Shape}, {X0, Y0, Z0}) ->
    {R, G, B} = color(Shape),
    Outline   = 0.5,
    Fill      = Outline * 0.9,
    gl:loadIdentity(),
    gl:translatef(X0+X, Y0+Y, Z0),
    gl:color3f(R-0.2, G-0.2, B-0.2),
    gl:glBegin(?GL_QUADS),
    gl:vertex3f(-Outline, Outline, 0.0),
    gl:vertex3f(Outline, Outline, 0.0),
    gl:vertex3f(Outline, -Outline, 0.0),
    gl:vertex3f(-Outline, -Outline, 0.0),
    gl:glEnd(),
    gl:color3f(R, G, B),
    gl:glBegin(?GL_QUADS),
    gl:vertex3f(-Fill, Fill, 0.0),
    gl:vertex3f(Fill, Fill, 0.0),
    gl:vertex3f(Fill, -Fill, 0.0),
    gl:vertex3f(-Fill, -Fill, 0.0),
    gl:glEnd().

color(i) -> {0.0, 1.0, 1.0};
color(j) -> {0.0, 0.0, 1.0};
color(l) -> {1.0, 0.5, 0.0};
color(o) -> {1.0, 1.0, 0.0};
color(s) -> {0.0, 1.0, 0.0};
color(t) -> {1.0, 0.0, 1.0};
color(z) -> {1.0, 0.0, 0.0};
color(g) -> {0.5, 0.5, 0.5}.

load_sprites(L) ->
    [{Shape, sdl_video:loadBMP("sprites/" ++ atom_to_list(Shape) ++ ".bmp")}
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
