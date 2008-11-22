-module(gui).
-author('sempetmer@gmail.com').

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("gl.hrl").

-compile(export_all).

-record(state, {window, game, sprites, font, delay = 0}).

-define(WIDTH, 480).
-define(HEIGHT, 640).
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
            [sdl_video:setColorKey(Sprite, ?SDL_SRCCOLORKEY,
                                   sdl_video:mapRGB(Sprite, 0, 0, 0)) ||
                {_Shape, Sprite} <- Sprites],
            Font = load_font("mario3.bmp"),
            sdl_video:setColorKey(Font, ?SDL_SRCCOLORKEY,
                                  sdl_video:mapRGB(Font, 255, 0, 255)),
            loop(#state{window = Window, game = Game,
                        sprites = Sprites, font = Font}),
            sdl:quit()
    end.

init_video() ->
    io:format("Initializing SDL.", []),
    sdl:init(?SDL_INIT_VIDEO),
    io:format("..~n", []),
    resize_window(?WIDTH, ?HEIGHT).

resize_window(Width, Height) ->
    io:format("Setting video mode.", []),
    case sdl_video:setVideoMode(Width, Height, ?BPP,
                                ?SDL_HWSURFACE bor
                                ?SDL_RESIZABLE bor
                                ?SDL_DOUBLEBUF) of
        error ->
            io:format("..~nCan't set video mode~n", []),
            error;
        Surface ->
            io:format("..~n", []),
            sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),
            Surface
    end.

loop(State) ->
    render(State),
    NewState = handle_event(State),
    case NewState of
        quit ->
            ok;
        #state{delay = 0, game = NewGame} ->
            loop(NewState#state{delay = delay(game:level(NewGame)),
                                game = game:tick(NewGame)});
        #state{delay = Delay} ->
            loop(NewState#state{delay = Delay - 1})
    end.

render(#state{window = Window, game = Game,
              sprites = Sprites, font = Font}) ->
    sdl_video:fillRect(Window, null, sdl_video:mapRGB(Window, 0, 0, 0)),
    sdl_video:fillRect(Window,
                       #sdl_rect{x=0,y=0,w=320,h=640},
                       sdl_video:mapRGB(Window, 10, 10, 10)),
    NextPiece = game:next(Game),
    NextShape = piece:shape(NextPiece),
    lists:foreach(fun (Block) -> draw_block(Block, Window, Sprites) end,
                  game:blocks(Game) ++
                  [{Coord, NextShape} ||
                      Coord <- piece:blocks(piece:translate(NextPiece, {6,-2}))]),
    draw_string("LEVEL: " ++ integer_to_list(game:level(Game)),
                Font, 352, 192, Window),
    draw_string("SCORE: " ++ integer_to_list(game:score(Game)),
                Font, 352, 201, Window),
    case game:live(Game) of
        no ->
            draw_string("GAME OVER MAN",
                        Font, 352, 210, Window);
        _ ->
            ok
    end,    
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

draw_string(S, Font, X, Y, Window) ->
    draw_string(S, Font, X, Y, Window, 0).

draw_string([], _Font, _X, _Y, _Window, _I) ->
    ok;
draw_string([C | S], Font, X, Y, Window, I) ->
    draw_char(C, Font, X + I * 9, Y, Window),
    draw_string(S, Font, X, Y, Window, I + 1).

draw_char(C, Font, X, Y, Window)
  when C >= $0, C =< $9 ->
    Source = #sdl_rect{x = 66 + (C - $0) * 9, y = 1, w = 8, h = 8},
    Dest   = #sdl_rect{x = X, y = Y, w = 8, h = 8},
    sdl_video:blitSurface(Font, Source, Window, Dest);
draw_char(C, Font, X, Y, Window)
  when C >= $A, C =< $Z ->
    Source = #sdl_rect{x = 191 + (C - $A) * 9, y = 1, w = 8, h = 8},
    Dest   = #sdl_rect{x = X, y = Y, w = 8, h = 8},
    sdl_video:blitSurface(Font, Source, Window, Dest);
draw_char(_C, _Font, _X, _Y, _Window) ->
    ok.

load_sprites(L) ->
    [{Shape, sdl_video:loadBMP("src/sprites/" ++ atom_to_list(Shape) ++ ".bmp")}
     || Shape <- L].

load_font(FontName) ->
    sdl_video:loadBMP("fonts/" ++ FontName).

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
                ?SDLK_SPACE ->
                    State#state{game = game:drop(Game)};
                _ ->
                    State
            end;
        _ ->
            timer:sleep(10),
            State
    end.

delay(Level) ->
    case 20 - Level of
        NewDelay when NewDelay < 0 ->
            0;
        NewDelay ->
            NewDelay
    end.
