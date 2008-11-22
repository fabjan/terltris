-module(game).
-author('sempetmer@gmail.com').

-compile(export_all).

-record(game, {width, height, next_piece, current_piece, ground,
               level = 0, next_level = 10, score = 0, live = yes}).

width(#game{width = Width}) ->
    Width.
height(#game{height = Height}) ->
    Height.
next(#game{next_piece = Next}) ->
    Next.
score(#game{score = Score}) ->
    Score.
level(#game{level = Level}) ->
    Level.
live(#game{live = Live}) ->
    Live.

new(Width, Height)
  when Width > 4, Height > 4 ->
    #game{width = Width, height = Height,
          next_piece = piece:translate(piece:new(),
                                       {round(Width/2), 0}),
          current_piece = piece:translate(piece:new(),
                                          {round(Width/2), 0}),
          ground = []}.

blocks(#game{current_piece = undefined, ground = Ground}) ->
    Ground;
blocks(#game{current_piece = Piece, ground = Ground}) ->
    Shape = piece:shape(Piece),
    [{Coord, Shape} || Coord <- piece:blocks(Piece)] ++ Ground.

get_block(Game, Coord) ->
    Blocks = blocks(Game),
    case lists:keysearch(Coord, 1, Blocks) of
        {value, {_, Shape}} ->
            Shape;
        _ ->
            undefined
    end.

move_left(Game = #game{live = no}) ->
    Game;
move_left(Game = #game{current_piece = Piece}) ->
    NewPiece = piece:translate(Piece, {-1, 0}),
    case lists:any(fun(Coord) -> outside(Coord, Game) or hit(Coord, Game) end,
                   piece:blocks(NewPiece)) of
        false ->
            Game#game{current_piece = NewPiece};
        _ ->
            Game
    end.

move_right(Game = #game{live = no}) ->
    Game;
move_right(Game = #game{current_piece = Piece}) ->
    NewPiece = piece:translate(Piece, {1, 0}),
    case lists:any(fun(Coord) -> outside(Coord, Game) or hit(Coord, Game) end,
                   piece:blocks(NewPiece)) of
        false ->
            Game#game{current_piece = NewPiece};
        _ ->
            Game
    end.

rotate(Game = #game{live = no}) ->
    Game;
rotate(Game = #game{current_piece = Piece}) ->
    NewPiece = piece:rotate(Piece, r),
    case lists:any(fun(Coord) -> outside(Coord, Game) or hit(Coord, Game) end,
                   piece:blocks(NewPiece)) of
        false ->
            Game#game{current_piece = NewPiece};
        _ ->
            Game
    end.

tick(Game = #game{live = no}) ->
    Game;
tick(Game = #game{next_level = 0, level = Level}) ->
    Game#game{next_level = 10, level = Level + 1};
tick(Game = #game{current_piece = Piece,
                  next_piece = NextPiece,
                  ground = Ground,
                  width = Width,
                  score = Score,
                  next_level = NextLevel}) ->
    NewPiece = piece:translate(Piece, {0, -1}),
    case lists:any(fun(Coord) -> hit(Coord, Game) end,
                   piece:blocks(NewPiece)) of
        false ->
            Game#game{current_piece = NewPiece};
        _ ->
            {NewGround, Points} = merge(Piece, Ground, Width),
            case lists:any(fun({{_X, Y}, _S}) -> Y >= 0 end,
                           NewGround) of
                false ->
                    Game#game{current_piece = NextPiece,
                              next_piece = piece:translate(piece:new(),
                                                           {round(Width/2), 0}),
                              ground = NewGround,
                              next_level = NextLevel - 1,
                              score = Score + Points};
                _ ->
                    Game#game{live = no}
            end
    end.

hit(Coord = {_X, Y}, #game{height = Height, ground = Ground}) ->
    lists:keymember(Coord, 1, Ground) or (Height + Y < 1).

outside({X, _Y}, #game{width = Width}) ->
    (X < 0) or (X >= Width).

merge(Piece, Ground, Width) ->
    Shape = piece:shape(Piece),
    NewGround = [{Coord, Shape} || Coord <- piece:blocks(Piece)] ++ Ground,
    case collect(NewGround, Width) of
        [] ->
            {NewGround, 0};
        Rows ->
            {chomp(NewGround -- lists:flatten(Rows),
                   lists:sort(fun({{_, Y1}, _},
                                  {{_, Y2}, _}) ->
                                      Y1 > Y2 end,
                              [hd(Row) || Row <- Rows])),
             round(math:pow(length(Rows), 2))}
    end.

collect([], _Width) ->
    [];
collect([Block0 | Blocks], Width) ->
    {{_, Y}, _} = Block0,
    Row = [Block || {{_, Y1}, _} = Block <- Blocks, Y1 == Y],
    case length(Row) of
        L when L == (Width - 1) ->
            [[Block0 | Row] | collect(Blocks -- Row, Width)];
        _ ->
            collect(Blocks -- Row, Width)
    end.

chomp(Blocks, []) ->
    Blocks;
chomp(Blocks, [Row | Rows]) ->
    chomp(fall(Blocks, Row), Rows).

fall(Blocks, Row = {{_RX, RY}, _RS}) ->
    lists:map(fun({{BX, BY}, BS}) ->
                      case BY >= RY of
                          true ->
                              {{BX, BY - 1}, BS};
                          _ ->
                              {{BX, BY}, BS}
                          end
              end,
              Blocks).
