-module(game).

-author('fabian@fmbb.se').

-export([new/2, tick/1]).
-export([blocks/1, get_size/1, level/1, live/1, next/1, score/1]).
-export([move_left/1, move_right/1, rotate/1]).
-export([soft_drop/1, hard_drop/1]).

%% scores per grid unit dropped
-define(SOFT_DROP_SCORE, 1).
-define(HARD_DROP_SCORE, 2).

points_per_row() ->
    #{0 => [100, 400, 900, 2000],
      1 => [100, 400, 900, 2000],
      2 => [200, 800, 1800, 4000],
      3 => [200, 800, 1800, 4000],
      4 => [300, 1200, 2700, 6000],
      5 => [300, 1200, 2700, 6000],
      6 => [400, 1600, 3600, 8000],
      7 => [400, 1600, 3600, 8000],
      higher => [500, 2000, 4500, 10000]}.

-record(game,
        {width,
         height,
         next_piece,
         current_piece,
         ground,
         level = 0,
         next_level = 10,
         score = 0,
         live = yes}).

next(#game{next_piece = Next}) ->
    Next.

score(#game{score = Score}) ->
    Score.

level(#game{level = Level}) ->
    Level.

live(#game{live = Live}) ->
    Live.

get_size(#game{width = Width, height = Height}) ->
    {Width, Height}.

new(Width, Height) when Width > 4, Height > 4 ->
    CurrPiece = piece:new(),
    NextPiece = piece:new(),
    #game{width = Width,
          height = Height,
          current_piece = piece:translate(CurrPiece, {round(Width / 2), 0}),
          next_piece = piece:translate(NextPiece, {round(Width / 2), 0}),
          ground = []}.

points(NumRows, Level) ->
    ScoreTables = points_per_row(),
    Default = maps:get(higher, ScoreTables),
    ScoreTable = maps:get(Level, ScoreTables, Default),
    lists:nth(NumRows, ScoreTable).

blocks(#game{current_piece = undefined, ground = Ground}) ->
    Ground;
blocks(#game{current_piece = Piece, ground = Ground}) ->
    Shape = piece:shape(Piece),
    [{Coord, Shape} || Coord <- piece:blocks(Piece)] ++ Ground.

is_blocked(Piece, Game) ->
    IsBlocked = fun(C) -> is_out_of_bounds(C, Game) orelse touches_ground(C, Game) end,
    lists:any(IsBlocked, piece:blocks(Piece)).

touches_ground(Coord = {_, Y}, #game{height = Height, ground = Ground}) ->
    Height + Y < 1 orelse lists:keymember(Coord, 1, Ground).

is_out_of_bounds({X, _}, #game{width = Width}) ->
    (X < 0) or (Width =< X).

move_left(Game = #game{live = no}) ->
    Game;
move_left(Game = #game{current_piece = Piece}) ->
    NewPiece = piece:translate(Piece, {-1, 0}),
    case is_blocked(NewPiece, Game) of
        false ->
            Game#game{current_piece = NewPiece};
        _ ->
            Game
    end.

move_right(Game = #game{live = no}) ->
    Game;
move_right(Game = #game{current_piece = Piece}) ->
    NewPiece = piece:translate(Piece, {1, 0}),
    case is_blocked(NewPiece, Game) of
        false ->
            Game#game{current_piece = NewPiece};
        _ ->
            Game
    end.

rotate(Game = #game{live = no}) ->
    Game;
rotate(Game = #game{current_piece = Piece}) ->
    NewPiece = piece:rotate(Piece, r),
    case is_blocked(NewPiece, Game) of
        false ->
            Game#game{current_piece = NewPiece};
        _ ->
            Game
    end.

tick(Game = #game{live = no}) ->
    Game;
tick(Game = #game{next_level = 0, level = Level}) ->
    Game#game{next_level = 10, level = Level + 1};
tick(Game =
         #game{current_piece = Piece,
               next_piece = NextPiece,
               width = Width,
               score = Score,
               next_level = NextLevel}) ->
    %% piece falls
    NewPiece = piece:translate(Piece, {0, -1}),
    case lists:any(fun(Coord) -> touches_ground(Coord, Game) end, piece:blocks(NewPiece)) of
        false ->
            %% no ground hit
            Game#game{current_piece = NewPiece};
        _ ->
            %% ground hit, make the piece part of it
            {NewGround, Points} = merge(Piece, Game),
            case lists:any(fun({{_X, Y}, _S}) -> Y >= 0 end, NewGround) of
                false ->
                    Game#game{current_piece = NextPiece,
                              next_piece =
                                  piece:translate(
                                      piece:new(), {round(Width / 2), 0}),
                              ground = NewGround,
                              next_level = NextLevel - 1,
                              score = Score + Points};
                _ ->
                    Game#game{live = no}
            end
    end.

merge(Piece,
      #game{ground = Ground,
            width = Width,
            level = Level}) ->
    Shape = piece:shape(Piece),
    NewGround = [{Coord, Shape} || Coord <- piece:blocks(Piece)] ++ Ground,
    case filled_rows(NewGround, Width) of
        [] ->
            {NewGround, 0};
        Rows ->
            %% TODO what is this doing, really?
            RowBlocks = [hd(Row) || Row <- Rows],
            RowBlocksSorted = lists:sort(fun({{_, Y1}, _}, {{_, Y2}, _}) -> Y1 > Y2 end, RowBlocks),
            ChompedGround = chomp(NewGround -- lists:flatten(Rows), RowBlocksSorted),
            Points = points(length(Rows), Level),
            PerfectMultiplier =
                case length(ChompedGround) of
                    0 ->
                        10;
                    _ ->
                        1
                end,
            {ChompedGround, Points * PerfectMultiplier}
    end.

filled_rows([], _) ->
    [];
filled_rows([ThisBlock | Blocks], Width) ->
    {{_, Y}, _} = ThisBlock,
    ThisRow = [Block || {{_, Y1}, _} = Block <- Blocks, Y1 == Y],
    case length(ThisRow) of
        L when L == Width - 1 ->
            OtherRows = Blocks -- ThisRow,
            [[ThisBlock | ThisRow] | filled_rows(OtherRows, Width)];
        _ ->
            filled_rows(Blocks -- ThisRow, Width)
    end.

chomp(Blocks, []) ->
    Blocks;
chomp(Blocks, [Row | Rows]) ->
    chomp(fall(Blocks, Row), Rows).

fall(Blocks, {{_, RY}, _}) ->
    lists:map(fun({{BX, BY}, BS}) ->
                 case BY >= RY of
                     true -> {{BX, BY - 1}, BS};
                     _ -> {{BX, BY}, BS}
                 end
              end,
              Blocks).

soft_drop(Game = #game{score = Score}) ->
    %% this is a bit of a hack but the actual time
    %% ticking is handled outside of the game module
    Game#game{score = Score + ?SOFT_DROP_SCORE}.

hard_drop(Game) ->
    hard_drop(Game, 0).

%% TODO this is probably not very efficient
hard_drop(Game = #game{current_piece = Piece, score = Score}, DropScore) ->
    NewPiece = piece:translate(Piece, {0, -1}),
    Touches = fun(Coord) -> touches_ground(Coord, Game) end,
    case lists:any(Touches, piece:blocks(NewPiece)) of
        false ->
            hard_drop(Game#game{current_piece = NewPiece}, DropScore + ?HARD_DROP_SCORE);
        _ ->
            tick(Game#game{score = Score + DropScore})
    end.
