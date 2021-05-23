-module(piece).

-author('fabian@fmbb.se').

-export([new/0]).
-export([rotate/2, translate/2]).
-export([blocks/1, shape/1]).

-record(piece, {shape, pos, blocks}).

shape(#piece{shape = Shape}) ->
    Shape.

blocks(#piece{pos = Pos, blocks = Blocks}) ->
    [translate1(Pos, Block) || Block <- Blocks].

%% should be called random
new() ->
    Shape =
        lists:nth(
            rand:uniform(7), [i, j, l, o, s, t, z]),
    #piece{shape = Shape,
           pos = {0, 0},
           blocks = construct(Shape)}.

construct(i) ->
    [{0, 1}, {0, 0}, {0, -1}, {0, -2}];
construct(j) ->
    [{0, 1}, {0, 0}, {-1, -1}, {0, -1}];
construct(l) ->
    [{0, 1}, {0, 0}, {0, -1}, {1, -1}];
construct(o) ->
    [{0, 1}, {1, 1}, {0, 0}, {1, 0}];
construct(s) ->
    [{0, 1}, {1, 1}, {-1, 0}, {0, 0}];
construct(t) ->
    [{0, 1}, {-1, 0}, {0, 0}, {1, 0}];
construct(z) ->
    [{-1, 1}, {0, 1}, {0, 0}, {1, 0}].

rotate(Piece = #piece{blocks = Blocks}, D) when D == r; D == l ->
    Piece#piece{blocks = [rotate1(Block, D) || Block <- Blocks]}.

rotate1({X, Y}, r) ->
    {0 + Y, 0 - X};
rotate1({X, Y}, l) ->
    {0 - Y, 0 + X}.

translate(Piece = #piece{pos = Pos}, Delta) ->
    Piece#piece{pos = translate1(Pos, Delta)}.

translate1({X, Y}, {Dx, Dy}) ->
    {X + Dx, Y + Dy}.
