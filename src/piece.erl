-module(piece).
-author('sempetmer@gmail.com').

-compile(export_all).

-record(piece, {shape, pos, blocks}).
shape(#piece{shape = Shape}) ->
    Shape.
blocks(#piece{pos = Pos, blocks = Blocks}) ->
    [translate(Pos,Block) || Block <- Blocks].

new() ->
    Shape = lists:nth(random:uniform(7), [i,j,l,o,s,t,z]),
    #piece{shape = Shape, pos = {0, 0}, blocks = construct(Shape)}.

construct(i) ->
    [{ 0, 1},
     { 0, 0},
     { 0,-1},
     { 0,-2}];
construct(j) ->
    [         { 0, 1},
              { 0, 0},
     {-1,-1}, { 0,-1}];
construct(l) ->
    [{ 0, 1},
     { 0, 0},
     { 0,-1}, { 1,-1}];
construct(o) ->
    [{ 0, 1}, { 1, 1},
     { 0, 0}, { 1, 0}];
construct(s) ->
    [         { 0, 1}, { 1, 1},
     {-1, 0}, { 0, 0}];
construct(t) ->
    [         { 0, 1},
     {-1, 0}, { 0, 0}, { 1, 0}];
construct(z) ->
    [{-1, 1}, { 0, 1},
              { 0, 0}, { 1, 0}].

rotate(Piece = #piece{blocks = Blocks}, D)
  when D == r; D == l ->
    Piece#piece{blocks = [rotate(Block, D) || Block <- Blocks]};
rotate({X,Y}, r) ->
    {0 + Y, 0 - X};
rotate({X,Y}, l) ->
    {0 - Y, 0 + X}.

translate(Piece = #piece{pos = Pos}, Delta) ->
    Piece#piece{pos = translate(Pos, Delta)};

translate({X, Y}, {Dx, Dy}) ->
    {X + Dx, Y + Dy}.
