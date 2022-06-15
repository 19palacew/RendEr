-module(stl).
-export([readStl/1]).

% Make sure stl is in ASCII
readStl(FileName) ->
    {ok, Data} = file:read_file(FileName),
    Text = string:tokens(binary_to_list(Data), " \n\r"),
    createPolygon(vertexExtract(Text)).

createPolygon([A|[B|[C|Tail]]]) -> [{A, B, C}|createPolygon(Tail)];
createPolygon([]) -> [].

vertexExtract([Head|Tail]) when Head == "vertex" -> [vertexExtractHelper(Tail)|vertexExtract(Tail)];
vertexExtract([_|Tail]) -> vertexExtract(Tail);
vertexExtract([]) -> [].

% Switches Y and Z because this is a Y-up render
vertexExtractHelper([A|[B|[C|_]]]) ->
    {X, []} = string:to_float(A),
    {Z, []} = string:to_float(B),
    {Y, []} = string:to_float(C),
    {X, Y, Z}.