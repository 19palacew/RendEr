-module(rendEr).
-include_lib("wx/include/wx.hrl").
-import(math3D, [convert3DTo2D/2, applyToMesh/2, scale/1]).
-import(stl, [readStl/1]).
-export([start/1]).
% Thanks to Joe Armstrong for the window creation example help

start(File) ->
    W = wx:new(),
    Frame = wxFrame:new(W, -1, "RendEr"),
    Panel = wxPanel:new(Frame, [{size,{400,600}}]),
    Vbox = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Vbox, Panel, [{proportion, 1}, {flag, ?wxEXPAND}]),

    Width = 200,
    Height = 200,

    %File = "Donut.stl",
    % Plant.stl helps show where the camera is looking in Blender
    % BUG: Y-axis will be mirrored on imported objects
    Mesh = applyToMesh(readStl(File), scale(1)),

    % D is the distance from the camera to the projection plane
    D = 100,

    % Rounding must take place before filtering duplicates because the rounding will create duplicates
    Temp = lists:filter(negativeFilter(),filterDuplicates(roundCords(createVertexCordList(convert3DTo2D(Mesh, D))))),
    CoordList = lists:sort(sortForDraw(), Temp),

    DrawnImage = drawVerts(CoordList, Width, Height),

    CurrentWorkingImage = wxImage:new(),
    wxImage:create(CurrentWorkingImage, Width, Height, list_to_binary(bmpListToArray(DrawnImage))),

    F = fun(I, _) -> redraw(wxBitmap:new(CurrentWorkingImage),I) end,
    wxPanel:connect(Panel, paint, [{callback,F}]),
    wxFrame:show(Frame).

redraw(Image, #wx{obj=Panel}) ->
    DC = wxPaintDC:new(Panel),
    wxDC:drawBitmap(DC,Image,{0,0}).

% Converts from list of tuples (RGB) to binary
bmpListToArray([{A, B, C}|T]) -> lists:append([A|[B|[C]]], bmpListToArray(T));
bmpListToArray([]) -> [].

% Filters out duplicates
filterDuplicates([H|T]) -> [H | [X || X <- filterDuplicates(T), X /= H]];
filterDuplicates([]) -> [].

% Vertices must be positive and within bounds. If an out of bounds cord enters, it will stop the draw of all behind it.
% Technically I should be drawing from 0 to Width-1 and Height-1 but it comes up short for data, maybe the image of Width actually has Width+1 spaces?
drawVerts(VertexCordList, Width, Height) -> drawVertsHelper(VertexCordList, Width, Height, 0, 0).

drawVertsHelper(_, Width, Height, XCount, YCount) when (XCount == Width) and (YCount == Height) ->
    [];
drawVertsHelper(List, Width, Height, XCount, YCount) when XCount == Width ->
    drawVertsHelper(List, Width, Height, 0, YCount+1);
drawVertsHelper([], Width, Height, XCount, YCount) ->
    [{0,0,0}|drawVertsHelper([], Width, Height, XCount+1, YCount)];
drawVertsHelper([{X,Y}|Tail], Width, Height, XCount, YCount) when (X =/= XCount) or (Y =/= YCount) ->
    [{0,0,0}|drawVertsHelper([{X,Y}|Tail], Width, Height, XCount+1, YCount)];
drawVertsHelper([{X,Y}|Tail], Width, Height, XCount, YCount) when (X == XCount) and (Y == YCount) ->
    [{255,0,0}|drawVertsHelper(Tail, Width, Height, XCount+1, YCount)].

% Takes the vertices out of the tuple that forms a polygon and creates a list of vertices
createVertexCordList([{A,B,C}|Tail]) -> [A|[B|[C|createVertexCordList(Tail)]]];
createVertexCordList([]) -> [].

% Converts the coordinates from float to integer
roundCords([{X,Y}|Tail]) -> [{round(X),round(Y)}|roundCords(Tail)];
roundCords([]) -> [].

% Sort the vertices to be read first by lowest Y coordinate then by lowest X if tie for Y.
sortForDraw() -> fun({X1, Y1},{X2, Y2}) -> sortForDrawHelper(X1, X2, Y1, Y2) end.

sortForDrawHelper(X1, X2, Y1, Y2) when Y1 == Y2 -> X1<X2;
sortForDrawHelper(_, _, Y1, Y2) -> Y1<Y2.

% Function to remove any negative coordinates
negativeFilter() -> fun({X, Y}) ->  (X >- 1) and (Y > -1) end.