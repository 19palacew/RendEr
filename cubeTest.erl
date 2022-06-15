% This is an early test of some of the math principles

-module(cubeTest).
-export([start/0, blankImage/2, assimilateList/1]).
-include_lib("wx/include/wx.hrl").
% Thanks to Joe Armstrong for the window creation help

start() ->
    W = wx:new(),
    Frame = wxFrame:new(W, -1, "w3D"),
    Panel = wxPanel:new(Frame, [{size,{400,600}}]),
    Vbox = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Vbox, Panel, [{proportion, 1}, {flag, ?wxEXPAND}]),
    Width = 200,
    Height = 200,
    AltImage = wxImage:new(),
    BlankImage = blankImage(Width, Height),

    % Cube Coordinates
    XValues = [1,2,1,2,1,2,1,2],
    YValues = [1,1,2,2,1,1,2,2],
    ZValues = [4,4,4,4,6,6,6,6],
    
    DrawnImage = drawCoord(BlankImage, Width, XValues, YValues, ZValues),

    Dat = assimilateList(DrawnImage),
    wxImage:create(AltImage, Width, Height, list_to_binary(Dat)),

    %wxImage:saveFile(AltImage, "temp.png"),

    Image = wxBitmap:new(AltImage),
    %io:fwrite("~w", [wxImage:getData(RefImage)]),
    F = fun(I, _) -> redraw(Image,I) end,
    wxPanel:connect(Panel, paint, [{callback,F}]),
    wxFrame:show(Frame).

redraw(Image, #wx{obj=Panel}) ->
    DC = wxPaintDC:new(Panel),
    wxDC:drawBitmap(DC,Image,{0,0}).

blankImage(Height, Width) ->
    Num = Height * Width,
    dataHelper(Num).

dataHelper(Num) when Num > 1 -> [{0,0,0} | dataHelper(Num-1)];
dataHelper(_) -> [{0,0,0}].

assimilateList([{A, B, C}|T]) -> lists:append([A|[B|[C]]], assimilateList(T));
assimilateList([]) -> [].

%coordinate(X, Y, Z) -> {X/Z, Y/Z}.

drawCoord(Image, Width, [XHead|XTail], [YHead|YTail], [ZHead|ZTail]) ->
    %This will be shifted 1 to the right but Im running low on time
    Scale = 100,
    XVal = round((XHead/ZHead)*Scale),
    YVal = round((YHead/ZHead)*Scale),
    Index = XVal + (YVal-1)*Width,
    io:fwrite("XVal: ~w, YVal: ~w ", [XVal, YVal]),
    Temp = lists:sublist(Image,Index) ++ [{255,255,255}] ++ lists:nthtail((Index+1),Image),
    drawCoord(Temp, Width, XTail, YTail, ZTail);
drawCoord(List, _, [], [], []) -> List.

