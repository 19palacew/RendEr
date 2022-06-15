-module(math3D).
-export([convert3DTo2D/2, applyToMesh/2, scale/1]).


%linesToCordsHelper(YIndex, [[X1|X2]|XTail], Color) -> [[lineRangeToCord(X1,X2, YIndex), Color]|linesToCordsHelper(YIndex+1, XTail, Color)].

%linesToCordsHelper(Counter, YMax, [[X1|X2]|XTail]) when Counter < YMax -> [[lineRangeToCord(X1,X2, Counter)]|linesToCordsHelper(Counter+1, YMax, XTail)];
%linesToCordsHelper(Counter, _, [], _) -> [].


% Takes a y value and the start an end of a line on the x-axis and creates the cordinates for every integer positon in (x,y) notation
lineRangeToCord(X1, X2, Y) when X1=<X2 -> [{X1, Y}|lineRangeToCord(X1+1, X2, Y)];
lineRangeToCord(_,_,_) -> [].

% Divides the x and y by z to create a 3D effect with 2D coordinates
%convert3DTo2D([{{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}}|Tail]) -> [{{X1/Z1,Y1/Z1},{X2/Z2,Y2/Z2},{X3/Z3,Y3/Z3}}|convert3DTo2D(Tail)];
%convert3DTo2D([]) -> [].


convert3DTo2D([{{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}}|Tail], D) -> [{{X1*(D/Z1),Y1*(D/Z1)},{X2*(D/Z2),Y2*(D/Z2)},{X3*(D/Z3),Y3*(D/Z3)}}|convert3DTo2D(Tail,D)];
convert3DTo2D([],_) -> [].

applyToMesh([{A,B,C}|Tail], Function) -> [{Function(A), Function(B), Function(C)}|applyToMesh(Tail, Function)];
applyToMesh([], _) -> [].

% Returns a function to be used with applyToMesh
scale(Value) -> fun({X, Y, Z}) -> {X * Value, Y * Value, Z * Value} end.



