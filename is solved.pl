% :-dynamic light/2.

neighbor(X,Y,L):-Y1 is Y-1,light(X,Y1),L=[X,Y1].
neighbor(X,Y,L):-Y1 is Y+1,light(X,Y1),L=[X,Y1].
neighbor(X,Y,L):-X1 is X-1,light(X1,Y),L=[X1,Y].
neighbor(X,Y,L):-X1 is X+1,light(X1,Y),L=[X1,Y].

neighbors(X,Y,L):-findall(Lst,neighbor(X,Y,Lst),L).

rowLeftNeighbors(X,Y,[]):-Y<1,!;wall(X,Y),!.
rowLeftNeighbors(X,Y,L):-size(_,C),Y<C+1,Y>0,not(wall(X,Y)),L=[X,Y];Y1 is Y-1,rowLeftNeighbors(X,Y1,L).

rowRightNeighbors(X,Y,[]):-size(_,C),Y>C,!;wall(X,Y),!.
rowRightNeighbors(X,Y,L):-size(_,C),Y<C+1,Y>0,not(wall(X,Y)),L=[X,Y];Y1 is Y+1,rowRightNeighbors(X,Y1,L).

rowNeighbors(X,Y,L):-Y1 is Y-1,rowLeftNeighbors(X,Y1,L);Y2 is Y+1,rowRightNeighbors(X,Y2,L);(not(wall(X,Y)),L=[X,Y]).
allRowNeighbors(X,Y,L):-findall(Lst,rowNeighbors(X,Y,Lst),L).


colUpNeighbors(X,Y,[]):-X<1,!;wall(X,Y),!.
colUpNeighbors(X,Y,L):-X<9,X>0,not(wall(X,Y)),L=[X,Y];X1 is X-1,colUpNeighbors(X1,Y,L).

colDownNeighbors(X,Y,[]):-size(R,_),X>R,!;wall(X,Y),!.
colDownNeighbors(X,Y,L):-size(R,_),X<R+1,X>0,not(wall(X,Y)),L=[X,Y];X1 is X+1,colDownNeighbors(X1,Y,L).% if we add % , we will cut some solves

colNeighbors(X,Y,L):-X1 is X-1,colUpNeighbors(X1,Y,L);X2 is X+1,colDownNeighbors(X2,Y,L);(not(wall(X,Y)),L=[X,Y]).
allColNeighbors(X,Y,L):-findall(Lst,colNeighbors(X,Y,Lst),L).% without !  allColNeighbors(5,4,L).L = [[4, 4], [3, 4], [2, 4], [1, 4], _, [6, 4], [7, 4], [8|...], _].


isMember(L):-allColNeighbors(4,5,Lst),member(L,Lst).

leftLightedRow(X,Y,0):-Y<1,!;wall(X,Y),!.
leftLightedRow(X,Y,B):-light(X,Y),B=1;Y1 is Y-1,leftLightedRow(X,Y1,B).
rightLightedRow(X,Y,0):-size(_,C),Y>C,!;wall(X,Y),!.
rightLightedRow(X,Y,B):-light(X,Y),B=1;Y1 is Y+1,rightLightedRow(X,Y1,B).

upLightedCol(X,Y,0):-X<1,!;wall(X,Y),!.
upLightedCol(X,Y,B):-light(X,Y),B=1;X1 is X-1,upLightedCol(X1,Y,B).

downLightedCol(X,Y,0):-size(R,_),X>R,!;wall(X,Y),!.
downLightedCol(X,Y,B):-light(X,Y),B=1;X1 is X+1,downLightedCol(X1,Y,B).

%cellIsLighted(X,Y):-
cellIsLighted(X,Y):-leftLightedRow(X,Y,B),B=:=1,!;rightLightedRow(X,Y,B),B=:=1;upLightedCol(X,Y,B),B=:=1;downLightedCol(X,Y,B),B=:=1,!. %!withouttest

neighborsLength(X,Y,Len):-findall(Lst,neighbor(X,Y,Lst),L),length(L,Len).
trueWallNum(X,Y):- wall_num(X,Y,Z),neighborsLength(X,Y,Len),Z=:=Len,!.

light_count(Len):- wall_num(X,Y,Z),neighborsLength(X,Y,Len),Z==Len.
light_count_correct:-findall(Z,wall_num(X,Y,Z),WNLst)
              ,
findall(L,light_count(L),LCLst)
                   ,length(WNLst,WNLstL),length(LCLst,LCLstL),WNLstL=:=LCLstL.

%%%%%%%%%%%%%%
all_cells_lighted:-check_grid(1),!.


check_row(_,Y):- size(_,C),Y>C,!.
%c:-(wall(X,Y) -> X1 is X+1,
check_row(X,Y):-not(wall(X,Y)),cellIsLighted(X,Y),Y1 is Y+1,check_row(X,Y1);wall(X,Y),Y1 is Y+1,check_row(X,Y1).

%;Y1 is Y+1,check_row(X,Y1).
check_grid(X):- size(R,_),X>R,!.
check_grid(X):-size(R,_),X<R+1,check_row(X,1),X1 is X+1,check_grid(X1).



rowRightNeighborsByCol(X,Y,[]):-size(_,C),Y>C,!;wall(X,Y),!.
rowRightNeighborsByCol(X,Y,L):-size(_,C),Y<C+1,Y>0,not(wall(X,Y)),light(X,Y),L=Y;Y1 is Y+1,rowRightNeighborsByCol(X,Y1,L).

listrowRightNeighborsByCol(X,Y,Lst):-findall(L,rowRightNeighborsByCol(X,Y,L),Lst).
allRowCells(_,Y):-size(_,C),Y>C,!.
allRowCells(X,Y):-size(_,C),Y<C+1,listrowRightNeighborsByCol(X,Y,Lst),length(Lst,Len),Len<3,Y1 is Y+1,allRowCells(X,Y1).

checkDouble(X):- size(R,_),X>R,!.
checkDouble(X):- size(R,_),X<R+1,allRowCells(X,1),X1 is X+1,checkDouble(X1).

no_double_light:-checkDouble(1).

solved:- all_cells_lighted,
    no_double_light ,
light_count_correct,! .% add cut !

%checkLightsNumber(X,Y):-

%checkRowLightCount(X,Y):-

%cr(X):-size(_,C),X>C+1.
%size(7,3).

print_row(_,Y):-size(_,C),Y>C.
print_row(X,Y):-size(_,C),Y<C+1,Y1 is Y+1,light(X,Y)
,write('L'),print_row(X,Y1);wall_num(X,Y,Z)
,write('B'),write(Z),Y1 is Y+1,print_row(X,Y1);not(wall_num(X,Y,Z)),wall(X,Y),write('B'),Y1 is Y+1,print_row(X,Y1)
;not(light(X,Y)),not(wall(X,Y)),write(*),Y1 is Y+1,print_row(X,Y1).
print_grid(X):- size(R,_),X>R.
print_grid(X):- size(R,_),X<R+1,write('\n'),X1 is X+1,print_row(X,1),print_grid(X1),!.
print:- print_grid(1),!.
/*
dynamic lt/2.
%lt(X,Y).
f(X,Y):-assert(lt(X,Y)).
ff:-retractall(lt(X,Y)).
*/

initialGame:-retractall(light(X,Y)),print.
%f1(X,Y):-assert(light(X,Y)).

add4Lights(X,Y):-Y1 is Y-1,assert(light(X,Y1))
,Y2 is Y+1,assert(light(X,Y2))
,X1 is X-1,assert(light(X1,Y))
,X2 is X+1,assert(light(X2,Y)).



searchFor4(_,Y):-size(_,C),Y>C.
searchFor4(X,Y):-size(_,C),Y<C+1,wall_num(X,Y,4)
,add4Lights(X,Y),!
%,write(Y),!
;Y1 is Y+1,searchFor4(X,Y1).

/*
size(12,12).

wall(1,3).
wall(1,10).
wall(2,2).
wall(2,5).
wall(2,8).
wall(2,11).
wall(3,2).
wall(3,3).
wall(3,10).
wall(3,11).
wall(4,4).
wall(4,5).
wall(4,8).
wall(4,9).
wall(6,1).
wall(6,5).
wall(6,6).
wall(6,7).
wall(6,8).
wall(6,12).
wall(7,1).
wall(7,5).
wall(7,6).
wall(6,7).
wall(7,8).
wall(7,12).
wall(9,4).
wall(9,5).
wall(9,8).
wall(9,9).
wall(10,2).
wall(10,3).
wall(10,10).
wall(10,11).
wall(11,2).
wall(11,5).
wall(11,8).
wall(11,11).
wall(12,3).
wall(12,10).

wall_num(1,3,2).
wall_num(2,2,2).
wall_num(2,8,2).
wall_num(2,11,3).
wall_num(4,4,1).
wall_num(4,5,3).
wall_num(4,9,1).
wall_num(7,1,1).
wall_num(7,8,1).
wall_num(7,12,1).
wall_num(11,2,3).
wall_num(11,5,3).
wall_num(11,11,3).

% light(1,1).
% light(1,4).
light(1,2).
light(1,8).
light(1,11).
light(2,3).
light(2,7).
light(2,10).
light(2,12).
light(3,1).
light(3,5).
light(4,3).
light(4,6).
light(4,10).
light(5,5).
light(6,4).
light(6,9).
light(7,2).
light(7,11).
light(8,8).
light(9,7).
light(10,5).
light(11,1).
light(11,3).
light(11,6).
light(11,10).
light(11,12).
light(12,2).
light(12,5).
light(12,11).
*/
size(3,3).
light(1,1).
%light(3,2).
light(2,3).


wall_num(2,2,1).
wall(2,2).

