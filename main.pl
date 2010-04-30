/**
 * FPR 2010 Nurikabe Pavel
 *
 */

%%%%  FILE  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Tests if character is EOF or LF.
isEndOfFile(C) :-
  C == -1, assert(souborJeNaKonci:-true).

isEONum(C) :-
  C == 10;
  C == 32.

readNoSpace(Stream,String):-
  get_byte(Stream,C),
  not(isEndOfFile(C)),
%  write('C="'),write(C),write('"'),
  (isEONum(C), String = [], !;
      readNoSpace(Stream,NextPart),
      [C|NextPart] = String
   ).

%naplni Number cislem prectenym z IO cislo je [0-9]+
readIntFromFile(Stream, Number) :-
  readNoSpace(Stream,String),
  (String\=[], string_to_int(String, Number),! ;
    readIntFromFile(Stream, Number)).

%strint_to_int
s_t_i([],Mezisoucet,Int) :-
  Int is Mezisoucet.
s_t_i([H|T],Mezisoucet,Int) :-
%  write('Mezisoucet '),write(Mezisoucet),nl,
%  write('HHH '),write(H),nl,
  Mezi is (Mezisoucet*10+(H-48)),
%  write('Mezi '),write(Mezi),nl,
  s_t_i(T,Mezi,Int).
string_to_int(List, Int):-
  s_t_i(List, 0, Int).


loadGrid(FileStream) :-
  assert(souborJeNaKonci:-fail),
  readIntFromFile(FileStream,RowsNum),%write('Rows: '),write(RowsNum),nl,
  assert((rows(Val):- Val=RowsNum)),
  readIntFromFile(FileStream,ColsNum),%write('Cols: '),write(ColsNum),nl,
  assert((cols(Val):- Val=ColsNum)),
  not((
  between(1, RowsNum, Y),
    between(1, ColsNum, X),
      not((readIntFromFile(FileStream,Number),
        (Number\=0,assert(field(X,Y,Number));true)
%        ,write('Number IS '),write(Number),nl
    ))
  )).

%%%% FILE  END  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%% POMOCNE PREDIKATY  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isInRange(X,Y) :-
  X > 0, Y > 0,
  cols(C), X =< C,
  rows(R), Y =< R.

isOutOfX(X) :- 
  cols(C), X > C.

isOutOfY(Y) :-
  rows(R), Y > R.

isBlack(X,Y) :-
  tmp_black(X,Y);black(X,Y).

isWhite(X,Y) :-
  tmp_white(X,Y);white(X,Y). 

level2Coors(Index,C) :- 
  cols(W),
  X is ((Index+1) mod W)+1,
  Y is ((Index+1) // W),
  [X,Y] = C.

coors2Level([X,Y],C) :- 
  cols(W),
  C is (Y-1)*W+X.
%%%% POMOCNE PREDIKATY  END  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%% KONTROLA SITUACE  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setBlack([X,Y]):-
  %souradnice souseda nad aktualnim
  (X1=X),(Y1 is (Y-1)),
  %souradnice souseda vpravo
  (X2 is X+1),(Y2=Y),
  %souradnice souseda pod aktualnim
  (X3=X),(Y3 is Y+1),
  %souradnice souseda vlevo
  (X4 is X-1), (Y4=Y),
  %neni-li mimo
  (
    isInRange(X1,Y1),assert(black(X1,Y1)),fail;
    isInRange(X2,Y2),assert(black(X2,Y2)),fail;
    isInRange(X3,Y3),assert(black(X3,Y3)),fail;
    isInRange(X4,Y4),assert(black(X4,Y4))
  );true.

setAllBlack([]):-
  true.
setAllBlack([(X,Y)|T]):-
  setBlack([X,Y]),
  setAllBlack(T).

% ohranice cerne jednicky
mark(1):-
  findall((X,Y),field(X,Y,1),FieldSet),
  setAllBlack(FieldSet).

%mark(Number):-
%  .

%%%% KONTROLA SITUACE  END  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%% STAOVY PROSTOR  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%zde se provadi kontrola jestli je plocha validni
combi(0) :-
  findall((G,H),tmp_black(G,H),Pole)%,write('cerne '),write(Pole), nl
  ,findall((R,S),tmp_white(R,S),Pole2)%,write('white '),write(Pole2),nl
  .

/*
combi(Level) :-
  level2Coors(Level,[X,Y]),
  field(X,Y,_),
  NextLevel is Level -1,
  combi(NextLevel)
  .
*/

%pokud je tam cerna nebo bila napevno
combi(Level) :-
  level2Coors(Level,[X,Y]),
  NextLevel is Level -1,
  (black(X,Y);white(X,Y);field(X,Y,_)) ->
  (
    combi(NextLevel)
  );
  (
    assert(tmp_black(X,Y)),combi(NextLevel),retract(tmp_black(X,Y)),
    assert(tmp_white(X,Y)),combi(NextLevel),retract(tmp_white(X,Y))
  )
  .

solve(Cols,Rows) :-
  % inicializace
  assert(tmp_black(-1,-1)),retract(tmp_black(-1,-1)),
  assert(tmp_white(-1,-1)),retract(tmp_white(-1,-1)),
  assert(black(-1,-1)),retract(black(-1,-1)),
  assert(white(-1,-1)),retract(white(-1,-1)),

  %test
  coors2Level([3,2],I), write('index: '), write(I), nl,

  mark(1),

%  findall((G,H),black(G,H),Pole),write('cerne '),write(Pole), nl,

  (Depth is Cols*Rows),
/*  not((
  between(1, Rows, Y),
    between(1, Cols, X),
    Coor = [X, Y],
%    write('souradnice: '),write(Coor),nl,
    fail
  )),
*/  
  combi(Depth).
%%%% STAOVY PROSTOR  END  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%% VYSTUP  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
printField(X,Y) :-
  isOutOfY(Y) -> !;
  isOutOfX(X) -> (Xnext is 1, Ynext is Y+1, nl, printField(Xnext,Ynext));
  field(X,Y,Z) -> write(Z), write(' '), Xnext is X+1, printField(Xnext,Y);
  isWhite(X,Y) -> (write('_ '), Xnext is X+1, printField(Xnext,Y));
  isBlack(X,Y) -> (write('# '), Xnext is X+1, printField(Xnext,Y));
  (write('u '), Xnext is X+1, printField(Xnext,Y))
  .

printDesk :-
  nl,write('=== reseni je: ==='),nl,
  printField(1,1).
 %%%% VYSTUP  END  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   


%%%%% KONTROLA JESTLI JSOU CERNE POLICKA SPOJITE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
	Smazne vsechny predikaty ulozene v pameti
*/
resetMemory :- 
		rows(R),cols(C),
		not((
		between(1, R, Y),
			between(1, C, X),
				(retract(black(X,Y))),
				(retract(white(X,Y))),
				(retract(field(X,Y,_))),fail
	    )).

/*
	Vymazani vsech docasne ulozenych promennych sousedu 
*/
deleteNeighbour([]) :- true.
deleteNeighbour([(X,Y)|T]) :-
	retract(neighbour(X,Y)),
	deleteNeighbour(T)
	.

resetNeighbours :- 
		findall((G,H), neighbour(G,H), Pole),
		deleteNeighbour(Pole),
%		write('delete ok'),nl,
		true.

/*
	rekurzivni pruchod a kontrola, zda maji vsechny cerne pole nejakeho souseda
*/
checkBlackNeighbours([]) :- fail.
checkBlackNeighbours((X,Y)) :-
%		write('checkuje '),write([X,Y]),nl,

		% je cernej, takze priradim do seznamu sousedu a proverim ostatni
		black(X,Y) -> (
%			write('cernoch: '),write([X,Y]),nl,
			Yu is Y-1, Yd is Y+1,
			Xl is X-1, Xr is X+1,
			(not(neighbour(X, Y)) -> assert(neighbour(X,Y))),
			(
				(not(neighbour(Xl, Y)) -> checkBlackNeighbours((Xl, Y))),fail;
				(not(neighbour(Xr, Y)) -> checkBlackNeighbours((Xr, Y))),fail;
				(not(neighbour(X, Yu)) -> checkBlackNeighbours((X, Yu))),fail;
				(not(neighbour(X, Yd)) -> checkBlackNeighbours((X, Yd)))
			),
			true
		),
		fail
		.

/*
	Kontrola, zda jsou cerne posloupne
*/
checkBlacks :- 
		findall((G,H), black(G,H), BlackOnes),	% najde vsechny cerne
		nth1(1, BlackOnes, Frst),				% vybere prvni prvek a od nej zacne hledat prilehle cerne

%		write('cernosi: '),write(BlackOnes),nl,

		% smaze z pameti vsechny docasne sousedy
		resetNeighbours,
		% zkontroluje jestli ma prvek souseda a rekurzivne hleda sousedy souseda
		not(checkBlackNeighbours(Frst)),

		%porovnani velikosti poli sousedu prvniho prvku a vsech cernych
		% kdyz nejsou stejne velka, tak to znamena, ze je nekde odlehla cerna
		findall((G,H), neighbour(G,H), Neighbours),	% najde vsechny sousedy
		length(BlackOnes, BlackCount),
		length(Neighbours, NeighCount),

%		write('Sousedi: '),write(Neighbours),nl,
%		write('cernochu: '),write(BlackCount),nl,write('sousedu: '),write(NeighCount),nl,

		BlackCount=NeighCount
		.
%%%%% /KONTROLA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
  unix(args(Args)),
  nth1(5,Args,FileName),
  write('kontrolni vypis jmena souboru: "'),write(FileName),write('"'),nl,
  open(FileName, read, FileStream),
  loadGrid(FileStream),
  close(FileStream),
  rows(RowsNum),write('Rows: '),write(RowsNum),nl,
  cols(ColsNum),write('Cols: '),write(ColsNum),nl,
  nl,
  solve(ColsNum,RowsNum),
  printDesk,
  write('Aplikace KONCI >>OK<<!'),nl
  .

% /*
prolog :- 
  main.
% */
