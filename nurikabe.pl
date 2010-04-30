/**
 *  Program   : nurikabe.hs
 *  Copyright : tvrdaci 2010
 *  Authors   : Jiri Melichar, Pavel Srb, Michal Svec
 *  Project   : FPR
 *  Version   : 0.0
 *  Modified  : 30.04.2010
 */

:- dynamic black/2, white/2, tmp_black/2, tmp_white/2, field/3, neighbour/2.

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
  cols(C), X > C;X<1.

isOutOfY(Y) :-
  rows(R), Y > R;Y<1.

isBlack(X,Y) :-
  tmp_black(X,Y);black(X,Y).

isWhite(X,Y) :-
  tmp_white(X,Y);white(X,Y). 

level2Coors(Index,C) :- 
  cols(W),
  Y is ((Index-1) // W)+1,
  X is ((Index-1) mod W)+1,
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

%Funkce pro kontrolu zda se na souradnici X,Y nachazi 4 cerne bunky
%pokud zabraly vsechny podminky, tak mam loop
isSquare(X,Y) :- 
  Xp is X + 1,
  Yp is Y + 1,
  isBlack( X  , Y  ),
  isBlack( Xp , Y  ),
  isBlack( X  , Yp ),
  isBlack( Xp , Yp )
  .

%vraci true pokud tam jsou loopy
pools :-
  cols(Width), rows(Height),
	X_Max is Width-1,	
	Y_Max is Height-1,
	between(1,Y_Max,Y),
		between(1,X_Max,X),
			(isSquare(X,Y),!;fail).	

sumNumbers([],Sum) :-
  Sum=0.
sumNumbers([(_,_,Z)|T],Sum):-
  sumNumbers(T,S),
  Sum is ( S + Z ).

tooManyBlacks :-
	%sezenu si seznam cernych poli
	setof((G,H), black(G,H), BlackOnes),
			%write('1'),
	%sezenu si seznam cisel na hraci plose
	setof((K,J,Z), field(K,J,Z), NumberList),
	%necham si udelat soucet cisel na hraci plose
	sumNumbers(NumberList,CC),
			%write('Sum:'),write(CC),nl,
	%ziskam pocet cernych poli
	length(BlackOnes,LengthOfBlack),
	%velikost hraci plochy
	cols(Width), rows(Height),
	(Size is Width*Height),
			%write('Velikost:'),write(Size),nl,
	%vim ze tam muze byt celkem Size - SoucetCisel cernych poli
	SpaceForBlack is Size - CC,
			%write('Space for black:'),write(SpaceForBlack),nl,
			%write('Cernych tam je:'),write(LengthOfBlack),nl,
	SpaceForBlack < LengthOfBlack.

%%%% KONTROLA SITUACE  END  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%% STAOVY PROSTOR  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%zde se provadi kontrola jestli je plocha validni
combi(0) :-
%  printDesk,
%  findall((G,H),isBlack(G,H),Pole),write('black '),write(Pole), nl,
%  ,findall((R,S),tmp_white(R,S),Pole2),write('white '),write(Pole2),nl
%  ,
  ( %if reseni je ok
    (checkBlacks,checkFields)
  , %then 
    printDesk
  ; %else
    true
  )
  .

/*
combi(Level) :-
  pools,!;
  level2Coors(Level,[X,Y]),
  ( %if
    (black(X,Y);white(X,Y);field(X,Y,_))
  , %then
    NextLevel is Level -1,
    combi(NextLevel)
  ; %else
    NextLevel is Level -1,
    assert(tmp_black(X,Y)),combi(NextLevel),retract(tmp_black(X,Y)),fail;
    assert(tmp_white(X,Y)),combi(NextLevel),retract(tmp_white(X,Y))
  )
  .
*/


combi(Level) :-
  pools,!;
  tooManyBlacks,!;
  level2Coors(Level,[X,Y]),
%  write('Level atd: '),write((Level,[X,Y])),nl,
  (black(X,Y);white(X,Y);field(X,Y,_)),
  NextLevel is Level -1,
  combi(NextLevel)
  .

%pokud je tam cerna nebo bila napevno
combi(Level) :-
  pools,!;
  tooManyBlacks,!;
  level2Coors(Level,[X,Y]),
%  write('Level atd: '),write((Level,[X,Y])),nl,
  NextLevel is Level -1,
  assert(tmp_black(X,Y)),combi(NextLevel),retract(tmp_black(X,Y)),
  assert(tmp_white(X,Y)),combi(NextLevel),retract(tmp_white(X,Y))
  .


solve :-
  % inicializace
  %oznaci kolem jednicek 
  mark(1),
  cols(Width), rows(Height),
  Depth is Width*Height,
%  printDesk, 
  combi(Depth).
%%%% STAOVY PROSTOR  END  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%% VYSTUP  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
printField(X,Y) :-
  isOutOfY(Y) -> !;
  isOutOfX(X) -> (Xnext is 1, Ynext is Y+1, nl, printField(Xnext,Ynext));
  field(X,Y,Z) -> write(Z), write(' '), Xnext is X+1, printField(Xnext,Y);
  isBlack(X,Y) -> (write('# '), Xnext is X+1, printField(Xnext,Y));
  isWhite(X,Y) -> (write('_ '), Xnext is X+1, printField(Xnext,Y));
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
		isBlack(X,Y) -> (
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
		setof((G,H), isBlack(G,H), BlackOnes),	% najde vsechny cerne
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
		


/*
	pokud je pole bile,
	nahazi do pameti BILE sousedy daneho pole!
*/
getWhiteNeighbours(X,Y) :-
%		write([X,Y]),nl,
		(
			not(neighbour(X,Y)), 
			not(isBlack(X,Y)), 
			not(field(X,Y,_)), 
			not(isOutOfX(X)),
			not(isOutOfY(Y))
		) ->
			(
				assert(neighbour(X,Y)),

				Xr is X+1, Xl is X-1,
				Yu is Y-1, Yd is Y+1,
%				write('dostavam: '), write([X,Y]),nl,
				(	% vsechny okolni prohledame
					getWhiteNeighbours(Xl, Y),fail;
					getWhiteNeighbours(Xr, Y),fail;
					getWhiteNeighbours(X, Yu),fail;
					getWhiteNeighbours(X, Yd)
				)
			)
			,fail
		.

/*
	Overeni pro jedno jednotlive pole
*/
checkFieldWhiteNeighbours(X,Y, Count) :-

		field(X,Y,_) -> 		% pokud neni cislo, vratime se vec, ale to by nemelo nastat
		resetNeighbours,		% reset sousedu, aby byli v pameti sousedi jen pro toto cislo
		
		Xr is X+1, Xl is X-1,
		Yu is Y-1, Yd is Y+1,

%		write('cekuju '),write(X),write('-'), write(Y), nl,
		(	% vsechny okolni prohledame
			getWhiteNeighbours(Xl, Y),fail;
			getWhiteNeighbours(Xr, Y),fail;
			getWhiteNeighbours(X, Yu),fail;
			getWhiteNeighbours(X, Yd),fail;

			findall((G,H), neighbour(G,H), Neighbours),
%			write('bila pole: '),
%			write(Neighbours),nl,
			
			length(Neighbours, NeighCnt),
			NeighReq is Count-1,	% bilych poli okolo - tzn. cislo-1!
			NeighReq = NeighCnt
		)
		.


checkFieldlist([]) :- !.
checkFieldlist([(X,Y,Z)|T]) :-
	checkFieldWhiteNeighbours(X,Y,Z) -> (checkFieldlist(T));
	false
	.


/*
	Kontrola, jestli ma kazde cislo okolo sebe PRAVE takovy pocet bilych mist 
	jako je dane cislo
*/
checkFields :- 
		findall((G,H,I), field(G,H,I), Fields),	% najde vsechny cerne
		checkFieldlist(Fields).

		
%%%%% /KONTROLA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
  unix(args(Args)),
  nth1(5,Args,FileName),
%  write('kontrolni vypis jmena souboru: "'),write(FileName),write('"'),nl,
  open(FileName, read, FileStream),
  loadGrid(FileStream),
  close(FileStream),
%  rows(RowsNum),write('Rows: '),write(RowsNum),nl,
%  cols(ColsNum),write('Cols: '),write(ColsNum),nl,
  solve,
  nl,write('Konec vsech reseni.'),nl
  .

% /*
prolog :- 
  main.
% */
