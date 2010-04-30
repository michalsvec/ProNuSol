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
isOutOfX(X) :- 
  cols(C), X > C.
isOutOfY(Y) :-
  rows(R), Y > R.
%%%% POMOCNE PREDIKATY  END  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%% KONTROLA SITUACE  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isBlack(X,Y) :-
  tmp_black(X,Y);black(X,Y).

isWhite(X,Y) :-
  tmp_white(X,Y);white(X,Y). 
%%%% KONTROLA SITUACE  END  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%% STAOVY PROSTOR  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
coo(Index,CCC) :- 
  cols(W),
  X is ((Index+1) mod W)+1,
  Y is ((Index+1) // W),
  [X,Y] = CCC.


combi(Level) :-
  Level > 0,
  coo(Level,[X,Y]),
  %nastav pole na souradnicich X,Y
%  (Level ==1,write('====================='),nl;true),
%  write('Uroven je '), write(Level), write(' '), write([X,Y]),nl,
  NextLevel is Level -1,
  assert(tmp_black(X,Y)),combi(NextLevel),retract(tmp_black(X,Y)),
  assert(tmp_white(X,Y)),combi(NextLevel),retract(tmp_white(X,Y)),!
    ;
    findall((G,H),tmp_black(G,H),Pole)%,write('cerne '),write(Pole), nl
    ,findall((R,S),tmp_white(R,S),Pole2)%,write('white '),write(Pole2), nl,nl
  .

combinuj(Cols,Rows) :-
  assert(tmp_black(-1,-1)),retract(tmp_black(-1,-1)),
  assert(tmp_white(-1,-1)),retract(tmp_white(-1,-1)),
  (Depth is Cols*Rows),
  not((
  between(1, Rows, Y),
    between(1, Cols, X),
    Coor = [X, Y],
%    write('souradnice: '),write(Coor),nl,
    fail
  )),
  combi(Depth).
%%%% STAOVY PROSTOR  END  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%% VYSTUP  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printDesk(X,Y) :-
%		write([X,Y]),
		isOutOfY(Y) -> fail,!;
		isOutOfX(X) -> (nl, Xnext is 1, Ynext is Y+1, nl, printDesk(Xnext,Ynext));
		number(X,Y,Z) -> write(Z), write(' '), Xnext is X+1, printDesk(Xnext,Y);
		white(X,Y) -> (write('_ '), Xnext is X+1, printDesk(Xnext,Y));
		black(X,Y) -> (write('# '), Xnext is X+1, printDesk(Xnext,Y))
		.
		%X2 is X+1, printDesk(X2,Y).

printDesk :-
  printDesk(1,1).
 %%%% VYSTUP  END  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   

main :-
  unix(args(Args)),
  nth1(5,Args,FileName),
  write('kontrolni vypis jmena souboru: "'),write(FileName),write('"'),nl,
  open(FileName, read, FileStream),
  loadGrid(FileStream),
  close(FileStream),
  rows(RowsNum),write('Rows: '),write(RowsNum),nl,
  cols(ColsNum),write('Cols: '),write(ColsNum),nl,
  combinuj(ColsNum,RowsNum),
%  combinuj(4,5),
  write('Aplikace KONCI >>OK<<!'),nl
  .

% /*
prolog :- 
  main.
% */
