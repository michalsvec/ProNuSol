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
level2Coors(Index,C) :- 
  cols(W),
  X is ((Index+1) mod W)+1,
  Y is ((Index+1) // W),
  [X,Y] = C.

coors2Level([X,Y],C) :- 
  cols(W),
  C is (Y-1)*W+X.

combi(Level) :-
  Level > 0,
  level2Coors(Level,[X,Y]),
  %nastav pole na souradnicich X,Y
%  (Level ==1,write('====================='),nl;true),
%  write('Uroven je '), write(Level), write(' '), write([X,Y]),nl,
  NextLevel is Level -1,
  assert(tmp_black(X,Y)),combi(NextLevel),retract(tmp_black(X,Y)),
  assert(tmp_white(X,Y)),combi(NextLevel),retract(tmp_white(X,Y)),!
    ; %else
    %pouze vypis
    findall((G,H),tmp_black(G,H),Pole)%,write('cerne '),write(Pole), nl
    ,findall((R,S),tmp_white(R,S),Pole2)%,write('white '),write(Pole2),nl
  .

solve(Cols,Rows) :-
  % inicializace
  assert(tmp_black(-1,-1)),retract(tmp_black(-1,-1)),
  assert(tmp_white(-1,-1)),retract(tmp_white(-1,-1)),
  assert(black(-1,-1)),retract(black(-1,-1)),
  assert(white(-1,-1)),retract(white(-1,-1)),

  %test
  coors2Level([3,2],I), write('index: '), write(I), nl,

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
  isOutOfY(Y) -> fail,!;
  isOutOfX(X) -> (Xnext is 1, Ynext is Y+1, nl, printField(Xnext,Ynext));
  field(X,Y,Z) -> write(Z), write(' '), Xnext is X+1, printField(Xnext,Y);
  isWhite(X,Y) -> (write('_ '), Xnext is X+1, printField(Xnext,Y));
  isBlack(X,Y) -> (write('# '), Xnext is X+1, printField(Xnext,Y));
  (write('u '), Xnext is X+1, printField(Xnext,Y))
  .

printDesk :-
  write('=== reseni je: ==='),nl,
  printField(1,1).
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
  nl,
  solve(ColsNum,RowsNum),
  printDesk,
  write('Aplikace KONCI >>OK<<!'),nl
  .

% /*
prolog :- 
  main.
% */
