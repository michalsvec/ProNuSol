/**
 * FPR 2010 Nurikabe Pavel
 *
 */

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

coo(Index,CCC) :- 
  cols(W),
  X is ((Index+1) mod 2)+1,
  Y is ((Index+1) // 2),
  [X,Y] = CCC.
  

combi(Level) :-
  Level > 0,
  coo(Level,[X,Y]),
  %nastav pole na souradnicich X,Y
  (Level ==1,write('====================='),nl;true),
%  write('Uroven je '), write(Level), write(' '), write([X,Y]),nl,
  NextLevel is Level -1,
  assert(black(X,Y)),combi(NextLevel),retract(black(X,Y)),
  assert(white(X,Y)),combi(NextLevel),retract(white(X,Y)),!
    ;
    findall((G,H),black(G,H),Pole),write('cerne '),write(Pole), nl
    ,findall((R,S),white(R,S),Pole2),write('white '),write(Pole2), nl,nl

  .


combinuj(Cols,Rows) :-
  assert(white(-1,-1)),retract(white(-1,-1)),
  (Depth is Cols*Rows),
  not((
  between(1, Rows, Y),
    between(1, Cols, X),
    Coor = [X, Y],
%    write('souradnice: '),write(Coor),nl,
    fail
  )),
  combi(Depth).


main :-
  unix(args(Args)),
  nth1(5,Args,FileName),
  write('kontrolni vypis jmena souboru: "'),write(FileName),write('"'),nl,
  open(FileName, read, FileStream),
  loadGrid(FileStream),
  close(FileStream),
  rows(RowsNum),write('Rows: '),write(RowsNum),nl,
  cols(ColsNum),write('Cols: '),write(ColsNum),nl,
  combinuj(2,2),
  write('Aplikace KONCI >>OK<<!'),nl
  .

% /*
prolog :- 
  main.
% */
