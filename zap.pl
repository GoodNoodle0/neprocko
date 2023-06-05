
won_row([X|Xs],A):-maplist(=(A),X),!;won_row(Xs,A).

item_on_index([X|_],0,X):-!.
item_on_index([_|Xs],N,Item):-N>0,
                            Ns is N - 1, 
                            item_on_index(Xs,Ns,Item).

won_file(_,[],_):-!.

won_file(N,[X|Xs],A):-
    item_on_index(X,N,A),
    won_file(N,Xs,A).

won_any_file(N,X,A):-won_file(N,X,A);
                    Ns is N - 1, Ns >= 0, 
                    won_any_file(Ns,X,A),!.

won_diagonal1([],_,_):-!.
won_diagonal1([X|Xs],A,Start):-item_on_index(X,Start,A),
                                TStart is Start + 1,
                                won_diagonal1(Xs,A,TStart).

won_diagonal2([],_,_):-!.
won_diagonal2([X|Xs],A,N):-item_on_index(X,N,A),
                                Ns is N - 1,
                                won_diagonal2(Xs,A,Ns).

create_empty_grid(Depth,[],Depth):-!.
create_empty_grid(N,[Empty_grid|Es],Depth):-
                                    length(Empty_grid,N),
                                    maplist(=('-'),Empty_grid),
                                    DepthT is Depth + 1,
                                    DepthT =< N,
                                    create_empty_grid(N,Es,DepthT).

replace([_|Xs],_,Index,Index,Item,[Item|Xs]):-!.
replace([X|Xs],N,Index,Depth,Item,[X|Ys]):- 
                                DepthT is Depth + 1,
                                DepthT < N,
                                replace(Xs,N,Index,DepthT,Item,Ys).

is_legal(State,Index):-item_on_index(State,Index,'-').


count([],_,0).
count([X|T],X,Y):-count(T,X,Z),Y is Z + 1.
count([F|T],X,Y):-F\=X,count(T,X,Y).

draw_row(R):-count(R,1,X),X\=0,count(R,-1,Y),Y\=0,!.

draw_rows([]).
draw_rows([R|Rs]):-draw_row(R),draw_rows(Rs).


count_file([],_,0,_,_).
count_file([X|A],Item,C,Index,Size):-nth0(Index, X, Item),count_file(A,Item,Z,Index,Size),C is Z + 1.
count_file([X|A],Item,C,Index,Size):-nth0(Index,X,K),K\=Item,count_file(A,Item,C,Index,Size).

draw_file(X,Size,Index):-count_file(X,1,C,Index,Size),count_file(X,-1,K,Index,Size),C\=0,K\=0,!.

draw_files(_,_,0):-!.
draw_files(X,Size,Index):-draw_file(X,Size,Index),Indexs is Index -1,Indexs >=0,draw_files(X,Size,Indexs).

count_diagonal([],_,_,0).
count_diagonal([X|Xs],Start,A,Count):-item_on_index(X,Start,A),
                    Starts is Start+1,
                    count_diagonal(Xs, Starts, A, C),
                    Count is C + 1.
count_diagonal([X|Xs],Start,A,Count):-item_on_index(X,Start,B),A\=B,
                    Starts is Start+1,
                    count_diagonal(Xs, Starts, A, Count).

count_diagonal2([],_,_,-1).
count_diagonal2([X|Xs],End,A,Count):-item_on_index(X,End,A),
                    Ends is End - 1,
                    count_diagonal2(Xs, Ends, A, C),
                    Count is C + 1.
count_diagonal2([X|Xs],End,A,Count):-item_on_index(X,End,B),A\=B,
                    Ends is End - 1,
                    count_diagonal2(Xs, Ends, A, Count).

draw_diagonals(X,Size):-count_diagonal(X,0,1,C),count_diagonal(X,0,-1,K),K\=0,C\=0,!;(Sizes is Size - 1,
                        count_diagonal(X,Sizes,1,C),count_diagonal(X,Sizes,-1,K),K\=0,C\=0,!).

draw(X,Size):-draw_diagonals(X,Size),S is Size -1,draw_files(X,Size,S),draw_rows(X).

x_has_won(State,N):-(won_any_file(N,State,1),!);
                    (won_diagonal1(State,1,0),!);
                    (Ns is N -1,won_diagonal2(State,1,Ns),!);
                    (won_row(State,1),!).

o_has_won(State,N):-(won_any_file(N,State,-1),!);
                    (won_diagonal1(State,-1,0),!);
                    (Ns is N -1,won_diagonal2(State,-1,Ns),!);
                    (won_row(State,-1),!).

utility(State,N,1):-x_has_won(State,N),!.
utility(State,N,-1):-o_has_won(State,N),!.
utility(_,_,0).

make_move_player(Old_state,X-Y,New_state,A,N):- \+ utility(Old_state,N,-1),item_on_index(Old_state,Y,Row),
                                                is_legal(Row,X),
                                                replace(Row,N,X,0,A,NewRow),
                                                replace(Old_state,N,Y,0,NewRow,New_state),!.

try(X-Y,Board,NewState,Colour,Size):-
    Nx1 is X + 1,
    Nx2 is X - 1,
    Ny1 is Y + 1,
    Ny2 is Y - 1,
    (make_move_player(Board,Nx1-Y,NewState,Colour,Size);
    make_move_player(Board,Nx2-Y,NewState,Colour,Size);
    make_move_player(Board,X-Ny1,NewState,Colour,Size);
    make_move_player(Board,X-Ny2,NewState,Colour,Size);
    make_move_player(Board,Nx1-Ny1,NewState,Colour,Size);
    make_move_player(Board,Nx2-Ny1,NewState,Colour,Size);
    make_move_player(Board,Nx1-Ny2,NewState,Colour,Size);
    make_move_player(Board,Nx2-Ny2,NewState,Colour,Size)).

generate_moves_optimized(Board,X-Y,New_state,Colour,N):- \+ utility(Board,N,-1),item_on_index(Board,Y,Row),
                                    (item_on_index(Row,X,1);item_on_index(Row,X,-1)),
                                    try(X-Y,Board,New_state,Colour,N).

generate_moves_optimized(Board,0-Y,New_state,Colour,N):-Ys is Y + 1,
                                    Ys < N,
                                    generate_moves_optimized(Board,0-Ys,New_state,Colour,N).

generate_moves_optimized(Board,X-Y,New_state,Colour,N):-Xs is X + 1,
                                    Xs < N,
                                    generate_moves_optimized(Board,Xs-Y,New_state,Colour,N).

grid_empty([]).
grid_empty([H|T]):- empty_row(H),grid_empty(T).

empty_row([]).
empty_row([H|T]):-H == '-',empty_row(T).

minimax(A,_,1,_,N,B):-grid_empty(A),C is div(N,2),make_move_player(A,C-C,B,1,N),print("funguju: "),print(C).

minimax(State,Depth,Colour,Val,N,Best_state):-
   % utility(State,N,0),
    Depth >0,
   % bagof(New_state, make_move(State,0-0,New_state, Colour,N), Next_state_list),
    bagof(New_state, generate_moves_optimized(State,0-0,New_state,Colour,N), Next_state_list1),
    sort(Next_state_list1, Next_state_list),    
    New_colour is -1*Colour,
    New_depth is Depth - 1,
    best(Next_state_list,New_colour,N,New_depth,Best_state,Val),!.

minimax(State,_,_,V,N,_):-utility(State,N,V).%,print(State),nl.



best([New_state],Colour,N ,Depth ,New_state, Value):-
    minimax(New_state,Depth,Colour,Value,N,_),!.



best([New_state|Rest],Colour,N ,Depth ,BestState, BestValue):-
   % print(Colour),nl,print(Depth),nl,print("stav z best: " ),print(New_state),nl,
    minimax(New_state,Depth,Colour,Value1,N,_),
    best(Rest,Colour,N, Depth ,New_state2, Value2),
    better_state(Colour,New_state,Value1,New_state2,Value2,BestState,BestValue).

better_state(Colour,New_state,Value1,_,Value2,New_state,Value1):-
   % print(Colour),nl,print("stary stav: "),print(O),nl,print(Value1),nl,print(Value2),nl,print(New_state),nl,
  % print(Colour),nl,print(Value2),print(Value1),nl,print("stav z best: " ),print(New_state),nl,

    (Colour == 1,Value1 < Value2,!);
    (Colour == -1,Value1 > Value2,!).

better_state(_,_,_,New_state2,Value2,New_state2,Value2).



print_grid([]).
print_grid([H|T]) :- print_each_element(H), nl, print_grid(T).

print_each_element([]):-!.
print_each_element([H|T]):-H == 1, write(x),print_each_element(T),!.
print_each_element([H|T]):-H == -1, write(o),print_each_element(T),!.
print_each_element([_|T]):-write('-'),print_each_element(T),!.



play_tic_tac_toe(Size,Board,_):-Board \= [],x_has_won(Board,Size),write("Počítač vyhrál :/"),nl,!.
play_tic_tac_toe(Size,Board,_):-Board \= [],o_has_won(Board,Size),write("Vyhrál jste :)"),nl,!.
play_tic_tac_toe(Size,Board,_):-Board \= [],draw(Board,Size),write("Remíza"),nl,!.



play_tic_tac_toe(Size,[],-1):- create_empty_grid(Size,New_grid,0),
                            print_grid(New_grid),
                            play_tic_tac_toe(Size, New_grid,1).

play_tic_tac_toe(Size,Board,1):- 
                            (minimax(Board,4,1,1,Size,New_state);minimax(Board,4,1,0,Size,New_state);minimax(Board,4,1,-1,Size,New_state)),
                            write("ted hraje AI:"),nl,
                            print_grid(New_state),
                            play_tic_tac_toe(Size,New_state,-1).

play_tic_tac_toe(Size,Board,-1):-read(X),read(Y),
                            make_move_player(Board,X-Y,State_1,-1,Size),
                            write("Tohle je váš tah: "),nl,
                            print_grid(State_1),
                            play_tic_tac_toe(Size,State_1,1).


test_draw1:-draw_files([[1,1,-1,-1],[-1,-1,1,1]],4,3). %má být true
test_draw2:-draw_files([[1,1,1,-1],[-1,-1,1,1],[-1,'-',1,1],[-1,-1,1,1]],4,3). % false

test1(N):-minimax([['-','-','-','-'],['-',-1,-1,'-'],['-',-1,-1,1],[1,-1,1,1]],4,1,0,4,N).
test2(N):-minimax([['-','-','-','-'],['-','-',-1,'-'],['-','-',-1,'-'],['-',1,-1,1]],4,1,0,4,N).
test3(N):-minimax([['-',1,-1,'-'],['-','-',-1,'-'],['-','-',-1,'-'],['-','-','-',1]],2,1,0,4,N).
test4(N):-minimax([['-',1,-1],['-','-',-1],['-','-','-']],2,1,0,3,N).
test5(N):-minimax([['-','-','-'],[1,'-',-1],['-','-',-1]],3,1,0,3,N).
test6(N):-minimax([['-','-','-'],[1,1,-1],[1,'-',-1]],3,1,1,3,N).

