:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(aggregate)).

% Main predicate to run the menu
play :- 
    write('--------------------------------------'), nl,
    write('|       Welcome to Ayu Game          |'), nl,
    write('--------------------------------------'), nl,
    choose_board_size(BoardSize),
    create_initial_board(BoardSize, Board),
    write('Game initialized!'), nl,
    game_cycle(game(Board, [human, human], red)).

% Board size selection menu
choose_board_size(BoardSize) :- 
    write('--------------------------------------'), nl,
    write('| Choose Board Size                  |'), nl,
    write('| 1. 11x11 (Original Size)           |'), nl,
    write('| 2. 13x13                           |'), nl,
    write('| 3. 9x9                             |'), nl,
    write('| Choose an option (1-3):            |'), nl,
    write('--------------------------------------'), nl,
    safe_read(Choice),
    handle_board_size_choice(Choice, BoardSize).

handle_board_size_choice(1, 11).
handle_board_size_choice(2, 13).
handle_board_size_choice(3, 9).
handle_board_size_choice(_, BoardSize) :-
    write('Invalid choice. Please try again.'), nl,
    choose_board_size(BoardSize).

% Safe input handling
safe_read(Input) :- 
    catch(read(Input), _, (write('Invalid input. Please try again.'), nl, fail)).

% Create initial boards
create_initial_board(11, Board) :- 
    Board = [
        [red, empty, red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red, empty, red]
    ].

create_initial_board(13, Board) :- 
    Board = [
        [red, empty, red, empty, red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red, empty, red, empty, red]
    ].

create_initial_board(9, Board) :- 
    Board = [
        [red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red],
        [empty, white, empty, white, empty, white, empty, white, empty],
        [red, empty, red, empty, red, empty, red, empty, red]
    ].

% Game loop
game_cycle(game(Board, Players, CurrentPlayer)) :- 
    display_game(game(Board, Players, CurrentPlayer)),
    play_turn(game(Board, Players, CurrentPlayer), NewGameState),
    game_cycle(NewGameState).

% Play turn for a given player
play_turn(game(Board, [human, human], CurrentPlayer), game(NewBoard, [human, human], NextPlayer)) :-
    write('Current player: '), write(CurrentPlayer), nl,
    write('Select the piece you want to move (Row Col): '), nl,
    safe_read(StartPos),
    valid_piece(Board, CurrentPlayer, StartPos),
    write('Select the destination (Row Col): '), nl,
    safe_read(EndPos),
    valid_move(Board, EndPos),
    write('Valid move!'), nl,
    apply_move(Board, StartPos, EndPos, CurrentPlayer, NewBoard),
    next_player(CurrentPlayer, NextPlayer).

% Safe input handling (ensures input is in correct format)
safe_read((Row, Col)) :- 
    catch(read(Row-Col), _, (write('Invalid input. Please try again.'), nl, fail)).

% Valid piece selection check (must belong to the current player)
valid_piece(Board, Player, (Row, Col)) :- 
    valid_position(Board, Row, Col),
    nth0(Row, Board, RowList),
    nth0(Col, RowList, Piece),
    Piece == Player.

valid_position(Board, Row, Col) :-
    length(Board, Rows),
    length(Board, Cols),
    Row >= 0, Row < Rows, Col >= 0, Col < Cols.

% Valid destination check (must be empty)
valid_move(Board, (Row, Col)) :- 
    valid_position(Board, Row, Col),
    nth0(Row, Board, RowList),
    nth0(Col, RowList, empty),
    !. % Only succeed if the destination is empty

% Apply the move: move a piece from StartPos to EndPos
apply_move(Board, (StartRow, StartCol), (EndRow, EndCol), Player, NewBoard) :-
    % Replace the Start position with empty
    nth0(StartRow, Board, StartRowList, TempRows1),
    replace_in_list(StartCol, StartRowList, empty, NewStartRowList),
    nth0(StartRow, TempBoard, NewStartRowList, TempRows1),

    % Replace the End position with the player's piece
    nth0(EndRow, TempBoard, EndRowList, TempRows2),
    replace_in_list(EndCol, EndRowList, Player, NewEndRowList),
    nth0(EndRow, NewBoard, NewEndRowList, TempRows2).

% Helper predicate to replace an element at a specific index in a list
replace_in_list(Index, List, NewElem, NewList) :-
    nth0(Index, List, _, TempList),
    nth0(Index, NewList, NewElem, TempList).

% Get next player
next_player(red, white).
next_player(white, red).

% Display the game board
display_game(game(Board, _, _)) :- 
    nl, write('--------------------------------------'), nl,
    write('Current Board: '), nl,
    display_board(Board).

% Display the board
display_board(Board) :- 
    length(Board, MaxRows),
    write('     '),
    forall(between(1, MaxRows, Col), print_column_number(Col)),
    nl,
    create_separator(MaxRows),
    display_rows(Board, 1),
    create_separator(MaxRows).

% Print column number
print_column_number(Col) :- 
    (Col < 10 -> write(' '), write(Col), write('  ') ; write(Col), write('  ')).

% Create separator for rows
create_separator(MaxColumns) :- 
    SeparatorLength is MaxColumns * 4 + 3, 
    write('   '), 
    forall(between(1, SeparatorLength, _), write('-')), nl.

% Display rows of the board
display_rows([], _).
display_rows([Row|Rest], RowNum) :- 
    (RowNum < 10 -> write(' '), write(RowNum), write(' |') ; write(RowNum), write(' |')),
    maplist(display_cell, Row),
    write(' |'), nl,
    NewRowNum is RowNum + 1,
    display_rows(Rest, NewRowNum).

% Display each cell (empty, red, or white)
display_cell(empty) :- write('  . ').
display_cell(red) :- write('  R ').
display_cell(white) :- write('  W ').
