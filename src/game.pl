:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(aggregate)).

% Main predicate to run the menu
play :- 
    write('--------------------------------------'), nl,
    write('|       Welcome to Ayu Game          |'), nl,
    write('--------------------------------------'), nl,
    choose_board_size(BoardSize),
    choose_players(Players),
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

% Player type selection
choose_players(Players) :-
    write('Choosing Player 1:'), nl,
    choose_player_type(Player1),
    write('Choosing Player 2:'), nl,
    choose_player_type(Player2),
    Players = [Player1, Player2].

choose_player_type(Player) :-
    write('--------------------------------------'), nl,
    write('| Choose Player Type                 |'), nl,
    write('| 1. Human                           |'), nl,
    write('| 2. PC - level 1                    |'), nl,
    write('| 3. PC - level 2                    |'), nl,
    write('| Choose an option (1-3):            |'), nl,
    write('--------------------------------------'), nl,
    safe_read(Choice),
    handle_player_choice(Choice, Player).

handle_player_choice(1, human).
handle_player_choice(2, pc_level1).
handle_player_choice(3, pc_level2).
handle_player_choice(_, Player) :-
    write('Invalid choice. Please try again.'), nl,
    choose_player_type(Player).

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
        [empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [red, empty, red, empty, red, empty, white, empty, white]
    ].

% Main game cycle with the new win condition
game_cycle(game(Board, Players, CurrentPlayer)) :-
    display_game(game(Board, Players, CurrentPlayer)),
    (game_over(Board, CurrentPlayer) -> 
        (write('Player '), write(CurrentPlayer), write(' wins!'), nl) ;
        (play_turn(game(Board, Players, CurrentPlayer), NewGameState),
        game_cycle(NewGameState))).

% Play turn for a given player
play_turn(game(Board, [human, human], CurrentPlayer), game(NewBoard, [human, human], NextPlayer)) :-
    get_move(Board, CurrentPlayer, StartPos, EndPos),
    move(game(Board, [human, human], CurrentPlayer), (StartPos, EndPos), game(NewBoard, [human, human], NextPlayer)),
    next_player(CurrentPlayer, NextPlayer).

% Get the piece to move and the destination
get_move(Board, CurrentPlayer, StartPos, EndPos) :-
    repeat,
    write('Select the piece you want to move (Row,Col): '), nl,
    safe_read(StartPos),
    (valid_piece(Board, CurrentPlayer, StartPos) -> true ; (write('Invalid piece selection. Please try again.'), nl, fail)),
    write('Select the destination (Row,Col): '), nl,
    safe_read(EndPos),
    EndPos = (EndRow, EndCol),
    EndRow0 is EndRow - 1,
    EndCol0 is EndCol - 1,
    (valid_position(Board, EndRow0, EndCol0) -> true ; (write('Invalid destination. Please try again.'), nl, fail)).

% Validate the move and apply it
move(game(Board, Players, CurrentPlayer), (StartPos, EndPos), game(NewBoard, Players, NextPlayer)) :-
    StartPos = (StartRow, StartCol),
    EndPos = (EndRow, EndCol),
    valid_move(Board, (StartRow, StartCol), (EndRow, EndCol), CurrentPlayer),
    apply_move(Board, StartPos, EndPos, CurrentPlayer, NewBoard).

% Safe input handling (ensures input is in correct format)
safe_read((Row, Col)) :- 
    catch(read(Row-Col), _, (write('Invalid input. Please try again.'), nl, fail)),
    Row > 0, Col > 0.

% Valid piece selection check (must belong to the current player)
valid_piece(Board, Player, (Row, Col)) :- 
    Row0 is Row - 1,
    Col0 is Col - 1,
    valid_position(Board, Row0, Col0),
    nth0(Row0, Board, RowList),
    nth0(Col0, RowList, Piece),
    (Piece == Player -> true ; 
    (write('Invalid piece selection. Please select a piece that belongs to you.'), nl, fail)).

valid_position(Board, Row, Col) :-
    length(Board, Rows),
    nth0(0, Board, FirstRow),
    length(FirstRow, Cols),
    Row >= 0, Row < Rows, Col >= 0, Col < Cols.

valid_distance((Row, Col), (StartRow, StartCol)) :-
    abs(Row - StartRow) =< 1,
    abs(Col - StartCol) =< 1.

% Get all valid moves for a player's piece from (StartRow, StartCol)
valid_moves(Board, Player, (StartRow, StartCol), Moves) :-
    findall((StartRow, StartCol, EndRow, EndCol),
            (between(-1, 1, DR), between(-1, 1, DC), % Check adjacent cells
             (DR \= 0 ; DC \= 0), % Exclude staying in place
             EndRow is StartRow + DR,
             EndCol is StartCol + DC,
             valid_move(Board, (StartRow, StartCol), (EndRow, EndCol), Player)),
            Moves).

% Check for valid moves
valid_move(Board, (StartRow, StartCol), (EndRow, EndCol), Player) :-
    Row0 is EndRow - 1,
    Col0 is EndCol - 1,
    StartRow0 is StartRow - 1,
    StartCol0 is StartCol - 1,
    valid_position(Board, Row0, Col0),        % Destination is within the board
    nth0(Row0, Board, RowList),              % Get the row of the destination
    nth0(Col0, RowList, empty),              % Ensure the destination cell is empty
    valid_distance((Row0, Col0), (StartRow0, StartCol0)), % Within move distance
    adjacent_same_color(Board, (Row0, Col0), Player, (StartRow0, StartCol0)). % Adjacent to same color

% Check if the destination cell is adjacent to at least one cell with the same color
adjacent_same_color(Board, (Row, Col), Color, (StartRow, StartCol)) :-
    % Collect valid neighbors
    findall((NRow, NCol), neighboring_position(Board, (Row, Col), (NRow, NCol), (StartRow, StartCol)), Neighbors),
    % Ensure at least one neighbor has the same color as the player
    member((NRow, NCol), Neighbors),
    nth0(NRow, Board, NeighborRow),
    nth0(NCol, NeighborRow, NeighborColor),
    NeighborColor == Color.

neighboring_position(Board, (Row, Col), (NRow, NCol), (StartRow, StartCol)) :-
    % Define the relative positions of neighbors
    member((DR, DC), [(-1, 0), (1, 0), (0, -1), (0, 1)]), % Up, Down, Left, Right
    NRow is Row + DR,
    NCol is Col + DC,
    valid_position(Board, NRow, NCol), % Ensure the position is valid
    (NRow \= StartRow ; NCol \= StartCol). % Exclude the start position

% Apply the move: move a piece from StartPos to EndPos
apply_move(Board, (StartRow, StartCol), (EndRow, EndCol), Player, NewBoard) :-
    StartRow0 is StartRow - 1,
    StartCol0 is StartCol - 1,
    EndRow0 is EndRow - 1,
    EndCol0 is EndCol - 1,
    % Replace the start position with empty
    nth0(StartRow0, Board, StartRowList, TempRows1),
    replace_in_list(StartCol0, StartRowList, empty, NewStartRowList),
    nth0(StartRow0, TempBoard, NewStartRowList, TempRows1),
    % Replace the end position with the player's piece
    nth0(EndRow0, TempBoard, EndRowList, TempRows2),
    replace_in_list(EndCol0, EndRowList, Player, NewEndRowList),
    nth0(EndRow0, NewBoard, NewEndRowList, TempRows2).

% Check if all pieces of a player are adjacent to each other
game_over(Board, Player) :-
    findall((Row, Col), piece_position(Board, Player, (Row, Col)), Positions),
    (Positions = [] -> fail ; Positions = [FirstPos|_], check_all_adjacent(Positions, [FirstPos], [FirstPos], Positions)).

% Find the position of a player's piece on the board
piece_position(Board, Player, (Row, Col)) :-
    nth0(Row, Board, RowList),
    nth0(Col, RowList, Player).

% Check if all positions are adjacent to each other
check_all_adjacent([], _, _, _).
check_all_adjacent(_, [], _, []).
check_all_adjacent(AllPositions, [Pos|Rest], Visited, Unvisited) :-
    findall(Neighbor, (adjacent(Pos, Neighbor), member(Neighbor, AllPositions), \+ member(Neighbor, Visited)), Neighbors),
    append(Neighbors, Rest, NewRest),
    select(Pos, Unvisited, NewUnvisited),
    check_all_adjacent(AllPositions, NewRest, [Pos|Visited], NewUnvisited).

% Check if two positions are adjacent
adjacent((Row, Col), (NRow, NCol)) :-
    member((DR, DC), [(-1, 0), (1, 0), (0, -1), (0, 1)]),
    NRow is Row + DR,
    NCol is Col + DC.

% Helper predicate to replace an element at a specific index in a list
replace_in_list(Index, List, NewElem, NewList) :-
    nth0(Index, List, _, TempList),
    nth0(Index, NewList, NewElem, TempList).

% Get next player
next_player(red, white).
next_player(white, red).

% Display the game board
display_game(game(Board, _, CurrentPlayer)) :- 
    nl, write('--------------------------------------'), nl,
    write('Current Board: '), nl,
    display_board(Board),
    nl, write('Current Player: '), write(CurrentPlayer), nl.

% Display the board with improved aesthetics and consistent formatting
display_board(Board) :-
    length(Board, MaxRows),
    write('    '),  % Top padding for column numbers
    forall(between(1, MaxRows, Col), print_column_number(Col)),
    nl,
    create_top_border(MaxRows),  % Initial top border
    display_rows_with_borders(Board, 1, MaxRows),
    create_top_border(MaxRows).  % Final bottom border

% Print column numbers with consistent spacing
print_column_number(Col) :-
    (Col < 10 -> write('  '), write(Col), write(' ') ; write(' '), write(Col), write(' ')).

% Create top/bottom border
create_top_border(MaxColumns) :-
    write('    +'),
    forall(between(1, MaxColumns, _), write('---+')),
    nl.

% Display rows with borders
display_rows_with_borders([], _, _).  % Base case: No rows left to display
display_rows_with_borders([Row|Rest], RowNum, MaxColumns) :-
    % Print row number with consistent spacing
    (RowNum < 10 -> write('  '), write(RowNum), write(' |') ; write(' '), write(RowNum), write(' |')),
    % Display row cells
    maplist(display_cell_with_border, Row),
    write('\n'),  % End of row
    % Add row separator if there are more rows to follow
    (Rest \= [] -> create_top_border(MaxColumns) ; true),
    % Recurse to display the remaining rows
    NewRowNum is RowNum + 1,
    display_rows_with_borders(Rest, NewRowNum, MaxColumns).

% Display a cell with borders
display_cell_with_border(empty) :- write('   |').
display_cell_with_border(red) :- write(' R |').
display_cell_with_border(white) :- write(' W |').
