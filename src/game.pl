% :- use_module(library(lists)).
% :- use_module(library(apply)).
% :- use_module(library(between)).
% :- use_module(library(aggregate)).
% :- use_module(library(random)).
% :- use_module(library(sets)).

% Main predicate to run the menu
play :- 
    write('--------------------------------------'), nl,
    write('|       Welcome to Ayu Game          |'), nl,
    write('--------------------------------------'), nl,
    choose_board_size(BoardSize),
    choose_players(Players),
    create_initial_board(BoardSize, Board),
    write('Game initialized!'), nl,
    game_cycle(game(Board, Players, red), []).

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

% Safe input handling (ensures input is in correct format)
safe_read((Row, Col)) :- 
    catch(read(Row-Col), _, (write('Invalid input. Please try again.'), nl, fail)),
    Row > 0, Col > 0.

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

% Main game cycle with cycle detection and draw condition
game_cycle(game(Board, Players, CurrentPlayer), PreviousBoards) :-
    display_game(game(Board, Players, CurrentPlayer)),
    (game_over(Board, Winner) -> 
        (write('Player '), write(Winner), write(' wins!'), nl) ;
        (member((Board, CurrentPlayer), PreviousBoards) ->
            write('Game ended in a draw due to repetition.'), nl ;
            (play_turn(game(Board, Players, CurrentPlayer), NewGameState),
            game_cycle(NewGameState, [(Board, CurrentPlayer)|PreviousBoards]))
        )
    ).

% Play turn for a given player
play_turn(game(Board, Players, CurrentPlayer), game(NewBoard, Players, NextPlayer)) :-
    (CurrentPlayer = red -> CurrentPlayerIndex = 1 ; CurrentPlayerIndex = 2),
    nth1(CurrentPlayerIndex, Players, PlayerType),
    write('Player type: '), write(PlayerType), nl,
    % Check if the current player has any valid moves
    (has_valid_moves(Board, CurrentPlayer) ->
        (PlayerType == human ->
            get_move(Board, CurrentPlayer, StartPos, EndPos), 
            move(game(Board, Players, CurrentPlayer), (StartPos, EndPos), game(NewBoard, Players, NextPlayer))
        ; PlayerType == pc_level1 ->
            choose_move(game(Board, Players, CurrentPlayer), 1, (StartPos, EndPos)),
            write('PC Level 1 chooses move from '), write(StartPos), write(' to '), write(EndPos), nl,
            move(game(Board, Players, CurrentPlayer), (StartPos, EndPos), game(NewBoard, Players, NextPlayer))
        ; PlayerType == pc_level2 ->
            choose_move(game(Board, Players, CurrentPlayer), 2, (StartPos, EndPos)),
            write('PC Level 2 chooses move from '), write(StartPos), write(' to '), write(EndPos), nl,
            move(game(Board, Players, CurrentPlayer), (StartPos, EndPos), game(NewBoard, Players, NextPlayer))
        )
    ;   % If no valid moves, the current player wins
        game_over(Board, CurrentPlayer)
    ).


choose_move(game(Board, _, CurrentPlayer), 1, (StartPos, EndPos)) :-
    % Find all pieces of the current player
    findall((Row1, Col1), (piece_position(Board, CurrentPlayer, (Row, Col)), Row1 is Row + 1, Col1 is Col + 1), PlayerPieces),
    % Randomly select one piece
    random_member(StartPos, PlayerPieces),
    % Get valid moves for the selected piece
    StartPos = (StartRow, StartCol),
    Row0 is StartRow - 1,
    Col0 is StartCol - 1,
    valid_moves(Board, CurrentPlayer, (Row0, Col0), Moves0),
    write('Valid moves: '), write(Moves0), nl,
    % Adjust valid moves to 1-based coordinates
    findall((EndRow1, EndCol1), (member((EndRow, EndCol), Moves0), EndRow1 is EndRow + 1, EndCol1 is EndCol + 1, valid_position(Board, EndRow, EndCol)), Moves),
    % Randomly select one move from the valid moves
    random_member(EndPos, Moves).

choose_move(game(Board, _, CurrentPlayer), 2, (BestStartPos, BestEndPos)) :-
    % Find all pieces of the current player
    findall((Row1, Col1), (piece_position(Board, CurrentPlayer, (Row, Col)), Row1 is Row + 1, Col1 is Col + 1), PlayerPieces),
    % Generate all possible moves for each piece
    findall((Score, StartPos, EndPos),
            (member(StartPos, PlayerPieces),
             StartPos = (StartRow, StartCol),
             Row0 is StartRow - 1,
             Col0 is StartCol - 1,
             valid_moves(Board, CurrentPlayer, (Row0, Col0), Moves0),
             findall((EndRow1, EndCol1),
                     (member((EndRow, EndCol), Moves0),
                      EndRow1 is EndRow + 1,
                      EndCol1 is EndCol + 1),
                     Moves),
             member(EndPos, Moves),
             value(Board, CurrentPlayer, (StartPos, EndPos), Score)),
            MovesWithScores),
    % Select the move with the highest score
    max_member((_, BestStartPos, BestEndPos), MovesWithScores).

% sum_list(+List, -Sum)
sum_list(List, Sum) :-
    sum_list(List, 0, Sum).

sum_list([], Acc, Acc).
sum_list([H|T], Acc, Sum) :-
    NewAcc is Acc + H,
    sum_list(T, NewAcc, Sum).

% Define opponent relationship
opponent(red, white).
opponent(white, red).

% Enhanced heuristic evaluation function
value(Board, CurrentPlayer, (StartPos, EndPos), Value) :-
    % Calculate the distance reduction from moving
    EndPos = (EndRow, EndCol),
    StartPos = (StartRow, StartCol),
    Row0 is EndRow - 1,
    Col0 is EndCol - 1,
    StartRow0 is StartRow - 1,
    StartCol0 is StartCol - 1,
    calculate_distance_reduction(Board, CurrentPlayer, (StartRow0, StartCol0), (Row0, Col0), DistanceReduction),
    
    % Count adjacent friendly and opponent pieces
    findall((NRow, NCol),
            neighboring_position(Board, (Row0, Col0), (NRow, NCol), _),
            Neighbors),
    opponent(CurrentPlayer, Opponent),
    include(piece_position(Board, CurrentPlayer), Neighbors, FriendlyNeighbors),
    include(piece_position(Board, Opponent), Neighbors, OpponentNeighbors),
    length(FriendlyNeighbors, FriendlyCount),
    length(OpponentNeighbors, OpponentCount),

    % Assign heuristic value
    Value is DistanceReduction * 10 + FriendlyCount * 5 - OpponentCount * 2.

% Calculate the reduction in distance between friendly groups
calculate_distance_reduction(Board, CurrentPlayer, (StartRow, StartCol), (EndRow, EndCol), Reduction) :-
    findall((Row, Col), piece_position(Board, CurrentPlayer, (Row, Col)), FriendlyPieces),
    % Compute total distance before the move
    total_distance((StartRow, StartCol), FriendlyPieces, OriginalDistance),
    % Simulate the move
    substitute(Board, (StartRow, StartCol), empty, TempBoard),
    substitute(TempBoard, (EndRow, EndCol), CurrentPlayer, NewBoard),
    findall((Row, Col), piece_position(NewBoard, CurrentPlayer, (Row, Col)), NewFriendlyPieces),
    % Compute total distance after the move
    total_distance((EndRow, EndCol), NewFriendlyPieces, NewDistance),
    Reduction is OriginalDistance - NewDistance.

% Compute total Manhattan distance between a piece and a list of positions
total_distance((Row, Col), Positions, TotalDistance) :-
    findall(Distance, 
            (member((PRow, PCol), Positions), manhattan_distance((Row, Col), (PRow, PCol), Distance)),
            Distances),
    sum_list(Distances, TotalDistance).

% Manhattan distance
manhattan_distance((Row1, Col1), (Row2, Col2), Distance) :-
    Distance is abs(Row1 - Row2) + abs(Col1 - Col2).

% Substitute a position in the board with a new value
substitute(Board, (Row, Col), Value, NewBoard) :-
    nth0(Row, Board, RowList, TempBoard),
    replace_in_list(Col, RowList, Value, NewRowList),
    nth0(Row, NewBoard, NewRowList, TempBoard).

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
    apply_move(Board, (StartRow, StartCol), (EndRow, EndCol), CurrentPlayer, NewBoard),
    next_player(CurrentPlayer, NextPlayer).

% Valid piece selection check (must belong to the current player)
valid_piece(Board, Player, (Row, Col)) :- 
    Row0 is Row - 1,
    Col0 is Col - 1,
    valid_position(Board, Row0, Col0),
    nth0(Row0, Board, RowList),
    nth0(Col0, RowList, Piece),
    (Piece == Player -> true ; 
    (write('Invalid piece selection. Please select a piece that belongs to you.'), nl, fail)).

% Check if the position is within the bounds of the board
valid_position(Board, Row, Col) :-
    length(Board, Rows),
    nth0(0, Board, FirstRow),
    length(FirstRow, Cols),
    Row >= 0, Row < Rows, Col >= 0, Col < Cols.

% Check if the position is within the bounds of the board (duplicate removed)
valid_position(Board, Row, Col) :-
    nth0(Row, Board, RowList),
    nth0(Col, RowList, _).  % Checks if the column exists in the row.

% Check if the move distance is valid
valid_distance((Row, Col), (StartRow, StartCol)) :-
    abs(Row - StartRow) =< 1,
    abs(Col - StartCol) =< 1.

% Get all valid moves for a player's piece from (StartRow, StartCol)
valid_moves(Board, Player, (StartRow, StartCol), Moves) :-
    findall((EndRow, EndCol),
            (between(-1, 1, DR), between(-1, 1, DC), % Check adjacent cells
             (DR \= 0 ; DC \= 0), % Exclude staying in place
             EndRow is StartRow + DR,
             EndCol is StartCol + DC,
             valid_position(Board, EndRow, EndCol),        % Destination is within the board
             nth0(EndRow, Board, RowList),              % Get the row of the destination
             nth0(EndCol, RowList, empty),              % Ensure the destination cell is empty
             valid_distance((EndRow, EndCol), (StartRow, StartCol)), % Within move distance
             adjacent_same_color(Board, (EndRow, EndCol), Player, (StartRow, StartCol))),
            RawMoves),
    % Convert list to set to remove duplicates
    list_to_set(RawMoves, Moves).

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

% Check for an adjacent position of the same color
neighboring_position(Board, (Row, Col), (NRow, NCol), Player) :-
    member((DR, DC), [(-1, 0), (1, 0), (0, -1), (0, 1)]),
    NRow is Row + DR,
    NCol is Col + DC,
    valid_position(Board, NRow, NCol),
    nth0(NRow, Board, RowList),
    nth0(NCol, RowList, Player).

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

% Check if the game is over, and return the winner
game_over(Board, Winner) :-
    % Check if all red pieces are connected
    (all_connected(Board, red) -> Winner = red ;
    % Check if all white pieces are connected
    all_connected(Board, white) -> Winner = white ;
    % Otherwise, the game is not over
    \+ has_valid_moves(Board, red) -> Winner = white ;
    % Check if white has no valid moves
    \+ has_valid_moves(Board, white) -> Winner = red ;
    % Otherwise, the game is not over
    fail).

has_valid_moves(Board, Player) :-
    findall((Row, Col), piece_position(Board, Player, (Row, Col)), PlayerPieces),
    member((StartRow, StartCol), PlayerPieces),
    valid_moves(Board, Player, (StartRow, StartCol), Moves),
    Moves \= [].

% Check if all pieces of a specific color are connected
all_connected(Board, Player) :-
    % Find all positions of the player's pieces
    findall((Row, Col), piece_position(Board, Player, (Row, Col)), PlayerPositions),
    % Ensure there are pieces on the board
    (PlayerPositions \= [] ->
        (   % Check if all positions are reachable from the first position
            [FirstPos | _] = PlayerPositions,
            reachable_positions(Board, FirstPos, Player, Reachable),
            % All player positions should be in the reachable set
            subset(PlayerPositions, Reachable)
        )
    ;   fail
    ).

% Find the position of a piece on the board
piece_position(Board, Player, (Row, Col)) :-
    nth0(Row, Board, RowList),
    nth0(Col, RowList, Player).

reachable_positions(Board, CurrentPos, Player, Reachable) :-
    reachable_positions(Board, [CurrentPos], [], Player, Reachable).

reachable_positions(_, [], Reachable, _, Reachable).
reachable_positions(Board, [CurrentPos | Queue], Visited, Player, Reachable) :-
    % Get neighbors of the current position
    findall((Row, Col), neighboring_position(Board, CurrentPos, (Row, Col), Player), Neighbors),
    % Mark visited positions
    subtract(Neighbors, Visited, Unvisited),
    append(Queue, Unvisited, NewQueue),
    append(Visited, [CurrentPos], NewVisited),
    reachable_positions(Board, NewQueue, NewVisited, Player, Reachable).

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
