% Main predicate to run the menu
play :-
    write('--------------------------------------'), nl,
    write('|       Welcome to Ayu Game          |'), nl,
    write('--------------------------------------'), nl,
    choose_board_size(BoardSize),
    choose_players(Players),
    create_initial_board(BoardSize, Board),
    initial_state(Board, Players, GameState),
    display_game(GameState),  % Display the game board once
    game_cycle(GameState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% INITIAL STATE %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialize the game state
initial_state(Board, Players, game(Board, Players, CurrentPlayer)) :-
    nth0(0, Players, CurrentPlayer),
    write('Game initialized!'), nl.

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

% Create the initial empty board
create_initial_board(Size, Board) :-
    length(Row, Size),
    maplist(=(empty), Row),
    length(Board, Size),
    maplist(=(Row), Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% DISPLAY GAME %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Display the game board with row and column numbers
display_game(game(Board, Players, CurrentPlayer)) :-
    nl, write('Current Game State:'), nl,
    write('Players: '), write(Players), nl,
    write('Current Player: '), write(CurrentPlayer), nl,
    write('Current Board'), nl,
    display_board(Board),
    nl.


% Display the board with numbered rows and columns
display_board(Board) :-
    % Display column numbers at the top
    write('     '),
    length(Board, MaxColumns),
    forall(between(1, MaxColumns, Col), 
           (print_column_number(Col))),
    nl,
    write('   -----------------------------------------------'), nl, 
    % Display each row with row numbers
    display_rows(Board, 1),
    write('   -----------------------------------------------'), nl.

% Print each column number with proper spacing
print_column_number(Col) :-
    (Col < 10 ->  % If the column is a single digit
        write(' '), write(Col), write('  ')
    ;  % If the column is a double digit
        write(Col), write('  ')
    ).

display_rows([], _).
display_rows([Row|Rest], RowNum) :-
    (RowNum < 10 ->  % If the row number is single digit
        write(' '), write(RowNum), write(' |')
    ;  % If the row number is double digit
        write(RowNum), write(' |')
    ),
    maplist(display_cell, Row),
    write(' |'), 
    nl,
    NewRowNum is RowNum + 1,
    display_rows(Rest, NewRowNum).

display_cell(empty) :- write('  . ').  
display_cell(Cell) :- write(' '), write(Cell), write('  '). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% MOVE HANDLING %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

choose_move(game(Board, [Player1, Player2], Player1), Player1, Move) :-
    display_board(Board),  % Display board after choosing move
    write('Player 1 (Human), it\'s your turn! Please enter your move (row-column):'), nl,
    safe_read(Move),
    valid_move(Board, Move),  % Validate the move
    !.

choose_move(game(Board, [Player1, Player2], Player2), Player2, Move) :-
    display_board(Board),  % Display board after choosing move
    write('Player 2 (Human), it\'s your turn! Please enter your move (row-column):'), nl,
    safe_read(Move),
    valid_move(Board, Move),  % Validate the move
    !.

choose_move(game(_, _, pc_level1), pc_level1, Move) :-
    write('PC Level 1 is choosing a move...'), nl,
    Move = dummy_move.  % Replace this with actual logic for PC level 1

choose_move(game(_, _, pc_level2), pc_level2, Move) :-
    write('PC Level 2 is choosing a move...'), nl,
    Move = dummy_move.  % Replace this with actual logic for PC level 2

choose_move(_, _, _) :-
    write('Invalid player type or move.'), nl, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% GAME CYCLE %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

game_cycle(GameState) :-
    game_over(GameState, Winner), !,
    congratulate(Winner).

game_cycle(GameState) :-
    choose_move(GameState, Player, Move),
    apply_move(GameState, Move, NewGameState),
    display_game(NewGameState),
    game_cycle(NewGameState).

% Placeholder for determining if the game is over
game_over(_, _) :- fail.

% Congratulate the winner
congratulate(Winner) :-
    write('Congratulations! The winner is: '), write(Winner), nl.
