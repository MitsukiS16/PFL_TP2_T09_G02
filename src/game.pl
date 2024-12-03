% Main predicate to run the menu
play :-
    write('--------------------------------------'), nl,
    write('|     Welcome to Ayu Game            |'), nl,
    write('--------------------------------------'), nl,
    % Initialize the game state by choosing player types and board size
    initial_state(PlayerType, BoardSize),
    % Display the selected options
    write('You selected: '), nl,
    write('Player Type: '), write(PlayerType), nl,
    write('Board Size: '), write(BoardSize), write('x'), write(BoardSize), nl,
    % Set the global board size for visualization
    retractall(board_size(_)),  % Remove any existing board_size value
    assert(board_size(BoardSize)),
    % Display the game board
    display_game,
    % Start the game loop
    move().


% Prompt for initial state (player type and board size)
initial_state(PlayerType, BoardSize) :-
    choose_player_type(PlayerType),
    choose_board_size(BoardSize).

% Player type selection menu
choose_player_type(PlayerType) :-
    write('--------------------------------------'), nl,
    write('| Choose Player Type                 |'), nl,
    write('| 1. Human vs Human                  |'), nl,
    write('| 2. Human vs PC                     |'), nl,
    write('| 3. PC vs Human                     |'), nl,
    write('| 4. PC vs PC                        |'), nl,
    write('| Choose an option (1-4):            |'), nl,
    write('--------------------------------------'), nl,
    read(Choice),
    handle_player_choice(Choice, PlayerType).

% Handle player's choice for type of player
handle_player_choice(1, 'Human vs Human').
handle_player_choice(2, 'Human vs PC').
handle_player_choice(3, 'PC vs Human').
handle_player_choice(4, 'PC vs PC').
handle_player_choice(_, PlayerType) :-
    write('Invalid choice. Please try again.'), nl,
    choose_player_type(PlayerType).

% Board size selection menu
choose_board_size(BoardSize) :-
    write('--------------------------------------'), nl,
    write('| Choose Board Size                  |'), nl,
    write('| 1. 11x11 (Original Size)           |'), nl,
    write('| 2. 13x13                           |'), nl,
    write('| 3. 9x9                             |'), nl,
    write('| Choose an option (1-3):            |'), nl,
    write('--------------------------------------'), nl,
    read(Choice),
    handle_board_size_choice(Choice, BoardSize).

% Handle user's choice for board size
handle_board_size_choice(1, 11).
handle_board_size_choice(2, 13).
handle_board_size_choice(3, 9).
handle_board_size_choice(_, BoardSize) :-
    write('Invalid choice. Please try again.'), nl,
    choose_board_size(BoardSize).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% BOARD %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Visualization of the game board
display_game :-
    write('Displaying the game board...'), nl,
    board_size(Size), % Retrieve the current board size
    write('Displaying a game board of size '), write(Size), write('x'), write(Size), nl,
    display_board(Size).

% Generate and display the board based on size
display_board(Size) :-
    display_separator(Size),         % Display the top border
    display_rows(Size),              % Call to display all rows
    display_separator(Size).         % Display the bottom border

% Display the separator line based on board size
display_separator(Size) :-
    TotalDashes is Size * 5 + 2,     % Calculate the total length 
    display_dashes(TotalDashes), 
    nl.

% Helper to display the dashes
display_dashes(0).                   % Base case: no more dashes to display

display_dashes(N) :-
    write('-'),
    NextN is N - 1,
    display_dashes(NextN).

% Display all rows
display_rows(0).                     % Base case: no more rows to display

display_rows(Size) :-
    board_size(BoardSize),           % Get the fixed board size
    write('|'),                      % Start of the row
    display_row(BoardSize),          % Display columns for the fixed board size
    write('|'),
    nl,                              % Newline after the row
    NextSize is Size - 1,            % Decrease the remaining number of rows
    display_rows(NextSize).          % Display next row

% Display the cells in a single row
display_row(0).                      % Base case: no more columns to display

display_row(Size) :-
    write('  .  '),                  % Placeholder for board cells
    NextSize is Size - 1,            % Decrease the remaining number of columns
    display_row(NextSize).           % Recursively display the next column

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% MOVE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Placeholder for game loop
move :-
    write('TODO: Implement the game move logic here.'), nl.
