% Main predicate to run the menu
main_menu :-
    write('--------------------------------------'), nl,
    write('|     Welcome to the Menu            |'), nl,
    write('--------------------------------------'), nl,
    write('| 1. Human vs Human                  |'), nl,
    write('| 2. Human vs PC                     |'), nl,
    write('| 3. PC VS Human                     |'), nl,
    write('| 4. PC vs PC                        |'), nl,
    write('| 0. Exit                            |'), nl,
    write('| Choose an option (0-4):            |'), nl,
    write('--------------------------------------'), nl,
    read(Choice),
    handle_choice(Choice).


% Human vs Human option
humanvshuman :-
    write('--------------------------------------'), nl,
    write('|            Human vs Human          |'), nl,
    write('--------------------------------------'), nl,
    write('TODOOOO'), nl, nl, nl.


% Human vs PC option
humanvspc :-
    write('--------------------------------------'), nl,
    write('|             PC vs Human            |'), nl,
    write('--------------------------------------'), nl,
    write('|   Select the dificulty of the PC   |'), nl,
    write('| 1. Easy                            |'), nl,
    write('| 2. Medium                          |'), nl,
    write('--------------------------------------'), nl,
    read(PC_Choice),
    handle_pc_choice(PC_Choice),
    write('TODOOOO'), nl, nl, nl.


% PC vs Human option
pcvshuman :-
    write('--------------------------------------'), nl,
    write('|             PC vs Human            |'), nl,
    write('--------------------------------------'), nl,
    write('|   Select the dificulty of the PC   |'), nl,
    write('| 1. Easy                            |'), nl,
    write('| 2. Medium                          |'), nl,
    write('--------------------------------------'), nl,
    read(PC_Choice),
    handle_pc_choice(PC_Choice),
    write('TODOOOO'), nl, nl, nl.
    

% PC vs PC option
pcvspc :-
    write('--------------------------------------'), nl,
    write('|              PC vs PC               |'), nl,
    write('--------------------------------------'), nl,
    write('|  Select the dificulty of the PC 1  |'), nl,
    write('| 1. Easy                            |'), nl,
    write('| 2. Medium                          |'), nl,
    write('--------------------------------------'), nl,
    read(PC_Choice1),
    handle_pc_choice(PC_Choice1),
    write('--------------------------------------'), nl,
    write('|  Select the dificulty of the PC 2  |'), nl,
    write('| 1. Easy                            |'), nl,
    write('| 2. Medium                          |'), nl,
    write('--------------------------------------'), nl,
    read(PC_Choice2),
    handle_pc_choice(PC_Choice2),
    write('TODOOOO'), nl, nl, nl.


% Handle the users choice
handle_choice(Choice) :-
    (   integer(Choice)
    ->  (Choice =:= 1 -> humanvshuman, main_menu;
         Choice =:= 2 -> humanvspc, main_menu;
         Choice =:= 3 -> pcvshuman, main_menu;
         Choice =:= 4 -> pcvspc, main_menu;
         Choice =:= 0 -> write('Exiting the program. Goodbye!'), nl;
         write('Invalid choice. Please try again.'), nl, main_menu)
    ;   write('Invalid input. Please enter a number between 0 and 4.'), nl, main_menu).


% Handle the pc dificulty
handle_pc_choice(Choice) :-
    (   integer(Choice)
    ->  (Choice =:= 1 -> easypc, main_menu;
         Choice =:= 2 -> mediumpc, main_menu;
         write('Invalid choice. Please try again.'), nl, main_menu)
    ;   write('Invalid input. Please enter a number between 0 and 4.'), nl, main_menu).

easypc:-
    write('TODOOOO'), nl, nl, nl.

mediumpc:-
    write('TODOOOO'), nl, nl, nl.

