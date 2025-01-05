# AYU

## Identification

**Topic**: AYU Game

**Group Designation**:

- PFL_TP2_T09_G02

- Members:
    
    - Clarisse Maria Teixeira de Carvalho, 50%

    - Henrique Silva Marques, 50%


## Installation and Execution

1. Go to `src` folder 

2. Open a Prolog interpreter

3. Compile the code using: `['game.pl'].`

4. Start the game using, `play.`

## Game Description

AYU is a strategic board game where players aim to group their pieces while preventing their opponent from doing the same. The player who cannot make a move on their turn wins, which typically happens when all their pieces are joined in a single group. Draws are theoretically possible but are avoided as players aim to win rather than cooperate.

**Rules**:

- Players alternate turns, selecting a piece and moving it according to the game's movement rules.

- A move involves specifying the current coordinates of the piece and its new position.

- If a player selects an invalid move, they must try again.

## Considerations for game extensions:

- **Variable-Sized Boards**: Designed to allow boards of different sizes for varied gameplay.

- **Gameplay Features**: Limited move suggestions to the top 5 options for easier decision-making.

- **Computer Players**: Implementation of random and greedy AI players

## Game Logic

### Game Configuration Representation

The initial configuration is represented using a structured Prolog predicate `initial_state/2`, which defines the board's size and initial piece placements.

### Internal Game State Representation

- **Representation**: The game state is modeled as a list of lists representing the board grid. Each cell contains an atom indicating its state (e.g., empty, occupied by Player 1, or Player 2).

- **Examples**:

    - **Initial State**: [empty, empty, player1, player2, empty]

    - **Intermediate State**: [player1, empty, empty, player2, player1]

    - **Final State**: [player1, player1, player1, player1, player1]

### Move Representation

- **Structure**: Moves are represented as tuples of coordinates, e.g., ((X1, Y1), (X2, Y2)).

- **Usage**: The move/3 predicate validates and executes moves, ensuring adherence to the rules

### User Interaction

- **Menu System**: A simple menu allows users to start a new game, view rules, or exit.

- **Input Validation**: Ensures valid piece selection and movement, prompting the player to retry invalid actions.

## Conclusions

The AYU game implementation successfully provides a foundation for strategic gameplay. However, there are limitations and areas for improvement:

**Known Issues**:

- Infinite move options can overwhelm players without suggested moves.

- AI players require further optimization.

**Future Developments**:

- Enhanced AI strategies.

- Support for additional game modes.

- Improved user interface

## Bibliography

Webpages:

- https://boardgamegeek.com/boardgame/114484/ayu

- https://www.mindsports.nl/index.php/arena/ayu

Tools Used:

- ChatGPT to refine game logic 