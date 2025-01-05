# AYU

```
Identification of the topic (game) and group (group designation, student number and full name of each
member of the group), as well as an indication of the contribution (in percentages, adding up to 100%,
and a brief description of tasks performed) of each member of the group to the assignment
```

## Installation and Execution

1. Go to `src` folder 

2. Open prolog interpreter, eg `swipl`

3. Compile the code, eg `['game.pl'].`

4. Run code, `play.`

## Game Description

```
a brief description of the game and its rules; you should also include the links
used to gather information (official game website, rule book, etc.)
```

## Considerations for game extensions:

```
describe the considerations taken into account when extending
the game design, namely when considering variable-sized boards, optional rules (e.g., simplified rules
for novice players, additional rules for expert players), and other aspects.
```

## Game Logic

```
Describe the main design decisions regarding the implementation of the game logic in
Prolog (do not copy the source code). This section should have information on the following topics,
among others
```

### Game Configuration Representation

```
describe the information required to represent the game
configuration, how it is represented internally and how it is used by the initial_state/2 predicate
```

### Internal Game State Representation
```
describe the information required to represent the game
state, how it is represented internally, including an indication of the meaning of each atom (i.e. how
different pieces are represented). Include examples of representations of initial, intermediate, and
final game states
```

### Move Representation
```
describe the information required to represent a move, and how it is
represented internally (e.g., the coordinates of a board location, and/or other information
necessary to represent a move) and how it is used by the move/3 predicate
```

### User Interaction
```
ly describe the game menu system, as well as how interaction with the user
is performed, focusing on input validation (e.g., when reading a move
```

## Conclusions
```
Conclusions about the work carried out, including limitations of the program (and known
issues), as well as possible improvements (future developments roadmap
```

## Bibliography
```
List of books, papers, web pages and other resources used during the development of the
assignment. If you used tools such as ChatGPT, list the queries used
```



## TODO

* Update board to original ayu board
* Update move parts -> the player shoudl seelct a piece (indicate the coordinates) nad than give th enew movemente (indicate the coordinates)
* because we have almost infinite options, just print 5 options of movement
* implement the random player
* implement the gready player
* implement when its a win -  If a player can't make a move on his turn, he wins. This usually occurs when said player has joined all his pieces in a single group. (its possible to have draws but ww will asume that players want to win and its not cooperate)
* implenete possible moves of a piece (because if a player sleect a impossible move , we should say its impossible and he have to try again), and this implementaion can be use for the pc select a move
* implement the update of the board when select move 
