# Fate / Objective Caml

## Introduction
This game was originally developed as an CS 3110 course assignment at Cornell University. Storyline, characters, and background settings of the game are adapted from game products of TYPE-MOON company, including but not limited to *Fate/Extella*, *Fate/Grand Order* and *Kara no Kyoukai*.

In this text adventure game, you will play as a participant of Holy Grail War, and you will be able to explore the virtual world of SE.RA.PH by typing in various commands into the game engine, which can be directly run from command line.

## How to install
- Clone or download this repo
- Make sure OCaml and its dependencies are installed (if you have any trouble installing OCaml, please refer to this site: http://www.cs.cornell.edu/courses/cs3110/2018sp/install.html)
- In command line, run `make play` to start the game engine
- Enter `fateoc.json` to start the game

## How to play
You may interact with the game using the following commands:
- `look` to display info about current location
- `inv` or `inventory` to display your inventory
- `score` to display your score
- `turns` to display number of turns taken
- `details` to display your inventory with descriptions of objects
- `go <location>` to move to another location
- `take <object>` to take an object and put it into your inventory
- `drop <object>` to drop an object
- `quit` to quit the game
