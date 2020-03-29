# Conway's Game of Life -- in Delphi!
A recreation of the classic Conway's Game Of Life written in Object Pascal with the Delphi VCL.

The code was written in (and targets) Delphi XE's RAD Studio Rio and should work with out of the box, though compatibility should run all the way down past Delphi 7.

# Rules
Conway's Game of Life is a Turing complete zero-player game that follows the following ruleset:
  - Any live cell with fewer than two live neighbours dies, as if by underpopulation.
  - Any live cell with two or three live neighbours lives on to the next generation.
  - Any live cell with more than three live neighbours dies, as if by overpopulation.
  - Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
  
Every cell in the game has this ruleset applied to each generation of cells and that is the fundamentals of how the game works.

# More info
For more information on the logic and history of Conway's Game of Life, visit the detailed wikipedia entry: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
