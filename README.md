# Conway's Game of Life -- in Delphi!
A recreation of the classic Conway's Game Of Life written in Object Pascal with the Delphi VCL. The entire project should be compatible with most versions of the Delphi IDE from the latest RAD Studio down to past Delphi 7.

![Game of Life screenshot](/etc/screen.jpg)

# Dependencies
This project requires that the TX Delphi Library is installed on your machine and locatable by the Delphi IDE as a library. This is because the grid that is used heavily in this project is contained within the TX library. You can find it [here](https://github.com/tomxxi/tx-delphi-lib)

# Rules
Conway's Game of Life is a Turing complete zero-player game that follows the following ruleset:
  - Any live cell with fewer than two live neighbours dies, as if by underpopulation.
  - Any live cell with two or three live neighbours lives on to the next generation.
  - Any live cell with more than three live neighbours dies, as if by overpopulation.
  - Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
  
Every cell in the game has this ruleset applied to each generation of cells and that is the fundamentals of how the game works. This is a 100% accurate recreation with an identical ruleset.

The only difference is that in the original game there were outer walls. In this version, the grid that the game is played on wraps around, making the game area essentially infinite.

# Code usage
This software is licensed with the GNU General Public License v3.0. This entitles you to basically do with it as you see fit, although credit should always be given with a link to this page so the code can be used by others further down the line.

# More info
For more information on the logic and history of Conway's Game of Life, visit the detailed wikipedia entry: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
