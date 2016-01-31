# Todo list
- Infinite grid in all directions
- When opening empty cells stop at the edge of the terminal (and update on move/resize)
- Mark cells with mine markers (▲) -- toggleable with a key (say "M"), when marked the cell cannot be opened
- when the number of markers matches the current cell's number: possibility to open neighbouring cells
- when mine explodes: ripple explode through the whole terminal
- stop the cursor from blinking
- possiblity to position the cursor not in the center (maybe with a margin of 5 cells to the sides)
- problem when resizing terminal too small: it breaks ncurses and exits