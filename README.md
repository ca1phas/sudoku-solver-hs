Project code for:

1. https://youtu.be/glog9DZh8G0?si=MTmY960q_WENki8P
2. https://youtu.be/O1-ruHzabAU?si=DC0mmSNJzfCMKbbY
3. https://youtu.be/ESDpXBd1cJM?si=8rDNRle1pr-0HrcS

# Sudoku solver

### Description

https://en.wikipedia.org/wiki/Sudoku

### How to use the program

1. Open the terminal window.
2. Type `ghci index.hs` and press Enter.
3. Type `[method] [difficulty]` and press Enter.<br>
   `[method]` =<br>
   a. `solve` -> take less than 10 seconds to terminate<br>
   b. `repeatedPruneSolve` -> takes a long time to terminate except `easy`<br>
   c. `pruneSolve` -> takes a long time to terminate<br>
   d. `basicSolve` -> takes forever to terminate<br>
   `[difficulty]` = `easy`/`gentle`/`diabolical`/`unsolvable`/`minimal`/`blank`

### Branches

1. redo -> attempt to recreate the solver with minimal referencing
