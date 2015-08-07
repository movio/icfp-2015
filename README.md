```scala
// load problem i: Int from http://icfpcontest.org/problems.html
val p: Problem = Problems.load(i)

// get stream of blocks (units) from source with seed index i
p.getSource(i)

// display grid
val s = new Simulator(problem, seedIndex)
Pretty.print(s.board)
```
