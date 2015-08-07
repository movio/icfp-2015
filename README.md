```scala
// load problem i: Int from http://icfpcontest.org/problems.html
val p: Problem = Problems.load(i)

// get an object represnting all the blocks (units) from source with seed index i
// WARNING object is mutable, calling `next` works like `dequeue`
p.getSource(i)

// display grid
val s = new Simulator(problem, seedIndex)
s.spawn() // spawns next piece
s.draw()
```
