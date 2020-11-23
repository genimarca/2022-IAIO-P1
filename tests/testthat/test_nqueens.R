library(Practica1)
context("NQueens Problem")

test_that("Create NQueens Problem", {

   # Create NQueens
   problem <- getNQueens()

   # Check Initial State
   initial.state <- initialState(problem)
   cost.state <- costState(problem, initial.state)

   # Check Same State
   expect_true(sameState(initial.state, initial.state))

   # New state
   possible.actions <- actions(problem, initial.state)
   expect_equal(length(possible.actions), 64)

   # Next State
   next.state <- nextState(problem, initial.state, possible.actions[[1]])
   expect_false(sameState(initial.state, next.state))

})

test_that("Print BFS NQueens",{
   # Create NQueens
   problem <- getNQueens()
   results.bfs.nqueens <- Practica1::bfs(problem)
   representPath(results.bfs.nqueens$problem, results.bfs.nqueens$path)
   expect_true(results.bfs.nqueens$completed)
})


test_that("Print DFS NQueens",{
   # Create NQueens
   problem <- getNQueens()
   results.dfs.nqueens <- Practica1::dfs(problem)
   representPath(results.dfs.nqueens$problem, results.dfs.nqueens$path)
   expect_true(results.dfs.nqueens$completed)
})

test_that("Print A* basic NQueens",{
   # Create NQueens
   problem <- getNQueens(n=6)
   results.astar.nqueens <- Practica1::astar(problem, "basic")
   representPath(results.astar.nqueens$problem, results.astar.nqueens$path)
   expect_true(results.astar.nqueens$completed)
})

test_that("Print A* ucs NQueens",{
   # Create NQueens
   problem <- getNQueens(n=6)
   results.astar.nqueens <- Practica1::astar(problem, "ucs")
   representPath(results.astar.nqueens$problem, results.astar.nqueens$path)
   expect_true(results.astar.nqueens$completed)
})

