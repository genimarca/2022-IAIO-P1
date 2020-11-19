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
   possible.actions <- actions(board, initial.state)
   expect_equal(length(possible.actions), 4)

   # Next State
   next.state <- nextState(board, initial.state, possible.actions[1])
   expect_false(sameState(initial.state, next.state))

})

test_that("Print BFS NQueens",{
   # Create NQueens
   problem <- getNQueens()
   results.bfs.nqueens <- Practica1::bfs(problem)
   representPath(results.bfs$problem, results.bfs$path)$img
   expect_true(results.bfs$completed)
})


test_that("Print DFS NQueens",{
   # Create NQueens
   problem <- getNQueens()
   results.dfs.nqueens <- Practica1::dfs(problem)
   representPath(results.bfs$problem, results.bfs$path)$img
   expect_true(results.bfs$completed)
})
