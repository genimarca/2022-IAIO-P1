library(Practica1)
context("MultiBoard Problem")

test_that("Create MultiBoard Problem", {

   # Create Board
   board <- getMultiBoard()

   # Check Initial State
   initial.state <- initialState(board)
   expect_equal(initial.state$position, c(10,10))
   expect_equal(initial.state$cost, 0)
   expect_equal(initial.state$prev, 0)

   # Check Same State
   expect_true(sameState(initial.state, initial.state))

   # New state
   possible.actions <- actions(board, initial.state)
   expect_equal(length(possible.actions), 4)

   # Next State
   next.state <- nextState(board, initial.state, possible.actions[1])
   expect_false(sameState(initial.state, next.state))

})

test_that("Print BFS MultiBoard",{
   # Create Board
   board <- getMultiBoard()
   results.bfs <- Practica1::bfs(board)
   representPath(results.bfs$problem, results.bfs$path)$img
   expect_true(results.bfs$completed)
})

test_that("Print DFS MultiBoard",{
   # Create Board
   board <- getMultiBoard()
   results.dfs <- Practica1::dfs(board)
   representPath(results.dfs$problem, results.dfs$path)$img
   expect_true(results.dfs$completed)
})

test_that("Print ASTAR MultiBoard euclidean",{
   # Create Board
   board <- getMultiBoard()
   results.astar <- Practica1::astar(board, "euclidean")
   representPath(results.astar$problem, results.astar$path)$img
   expect_true(results.astar$completed)
})

test_that("Print ASTAR MultiBoard euclidean2",{
   # Create Board
   board <- getMultiBoard()
   results.astar <- Practica1::astar(board, "euclidean2")
   representPath(results.astar$problem, results.astar$path)$img
   expect_true(results.astar$completed)
})

test_that("Print ASTAR MultiBoard manhattan",{
   # Create Board
   board <- getMultiBoard()
   results.astar <- Practica1::astar(board, "manhattan")
   representPath(results.astar$problem, results.astar$path)$img
   expect_true(results.astar$completed)
})

test_that("Print ASTAR MultiBoard chessboard",{
   # Create Board
   board <- getMultiBoard()
   results.astar <- Practica1::astar(board, "chessboard")
   representPath(results.astar$problem, results.astar$path)$img
   expect_true(results.astar$completed)
})

test_that("Print ASTAR MultiBoard ucs",{
   # Create Board
   board <- getMultiBoard()
   results.astar <- Practica1::astar(board, "ucs")
   representPath(results.astar$problem, results.astar$path)$img
   expect_true(results.astar$completed)
})
