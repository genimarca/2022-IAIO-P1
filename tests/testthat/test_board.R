library(Practica1)
context("Board Problem")

test_that("Create Board Problem", {

   # Create Board
   board <- getBoard()

   # Check Initial State
   initial.state <- initialState(board)
   expect_equal(initial.state$position, c(2,2))
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

test_that("Print BFS",{
   # Create Board
   board <- getBoard()
   results.bfs <- Practica1::bfs(board)
   representPath(results.bfs$problem, results.bfs$path)$img
   expect_true(results.bfs$completed)
})

test_that("Print DFS",{
   # Create Board
   board <- getBoard()
   results.dfs <- Practica1::dfs(board)
   representPath(results.dfs$problem, results.dfs$path)$img
   expect_true(results.dfs$completed)
})

test_that("Print ASTAR euclidean",{
   # Create Board
   board <- getBoard()
   results.astar <- Practica1::astar(board, "euclidean")
   representPath(results.astar$problem, results.astar$path)$img
   expect_true(results.astar$completed)
})

test_that("Print ASTAR euclidean2",{
   # Create Board
   board <- getBoard()
   results.astar <- Practica1::astar(board, "euclidean2")
   representPath(results.astar$problem, results.astar$path)$img
   expect_true(results.astar$completed)
})

test_that("Print ASTAR manhattan",{
   # Create Board
   board <- getBoard()
   results.astar <- Practica1::astar(board, "manhattan")
   representPath(results.astar$problem, results.astar$path)$img
   expect_true(results.astar$completed)
})

test_that("Print ASTAR chessboard",{
   # Create Board
   board <- getBoard()
   results.astar <- Practica1::astar(board, "chessboard")
   representPath(results.astar$problem, results.astar$path)$img
   expect_true(results.astar$completed)
})


test_that("Print ASTAR ucs",{
   # Create Board
   board <- getBoard()
   results.astar <- Practica1::astar(board, "ucs")
   representPath(results.astar$problem, results.astar$path)$img
   expect_true(results.astar$completed)
})
