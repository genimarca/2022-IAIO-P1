#' @title Get Definition of Board Problem
#'
#' @param n Length of the board
#' @export
getBoard <- function(n = 10){
   start <- c(2, 2)
   goal <- c(2, n-1)

   board <- matrix(1, nrow = n, ncol = n)
   board[start[1], start[2]] <- 0
   board[c(1, n), ] <- Inf
   board[1:5, 5] <- Inf
   board[ , c(1, n)] <- Inf

   cost.matrix <- matrix(NA, nrow = n, ncol = n)
   cost.matrix[start[1], start[2]] <- 0
   cost.matrix[c(1, n), ] <- Inf
   cost.matrix[1:5, 5] <- Inf
   cost.matrix[ , c(1, n)] <- Inf

   problem <- list("board"=board,
                   "start"=start,
                   "goal"=goal,
                   "cost.matrix"=cost.matrix)
   class(problem) <- "Board"

   return(problem)
}

#' @title costState for Board Problem
#'
#' @param problem Problema a resolver
#' @param state Estado a calcular el coste
#' @export
#' @method costState Board
costState.Board <- function(problem, state){
   return(problem$board[state$position[1], state$position[2]])
}

#' @title setCost for Board Problem
#'
#' @param problem Problema a resolver
#' @param state Estado a cambiar el coste
#' @param new.cost Nuevo coste a añadir
#' @export
#' @method setCost Board
setCost.Board <- function(problem, state, new.cost){
   problem$cost.matrix[state$position[1], state$position[2]] <- new.cost
   return(problem)
}


#' @title Initial State for Board Problem
#'
#' @param problem Problema para obtener el estado inicial
#' @export
#' @method initialState Board
initialState.Board <- function(problem){
   state <- list()
   state$position <- problem$start
   state$cost <- costState(problem, state)
   state$prev <- 0

   class(state) <- c("BoardState")
   return(state)
}

#' @title Get actions for Board Problem
#'
#' @param problem Board problem
#' @param state Estado actual del problema
#' @export
#' @method actions Board
actions.Board <- function(problem, state){
   return(c("U", "R", "D", "L"))
}

#' @title Check State for Board Problem
#'
#' @param problem Problema a resolver
#' @param state Estado actual
#' @param action Accion tomada
#' @export
#' @method checkValidState Board
checkValidState.Board <- function(problem, state, action){
   n <- nrow(problem$board)

   if(state$position[1] > n)
      state$position[1] = n
   else if(state$position[1] < 1)
      state$position[1] = 1
   else if(state$position[2] > n)
      state$position[2] = n
   else if(state$position[2] < 1)
      state$position[2] = 1

   return(state)
}

#' @title Next State for Board Problem
#'
#' @param problem Problema a resolver
#' @param state Estado actual
#' @param action Accion tomada
#' @export
#' @method nextState Board
nextState.Board <- function(problem, state, action){
   new.state <- state

   # Apply action
   if(action == "U")
      new.state$position[2] <- new.state$position[2] + 1
   else if(action == "D")
      new.state$position[2] <- new.state$position[2] - 1
   else if(action == "L")
      new.state$position[1] <- new.state$position[1] - 1
   else if(action == "R")
      new.state$position[1] <- new.state$position[1] + 1

   new.state <- checkValidState(problem, new.state, action)

   new.state$cost <- new.state$cost + costState(problem, new.state)
   new.state$prev <- 0

   return(new.state)
}

#' @title Final State for Board Problem
#'
#' @param problem Problema a resolver
#' @param state Estado a comparar si es final
#' @export
#' @method finalState Board
finalState.Board <- function(problem, state){
   final <- all(problem$goal == state$position)
   return(final)
}

#' @title Same State for Board Problem
#'
#' @param state1 Estado 1 a comparar
#' @param state2 Estado 2 a comparar
#' @export
#' @method sameState BoardState
sameState.BoardState <- function(state1, state2){
   same <- all(state1$position == state2$position)
   return(same)
}


#' @title Heuristic for Board Problem
#'
#' @param problem Problema a calcular
#' @param state Estado
#' @param heuristic.method Método
#' @export
#' @method heuristic Board
heuristic.Board <- function(problem, state, heuristic.method){
   heuristic.value <- switch (heuristic.method,
      "euclidean" = sqrt(sum(state$position-problem$goal)**2),
      "euclidean2" = sum(state$position-problem$goal)**2,
      "ucs" = 0
   )
   return(heuristic.value)
}
