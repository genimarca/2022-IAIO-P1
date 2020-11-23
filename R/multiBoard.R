#' @title Construir MultiBoard
#'
#' @param n Longitud del tablero cuadrado
#' @return Problema MultiBoard
#' @export
getMultiBoard <- function(n = 20){
   start <- c(10, 10)
   goals <- data.frame(X=c(2,2,n-1,n-1), Y=c(2,n-1,2,n-1))

   board <- matrix(1, nrow = n, ncol = n)
   board[start[1], start[2]] <- 0
   board[c(1, n), ] <- Inf
   board[ , c(1, n)] <- Inf
   board[c(4, 16), c(4:8, 13:16)] <- Inf
   board[c(4:8, 13:16), c(4, 16)] <- Inf
   board[9:13, c(8,12)] <- Inf
   board[13, 8:12] <- Inf

   cost.matrix <- board
   cost.matrix[cost.matrix==1] <- NA

   problem <- list("board"=board,
                   "start"=start,
                   "goals"=goals,
                   "cost.matrix"=cost.matrix)
   class(problem) <- "MultiBoard"

   return(problem)
}

#' @title Calcular coste para un estado de un problema de MultiBoard
#'
#' @param problem Problema a resolver
#' @param state Estado del que se va a calcular el coste
#' @return Coste del estado dado
#' @export
#' @method costState MultiBoard
costState.MultiBoard <- function(problem, state){
   return(problem$board[state$position[1], state$position[2]])
}

#' @title Establecer el coste para la reconstrucción de un problema de MultiBoard
#'
#' @param problem Problema a resolver
#' @param state Estado del que se cambia el coste
#' @param new.cost Coste calculado
#' @return Problema con el coste cambiado
#' @export
#' @method setCost MultiBoard
setCost.MultiBoard <- function(problem, state, new.cost){
   problem$cost.matrix[state$position[1], state$position[2]] <- new.cost
   return(problem)
}

#' @title Estado inicial para un problema de MultiBoard
#'
#' @param problem Problema a resolver
#' @return Estado inicial para el problema dado
#' @export
#' @method initialState MultiBoard
initialState.MultiBoard <- function(problem){
   state <- list()
   state$position <- problem$start
   state$cost <- costState(problem, state)
   state$prev <- 0
   state$found.goals <- rep(FALSE, nrow(problem$goals))

   class(state) <- c("MultiBoardState")
   return(state)
}

#' @title Conseguir acciones para un problema de MultiBoard
#'
#' @param problem Problema a resolver
#' @param state Estado actual del problema
#' @return Posibles acciones
#' @export
#' @method actions MultiBoard
actions.MultiBoard <- function(problem, state){
   return(c("U", "R", "D", "L"))
}

#' @title Comprobar validez de un estado de un problema de MultiBoard
#'
#' @param problem Problema a resolver
#' @param state Estado actual
#' @param action Última accion tomada
#' @return Estado corregido para ser válida
#' @export
#' @method checkValidState MultiBoard
checkValidState.MultiBoard <- function(problem, state, action){
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

#' @title Calcular el siguiente estado para un problema de MultiBoard
#'
#' @param problem Problema a resolver
#' @param state Estado actual
#' @param action Accion tomada
#' @return Nuevo estado
#' @export
#' @method nextState MultiBoard
nextState.MultiBoard <- function(problem, state, action){
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

   found.new.goals <- apply(problem$goals, 1, function(x){
     all(new.state$position == x)
   })

   new.state$found.goals <- new.state$found.goals | found.new.goals

   return(new.state)
}

#' @title Calcular si un estado es final para un problema MultiBoard dado
#'
#' @param problem Problema a comprobar
#' @param state Estado a comparar si es final
#' @return Comprobación del estado
#' @export
#' @method finalState MultiBoard
finalState.MultiBoard <- function(problem, state){
   final <- all(state$found.goals)
   return(final)
}


#' @title Comparación de la igualdad de dos estados de MultiBoard
#'
#' @param state1 Estado 1 a comparar
#' @param state2 Estado 2 a comparar
#' @return Comprobación de la igualdad de dos estados
#' @export
#' @method sameState MultiBoardState
sameState.MultiBoardState <- function(state1, state2){
   same <- all(state1$position == state2$position) && all(state1$found.goals == state2$found.goals)
   return(same)
}

#' @title Método heurístico para problema de MultiBoard
#'
#' @param problem Problema a calcular
#' @param state Estado
#' @param heuristic.method Método
#' @export
#' @method heuristic MultiBoard
heuristic.MultiBoard <- function(problem, state, heuristic.method){
  unfound.goals <- problem$goals[!state$found.goals, ]
  heuristic.value <- ifelse(nrow(unfound.goals) == 0, 0,
         switch(heuristic.method,
           "euclidean" = min(apply(unfound.goals, 1, function(x){
             sqrt(sum(state$position-problem$goal)**2)
           })),
           "euclidean2" = min(apply(unfound.goals, 1, function(x){
             sum(state$position-problem$goal)**2
           })),
           "manhattan" = min(apply(unfound.goals, 1, function(x){
             sum(abs(state$position-problem$goal))
           })),
           "chessboard" = min(apply(unfound.goals, 1, function(x){
             max(abs(state$position-problem$goal))
           })),
           "ucs" = 0
         ))
  return(heuristic.value)
}
