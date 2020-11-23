#' @title Construir Problema n-reinas
#'
#' @param n Longitud Problema N-reinas
#' @return Problema N-Reinas
#' @export
getNQueens <- function(n = 8){
  n <- n
  cost.matrix <- matrix(0, nrow=n, ncol=n)
  problem <- list("n"=n, "cost.matrix"=cost.matrix)
  class(problem) <- "NQueens"

  return(problem)
}

#' @title Estado inicial para un problema de n-reinas
#'
#' @param problem Problema a resolver
#' @return Estado inicial para el problema dado
#' @export
#' @method initialState NQueens
initialState.NQueens <- function(problem){
   state <- list()
   state$board <- rep(0, problem$n)
   state$cost <- 0
   state$prev <- 0

   class(state) <- c("NQueensState")
   return(state)
}

#' @title Calcular coste para un estado de un problema de n-reinas
#'
#' @param problem Problema a resolver
#' @param state Estado del que se va a calcular el coste
#' @return Coste del estado dado
#' @export
#' @method costState NQueens
costState.NQueens <- function(problem, state){
  n <- problem$n
  n.settled.queens <- sum(state$board != 0)


                                        # Return Inf if invalid
                                        # Check colums
  same.column <- length(unique(state$board[state$board != 0])) != n.settled.queens
  df <- data.frame(index=1:n, value=state$board)
  diag <- sapply(1:n, function(i){
    if(df[i, 2] == 0){
      return(FALSE)
    }
    else{
      index.diff <- abs(i - df$index)
      value.diff <- abs(df$value[i] - df$value)

      diag <- index.diff == value.diff
      diag <- diag[df$value != 0 & df$index != i]

      return(any(diag))
    }
  })
  return(ifelse(same.column || any(diag), Inf, 1))
}

#' @title Establecer el coste para la reconstrucción de un problema de n-reinas
#'
#' @param problem Problema a resolver
#' @param state Estado del que se cambia el coste
#' @param new.cost Coste calculado
#' @return Problema con el coste cambiado
#' @export
#' @method setCost NQueens
setCost.NQueens <- function(problem, state, new.cost){
  for(i in 1:problem$n){
    problem$cost.matrix[i, state$board[i]] <- new.cost
  }
  return(problem)
}


#' @title Conseguir acciones para un problema de n-reinas
#'
#' @param problem Problema a resolver
#' @param state Estado actual del problema
#' @return Posibles acciones
#' @export
#' @method actions NQueens
actions.NQueens<- function(problem, state){
  n <- problem$n

  to.fill <- which(state$board == 0)
  in.board <- 1:n %in% state$board

  add.queen <- expand.grid(to.fill, (1:n)[!in.board])
  remove.queen <- expand.grid(c(0), which(state$board != 0))

  actions <- rbind(remove.queen, add.queen) %>% split(., seq(nrow(.)))
  return(actions)
}

#' @title Calcular el siguiente estado para un problema de n-reinas
#'
#' @param problem Problema a resolver
#' @param state Estado actual
#' @param action Accion tomada
#' @return Nuevo estado
#' @export
#' @method nextState NQueens
nextState.NQueens <- function(problem, state, action){
   new.state <- state
   new.state$board[action[,1]] <- action[,2]

   new.state$cost <- new.state$cost + costState(problem, new.state)
   new.state$prev <- 0

   return(new.state)
}

#' @title Calcular si un estado es final para un problema n-reinas dado
#'
#' @param problem Problema a comprobar
#' @param state Estado a comparar si es final
#' @return Comprobación del estado
#' @export
#' @method finalState NQueens
finalState.NQueens <- function(problem, state){
   return(length(state$board[state$board!=0]) == problem$n)
}


#' @title Comparación de la igualdad de dos estados de n-reinas
#'
#' @param state1 Estado 1 a comparar
#' @param state2 Estado 2 a comparar
#' @return Comprobación de la igualdad de dos estados
#' @export
#' @method sameState NQueensState
sameState.NQueensState <- function(state1, state2){
   same <- all(state1$board == state2$board)
   return(same)
}

#' @title Método heurístico para problema de n-reinas
#'
#' @param problem Problema a calcular
#' @param state Estado
#' @param heuristic.method Método
#' @export
#' @method heuristic NQueens
heuristic.NQueens <- function(problem, state, heuristic.method){
  n.to.fill <- sum(state$board == 0)
  n.to.fill <- ifelse(n.to.fill == 1, 0, n.to.fill)
  heuristic.value <- switch(heuristic.method,
                            "basic" = n.to.fill,
                            "ucs" = 0)
  return(heuristic.value)
}

