#' A* algorithm
#' @param problem Problema a buscar
#' @param heuristic Función heurística
#' @export
astar <- function(problem, heuristic.method="ucs"){
  current.state <- initialState(problem)
  set.open <- list(current.state)
  set.open.costs <- c(0)
  set.open.heuristic <- c(heuristic(problem, current.state, heuristic.method))

  set.closed <- list()
  set.closed.cost <- numeric(0)

  nodos <- 0
  index <- 1

                                        # Element in OPEN with smallest cost
  index = which.min(set.open.costs+set.open.heuristic)

  while (!finalState(problem, current.state) && length(set.open) > 0) {
    nodos <- nodos + 1

    set.closed <- append(set.closed,
                         list(set.open[[index]]))
    set.closed.cost <- append(set.closed.cost,
                              set.open.costs[[index]])

                                        # Update Open
    set.open[[index]] <- NULL
    set.open.costs <- set.open.costs[-index]
    set.open.heuristic <- set.open.heuristic[-index]


                                        # For each of these neighbor spaces, assign cost and pointers;
                                        # and if some are in the closed set and their costs are smaller,
                                        # update their cost and pointers

                                        # Find cost and heuristic of moving to neighbor spaces to goal
    actions = actions(problem, current.state)

    for(act in actions){
                                        #Compute next state
      next.state <- nextState(problem, current.state, act)
      next.state$prev <- length(set.closed)
      next.cost <- next.state$cost #costState(problem, next.state)
      next.heuristic <- heuristic(problem, next.state, heuristic.method)
                                        # if cost is infinite, then it's an invalid state, so ignore
      if( is.finite(next.cost) ){
        next.node.is.open <- searchList(state = next.state, search.list = set.open)
        next.node.is.closed <- searchList(state = next.state, search.list = set.closed)
                                        # If node is not in OPEN or CLOSED then insert node in OPEN
        if(!next.node.is.open && !next.node.is.closed){
          problem <- setCost(problem, next.state, next.cost)
          set.open <- append(set.open, list(next.state))
          set.open.costs <- c(set.open.costs, next.cost)
          set.open.heuristic <- c(set.open.heuristic, next.heuristic)
        }
                                        # else node has already been seen, so check to see if we have found a
                                        # better route to it
        else if(next.node.is.open){
          index <- searchList(next.state, set.open)
                                        # Update if we have a better route
          if(set.open.costs[index] > next.cost){
            problem <- setCost(problem, next.state, next.cost)
            set.open[[index]]$cost <- next.cost
            set.open[[index]]$prev <- length(set.closed)
            set.open.costs[index] <- next.cost
            set.open.heuristic[index] <- next.heuristic
          }
        }
                                        # else node has already been closed, so check to see if we have found a
                                        # better route to it
        else{
                                        # Find relevant node in closed
          index <- searchList(next.state, set.closed)
                                        # Update if we have a better route
          if(set.closed.cost[index] > next.cost){
            print('WARNING: Best route through closed node... Inadmissible heuristic?');
            problem <- setCost(problem, next.state, next.cost)
            set.closed.cost[index] <- next.cost
            set.closed[[index]]$cost <- next.cost
            set.closed[[index]]$prev <- length(set.closed)
          }
        }
      }

    }

    # Element in open with the smallest cost
    if(length(set.open) > 0){
      index <- which.min(set.open.costs + set.open.heuristic)
      current.state <- set.open[[index]]
    }
  }

  results <- list()

  if(finalState(problem, current.state)){
    print("Solucion Encontrada :D")
    print(paste("- Nodos expandidos: ", nodos))

    results <- list(
       "completed" = TRUE,
       "nodos" = nodos,
       "path" = path(problem, current.state, set.closed),
       "problem" = problem
    )
  }
  else if(length(set.open) == 0){
    print("No se ha encontrado una solucion :(")
    print(paste("- Nodos expandidos: ", nodos))

    results <- list(
       "completed" = FALSE,
       "nodos" = nodos,
       "problem" = problem
    )
  }

  return(results)
}
