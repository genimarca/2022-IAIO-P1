#' @title Represent Solution for Board Problem
#'
#' @param problem Problema a representar
#' @param path Camino de la solución
#' @importFrom dplyr `case_when`
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#' @export
#' @method representPath Board
representPath.Board <- function(problem, path){
   # Board
   p.board <- problem$board %>%
      reshape2::melt() %>%
      dplyr::mutate(value=dplyr::case_when(value == Inf ~ NaN,
                                           TRUE ~ 0))
   colnames(p.board) <- c("X", "Y", "value")

   # Cost
   cost.tiles <- problem$cost.matrix %>% reshape2::melt()
   colnames(cost.tiles) <- c("X", "Y", "cost")

   # Inner join
   p.board <- dplyr::left_join(p.board, cost.tiles, by=c("X","Y"))
   n.pasos <- length(path)
   p.board$pasos <- 1
   gif.board <- p.board
   p.board <- p.board %>%
      dplyr::mutate(
         value = dplyr::case_when(X == path[[1]]$position[1] & Y == path[[1]]$position[2] ~ 1.0,
                                  TRUE ~ value*1.0))

   for(i in 2:n.pasos){
      paso <- path[[i]]
      p.board$pasos <- i

      p.board <- p.board %>%
         dplyr::mutate(value = dplyr::case_when(X == paso$position[1] & Y == paso$position[2] ~ i*1.0,
                                                TRUE ~ value))

      gif.board <- rbind(gif.board, p.board)
   }

   img <- ggplot2::ggplot(gif.board, ggplot2::aes(x=.data$X, y=.data$Y, fill=.data$cost, color=.data$value)) +
      ggplot2::geom_tile(ggplot2::aes(width=0.95, height=0.9), size=1.8) +
      ggplot2::scale_fill_gradient2(low = "red", mid = "yellow", midpoint = n.pasos/2, na.value = "grey",
                                    high = "darkblue", space = "Lab") +
      ggplot2::scale_color_gradientn(colours = c("white", "white", "palegreen", "darkgreen"),
                                     values = scales::rescale(c(0,0.1,1,n.pasos),to=c(0,1)),
                                     na.value = "grey") +
      ggplot2::scale_x_continuous(breaks=1:10, labels = 1:10, minor_breaks = NULL) +
      ggplot2::scale_y_continuous(breaks=1:10, labels = 1:10, minor_breaks = NULL)
   gif <- img +
      gganimate::transition_states(.data$pasos)

   return(list("img"=img, "gif"=gif))
}
