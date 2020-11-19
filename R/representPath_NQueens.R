#' @title Mostrar gráfico con solución de un problema n-reinas
#'
#' @param problem Problema a representar
#' @param path Camino de la solución encontrada
#' @importFrom dplyr `case_when`
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#' @export
#' @method representPath NQueens
representPath.NQueens <- function(problem, path){
   # Board
   n <- problem$n
   last.step <- path[[length(path)]]$board
   last.step.melt <- sapply(1:problem$n, function(x) c(x,last.step[x])) %>% t() %>% as.data.frame()
   colnames(last.step.melt) <- c("X", "Y")
   last.step.melt$Queen <- "♛"


   cost.board <- problem$cost.matrix %>% reshape2::melt()
   colnames(cost.board) <- c("X", "Y", "value")


   p.board <- dplyr::left_join(cost.board, last.step.melt, by=c("X","Y")) %>%
      mutate(Queen = case_when(is.na(Queen) ~ "", TRUE ~ Queen))

   img <- ggplot2::ggplot(p.board, ggplot2::aes(x=.data$X, y=.data$Y, fill=.data$value)) +
      ggplot2::geom_tile(color="black") +
      ggplot2::scale_fill_gradient(low = "white", high="darkgreen") +
      ggplot2::geom_text(ggplot2::aes(label=.data$Queen), size=10) +
      ggplot2::scale_x_continuous(breaks=1:n, labels = letters[1:n], minor_breaks = NULL) +
      ggplot2::scale_y_continuous(breaks=1:n, labels = 1:n, minor_breaks = NULL)

   return(img)
}
