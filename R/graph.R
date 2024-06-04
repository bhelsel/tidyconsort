#' @title create_graph
#' @description Creates an empty dot diagram from `DiagrammeR::grViz`
#' @param splines Controls how edges are represented (\url{https://graphviz.org/docs/attrs/splines/}), Default: 'ortho'
#' @param layout Choose the layout engine to use (\url{https://graphviz.org/docs/attrs/layout/}), Default: 'neato'
#' @param outputorder Specify the order in which nodes and edges are drawn (\url{https://graphviz.org/docs/attr-types/outputMode/}), Default: 'edgesfirst'
#' @param width Minimum width of the diagram, Default: 8
#' @param height Minimum height of the diagram, Default: 8
#' @param ratio Sets the aspect ratio of the height and width of the drawing (\url{https://graphviz.org/docs/attrs/ratio/}), Default: 'auto'
#' @param verbose Prints out the dot diagram in html format, Default: FALSE
#' @return A graph object of class grViz and htmlwidget
#' @details Creates an empty dot diagram from `DiagrammeR::grViz`
#' @seealso
#'  \code{\link[DiagrammeR]{grViz}}
#' @rdname create_graph
#' @export
#' @importFrom DiagrammeR grViz


create_graph <- function(splines = "ortho", layout = "neato", outputorder = "edgesfirst",
                         width = 8, height = 8, ratio = "auto", verbose = FALSE){
  size <- sprintf("'%s,%s!'", width, height)
  graphInput <- sprintf(
    "splines = %s, layout = %s, outputorder = %s, size = %s, ratio = %s",
    splines, layout, outputorder, size, ratio
    )
  diagram <- sprintf("digraph TD {\n graph [\n  %s \n ]\n}", graphInput)
  if(verbose) message(diagram)
  return(DiagrammeR::grViz(diagram))
}
