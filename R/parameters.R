#' @title set_node_parameters
#' @description Sets multiple parameters of a node until those parameters are altered.
#' @param graph A dot diagram created from `DiagrammeR::grViz`
#' @param verbose Prints out the dot diagram in html format, Default: FALSE
#' @param ... Parameters passed to \code{\link[DiagrammeR]{grViz}}
#' @return A graph object of class grViz and htmlwidget
#' @details Sets multiple parameters of a node until those parameters are altered.
#' @seealso
#'  \code{\link[DiagrammeR]{grViz}}
#' @rdname set_node_parameters
#' @export
#' @importFrom DiagrammeR grViz

set_node_parameters <- function(graph, verbose = FALSE, ...){
  params <- list(...)
  params <- check_params(params = params)
  node_params <- paste0(names(params), " = ", params, collapse = ", ")
  node <- sprintf(" node [\n  %s \n ]", node_params)
  diagram <- paste0(gsub("}", "", graph$x$diagram), node, "\n}")
  if(verbose) message(diagram)
  return(DiagrammeR::grViz(diagram))
}


#' @title set_edge_parameters
#' @description Sets multiple parameters of an edge until those parameters are altered.
#' @param graph A dot diagram created from `DiagrammeR::grViz`
#' @param verbose Prints out the dot diagram in html format, Default: FALSE
#' @param ... Parameters passed to \code{\link[DiagrammeR]{grViz}}
#' @return A graph object of class grViz and htmlwidget
#' @details Sets multiple parameters of an edge until those parameters are altered.
#' @seealso
#'  \code{\link[DiagrammeR]{grViz}}
#' @rdname set_edge_parameters
#' @export
#' @importFrom DiagrammeR grViz

set_edge_parameters <- function(graph, verbose = FALSE, ...){
  params <- list(...)
  params <- check_params(params = params)
  edge_params <- paste0(names(params), " = ", params, collapse = ", ")
  edge <- sprintf(" edge [\n  %s \n ]", edge_params)
  diagram <- paste0(gsub("}", "", graph$x$diagram), edge, "\n}")
  if(verbose) message(diagram)
  return(DiagrammeR::grViz(diagram))
}
