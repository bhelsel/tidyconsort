#' @title add_edge
#' @description Adds an edge or edges to connect two or more nodes together
#' @param graph A dot diagram created from `DiagrammeR::grViz`
#' @param from The node or nodes to draw the edge from. Multiple nodes can be specified using `c()`
#' @param to The node or nodes to draw the edge to. Multiple nodes can be specified using `c()`
#' @param verbose Prints out the dot diagram in html format, Default: FALSE
#' @param ... Parameters passed to \code{\link[DiagrammeR]{grViz}}
#' @return A graph object of class grViz and htmlwidget
#' @details Adds an edge or edges to connect two or more nodes together
#' @seealso
#'  \code{\link[DiagrammeR]{grViz}}
#' @rdname add_edge
#' @export
#' @importFrom DiagrammeR grViz

add_edge <- function(graph, from, to, verbose = FALSE, ...){
  edge <- paste0(sprintf(" %s->%s", from, to), collapse = "\n")
  diagram <- paste0(gsub("}", "", graph$x$diagram), edge, "\n}")
  if(verbose) message(diagram)
  return(DiagrammeR::grViz(diagram))
}

#' @title add_node
#' @description Adds a node or nodes to a dot diagram
#' @param graph A dot diagram created from `DiagrammeR::grViz`
#' @param name The names given to the node or nodes. Multiple nodes can be specified using `c()`
#' @param stringWidth Positive integer giving the line width (in number of characters), Default: 50
#' @param verbose Prints out the dot diagram in html format, Default: FALSE
#' @param ... Parameters passed to \code{\link[DiagrammeR]{grViz}}
#' @return A graph object of class grViz and htmlwidget
#' @details Adds a node or nodes to a dot diagram
#' @seealso
#'  \code{\link[DiagrammeR]{grViz}}
#' @rdname add_node
#' @export
#' @importFrom DiagrammeR grViz

add_node <- function(graph, name, stringWidth = 50, verbose = FALSE, ...){
  params <- list(...)
  params <- check_params(name = name, params = params, stringWidth = stringWidth)

  if(length(name) == 1){
    node_params <- paste0(names(params), " = ", params, collapse = ", ")
    node <- sprintf(" %s [\n  %s \n ]", name, node_params)
  }

  if(length(name) > 1){
    combine_attributes <- function(i, params) {
      names(params) |>
        sapply(function(attr_name) {
          paste(attr_name, "=", params[[attr_name]][i])
        }, USE.NAMES = FALSE) |>
        paste(collapse = ", ")
    }

    create_dot_string <- function(i, params) {
      sprintf(" %s [\n  %s \n ]", name[i], combine_attributes(i, params))
    }
    node <- paste0(sapply(seq_along(name), create_dot_string, params), collapse = "\n")
  }

  diagram <- paste0(gsub("}", "", graph$x$diagram), node, "\n}")
  if(verbose) message(diagram)
  return(DiagrammeR::grViz(diagram))

}
