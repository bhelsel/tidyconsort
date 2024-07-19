#' @title check_params
#' @description Formats the node or edge parameters to be used within the dot diagram
#' @param name The names given to the nodes as a check for labels of equal length in the parameters, Default: NULL
#' @param params The parameters that will be used for the node or edge of the dot diagram
#' @param stringWidth Positive integer giving the line width (in number of characters), Default: 50
#' @return A list of formatted parameters to be added to a node or edge within the dot diagram
#' @details Formats the node or edge parameters to be used within the dot diagram
#' @rdname check_params
#' @importFrom stringr str_wrap
#' @noRd
#' @keywords internal

check_params <- function(name = NULL, params, stringWidth = 50){
  if("style" %in% names(params)) params$style <- sprintf("'%s'", params$style)
  if("label" %in% names(params)) {
    if(length(name) == length(params$label)){
      if(any(grepl("<|>", params$label))){
        params$label <- sprintf("<%s>", params$label)
        if(any(grepl("<br>", params$label))){
          params$label <- gsub("<br>", "<br/>", params$label)
        }
        if(any(grepl("\n", params$label))){
          params$label <- gsub("\n", "<br/>", params$label)
        }
      } else{
        newline <- grepl("\n", params$label)
        params$label <- sprintf("'%s'", params$label)
        params$label[!newline] <- stringr::str_wrap(params$label[!newline], width = stringWidth)
      }
    } else{
      stop(sprintf("The length of 'names' is %s and the length of 'label' is %s", length(name), length(params$label)))
    }
  }
  if("pos" %in% names(params)) params$pos <- sapply(seq(1, length(params$pos), 2), function(i) sprintf("'%s, %s!'", params$pos[i], params$pos[i+1]))
  if("color" %in% names(params)) params$color <- unname(sapply(params$color, check_colors))
  if("fillcolor" %in% names(params)) params$fillcolor <- unname(sapply(params$fillcolor, check_colors))
  if("fontcolor" %in% names(params)) params$fontcolor <- unname(sapply(params$fontcolor, check_colors))

  maxElements <- max(unlist(lapply(params, length)))
  params <-
    lapply(params, function(x){
      if(length(x) < maxElements){
        x <- rep(x, length.out = maxElements)
      } else{
        x
      }
    })

  return(params)
}


#' @title check_colors
#' @description Checks the color formatting within the node or edge parameters
#' @param color A color name or 6-digit hex code, Default: NULL
#' @return A color name or 6-digit hex code formatted for the node or edge parameters.
#' @details Checks the color formatting within the node or edge parameters and returns
#'  a color name or 6-digit hex code with black as the default if the color does not
#'  match the formatting requirements.
#' @rdname check_colors
#' @importFrom stringr str_wrap regex
#' @importFrom grDevices colors
#' @noRd
#' @keywords internal

check_colors <- function(color){
  if(color %in% grDevices::colors()){
    color <- sprintf("'%s'", color)
  } else if(grepl(stringr::regex("^#([A-Fa-f0-9]{6})$"), color)) {
    color <- sprintf("'%s'", color)
  } else {
    color <- "'#000000'"
  }
  return(color)
}


#' @title rotate_labels
#' @description Rotates the labels of the specified nodes from the grViz and htmlwidget object
#' @param graph A dot diagram created from `DiagrammeR::grViz`
#' @param nodes An integer or numeric vector containing the node number(s) indicating which labels to rotate
#' @param angle The angle for which the labels should be rotated, Default: -90
#' @param moveX A numeric value to shift the label along the x-axis
#' @param moveY A numeric value to shift the label along the y-axis
#' @return A graph object of class grViz and htmlwidget with rotated labels
#' @details Rotates the labels of the specified nodes from the grViz and htmlwidget object
#' @seealso
#'  \code{\link[htmltools]{HTML}}
#'  \code{\link[htmlwidgets]{prependContent}}, \code{\link[htmlwidgets]{onStaticRenderComplete}}
#' @rdname rotate_labels
#' @export
#' @importFrom htmltools HTML
#' @importFrom htmlwidgets appendContent onStaticRenderComplete

rotate_labels <- function(graph, nodes, angle = -90, moveX = 0, moveY = 0){

  first_line <- paste0("[", paste0(sprintf('["node%s", ["%s", "%s"]]', nodes, moveX, moveY), collapse = ", "), "]")

  javascript <-
    sprintf('
         const nodeMap = new Map(%s);
         for (const [node, shift] of nodeMap) {
           var theDiv = document.getElementById(node);
           var theText = theDiv.querySelector("text");
           var attrX = theText.getAttribute("x");
           var attrY = theText.getAttribute("y");
           theText.setAttribute("y",parseFloat(attrX)+parseFloat(shift[0]))
           theText.setAttribute("x",(parseFloat(attrY)*-1)+parseFloat(shift[1]))
           theText.setAttribute("style","transform: rotate(%sdeg);")
           theText.setAttribute("dominant-baseline", "middle")
         }
      ', first_line, angle)

  javascript <- htmltools::HTML(javascript)

  graph <- htmlwidgets::appendContent(graph, htmlwidgets::onStaticRenderComplete(javascript))

  return(graph)

}
