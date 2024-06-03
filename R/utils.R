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
      params$label <- stringr::str_wrap(sprintf("'%s'", params$label), width = stringWidth)
    } else{
      stop(sprintf("The length of 'names' is %s and the length of 'label' is %s", length(name), length(params$label)))
    }
  }
  if("pos" %in% names(params)) params$pos <- sapply(seq(1, length(params$pos), 2), function(i) sprintf("'%s, %s!'", params$pos[i], params$pos[i+1]))

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
