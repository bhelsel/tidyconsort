#' @title save_consort_as_pdf
#' @description Saves the consort diagram built in tidyconsort to a pdf file
#' @param graph A dot diagram created from `DiagrammeR::grViz`
#' @param outdir Output directory to save the pdf file
#' @param moveX A numeric value to shift the label along the x-axis
#' @param moveY A numeric value to shift the label along the y-axis
#' @return Saves the consort diagram as a pdf document
#' @details Saves the consort diagram built in tidyconsort to a pdf file
#' @seealso
#'  \code{\link[htmlwidgets]{saveWidget}}
#'  \code{\link[xml2]{read_xml}}, \code{\link[xml2]{xml_text}}, \code{\link[xml2]{xml_find_all}}, \code{\link[xml2]{xml_ns}}, \code{\link[xml2]{xml_attr}}, \code{\link[xml2]{write_xml}}
#'  \code{\link[DiagrammeRsvg]{export_svg}}
#'  \code{\link[stringr]{str_split}}, \code{\link[stringr]{str_remove}}, \code{\link[stringr]{str_match}}
#'  \code{\link[rsvg]{rsvg}}
#' @rdname save_consort_as_pdf
#' @export
#' @import magrittr
#' @importFrom htmlwidgets saveWidget
#' @importFrom xml2 read_html xml_text xml_find_first read_xml xml_ns xml_attr write_xml
#' @importFrom DiagrammeRsvg export_svg
#' @importFrom stringr str_split str_remove_all str_match str_split_fixed
#' @importFrom rsvg rsvg_pdf


save_consort_as_pdf <- function(graph, outdir, moveX = 0, moveY = 0){

  temp_path <- file.path(outdir, "consort_temp")

  if(!dir.exists(temp_path)) {
    dir.create(temp_path)
  } else {
    unlink(temp_path, recursive = TRUE)
    dir.create(temp_path)
  }

  tmpfilehtml <- file.path(temp_path, "consort.html")
  tmpfilesvg <- file.path(temp_path, "consort.svg")

  htmlwidgets::saveWidget(graph, file = tmpfilehtml)

  htmldata <- xml2::read_html(tmpfilehtml)
  js <- xml2::xml_text(xml2::xml_find_first(htmldata, "//div[contains(@class, \"grViz\")]//following-sibling::script"))
  svg <- DiagrammeRsvg::export_svg(graph)
  svg <- xml2::read_xml(svg)

  jsnode <- js %>%
    stringr::str_match("const nodeMap = new Map\\(\\[(.*)\\]\\);") %>%
    magrittr::extract2(2) %>%
    stringr::str_split(",\\s", simplify = TRUE) %>%
    stringr::str_remove_all("\\[|\"|]") %>%
    {.[grepl("node", .)]}

  len <- length(jsnode)

  if(len != length(moveX)) moveX <- rep(moveX, 3)
  if(len != length(moveY)) moveY <- rep(moveY, 3)

  for (i in 1:len) {
    matsp <- stringr::str_split_fixed(jsnode[[i]][1], ",", 3)
    xml_text_node <- xml2::xml_find_first(svg, paste0("//d1:g[@id=\"", matsp[, 1], "\"]//d1:text"), xml2::xml_ns(svg))
    attr_x <- xml2::xml_attr(xml_text_node, "x")
    attr_y <- xml2::xml_attr(xml_text_node, "y")
    xml2::xml_attr(xml_text_node, "x") <- (as.numeric(attr_y) * -1) + as.numeric(moveX[i])
    xml2::xml_attr(xml_text_node, "y") <- as.numeric(attr_x) + 2 + as.numeric(moveY[i])
    xml2::xml_attr(xml_text_node, "transform") <- "rotate(-90)"
  }

  xml2::write_xml(svg, file = tmpfilesvg)
  rsvg::rsvg_pdf(tmpfilesvg, file.path(outdir, "consort.pdf"))
  unlink(temp_path, recursive = TRUE)
}


