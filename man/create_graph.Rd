% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph.R
\name{create_graph}
\alias{create_graph}
\title{create_graph}
\usage{
create_graph(
  splines = "ortho",
  layout = "neato",
  outputorder = "edgesfirst",
  width = 8,
  height = 8,
  ratio = "auto",
  verbose = FALSE
)
}
\arguments{
\item{splines}{Controls how edges are represented (\url{https://graphviz.org/docs/attrs/splines/}), Default: 'ortho'}

\item{layout}{Choose the layout engine to use (\url{https://graphviz.org/docs/attrs/layout/}), Default: 'neato'}

\item{outputorder}{Specify the order in which nodes and edges are drawn (\url{https://graphviz.org/docs/attr-types/outputMode/}), Default: 'edgesfirst'}

\item{width}{Minimum width of the diagram, Default: 8}

\item{height}{Minimum height of the diagram, Default: 8}

\item{ratio}{Sets the aspect ratio of the height and width of the drawing (\url{https://graphviz.org/docs/attrs/ratio/}), Default: 'auto'}

\item{verbose}{Prints out the dot diagram in html format, Default: FALSE}
}
\value{
A graph object of class grViz and htmlwidget
}
\description{
Creates an empty dot diagram from `DiagrammeR::grViz`
}
\details{
Creates an empty dot diagram from `DiagrammeR::grViz`
}
\seealso{
\code{\link[DiagrammeR]{grViz}}
}
