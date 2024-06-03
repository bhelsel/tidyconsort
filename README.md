
# tidyconsort

<!-- badges: start -->
<!-- badges: end -->

The goal of tidyconsort is to make creating consort diagrams easier with the DiagrammeR package.

## Installation

You can install the development version of tidyconsort like so:

``` r
devtools::install_github("bhelsel/tidyconsort")
```

## Example

This is a basic example of adding nodes and edges:

``` r
library(tidyconsort)

create_graph() |>
  set_node_parameters(shape = "box", fontsize = 12,
                      fontname = "Arial", style = "rounded,filled",
                      fillcolor = "White") |>
  add_node(name = "n1", label = "First Node", pos = c(8, 8)) |>
  add_node(name = "n2", label = "Second Node", pos = c(4, 8)) |>
  set_edge_parameters(arrowhead = "normal", arrowtail = "normal") |>
  add_edge(from = "n1", to = "n2", vebose = TRUE)

```

## Template

Here is a template to get started.

``` r

use_template_1(
  eligible = 0,
  randomized = 0,
  timepoints = 2,
  labels = c("Timepoint 1", "Timepoint 2"),
  descriptions = c("Baseline Testing", "Post-intervention"),
  intervention = "Intervention",
  intervention_n = 0,
  control = "Control",
  control_n = 0,
  verbose = FALSE
)

```

<img src="inst/extdata/consort.png" align="left" width="50%" height="50%"/>







