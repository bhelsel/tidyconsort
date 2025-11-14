#' @title use_template_1
#' @description A basic example of a CONSORT diagram created with tidyconsort
#' @param eligible Number of study participants eligible for the study, Default: 0
#' @param randomized Number of study participants randomized during the study, Default: 0
#' @param timepoints The number of time points to include in the CONSORT diagram (range: 2-4), Default: 2
#' @param labels The labels for the time points (e.g., baseline, 6 months, etc.), Default: c("Timepoint 1", "Timepoint 2")
#' @param descriptions A description of the trial phase completed (e.g., active intervention, no contact follow-up), Default: c("Baseline Testing", "Post-intervention")
#' @param intervention A name or descriptor for the intervention group, Default: 'Intervention'
#' @param intervention_n The number of individuals completing each trial phase (this should match the length of the descriptions), Default: 0
#' @param control A name or descriptor for the intervention group (e.g., placebo), Default: 'Control'
#' @param control_n The number of individuals completing each trial phase (this should match the length of the descriptions), Default: 0
#' @param verbose Prints out the dot diagram in html format, Default: FALSE
#' @return A graph object of class grViz and htmlwidget
#' @details A basic example of a CONSORT diagram created with tidyconsort. The
#'     default is a two-arm randomized trial with changeable parameters to add up
#'     to 4 time points.
#' @examples
#' \dontrun{
#' if(interactive()){
#'   use_template_1()
#'  }
#' }
#' @rdname use_template_1
#' @export

use_template_1 <- function(
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
) {
  if (timepoints == 0) {
    stop("You must provide a positive integer for the timepoints argument.")
  }

  timepoint_names <- paste0("t", 1:timepoints)
  if (length(labels) != timepoints) {
    stop("The length of the labels must equal timepoints.")
  }
  timepoint_positions <- c(4, 6, 4, 5, 4, 4, 4, 3)

  group_names <- paste0("g", 1:(timepoints * 2))
  group_positions <- list(
    intervention = c(1.5, 6, 1.5, 5, 1.5, 4, 1.5, 3),
    control = c(6.5, 6, 6.5, 5, 6.5, 4, 6.5, 3)
  )

  if (length(intervention_n) != timepoints & all(intervention_n != 0)) {
    stop(sprintf(
      "Expecting %s N values for intervention_n but only %s found.",
      timepoints,
      length(intervention_n)
    ))
  } else if (all(intervention_n == 0)) {
    intervention_n <- rep(intervention_n, timepoints)
  }

  if (length(control_n) != timepoints & all(control_n != 0)) {
    stop(sprintf(
      "Expecting %s N values for control_n but only %s found.",
      timepoints,
      length(control_n)
    ))
  } else if (all(control_n == 0)) {
    control_n <- rep(control_n, timepoints)
  }

  create_graph() |>
    set_node_parameters(
      shape = "box",
      fontsize = 12,
      fontname = "Arial",
      style = "rounded,filled",
      width = 3.5,
      height = 0.5,
      fillcolor = "#FFFFFF"
    ) |>
    add_node(
      name = c("eligible", "randomized"),
      label = c(
        sprintf("Eligible (n = %s)", eligible),
        sprintf("Randomized (n = %s)", randomized)
      ),
      pos = c(4, 8, 4, 7)
    ) |>
    add_node(
      name = c("i1", "i2"),
      pos = c(1.5, 7, 6.5, 7),
      width = 0,
      height = 0,
      style = "invis",
      label = c("", ""),
      shape = "square"
    ) |>
    add_node(
      name = timepoint_names,
      label = labels,
      pos = timepoint_positions[1:(2 * timepoints)],
      color = "#FFFFFF",
      width = 0
    ) |>
    add_node(
      name = "intervention",
      label = sprintf("Allocated to %s", intervention),
      pos = c(1.5, 6.5),
      color = "#FFFFFF",
      height = 0.25
    ) |>
    add_node(
      name = "control",
      label = sprintf("Allocated to %s", control),
      pos = c(6.5, 6.5),
      color = "#FFFFFF",
      height = 0.25
    ) |>
    add_node(
      name = group_names[1:timepoints],
      label = paste0(
        "Completed ",
        tolower(descriptions),
        " (n = ",
        intervention_n,
        ")"
      ),
      pos = group_positions$intervention[1:(2 * timepoints)]
    ) |>
    add_node(
      name = group_names[(timepoints + 1):length(group_names)],
      label = paste0(
        "Completed ",
        tolower(descriptions),
        " (n = ",
        control_n,
        ")"
      ),
      pos = group_positions$control[1:(2 * timepoints)]
    ) |>
    set_edge_parameters(
      color = "Black",
      arrowhead = "normal",
      arrowtail = "none",
      style = "filled",
      tailport = "center"
    ) |>
    add_edge(from = "eligible", to = "randomized") |>
    add_edge(from = c("i1", "i2"), to = c("intervention", "control")) |>
    set_edge_parameters(arrowhead = "none") |>
    add_edge(from = c("randomized", "randomized"), to = c("i1", "i2")) |>
    add_edge(
      from = group_names[1:timepoints][
        1:(length(group_names[1:timepoints]) - 1)
      ],
      to = group_names[1:timepoints][2:length(group_names[1:timepoints])]
    ) |>
    add_edge(
      from = group_names[(timepoints + 1):length(group_names)][
        1:(length(group_names[(timepoints + 1):length(group_names)]) - 1)
      ],
      to = group_names[(timepoints + 1):length(group_names)][
        2:length(group_names[(timepoints + 1):length(group_names)])
      ],
      verbose = verbose
    )
}
