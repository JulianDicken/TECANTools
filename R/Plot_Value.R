#' Preview the input group layout.
#'
#' This function takes in data and plots it in a spatially similar format as the Plot Layout.
#'
#' @param data the data.frame containing data output by one of the Extract.* functions
#' @param data.preview.value The value to compare in the plot. By default it is "Value"
#' @param datagroups.layout.Rows The row values. By default for a 96-Well Plate this would be LETTERS[1:8] (A, B, C, D, E, F, G, H)
#' @param datagroups.layout.Columns The row values. By default for a 96-Well Plate this would be 1:12 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#' @param data.preview.scaleFactor The scale factor for displaying the preview plot. I find a scale factor of 0.6825 to work for _me_. It might not work well on _your_ display.
#' @param data.preview.dimensions  The dimensions (in cm) of the preview plot. By default A4 paper size in cm: c(29.7, 21.0),
#' @param data.preview.showPreview Whether or not to show the preview.
#' @return ggplot2::plot
#' @export
Plot.Value <- function(
    data,
    data.preview.value        = "Value",
    datagroups.layout.Rows    = LETTERS[1:8],
    datagroups.layout.Columns = 1:12,
    data.preview.scaleFactor = 0.6825,
    data.preview.dimensions  = c(29.7, 21.0),
    data.preview.showPreview = TRUE
) {
  datagroups.ValueGraph <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$Column, y = .data$Row, fill = .data[[data.preview.value]])
  ) +
    ggplot2::geom_tile() +
    ggplot2::coord_fixed(
      ratio = 1
    ) +
    ggplot2::scale_x_discrete(
      breaks = datagroups.layout.Columns
    ) +
    ggplot2::scale_y_discrete(
      breaks = datagroups.layout.Rows,
      limits = rev
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "plasma"
    ) +
    ggplot2::facet_wrap(~ .data$Dataset)

  if (data.preview.showPreview) {
    # stolen from https://stackoverflow.com/a/77190155 and modified
    grid::grid.newpage()
    grid::rectGrob(gp = grid::gpar(fill = "white")) %>%
      grid::grid.draw()

    grid::viewport(
      width = ggplot2::unit(data.preview.dimensions[1], "cm"),
      height = ggplot2::unit(data.preview.dimensions[2], "cm")
    ) %>%
      grid::pushViewport()

    ggplot2::ggplot_build(datagroups.ValueGraph) %>%
      ggplot2::ggplot_gtable() %>%
      grid::grid.draw()
  }
  return(datagroups.ValueGraph)
}
