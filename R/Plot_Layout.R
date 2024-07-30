#' Preview the input group layout.
#'
#' This function takes in datagroups and plots them to visually communicate the datagroups.
#'
#' @param datagroups The user-defined data groups. They follow the pattern list(group1 = list(pattern1, pattern2), group2 = list(pattern1, pattern2))
#' @param datagroups.layout.Rows The row values. By default for a 96-Well Plate this would be LETTERS[1:8] (A, B, C, D, E, F, G, H)
#' @param datagroups.layout.Columns The row values. By default for a 96-Well Plate this would be 1:12 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#' @param layoutGraph.scaleFactor The scale factor for displaying the preview plot. I find a scale factor of 0.6825 to work for _me_. It might not work well on _your_ display.
#' @param layoutGraph.dimensions  The dimensions (in cm) of the preview plot. By default A4 paper size in cm: c(29.7, 21.0),
#' @param .datagroups.prepostfix This adds some sensible defaults to the regexp. Only ever touch this if you understand the following pattern: ^(?=.*[A-Za-z])(?=.*\d)[A-Za-z\\d]{8,}$
#' @return ggplot2::plot
#' @export
Plot.Layout <- function(
    datagroups,
    datagroups.layout.Rows    = LETTERS[1:8],
    datagroups.layout.Columns = 1:12,
    layoutGraph.scaleFactor = 0.6825,
    layoutGraph.dimensions = c(29.7, 21.0),

    .datagroups.prepostfix = TRUE
) {

  if (.datagroups.prepostfix) {
    datagroups <- purrr::map(
      datagroups, ~ purrr::map(.x, ~ paste0("^(", .x, "){1}$"))
    )
  }
  datagroups.layout <- data.frame(
    Row = rep(
      datagroups.layout.Rows, each = length(datagroups.layout.Columns)
    )
  )
  datagroups.layout <- datagroups.layout %>%
    cbind(
      Column = rep(datagroups.layout.Columns, times = length(datagroups.layout.Rows))
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), factor))

  datagroups.layout <- datagroups.layout %>%
    dplyr::mutate(Coordinate = paste0(.data$Row, .data$Column))

  for (datagroup.name in names(datagroups)) {
    datagroup <- datagroups[[datagroup.name]]
    datagroup.conditions <- Utility.case_when_from_datagroup_rules(datagroup, "Coordinate")

    # here I am doing codegen which is bad but IMO the best way of doing this. If you know a better way, PR it.
    datagroups.layout <- datagroups.layout %>%
      dplyr::mutate(!!datagroup.name := eval(parse(text = datagroup.conditions)))
  }
  datagroups.layout.labels <- datagroups.layout %>%
    dplyr::select(
      -.data$Row, -.data$Column, -.data$Coordinate
    ) %>%
    dplyr::mutate(
      Label = apply(dplyr::across(dplyr::everything()), 1, paste, collapse = "\n ")
    ) %>%
    dplyr::select(
      .data$Label
    )
  datagroups.layout <- cbind(datagroups.layout, datagroups.layout.labels)

  datagroups.count <- length(unique(datagroups.layout$Label))
  datagroups.fillColors <- colorspace::qualitative_hcl(datagroups.count, "Dark 3") %>% colorspace::lighten(0.15)

  layoutGraph.dimensions <- layoutGraph.dimensions * layoutGraph.scaleFactor
  layoutGraph.dimensions[3] <- sqrt(layoutGraph.dimensions[1]^2 + layoutGraph.dimensions[2]^2)

  plot.layoutGraph <- ggplot2::ggplot(
    datagroups.layout,
    ggplot2::aes(x = .data$Column, y = .data$Row, fill = .data$Label, label = .data$Label)
  ) +
    ggplot2::geom_tile() +
    shadowtext::geom_shadowtext(
      color = "white",
      size = layoutGraph.dimensions[3] / 12
    ) +
    ggplot2::scale_fill_manual(
      values = datagroups.fillColors
    ) +
    ggplot2::scale_color_manual(
      values = datagroups.fillColors
    ) +
    ggplot2::scale_y_discrete(
      limits = rev
    ) +
    ggplot2::coord_fixed(
      ratio = 1
    ) +
    ggplot2::theme(
      legend.position = "none"
    )

  return(plot.layoutGraph)
}
