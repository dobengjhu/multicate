#' Custom ggplot2 Theme
#'
#' A reusable custom ggplot2 theme for consistent styling.
#'
#' @export
theme_MH <- ggplot2::theme(
  plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.15), color = "lightcyan4",
                                        hjust = 0, vjust = 2),
  plot.background = ggplot2::element_rect(fill = "white"),
  panel.background = ggplot2::element_rect(colour = "grey80", fill = "white"),
  panel.grid.major = ggplot2::element_line(colour = "grey90", size = 0.20),
  axis.text = ggplot2::element_text(size = 12, colour = "grey50", vjust = 0),
  axis.text.x = ggplot2::element_text(angle = 90),
  axis.ticks.x = ggplot2::element_line(colour = "grey50"),
  axis.ticks.y = ggplot2::element_line(colour = "grey50"),
  axis.ticks.length = ggplot2::unit(0.25, "cm"),
  axis.title.y = ggplot2::element_text(angle = 90, vjust = 0),
  axis.title.x = ggplot2::element_text(angle = 0)
)
