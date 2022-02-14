#' Farm Business Survey Theme
#'
#' Apply the Farm Business Survey (FBS) theme to any plot
#'
#' @details
#' Applies the default FBS theme to a plot. By default this is applied to any of the chart templates
#' within this package.
#'
#' This function enables the application of the FBS theme to charts which do not have a set
#' template. This can be combined with the scale_*_govuk set of functions to match the theming and
#' colour scheme used within the templates.
#' @param horizontal Set to \code{TRUE} to adjust the theme for horizontal plots.
#' @param text_scale Scale up or down the text size within the plot.
#' @param font Change the default font.
#' @examples
#' # Add the FBS theme to a basic boxplot
#' ggplot(mpg, aes(class, hwy, colour = drv)) +
#'   geom_boxplot(size = 1.2) +
#'   scale_colour_govuk() +
#'   theme_fbs()
#' @seealso
#'  \code{\link[ggplot2]{theme}}
#' @rdname theme_fbs
#' @export
theme_fbs <- function(horizontal = FALSE,
                      text_scale = 1,
                      font = "sans") {

  t <- ggplot2::theme(

    # Text format
    # Chart title
    plot.title = ggplot2::element_text(
      family = font,
      size = 22 * text_scale,
      face = "bold",
      color = "#000000"
      # vjust = -8,
      # margin = grid::unit(c(-10,0,0,0), "mm")
    ),
    plot.title.position = "panel",
    plot.caption.position = "panel",

    # Chart subtitle
    plot.subtitle = ggplot2::element_text(vjust = -10, size = 20),

    # Chart caption, set blank, can be added to chart when editing the final theme
    plot.caption = ggplot2::element_blank(),

    # Legend format
    # Sets the position and alignment to the top, removes a title and background, and sets text.
    # May need tweaking before finalising a plot.
    legend.position = "top",
    legend.justification = c(1, 0),
    legend.text.align = 0,
    legend.direction = "horizontal",
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = font, size = 20 * text_scale, color = "#000000"),

    # Axis format
    # Text font, size and colour for the axis text, sets the margins and removes lines and ticks.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family = font, size = 18 * text_scale, color = "#000000"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_line(color = "#000000"),

    # Grid lines
    # Removes all minor gridlines and adds major y gridlines. x gridlins need to be set manually
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),

    # Blank background
    panel.background = ggplot2::element_blank(),

    # Strip background, sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = 22 * text_scale, hjust = 0)
  )

  if (horizontal) {
    t <- t +
      theme(
        # Gridlines
        panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.major.y = ggplot2::element_blank(),

        # Axis lines
        axis.line.y = ggplot2::element_blank(),

        # Axis text
        axis.text.y = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
        axis.title.x = ggplot2::element_text(size = 20)
      )
  }

  t
}
