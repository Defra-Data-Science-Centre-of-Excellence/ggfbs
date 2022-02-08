# Bar plot
GeomfbsBar <- ggplot2::ggproto("GeomfbsBar", ggplot2::GeomBar,
                               default_aes = ggplot2::aes(
                                 colour = NA, fill = default_colour,
                                 size = 0.5, linetype = 1, alpha = NA)
                               )

#' geom_fbsbar
#'
#' Wrapper for \link[ggplot2]{geom_bar} using default FBS style.
#'
#' @details
#' See \code{\link[ggplot2]{geom_bar}} for function details.
#' @inheritParams ggplot2::geom_bar
#' @inheritParams ggplot2::layer
#' @export
geom_fbsbar <- function(mapping = NULL,
                        data = NULL,
                        stat = "identity",
                        position = "dodge",
                        ...,
                        width = 0.75,
                        na.rm = FALSE,
                        orientation = NA,
                        show.legend = NA,
                        inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomfbsBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

# Line plot
GeomfbsLine <- ggplot2::ggproto("GeomfbsLine", ggplot2::GeomLine,
                                default_aes = ggplot2::aes(
                                  colour = default_colour,
                                  size = 1.5, linetype = 1, alpha = NA)
)

#' geom_fbsline
#'
#' Wrapper for \link[ggplot2]{geom_line} using default FBS style.
#'
#' @details
#' See \code{\link[ggplot2]{geom_line}} for function details.
#' @inheritParams ggplot2::geom_line
#' @inheritParams ggplot2::layer
#' @export
geom_fbsline <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         na.rm = FALSE,
                         orientation = NA,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomfbsLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      ...)
  )
}
