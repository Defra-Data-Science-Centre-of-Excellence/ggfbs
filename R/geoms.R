# Bar plot
GeomfbsBar <- ggplot2::ggproto("GeomfbsBar", ggplot2::GeomBar,
                               default_aes = ggplot2::aes(
                                 colour = NA, fill = default_colour,
                                 size = 0.5, linetype = 1, alpha = NA)
                               )

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mapping PARAM_DESCRIPTION, Default: NULL
#' @param data PARAM_DESCRIPTION, Default: NULL
#' @param stat PARAM_DESCRIPTION, Default: 'identity'
#' @param position PARAM_DESCRIPTION, Default: 'dodge'
#' @param ... PARAM_DESCRIPTION
#' @param width PARAM_DESCRIPTION, Default: 0.75
#' @param na.rm PARAM_DESCRIPTION, Default: FALSE
#' @param orientation PARAM_DESCRIPTION, Default: NA
#' @param show.legend PARAM_DESCRIPTION, Default: NA
#' @param inherit.aes PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ggplot2]{geom_bar}}
#' @rdname geom_fbsbar
#' @export
#' @importFrom ggplot2 layer
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
