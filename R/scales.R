set_palette <- function(pal) {
  switch(pal,
    "gss" = gss_pal(),
    "govuk" = govuk_pal(),
    stop("Unknown Palette")
  )
}

#' Discrete scales with default styling
#'
#' Provides a discrete colour scheme in the gov.uk style See
#' \code{\link{ggfbs_colours}} for the list of colours used within these scales.
#'
#' @details
#' These scales apply the default colour scheme for publishing on gov.uk,
#' defaulting to \code{gss}.
#'
#' These scales are automatically applied within each chart template when their respective
#' aesthetics is provided within the aesthetic specification.
#'
#' @param palette Character string indicating the colour scheme to apply to
#' scale. Default = gss
#' @param ... Other arguments passed on to \link[ggplot2]{discrete_scale}
#' @examples
#' \dontrun{
#' # Add the FBS theme to a basic boxplot
#' ggplot(mpg, aes(class, hwy, colour = drv)) +
#'   geom_boxplot(size = 1.2) +
#'   scale_colour_govuk() +
#'   theme_fbs()
#' }
#' @seealso
#' \code{\link[ggplot2]{discrete_scale}}
#' @name scale_colour_govuk
#' @export
scale_colour_govuk <- function(palette = getOption("ggfbs.default_palette"), ...) {
  ggplot2::discrete_scale("colour", "govuk", set_palette(palette), ...)
}

#' @rdname scale_colour_govuk
#' @export
scale_fill_govuk <- function(palette = getOption("ggfbs.default_palette"), ...) {
  ggplot2::discrete_scale("fill", "govuk", set_palette(palette), ...)
}
