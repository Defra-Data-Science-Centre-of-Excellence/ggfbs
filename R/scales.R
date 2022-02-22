# Colours in the govuk palette and the order they are called
govuk_colours <- c(
  "blue" = "#1d70b8",
  "turquoise" = "#28a197",
  "green" = "#00703c",
  "light-green" = "#85994b",
  "yellow" = "#ffdd00",
  "orange" = "#f47738",
  "red" = "#d4351c",
  "bright-purple" = "#912b88",
  "black" = "#0b0c0c"
)

# Default colour to use if no govuk scale need be applied.
# Currently the first colour in govuk_colours(): "blue" = "#1d70b8"
default_colour <- govuk_colours[[1]]

#' gov.uk Colours
#'
#' Generate a named vector of colours in the order they would appear when within the
#' automatically generated charts on gov.uk.
#'
#' @details
#' Colours available in palette in order of appearance are:
#'
#' \enumerate{
#'   \item "blue" = "#1d70b8",
#'   \item "turquoise"" = "#28a197"
#'   \item "green" = "#00703c",
#'   \item "light-green" = "#85994b",
#'   \item "yellow" = "#ffdd00",
#'   \item "orange" = "#f47738",
#'   \item "red" = "#d4351c",
#'   \item "bright-purple" = "#912b88",
#'   \item "black" = "#0b0c0c"
#'   }
#'
#' @param ... character or numeric vector denoting the colours of the palette to return. See details
#' for list of colours
#' @return Named vector of colours.
#' @examples
#' # Return all colours in the palette
#' govuk_cols()
#'
#' # Return the first colour in the palette by number
#' govuk_cols(1)
#'
#' # Return multiple colours in palette by number
#' govuk_cols(1:3)
#' govuk_cols(3:6)
#'
#' # Return colours by name
#' govuk_cols("blue")
#' govuk_cols(c("blue", "turquoise", "green", "light-green"))
#' @export
govuk_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(govuk_colours)
  }

  govuk_colours[cols]
}

#' gov.uk Colour Palette
#'
#' Palette function similar to those found within the scales package.
#'
#' @details
#' Calling the govuk_pal will return the palette function passed into
#' \code{\link{scale_colour_govuk}} and \code{\link{scale_fill_govuk.}}.
#'
#' This function can also be used to return a character vector containing \code{x} number of
#' colours with the syntax \code{govuk_pal()(x)}. For example, \code{govuk_pal()(5)} will return a
#' character vector of 5 colours. \strong{Note}: Unlike \code{\link{govuk_cols}}, named colours are
#' not accepted. Only a single number may be passed into the palette.
#'
#' @return
#' Palette function similar to those found in the scales package (e.g.
#' \code{\link[scales]{hue_pal}}).
#' @examples
#'
#' @export
govuk_pal <- function() {
  function(n) {
    col_len <- length(govuk_cols())
    if (n > col_len) {
      warning(
        paste0("Number of breaks (", n, ") exceeds colours in palette (", col_len, ")")
        )
    }
    unname(govuk_cols(1:n))
  }
}

#' Discrete scales in the gov.uk style
#'
#' Provides a discrete colour scheme in the gov.uk type. See \code{\link{govuk_cols}} for the list
#' of colours used within these scales.
#'
#' @details
#' These scales are automatically applied within each chart template when their respective
#' aesthetics is provided within the aesthetic specification.
#'
#' These scales can additionally be added to non-template charts to provide the colour scheme used
#' within the templates.
#' @param ... Other arguments passed on to \link[ggplot2]{discrete_scale}
#' @examples
#' # Add the FBS theme to a basic boxplot
#' ggplot(mpg, aes(class, hwy, colour = drv)) +
#'   geom_boxplot(size = 1.2) +
#'   scale_colour_govuk() +
#'   theme_fbs()
#' @seealso
#' \code{\link[ggplot2]{discrete_scale}}
#' @name scale_colour_govuk
#' @export
scale_colour_govuk <- function(...) {
  ggplot2::discrete_scale("colour", "govuk", govuk_pal(), ...)
}

#' @rdname scale_colour_govuk
#' @export
scale_fill_govuk <- function(...) {
  ggplot2::discrete_scale("fill", "govuk", govuk_pal(), ...)
}
