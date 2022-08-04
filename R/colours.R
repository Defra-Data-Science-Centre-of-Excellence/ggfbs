# Default GSS colours
gss_colours <- c(
  "dark-blue" = "#12436d",
  "turquoise" = "#28a197",
  "dark-pink" = "#801650",
  "orange" = "#f46a25",
  "dark-grey" = "#3d3d3d",
  "plum" = "#a285d1"
)

# Legacy gov.uk colours
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

#' Colour functions
#'
#' Generate a named vector of colours in the order they would appear within the
#' automatically generated charts.
#'
#' Default colour palette: gss.
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
#' @name colours
#' @export
gss_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(gss_colours)
  }

  gss_colours[cols]
}

#' @rdname colours
#' @export
govuk_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(govuk_colours)
  }

  govuk_colours[cols]
}

#' Colour Palettes
#'
#' Palette functions similar to those found within the scales package.
#'
#' Default colour palate: gss.
#'
#' @details
#' Calling the these functions will return the palette function passed into
#' \code{\link{scale_colour_govuk}} and \code{\link[=scale_colour_govuk]{scale_fill_govuk}}.
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
#' @name pallettes
#' @export
gss_pal <- function() {
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

#' @rdname pallettes
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
