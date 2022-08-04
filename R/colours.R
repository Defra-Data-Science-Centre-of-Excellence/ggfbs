# GSS colours
gss_colours <- c(
  "dark-blue" = "#12436d",
  "turquoise" = "#28a197",
  "dark-pink" = "#801650",
  "orange" = "#f46a25",
  "dark-grey" = "#3d3d3d",
  "plum" = "#a285d1"
)

# gov.uk colours
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

#' Colour functions
#'
#' Generate a named vector of colours in the order they would appear within the
#' automatically generated charts.
#'
#' Default colour palette: gss.
#'
#' @param ... character or numeric vector denoting the colours of the palette to return.
#' @examples
#' # Return all colours in the palette
#' gss_cols()
#'
#' # Return the first colour in the palette by number
#' gss_cols(1)
#'
#' # Return multiple colours in palette by number
#' gss_cols(1:3)
#' gss_cols(3:6)
#'
#' # Return colours by name
#' gss_cols("dark-blue")
#' gss_cols(c("dark-blue", "turquoise", "dark-pink", "orange"))
#' @name ggfbs_colours
#' @export
gss_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(gss_colours)
  }

  gss_colours[cols]
}

#' @rdname ggfbs_colours
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
#' Palette functions similar to those found within the scales package. (e.g.
#' \code{\link[scales]{hue_pal}}).
#'
#' Default colour palate: gss.
#'
#' @details
#' Calling these functions will return the palette function passed into
#' \code{\link{scale_colour_govuk}} and \code{\link[=scale_colour_govuk]{scale_fill_govuk}}.
#'
#' This function can also be used to return a character vector containing \code{x} number of
#' colours with the syntax \code{gss_pal()(x)}. For example, \code{gss_pal()(5)} will return a
#' character vector of 5 colours. \strong{Note}: Unlike \code{\link{gss_cols}}, named colours are
#' not accepted. Only a single number may be passed into the palette.
#' @examples
#' # Return the palette function
#' gss_pal()
#' govuk_pal()
#'
#' # Return a vector of colours
#' gss_pal()(4)
#' govuk_pal()(4)
#' @name pallettes
#' @export
gss_pal <- function() {
  function(n) {
    col_len <- length(gss_cols())
    if (n > col_len) {
      warning(
        paste0("Number of breaks (", n, ") exceeds colours in palette (", col_len, ")")
      )
    }
    unname(gss_cols(1:n))
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
