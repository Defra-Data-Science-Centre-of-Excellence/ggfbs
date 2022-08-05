# Helper function for the `continuous_format` argument
format_label_continuous <- function(label_format, ...) {
  switch(label_format,
    "identity" = scales::label_number(...),
    "comma" = scales::label_comma(...),
    "percent" = scales::label_percent(..., accuracy = 1),
    "abbreviated" = scales::label_number_si(...),
    "scientific" = scales::label_scientific(...),
    stop(paste0("Unknown label format: ", label_format))
  )
}

# Check the aesthetic passed into the template to allow custom visualisations to be applied
check_aesthetic <- function(aesthetic) {
  aesthetic_names <- names(aesthetic)
  list(
    x = "x" %in% aesthetic_names,
    y = "y" %in% aesthetic_names,
    fill = "fill" %in% aesthetic_names,
    colour = any(c("colour", "color") %in% aesthetic_names),
    ymax  = "ymax" %in% aesthetic_names,
    ymin = "ymin" %in% aesthetic_names,
    xmax = "xmax" %in% aesthetic_names,
    xmin = "xmin" %in% aesthetic_names,
    label = "label" %in% aesthetic_names
  )
}

# Chart Templates ----
#' Plot Templates
#'
#' Set of functions containing templates for creating publication ready charts in Defra's
#' Farm Business Survey style.
#'
#' @details
#' This set of functions can be used in isolation to generate basic publication ready charts in the
#' FBS style or can be combined with other ggplot2 layers for a deep level of customisation.
#'
#' For a list of accepted aesthetics for \code{fbs_barplot}, \code{fbs_stackplot} and
#' \code{fbs_distribution_plot}, see \code{\link[ggplot2]{geom_bar}}.
#'
#' For a list of accepted aesthetics for \code{fbs_lineplot} see \code{\link[ggplot2]{geom_line}}.
#'
#' The following aesthetics are automatically scaled if supplied with their respective
#' \code{scale_*_govuk} scale. All other aesthetics must be scaled manually with additional ggplot
#' layers.
#'
#' \itemize{
#'   \item \code{fill: \link{scale_fill_govuk}}
#'   \item \code{colour: \link{scale_colour_govuk}}
#' }
#'
#' @param data Default dataset used for plot creation, passed into \code{\link[ggplot2]{ggplot}}.
#' @param aesthetic Aesthetic mapping used for the plot created by \link[ggplot2]{aes}. See details
#' for a list of accepted aesthetics for each template.
#' @param error Set the columns within the data to generate error bars from within
#' \code{fbs_barplot()}
#'
#' Either a single character corresponding to errors in the plus/minus format or a character vector
#' of length two to set the minimum and maximum values separately where the first character
#' corresponds to the minimum value and second character to the maximum value e.g.
#' \code{c("min_value", "max_value")}
#' @param title Character string of text used as the plot title. Default: NULL
#' @param value_name Character string of text used to label the continuous axis, Default: NULL
#' @param horizontal Set to \code{TRUE} to change the chart orientation to horizontal,
#' Default: FALSE
#' @param legend_hide Set to \code{TRUE} to hide the plot legend, Default: FALSE
#' @param continuous_format Character string denoting the format that the continuous (y) axis should
#' take.
#'
#' One of "identity", "comma", "percent", "abbreviated" or "scientific". Defaults to "percent" for
#' \code{fbs_distribution_plot()} and "comma" for all other plots.
#' @param font Change the default font.
#' @param text_scale Scale up or down the text size within the plot.
#' @param zero_axis Should the y axis on \code{fbs_lineplot()} start at zero. Default: TRUE.
#' @param palette Change the default colour palette applied to template.
#' @return Returns a ggplot object
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#' \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{geom_bar}},
#' \code{\link[ggplot2]{geom_line}}
#' @name fbs_templates
#' @export
fbs_barplot <- function(
  data,
  aesthetic,
  error = NULL,
  title = NULL,
  value_name = NULL,
  horizontal = FALSE,
  legend_hide = FALSE,
  continuous_format = "comma",
  font = "sans",
  text_scale = 1,
  palette = getOption("ggfbs.default_palette")
) {

  aes_spec <- check_aesthetic(aesthetic)

  # Initial ggplot object
  p <- ggplot2::ggplot(data, aesthetic)

  # Add primary geom
  # Modify default colour if palette is change and no fill aesthetic supplied
  if (any(identical(getOption("ggfbs.default_palette"), palette), aes_spec$fill)) {
    p <- p + geom_fbsbar()
  } else {
    p <- p + geom_fbsbar(fill = set_palette(palette)(1))
  }

  # Add line at 0
  p <- p + ggplot2::geom_hline(yintercept = 0)

  # Add error bars if needed
  if (!is.null(error)) {
    # Check length of `error` argument passed
    if (length(error) == 1) {
      # Create aesthetic for symmetrical errors with one column within the data
      aes_error <- ggplot2::aes(
        ymin = .data[[rlang::as_name(aesthetic[["y"]])]] - .data[[error]],
        ymax = .data[[rlang::as_name(aesthetic[["y"]])]] + .data[[error]]
      )
    } else if (length(error) == 2) {
      # Create aesthetic for errors with two columns with the data
      aes_error <- ggplot2::aes(ymin = .data[[error[1]]], ymax = .data[[error[2]]])
    } else {
      stop(
        "`error` must either be a single character or character vector of length 2"
      )
    }
    # Add error bars to plot
    p <- p +
      ggplot2::geom_errorbar(
        aes_error,
        width = 0.2,
        position = ggplot2::position_dodge(0.75),
        size = 0.6,
        colour = "black"
      )
  }

  # Add labels
  p <- p +
    ggplot2::labs(title = title)

  # Check plot orientation and add scales
  if (horizontal) {
    p <- p +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(6),
        labels = format_label_continuous(continuous_format),
        expand = ggplot2::expansion(mult = c(0, 0.2))
      ) +
      ggplot2::ylab(value_name)
  } else {
    p <- p +
      ggplot2::scale_x_discrete(labels = scales::label_wrap(10)) +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(10),
        labels =  format_label_continuous(continuous_format)
      ) +
      ggplot2::labs(subtitle = value_name)
  }

  # Add scale if fill passed into aesthetic
  if (aes_spec$fill) {
    p <- p +
      scale_fill_govuk(palette = palette)
  }

  # Add theme
  p <- p +
    theme_fbs(
      horizontal = horizontal,
      text_scale = text_scale,
      font = font
    )

  # Hide legend if requested
  if (legend_hide) {
    p <- p +
      ggplot2::theme(legend.position = "none", plot.subtitle = ggplot2::element_text(vjust = 0))
  }

  p
}

#' @rdname fbs_templates
#' @export
fbs_stackplot <- function(
  data,
  aesthetic,
  title = NULL,
  value_name = NULL,
  horizontal = FALSE,
  legend_hide = FALSE,
  continuous_format = "comma",
  font = "sans",
  text_scale = 1,
  palette = getOption("ggfbs.default_palette")
) {

  # Primary geom
  p <- data %>%
    ggplot2::ggplot(aesthetic) +
    # Add bar plot
    geom_fbsbar(position = ggplot2::position_stack()) +
    # Add line at 0
    ggplot2::geom_hline(yintercept = 0)

  # Add labels
  p <- p +
    ggplot2::labs(title = title)

  # Check plot orientation and add scales
  if (horizontal) {
    p <- p +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(6),
        labels = format_label_continuous(continuous_format),
        expand = ggplot2::expansion(mult = c(0, 0.1))
      ) +
      ylab(value_name)

  } else {
    p <- p +
      ggplot2::scale_x_discrete(labels = scales::label_wrap(10)) +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(6),
        labels = format_label_continuous(continuous_format)
      ) +
      ggplot2::labs(subtitle = value_name)

  }

  p <- p +
    scale_fill_govuk(palette = palette)

  # Add theme
  p <- p +
    theme_fbs(
      horizontal = horizontal,
      text_scale = text_scale,
      font = font
    )

  # Hide legend if requested
  if (legend_hide) {
    p <- p +
      ggplot2::theme(legend.position = "none", plot.subtitle = ggplot2::element_text(vjust = 0))
  }

  p
}

#' @rdname fbs_templates
#' @export
fbs_distribution_plot <- function(
  data,
  aesthetic,
  title = NULL,
  value_name = NULL,
  horizontal = FALSE,
  legend_hide = FALSE,
  continuous_format = "percent",
  font = "sans",
  text_scale = 1,
  palette = getOption("ggfbs.default_palette")
) {

  aes_spec <- check_aesthetic(aesthetic)

  # Primary geom
  p <- data %>%
    ggplot2::ggplot(aesthetic) +
    # Add bar plot
    geom_fbsbar(position = ggplot2::position_fill()) +
    # Add line at 0
    ggplot2::geom_hline(yintercept = 0)

  # Add label geom if supplied in aesthetic
  if (aes_spec$label) {
    p <- p +
      ggplot2::geom_text(position = ggplot2::position_fill(0.5), size = 7)
  }

  # Add labels
  p <- p +
    ggplot2::labs(title = title) +
    ggplot2::ylab(label = value_name)

  # Check plot orientation and add scales
  if (horizontal) {
    p <- p +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(6),
        labels = format_label_continuous(continuous_format),
        expand = ggplot2::expansion(mult = c(0, 0.1))
      )
  } else {
    p <- p +
      ggplot2::scale_x_discrete(labels = scales::label_wrap(10)) +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(6),
        labels = format_label_continuous(continuous_format)
      )
  }

  p <- p +
    scale_fill_govuk(palette = palette)

  # Add theme
  p <- p +
    theme_fbs(
      horizontal = horizontal,
      text_scale = text_scale,
      font = font
    )

  # Hide legend if requested
  if (legend_hide) {
    p <- p +
      ggplot2::theme(legend.position = "none", plot.subtitle = ggplot2::element_text(vjust = 0))
  }

  p
}

#' @rdname fbs_templates
#' @export
fbs_lineplot <- function(
  data,
  aesthetic,
  title = NULL,
  value_name = NULL,
  legend_hide = FALSE,
  continuous_format = "comma",
  zero_axis = TRUE,
  font = "sans",
  text_scale = 1,
  palette = getOption("ggfbs.default_palette")
) {

  aes_spec <- check_aesthetic(aesthetic)

  # Initial ggplot object
  p <- ggplot2::ggplot(data, aesthetic)

  # Add primary geom
  # Modify default colour if palette is change and no colour aesthetic supplied
  if (any(identical(getOption("ggfbs.default_palette"), palette), aes_spec$colour)) {
    p <- p + geom_fbsline()
  } else {
    p <- p + geom_fbsline(colour = set_palette(palette)(1))
  }

  # Modify x axis position
  if (zero_axis) {
    p <- p +
      ggplot2::geom_hline(yintercept = 0)
  } else {
    p <- p +
      ggplot2::geom_hline(ggplot2::aes(yintercept = min(.data[[rlang::as_name(aesthetic[["y"]])]])))
  }

  # Add labels
  p <- p +
    ggplot2::labs(title = title, subtitle = value_name)

  # Add scales
  if (is.double(data[[rlang::as_name(aesthetic[["x"]])]])) {
    p <- p +
      ggplot2::scale_x_continuous()
  } else {
    p <- p +
      ggplot2::scale_x_discrete(labels = scales::label_wrap(10)) +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(6),
        labels = format_label_continuous(continuous_format)
      )
  }

  p <- p +
    scale_colour_govuk(palette = palette)

  # Add default styling and additional styling for line plot
  p <- p +
    theme_fbs(
      horizontal = FALSE,
      text_scale = text_scale,
      font = font
    ) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
      axis.ticks.x = ggplot2::element_blank()
    )

  # Hide legend if requested
  if (legend_hide) {
    p <- p +
      ggplot2::theme(legend.position = "none", plot.subtitle = ggplot2::element_text(vjust = 0))
  }

  p
}
