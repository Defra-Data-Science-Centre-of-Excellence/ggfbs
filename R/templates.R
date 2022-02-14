add_error_bars <- function(data, values, ci) {
  ggplot2::geom_errorbar(
    ggplot2::aes(ymax = mean + confidence_interval, ymin = mean - confidence_interval),
    width = 0.2,
    position = ggplot2::position_dodge(0.75)
  )
}

set_label_position <- function(max_value, position_scaler = 0.025) {
  pos <- position_scaler * 10 ^ floor(log(max_value, 10))
  pos
}

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
#' @param error Column containing values to display as error bars within \code{fbs_barplot()}. Currently
#' only supports symmetrical errors. Default: NULL
#' @param title Character string of text used as the plot title. Default: NULL
#' @param value_name Character string of text used to label the continuous axis, Default: NULL
#' @param horizontal Set to \code{TRUE} to change the chart orientation to horizontal,
#' Default: FALSE
#' @param label_bars EXPERIMANTAL: Specify a column within the data to use a text label for each
#' item. Default: NULL
#' @param label_position EXPERIMANTAL: Position of text label.
#' @param legend_hide Set to \code{TRUE} to hide the plot legend, Default: FALSE
#' @param continuous_format Character string denoting the format that the continuous (y) axis should
#' take.
#'
#' One of "identity", "comma", "percent", "abbreviated" or "scientific". Defaults to "percent" for
#' \code{fbs_distribution_plot()} and "comma" for all other plots.
#' @param font Change the default font.
#' @param text_scale Scale up or down the text size within the plot.
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
  label_bars = FALSE,
  label_position = "top",
  legend_hide = FALSE,
  continuous_format = "comma",
  font = "sans",
  text_scale = 1
) {

  plot_type <- "bar"
  aes_spec <- check_aesthetic(aesthetic)

  to_plot <- data

  # Primary geom
  p <- to_plot %>%
    ggplot2::ggplot(aesthetic) +
    # Add bar plot
    geom_fbsbar() +
    # Add line at 0
    ggplot2::geom_hline(yintercept = 0)

  if (label_bars) {
    max_value <- max(to_plot[[values]])

    p <- p + ggplot2::geom_text(
      ggplot2::aes(
        label = .data[[values]],
        x = .data[[categories]],
        y = .data[[values]] + set_label_position(max_value)
      ),
      position = ggplot2::position_dodge(0.9),
      vjust = 0
    )
  }

  # Add error bars if needed
  if (!is.null(error)) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymax = .data[[rlang::as_name(aesthetic[["y"]])]] + .data[[error]],
          ymin = .data[[rlang::as_name(aesthetic[["y"]])]] - .data[[error]]
        ),
        width = 0.2,
        position = ggplot2::position_dodge(0.75),
        size = 0.1,
        colour = "black"
      )
  }

  # Add labels
  p <- p +
    ggplot2::labs(title = title)

  # Add scales
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
      scale_fill_govuk()
  }

  # Add styling
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
  text_scale = 1
) {

  plot_type <- "stackbar"

  to_plot <- data

  # Primary geom
  p <- to_plot %>%
    ggplot2::ggplot(aesthetic) +
    # Add bar plot
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack(), width = 0.75) +
    # Add line at 0
    ggplot2::geom_hline(yintercept = 0)

  # Add labels
  p <- p +
    ggplot2::labs(title = title)

  # Add scales
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
    scale_fill_govuk()

  # Add styling
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
  text_scale = 1
) {

  plot_type <- "distbar"

  to_plot <- data

  # Primary geom
  p <- to_plot %>%
    ggplot2::ggplot(aesthetic) +
    # Add bar plot
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_fill(), width = 0.75) +
    # Add line at 0
    ggplot2::geom_hline(yintercept = 0)

  # Add labels
  p <- p +
    ggplot2::labs(title = title) +
    ggplot2::ylab(label = value_name)

  # Add scales
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
    scale_fill_govuk()

  # Add styling
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
  font = "sans",
  text_scale = 1
) {

  plot_type <- "line"

  to_plot <- data

  # Primary geom
  p <- to_plot %>%
    ggplot2::ggplot(aesthetic) +
    # Add bar plot
    geom_fbsline() +
    # Add line at 0
    ggplot2::geom_hline(yintercept = 0)

  # Add labels
  p <- p +
    ggplot2::labs(title = title, subtitle = value_name)

  # Add scales
  p <- p +
    ggplot2::scale_x_discrete(labels = scales::label_wrap(10)) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(6),
      labels = format_label_continuous(continuous_format)
    )


  p <- p +
    scale_colour_govuk()

  # Add default styling and additonal styling for line plot
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
