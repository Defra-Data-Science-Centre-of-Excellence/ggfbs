test_data <- read.csv("test-input/barplot-data.csv")

test_that("fbs_barplot generates base plot with no errors", {

  plot_data <- data.frame(
    x = c("a", "b", "c"),
    y = c(10000, 5000, 7000)
  )

  p <- plot_data %>%
    fbs_barplot(aes(x = x, y = y))

    expect_true(ggplot2::is.ggplot(p))
    expect_silent(print(p))
})

test_that("fbs_barplot generates grouped plot with no errors", {

  plot_data <- data.frame(
    x = c("a", "b", "c", "a", "b", "c"),
    y = c(10000, 5000, 7000, 8000, 15000, 6000),
    f = c("x", "y", "x", "y", "x", "y")
  )

  p <- plot_data %>%
    fbs_barplot(aes(x = x, y = y, fill = f))

  expect_true(ggplot2::is.ggplot(p))
  expect_silent(print(p))
})

test_that("fbs_stackplot generates base plot with no errors", {

  plot_data <- data.frame(
    x = c("a", "b", "c", "a", "b", "c"),
    y = c(10000, 5000, 7000, 8000, 15000, 6000),
    f = c("x", "y", "x", "y", "x", "y")
  )

  p <- plot_data %>%
    fbs_stackplot(aes(x = x, y = y, fill = f))

  expect_true(ggplot2::is.ggplot(p))
  expect_silent(print(p))
})

test_that("fbs_distribution_plot generates base plot with no error", {

  plot_data <- data.frame(
    x = c("a", "a", "a",
          "b", "b", "b",
          "c", "c", "c"),
    y = c(0.2, 0.6, 0.2,
          0.5, 0.2, 0.3,
          0.1, 0.9, 0),
    f = c("x", "y", "z")
  )

  p <- plot_data %>%
    fbs_distribution_plot(aes(x = x, y = y, fill = f))

  expect_true(ggplot2::is.ggplot(p))
  expect_silent(print(p))
})

test_that("fbs_lineplot generates base plot with no error", {

  plot_data <- data.frame(
    x = c("2010",
          "2011",
          "2012"),
    y = c(20000,
          17000,
          21000)
  )

 p <-  plot_data %>%
    fbs_lineplot(aes(x = x, y = y, group = 1))

  expect_true(ggplot2::is.ggplot(p))
  expect_silent(print(p))

})
