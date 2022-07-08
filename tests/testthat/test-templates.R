# 1. fbs_barplot ----
test_that("1.1 fbs_barplot generates base plot with no errors", {

  plot_data <- data.frame(
    x = c("a", "b", "c"),
    y = c(10000, 5000, 7000)
  )

  p <- plot_data %>%
    fbs_barplot(ggplot2::aes(x = x, y = y))

    expect_true(ggplot2::is.ggplot(p))
    expect_silent(print(p))
})

test_that("1.2 fbs_barplot generates grouped plot with no errors", {

  plot_data <- data.frame(
    x = c("a", "b", "c", "a", "b", "c"),
    y = c(10000, 5000, 7000, 8000, 15000, 6000),
    f = c("x", "y", "x", "y", "x", "y")
  )

  p <- plot_data %>%
    fbs_barplot(ggplot2::aes(x = x, y = y, fill = f))

  expect_true(ggplot2::is.ggplot(p))
  expect_silent(print(p))
})

test_that("1.3 fbs_barplot generates plot with symmetrical error bars", {

  plot_data <- data.frame(
    x = c("a", "b", "c"),
    y = c(10000, 5000, 7000),
    error = c(2000, 500, 6000)
  )

  p <- plot_data %>%
    fbs_barplot(ggplot2::aes(x = x, y = y), error = "error")

  expect_true(ggplot2::is.ggplot(p))
  expect_silent(print(p))
})

test_that("1.4 fbs_barplot generates plot with asymmetrical error bars", {

  plot_data <- data.frame(
    x = c("a", "b", "c"),
    y = c(10000, 5000, 7000),
    error_min = c(2000, 500, 6000),
    error_max = c(11000, 7000, 13000)
  )

  p <- plot_data %>%
    fbs_barplot(ggplot2::aes(x = x, y = y), error = c("error_min", "error_max"))

  expect_true(ggplot2::is.ggplot(p))
  expect_silent(print(p))
})

# 2. fbs_stackplot ----
test_that("2.1 fbs_stackplot generates base plot with no errors", {

  plot_data <- data.frame(
    x = c("a", "b", "c", "a", "b", "c"),
    y = c(10000, 5000, 7000, 8000, 15000, 6000),
    f = c("x", "y", "x", "y", "x", "y")
  )

  p <- plot_data %>%
    fbs_stackplot(ggplot2::aes(x = x, y = y, fill = f))

  expect_true(ggplot2::is.ggplot(p))
  expect_silent(print(p))
})

# 3. fbs_distribution_plot ----
test_that("3.1 fbs_distribution_plot generates base plot with no error", {

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
    fbs_distribution_plot(ggplot2::aes(x = x, y = y, fill = f))

  expect_true(ggplot2::is.ggplot(p))
  expect_silent(print(p))
})

test_that("3.2 fbs_distribution_plot generates plot with labels", {

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
    fbs_distribution_plot(ggplot2::aes(x = x, y = y, fill = f, label = y))

  expect_true(ggplot2::is.ggplot(p))
  expect_silent(print(p))
})

# 4. fbs_lineplot ----
test_that("4.1 fbs_lineplot generates base plot with no error", {

  plot_data <- data.frame(
    x = c("2010",
          "2011",
          "2012"),
    y = c(20000,
          17000,
          21000)
  )

 p <- plot_data %>%
    fbs_lineplot(ggplot2::aes(x = x, y = y, group = 1))

  expect_true(ggplot2::is.ggplot(p))
  expect_silent(print(p))

})

test_that("4.2 fbs_lineplot generates base plot when x is continuous", {

  plot_data <- data.frame(
    x = c(200,
          300,
          600),
    y = c(20000,
          17000,
          21000)
  )

  p <- plot_data %>%
    fbs_lineplot(ggplot2::aes(x = x, y = y))

  expect_true(ggplot2::is.ggplot(p))
  expect_silent(print(p))

})

test_that("4.3 fbs_lineplot generates plot when zero_axis=FALSE", {

  plot_data <- data.frame(
    x = c("2010",
          "2011",
          "2012"),
    y = c(20000,
          17000,
          21000)
  )

  p <-  plot_data %>%
    fbs_lineplot(ggplot2::aes(x = x, y = y, group = 1), zero_axis = FALSE)

  expect_true(ggplot2::is.ggplot(p))
  expect_silent(print(p))

})
