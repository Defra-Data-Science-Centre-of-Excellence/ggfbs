# ggfbs

## Status

ggfbs is superseded, please now use [afcharts](https://github.com/best-practice-and-impact/afcharts/). ggfbs will remain available for current users but will no longer receive any updates.

## Overview

ggfbs is a package containing convenient functions to create publication
ready charts. This package was built with and designed to work in conjunction with
[ggplot2](https://ggplot2.tidyverse.org/) and provides a set of
templates to create basic chart whilst also allowing a deep level of
customisation. Originally designed by Defra’s Farm Business Survey (FBS) 
team, its usage is generic and can be used in any statistical publication or for general usage
and incorperates style guidance and colour scales from the 
[Analysis Function](https://analysisfunction.civilservice.gov.uk/).

## Other tools

Along with ggfbs, the FBS team has developed several tools for working
with FBS data and to aid in the implemtations of Reproducible Analytical
Pipelines (RAP).

-   [rgovspeak](https://github.com/Defra-Data-Science-Centre-of-Excellence/rgovspeak):
    Allows govspeak notation to be used within rmarkdown and provides
    html render functionality to preview your page as if it were
    published on gov.uk, along with a govspeak markdown template for
    rmarkdown
    
## Installation

The package can be installed directly through R using
one of the methods below.

If you have a http proxy setup use:

``` r
devtools::install_github("Defra-Data-Science-Centre-of-Excellence/ggfbs")
```

Otherwise, download the latest release and install the package in R
using:

``` r
install.packages("~/Downloads/ggfbs.zip", repos=NULL)
```

## Usage

There are three main ways this package may be used:

1.  Utilise the in built templates to create basic charts.
2.  Combine the basic templates with other ggplot layers for further
    customisation.
3.  Forego the templates and apply the styling to any ggplot object
    through the use of `theme_fbs()` and the `scale_*_govuk()` set of
    functions.

### Basic Chart Templates

``` r
# Load libraries
library(dplyr)
library(ggplot2)
library(ggfbs)

# Read in some data
fbi_data <- read.csv(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1031993/fbs-farmbusinessincome-series_9nov21.csv"
)

# Manipulate dataset
plot_data <- fbi_data %>% 
  filter(variable == "Farm Business Income", fbsyear %in% c("2019/20", "2020/21"))
  
# Create basic grouped bar plot
plot_data %>% 
  fbs_barplot(aes(x = factor_name, y = mean, fill = fbsyear))
```

![](examples/usage-barplot-1-1.png)<!-- -->

### Customise with ggplot2

ggfbs allows extensive customisation by adding additional ggplot2 layers
to your charts. Here an extension of `fbs_lineplot()` is shown.

Firstly, we will create a basic time series comparing the Farm Business
Income of Cereal farms with the overall average.

``` r
# Manipulate dataset
years_include <- purrr::map_chr(2010:2020, ~ paste0(.x,"/", as.numeric(substr(.x, 3, 4)) + 1))

plot_data <- fbi_data %>% 
  filter(
    variable == "Farm Business Income", 
    fbsyear %in% years_include, 
    factor_name %in% c("Cereals", "All farms")
  ) %>% 
  mutate(factor_name = forcats::fct_rev(factor_name))

# Create initial plot with two groups and combining colours and line types
p <- plot_data %>% 
  fbs_lineplot(
    aes(x = fbsyear, y = mean, group = interaction(factor_name, typology), 
        colour = factor_name, linetype = factor_name),
    value_name = "£"
  )
  
p
```

![](examples/usage-lineplot-1-1.png)<!-- -->

Now that we have the basic line plot, we can see that we have two series
breaks. Here it might be useful to add two vertical lines to highlight
these breaks.

We can also change the types of lines displayed by added a scale.

``` r
p2 <- p +
  geom_vline(xintercept = c("2012/13", "2017/18"), colour = "#333333")  +
  scale_linetype_manual(values = c("solid", "longdash"))
  
p2
```

![](examples/usage-lineplot-2-1.png)<!-- -->

When creating a line chart, it is sometimes preferable to omit the
legend and instead label the lines directly.

Here we set the `legend_hide` argument to `TRUE` within `fbs_lineplot()`
and create two annotation layers, allowing us to finely control the
position and size of the label. Additionally, to ensure consistency with
our theme, we use the `gss_cols()` helper function to match the colours
of the text to those of the lines.

``` r
p3 <- plot_data %>% 
  fbs_lineplot(
    aes(x = fbsyear, y = mean, group = interaction(factor_name, typology), 
        colour = factor_name, linetype = factor_name),
    value_name = "£",
    legend_hide = TRUE
  ) +
  geom_vline(xintercept = c("2012/13", "2017/18"), colour = "#333333") +
  scale_linetype_manual(values = c("solid", "longdash")) +
  annotate(
    geom = "text", 
    x = 11,
    y = c(75000, 45000),
    label = c("Cereals" ,"All Farms"),
    size = 8,
    colour = gss_cols(1:2)
  )

p3
```

![](examples/usage-lineplot-3-1.png)<!-- -->

### Apply styling to your own charts

Alternatively, to apply the FBS theming to chart types not included in
the templates, apply `theme_fbs()` in conjunction with one of the govuk
scales.

``` r
p4 <- ggplot(mpg, aes(class, hwy, colour = drv)) + 
  geom_boxplot(size = 1.2) +
  scale_colour_govuk() +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  theme_fbs()

p4
```

![](examples/usage-boxplot-1-1.png)<!-- -->

## Export Chart

By default, the theme applied to the charts within this package is
optimised for publication on gov.uk. For the charts to output correctly
the size and dpi of the charts must be set to:

-   wdith: 960
-   height: 640
-   dpi: 72

### 1. Working in Rmarkdown:

Set each chunk option using the options bellow (ignoring the #):

    # ```{r, fig.width=960/72, fig.height=640/72, dpi=72}

Or set all the chunks to output in the same format by adding the code
below to the setup chunk

``` r
knitr::opts_chunk$set(
  fig.width = 960 / 72,
  fig.height = 640 / 72,
  dpi = 72
)
```

### 2. Save your chart:

Use the `ggsave()` function as below to save your plot.

``` r
p <- plot_data %>% 
  fbs_barplot(aes(x = factor_name, y = mean, fill = fbsyear))

ggsave("bar-plot.png", p, width = 960/72, height = 640/72, dpi = 72)
```

### 3. Use the `text_scale` arguement:

All templates allow you to scale the size of the text up or down through
the `text_scale` argument. e.g. setting `text_scale` to `0.5` reduced
the size of the text by 50%. These charts can then be exported through
any of the usual methods.

## Help and Resources

Data visualisation and ggplot2

<https://ggplot2.tidyverse.org/index.html>  
<https://r-graphics.org/>

Coding in R and programming with ggplot2

<https://r4ds.had.co.nz/>  
<https://ggplot2-book.org/index.html>
