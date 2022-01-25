## ggfbs

## Features

ggfbs is a package containing convenient functions to create publication ready charts with Defra's Farming Statistics style. This package was built with and designed to work in conjunction with ggplot2 and provides a set of templates to create basic chart whilst also allowing a deep level of customisation.

## Installation

PLACEHOLDER

## Usage

There are three main ways this package may be used:

1. Utilise the in built templates to create basic charts.
2. Combine the basic templates with other ggplot layers for further customisation.
3. Forego the templates and apply the styling to any ggplot object through the use of `theme_fbs()` and the `scale_*_govuk()` set of functions.

### Basic Chart Templates

```r
# Load libraries
library(ggplot2)
library(ggfbs)

# Read in some data
fbi_data <- read.csv("fbi-data.csv", row.names = FALSE)

# Manipulate dataset
plot_data <- fbi_data %>% 
  filter(variable == "Farm Business Income", fbsyear %in% c("2019/20", "2020/21"))
  
# Create basic grouped bar plot
plot_data %>% 
  fbs_barplot(aes(x = factor_name, y = mean))
```
![](examples/usage-barplot-1.png)<!-- -->

### Customise with ggplot2

PLACEHOLDER

### Apply styling to your own charts

PLACEHOLDER

## Export Chart

PLACEHOLDER

## Help and Resources
Data visualisation and ggplot2

https://ggplot2.tidyverse.org/index.html  
https://r-graphics.org/

Coding in R and programming with ggplot2

https://r4ds.had.co.nz/  
https://ggplot2-book.org/index.html

## Other Similar Tools
https://github.com/bbc/bbplot  
https://github.com/Greater-London-Authority/gglaplot  
https://github.com/houseofcommonslibrary/clcharts
