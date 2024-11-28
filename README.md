# Linear Regression Model for CPU Price Prediction and Using ANOVA 

## Overview
This project explores the use of linear regression models to predict the price of CPU and analyze the influence of consumer objects on its price. The objective is to create a model that accurately predicts CPU prices based on historical data, and to assess the impact of various consumer-related factors on the price.

## Requirements
This project was developed in R version 4.4.2. The following packages are required for running the analysis and generating visualizations:

- `tidyverse`: For data manipulation and visualization
- `mice`: For handling missing data using multiple imputation
- `VIM`: For visualizing missing data
- `readr`: For reading data files
- `dplyr`: For data manipulation
- `stringr`: For string operations
- `ggplot2`: For creating high-quality graphics and plots
- `psych`: For descriptive statistics and data exploration
- `knitr`: For dynamic report generation
- `nortest`: For normality tests
- `car`: For regression analysis and diagnostics
- `zoo`: For handling time series data

## Installation

Before running the analysis, ensure that you have installed the required packages. You can install them by running the following commands in your R environment:

```r
install.packages(c("tidyverse", "mice", "VIM", "readr", "dplyr", "stringr", "ggplot2", "psych", "knitr", "nortest", "car", "zoo"))
