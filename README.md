# R-portfolio

This repository contains an R package that showcases a collection of custom functions, statistical methods, and example analyses.  
It serves as a portfolio of R programming work, including data analysis scripts, utility functions, and Shiny app examples.

## Purpose

- Demonstrates proficiency in R programming and package development.
- Provides reusable statistical and data analysis functions.
- Includes example analyses in R Markdown and Shiny applications.

## File Overview

### Functions (`R/` folder)

- **impure.R** – Calculates impurity measures such as Gini or entropy for classification tasks.
- **mlr.R** – Performs multiple linear regression.
- **my_tsma.R** – Applies a time series moving average.
- **myknn.R** – Implements K-Nearest Neighbors classification.
- **mylogistic.R** – Fits a logistic regression model.
- **myrgb.R** – Handles RGB color value operations (conversion, normalization).
- **myttest.R** – Runs t-tests (one-sample or two-sample).
- **newrap.R** – Wraps strings for cleaner console output.
- **tetr.R** – Example/demo function.
- **wmean.R** – Calculates a weighted mean.
- **shiny.R** – Shiny app script for interactive demonstration.

**Shiny App Screenshot:**  
![Shiny App Screenshot](shiny_image.png)

---

### Analyses

- **taco.Rmd** – R Markdown analysis of a taco dataset.  
  **taco_analysis.pdf** – Knitted output of the taco analysis.
- **Cola.Rmd** – R Markdown analysis focused on Coca-Cola data.  
  **Coca-Cola.pdf** – Knitted output of the Coca-Cola analysis.

---

## Notes

- All function documentation is available via R help pages after installation.
- PDF files are knitted outputs of their respective `.Rmd` files.
- The Shiny app can be run directly from the script or adapted into a package `inst/` folder for distribution.