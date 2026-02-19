# Required R Packages

The analysis script (`Food_Delivery_Times.R`) depends on the following CRAN packages. Install them before running the script:

```r
install.packages(c(
  "dplyr",        # data manipulation
  "ggplot2",      # plotting
  "readr",        # CSV reading
  "patchwork",    # combining plots
  "rpart",        # decision tree modeling
  "rpart.plot",   # visualizing decision trees
  "shiny",        # interactive web application
  "bslib"         # theming for Shiny/HTML
))
```

You can check the installed versions with `packageVersion("<pkg>")` if needed.

> ⚠️ The script assumes these packages are loaded at the top with `library(...)` calls; missing packages will cause an error.
