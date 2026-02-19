# Food Delivery Analysis

This repository contains an R script and supporting data for analyzing food delivery times. The analysis covers data cleaning, feature engineering, exploratory visualization, clustering, decision‑tree modeling, and even an interactive Shiny app for exploration.

## Contents

- `Food_Delivery_Times.R` – primary R script that:
  * reads the raw dataset from `DataSet/Food_Delivery_Times.csv`,
  * checks for duplicates, outliers and missing values,
  * imputes or removes bad records and creates new features (speed, total time, late flag, customer rating),
  * writes a cleaned version back to `DataSet/clean_delivery_data.csv`,
  * generates a battery of plots saved under `plots/`,
  * performs k‑means clustering and builds decision trees (classification & regression),
  * and contains a Shiny UI/server pair for interactive data upload, visualization, clustering and tree modelling.
- `DataSet/` – input datasets (raw and cleaned).
- `plots/` – PNG files produced by the script’s visualization section.
- `.gitignore` – excludes R artifacts, data and outputs.
- `.Rhistory` / `.RData` – session artifacts (ignored by git).

## Configuration

Before running the script you may wish to customize the file paths used for reading and writing data and plots. At the top of `Food_Delivery_Times.R` several placeholder strings appear such as `"Data_Set_Path"`, `"Cleaned_Data_Set_Path"`, and `"plots_Path"`. Replace these with the actual relative or absolute paths appropriate for your environment (e.g. `"DataSet/Food_Delivery_Times.csv"` or `"plots"`).

If you rename the local folder (e.g. from `Op.Sudaines` to `Food_Delivery_Analysis`), make sure any hard‑coded paths in the script or documentation reflect the new name. Similarly, update the remote URL with `git remote set-url origin <new-url>` after changing the GitHub repository name.

## Getting Started

1. **Clone the repository:**
   ```bash
   git clone https://github.com/A7medF0uad/Food_Delivery_Analysis.git
   cd Food_Delivery_Analysis
   ```

2. **Install required packages** (you can install them all at once):
   ```r
   install.packages(c(
     "dplyr", "ggplot2", "readr", "patchwork",
     "rpart", "rpart.plot", "shiny", "bslib"
   ))
   ```

3. **Run the script**
   - Open `Food_Delivery_Times.R` in R or RStudio and execute it (source the file).  This will perform data cleaning, write the cleaned dataset, generate a collection of plots in the `plots/` folder, and output several intermediate CSVs used by the models.

4. **Use the interactive Shiny app**
   - At the end of `Food_Delivery_Times.R` there is a Shiny application.  After sourcing the script, run `shinyApp(ui, server)` or simply click "Run App" in RStudio to launch a web interface where you can upload your own data, explore visualizations, cluster settings, and build decision trees.


5. **Plot files** are saved automatically; feel free to open them to view the static results.

## Analysis & Visualizations

The R script produces a number of insights:

- **Data cleaning** removes duplicate rows and outliers, imputes missing values using medians or modes, and calculates new features such as delivery speed, total transaction time, a binary late/ontime flag, and a customer rating based on delivery duration.
- **Exploratory plots** saved under `plots/` include:
  - Overall distribution of total delivery time (histogram/boxplot).
  - Delivery performance broken down by vehicle type.
  - Customer rating vs. delivery time.
  - Histograms of distance, preparation time, courier experience and delivery speed.
  - Boxplots showing how traffic level and weather affect delivery times.
  - Counts of orders by weather and traffic conditions.
  - Scatterplot of distance vs. delivery time with fitted smooth line.
  - Delivery times by time of day.
- **Clustering**: numeric features are scaled and k‑means is performed (default k=3); results are appended to the dataset and basic aggregate summaries are exported.
- **Decision trees**: the cleaned data is split into training/testing sets. A classification tree predicts whether a delivery is late, and a regression tree predicts delivery time. Both models are plotted and evaluation metrics (accuracy/rmse) are printed.

The Shiny app exposes these capabilities with uploadable data and user controls.

## Custom Functions

The script defines a single helper function to support data cleaning:

```r
get_mode <- function(x) {
  # Identifies the most frequent value in a vector while ignoring NA's
  uniq <- unique(x[!is.na(x)])
  uniq[which.max(tabulate(match(x, uniq)))]
}
```

**Purpose:** Used to impute missing entries in categorical columns (`Weather`, `Traffic_Level`, `Time_of_Day`) by finding the mode (most common value).

All other logic relies on base R and package functions; expand this section if you add more utilities.

## Sample Plots

Below are descriptive summaries of the visualizations generated by the script.  The actual images are saved in the `plots/` directory and will update if you rerun the analysis:

- **Overall spread of total time** – Histogram and boxplot showing the distribution of combined preparation and delivery durations.
- **Vehicle performance** – Boxplot comparing delivery times across different vehicle types.
- **Delivery by time of day** – Boxplots illustrating how delivery duration varies throughout the day.
- **Distance vs. time** – Scatter plot with a fitted line showing the relationship between delivery distance and time.
- **Distance and prep time distributions** – Side-by-side histograms of delivery distances and preparation times.
- **Courier experience & speed** – Histograms comparing courier experience years and calculated delivery speed.
- **Traffic & delivery time** – Boxplot highlighting how different traffic conditions impact delivery times.
- **Weather & traffic counts** – Bar chart of order counts segmented by weather and traffic conditions.

## License

This project can be freely used for educational purposes.

> ⚠️ The data files may contain sensitive delivery information; treat appropriately if sharing.
