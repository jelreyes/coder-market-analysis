# New Coders Market Analysis

## Overview
This project aims to identify the best market to advertise programming courses, using the open dataset from the 2018 freeCodeCamp new coder survey.

## Data Source
- **Source**: Open dataset from [freeCodeCamp's 2018 survey](https://github.com/freeCodeCamp/2018-new-coder-survey/blob/master/raw-data/2018-new-coder-survey.csv)
- Contains 31,226 rows and 136 columns

## Tech Stack
- **R**: The primary language for data manipulation and analysis.
- **Tidyverse**: A collection of R packages (including dplyr and ggplot2) used for data wrangling and visualization.
- **readr**: For fast reading of large datasets in .csv format.

Make sure you have the required packages installed before running the scripts:
```r
install.packages(c("readr", "tidyverse", "tools"))
```

## Data Preparation
Before conducting any analysis, we need to clean the raw data using the ```survey-data-cleanup.R``` script which performs the following:
- Renaming columns for better readability.
- Filtering out invalid or inconsistent responses (e.g., age discrepancies, unreasonable income values, invalid child information).
- Converting boolean responses and handling "Other" text fields by formatting them in title case.

To run ```survey-data-cleanup.R```:
```r
# Ensure the dataset file is named '2018-new-coder-survey.csv' in the working directory.
source("survey-data-cleanup.R")
```
After running the script, the cleaned dataset will be saved as ```cleaned-coder-survey.csv```. Here's a brief snippet of the rename function:
```r
data %>%
  rename(
    id = "#",
    is_softdev = "Are you already working as a software developer?",
    ...
    num_children = "How many children do you have?",
    ...
    ts_start = "Start Date (UTC)",
    ts_end = "Submit Date (UTC)"
  )
```
## Data Analysis
Use the ```survey-data-analysis.R``` script to analyze the cleaned dataset.

### Career Interests Analysis
We analyze career interests of new coders using the ```calculate_percentage()``` function, which calculates the percentage of coders interested in various career paths. This data is then visualized with a horizontal bar chart.

```r
career_intrst <- calculate_percentage(coder_survey, "career_")
create_plot(career_intrst, "Career interests of new coders (2018)", "career_interest_plot.png")
```

![career_interest](Plots/career_interest.png)

### Country and Spending Analysis
To focus on participants from English-speaking countries, we filtered for those residing in the United States, India, the United Kingdom, and Canada. We analyze how much they spend monthly on learning to code by calculating the average monthly_spent and visualizing the distribution with a boxplot.

```r
ggplot(top4_df, aes(x = residence, y = monthly_spent)) +
  geom_boxplot() +
  ggtitle("Monthly spent for learning per country (Distributions)") +
  xlab("Country") +
  ylab("Monthly spent (US dollars)") +
  theme_bw()
```
![spent_distribution](Plots/spent_distribution.png)

### Outlier Detection
During the analysis, significant outliers were detected. Some respondents reported spending more than $20,000 per month on learning. These responses were excluded to provide a more realistic representation. For example, participants who spent large amounts but had little coding experience or did not attend boot camps were removed.
```r
us_outlier <- top4_df %>%
  filter(
    residence == "United States of America",
    monthly_spent >= 6000,
    months_code <= 3 | bootcamp_attend != 1
  )

top4_df  <-  top4_df %>% 
  filter(!(index %in% us_outlier$index))
```

## Key Findings
The top countries where new coders reside are the United States (36.5%), India (13.6%), the UK (3.94%), and Canada (2.92%).

After filtering outliers, the average monthly expenditure on programming resources is:
- United States: $136
- Canada: $104
- United Kingdom: $63.5
- India: $62.3

However, India, with a much larger number of new coders (1,607 respondents), represents a potentially vast untapped market, despite its lower average spending.

## Actionable Recommendations:
- **Focus advertising campaigns in the U.S.** where the willingness to spend is highest, targeting both online and offline channels.
- **Explore secondary markets** such as **Canada** and **India**. In India, although average spending is lower, the large population of new coders can drive significant volume in course sales, especially with affordable pricing options.
- Localized Pricing: Offer flexible pricing strategies to accommodate the different spending behaviors across countries. For example, premium courses for U.S. and Canada, while offering budget-friendly options in India and the UK.
