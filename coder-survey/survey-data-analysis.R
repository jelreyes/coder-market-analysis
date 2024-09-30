library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Read the cleaned dataset
data_path <- "cleaned-coder-survey.csv"
coder_survey <- read_csv(data_path)

# Define function to calculate percentage for specified columns
calculate_percentage <- function(data, prefix) {
  data %>%
    summarise(across(starts_with(prefix) & !contains("_other"), ~ sum(. == 1, na.rm = TRUE) / n())) %>%
    pivot_longer(cols = everything(), names_to = "column", values_to = "percentage")
}

# Define function to create and save a plot
create_plot <- function(career_data, title, filename) {
  plot <- ggplot(career_data, aes(x = percentage * 100, y = reorder(column, percentage))) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = paste0(round(percentage * 100, 1), "%")), 
              hjust = -0.2, size = 3.5) + 
    labs(y = NULL) +                    
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "white"),  
      plot.background = element_rect(fill = "white", color = "white"),   
      axis.title.x = element_blank(),       
      axis.text.x = element_blank(),        
      axis.title.y = element_blank(),       
      panel.grid = element_blank(),         
      axis.text = element_text(size = 10)   
    ) +
    ggtitle(title) +
    coord_cartesian(clip = "off")           
  
  # Save plot
  ggsave(filename, plot = plot, width = 8, height = 6)
}

#' The "career_" columns contain the careers the participants are interested in
#' and are bool-type, having 1 and NA values only

#' Calculate career interests and generate plot
career_intrst <- calculate_percentage(coder_survey, "career_")
create_plot(career_intrst, "Career interests of new coders (2018)", "career_interest_plot.png")

# Filter data for further analysis
filtered_df <- coder_survey %>%
  filter(rowSums(is.na(select(., starts_with("career_")))) != ncol(select(., starts_with("career_"))))

# Group by residence column to calculate location density
loc_density <- filtered_df %>%
  group_by(residence) %>%
  summarise(
    `Absolute frequency` = n(),
    `Percentage` = n() * 100 / nrow(filtered_df)
  ) %>%
  arrange(desc(Percentage))

# Replace 0 in months_code with 1, and create monthly_spent column
filtered_df <- filtered_df %>%
  mutate(MonthsProgramming = replace(months_code, months_code == 0, 1)) %>%
  mutate(monthly_spent = amt_spent_code / months_code) %>%
  drop_na(monthly_spent)

#' Filter responses for top 4 countries
top4_df <- filtered_df %>%
  filter(residence %in% c('United States of America', 'India', 'United Kingdom', 'Canada'))

# Plot distribution of monthly spent for each country
ggplot( data = top4_df, aes(x = residence, y = monthly_spent)) +
  geom_boxplot() +
  ggtitle("Monthly spent for learning per country\n(Distributions)") +
  xlab("Country") +
  ylab("Monthly spent (US dollars)") +
  theme_bw()

# Filter out outliers for each country
top4_df  <- top4_df %>% 
  filter(monthly_spent < 20000)

# Assign index for each rows to easily apply filters
top4_df <- top4_df %>%
  mutate(index = row_number())

#' Define function to check outliers by country, showing:
#' if they attended boot camp
#' the amount they spent for learning to code
#' number of months spent programming

outlier_df <- function(df, residence_value, monthly_spent_value) {
  outliers <- df %>%
    filter(
      residence == residence_value,
      monthly_spent > monthly_spent_value,
    )
  
  outliers <- select(outliers, index, bootcamp_attend, monthly_spent, months_code)
  
  outliers
}

#' In Canada, one of the outliers did not attend bootcamp and spent 8,500 USD each month
#' while the other attended a boot camp but spent 18,500 USD each month
#' Both of them are learning to code in 1 or less than a month when they completed the survey
#' The amount of money spent per month is unrealistic and should be significantly lower
#' so we are removing both of these outliers

canada_outlier <- outlier_df(top4_df, "Canada", 5000)
top4_df  <-  top4_df %>% 
  filter(!(index %in% canada_outlier$index))

#' All of the 6 outliers did not attend any boot camp, four of them even indicate they spent 
#' >= 10,000 USD in each month for less than 4 months in learning to code
#' The question from the survey says "Aside from university tuition, about how much money have you spent on learning to code so far (in US dollars)?"
#' So the question might have been misinterpreted and for this reason, we are removing these responses

india_outlier <- outlier_df(top4_df, "India", 5000)

#' UK has similar instances to Canada, with one spending over 16,000 USD per month in 3 months
#' without attending any boot camp and the other attended but only has been programming for one or less than
#' a month but spent 8,000 USD per month. We are also removing these outliers

top4_df  <-  top4_df %>% 
  filter(!(index %in% india_outlier$index))
uk_outlier <- outlier_df(top4_df, "United Kingdom", 5000)

#' US respondents have higher amount spent per month for outliers (~9,000 USD)
#' It's hard to identify if these are good data so we'll closely examine
#' respondents with monthly spent amounting to more than 5,000 USD

top4_df  <-  top4_df %>% 
  filter(!(index %in% uk_outlier$index))
us_outlier <- outlier_df(top4_df, "United States of America", 5000)

#' 15 out of 29 either did not attend boot camp or only have been learning to code for
#' <=3 months, but spending >= 6,000 USD per month so we are also removing participants
#' from US with these criteria

us_outlier <- top4_df %>%
  filter(
    residence == "United States of America",
    monthly_spent >= 6000,
    months_code <= 3 | bootcamp_attend != 1
  )

top4_df  <-  top4_df %>% 
  filter(!(index %in% us_outlier$index))

# Compute average monthly spending per country
mean_per_country <- top4_df %>% 
  group_by(residence) %>%
  summarize(mean = mean(monthly_spent)) %>%
  arrange(desc(mean))

# Compute number of coders per country
numcoder_per_country <- top4_df %>%
  group_by(residence) %>%
  summarise(freq = n() ) %>%
  arrange(desc(freq))