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
              hjust = 1.5, size = 3.5, color = "black") +  # Position labels at the end of the bars
    labs(y = NULL) +                    
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "white"),  
      plot.background = element_rect(fill = "white", color = "white"),   
      axis.title.x = element_blank(),       
      axis.text.x = element_blank(),        
      axis.title.y = element_blank(),       
      panel.grid = element_blank(),         
      axis.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5)  # Center plot title
    ) +
    ggtitle(title) +
    coord_cartesian(clip = "off")           
  
  # Save plot
  ggsave(filename, plot = plot, width = 8, height = 6)
}

#' The "career_" columns contain the careers the participants are interested in
#' and are bool-type, having 1 and NA values only

#' Calculate career interests, rename column names, and generate plot
career_intrst <- calculate_percentage(coder_survey, "career_")
career_intrst$column <- recode(career_intrst$column,
                               career_fullstack ="Full-Stack Web Developer",
                               career_backend = "Back-End Web Developer",
                               career_frontend = "Front-End Web Developer",
                               career_mobdev = "Mobile Developer",
                               career_devops = "DevOps / SysAdmin",
                               career_datasci = "Data Scientist",
                               career_teach = "Teacher / Trainer / Developer Evangelist",
                               career_qa = "Quality Assurance Engineer",
                               career_ux = "User Experience Designer",
                               career_prodm = "Product Manager",
                               career_gamedev = "Game Developer",
                               career_infosec = "Information Security",
                               career_dataeng = "Data Engineer"
)
create_plot(career_intrst, "Career interests of new coders", "plots/career_interest.png")

# Remove responses with no career interests
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

#' Filter responses for top 4 countries in loc_density
top4_df <- filtered_df %>%
  filter(residence %in% c('United States of America', 'India', 'United Kingdom', 'Canada'))

# Plot distribution of monthly spent for each country
distribution <- ggplot( data = top4_df, aes(x = residence, y = monthly_spent)) +
  geom_boxplot() +
  ggtitle("Distribution of Monthly Learning Expenses by Country") +
  xlab(NULL) +
  ylab("Monthly spent (US dollars)") +
  theme_bw()

ggsave("plots/spent_distribution.png", plot = distribution, width = 8, height = 6)

# Filter out responses with unrealistic amount spent for learning code in a month (20,000 USD)
filtered_top4_df  <- top4_df %>% 
  filter(monthly_spent < 20000)

# Assign index for each rows to easily apply filters
filtered_top4_df <- filtered_top4_df %>%
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

#' Applying specific filters for each country after examining the plot

canada_outlier <- outlier_df(filtered_top4_df, "Canada", 5000)
filtered_top4_df  <-  filtered_top4_df %>% 
  filter(!(index %in% canada_outlier$index))

india_outlier <- outlier_df(filtered_top4_df, "India", 5000)
filtered_top4_df  <-  filtered_top4_df %>% 
  filter(!(index %in% india_outlier$index))

uk_outlier <- outlier_df(filtered_top4_df, "United Kingdom", 5000)
filtered_top4_df  <-  filtered_top4_df %>% 
  filter(!(index %in% uk_outlier$index))

us_outlier <- outlier_df(filtered_top4_df, "United States of America", 5000)
us_outlier <- filtered_top4_df %>%
  filter(
    residence == "United States of America",
    monthly_spent >= 6000,
    months_code <= 3 | bootcamp_attend != 1
  )
filtered_top4_df  <-  filtered_top4_df %>% 
  filter(!(index %in% us_outlier$index))

# Generating plot for filtered data
distribution2 <- ggplot( data = filtered_top4_df, aes(x = residence, y = monthly_spent)) +
  geom_boxplot() +
  ggtitle("Distribution of Monthly Learning Expenses by Country") +
  xlab(NULL) +
  ylab("Monthly spent (US dollars)") +
  theme_bw()

ggsave("plots/spent_distribution2.png", plot = distribution2, width = 8, height = 6)

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