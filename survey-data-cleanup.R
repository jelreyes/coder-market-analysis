#' We are using the open dataset from freeCodeCamp's 2018 survey
#' https://github.com/freeCodeCamp/2018-new-coder-survey/blob/master/raw-data/2018-new-coder-survey.csv
#' This script cleans the data with 31,226 rows and 136 columns for further analysis

library(readr)
library(tidyverse)
library(dplyr)

#' Main function to read, clean, and write the survey data in the same directory
#' Below this are the functions used for cleaning the data
main <- function() {
  data_path <- "2018-new-coder-survey.csv"
  coder_survey <- read_csv(data_path)
  
  cleaned_data <- clean_data(coder_survey)
  
  output_path <- "cleaned-coder-survey.csv"
  
  write_csv(cleaned_data, output_path)
}

#' Functions to apply sanity checks / filter for the data
clean_data <- function(data) {
  # Rename columns
  renamed_data <- rename_col(data)
  
  # Filtering steps
  filtered_data <- renamed_data %>%
    
    # Remove answered form too fast
    filter(difftime(ts_end, ts_start, units = "mins") > 1) %>%
    
    # Remove invalid number of kids (eg, answer is with children but answer is 0 in no. of children)
    filter((has_chldren == 1 & (num_children > 0 | is.na(num_children))) |
             (has_chldren == 0 & (num_children == 0 | is.na(num_children))) |
             (is.na(has_chldren) & is.na(num_children))) %>%
    filter(num_children < 20 | is.na(num_children)) %>%
    
    # Remove when years spent in coding is more than the age
    filter(is.na(age) | is.na(months_code) | age * 12 > months_code) %>%
    
    # Cap age to 116 (oldest living person in the world as of Sep 2024 accd to Guinness World Records)
    filter(is.na(age) | age < 116) %>%
    
    # Remove when last year income > 1m
    filter(lastyr_income < 1000000 | is.na(lastyr_income))
  
  # Transform some columns to become countable
  prefixes <- c("career_", "rsrc_", "codepodcast_", "codeyt_", "codeevent_")
  pattern <- paste0(prefixes, collapse = "|")
  
  bool_df <- filtered_data %>%
    mutate(across(matches(pattern) & !ends_with("_other"), 
                  ~ ifelse(!is.na(.), 1, .)))
  
  # Rewrite values in "Other" in title case for later presentation
  final_df <- bool_df %>%
    mutate(across(ends_with("_other"), ~ ifelse(is.na(.), ., tools::toTitleCase(.))))
  
  final_df
}

# Function to rename columns
rename_col <- function(data) {
  data %>%
    rename(
      id = "#",
      is_softdev = "Are you already working as a software developer?",
      is_first_softdev_job = "Is this your first software development job?",
      months_jobsearch = "Before you got this job, how many months did you spend looking for a job?",
      
      # work preferences and interest
      work_pref = "Would you prefer to...",
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
      career_dataeng = "Data Engineer",
      career_other = "Other...19",
      
      job_start = "When do you plan to start applying for developer jobs?",
      salary_frstjob = "About how much money do you expect to earn per year at your first developer job (in US Dollars)?",
      loc_pref = "Would you prefer to work...",
      will_reloc = "Are you willing to relocate for a job?",
      
      # reasons to code
      code_goal = "What is your biggest reason for learning to code?",
      code_goal_other = "Other...25",
      
      # online learning resources
      rsrc_fcc = "freeCodeCamp",
      rsrc_mdn = "Mozilla Developer Network (MDN)",
      rsrc_stack = "Stack Overflow",
      rsrc_edx = "EdX",
      rsrc_coursera = "Coursera",
      rsrc_khan = "Khan Academy",
      rsrc_pluralsight = "Pluralsight",
      rsrc_codecademy = "Codecademy",
      rsrc_udacity = "Udacity",
      rsrc_udemy = "Udemy",
      rsrc_codewars = "Code Wars",
      rsrc_treehouse = "Treehouse",
      rsrc_HackerRank = "HackerRank",
      rsrc_fem = "Front End Masters",
      rsrc_lynda = "Lynda.com",
      rsrc_egghead = "Egghead.io",
      rsrc_csstricks = "CSS Tricks",
      rsrc_other = "Other...43",
      
      #in person coding related events attended
      codeevent_fcc = "freeCodeCamp study groups",
      codeevent_hckathon = "hackathons",
      codeevent_conf = "conferences",
      codeevent_wrkshp = "workshops",
      codeevent_startupwknd = "Startup Weekend",
      codeevent_nodeschool = "NodeSchool",
      codeevent_wwc = "Women Who Code",
      codeevent_gdi = "Girl Develop It",
      codeevent_coderdojo = "CoderDojo",
      codeevent_meetupcom = "Meetup.com events",
      codeevent_railsbridge = "RailsBridge",
      codeevent_gamejam = "Game Jam",
      codeevent_railsgrl = "Rails Girls",
      codeevent_djangogrl = "Django Girls",
      codeevent_wkndbootcmp = "weekend bootcamps",
      codeevent_other = "Other...59",
      
      #coding related podcasts listened to
      codepodcast_fcc = "The freeCodeCamp Podcast",
      codepodcast_codenewbie = "Code Newbie",
      codepodcast_changelog = "The Changelog",
      codepodcast_sed = "Software Engineering Daily",
      codepodcast_jsjabber = "JavaScript Jabber",
      codepodcast_syntax = "Syntax.fm",
      codepodcast_ltcwm = "Learn To Code With Me",
      codepodcast_fullstckrad = "Full Stack Radio",
      codepodcast_frntendhh = "Front End Happy Hour",
      codepodcast_codeblck = "Coding Blocks",
      codepodcast_sts = "Shop Talk Show",
      codepodcast_devtea = "Developer Tea",
      codepodcast_prgthrdwn = "Programming Throwdown",
      codepodcast_gkspk = "Geek Speak",
      codepodcast_hanselmins = "Hanselminutes",
      codepodcast_tlkpython = "Talk Python To Me",
      codepodcast_rubyrogues = "Ruby Rogues",
      codepodcast_codepen = "CodePen Radio",
      codepodcast_ser = "Software Engineering Radio",
      codepodcast_other = "Other...79",
      
      # coding related youtube channel
      codeyt_mit = "MIT Open Courseware",
      codeyt_fcc = "freeCodeCamp's YouTube channel",
      codeyt_comphile = "Computerphile",
      codeyt_devtips = "DevTips",
      codeyt_csdojo = "CS Dojo",
      codeyt_engtruth = "Engineered Truth",
      codeyt_learncode = "LearnCode.Academy",
      codeyt_lvluptuts = "LevelUpTuts",
      codeyt_fff = "Fun Fun Function",
      codeyt_ct360 = "Coding Tutorials 360",
      codeyt_codetrain = "Coding Train",
      codeyt_drkbanas = "Derek Banas",
      codeyt_simplilrn = "Simplilearn",
      codeyt_bulldog = "Simple Programmer (Bulldog Mindset)",
      codeyt_mozilla = "Mozilla Hacks",
      codeyt_googledev = "Google Developers",
      codeyt_other = "Other...96",
      
      # learning habit
      hrs_study = "About how many hours do you spend learning each week?",
      months_code = "About how many months have you been programming for?",
      
      # boothcamp attended
      bootcamp_attend = "Have you attended a full-time coding bootcamp?",
      bootcamp_name = "Which one?",
      boothcamp_finish = "Have you finished yet?",
      boothcamp_loan = "Did you take out a loan to pay for the bootcamp?",
      boothcamp_recommend = "Based on your experience, would you recommend this bootcamp to your friends?",
      
      amt_spent_code = "Aside from university tuition, about how much money have you spent on learning to code so far (in US dollars)?",
      
      # personal information
      age = "How old are you?",
      gender = "What's your gender?",
      gender_other = "Other...107",
      citizenship = "Which country are you a citizen of?",
      residence = "Which country do you currently live in?",
      rsdnce_pop = "About how many people live in your city?",
      is_etnic_grp = "Are you an ethnic minority in your country?",
      speaking_lang = "Which language do you you speak at home with your family?",
      degree = "What's the highest degree or level of school you have completed?",
      major = "What was the main subject you studied in university?",
      marital = "What's your marital status?",
      
      # financial status
      has_financial_dep = "Do you financially support any dependents?",
      has_chldren = "Do you have children?",
      num_children = "How many children do you have?",
      is_spprt_relative = "Do you financially support any elderly relatives or relatives with disabilities?",
      has_debt = "Do you have any debt?",
      has_mortgage = "Do you have a home mortgage?",
      amt_mortgage = "About how much do you owe on your home mortgage (in US Dollars)?",
      has_studloan = "Do you have student loan debt?",
      amt_studloan = "About how much do you owe in student loans (in US Dollars)?",
      
      # employment
      employmnt_stat = "Regarding employment status, are you currently...",
      employmnt_stat_other = "Other...126",
      field = "Which field do you work in?",
      lastyr_income = "About how much money did you make last year (in US dollars)?",
      workperday_min = "About how many minutes does it take you to get to work each day?",
      is_underemployed = "Do you consider yourself under-employed?",
      served_military = "Have you served in your country's military before?",
      has_disability_bnft = "Do you receive disability benefits from your government?",
      has_internet = "Do you have high speed internet at your home?",
      
      # Others
      ts_start = "Start Date (UTC)",
      ts_end = "Submit Date (UTC)",
      network_id = "Network ID"
    )
}

# Run the main function
main()
