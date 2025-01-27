
### Welcome to Intro to R!

# Agenda for Day 2

# Revisiting Exercises from Day 1

# Importing Data
## CSVs
## Google Sheets
## .sav, etc.
## Qualtrics (package)
## Databases

# Exploratory Data Analysis
## Functions
## Skimr
## Counting & Dealing with NAs

# Cleaning Data
## Data Types & Cleaning/Recoding Pipelines

# ACTIVITY 3
## Analyzing basic statistics in the survey data.

# Cleaning Data - Full Example
## Applied to survey dataset

# Manipulating DataFrame Width/Length

# Basic Analysis
## Tables (Prop Tables, Cross Tabs; Two Way Tables, Three Way Tables)
## Means, SDs
## SEs, Confidence Intervals
## CIs for Proportions

## T-Tests
## ANOVAs

## Correlations

# Basic Regressions
## Linear Regression
## Linear Regression Output
## Logistic Regression
## Tools to Help (e.g., `broom`)

# Basic Text Analysis
## Text Manipulation & Pattern Matching
## Content Analysis
## Inter-rater Reliability

# ACTIVITY 4
## Complex analysis applied to survey dataset

# ==========================================================

# Remember packages? Let's do best practice and call them first:

library(tidyverse)
# You can also use "require(tidyverse)" but it'll execute code without it...!

# Answers for Activity 2

# 0. In another new R script...
# 1. Load the 'mtcars' data (it has information about cars and their specs), and look through the data frame structure.

mtcars.df <- mtcars
mtcars.df 
str(mtcars.df)
summary(mtcars.df)

# 2. Rename the first column "car_models" (currently it is unnamed).

colnames(mtcars)
rownames(mtcars)
dimnames(mtcars)

mtcars.df$car_models <- rownames(mtcars.df) #tibble::rownames_to_column(mtcars.df, "car_models")

# 3. Select car_models, mpg, cyl, and gear columns.

mtcars.df %>% 
  select(car_models, mpg, cyl, gear)

mtcars.subset <- mtcars.df %>% 
  select(car_models, mpg, cyl, gear) 

# 4. Filter the data to retain only the 4 cylinder records.

mtcars.df %>% 
  select(car_models, mpg, cyl, gear) %>%
  filter(cyl == 4)

mtcars.subset %>%
  filter(cyl == 4)

mtcars.filtered <- mtcars.subset %>%
  filter(cyl == 4)

# 5. Create a new column that calculates MPG per cylinder.

mtcars.filtered %>%
  mutate(mpg_per_cyl = mpg/cyl)

# 6. If you haven't already, combine all of the above into a single tidyverse "pipeline."

mtcars.df %>% 
  select(car_models, mpg, cyl, gear) %>%
  filter(cyl == 4) %>%
  mutate(mpg_per_cyl = mpg/cyl)

# 7. Challenge: Starting from the base mtcars dataset, calculate mean MPG per cylinder group. (Hint: You'll need to use group_by and summarise).

mtcars.df %>% 
  select(mpg, cyl, vs) %>%
  group_by(vs) %>%
  summarise(mean_mpg_vs = mean(mpg))

## TODAY'S LESSON

# We'll be using survey data! 
# Check it out: https://docs.google.com/spreadsheets/d/1u8_iW25YpL2__q_0FJQ5CQ-LNQkUchz3yVOFUm2oHIQ/edit

# Importing Data

## CSVs
# from tidyverse
# https://readr.tidyverse.org/reference/read_delim.html
#library(readr)
survey_data.df <- read_csv("https://alexleavitt.net/showerthoughtsurveys/1_musicinyourhead/musicinyourheadsurvey.csv")
str(survey_data.df)

## Google Sheets
install.packages('googlesheets4')
library(googlesheets4)
survey_data.df <- read_sheet('https://docs.google.com/spreadsheets/d/1u8_iW25YpL2__q_0FJQ5CQ-LNQkUchz3yVOFUm2oHIQ/edit')
str(survey_data.df)

## Qualtrics (package)
# https://cran.r-project.org/web/packages/qualtRics/vignettes/qualtRics.html

## .sav, etc.
# https://cran.r-project.org/web/packages/foreign/index.html

## Databases
# https://github.com/r-dbi/RMySQL

# Exploratory Data Analysis

#survey_data.df <- read_sheet('https://docs.google.com/spreadsheets/d/1u8_iW25YpL2__q_0FJQ5CQ-LNQkUchz3yVOFUm2oHIQ/edit')
#survey_data.df <- read_csv("https://alexleavitt.net/showerthoughtsurveys/1_musicinyourhead/musicinyourheadsurvey.csv")

# Best practice: save a backup, so you don't have to reload
survey_data.df.raw <- survey_data.df

## Functions
dimnames(survey_data.df)
plot(survey_data.df$`Timestamp`)

## Skimr
library(skimr)
skim(survey_data.df)

## Counting & Dealing with NAs, NULLs, NaNs

# Finding NAs:
is.na(survey_data.df) # this will return booleans for every datapoint...
sum(is.na(survey_data.df)) # count across all cells (row/column)

# Removing NAs:
# filter(!is.na(variable_x))

# na.omit()
# drop_na()

# Cleaning Data
## Cleaning/Recoding Pipelines

# A couple of key functions:
## rename: allows you to change the name of your column - good when you have survey platforms that put the question wording in the column
## as.factor: changes characters into categorical 'factors'
## fct_relevel and fct_recode: allows you to change factor 'levels' and associate them with numbers
## ifelse and case_when: logical functions to automate recoding

cleaning.test <- survey_data.df %>% 
  select(`How frequently do you hear music playing in your head, if at all?`,
         `When you hear music in your head, is it a pleasant or annoying experience?`) %>% 
  rename(how_frequently_hear_music = `How frequently do you hear music playing in your head, if at all?`,
         pleasant_or_annoying = `When you hear music in your head, is it a pleasant or annoying experience?`)
View(cleaning.test)

cleaning.test <- cleaning.test %>%
  mutate(how_frequently_hear_music = fct_relevel(as.factor(how_frequently_hear_music),
                                                 'Never', 'Rarely', 'Sometimes', 'Frequently', 'Almost all the time'),
          how_frequently_hear_music_num = fct_recode(how_frequently_hear_music,
                                                               '1' = 'Never',
                                                               '2' = 'Rarely',
                                                               '3' = 'Sometimes',
                                                               '4' = 'Frequently',
                                                               '5' = 'Almost all the time')) %>%
    mutate(how_frequently_hear_music_num = as.numeric(how_frequently_hear_music_num))
View(cleaning.test)

cleaning.test <- cleaning.test %>%
  mutate(how_frequent_binary = ifelse(how_frequently_hear_music_num > 3, 'High', 'Low')) %>%
  mutate(how_frequent_threes = case_when(how_frequently_hear_music_num > 3 ~ 'High',
                                         how_frequently_hear_music_num == 3 ~ 'Mid',
                                         how_frequently_hear_music_num < 3 ~ 'Low'))
View(cleaning.test)


# ACTIVITY 3
## Analyzing basic statistics in the survey data.

# 1. Create a version of the original survey_data.df table where you choose 
# 7 variables (include how_frequently_hear_music; and 3 others should be numeric, and 3 others categorical).

# 2. Explore the distributions of gender, age, country, religiosity, and political affiliation.

# 3. Does how_frequently_hear_music vary by gender?
# 3b. What about age?
# 3c. Choose another variable you picked. How does it vary by country? 
# 3d. By religiosity?


# Cleaning Data - Full Example
# What this looks like in practice:

survey_data.df <- rename(survey_data.df, 
                         timestamp = `Timestamp`, 
                         how_frequently_hear_music = `How frequently do you hear music playing in your head, if at all?`,
                         kind_of_music = `When you hear music in your head, what kind of music is it?`,
                         pleasant_or_annoying = `When you hear music in your head, is it a pleasant or annoying experience?`,
                         music_last_how_long = `When you hear music in your head, how long does it tend to last?`,
                         music_type_hear_most_often = `During an average period when you hear music in your head, which of the following do you tend to hear MOST often?`,
                         how_often_daydream = `How often do you find yourself fantasizing or day-dreaming?`,
                         visualize_objects_easy_hard = `Is it easy or difficult for you to visualize objects, places, or concepts in your head?`,
                         focus_attention_easy_hard = `Is it easy or difficult for you to focus your attention on everyday tasks or conversations with people?`,
                         inner_voice_yn = `Do you have an "inner voice" with which you speak to yourself?`,
                         how_often_listen_play_music = `On average, how many days per week do you listen to or play music?`,
                         hobbies_related_music_yn = `Are your hobbies related to music?`,
                         professional_music_training_yn = `Have you ever been professionally trained as a musician, singer, composer, etc.?`,
                         occupation_music_yn = `Is your occupation related to music?`,
                         grid_how_often_whistle = `How often do you do any of the following when alone or in small groups? [Whistle]`,
                         grid_how_often_dance = `How often do you do any of the following when alone or in small groups? [Dance]`,
                         grid_how_often_hum = `How often do you do any of the following when alone or in small groups? [Hum]`,
                         grid_how_often_sing = `How often do you do any of the following when alone or in small groups? [Sing]`,
                         grid_how_often_tap = `How often do you do any of the following when alone or in small groups? [Tap feet/fingers]`,
                         stucksong_pleasing_annoying = `If you happen to get one song stuck in your head, how pleasing or annoying is that experience?`,
                         lastmonth_satisfaction = `During the last month, how satisfied have you been with your life?`,
                         lastmonth_goodbad_felt = `During the last month, how good or bad have you felt?`,
                         introvert_extrovert = `Would you consider yourself to be more of an introverted or an extroverted individual?`,
                         year_born = `What year were you born?`,
                         country = `What country do you currently reside in?`,
                         education_years = `How many years of formal education have you had?`,
                         gender = `What gender do you identify as?`,
                         religious_yn = `Would you consider yourself to be a religious person?`,
                         political_pov = `How would you describe your own political point of view?`,
                         hearmusic_experience_openend = `When you hear music in your head, describe what the experience is like (in as much detail as possible).`)

# RECODING DATA
survey_data.df <- survey_data.df %>%
  mutate(how_frequently_hear_music = fct_relevel(as.factor(how_frequently_hear_music),
                                                 'Never', 'Rarely', 'Sometimes', 'Frequently', 'Almost all the time'),
         how_frequently_hear_music_num = as.numeric(fct_recode(how_frequently_hear_music,
                                                               '1' = 'Never',
                                                               '2' = 'Rarely',
                                                               '3' = 'Sometimes',
                                                               '4' = 'Frequently',
                                                               '5' = 'Almost all the time')),
         kind_of_music = fct_relevel(as.factor(kind_of_music),
                                     'Only vocal music (e.g., word sounds)', 'Both vocal and instrumental music', 'Only instrumental music (e.g., non-word sounds)'),
         pleasant_or_annoying = fct_relevel(as.factor(pleasant_or_annoying),
                                            'Very annoying', 'Somewhat annoying', 'Neither pleasant nor annoying', 'Somewhat pleasant', 'Very pleasant'),
         pleasant_or_annoying_num = as.numeric(fct_recode(pleasant_or_annoying,
                                                          '1' = 'Very annoying',
                                                          '2' = 'Somewhat annoying',
                                                          '3' = 'Neither pleasant nor annoying',
                                                          '4' = 'Somewhat pleasant',
                                                          '5' = 'Very pleasant')),
         music_last_how_long = fct_relevel(as.factor(music_last_how_long),
                                           'Less than 15 minutes', 'Between 30 minutes and 15 minutes', 'Between 1 hour and 30 minutes', 'A couple of hours', 'Many hours'),
         music_last_how_long_num = as.numeric(fct_recode(music_last_how_long,
                                                         '1' = 'Less than 15 minutes',
                                                         '2' = 'Between 30 minutes and 15 minutes',
                                                         '3' = 'Between 1 hour and 30 minutes',
                                                         '4' = 'A couple of hours',
                                                         '5' = 'Many hours')),
         music_type_hear_most_often = ifelse(music_type_hear_most_often %in% c('One song or tune repeated',
                                                                               'Multiple songs or tunes in sequence, one after another',
                                                                               'A random assortment of sounds'), music_type_hear_most_often, 'Another experience'),
         music_type_hear_most_often = fct_relevel(as.factor(music_type_hear_most_often),
                                                  'One song or tune repeated',
                                                  'Multiple songs or tunes in sequence, one after another',
                                                  'A random assortment of sounds',
                                                  'Another experience'),
         how_often_daydream = fct_relevel(as.factor(how_often_daydream),
                                          'Never', 'Rarely', 'Sometimes', 'Frequently', 'Almost all the time'),
         how_often_daydream_num = as.numeric(fct_recode(how_often_daydream,
                                                        '1' = 'Never',
                                                        '2' = 'Rarely',
                                                        '3' = 'Sometimes',
                                                        '4' = 'Frequently',
                                                        '5' = 'Almost all the time')),
         visualize_objects_easy_hard = fct_relevel(as.factor(visualize_objects_easy_hard),
                                                   'Very difficult', 'Somewhat difficult', 'Neither easy nor difficult', 'Somewhat easy', 'Very easy'),
         visualize_objects_easy_hard_num = as.numeric(fct_recode(visualize_objects_easy_hard,
                                                                 '1' = 'Very difficult',
                                                                 '2' = 'Somewhat difficult',
                                                                 '3' = 'Neither easy nor difficult',
                                                                 '4' = 'Somewhat easy',
                                                                 '5' = 'Very easy')),
         focus_attention_easy_hard = fct_relevel(as.factor(focus_attention_easy_hard),
                                                 'Very difficult', 'Somewhat difficult', 'Neither easy nor difficult', 'Somewhat easy', 'Very easy'),
         focus_attention_easy_hard_num = as.numeric(fct_recode(focus_attention_easy_hard,
                                                               '1' = 'Very difficult',
                                                               '2' = 'Somewhat difficult',
                                                               '3' = 'Neither easy nor difficult',
                                                               '4' = 'Somewhat easy',
                                                               '5' = 'Very easy')),
         inner_voice_yn = as.factor(inner_voice_yn),
         inner_voice_yn_num = as.numeric(fct_recode(inner_voice_yn,
                                                    '0' = 'No',
                                                    '1' = 'Yes')),
         how_often_listen_play_music = as.numeric(how_often_listen_play_music),
         hobbies_related_music_yn = as.factor(hobbies_related_music_yn),
         hobbies_related_music_yn_num = as.numeric(fct_recode(hobbies_related_music_yn,
                                                              '0' = 'No',
                                                              '1' = 'Yes')),
         professional_music_training_yn = as.factor(professional_music_training_yn),
         professional_music_training_yn_num = as.numeric(fct_recode(professional_music_training_yn,
                                                                    '0' = 'No',
                                                                    '1' = 'Yes')),
         occupation_music_yn = as.factor(occupation_music_yn),
         occupation_music_yn_num = as.numeric(fct_recode(occupation_music_yn,
                                                         '0' = 'No',
                                                         '1' = 'Yes')),
         stucksong_pleasing_annoying = fct_relevel(as.factor(stucksong_pleasing_annoying),
                                                   'Very annoying', 'Somewhat annoying', 'Neither annoying nor pleasing', 'Somewhat pleasing', 'Very pleasing'),
         stucksong_pleasing_annoying_num = as.numeric(fct_recode(stucksong_pleasing_annoying,
                                                                 '1' = 'Very annoying',
                                                                 '2' = 'Somewhat annoying',
                                                                 '3' = 'Neither annoying nor pleasing',
                                                                 '4' = 'Somewhat pleasing',
                                                                 '5' = 'Very pleasing')),
         lastmonth_satisfaction = fct_relevel(as.factor(lastmonth_satisfaction),
                                              'Not satisfied at all', 'Only a little satisfied', 'Somewhat satisfied', 'Very satisfied', 'Extremely satisfied'),
         lastmonth_satisfaction_num = as.numeric(fct_recode(lastmonth_satisfaction,
                                                            '1' = 'Not satisfied at all',
                                                            '2' = 'Only a little satisfied',
                                                            '3' = 'Somewhat satisfied',
                                                            '4' = 'Very satisfied',
                                                            '5' = 'Extremely satisfied')),
         lastmonth_goodbad_felt = fct_relevel(as.factor(lastmonth_goodbad_felt),
                                              'Very bad', 'Somewhat bad', 'Neither good nor bad', 'Somewhat good', 'Very good'),
         lastmonth_goodbad_felt_num = as.numeric(fct_recode(lastmonth_goodbad_felt,
                                                            '1' = 'Very bad',
                                                            '2' = 'Somewhat bad',
                                                            '3' = 'Neither good nor bad',
                                                            '4' = 'Somewhat good',
                                                            '5' = 'Very good')),
         introvert_extrovert = fct_relevel(as.factor(introvert_extrovert),
                                           'A lot more introverted', 'A bit more introverted', 'About the same', 'A bit more extroverted', 'A lot more extroverted'),
         introvert_extrovert_num = as.numeric(fct_recode(introvert_extrovert,
                                                         '1' = 'A lot more introverted',
                                                         '2' = 'A bit more introverted',
                                                         '3' = 'About the same',
                                                         '4' = 'A bit more extroverted',
                                                         '5' = 'A lot more extroverted'))) %>%
  mutate_at(vars(starts_with('grid_how_often')), list(~fct_relevel(as.factor(.),
                                                                   'Never', 'Sometimes', 'Often'))) %>%
  mutate_at(vars(starts_with('grid_how_often')), list(num = ~as.numeric(fct_recode(.,
                                                                                   '1' = 'Never',
                                                                                   '2' = 'Sometimes',
                                                                                   '3' = 'Often')))) %>%
  mutate(age = 2021 - year_born,
         country = as.factor(country),
         education_years = ifelse(education_years %in% c('12', '13', '14', '15', '16',
                                                         'More than 16',
                                                         'More than 12'), '12 or more', 'Less than 12'),
         education_years = as.factor(education_years),
         gender = case_when(gender %in% c('Male', 'Female', 'Non-binary or Another Gender') ~ gender, 
                            gender == 'Other' ~ 'Non-binary or Another Gender'),
         gender = fct_relevel(as.factor(gender), 'Male', 'Female'),
         religious_yn = as.factor(religious_yn),
         political_pov = fct_relevel(as.factor(political_pov),
                                     'Not political', 'Very conservative', 'Conservative', 'Moderate', 'Liberal', 'Very liberal')) %>%
  mutate(hearmusic_experience_openend = hearmusic_experience_openend) %>%
  mutate(country = fct_recode(country, 'United Kingdom' = 'England', 
                              'United Kingdom' = 'Scotland', 
                              #'United Kingdom' = 'Northern Ireland',
                              'United Kingdom' = 'Wales')) %>%
  mutate(country_usa_yn = as.factor(ifelse(country == 'United States', 'United States', 'Outside US')))

# One complex note:
# You'll see that we used a line like this:

# mutate_at(vars(starts_with('grid_how_often')), list(~fct_relevel(as.factor(.),
#                                                                 'Never', 'Sometimes', 'Often')))

# There are ways to use functions like mutate ACROSS different columns. It's
# a bit more complex, so we don't teach it here, but you can use a variety of
# tidyverse functions like mutate_at (and others, like mutate_all), ways to 
# select multiple columns like starts_with, and the application of functions
# across a list of items (ie., the list ~ notation).
# You can learn more about these on the tidyverse page: 
# https://dplyr.tidyverse.org/reference/mutate_all.html

# Returning to skim, briefly:
skim(survey_data.df)

# Manipulating DataFrame Width/Length

# Learning "pivot" behavior is important for manipulating data, because of the 
# ways in which some data gets aggregated for calculations

cleaning.test #Let's return to this temp dataframe

cleaning.test %>% 
  select(pleasant_or_annoying) %>%
  group_by(pleasant_or_annoying) %>%
  summarise(count = n())

cleaning.test %>% 
  select(pleasant_or_annoying) %>%
  pivot_longer(cols = pleasant_or_annoying, names_to = 'variable', values_to = 'value') %>%
  group_by(value) %>%
  summarise(count = n())

cleaning.test %>%
  select(pleasant_or_annoying) %>%
  pivot_longer(cols = pleasant_or_annoying, names_to = 'variable', values_to = 'value') %>%
  group_by(variable, value) %>%
  summarise(count = n())

cleaning.test %>% 
  select(pleasant_or_annoying, how_frequent_binary) %>%
  pivot_longer(cols = pleasant_or_annoying:how_frequent_binary, names_to = 'variable', values_to = 'value') %>%
  group_by(variable, value) %>%
  summarise(count = n())

# Basic Analysis
## Tables (Prop Tables, Cross Tabs; Two Way Tables, Three Way Tables)

# Basic table results: shows just counts
survey_data.df %>% 
  filter(country == 'United States') %>%
  select(how_often_daydream) %>%
  table()

# One way proportions table: shows proportions up to 1 (ie., 100%)
survey_data.df %>% 
  filter(country == 'United States') %>%
  select(how_often_daydream) %>%
  table() %>% 
  prop.table() %>% 
  round(., digits = 2)

# Two way basic table
survey_data.df %>% 
  filter(country == 'United States') %>%
  select(how_often_daydream, focus_attention_easy_hard) %>%
  table()

# Two way prop table
survey_data.df %>% 
  filter(country == 'United States') %>%
  select(how_often_daydream, focus_attention_easy_hard) %>%
  table() %>% 
  prop.table(., margin = 2) %>% 
  round(., digits = 2)

# Three way prop table
survey_data.df %>% 
  filter(country == 'United States') %>%
  select(inner_voice_yn, how_often_daydream, focus_attention_easy_hard) %>%
  table() %>% 
  prop.table(., margin = 2) %>% 
  round(., digits = 2)

## Means, SDs

survey_data.df %>% 
  select(how_often_daydream_num, country_usa_yn) %>%
  group_by(country_usa_yn) %>% 
  summarise(n = sum(!is.na(how_often_daydream_num)), 
            mean = mean(how_often_daydream_num, na.rm = T), 
            sd = sd(how_often_daydream_num, na.rm = T))

survey_data.df %>% 
  select(how_often_daydream_num, country_usa_yn) %>%
  na.omit() %>%
  group_by(country_usa_yn) %>% 
  summarise(n = n(), 
            mean = mean(how_often_daydream_num), 
            sd = sd(how_often_daydream_num))

## SEs, Confidence Intervals

survey_data.df %>% 
  select(how_often_daydream_num, country_usa_yn) %>%
  na.omit() %>%
  group_by(country_usa_yn) %>% 
  summarise(n = n(), 
            mean = mean(how_often_daydream_num), 
            sd = sd(how_often_daydream_num),
            se = sd/sqrt(n), 
            ci_low = mean - qnorm(1 - (.05/2))*se,  # qnorm() --> Z-score
            ci_high = mean + qnorm(1 - (.05/2))*se)

# Calculating across variables: this is where pivoting comes in!

survey_data.df %>%
  select(how_often_daydream_num, pleasant_or_annoying_num, country_usa_yn) %>%
  na.omit() %>%
  pivot_longer(cols = c(how_often_daydream_num, pleasant_or_annoying_num), names_to = 'variable', values_to = 'value') %>%
  group_by(country_usa_yn, variable) %>% 
  summarise(n = n(), 
            mean = mean(value), 
            sd = sd(value),
            se = sd/sqrt(n), 
            ci_low = mean - qnorm(1 - (.05/2))*se,  # qnorm() --> Z-score
            ci_high = mean + qnorm(1 - (.05/2))*se)

survey_data.df %>% 
  select(how_often_daydream_num, pleasant_or_annoying_num, country_usa_yn) %>%
  na.omit() %>%
  pivot_longer(cols = c(how_often_daydream_num, pleasant_or_annoying_num), names_to = 'variable', values_to = 'value') %>%
  # Look at what happens when we switch the grouping order
  group_by(variable, country_usa_yn) %>%
  summarise(n = n(), 
            mean = mean(value), 
            sd = sd(value),
            se = sd/sqrt(n), 
            ci_low = mean - qnorm(1 - (.05/2))*se,  # qnorm() --> Z-score
            ci_high = mean + qnorm(1 - (.05/2))*se)


## CIs for Proportions

survey_data.df %>% 
  # change back to the factor version of the variable
  select(how_often_daydream, country_usa_yn) %>%
  na.omit() %>%
  group_by(country_usa_yn, how_often_daydream) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), 
         se = sqrt(prop * (1 - prop) / n), #calculated a bit different from the one for means
         ci_lo = prop - qnorm(1 - (.05/2))*se, 
         ci_hi = prop + qnorm(1 - (.05/2))*se)

## Survey Data Pipelines

library(srvyr)
# https://cran.r-project.org/web/packages/srvyr/vignettes/srvyr-vs-survey.html
# The srvyr package is a tidy-like package for survey analysis

# One variable, means
survey_data.df %>%
  select(how_frequently_hear_music_num) %>%
  na.omit() %>%
  as_survey_design() %>%
  #as_survey_design(weights = weight) %>%
  summarize(frequency_mean = survey_mean(how_frequently_hear_music_num, vartype = 'ci', level = c(0.95)))

# Two variables, means
survey_data.df %>%
  select(how_frequently_hear_music_num, pleasant_or_annoying_num) %>%
  na.omit() %>%
  pivot_longer(cols = how_frequently_hear_music_num:pleasant_or_annoying_num, names_to = 'variable', values_to = 'value') %>%
  as_survey_design() %>%
  group_by(variable) %>%
  summarize(mean = survey_mean(value, vartype = 'ci', level = c(0.95)))

# One variable, proportions
survey_data.df %>%
  select(how_frequently_hear_music) %>%
  na.omit() %>%
  as_survey_design() %>%
  group_by(how_frequently_hear_music) %>%
  summarize(proportions = survey_mean(vartype = 'ci', level = c(0.95)),
            total = survey_total())

# Two variables, proportions
survey_data.df %>%
  select(how_frequently_hear_music, pleasant_or_annoying) %>%
  na.omit() %>%
  pivot_longer(cols = how_frequently_hear_music:pleasant_or_annoying, names_to = 'variable', values_to = 'value') %>%
  as_survey_design() %>%
  group_by(variable, value) %>%
  summarize(proportions = survey_mean(vartype = 'ci', level = c(0.95)),
            total = survey_total())

## Statistical Testing

## T-Tests
# Some basic functions in R aren't tidyverse friendly, so we have to do it a bit more manually.
# Stats help: https://uc-r.github.io/t_test

t.test(survey_data.df$how_often_daydream_num, mu = 3)

# Note: some cool packages can help you explore this in a more automated way
# e.g., https://cran.r-project.org/web/packages/infer/
#       https://cran.r-project.org/web/packages/infer/vignettes/t_test.html

## ANOVAs

one.way <- aov(how_often_daydream_num ~ country_usa_yn, data = survey_data.df)
summary(one.way)

## Correlations

# Basic approach:
cor(survey_data.df$how_frequently_hear_music_num, survey_data.df$how_often_daydream_num,
    use = "complete.obs") # this argument helps us deal with NAs

# More complicated, using a correlation package:
library(correlation)
survey_data.df.corr <- survey_data.df %>% select(how_frequently_hear_music_num, 
                                                 how_often_daydream_num, 
                                                 visualize_objects_easy_hard_num, 
                                                 focus_attention_easy_hard_num) %>%
  correlation()
summary(survey_data.df.corr)

# Basic Regressions

## Linear Regression

linear.model <- lm(how_frequently_hear_music_num ~ how_often_daydream_num,
                   data = survey_data.df)
summary(linear.model)

linear.model <- lm(how_frequently_hear_music_num ~ how_often_daydream_num +
                                                   visualize_objects_easy_hard_num + 
                                                   focus_attention_easy_hard_num +
                                                   gender +
                                                   age,
                   data = survey_data.df)
summary(linear.model)

## Logistic Regression
survey_data.df$inner_voice_yn_num
survey_data.df$inner_voice_yn_num <- survey_data.df$inner_voice_yn_num - 1
  
logistic.model <- glm(inner_voice_yn_num ~ how_often_daydream_num,
                   data = survey_data.df,
                   family = 'binomial')
summary(logistic.model)

## Tools to Help (e.g., `broom`)
library(broom)
summary(linear.model)
tidy(linear.model, conf.int = TRUE)

# =================================================
# RETURN TO THIS SPOT ON DAY 3
# =================================================

# Basic Experiments
# https://declaredesign.org/r/estimatr/

library(estimatr)
survey_data.df$experiment_holdout <- survey_data.df$inner_voice_yn
experiment.model <- lm_lin(how_often_daydream_num ~ experiment_holdout, 
                           covariates = ~ gender + age,
                           data = survey_data.df)
summary(experiment.model)

# Power Analysis
# https://www.statmethods.net/stats/power.html
install.packages('pwr')
library(pwr)
pwr.t.test(d = 0.1, sig.level = .05, power = 0.8, type = c("one.sample"))

# Factor Analysis
# https://towardsdatascience.com/exploratory-factor-analysis-in-r-e31b0015f224

survey_factors.df <- survey_data.df %>%
  select(pleasant_or_annoying_num,
         how_often_daydream_num,
         visualize_objects_easy_hard_num, 
         focus_attention_easy_hard_num,
         stucksong_pleasing_annoying_num,
         lastmonth_satisfaction_num,
         lastmonth_goodbad_felt_num)

library(psych)
scree(survey_factors.df, pc=FALSE)
fa.parallel(survey_factors.df, fa="fa")

factorfit <- factanal(na.omit(survey_factors.df), 2, rotation="promax")
print(factorfit, digits = 2, cutoff = 0.3, sort = TRUE)

# Principle Components
library(FactoMineR)
pca.results <- PCA(survey_factors.df)

# Basic Text Analysis
## Text Manipulation & Pattern Matching

# Stringr is the tidyverse package that deals with text anaylsis & search.
# https://stringr.tidyverse.org/articles/stringr.html

survey_data.df %>% 
  select(hearmusic_experience_openend) %>%
  na.omit() %>%
  print(., n = 20)  # Print the first 20 responses

survey_data.df %>% 
  select(hearmusic_experience_openend) %>%
  filter(str_detect(hearmusic_experience_openend, 'background noise')) %>%
  na.omit() %>%
  print(., n = 20)  # (Note: this time, it only returns less than 20.)

## Content Analysis & Inter-rater Reliability
# https://www.datanovia.com/en/lessons/inter-rater-reliability-analyses-quick-r-codes/
survey_data.df %>% 
  select(hearmusic_experience_openend) %>%
  na.omit() %>%
  mutate(bgnoise_yn = ifelse(str_detect(hearmusic_experience_openend, 'background noise'), 1, 0)) %>%
  mutate(radio_yn = ifelse(str_detect(hearmusic_experience_openend, 'radio'), 1, 0)) %>%
  mutate(song_yn = ifelse(str_detect(hearmusic_experience_openend, 'song'), 1, 0)) %>%
  mutate(headphone_yn = ifelse(str_detect(hearmusic_experience_openend, 'headphone'), 1, 0)) %>%
  select(bgnoise_yn, radio_yn, song_yn, headphone_yn) %>%
  pivot_longer(cols = bgnoise_yn:headphone_yn, names_to = 'variable', values_to = 'value') %>%
  group_by(variable, value) %>%
  summarise(n = n()) %>%
  filter(value == 1)

## Exploring Sentiment Analysis
library(tidytext)

survey_data.df.text <- survey_data.df %>% 
  mutate(stucksong_pleasing_annoying_buckets = ifelse(stucksong_pleasing_annoying_num >= 3, 'Positive/Neutral', 'Negative')) %>% 
  select(hearmusic_experience_openend, stucksong_pleasing_annoying_buckets) %>%
  na.omit() %>%
  mutate(id = seq.int(nrow(.))) %>%
  unnest_tokens(word, hearmusic_experience_openend) %>%
  anti_join(get_stopwords())

bing <- get_sentiments("bing")

bing_word_counts_joined <- survey_data.df.text %>%
  inner_join(bing) %>%
  group_by(stucksong_pleasing_annoying_buckets) %>%
  count(word, sentiment, sort = TRUE)

View(bing_word_counts_joined)

bing_word_counts_joined %>%
  filter(n > 5) %>%
  mutate(stucksong_pleasing_annoying_buckets = as.factor(stucksong_pleasing_annoying_buckets)) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>% 
  filter(word != 'like') %>%
  group_by(stucksong_pleasing_annoying_buckets) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, stucksong_pleasing_annoying_buckets)) %>%
  mutate(word = as.factor(word)) %>%
  ggplot(., aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  scale_fill_discrete(guide = 'none') + 
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~stucksong_pleasing_annoying_buckets, scales = 'free') +
  ylab("Sentiment Score") + xlab("Words") +
  ggtitle("Sentiment Text Analysis of Open-Ended Responses",
          subtitle = "Split by Experience: Annoying vs. Pleasing")
  

# ACTIVITY 4
## Complex analysis applied to survey dataset

# 1. Group people into age buckets (20s, 30s, 40s, etc.; or however you like: 
# e.g., quartiles - there are functions for this) and analyze differences in
# annoyance with music stuck in head ("stucksong_pleasing_annoying_num").

survey_data.df %>% select(age, stucksong_pleasing_annoying_num) %>%
  na.omit() %>% 
  mutate(age_buckets = case_when(age < 20 ~ '10s-',
                                 age < 30 & age >= 20 ~ '20s',
                                 age < 40 & age >= 30 ~ '30s',
                                 age < 50 & age >= 40 ~ '40s',
                                 age < 60 & age >= 50 ~ '50s',
                                 age >= 60 ~ '60s+')) %>%
  group_by(age_buckets) %>%
  summarise(mean = mean(stucksong_pleasing_annoying_num))

# 2. Repeat #1, but also add in gender differences (both gender on its own, as 
# well as gender X age).

survey_data.df %>% select(age, gender, stucksong_pleasing_annoying_num) %>%
  na.omit() %>% 
  mutate(age_buckets = case_when(age < 20 ~ '10s-',
                                 age < 30 & age >= 20 ~ '20s',
                                 age < 40 & age >= 30 ~ '30s',
                                 age < 50 & age >= 40 ~ '40s',
                                 age < 60 & age >= 50 ~ '50s',
                                 age >= 60 ~ '60s+')) %>%
  group_by(gender, age_buckets) %>%
  summarise(mean = mean(stucksong_pleasing_annoying_num))

# 3. What is the common year that people are born? What's the second most common?
# What about the earliest and most recent years?

survey_data.df %>%
  select(year_born) %>%
  group_by(year_born) %>%
  summarise(n = n()) %>%
  arrange(-n)

survey_data.df %>%
  select(year_born) %>%
  na.omit() %>%
  summarise(max(year_born), min(year_born))

survey_data.df %>%
  select(year_born) %>%
  na.omit() %>%
  summarise(min(year_born))

# 4. Is there a relationship between if someone is religious and 
# if they hear music frequently?

summary(lm(data = survey_data.df, how_frequently_hear_music_num ~ religious_yn))

# 5. Challenge: There is a matrix question ("grid_*" column names) looking at how
# often people do certain musical behaviors. Which is the most common on average?
# And are there differences between people who do or don't have hobbies related to music?
# Bonus: How do these behaviors relate to each other (e.g., do they correlate)?

survey_data.df %>%
  select(grid_how_often_whistle_num:grid_how_often_tap_num) %>%
  na.omit() %>%
  pivot_longer(cols = grid_how_often_whistle_num:grid_how_often_tap_num, names_to='variable',
               values_to='value') %>%
  group_by(variable) %>%
  summarise(mean = mean(value))

# 6. Extra challenge: what is the most common word in the open-ended responses about
# people's musical experiences?

survey_data.df.text %>%
  select(word) %>%
  group_by(word) %>%
  summarise(n = n()) %>%
  arrange(-n)
