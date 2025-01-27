
### Welcome to Intro to R!

# Agenda for Day 3

# Revisiting Exercises from Day 2

# Revisiting the Dataset

# Additional Stats Analyses
# Text Analysis

# Setting Up for Data Visualization
## ggplot2

# Revisiting the Dataset

# Basic Visualization
## Piping for GGPlot

# Geoms
## Constants vs. Variables
## Geom Functions/Chart Types
## Multiple Geoms
## Facets

# Secondary Adjustments
## Scales
## Labeling, Titles, etc.

# Calculating "Data Tables" Before Visualization
## Summarizing Final Data

# Position Adjustments
## Calculating Visualization-Specific Metadata

# Saving the Results
## Manual Processes
## Exporting Figures

# ACTIVITY 5

# Using R Notebooks

# Conclusion

# ==========================================================

# Reloading Packages: Reminder of this best practice!

library(tidyverse)
library(ggplot2)

# Revisiting Exercises from Day 2

# Revisiting the Dataset

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

str(survey_data.df)

# Additional Stats Analyses
# Text Analysis

# Setting Up for Data Visualization
## ggplot2

# Revisiting the Dataset

str(survey_data.df)

# Basic Visualization

# Visualizations in ggplot2 follow a basic pattern:
# You start with a data frame. Then create a blank "canvas" on which you'll plot items.
# You can add various layers on top of your canvas "in order," like how you layer a painting.
# You can then modify those layers to look however you want.

# Example of a cheat sheet: https://github.com/rstudio/cheatsheets/blob/main/data-visualization.pdf

# The best practice is to *CREATE YOUR FINAL DATA TABLE FIRST.* While you CAN
# technically do operations inside of ggplot2 code, it's best to know what you're pulling in,
# rather than hoping the code does what you expect. Then you can always look back
# at your data table to make sure your plot is showing the correct things.

## Piping for GGPlot

survey_data.df %>% 
  select(inner_voice_yn) %>%
  na.omit() %>%
  group_by(inner_voice_yn) %>% 
  summarize(n = n()) %>%
  ggplot(., aes(x = inner_voice_yn, y = n)) + 
  geom_col()

# We can break this tidyverse pipeline into two sections: the analysis, and the viz.

survey_data.df %>% 
  
  # this is the analysis portion
  select(inner_voice_yn) %>%
  na.omit() %>%
  group_by(inner_voice_yn) %>% 
  summarize(n = n()) %>%
  
  # this is the visualization portion
  ggplot(temp_plot.df, aes(x = inner_voice_yn, y = n)) + 
  geom_col()

# You can write one pipeline that includes both!

# Geoms
# What happens if we just run this?
survey_data.df %>% 
  select(inner_voice_yn) %>%
  na.omit() %>%
  group_by(inner_voice_yn) %>% 
  summarize(n = n()) %>%
  ggplot(.)

# Aha, it returns that blank canvas!

# What about if we add in some variables?
survey_data.df %>% 
  select(inner_voice_yn) %>%
  na.omit() %>%
  group_by(inner_voice_yn) %>% 
  summarize(n = n()) %>%
  ggplot(., aes(x = inner_voice_yn, y = n))

# Ooh, now it's even closer!
# Adding in the key part: aes() is short for aesthetics, and it's where you include
# dimensions to vary the data by (ie., variables for x, y, grouping, color, shapes, etc.)

# Finally, adding in a geom type: 

survey_data.df %>% 
  select(inner_voice_yn) %>%
  na.omit() %>%
  group_by(inner_voice_yn) %>% 
  summarize(n = n()) %>%
  ggplot(., aes(x = inner_voice_yn, y = n)) + 
  geom_col()

## Constants vs. Variables

# Variable Types

# Let's show what happens when you use numeric variables, rather than factors:

survey_data.df %>%
  select(inner_voice_yn_num) %>%
  na.omit() %>%
  group_by(inner_voice_yn_num) %>% 
  summarize(n = n()) %>%
  ggplot(., aes(x = inner_voice_yn_num, y = n)) + 
  geom_col()

# Look at the bottom x-axis: it's not showing our "categorical" variable anymore.

# Also, beyond the type of data point, we can change constants vs. variables inside
# the ggplot function to manipulate colors, etc. manually or dynamically:

survey_data.df %>% 
  select(inner_voice_yn, grid_how_often_sing) %>%
  na.omit() %>%
  group_by(inner_voice_yn, grid_how_often_sing) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = inner_voice_yn, y = n, fill = grid_how_often_sing)) + 
  geom_col()

survey_data.df %>% 
  select(inner_voice_yn, grid_how_often_sing) %>%
  na.omit() %>%
  group_by(inner_voice_yn, grid_how_often_sing) %>% 
  summarize(n = n()) %>%
  ggplot(aes(x = inner_voice_yn, y = n, fill = grid_how_often_sing)) + 
  geom_col() +
  scale_fill_manual(values = c('#cccccc', '#aaaaaa', '#222222'))

survey_data.df %>% 
  select(inner_voice_yn, grid_how_often_sing) %>%
  na.omit() %>%
  group_by(inner_voice_yn, grid_how_often_sing) %>% 
  summarize(n = n()) %>%
  ggplot(aes(x = inner_voice_yn, y = n, fill = grid_how_often_sing)) + 
  geom_col() + 
  scale_fill_brewer(palette = "Greens")

survey_data.df %>% 
  select(inner_voice_yn, grid_how_often_sing) %>%
  na.omit() %>%
  group_by(inner_voice_yn, grid_how_often_sing) %>% 
  summarize(n = n()) %>%
  ggplot(aes(x = inner_voice_yn, y = n, fill = grid_how_often_sing)) + 
  geom_col(position = 'dodge') + 
  scale_fill_manual(values = c('#cccccc', '#aaaaaa', '#222222'))

## Geom Functions/Chart Types

survey_data.df %>% 
  select(how_frequently_hear_music_num, age) %>%
  na.omit() %>%
  ggplot(aes(x = age, y = how_frequently_hear_music_num)) + 
  #geom_point()
  #geom_line()
  #geom_col()
  #geom_tile()
  #geom_bar()
  geom_smooth()

## Multiple Geoms

# And you can even combine some: try POINT and SMOOTH

survey_data.df %>% 
  select(how_frequently_hear_music_num, age) %>%
  na.omit() %>%
  ggplot(aes(x = age, y = how_frequently_hear_music_num)) + 
  geom_point() +
  geom_smooth()

## Facets

survey_data.df %>% 
  select(how_frequently_hear_music_num, age, country_usa_yn) %>%
  na.omit() %>%
  ggplot(aes(x = age, y = how_frequently_hear_music_num)) + 
  #geom_point() +
  geom_smooth() +
  facet_wrap(~ country_usa_yn)

# Secondary Adjustments
## Scales & Labels

# Lots of different scales: https://ggplot2.tidyverse.org/reference/index.html#scales

survey_data.df %>% 
  select(inner_voice_yn, grid_how_often_sing) %>%
  na.omit() %>%
  group_by(inner_voice_yn, grid_how_often_sing) %>% 
  summarize(n = n()) %>%
  ggplot(., aes(x = inner_voice_yn, y = n, fill = grid_how_often_sing)) + 
  geom_col(position = 'dodge') + 
  scale_fill_manual(values = c('#cccccc', '#aaaaaa', '#222222'),
                    name = "How Often Sing") +
  xlab("Have an Inner Voice") +
  ylab("Total Count") +
  ggtitle("Total Count of How Many People \nHave an Inner Voice") +
  scale_y_log10()

# Calculating "Data Tables" Before Visualization
## Summarizing Final Data
# Position Adjustments
## Calculating Visualization-Specific Metadata

survey_data.df %>% 
  select(inner_voice_yn, grid_how_often_sing) %>%
  na.omit() %>%
  group_by(inner_voice_yn, grid_how_often_sing) %>% 
  summarize(n = n()) %>% 
  # let's add in some "position" data to make before graphing!
  mutate(n_pos = n + 30) %>% 
  ggplot(aes(x = inner_voice_yn, y = n, fill = grid_how_often_sing,
             label = n)) + 
  geom_col(position = 'dodge') + 
  geom_text(aes(y = n_pos), position = position_dodge(1)) +
  scale_fill_manual(values = c('#cccccc', '#aaaaaa', '#222222'),
                    name = "How Often Sing") +
  xlab("Have an Inner Voice") +
  ylab("Total Count") +
  ggtitle("Total Count of How Many People \nHave an Inner Voice")

library(srvyr)
survey_data.df %>%
  select(how_frequently_hear_music_num, political_pov) %>%
  na.omit() %>%
  as_survey_design() %>%
  group_by(political_pov) %>%
  summarize(frequency_mean = survey_mean(how_frequently_hear_music_num, 
                                         vartype = 'ci', level = c(0.95))) %>%
  ggplot(., aes(x = political_pov, y = frequency_mean, 
                color = political_pov, 
                #fill = political_pov
                )) + 
  geom_point(position = position_dodge()) + 
  #geom_bar(stat='identity', position = position_dodge()) + 
  scale_color_manual(values = c('grey', 'darkred', 'red', 'purple', 'blue', 'darkblue'),
                     guide = 'none') +
  #scale_fill_manual(values = c('grey', 'darkred', 'red', 'purple', 'blue', 'darkblue'),
  #                    guide = 'none') +
  geom_errorbar(aes(ymin = frequency_mean_low, ymax = frequency_mean_upp), 
                width = .2, position = position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab('Mean Frequency Hearing Music') + xlab('Political Orientation') +
  ggtitle("Frequency of Hearing Music", subtitle="Per Political Background")

# Saving the Results
## Manual Processes

# See the "plot" window

## Exporting Figures

# To save, "ggsave" is your friend!

survey_data.df %>%
  select(how_frequently_hear_music_num, political_pov) %>%
  na.omit() %>%
  as_survey_design() %>%
  group_by(political_pov) %>%
  summarize(frequency_mean = survey_mean(how_frequently_hear_music_num, 
                                         vartype = 'ci', level = c(0.95))) %>%
  ggplot(., aes(x = political_pov, y = frequency_mean, 
                #color = political_pov, 
                fill = political_pov
  )) + 
  #geom_point(position = position_dodge()) + 
  geom_bar(stat='identity', position = position_dodge()) + 
  scale_color_manual(values = c('grey', 'darkred', 'red', 'purple', 'blue', 'darkblue'),
                     guide = 'none') +
  scale_fill_manual(values = c('grey', 'darkred', 'red', 'purple', 'blue', 'darkblue'),
                    guide = 'none') +
  geom_errorbar(aes(ymin = frequency_mean_low, ymax = frequency_mean_upp), 
                width = .2, position = position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab('Mean Frequency Hearing Music') + xlab('Political Orientation') +
  ggtitle("Frequency of Hearing Music", subtitle="Per Political Background")

# note: you don't need to save the pipeline into any variable
# you just plot the "most recent" plot
ggsave("politicalmusic.png", width = 6, height = 4)


# ACTIVITY 5

# 1. Create a simple visualization to explore the relation between
# how often people hum and if they have hobbies related to music.

survey_data.df %>%
  select(grid_how_often_hum_num, hobbies_related_music_yn) %>%
  na.omit() %>%
  group_by(hobbies_related_music_yn) %>%
  summarise(mean = mean(grid_how_often_hum_num)) %>%
  ggplot(., aes(x = hobbies_related_music_yn, y = mean)) + 
  geom_col(position = 'dodge')

# 2. Create a variable that buckets age (20s, 30s, etc.) and plot the mean
# experience of pleasantness or annoyingness of getting music stuck in your head.

survey_data.df %>%
  select(age, how_frequently_hear_music_num) %>%
  na.omit() %>%
  mutate(age_buckets = case_when(age < 20 ~ '10s-',
                                 age < 30 & age >= 20 ~ '20s',
                                 age < 40 & age >= 30 ~ '30s',
                                 age < 50 & age >= 40 ~ '40s',
                                 age < 60 & age >= 50 ~ '50s',
                                 age >= 60 ~ '60s+')) %>%
  as_survey_design() %>%
  group_by(age_buckets) %>%
  summarize(mean = survey_mean(how_frequently_hear_music_num, 
                                         vartype = 'ci', level = c(0.95))) %>%
  ggplot(., aes(x = age_buckets, y = mean)) + 
  geom_point()

# 3. Extend #2 by plotting it with confidence intervals.

survey_data.df %>%
  select(age, how_frequently_hear_music_num) %>%
  na.omit() %>%
  mutate(age_buckets = case_when(age < 20 ~ '10s-',
                                 age < 30 & age >= 20 ~ '20s',
                                 age < 40 & age >= 30 ~ '30s',
                                 age < 50 & age >= 40 ~ '40s',
                                 age < 60 & age >= 50 ~ '50s',
                                 age >= 60 ~ '60s+')) %>%
  as_survey_design() %>%
  group_by(age_buckets) %>%
  summarize(mean = survey_mean(how_frequently_hear_music_num, 
                               vartype = 'ci', level = c(0.95))) %>%
  ggplot(., aes(x = age_buckets, y = mean)) + 
  geom_point() +
  geom_errorbar(aes(ymin = mean_low, ymax = mean_upp))

# 4. Now facet it based on country.

survey_data.df %>%
  select(age, how_frequently_hear_music_num, country_usa_yn) %>%
  na.omit() %>%
  mutate(age_buckets = case_when(age < 20 ~ '10s-',
                                 age < 30 & age >= 20 ~ '20s',
                                 age < 40 & age >= 30 ~ '30s',
                                 age < 50 & age >= 40 ~ '40s',
                                 age < 60 & age >= 50 ~ '50s',
                                 age >= 60 ~ '60s+')) %>%
  as_survey_design() %>%
  group_by(country_usa_yn, age_buckets) %>%
  summarize(mean = survey_mean(how_frequently_hear_music_num, 
                               vartype = 'ci', level = c(0.95))) %>%
  ggplot(., aes(x = age_buckets, y = mean)) + 
  geom_point() +
  geom_errorbar(aes(ymin = mean_low, ymax = mean_upp)) +
  facet_wrap(~ country_usa_yn)

# 5. Plot the gender differences in well-being measures (satisfaction and good/bad feelings).

survey_data.df %>%
  select(gender, lastmonth_satisfaction_num, lastmonth_goodbad_felt_num) %>%
  na.omit() %>%
  pivot_longer(cols = c(lastmonth_satisfaction_num, lastmonth_goodbad_felt_num), 
               names_to = 'variable', values_to = 'value') %>%
  as_survey_design() %>%
  group_by(gender, variable) %>%
  summarize(mean = survey_mean(value, 
                               vartype = 'ci', level = c(0.95))) %>%
  ggplot(., aes(x = gender, y = mean, color = gender)) + 
  geom_point() +
  geom_errorbar(aes(ymin = mean_low, ymax = mean_upp)) +
  facet_wrap(~variable, ncol = 1)

# 6. Challenge: see if you can use pivoting to plot all 5 of the "how often" grid 
# variables: plot their means, with confidence intervals, and facet by religiosity.

survey_data.df %>%
  select(religious_yn, grid_how_often_whistle_num:grid_how_often_tap_num) %>%
  na.omit() %>%
  pivot_longer(cols = grid_how_often_whistle_num:grid_how_often_tap_num, 
               names_to = 'variable', values_to = 'value') %>%
  as_survey_design() %>%
  group_by(variable, religious_yn) %>%
  summarize(mean = survey_mean(value, 
                               vartype = 'ci', level = c(0.95))) %>%
  mutate(variable = fct_recode(as.factor(variable),
                               "How Often Dance" = "grid_how_often_dance_num",
                               "How Often Hum" = "grid_how_often_hum_num",
                               "How Often Sing" = "grid_how_often_sing_num",
                               "How Often Tap" = "grid_how_often_tap_num",
                               "How Often Whistle" = "grid_how_often_whistle_num")) %>%
  ggplot(., aes(x = religious_yn, y = mean, color = variable)) + 
  geom_point() +
  geom_errorbar(aes(ymin = mean_low, ymax = mean_upp)) +
  scale_color_discrete(guide = 'none') +
  facet_wrap(~variable, ncol = 1) +
  xlab('Religious or Not') + ylab('Mean') +
  coord_flip() +
  ggtitle("How Often Musical Activities", subtitle = "By Religious or Not")


# Using R Notebooks
# We don't have enough time to cover R Notebooks in this class, but they can be
# powerful tools for writing up reports at work, especially sharing the analysis,
# visualizations, AND code with fellow colleagues.
# Read https://bookdown.org/yihui/rmarkdown/notebook.html for more information



# Conclusion

## Thank you SO MUCH for joining this class. I really appreciate your support.
## Please fill out the course survey: it helps make the class significant better
## for future students and improves my teaching style as well.
## Also, if you liked the class: please consider referring friends or colleagues!
## Finally, as you find yourself integrating these skills into your, let me know!!
## Feel free to email me anytime to update me on your progress, and please
## consider writing a testimonial for the course page if it was impactful on your work.



Things to add:
  - Outliers
  - Proportions for graphing (e.g., generating percents)

