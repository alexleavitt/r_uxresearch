
### Welcome to Intro to R!

# Agenda for Day 1

# Navigating RStudio

# R Syntax
# Built-in Functions
# Data Types
# Variables
# Functions & Arguments ("Inputs")

# Vectors
# Manipulating Vectors
# Functions on Vectors
# Data Types on Vectors

# Data Frames

# How to get help
# Inside RStudio
# Outside RStudio

# ACTIVITY 1

# Packages
# Installing Packages
# Tidyverse
# Tidyverse Functions
## select, filter, mutate, group_by, arrange, join, etc.
# Tidyverse Pipes

# ACTIVITY 2

# Importing Data
# Exploratory Data Analysis
## (We'll return to this more in depth in Day 2.)

# ==========================================================

# Syntax

"This is a line of code." #end of a line of code
# This is a comment

# Numbers
2
10.7
2 + 4
2 + 10.7
2**3
10%%3

# What do we see?
# 1. The results that we expect
# 2. "[1]": This designates the "index" or position of the value. 

# Writing out code

1 + 2
3 + 2
1 + 2; 3 + 2

2 * 15
2*15

2 * 15; 3
# Oops, what happened?

2 *
# Hm...

# Other types of data?

3  

"word"
'word'
"word_of_the_day"

TRUE
FALSE
1 > 2
2 < 3
"Alex" == "Alex"
"Alex" != "Alex"
"Alex" == "Sarah"

2023-02-14
"2023-02-14"
as.Date("2023-02-14")

typeof()
class()

# character [words]
# numeric
# logical
# double
# integer

typeof(2023-02-14)
typeof("2023-02-14")
typeof(as.Date("2023-02-14"))

class(2023-02-14)
class("2023-02-14")
class(as.Date("2023-02-14"))

# More info on this strange issue here:
# https://stackoverflow.com/a/39458990/1321181

# Built-in Functions

sqrt(49)
log(100)
log (10 + 90)
sqrt(log(100))
max(2)
max(10)
max(2, 4, 10)
length("King George the IV")
nchar("King George the IV")
str_length("King George the IV")

# Variables

a <- 4
b <- 7

a + b
b - a
a ** b

# Where are these going? -->

test_c <- 20
test_c
rm(test_c)
test_c

c <- 20
a
b
c

c - 10
c

c <- c - 10
c

d <- 4
e <- 5
f <- 6
d; e; f

d <- f
d; e; f
f <- 10
d; e; f

my_var <- 20
my_var
call_it_whatever.YOU.want <- 100
call_it_whatever.YOU.want
just.be_verycareful <- 1000

# Functions & Arguments ("Inputs")
rep(3, 7)
# this is the rep function; the first argument is the number to be repeated
# and the second is the number of times it will repeat
rep(x = 3, times = 7)
rep(times = 7, x = 3)
rep(times = 7, 3)
rep(7, x = 3)
rep(40, 1000)

# Vectors
1:12
c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
a:f
# Wait, what? Let's revisit what we did earlier!

# Use the "c" function (concatenate) to collect values into a vector
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# Use the repeat function to generate a vector of a repeated value
y <- rep(1, 10)

# Use the ":" operator to generate a vector of numbers in a range
z <- 10:20

# Starting to group together data:
abc <- c(~colA, ~colB,
         "x",   1,
         "y",   2,
         "z",   3
)
abc

library(tidyverse)
xyz <- tribble(~colA, ~colB, ~ colC,
                    "x",   1,     x,
                    "y",   2,     y,
                    "z",   3,     z
)
xyz

# Manipulating Vectors
xyz[1]
xyz[2]
xyz[3]
xyz[3, 1]
xyz[1, 3]
xyz[3, ]
xyz[, 3]

xyz$colA
xyz$colA[1]
xyz$colC
xyz$colC[1]

# Functions on Vectors
max(4)
max(1:100)

a <- 1:12
# summary functions
mean(a)
median(a)
# itemized functions
log(a)
sqrt(a)

x <- 1:5
z <- 2:6
x + z
x * z

# But...
x <- 1:5
z <- 1:7
x + z

# Data Types on Vectors

my_name <- "Brian"
my_team <- c("Brian", "Justin", "Ryan")
my_team

our_test_scores <- c(89, 91, 97)
our_test_scores

did_it_hit_the_target <- c(FALSE, FALSE, TRUE, FALSE)
did_it_hit_the_target

soccer_jersey_numbers <- c(1, 5, 17, 20, 31)
soccer_jersey_numbers
soccer_jersey_numbers <- as.character(soccer_jersey_numbers)
soccer_jersey_numbers
soccer_jersey_numbers[1]
soccer_jersey_numbers[5]

soccer_teammates <- c('Mary', 'Takeshi', 'Rohit', 'Farah', 'Carlos')
soccer_teammates
as.numeric(soccer_teammates)
soccer_teammates[2]
soccer_teammates[e]
soccer_teammates[5]
# Wait, what?? Again, let's revisit what we did earlier...!

# Data Frames
# First, some data:
trees
?trees

# How to get help
## Inside RStudio
?trees

# Outside RStudio
## Google: "r trees dataset"
## --> https://r-data.pmagunia.com/dataset/r-dataset-package-datasets-trees

# What do each of these do?
head(trees)
tail(trees)
str(trees)
summary(trees)

colnames(trees)
rownames(trees) #usually these don't exist; sometimes they do
dimnames(trees)

nrow(trees)
ncol(trees)
dim(trees)

View(trees)

# ACTIVITY 1

# 0. Create a new R script, and complete the following exercises in that script.
# 1. What is the answer of the math equation where you add 7 and 8, multiply by 3, and subtract 2?
7 + 8 * 3 - 2
(7 + 8) * 3 - 2
(7 + 8) * (3 - 2)

# 2. Write down the ages of up to 10 family members you know of, and put them into a vector.
## Then find the average age of your family members.
family_ages <- c(43, 17, 56, 98, 2, 35, 33, 7)
mean(family_ages)

# 3. How many letters are in "supercalifragilisticexpialidocious"? Can you select the 'x'?
x <- "supercalifragilisticexpialidocious"
x
nchar(x)

x[22] # huh???

library(stringr)
str_locate_all(pattern = "x", "supercalifragilisticexpialidocious")

# 4. Load the built-in dataset "ChickWeight" (the weight of baby chickens over time). 
ChickWeight
## First, how many rows and columns are there? What are the names of each column?
nrow(ChickWeight)
ncol(ChickWeight)
dim(ChickWeight)
colnames(ChickWeight)
## What are the variable types of each column? 
str(ChickWeight)
## How many types of diets are there? 
unique(ChickWeight$diet)
## What is the minimum and maximum weight?
min(ChickWeight$Weight)
max(ChickWeight$Weight)

# Packages

library(tidyverse)

# Examples:
# - caret is a great package for a wide variety of machine learning applications.
# - survey is a great package for weighting survey data (e.g., post stratification, raking).
# - boot is a great package for bootstrapping.
# - lavaan is a great package for structural equation modeling.
# - lme4 is a great package for multi-level modeling.

# Installing Packages

# Easily manipulate date and time data
install.packages('lubridate')
library(lubridate)
# Quickly summarize a dataset
install.packages('skimr')
library(skimr)

# Tidyverse

## https://tidyverse.tidyverse.org/
# ggplot2, for data visualization.
# dplyr, for data manipulation.
# tidyr, for data tidying.
# readr, for data import.
# purrr, for functional programming.
# tibble, for tibbles, a modern re-imagining of data frames.
# stringr, for strings.
# forcats, for factors.

# Tidyverse Functions

## select, filter, mutate, group_by, arrange, join, etc.

chicks.df <- ChickWeight
summary(chicks.df)

# Tidyverse Pipes

chicks.df %>% 
  select(weight, Time) #function to select

chicks.df %>% 
  select(weight, Time) %>% 
  filter(Time > 10) #function to sub-select

chicks.df %>% 
  select(weight) %>% 
  filter(weight > 300) %>%
  View() # We can tack this on (always capital-V) to get a more "Excel"-like viewpoint

chicks.df %>% 
  select(weight) %>% 
  filter(weight > 300) %>%
  summarise(n = n(),
            mean = mean(weight)) #function to generate some key stats

chicks.df %>% 
  select(weight) %>% 
  filter(weight > 300) %>%
  mutate(extra_weight = weight + 100) %>% #function to generate custom stats
  View()

# ACTIVITY 2

# 0. In another new R script...
# 1. Load the 'mtcars' data (it has information about cars and their specs), and look through the data frame structure.
# 2. Rename the first column "car_models" (currently it is unnamed).
# 3. Select car_models, mpg, cyl, and gear columns.
# 4. Filter the data to retain only the 4 cylinder records.
# 5. Create a new column that calculates MPG per cylinder.
# 6. If you haven't already, combine all of the above into a single tidyverse "pipeline."
# 7. Challenge: starting from the base mtcars dataset, calculate mean MPG per cylinder group. (Hint: You'll need to use group_by and summarise).

# Tomorrow: 
## We'll talk about importing data and some exploratory data analysis techniques.
