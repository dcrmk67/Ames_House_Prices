
# Code to get started with the Ames House Prices competition on Kaggle

library(tidyverse)
library(data.table)
library(lubridate)
library(caret)

# Read csv files into global environment
train <- read_csv("./train.csv", col_names=TRUE) %>% 
  mutate_if(is.character, as.factor)
names(train) <- tolower(names(train))

test <- read_csv("./test.csv", col_names=TRUE)
names(test) <- tolower(names(test))

summary(train)