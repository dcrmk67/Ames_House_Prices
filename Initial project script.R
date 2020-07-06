
# Code to get started with the Ames House Prices competition on Kaggle

library(tidyverse)
library(data.table)
library(lubridate)
library(caret)

train <- read_csv("./train.csv", col_names=TRUE) 
names(train) <- tolower(names(train))

test <- read_csv("./test.csv", col_names=TRUE)
names(test) <- tolower(names(test))
