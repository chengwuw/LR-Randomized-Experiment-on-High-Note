
############################################### High Note ###############################################
# Chengwu Weng
# 02/25/2021
## https://rstudio-pubs-static.s3.amazonaws.com/594181_27503022bbc040dba00a68bd0ccd36f4.html
## https://rpubs.com/sebastk1/hnpsm
## https://github.com/chavisingal/High-Note-Case-Study
## https://nijanthanand.github.io/analytics/projects/proj-2.html

# load packages needed
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(psych)
library(MatchIt) 

# set working direction and load data
data <- read.csv("HighNote Data Midterm.csv", header = T)
head(data)

# remove the column: ID
data <- data %>%
  select( c(2:16) ) 

#################################################### Q1 ###################################################
# Summary statistics

# set groups of adopter and non-adopter
is_adopter <- data %>%
  filter(adopter == 1) %>%
  select( c(1:12,14:15) ) 

not_adopter <- data %>%
  filter(adopter == 0) %>%
  select( c(1:12,14:15) ) 

summary(is_adopter)
summary(not_adopter)

# t-test in the both groups of adopter and non-adopter
colname0 <- names(is_adopter)
lapply(colname0, function(i) {
  t.test(is_adopter[, i] , not_adopter[, i])
})

# make descriptive tables of both group
summary_is_adopter <- psych::describe(is_adopter) #skew=see skewness to take log
summary_is_adopter <- round(summary_is_adopter,3)
summary_not_adopter <- psych::describe(not_adopter)
summary_not_adopter <- round(summary_not_adopter,3)

htmlTable::htmlTable(summary_is_adopter)
htmlTable::htmlTable(summary_not_adopter)

