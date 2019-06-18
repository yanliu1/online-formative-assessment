## This R file is to demonstrate how to clean and prepare data for data visualization
## We have three sets of data: responses (correctness.csv), response orders (orders.csv)
## and response times (timesToAnswerSec.csv).

library("tibble")
library(dplyr)
library(tidyverse)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Import 3 sets of data:                  ##
## correctness, orders and response times  ##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
answers <- read.csv("correctness.csv") %>% as.tibble
answers<-answers
str(answers)

orders <- read.csv("orders.csv")
orders <- orders
View(orders)

times <- read.csv("timesToAnswerSec.csv")
times<- times
str(times)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## transform data from wide to long format ##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## correctness
correct.long <- answers %>%
  gather(-X, -id, key="quizitem", value="correct") %>%
  mutate(quiz = map_chr(quizitem, function(x){strsplit(x,"q")[[1]][1] %>% as.character}),
         item = map_int(quizitem, function(x){strsplit(x,"q")[[1]][2] %>% as.integer}))
str(correct.long)

## response orders
orders.long <- orders %>% 
  gather(-id, key="quizorder", value="item") %>%
  mutate(quiz = map_chr(quizorder, function(x){strsplit(x,"_")[[1]][1] %>% as.character}),
         order = map(quizorder, function(x)strsplit(x,"_")[[1]][2])) %>%
  mutate(order = map_int(order, function(x){strsplit(x,"th")[[1]][1] %>% as.integer}))
View(orders.long)

## response time in seconds 
times.long <- times %>%
  gather(-id, key="quizitem", value="timeinsec") %>%
  mutate(timeinsec = as.integer(timeinsec))
View(times.long)

## merg all the data together
data_all <- correct.long %>%
  full_join(times.long, by=c("id", "quizitem")) %>%
  full_join(orders.long, by=c("id", "quiz", "item"))
View(data_all)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Data cleaning & creating variables ##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## assign "0" to missing values 
data_all<-data_all %>%
  mutate(timeinsecimpute = ifelse(is.na(timeinsec) & order==1, 0, timeinsec))
View(data_all)

## check if any missing values are omitted
data_all %>% filter(is.na(timeinsecimpute))

## Select Quiz #4 from a large data pool
## Need to remove the frist two and last two items b.c. they are survey questions 
data_Q4 <- data_all %>%
  filter(quiz=="Q4", item %in% 3:16) %>% 
  group_by(id) %>%
  mutate(order = rank(order)) %>% #need to recalculate orders because 2 survey questions are removed 
  ungroup()

## Creat cumulative response times and cumulative scores and 
## Compute the passing criterion, at least getting half of the items correct
data_Q4 %<>%
  arrange(X, order) %>%  # X is another subject id
  group_by(X) %>% 
  mutate(cumtime = cumsum(timeinsecimpute),
         cumscore = cumsum(correct),
         n_items = n(),
         gt50pct = floor(0.5*n_items),
         totmark = max(cumscore),
         tottime = max(cumtime),
         pass=(totmark>=gt50pct)) %>%
  ungroup() 
  

