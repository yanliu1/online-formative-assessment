## This R file is to show the data visualization in Figures 3 and 4.
## The data set is used from the one generated from DataViz_Data_cleaning_May12_2019.R.

# source("DataViz_Data_cleaning_June2019.R")
# dir.create("output")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(magrittr)
#~~~~~~~~~~~~~~~~~~~~~~~##
# Response Order Plot   ##
#~~~~~~~~~~~~~~~~~~~~~~~##
# This data set is generated from the source code "DataViz_Data_cleaning_June2019.R"
data_Q4 <- read.csv("data_Q4.csv") 

## Response orders with all participants in one plot
ggplot(data=data_Q4 %>% mutate(itemtranslate = item-2),
       aes(x=jitter(order, factor=0.5), y=jitter(itemtranslate, factor=0.5), group=X, color=pass)) +
  geom_point() +
  geom_line() +
  labs(x="The Order of Items Attempted", y="The Actual Item Number") +
  scale_color_manual(name='', 
                     values=c("red","blue"),
                     labels=c("Student who failed the quiz", "Student who passed the quiz")) +
  theme(legend.position = "bottom",
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    legend.title=element_text(size=13), 
    legend.text=element_text(size=12)) 

## multiple small plot: response orders of each individual per small plot 
ggplot(data=data_Q4 %>% 
  mutate(itemtranslate = item-2), aes(x=order, y=itemtranslate, group=X, color=pass)) +
  geom_point() +
  geom_line() +
  labs(x="The Order of Items Attempted", y="The Actual Item Number") +
  scale_color_manual(name='', 
                     values=c("red","blue"),
                     labels=c("Student who failed the quiz", "Student who passed the quiz")) +
  facet_wrap(~X) +
  theme(legend.position = "bottom",
        strip.background = element_blank(), # optional to remove the elements in the background
        strip.text.x = element_blank(), # optional to remove the text 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        legend.title=element_text(size=13), 
        legend.text=element_text(size=12)) 

