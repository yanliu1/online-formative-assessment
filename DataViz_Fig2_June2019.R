## This R file is to show the data visualization in Figure 2.
## For this figure, we only need data from
## responses (correctness.csv) and response times (timesToAnswerSec.csv).

dat.Q4 <- read.csv("dat4.csv") # This data set is in wide format.

# Compute item easiness and the median of item response times for 14 items
easiness <-  apply(dat.Q4[,6:19], 2, mean)
time.medn <- apply(dat.Q4[,24:37]/60,2, median)

# create cognitive index for 14 items
cog<-c(1,0,0,1,1,2,2,2,1,1,1,1,0,2)

# combine easiness, median of item response times, and cognitive index in one file
mydata <-data.frame(easiness,time.medn,cog)

# make sure that the cognitive index is a factor
mydata$cog <- as.factor(mydata$cog)
levels(mydata$cog) <- list(recall = '0', understand ='1', apply='2')

# create item#id
mydata$id <- paste("q",1:14,sep = "")

# Figure 2: plot the relationships of item easiness, item response times and cognitive levels of 14 items
library(ggplot2)
ggplot(mydata, aes(x=time.medn, y=easiness, shape=cog, color=cog)) +
  geom_point(size=2)+ geom_text(aes(label=id),hjust=-0.3, vjust=0) +
  scale_x_continuous(expand = c(0.2, 0.2)) +
   labs(x = "Median of Response Times") +
   labs(y = "Item Easiness") +
   labs(shape = "Cognitive Level", color = "Cognitive Level") +
   theme(
     axis.title.y = element_text(size = 14),
     axis.title.x = element_text(size = 14),
     axis.text.y = element_text(size = 11),
     axis.text.x = element_text(size = 11),
     legend.title=element_text(size=13), 
     legend.text=element_text(size=12)) 

  