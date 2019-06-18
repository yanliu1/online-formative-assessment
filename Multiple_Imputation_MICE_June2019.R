## This R file demonstrates how to conduct multiple imputation
## We merged three data sets, then conducted missing impuation, and also restructured the data to long format.

library(mice)
library(data.table)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get correctness, survey questions & total scores from other quizzes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

correct <- read.csv("correctness.csv") 
response <- read.csv("responses_2014.csv") 

name1<-c("id", paste0("Q",4,"q",c(1:2,18)))  # survey q1, q2 & q4
name2<-paste0("Q",4,"q",3:16) # Q4 has 14 questions q1-q14

correct1 <- correct[, name2] 
correct1[] <- lapply(correct1, function(x) as.factor(as.character(x)))

str(correct1)
correct1$id <- correct$id
# data set-3 three survey questions
response1 <-response[, name1] # need to rescale survey q1

##Create dummy variables for the 1st survey question
table(response1$Q4q1)

Q4q1 <- as.character(response1$Q4q1)
Q4q1r<-rep(3,length(response1$Q4q1))
Q4q1r[is.na(response1$Q4q1)]<-NA
Q4q1r[response1$Q4q1=="0"]<-0  # 0 =  attended lectures only
Q4q1r[response1$Q4q1=="0,1"]<-1  #1 =  attended and/or watched and/or reviewed lectures
Q4q1r[response1$Q4q1=="1,2"]<-1
Q4q1r[response1$Q4q1=="0,1,2"]<-1
Q4q1r[response1$Q4q1=="0,1,5"|response1$Q4q1=="1,2,5"|response1$Q4q1=="0,1,2,5"|response1$Q4q1=="0,2,5"]<-2

response1$Q4q1 <- Q4q1r

# merge data
dat1<- merge(response1,correct1, by="id")
str(dat1)

# get total scores from Quizes #4, #5, #6 and #7 and will be used for Multiple Imputation
correctness <- fread("correctness.csv", head=T)
totscores <- correctness[,list(total4=Q4q3+Q4q4+Q4q5+Q4q6+Q4q7+Q4q8+Q4q9+Q4q10+Q4q11+Q4q12+Q4q13+Q4q14+Q4q15+Q4q16,
                               total5=Q5q3+Q5q4+Q5q5+Q5q6+Q5q7+Q5q8+Q5q9+Q5q10+Q5q11+Q5q12,
                               total6=Q6q3+Q6q4+Q6q5+Q6q6+Q6q7+Q6q8+Q6q9+Q6q10+Q6q11+Q6q12+Q6q13+Q6q14+Q6q15+Q6q16,
                               total7=Q4q3+Q4q4+Q4q5+Q4q6+Q4q7+Q4q8+Q4q9+Q4q10+Q4q11+Q4q12+Q4q13)]

dat2<- cbind(dat1, totscores)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create Response order measure (ROM)
# quantify response order data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
orders<-read.csv("orders.csv")

name.order<-c("id", paste0("Q",4,"_",1:18,"th")) 

# data set-1 correctness
order_q4 <- orders[, name.order] 
str(order_q4)

order.q4<-order_q4[,-1]

result1<-matrix(rep(NA, 170*18), nrow=170, ncol=18) 
result2<-matrix(rep(NA, 170*17), nrow=170, ncol=17) 

for (i in 1:ncol(order.q4)) {
  result1[,i]<- abs(order.q4[,i] - i)
}

for (i in 1:(ncol(order.q4)-1)) {
  result2[,i]<- abs(order.q4[,i+1] - order.q4[,i])
}

# get the index
order0<-rowSums(result1) + rowSums(result2)- (ncol(order.q4)-1)

class.q4<-data.frame(cbind(order_q4$id,order0))
colnames(class.q4)<-c("id","order0")

# merge data
dat3<- merge(dat2,class.q4, by="id")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get response time
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get Q4 response time
time <- read.csv("timesToAnswerSec.csv")
name2<- paste0("Q",4,"q",3:16) 
time1 <- time[, name2]  

# rescale time data to numeric and rename the variables
time1[] <- lapply(time1, function(x) as.numeric(as.character(x)))

for(i in 1:14){
  time1[,i] <- ifelse(time1[,i] > 20*60, NA, time1[,i])
}

time1<-cbind(time$id,time1)
colnames(time1) <- c("id", paste0("q",3:16)) 
str(time1)

dat4 <- merge(dat3,time1, by="id")
str(dat4)

# save merged data that is in wide format
# write.csv(dat4, "dat4.csv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Multiple Imputation using MICE R package
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## examine missingness
library(lattice)
require(VIM)
summary(is.na(dat4))
md.pattern(dat4)
aggr(dat4, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(dat4), cex.axis=.7, 
                   gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

marginplot(dat4[, c("q3", "q4")], col = mdc(1:2), cex = 1.2,
           cex.lab = 1.2, cex.numbers = 1.3, pch = 19)

## mice imputation
impdata <- mice(dat4[,-1], m=10, seed=23109)
print(impdata)

# obtain a summary of imputation
capture.output(impdata, file= "summary_imp.csv")

# Checking imputations visually
xyplot(impdata, q3 ~ q4 | .imp, pch = 20, cex = 1.4)

densityplot(impdata)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### reshape imputed data sets and add cognitive level variable
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# save a particular imputed data set
library(plyr)
data_copy <- list()

for(i in 1:10){
   imp = complete(impdata, i)
   imp$id<- dat4$id # dat4 is the data before imputation
   
   # the columns may change if a different quiz is used; two time varying variables
   correctness<-colnames(imp[, 4:17]) # get correctness of all items
   resp_time <-colnames(imp[, 23:36]) # get response time of all items
   
   # change the data from wide to long format
   dat.new<-reshape(imp, idvar=c("id","Q4q1","Q4q2","Q4q18","order0", "total4"),
                 varying=list(correctness,resp_time),
                 v.names=c("correct","time_used"), # the values for each time point/item
                 times = paste0("Q",4,"q",3:16), # the labels for the time points/items
                 direction = "long",
                 sep= "_")
   
   # change the variable names
    colnames(dat.new) <-c("s1","s2","s4","totQ4","totQ5","totQ6", "totQ7","order","id","item","correct","time")
   
   # add cognitive level variable, recall=0, comprehension =1, apply =2
    dat.new$cog <- revalue(dat.new$item, c("Q4q3"=1, "Q4q4"=0,"Q4q5"=0,"Q4q6"=1,
                                           "Q4q7"=1,"Q4q8"=2,"Q4q9"=2,"Q4q10"=2,"Q4q11"=1,"Q4q12"=1,
                                           "Q4q13"=1,"Q4q14"=1,"Q4q15"=0,"Q4q16"=2))
    dat.new$cog <-as.factor(dat.new$cog)
    # save the data to the list
   data_copy[[i]] <- data.frame(dat.new)
   rm(dat.new)
 }


## assign each imputed data a name
for (i in seq(1:10)){
   assign(paste0("DATA", i), data_copy[[i]])
}


# save 10 impuated data sets in .csv data formate
for (df in grep("DATA", ls(), value = TRUE)){
   write.csv(eval(parse(text = df)), file = paste0(df, ".csv"), row.names = FALSE)
}
