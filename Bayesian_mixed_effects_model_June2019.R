## This R file is to demonstrate how to run Bayesian Mixed Effects Model 
## using 10 multiple imputed data sets                                    

library(MCMCglmm)
library(tidyverse)
library(plyr)
library(parallel)
library(coda)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# In the for loop, we conduct the models for each impuated data set  #
# and then store all the results in several files                    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
m=10
nitt = 50000
thin = 50
burnin = 10000
set.seed(688)

# used for store the results from the analyses
pooled_fixeffect <- NULL
pooled_randomeffect <- NULL
pooled_gelman <- list() # check model convergence
output.correct.Bayes <-list() # a list to store each model

# in the for loop, the analyses are conducted for 10 imputed data sets 
for (i in 1:m) {
  file.name <- paste("DATA", i ,".csv",sep="")
  data.to.use <- read.csv(file.name)
  
  # format variables
  data.to.use$id <-factor(data.to.use$id)
  data.to.use$s1 <- factor(data.to.use$s1)
  data.to.use$correct<-as.numeric(as.character(data.to.use$correct))
  data.to.use$cog <-factor(data.to.use$cog)
  data.to.use$cog2 <- revalue(data.to.use$cog, c("0"="recall", "1"="understand","2"="apply"))
  # convert response time to log scale
  data.to.use$timelogc <- log(data.to.use$time/60) - mean(log(data.to.use$time/60))
  
  # Bayesian GLM
  prior0 = list(R = list(V = 1, fix=1), G = list(G1 = list(V = 1, nu = 0.01)))
  
  output.correct.Bayes[[i]] <- mclapply(1:3, function(j) {
    MCMCglmm(correct ~ time * cog2 + order + s1 + s2, 
             random = ~ id, family="categorical",
             data=data.to.use, nitt=nitt, thin = thin, burnin = burnin, prior = prior0)
  }, mc.cores=1) # users can change the value of mc.cores based on their own computer 
  
  # save gelman diagnostic statistics
  mod_3chain  <- lapply(output.correct.Bayes[[i]], function(m) m$Sol)
  pooled_gelman[[i]] <-gelman.diag(mod_3chain)
  
  # save pooled posterior means for fixed effects
  fixeffect <- do.call(rbind.data.frame, mod_3chain)
  pooled_fixeffect <-rbind(pooled_fixeffect,fixeffect)
  
  # save pooled psterior means for random effect
  out_random <-do.call(cbind.data.frame, lapply(output.correct.Bayes[[i]], function(m) m$VCV[,1]))
  colnames(out_random)=c("chain1","chain2","chain3")
  
  randomeffect <- out_random %>%
    gather(key="chain", value= "raneff")
  pooled_randomeffect <-rbind(pooled_randomeffect,randomeffect) 
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## calculate the posterior mean and 95% CrI of           #
## all the estimates obtained from 10 imputed data sets  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## Part I: obtain the average of posterior means of fix effects and corresponding 95% Crl 
posterior_means <- colMeans(pooled_fixeffect)
lowerCI <- apply(pooled_fixeffect, 2, function(x) sort(x)[m*3*(nitt-burnin)/thin * 0.025])
upperCI <- apply(pooled_fixeffect, 2, function(x) sort(x)[m*3*(nitt-burnin)/thin * 0.975])

# showing regression coefficients
round(cbind(posterior_means,lowerCI,upperCI),3)

# showing odds ratios
round(exp(cbind(posterior_means,lowerCI,upperCI)),3)

## Part II: obtain the average of poserior mean of random effect and corresponding 95% Crl 
posterior_means_random <- mean(pooled_randomeffect$raneff) 
round(posterior_means_random,3)

CI95_random <-quantile(sort(pooled_randomeffect$raneff), c(0.025, 0.975))
round(CI95_random,3)

## Part III: obtain gelman diagnostic obtained from 10 imputed data sets
pooled_gelman

