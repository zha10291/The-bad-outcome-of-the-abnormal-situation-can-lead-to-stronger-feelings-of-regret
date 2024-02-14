library(MBESS)
library(psych)
library(ggplot2)
library(dplyr) 
library(Hmisc)
library(effsize)
library(rstudioapi)
library(jmv)
library(Rcpp)
# setting formatting options
options(scipen=999.99, digits =7)

# load our dataset 
data<- readRDS("/cloud/project/input/data/osf-past-normality-regret-replication-exp1-data.rds")

#convert rds file to csv
write.csv(data, "data.csv",row.names = FALSE)
