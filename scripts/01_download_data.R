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
data <- read.csv("C:/Users/news/Desktop/osf-past-normality-regret-replication-exp1-data.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
str(data)
