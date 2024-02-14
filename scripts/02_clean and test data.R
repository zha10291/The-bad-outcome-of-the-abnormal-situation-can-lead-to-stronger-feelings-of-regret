
#checking bound minimum
min(data$Q_TotalDuration, na.rm=TRUE) > 0

#clean data
data<-data[which(data$Q_TotalDuration!=0),]
#Gender
data$gender
table(data$gender)
#Age
data$age
data$age[data$age==99] <- NA
mean(data$age, na.rm = TRUE)
sd(data$age, na.rm = TRUE)