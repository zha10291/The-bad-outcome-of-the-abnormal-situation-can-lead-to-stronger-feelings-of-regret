# calculate condition
data$Sc3condition <- 0
data$compensationagg <- 0
data$regretagg <- 0
for (i in 1:nrow(data)){
  if (!is.na(data$Sc3_C1_text[i])){
    data$Sc3condition[i] <- 1
    data$compensationagg[i] <- data$sc3_c1_compensation[i]
    data$regretagg[i] <- data$sc3_c1_regret[i]
  }
  else if (!is.na(data$Sc3_C2_text[i])){
    data$Sc3condition[i] <- 2
    data$compensationagg[i] <- data$sc3_c2_compensation[i]
    data$regretagg[i] <- data$sc3_c2_regret[i]
  }
  else if (!is.na(data$Sc3_C3_text[i])){
    data$Sc3condition[i] <- 3
    data$compensationagg[i] <- data$sc3_c3_compensation[i]
    data$regretagg[i] <- data$sc3_c3_regret[i]
  }
  else {
    data$Sc3condition[i] <- NA
    data$compensationagg[i] <- NA
    data$regretagg[i] <- NA
  }
}

#value labels
data$Sc3conditionl<-factor(data$Sc3condition,levels = c(1,2,3), labels=c("Routine", "Self-produced exception", "Other-produced exception"))

# let's have a look at this
table(data$Sc3conditionl)

#Adjust Copensation-Scale to original paper (Values from 0 to 10, instead of 1 to 11)
data$compensationaggrecoded = data$compensationagg-1

#Adjust Regret-Scale (Values from 1 to 5, instead of 0 to 4)
data$regretaggrecoded = data$regretagg+1


# Label Variables
names (data$compensationaggrecoded) <- c("0", "100,000", "200,000", "300,000", "400,000", "500,000", "600,000", "700,000", "800,000", "900,000", "1,000,000")
names (data$regretaggrecoded) <- c("no regret", "weak regret", "medium regret", "strong regret", "very strong regret")


########
# Compensation
########

# get descriptives
describeBy(data$compensationaggrecoded, data$Sc3conditionl, mat=TRUE)

# run an omnibus ANOVA for compensation
# from https://blogs.uoregon.edu/rclub/2015/11/03/anova-contrasts-in-r/ 
model <- aov(compensationaggrecoded ~ Sc3conditionl, data = data)
summary(model)

# contrasts
TukeyHSD(model)
t.test(
  data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded, 
  data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded)
t.test(
  data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded, 
  data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded)
t.test(
  data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded, 
  data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded)
ci.smd(smd=smd(data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded, 
               data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded), 
       n.1=length(data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded), 
       n.2=length(data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded), 
       conf.level=0.95)
ci.smd(smd=smd(data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded, 
               data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded), 
       n.1=length(data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded), 
       n.2=length(data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded), 
       conf.level=0.95)
ci.smd(smd=smd(data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded, 
               data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded), 
       n.1=length(data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded), 
       n.2=length(data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded), 
       conf.level=0.95)


# plot it out
data.plot = data[which(!is.na(data$Sc3conditionl)),]
groups <- group_by(data.plot, Sc3conditionl) # this just prepares it for us to calculate eveyrthing within each condition
plot.data <- summarise(groups,
                       mean = mean(compensationaggrecoded, na.rm=TRUE),
                       sd = sd(compensationaggrecoded, na.rm=TRUE),
                       n = n(),
                       se=sd/sqrt(n),
                       ci = qt(0.975, df=n-1)*se)
plot.data # take a peek
ggplot(plot.data, aes(x=Sc3conditionl, y=mean, group = factor(1))) +
  geom_line() +
  geom_point() + 
  xlab("Condition") +
  ylab("Compensation") +
  expand_limits(y=4) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1)

threeconditionscompensation <- ggplot(data, aes(x=Sc3conditionl, y=compensationaggrecoded)) + 
  geom_violin() + geom_violin(trim=FALSE) + 
  scale_x_discrete(limits=c("Routine", "Other-produced exception", "Self-produced exception")) +
  geom_jitter(shape=16, position=position_jitter(height=0.1, width=0.2)) +
  scale_color_grey() + scale_fill_grey() +
  theme(axis.title.x=element_blank(), 
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        panel.grid.major = element_line(colour="grey", size = (0.1)),
        panel.grid.minor = element_line(size = (0.1), colour="grey"),
        text=element_text(family="Arial"))+
  xlab("Condition") +
  ylab("Compensation") +
  scale_y_continuous(minor_breaks = seq(-10, 15, 5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.90), width = 0.2) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3)
threeconditionscompensation


########
# Regret
########


# get descriptives
describeBy(data$regretaggrecoded, data$Sc3conditionl, mat=TRUE)


# run an omnibus ANOVA
model <- aov(regretaggrecoded ~ Sc3conditionl, data = data)
summary(model)

# contrasts
TukeyHSD(model)
t.test(
  data[which(data$Sc3conditionl=="Routine"),]$regretaggrecoded, 
  data[which(data$Sc3conditionl=="Self-produced exception"),]$regretaggrecoded)
t.test(
  data[which(data$Sc3conditionl=="Routine"),]$regretaggrecoded, 
  data[which(data$Sc3conditionl=="Other-produced exception"),]$regretaggrecoded)
t.test(
  data[which(data$Sc3conditionl=="Self-produced exception"),]$regretaggrecoded, 
  data[which(data$Sc3conditionl=="Other-produced exception"),]$regretaggrecoded)
ci.smd(smd=smd(data[which(data$Sc3conditionl=="Routine"),]$regretaggrecoded, 
               data[which(data$Sc3conditionl=="Self-produced exception"),]$regretaggrecoded), 
       n.1=length(data[which(data$Sc3conditionl=="Routine"),]$regretaggrecoded), 
       n.2=length(data[which(data$Sc3conditionl=="Self-produced exception"),]$regretaggrecoded), 
       conf.level=0.95)
ci.smd(smd=smd(data[which(data$Sc3conditionl=="Routine"),]$regretaggrecoded, 
               data[which(data$Sc3conditionl=="Other-produced exception"),]$regretaggrecoded), 
       n.1=length(data[which(data$Sc3conditionl=="Routine"),]$regretaggrecoded), 
       n.2=length(data[which(data$Sc3conditionl=="Other-produced exception"),]$regretaggrecoded), 
       conf.level=0.95)
ci.smd(smd=smd(data[which(data$Sc3conditionl=="Self-produced exception"),]$regretaggrecoded, 
               data[which(data$Sc3conditionl=="Other-produced exception"),]$regretaggrecoded), 
       n.1=length(data[which(data$Sc3conditionl=="Self-produced exception"),]$regretaggrecoded), 
       n.2=length(data[which(data$Sc3conditionl=="Other-produced exception"),]$regretaggrecoded), 
       conf.level=0.95)




# plot it out
data.plot = data[which(!is.na(data$Sc3conditionl)),]
groups <- group_by(data.plot, Sc3conditionl) # this just prepares it for us to calculate eveyrthing within each condition
plot.data <- summarise(groups,
                       mean = mean(regretaggrecoded, na.rm=TRUE),
                       sd = sd(regretaggrecoded, na.rm=TRUE),
                       n = n(),
                       se=sd/sqrt(n),
                       ci = qt(0.975, df=n-1)*se)
# plot
ggplot(plot.data, aes(x=Sc3conditionl, y=mean, group = factor(1))) +
  geom_line() +
  geom_point() +
  xlab("Condition") +
  ylab("Regret") +
  expand_limits(y=3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1)



threeconditionsregret <- ggplot(data, aes(x=Sc3conditionl, y=regretaggrecoded)) + 
  geom_violin() + geom_violin(trim=FALSE) + 
  scale_x_discrete(limits=c("Routine", "Other-produced exception", "Self-produced exception")) +
  geom_jitter(shape=16, position=position_jitter(height=0.1, width=0.2)) +
  scale_color_grey() + scale_fill_grey() +
  theme(axis.title.x=element_blank(), 
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        panel.grid.major = element_line(colour="grey", size = (0.1)),
        panel.grid.minor = element_line(size = (0.1), colour="grey"),
        text=element_text(family="Arial"))+
  xlab("Condition") +
  ylab("Regret") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.90), width = 0.2) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3)
threeconditionsregret




#Preparation for t-test (Original analysis)
#Bring data it format with two rows (Compensation and Condition)

# calculate condition
data$Sc3condition1 <- 0
data$compensationagg1 <- 0
data$regretagg1 <- 0
for (i in 1:nrow(data)){
  if (!is.na(data$Sc3_C1_text[i])){
    data$Sc3condition1[i] <- 1
    data$compensationagg1[i] <- data$sc3_c1_compensation[i]
    data$regretagg1[i] <- data$sc3_c1_regret[i]
  }
  else if (!is.na(data$Sc3_C2_text[i])){
    data$Sc3condition1[i] <- 2
    data$compensationagg1[i] <- data$sc3_c2_compensation[i]
    data$regretagg1[i] <- data$sc3_c2_regret[i]
  }
  else if (!is.na(data$Sc3_C3_text[i])){
    data$Sc3condition1[i] <- 2
    data$compensationagg1[i] <- data$sc3_c3_compensation[i]
    data$regretagg1[i] <- data$sc3_c3_regret[i]
  }
  else {
    data$Sc3condition1[i] <- NA
    data$compensationagg1[i] <- NA
    data$regretagg1[i] <- NA
  }
}

#value labels
data$Sc3conditionl1<-factor(data$Sc3condition1,levels = c(1,2), labels=c("Routine", "Exception"))

# let's have a look at this
table(data$Sc3conditionl1)

#Adjust Copensation-Scale to original paper (Values from 0 to 10, instead of 1 to 11)
data$compensationaggrecoded1 = data$compensationagg1-1

#Adjust Regret Scale (Values 1 to 5, indead of 0 to 4)
data$regretaggrecoded1 = data$regretagg1+1

# Label Variables
names (data$compensationaggrecoded1) <- c("0", "100,000", "200,000", "300,000", "400,000", "500,000", "600,000", "700,000", "800,000", "900,000", "1,000,000")
names (data$regretaggrecoded1) <- c("no regret", "weak regret", "medium regret", "strong regret", "very strong regret")



#######
# Compensation
########

# get descriptives
describeBy(data$compensationaggrecoded1, data$Sc3conditionl1, mat=TRUE)

# run an omnibus ANOVA for compensation
# from https://blogs.uoregon.edu/rclub/2015/11/03/anova-contrasts-in-r/ 
model <- aov(compensationaggrecoded1 ~ Sc3conditionl1, data = data)
summary(model)

# contrasts
TukeyHSD(model)
t.test(
  data[which(data$Sc3conditionl1=="Routine"),]$compensationaggrecoded1, 
  data[which(data$Sc3conditionl1=="Exception"),]$compensationaggrecoded1)

ci.smd(smd=smd(data[which(data$Sc3conditionl1=="Exception"),]$compensationaggrecoded1,
               data[which(data$Sc3conditionl1=="Routine"),]$compensationaggrecoded1), 
       n.1=length(data[which(data$Sc3conditionl1=="Routine"),]$compensationaggrecoded1), 
       n.2=length(data[which(data$Sc3conditionl1=="Exception"),]$compensationaggrecoded1), 
       conf.level=0.95)

# what was the effect in the original study?
ci.smd(ncp=2.17, n.1=58, n.2 = 105, conf.level=0.95)

exceptioncombinedcompensationplot <- ggplot(data, aes(x=Sc3conditionl1, y=compensationaggrecoded1)) + 
  geom_violin() + geom_violin(trim=FALSE) + 
  scale_x_discrete(limits=c("Routine", "Exception")) +
  geom_jitter(shape=16, position=position_jitter(height=0.1, width=0.2)) +
  scale_color_grey() + scale_fill_grey() +
  theme(axis.title.x=element_blank(), 
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        panel.grid.major = element_line(colour="grey", size = (0.1)),
        panel.grid.minor = element_line(size = (0.1), colour="grey"),
        text=element_text(family="Arial"))+
  xlab("Condition") +
  ylab("Compensation") +
  scale_y_continuous(minor_breaks = seq(-10, 15, 5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.90), width = 0.2) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3)
exceptioncombinedcompensationplot


########
# Regret
########

# get descriptives
describeBy(data$regretaggrecoded1, data$Sc3conditionl1, mat=TRUE)

# run an omnibus ANOVA for compensation
# from https://blogs.uoregon.edu/rclub/2015/11/03/anova-contrasts-in-r/ 
model <- aov(regretaggrecoded1 ~ Sc3conditionl1, data = data)
summary(model)

# contrasts
TukeyHSD(model)
t.test(
  data[which(data$Sc3conditionl1=="Routine"),]$regretaggrecoded1, 
  data[which(data$Sc3conditionl1=="Exception"),]$regretaggrecoded1)

ci.smd(smd=smd(data[which(data$Sc3conditionl1=="Exception"),]$regretaggrecoded1,
               data[which(data$Sc3conditionl1=="Routine"),]$regretaggrecoded1), 
       n.1=length(data[which(data$Sc3conditionl1=="Routine"),]$regretaggrecoded1), 
       n.2=length(data[which(data$Sc3conditionl1=="Exception"),]$regretaggrecoded1), 
       conf.level=0.95)

exceptioncombinedregretplot <- ggplot(data, aes(x=Sc3conditionl1, y=regretaggrecoded1)) + 
  geom_violin() + geom_violin(trim=FALSE)+ 
  scale_x_discrete(limits=c("Routine", "Exception")) +
  geom_jitter(shape=16, position=position_jitter(height=0.1, width=0.2)) +
  scale_color_grey() + scale_fill_grey()  +
  theme(axis.title.x=element_blank(), 
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        panel.grid.major = element_line(colour="grey", size = (0.1)),
        panel.grid.minor = element_line(size = (0.1), colour="grey"),
        text=element_text(family="Arial"))+
  xlab("Condition") +
  ylab("Regret") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.90), width = 0.2) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3)