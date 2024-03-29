# EXPERIMENT 1 (Hitchhiker-Scenario)

# JAMOVI requires factors, while R imports as numeric. So, need to convert from numeric to factor.
data$Sc1_regret<- factor(data$Sc1_regret)
data$sc1_socnorms1<- factor(data$sc1_socnorms1)
data$sc1_socnorms2<- factor(data$sc1_socnorms2)
data$sc1_combinednorms<- factor(data$sc1_combinednorms)

# Let's label the values better, so it's easier to understand the output.
data$Sc1_regret <- ordered(data$Sc1_regret, levels = c(1,2), labels = c("Exception Jones", "Routine Smith"))
data$sc1_socnorms1 <- ordered(data$sc1_socnorms1, levels = c(1,2), labels = c("Exception Jones", "Routine Smith"))
data$sc1_socnorms2 <- ordered(data$sc1_socnorms2, levels = c(1,2), labels = c("Exception Jones", "Routine Smith"))
data$sc1_combinednorms <- ordered(data$sc1_combinednorms, levels = c(1,2), labels = c("Exception Jones", "Routine Smith"))

# Let's label the variables better, so we'll remember what those mean and it's easier to understand the output when those are reported.
label(data$Sc1_regret) <- "Who experiences higher regret (direct replication)" 
label(data$sc1_socnorms1) <- "Descriptive norms - which is more common?" 
label(data$sc1_socnorms2) <- "Injunctive norms - who is more criticized by society?" 
label(data$sc1_combinednorms) <- "Who experiences higher regret, when asking participants to consider the norm" 


# Let's run the JAMOVI imported syntax 

# Descriptives for the main variables.
# Plots appear in the R Studio Plots section
jmv::descriptives(
  data=data,
  vars=c(
    "Sc1_regret",
    "sc1_socnorms1",
    "sc1_socnorms2",
    "sc1_combinednorms"),
  freq=TRUE)


# binomial Z
jmv::propTest2(
  data=data,
  vars=c(
    "Sc1_regret",
    "sc1_socnorms1",
    "sc1_socnorms2",
    "sc1_combinednorms"),
  ci=TRUE)

jmv::propTestN(
  data=data,
  var="Sc1_regret",
  expected=TRUE,
  ratio=c(1, 1))

jmv::propTestN(
  data=data,
  var="sc1_socnorms1",
  expected=TRUE,
  ratio=c(1, 1))

jmv::propTestN(
  data=data,
  var="sc1_socnorms2",
  expected=TRUE,
  ratio=c(1, 1))

jmv::propTestN(
  data=data,
  var="sc1_combinednorms",
  expected=TRUE,
  ratio=c(1, 1))

# plot charts
summary(data$Sc1_regret)
summary(data$sc1_socnorms1)
summary(data$sc1_socnorms2)
summary(data$sc1_combinednorms)

data2<-as.data.frame(table(data$Sc1_regret))
data2$pro<-data2$Freq/342
data2$state<-"regret"
data3<-as.data.frame(table(data$sc1_socnorms1))
data3$pro<-data3$Freq/342
data3$state<-"Descriptive norms"
data4<-as.data.frame(table(data$sc1_socnorms2))
data4$pro<-data4$Freq/342
data4$state<-"Injunctive norms"
data5<-as.data.frame(table(data$sc1_combinednorms))
data5$pro<-data5$Freq/342
data5$state<-"Negative effect"
newdata<-rbind(data2,data3,data4,data5)

newdata |> 
  ggplot(aes(x = state,y= pro, fill = Var1)) +
  geom_col(position = "dodge2") + 
  labs( y = "percentage")