#Measure correction: All particpants who indicated (5- somwhat agree) on the question regarding random chance (data$Sc2_random_1 or data$Sc2_random_2) had accidently assigned the value '56' instead of '5' in Qualtrics
data$Sc2_random_1[data$Sc2_random_1==56] <- 5
data$Sc2_random_2[data$Sc2_random_2==56] <- 5

# Conversion from numeric to factors 
data$Sc2_regret<- factor(data$Sc2_regret)
data$Sc2_lucky<- factor(data$Sc2_lucky)

# Label values
data$Sc2_regret <- ordered(data$Sc2_regret, levels = c(1,2), labels = c("Routine Adams", "Exception White"))
data$Sc2_lucky <- ordered(data$Sc2_lucky, levels = c(1,2), labels = c("Adams less lucky", "White less lucky"))
names (data$Sc2_random_1) <- c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")
names (data$Sc2_random_2) <- c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")

# Label Variables
label(data$Sc2_regret) <- "Who feels more upset (direct replication)"
label(data$Sc2_random_1) <- "Adam's (Routine) accident is a random coincidence"
label(data$Sc2_random_2) <- "White's' (Exception) accident is a random coincidence"
label(data$Sc2_lucky) <- "Who is less lucky"

# Descriptives for main variables
jmv::descriptives(
  data=data, 
  vars=c(
    "Sc2_regret",
    "Sc2_random_1",
    "Sc2_random_2",
    "Sc2_lucky"),
  freq=TRUE)



# binomial Z
jmv::propTest2(
  data=data,
  vars=c(
    "Sc2_regret",
    "Sc2_lucky"),
  ci=TRUE)

jmv::propTestN(
  data=data,
  var="Sc2_regret",
  expected=TRUE,
  ratio=c(1, 1))

jmv::propTestN(
  data=data,
  var="Sc2_lucky",
  expected=TRUE,
  ratio=c(1, 1))

#Chi2 without Jamovi for regret and luck

x5 <- sum((data$Sc2_regret[!is.na(data$Sc2_regret)])=="Exception White")
n5 <- length(data$Sc2_regret[!is.na(data$Sc2_regret)])
prop.test(x5, n5, p=0.5, correct = FALSE)

x6 <- sum((data$Sc2_lucky[!is.na(data$Sc2_lucky)])=="Exception White")
n6 <- length(data$Sc2_lucky[!is.na(data$Sc2_lucky)])
prop.test(x6, n6, p=0.5, correct = FALSE)

#t-test for "random chance"-Variable

data$Sc2_random_1
data$Sc2_random_2
table(data$Sc2_random_1)
table(data$Sc2_random_2)
summary(data$Sc2_random_1)
summary(data$Sc2_random_2)
sd(data$Sc2_random_1, na.rm = TRUE)
sd(data$Sc2_random_2, na.rm = TRUE)
t.test(data$Sc2_random_1,data$Sc2_random_2, paired=TRUE)
hist(data$Sc2_random_1, main = "Histogram of routine randomness", xlab = "Adam's (Routine) accident is a random coincidence")
hist(data$Sc2_random_2, main = "Histogram of exception randomness", xlab = "White's' (Exception) accident is a random coincidence")

#plot charts
data6<-as.data.frame(table(data$Sc2_regret))
data6$pro<-data6$Freq/342
data6$state<-"regret"
data7<-as.data.frame(table(data$Sc2_lucky))
data7$pro<-data7$Freq/342
data7$state<-"luck"
newdata2<-rbind(data6,data7)
newdata2$perceived<-c("Adams-Routine","White-Exception","Adams-Routine","White-Exception")
newdata2 |> 
  ggplot(aes(x = state,y= pro, fill =perceived)) +
  geom_col(position = "dodge2") + 
  labs( y = "percentage")