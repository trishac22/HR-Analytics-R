#Load the data
library(ggplot2)
mydata = read.csv("C:/Users/DELL/Downloads/Human-Resource-Analytics-in-R-master/Dataset/HR_comma_sep.csv")
names(mydata)
#********************DATA MANIPULATION AND DATA PREPARATION********************

#Adding a new column called 'salaryOrder'
mydata$salaryOrder[which(mydata$salary == "low")] = 1
mydata$salaryOrder[which(mydata$salary == "medium")] = 2
mydata$salaryOrder[which(mydata$salary == "high")] = 3

#Adding a new column called 'employee_satisfaction'
mydata$employee_satisfaction[mydata$satisfaction_level >= 0.9] = '1.Maximum'
mydata$employee_satisfaction[mydata$satisfaction_level >= 0.8 & mydata$satisfaction_level < 0.9 ] = '2.High'
mydata$employee_satisfaction[mydata$satisfaction_level >= 0.6 & mydata$satisfaction_level < 0.8 ] = '3.Good'
mydata$employee_satisfaction[mydata$satisfaction_level >= 0.4 & mydata$satisfaction_level < 0.6 ] = '4.Average'
mydata$employee_satisfaction[mydata$satisfaction_level >= 0.2 & mydata$satisfaction_level < 0.4 ] = '5.Low'
mydata$employee_satisfaction[mydata$satisfaction_level <  0.2] = '6.Minimum'

#Converting the employee_satisfaction column as a factor
mydata$employee_satisfaction = as.factor(mydata$employee_satisfaction)

#One more new variable for 'left' for string representation.
mydata$leftFlag[mydata$left ==  1] = 'Left'
mydata$leftFlag[mydata$left ==  0] = 'Not Left'

#********************EDA********************

#********************SUMMARY********************

#Data Summary
dim(mydata)
str(mydata)
summary(mydata)
#Get the class
sapply(mydata,class)

#par(mfrow=c(1,1))
#attach(mydata)

#********************HISTOGRAMS********************

par(mfrow=c(2,2))
hist(mydata$satisfaction_level, main = "Histogram of Satisfaction", xlab = "Satisfaction")
hist(mydata$last_evaluation, main = "Histogram of Last Evaluation", xlab = "Last Evaluation")
hist(mydata$average_montly_hours, main = "Histogram of Avg Hours Spent", xlab = "Avg Hours Spent")
hist(mydata$time_spend_company, main = "Histogram of Time Spent in Company", xlab = "Time Spent in Company")

#********************BOXPLOTS********************

par(mfrow=c(2,2))
boxplot(mydata$satisfaction_level, main = "Satisfaction")
boxplot(mydata$last_evaluation, main = "Last Evaluation")
boxplot(mydata$average_montly_hours, main = "Avg Hours Spent")
boxplot(mydata$time_spend_company, main = "Time Spent in Company")

#********************Satisfaction Vs Employees Left / Not Left********************

par(mfrow=c(1,1))
#Create a barplot 'Employees left vs Satisfaction'
SatisfactionAndLeftTable <- table(mydata$leftFlag, mydata$employee_satisfaction)
barplot(SatisfactionAndLeftTable, main="Satisfaction Vs Employees Left / Not Left",
        xlab="Satisfaction Level", col=c("purple","orange"),
        legend = rownames(SatisfactionAndLeftTable), beside=TRUE)

#********************Employees Left / Not Left vs No. of Projects********************

projectsPlotData <- table(mydata$leftFlag, mydata$number_project)
barplot(projectsPlotData, main="Employees Left / Not Left vs No. of Projects",
        xlab="Number of Projects", col=c("purple","orange"),
        legend = rownames(projectsPlotData), beside=TRUE)

#********************PIE CHART********************

p = ggplot(subset(mydata,left==1), aes(x = factor('Salary'), fill = factor(salary))) +
  geom_bar(width = 1, position = "fill", color = "black") + coord_polar(theta = "y")+theme_bw()+
  ggtitle("Salary Splitup") +xlab("")+ylab("") + scale_fill_discrete(name="Salary")

p = p + theme(
  plot.title = element_text(color="Black", size=14, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="Black", size=14, face="bold"),
  axis.title.y = element_text(color="Black", size=14, face="bold")
)
print(p)

#********************Frequency By Salary Order of Employees********************

table1<-table(mydata$salaryOrder,(mydata$employee_satisfaction))
#print(table1)
table1<-as.data.frame(table1)
table1$salaryOrder = table1$Var1
table1$employee_satisfaction = table1$Var2
table1$Var1= NULL
table1$Var2= NULL

print(table1)
library(ggplot2)

p<-ggplot(table1, aes(x=salaryOrder,y=Freq,fill=employee_satisfaction)) +
  geom_bar(position="dodge",stat='identity') +
  ggtitle("Frequency By Salary Order of Employees") +xlab("Salary Order") +ylab("Frequency")

p = p + theme(
  plot.title = element_text(color="Black", size=14, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="Black", size=14, face="bold"),
  axis.title.y = element_text(color="Black", size=14, face="bold")  
)
print(p)

#********************Number of Employees left for each Department********************
roleTable<-table(mydata$role,mydata$left)
roledf<-as.data.frame(roleTable)
roledf$role = roledf$Var1
roledf$leftFlag = roledf$Var2
roledf$Var1= NULL
roledf$Var2= NULL

roledfLeft<-subset(roledf,leftFlag==1)
print(roledfLeft)

#Employees Left By Department
roledfLeft$role <- factor(roledfLeft$role, levels = roledfLeft$role[order(-roledfLeft$Freq)])
e<-ggplot(roledfLeft, aes(x=role,y=Freq,fill="Orange")) +
  geom_bar(stat='identity') +theme(axis.text.x = element_text(angle = 45, hjust = 1))+ guides(fill=FALSE) +
  ggtitle("Number of Employees left for each Department") +xlab("Department")

e = e + theme(
  plot.title = element_text(color="Black", size=14, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="Black", size=14, face="bold"),
  axis.title.y = element_text(color="Black", size=14, face="bold")
)
print(e)

#********************Department Vs Satisfaction of Employees********************
library(dplyr)
groupedByRole = mydata %>%
  group_by(role) %>%
  summarise(mean=mean(satisfaction_level), sd=sd(satisfaction_level), count=n())

groupedByRole = data.frame(groupedByRole)
groupedByRole = groupedByRole[order(groupedByRole$mean),]
p<-ggplot(groupedByRole, aes(x=reorder(role, -mean),y=mean,fill="Orange")) +
  geom_bar(stat='identity') +theme(axis.text.x = element_text(angle = 45, hjust = 1))+ guides(fill=FALSE) +coord_cartesian(ylim = c(0.55, 0.63)) +
  ggtitle("Department Vs Satisfaction of Employees") +xlab("Department")

p = p + theme(
  plot.title = element_text(color="Black", size=14, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="Black", size=14, face="bold"),
  axis.title.y = element_text(color="Black", size=14, face="bold")
)
print(p)

#********************Number of Projects Vs Satisfaction of Employees********************

p<-ggplot(mydata, aes(x = factor(number_project), y = satisfaction_level, fill=factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("orange", "purple"))+
  ggtitle("Number of Projects Vs Satisfaction of Employees") +xlab("Number of Projects") +ylab("Satisfaction Level")

p = p + theme(
  plot.title = element_text(color="Black", size=14, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="Black", size=14, face="bold"),
  axis.title.y = element_text(color="Black", size=14, face="bold")
)
print(p)

#********************Number of Projects Vs  Last Evaluation Score of Employees********************

p<-ggplot(mydata, aes(x = factor(number_project), y = last_evaluation, fill=factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("orange", "purple"))+
  ggtitle("Number of Projects Vs  Last Evaluation Score of Employees") +xlab("Number of Projects") +ylab("Last Evaluation")

p = p + theme(
  plot.title = element_text(color="Black", size=14, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="Black", size=14, face="bold"),
  axis.title.y = element_text(color="Black", size=14, face="bold")
)
print(p)

#********************Number of Projects Vs  Average Montly Hours of Employees********************

p<-ggplot(mydata, aes(x = factor(number_project), y = average_montly_hours, fill=factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("orange", "purple"))+
  ggtitle("Number of Projects Vs  Average Montly Hours of Employees") +xlab("Number of Projects") +ylab("Average Montly Hours")

p = p + theme(
  plot.title = element_text(color="Black", size=14, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="Black", size=14, face="bold"),
  axis.title.y = element_text(color="Black", size=14, face="bold")
)
print(p)

#********************CORRELATIONS********************
library("GGally")
ggpairs(mydata, columns=c("satisfaction_level","last_evaluation","number_project","average_montly_hours","time_spend_company","left"))
#The correlations can be interpreted as -
#Employees who worked on more projects spent more average monthly hours and their 
#last evaluation was good.
#Also work accidents and whether or not an employee got promoted in last five years could 
#be reasons for leaving the company.
#But most importantly, when an employee was dis-satisified they left the company.

summary(mydata)
names(mydata)
mydata$left <- factor(mydata$left)

#It can be seen that 11428 employees stayed and 3571 employees left.

#********************LOGISTIC REGRESSION FOR 'LEFT' VARIABLE********************
#Logistic regression is a method for fitting a regression curve, y = f(x), when y is a categorical variable.
#Since the model we are trying to build will be predicting whether an employee will stay (0) or leave (1) the company logistic regression model suits the best

#*************Split the data set to train and test data sets**********************
train <- mydata[1:12000,]
test <- mydata[12001:14999,]
dim(test)
dim(train)
library(pscl)
model<-glm(left~satisfaction_level+last_evaluation+average_montly_hours+salary+number_project,data=train,binomial())
pR2(model)
summary(model)
fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.95,1,0)
misClasificError <- mean(fitted.results != test$left, na.rm = T)
print(paste('Accuracy',1-misClasificError))

#******************T Test to confirm the hypothesis:*************************

#Let's conduct a t-test at 95% confidence level and see if it correctly rejects the null hypothesis that the sample comes from the same distribution as the employee population. To conduct a one sample t-test, we can use the stats.ttest_1samp() function:
overallSatisfaction <-mean(mydata$satisfaction_level)
left_pop<-subset(mydata,left==1)

emp_turnover_satisfaction <-mean(left_pop$satisfaction_level)
emp_turnover_satisfaction
#One Sample T Test
t.test(left_pop$satisfaction_level,mu=overallSatisfaction)
#p<0.05 - keep alt hypo; reject NULL hypo
#Reject the null hypothesis because:
#P-value is lower than confidence level of 5%

#Two Sample T Test
t.test(left_pop$satisfaction_level, mydata$satisfaction_level)
#p>0.05 - reject alt hypo
#p<0.05 - keep alt hypo; reject NULL hypo

library(ROCR)
p <- predict(model, newdata=test, type="response")
pr <- prediction(p, test$left)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#We see that the model accuracy is not good so we try to improve our model
#One of the best ways of improving the model is by using floor and ceiling 
#rounding of numbers in our data set and splitting it 80% to train and 20% to test.

#Floor method
data_size <- floor(0.8 * nrow(mydata))

#Set the seed to make your partition reproductible
set.seed(100)
train_data <- sample(seq_len(nrow(mydata)), size = data_size)
train1 <- mydata[train_data, ]
test1 <- mydata[-train_data, ]
dim(test1)
dim(train1)
model1<-glm(left ~ satisfaction_level + last_evaluation + number_project 
            + average_montly_hours + time_spend_company + Work_accident
            + promotion_last_5years + salaryOrder,data=train1,binomial())

summary(model1)
anova(model1, test="Chisq")

pR2(model1)

fitted.results <- predict(model1,newdata=test1,type='response')
fitted.results <- ifelse(fitted.results > 0.95,1,0)
misClasificError <- mean(fitted.results != test1$left, na.rm = T)
print(paste('Accuracy',1-misClasificError))

p <- predict(model1, newdata=test1, type="response")
pr <- prediction(p, test1$left)

prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
#We see a lot of improvement in our model through floor effect

#Ceiling effect
data_size1 <- ceiling(0.8 * nrow(mydata))

#Set the seed to make your partition reproductible
set.seed(100)
train_data1 <- sample(seq_len(nrow(mydata)), size = data_size1)
train2 <- mydata[train_data1, ]
test2 <- mydata[-train_data1, ]
dim(test2)
dim(train2)

model2<-glm(left ~ satisfaction_level + last_evaluation + number_project 
            + average_montly_hours + time_spend_company + Work_accident
            + promotion_last_5years + salaryOrder,data=train2,binomial())

summary(model2)
anova(model2, test="Chisq")

pR2(model2)
fitted.results <- predict(model2,newdata=test2,type='response')
fitted.results <- ifelse(fitted.results > 0.95,1,0)
misClasificError <- mean(fitted.results != test2$left, na.rm = T)
print(paste('Accuracy',1-misClasificError))

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
#We see similar accuracy in ceiling as in floor, both gave better accuracy than manual split model

#Find the mean of employees population who stayed and mean of employees who left
data_set_satisfaction <-mean(mydata$satisfaction_level)
left_satisfaction <-subset(mydata,left==1)
stay_satisfaction <-subset(mydata,left==0)
data_set_left_satisfaction <-mean(left_satisfaction$satisfaction_level)
data_set_stay_satisfaction <-mean(stay_satisfaction$satisfaction_level)
print( c(data_set_stay_satisfaction,data_set_left_satisfaction ) )

#Welch Two Sample t-test
t.test(left_satisfaction$satisfaction_level,mu=data_set_stay_satisfaction) 
# Employee Population mean ssatisfaction

#Convert the variable left to numeric and find the confidence interval
left_new <- sum(as.numeric(mydata$left))
LWR <-qt(0.025,left_new)  # Low Quartile
UPR <-qt(0.95,left_new)  # High Quartile
print (c(LWR, UPR))



#To summarize our analysis on why employees leave the company, out of all the contributing factors 
#the strongest predictor is Employee satisfaction.
#Employees generally leave when they are overworked(more than 250 average_monthly_hours) or underworked 
#(less than 150 average_monthly_hours)
#Employees with low or really high evaluations are probably leaving the company
#Employees with low or medium salaries left the company
#Employees who had less(less than 3 number of projects) or more (6 or above) project count are leaving the company


