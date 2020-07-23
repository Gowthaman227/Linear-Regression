install.packages(lattice)
library(lattice)
#Calories Consumed
cal_data <- read.csv(file.choose())
View(cal_data)
colnames(cal_data) <- c("Weight","Calories")
View(cal_data)
#EDA
summary(cal_data)
attach(cal_data)
hist(Weight)
hist(Calories)
qqnorm(Weight)
qqline(Weight)
qqnorm(Calories)
qqline(Calories)
dotplot(cal_data$Weight,main="Dot Plot of Weight")
#Scatter Plot
plot(Calories,Weight,main="Scatter Plot")
# Linear Regression Model
# Model 1
cal_reg <- lm(Weight~Calories,data=cal_data)
summary(cal_reg)
confint(cal_reg,level=0.95)
cal_pred <- predict(cal_reg,interval="predict")
cal_pred <- as.data.frame(cal_pred)
View(cal_pred)
# Model 2
cal_sqrt <- lm(Weight~sqrt(Calories),data=cal_data)
summary(cal_sqrt)
confint(cal_sqrt,level=0.95)
cal_pred1 <- predict(cal_sqrt,interval="predict")
cal_pred1 <- as.data.frame(cal_pred1)
View(cal_pred1)
# Model 3
cal_log <- lm(Weight~log(Calories),data=cal_data)
summary(cal_log)
confint(cal_log,level=0.95)
cal_pred2 <- predict(cal_log,interval="predict")
cal_pred2 <- as.data.frame(cal_pred2)
View(cal_pred2)
# Model 4
cal_reg1 <- lm(log(Weight)~Calories+I(Calories*Calories),data=cal_data)
summary(cal_reg1)
confint(cal_reg1,level=0.95)
cal_pred3 <- predict(cal_reg1,interval="predict")
cal_pred3 <- as.data.frame(cal_pred3)
View(cal_pred3)
# Model 5
cal_reg2 <- lm(sqrt(Weight)~sqrt(Calories),data=cal_data)
summary(cal_reg2)
confint(cal_reg2,level=0.95)
cal_pred4 <- predict(cal_reg2,interval="predict")
cal_pred4 <- as.data.frame(cal_pred4)
View(cal_pred4)
# Model 1 has highest R squared value of 0.8968 which is suitable for Weighted gained prediction
#Delivery Time
del_time <- read.csv(file.choose())
View(del_time)
#EDA
summary(del_time)
attach(del_time)
hist(Delivery.Time)
hist(Sorting.Time)
qqnorm(Delivery.Time)
qqline(Delivery.Time)
qqnorm(Sorting.Time)
qqline(Sorting.Time)
#Scatter Plot
plot(Sorting.Time,Delivery.Time,main="Scatter Plot")
# Linear Regression Model
# Model 1
del_reg <- lm(Delivery.Time~Sorting.Time,data=del_time)
summary(del_reg)
confint(del_reg,level=0.95)
del_pred <- predict(del_reg,interval="predict")
del_pred <- as.data.frame(del_pred)
View(del_pred)
# Model 2
del_sqrt <- lm(Delivery.Time~sqrt(Sorting.Time),data=del_time)
summary(del_sqrt)
confint(del_sqrt,level=0.95)
del_pred1 <- predict(del_sqrt,interval="predict")
del_pred1 <- as.data.frame(del_pred1)
View(del_pred1)
# Model 3
del_log <- lm(Delivery.Time~log(Sorting.Time),data=del_time)
summary(del_log)
confint(del_log,level=0.95)
del_pred2 <- predict(del_log,interval="predict")
del_pred2 <- as.data.frame(del_pred2)
View(del_pred2)
# Model 4
del_log1 <- lm(log(Delivery.Time)~Sorting.Time+I(Sorting.Time*Sorting.Time),data=del_time)
summary(del_log1)
confint(del_log1,level=0.95)
del_pred3 <- predict(del_log1,interval="predict")
del_pred3 <- as.data.frame(del_pred3)
View(del_pred3)
# Model 5
del_reg1 <- lm(log(Delivery.Time)~log(Sorting.Time),data=del_time)
summary(del_reg1)
confint(del_reg1,level=0.95)
del_pred4 <- predict(del_reg1,interval="predict")
del_pred4 <- as.data.frame(del_pred4)
View(del_pred4)
# Model 6
del_reg2 <- lm(sqrt(Delivery.Time)~sqrt(Sorting.Time),data=del_time)
summary(del_reg2)
confint(del_reg2,level=0.95)
del_pred5 <- predict(del_reg2,interval="predict")
del_pred5 <- as.data.frame(del_pred5)
View(del_pred5)
# Model 5 has highest R squared value of 0.7752 which is suitable for Delivery time prediction


#Employee Data
emp_data <- read.csv(file.choose())
View(emp_data)
#EDA
summary(emp_data)
hist(Salary_hike)
hist(Churn_out_rate)
qqnorm(Salary_hike)
qqline(Salary_hike)
qqnorm(Churn_out_rate)
qqline(Churn_out_rate)
#Scatter Plot
plot(Salary_hike,Churn_out_rate,main="Scatter Plot")
#Linear Regression Model
# Model 1
emp_reg <- lm(Churn_out_rate~Salary_hike,data=emp_data)
summary(emp_reg)
confint(emp_reg,level=0.95)
emp_pred <-predict(emp_reg,interval="predict") 
emp_pred <- as.data.frame(emp_pred)
View(emp_pred)
# Transform the variables to get better R squared value for better model
# Model 2
emp_sqrt <- lm(Churn_out_rate~sqrt(Salary_hike),data=emp_data)
summary(emp_sqrt)
confint(emp_sqrt,level=0.95)
emp_pred1 <-predict(emp_sqrt,interval="predict") 
emp_pred1 <- as.data.frame(emp_pred1)
View(emp_pred1)
# Model 3
emp_log <- lm(Churn_out_rate~log(Salary_hike),data=emp_data)
summary(emp_log)
confint(emp_log,level=0.95)
emp_pred2 <-predict(emp_log,interval="predict") 
emp_pred2 <- as.data.frame(emp_pred2)
View(emp_pred2)
# Model 4
emp_log1 <- lm(log(Churn_out_rate)~Salary_hike+I(Salary_hike*Salary_hike),data=emp_data)
summary(emp_log1)
confint(emp_log1,level=0.95)
emp_pred3 <-predict(emp_log1,interval="predict") 
emp_pred3 <- as.data.frame(emp_pred3)
View(emp_pred3)
# Model 4 has highest R squared value of 0.9836 which is suitable for Churn_out_rate prediction


#Salary Data
sal_data <- read.csv(file.choose())
View(sal_data)
#EDA
summary(sal_data)
attach(sal_data)
hist(Salary)
hist(YearsExperience)
qqnorm(Salary)
qqline(Salary)
qqnorm(YearsExperience)
qqline(YearsExperience)
dotplot(sal_data$Salary,main="Salary")
#Scatter Plot
plot(Salary,YearsExperience,main="Scatter Plot")
#Linear Regression Model#
#Model1
sal_reg <- lm(Salary~YearsExperience,data=sal_data)
summary(sal_reg)
confint(sal_reg,level=0.95)
sal_pred <- predict(sal_reg,interval="predict")
sal_pred <- as.data.frame(sal_pred)
sal_pred
View(sal_pred)

# Transform the variables to get better R squared value for better model
# Model 2
reg_sqrt <- lm(Salary~sqrt(YearsExperience),data=sal_data)
summary(reg_sqrt)
confint(reg_sqrt,level=0.95)
sal_pred1 <- predict(reg_sqrt,interval="predict")
sal_pred1 <- as.data.frame(sal_pred1)
View(sal_pred1)
# Model 3
reg_log <- lm(log(Salary)~YearsExperience,data=sal_data)
summary(reg_log)
confint(reg_log,level=0.95)
sal_pred2 <- predict(reg_log,interval="predict")
sal_pred2 <- as.data.frame(sal_pred2)
View(sal_pred2)
# Model 4
reg1 <- lm(log(Salary)~YearsExperience+I(YearsExperience*YearsExperience),data=sal_data)
summary(reg1)
confint(reg1,level=0.95)
sal_pred3 <- predict(reg1,interval="predict")
sal_pred3 <- as.data.frame(sal_pred3)
View(sal_pred3)
# Model 5
reg2 <- lm(log(Salary)~log(YearsExperience),data=sal_data)
summary((reg2))
confint(reg2,level=0.95)
sal_pred4 <- predict(reg2,interval="predict")
sal_pred4 <- as.data.frame(sal_pred4)
View(sal_pred4)
# Model 6
reg3 <- lm(sqrt(Salary)~YearsExperience,data=sal_data)
summary(reg3)
confint(reg4,level=0.95)
sal_pred5 <- predict(reg3,interval="predict")
sal_pred5 <- as.data.frame(sal_pred5)
View(sal_pred5)
# Model 1 has highest R squared value of 0.957 which is suitable for Salary_Hike prediction