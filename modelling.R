######### Reading Data #########
setwd("/Users/Asus/Downloads/")
library(readr)
library(car)

rm(list=ls())
data <- read_csv("with_outliers.csv")
View(data)

# Check the types of the variables in dataset
data <- subset(data, select=-c(...1))
sapply(data, class)
attach(data)

# Change the data types of categorical variables
data$gender = as.factor(gender)
data$is45OrOlder = as.factor(is45OrOlder)
data$isMarried = as.factor(isMarried)
data$hasKids = as.factor(hasKids)
data$termLifeInsurance = as.factor(termLifeInsurance)
data$multipleTermLifePolicies = as.factor(multipleTermLifePolicies)
data$healthInsurance = as.factor(healthInsurance)
data$premiumFrequency = as.factor(premiumFrequency)
data$eStatements = as.factor(eStatements)
data$renewal = as.factor(renewal)
data$rider1 = as.factor(rider1)
data$rider2 = as.factor(rider2)
data$rider3 = as.factor(rider3)
data$rider4 = as.factor(rider4)

sapply(data, class)
attach(data)

######### Data Sampling  #########
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- data[sample, ]
test <- data[!sample, ]

######### Model Fitting  #########
model <- glm(renewal ~.,family=binomial(link='logit'),data=train)
summary(model)
Anova(model, type = "II")

######### Model Evaluation  #########
fitted.results <- predict(model,test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$renewal)
print(paste('Accuracy',1-misClasificError))

######### Remove Insignificant Variables  #########
model2 <- glm(renewal ~ is45OrOlder+termLifeInsurance+multipleTermLifePolicies+healthInsurance+premiumFrequency+
                eStatements+monthlyPremium+totalPremium+rider1+rider2,family=binomial(link='logit'),data=data)
summary(model2)
Anova(model2, type = "II")

fitted.results <- predict(model2,test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$renewal)
print(paste('Accuracy',1-misClasificError))

# Conclusion:
# Important features to determine insurance renewal are:
# is45OrOlder, termLifeInsurance, multipleTermLifePolicies, healthInsurance, premiumFrequency, 
# eStatements, monthlyPremium, totalPremium, rider1, rider2
# as they have p-values < 0.05 hence we reject the null hypothesis that the coefficient estimates = 0
# at the 5% significance level (Note: Removing outliers gives the same features)