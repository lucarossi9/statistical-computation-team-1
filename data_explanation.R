library(readr)
library(car)
library(ggplot2)
library(hrbrthemes)
#library(rstanarm)
data <- read_delim("student-por.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
#View(student_por)

# Removing data with grade 0 at one of the semesters
data <- subset(data, data$G1 > 2 & data$G2 > 1 & data$G3 > 1)

# Preprocessing
data$sex<-as.factor(data$sex)
data$address<-as.factor(data$address)
data$famsize<-as.factor(data$famsize)
data$Pstatus<-as.factor(data$Pstatus)
data$Mjob<-as.factor(data$Mjob)
data$Fjob<-as.factor(data$Fjob)
data$reason<-as.factor(data$reason)
data$guardian<-as.factor(data$guardian)
data$schoolsup<-as.factor(data$schoolsup)
data$famsup<-as.factor(data$famsup)
data$paid<-as.factor(data$paid)
data$nursery<-as.factor(data$nursery)
data$higher<-as.factor(data$higher)
data$internet<-as.factor(data$internet)
data$romantic<-as.factor(data$romantic)
data$school <- as.factor(data$school)

# Take a look at the data
summary(data$G3)
summary(data)
ggplot(data, aes(x=G3)) + 
  geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
boxplot(data$G3, ylab = "Grade")
scatterplot(data$G2,data$G3,smooth=F,box=F)
scatterplot(data$G1,data$G3,smooth=F,box=F)
scatterplot(data$absences,data$G3,smooth=F,box=F)
ggplot(data, aes(x=G3, color=sex))  +
  geom_histogram(fill="white", binwidth=1, alpha=0.5, position="identity")
ggplot(data, aes(x=G3, group=sex, fill=sex)) +
  geom_density(adjust=1.5, alpha=.4)
ggplot(data, aes(x = sex, y = G3)) + geom_boxplot()
ggplot(data, aes(x=G3, group=higher, fill=higher)) +
  geom_density(adjust=1.5, alpha=.4)
ggplot(data, aes(x = higher, y = G3)) + geom_boxplot()
ggplot(data, aes(x=absences, y=G3)) + 
  geom_point()+
  geom_smooth(method=lm)
ggplot(data, aes(x=studytime, y=G3)) + 
  geom_point()+
  geom_smooth(method=lm)
ggplot(data, aes(x = G1, y = G3)) + geom_point()
ggplot(data, aes(x = G2, y = G3)) + geom_point()
ggplot(data, aes(x = sex, y = G3)) + geom_boxplot()
ggplot(data, aes(x = G1,y = G3, color = sex)) + geom_point()
ggplot(data, aes(x = address, y = G3)) + geom_boxplot()
ggplot(data, aes(x = Pstatus, y = G3)) + geom_boxplot()
ggplot(data, aes(x = G3)) + 
  geom_histogram(aes(fill = Mjob)) +
  facet_wrap(~Mjob)
ggplot(data, aes(x=G3, group=Mjob, fill=Mjob)) +
  geom_density(adjust=1.5) +
  facet_wrap(~Mjob)
ggplot(data, aes(studytime, G3, color=studytime)) +
  geom_jitter(height = 0)
ggplot(data, aes(x = Pstatus, y = G3)) + geom_boxplot()
ggplot(data, aes(Medu, G3, color=Medu)) +
  geom_jitter(height = 0)
ggplot(data, aes(x=studytime, y=G3)) + 
  geom_point()+
  geom_smooth(method=lm)+geom_jitter()
ggplot(data, aes(x=as.factor(Medu), y=G3)) + 
  geom_violin()+geom_dotplot(binaxis='y', stackdir='center', dotsize=1)

# linear model
M1<-lm(G3~.,data)
print(summary(M1))
# (Perform backward variable selction here)
# Model with remaining variables
M1 <- lm(G3 ~ G2+G1+age+failures+higher+schoolsup+traveltime+absences+school+sex, data)
print(summary(M1))
Anova(M1)
# Test if we need other variables apart from the previous grades
M2 <- lm(G3~G2+G1, data)
print(summary(M2))
anova(M1, M2)
#Multicolinearity
vif(M1)
# remove G1
M1 <- lm(G3 ~ G2+age+failures+higher+schoolsup+school+sex, data)
vif(M1)
print(summary(M1))
library(xtable)
print(xtable(summary(M1), type = "latex"), file = "taules.tex")
# Performance of the model
MSE <- mean(summary(M1)$residuals^2)
print(MSE)
MAE <- mean(abs(summary(M1)$residuals))
print(MAE)

#Diagnostic
plot(predict(M1),resid(M1), xlab = "Fitted", ylab="Residuals") 
plot(rstudent(M1))
plot(cooks.distance(M1), ylim=c(0.00,0.05), cex=.75) 

#Diagnostics with ggplot
p1<-ggplot(M1, aes(.fitted, .resid))+geom_point()
p1<-p1+geom_hline(yintercept=0, col="red", linetype="dashed")
p1<-p1+xlab("Fitted values")+ylab("Residuals")
p1
p2<-ggplot(M1, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)+xlim(-2.5, 2.5)+ylim(-2.5, 2.5)
p2<-p2+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
p2<-p2+theme_bw()
p2
ggplot(M1, aes(x=fitted(M1),  y=rstudent(M1))) + 
  geom_hline(yintercept=0, col="red", linetype="dashed") +geom_point() + xlab("Fitted Values") + 
  ylab("Studentized Residuals")
p4<-ggplot(M1, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")+ylim(0.00,0.05)
p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
p4<-p4+theme_bw()
p4
### Trying to predict without the previous grades
M3<-lm(G1~sex+age+address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+schoolsup+famsup+paid+activities+nursery+higher+internet+romantic+famrel+freetime+goout+Dalc+Walc+health+absences,data)
print(summary(M3))
vif(M3)
anova(M3)
M3<-lm(G1~sex+age+address+Medu+Fedu+Mjob+Fjob+reason+guardian+studytime+failures+schoolsup+famsup+activities+higher+romantic+famrel+freetime+goout+Dalc+health+absences,data)
print(summary(M3))
anova(M3)
M3<-lm(G1~sex+address+Medu+Fjob+reason+guardian+studytime+failures+schoolsup+higher+absences,data)
print(summary(M3))
anova(M3)
MSE <- mean(summary(M3)$residuals^2)
print(MSE)
MAE <- mean(abs(summary(M3)$residuals))
print(MAE)
plot(predict(M3),resid(M3)) 
plot(rstudent(M3))
plot(cooks.distance(M3), ylim=c(0.00,0.05), cex=.75) 

### Generalized linear models 
# data2 - dataset for classifications
data2 <- data
data2$G3[data2$G3 < 10] <- 0
data2$G3[data2$G3 >= 10] <- 1
library(dplyr)
data.train <- sample_frac(data2, 0.7)
train_index <- as.numeric(rownames(data.train))
data.test <- data2[-train_index, ]
#data <- data[-c(31:32)]
library(party)
cf1 <- cforest(G3~., data.train, control=cforest_unbiased(mtry=2,ntree=50))
varimp(cf1)
#data.train <- data.train[-c(3, 5, 6, 10, 11, 12, 13, 17, 18, 19, 20, 22, 23, 24, 25, 26, 29, 30)]
M4<-glm(G3~., data.train, family = binomial(link = "logit"))
print(summary(M4))
print(confint(M4, level = 0.9))
anova(M4)
p <- predict(M4, data.test)
p <- exp(p)/(1+exp(p))
p <- 1*(p > 0.5)
accuracy <- (sum(data.test$G3 & p)+sum(!data.test$G3 & !p))/190
cat("Accuracy: ", accuracy)
# Roc curve to check the performance of the classifier
library(caret)
library(pROC)
confusionMatrix(factor(p), factor(data.test$G3))
roc <- roc(p, data.test$G3)
plot(roc)
plot(roc, add=TRUE, col="blue")
legend("bottomright", legend=c("Empirical", "Smoothed"),
       col=c(par("fg"), "blue"), lwd=2)
plot(roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="lightblue", print.thres=TRUE)

# giving more importance to detect failed students
library(survey)
freq <- sum(data.train$G3[data.train$G3 > 0])/442
freq <- freq/(1-freq)
# freq <- 7 
data.train$w <- (1-data.train$G3)*freq + data.train$G3
design.ps <- svydesign(ids=~1, weights=~w, data=data.train)
M5 <- svyglm(G3 ~sex+school+age+address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+guardian+studytime+failures+schoolsup+famsup+paid+activities+nursery+higher+internet+romantic+famrel+freetime+goout+Dalc+Walc+health+absences+G1+G2, design=design.ps, family = binomial(link = "logit"))
p <- predict(M5, data.test)
p <- exp(p)/(1+exp(p))
p <- 1*(p > 0.5)
accuracy <- (sum(data.test$G3 & p)+sum(!data.test$G3 & !p))/190
cat("Accuracy: ", accuracy)
library(caret)
library(pROC)
confusionMatrix(factor(p), factor(data.test$G3))
roc <- roc(p, data.test$G3)
plot(roc)
plot(roc, add=TRUE, col="blue")
legend("bottomright", legend=c("Empirical", "Smoothed"),
       col=c(par("fg"), "blue"), lwd=2)
plot(roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="lightblue", print.thres=TRUE)


# classification improving the grade or not 
data3 <- data
data3$G3[data3$G3-data3$G2 <= 0] <- 0
data3$G3[data3$G3-data3$G2 > 0] <- 1
#data3$DG <- data3$G2-data3$G1
library(dplyr)
data.train <- sample_frac(data3, 0.7)
train_index <- as.numeric(rownames(data.train))
data.test <- data3[-train_index, ]

library(party)
cf1 <- cforest(G3~., data.train, control=cforest_unbiased(mtry=2,ntree=50))
varimp(cf1)
data.train <- data.train[c(3, 6, 9, 10, 11, 12, 13, 14, 16, 17, 24, 25, 27, 28 , 31, 32)]
M4<-glm(G3~., data.train, family = binomial(link = "logit"))
print(summary(M4))
print(confint(M4, level = 0.9))
anova(M4)
p <- predict(M4, data.test)
p <- exp(p)/(1+exp(p))
p <- 1*(p > 0.5)
accuracy <- (sum(data.test$G3 & p)+sum(!data.test$G3 & !p))/190
cat("Accuracy: ", accuracy)
library(caret)
library(pROC)
confusionMatrix(factor(p), factor(data.test$G3))
roc <- roc(p, data.test$G3)
plot(roc)
plot(roc, add=TRUE, col="blue")
legend("bottomright", legend=c("Empirical", "Smoothed"),
       col=c(par("fg"), "blue"), lwd=2)
plot(roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="lightblue", print.thres=TRUE)


## Bayesian linear regression
library(rstanarm)
library(bayestestR)
library(insight)
library(rstan)

# Define model as in the frequentist case
model <- stan_glm(G3 ~ failures+higher+Medu+studytime+Fedu+absences+G2, data = data)
# sample from the posterior distribution of the parameters
# The default values are 4 chains with 2000 samples each one and removing the first half
posteriors <- insight::get_parameters(model)
head(posteriors)
stan_trace(model, pars=c("G2"))
# plot the posterior distributions
ggplot(posteriors, aes(x = failures)) +
  geom_density(fill = "red", alpha = 0.5)+ geom_vline(xintercept = 0, size=1.5)
ggplot(posteriors, aes(x = Medu)) +
  geom_density(fill = "yellow", alpha = 0.5) + geom_vline(xintercept = 0, size=1.5)
ggplot(posteriors, aes(x = Fedu)) +
  geom_density(fill = "orange", alpha = 0.5) + geom_vline(xintercept = 0, size=1.5)  
ggplot(posteriors, aes(x = studytime)) +
  geom_density(fill = "green", alpha = 0.5) + geom_vline(xintercept = 0, size=1.5)
ggplot(posteriors, aes(x = absences)) +
  geom_density(fill = "grey", alpha = 0.5) + geom_vline(xintercept = 0, size=1.5)
ggplot(posteriors, aes(x = G2)) +
  geom_density(fill = "blue", alpha = 0.5)

# compute mean/median/MAP of the posterior
cat("mean", mean(posteriors$failures))
cat("median", median(posteriors$failures))
cat("mean", map_estimate(posteriors$failures))

# uncertainty in the parameters
hdi(posteriors$failures, ci = 0.9)

# Probability of direction
p_direction(posteriors$failures)
p_direction(posteriors$Medu)
p_direction(posteriors$Fedu)
p_direction(posteriors$studytime)
p_direction(posteriors$G2)
p_direction(posteriors$absences)

# general description
library("logspline")
describe_posterior(model, test = c("p_direction", "rope", "bayesfactor"))

# individual effect of each variable
data_b <- data[c(7, 8, 14, 15, 21, 30, 32, 33)]
fit = posterior_predict(model, newdata = data_b)
resid = resid(model)
fit = fitted(model)
ggplot() + geom_point(data = NULL, aes(y = resid, x = fit))
ggplot() + geom_point(data = NULL, aes(y = data_b$G3, x = round(fit)))

                      