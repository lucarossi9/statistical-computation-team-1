library(readr)
library(car)
library(ggplot2)
library(rstanarm)
data <- read_delim("student-por.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(student_por)

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
ggplot(data, aes(x=G3)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)
boxplot(data$G3, ylab = "Grade")
ggplot(data, aes(x = G1, y = G3)) + geom_point()
ggplot(data, aes(x = G2, y = G3)) + geom_point()
ggplot(data, aes(x = sex, y = G3)) + geom_boxplot()
ggplot(data, aes(x = G1,y = G3, color = sex)) + geom_point()
ggplot(data, aes(x = address, y = G3)) + geom_boxplot()
ggplot(data, aes(x = Pstatus, y = G3)) + geom_boxplot()
ggplot(data, aes(x = G3)) + 
  geom_histogram(aes(fill = Mjob)) +
  facet_wrap(~Mjob)

# linear model
M1<-lm(G3~sex+age+address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+schoolsup+famsup+paid+activities+nursery+higher+internet+romantic+famrel+freetime+goout+Dalc+Walc+health+absences+G1+G2,data)
print(summary(M1))
plot(predict(M1), residuals(M1)) #check homogeneity of variance
plot(rstudent(M1)) # outliers
plot(dffits(M1)) # influential values
influencePlot(M1)

# Multicolinearity
vif(M1) 
# remove G1 to avoid variance inflation
M1<-lm(G3~sex+age+address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+schoolsup+famsup+paid+activities+nursery+higher+internet+romantic+famrel+freetime+goout+Dalc+Walc+health+absences+G2,data)
vif(M1)

# Variable relevance
Anova(M1)
# remove variables that are not relevant for the model
M1 <- lm(G3~age+failures+higher+G2, data)
print(summary(M1))
print(summary(M1)$coefficients)
anova(M1)
# Compare M1 with the model that uses only the grades
M2 <- lm(G3~G2, data)
print(summary(M2))
anova(M1, M2)

# Performance of the model
MSE <- mean(summary(M1)$residuals^2)
print(MSE)
MAE <- mean(abs(summary(M1)$residuals))
print(MAE)
plot(predict(M1),resid(M1)) 
plot(rstudent(M1))
plot(cooks.distance(M1), ylim=c(0.00,0.05), cex=.75) 


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
# I still have to deal with the problem of an imbalanced data set ()
data$G3[data$G3 < 10] <- 0
data$G3[data$G3 >= 10] <- 1
#data <- data[-c(31:32)]
library(party)
cf1 <- cforest(G3~., data, control=cforest_unbiased(mtry=2,ntree=50))
varimp(cf1)
data <- data[-c(3, 5, 6, 10, 11, 12, 13, 17, 18, 19, 20, 22, 23, 24, 25, 26, 29, 30)]
M4<-glm(G3~., data, family = binomial(link = "logit"))
print(summary(M4))
print(confint(M4, level = 0.9))
anova(M4)
p <- predict(M4)
p <- exp(p)/(1+exp(p))
p <- 1*(p > 0.5)
accuracy <- (sum(data$G3 & p)+sum(!data$G3 & !p))/632
cat("Accuracy: ", accuracy)

# classification improving the grade or not 

## Want to implement GPs or Bayesian linear regression


  