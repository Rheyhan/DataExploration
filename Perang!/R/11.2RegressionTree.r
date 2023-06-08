#libraries
library(readxl)
library(rpart)
library(rpart.plot)

#set DF
Jantung <- read_xlsx("D:/Kuliah/!yes/R/AED/dummydf/heart_2020_cleaned.xlsx")

#Training: 80 | Testing: 20
set.seed(123)
n <- round(nrow(Jantung)*0.80)
n
samp=sample(1:nrow(Jantung),n)
data.train = Jantung[samp,]
data.test = Jantung[-samp,]
dim(data.train)
dim(data.test)

#Model BMI and physical correlate with heartdisease
fit <- rpart(HeartDisease~BMI + PhysicalHealth, data = data.train, method = 'class', control=rpart.control(minsplit = 2, cp=0))
fit

#visualization
rpart.plot(fit, extra=4)

#correlate
fit$variable.importance

#predict
jantung.tiba2 <- data.frame(BMI = 31.66, PhysicalHealth = 30)
predict(fit, jantung.tiba2)
