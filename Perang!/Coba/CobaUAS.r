library("ggplot2")
library("SciViews")

setwd("D:/Kuliah/Semester 4/AED/Perang!/Coba")
df<-read.csv("uas AED 2022.csv")

#1
qplot(df$x,df$y) #exponential
model<-lm(y~x, df)
summary(model)
anova(model)
abline(model, lwd = 2)

#2 Materi Anreg transformasi
  #remove amatan=0 || Merusak  algoritma
df<-df[df$y!=0,]
dftrans<-data.frame(x=ln(df$x),
                    y=ln(df$y))
plot(dftrans$x, dftrans$y)
model.trans<-lm(y~x, dftrans)
abline(model.trans, lwd = 2)

#3
summary(model.trans)
anova(model.trans)
predict(model.trans,"Inputwhattopredict!")