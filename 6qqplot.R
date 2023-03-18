#qqplot (memeriksa data menyebar normal atau tidak)

par(mfrow=c(2,2))
bilnorm<-rnorm(1000,2,5)
hist(bilnorm)

qqnorm(bilnorm)
qqline(bilnorm, col="blue",lwd=3)

#qqplot uniform
bilunif<-runif(1000,0,1)
hist(bilunif)
qqnorm(bilnorm)
qqline(bilnorm, col="red", lwd=2)

  #mengecek semua bentuk sebaran
qqplot(x=qunif(ppoints(1000), #qunif = fungsi quantil dari uniform
               min = 0,
               max = 1),
       y = bilunif)

#qqplot ki square
iniki<-rchisq(1000,5)
hist(iniki)
qqnorm(iniki)
qqline(iniki,col="green",lwd=3)

#QQ PLOT - Weibull (parameternya shape dan scale)
x = c(3.367, 0.769,0.8, 1, 1.2)
data.weibull = rweibull(5, 3, 2)
qqplot(data.weibull, x)
qqline(data.weibull, col = "red", lwd = 2)
#Jika plotnya terbuka ke atas dapat dipastikan data menjulur ke kanan
#Jika plot terbuka ke bawah data menjulur ke kiri

#Goodness Fits of Test
  #Chi-Square Test
a <- c(1:6)
b <- c(57,56,63,52,48,63) #dalam 340 kali percobaan
exp.dice <- c(1/6,1/6,1/6,1/6,1/6,1/6)
dice.data <- data.frame(a,b,exp.dice)
test <- chisq.test(dice.data$b, p=exp.dice)
test


country <- c("US","Germany","France","Norway","Spain","China")
exp.percent <- c(0.25,0.12,0.18,0.14,0.11,0.20)
medals <- c(33,6,18,15,12,36)

medal.data <- data.frame(country, exp.percent, medals)
  #Chi-Square Test
test <- chisq.test(medal.data$medals, p=exp.percent)
test

  #Kolmogorov-Smirnov Test
ks.test(bilnorm, "pnorm", 18.27, 39.21)
ks.test(bilunif, "punif", 20.2, 0.972)
