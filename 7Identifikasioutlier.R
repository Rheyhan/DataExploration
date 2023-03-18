#deteksi outlier
par(mfrow=c(1,1))

#Histogram, scatterplot, dan boxplot
#Interquartile Range (IQR)
#Mean dan standar deviasi
#Median dan Median Absolute Deviation (MAD)
#Dixon's Q Test
#Grubb's test
#Rosner's Test
#Chi-squared test for outlier

x<-c(10,4,6,8,9,8,7,6,12,14,11,9,8,4,5,10,14,12,15,7,10,14,24,28)
par(mfrow=c(2,2))
hist(x)
boxplot(x)
plot(x)
qqnorm(x)

  #misal mendeteksi outlier menggunakan histo
library("ggplot2")
df<-mpg
par(mfrow=c(1,1))
hist(df$hwy, breaks=40)
  #modifikasi
ggplot(df,aes(df$hwy))+geom_histogram()

  #outlier menggunakan boxplot
boxplot(df$hwy)
  #modifikasi
ggplot(df,aes(df$hwy))+geom_boxplot()+coord_flip()

#IQR I = (Q_1 -1.5IQR ; Q_3 +1.5IQR)
maudicari<-df$hwy
lower_bound <- 18 - 1.5 * maudicari
upper_bound <- 27 + 1.5 * maudicari
outlier_iqr <- which(df$hwy < lower_bound | df$hwy > upper_bound)
outlier_iqr


# Mean dan Standar deviasi
x = c(10,4,6,8,9,8,7,6,12,14,11,9,8,4,5,10,14,12,15,7,10,14,24,28)
mean = mean(x)
std = sd(x)

Tmin = mean-(3*std)
Tmax = mean+(3*std)

x[which(x < Tmin | x > Tmax)]

#Median dan Median Absolute Deviation (MAD)
x = c(10,4,6,8,9,8,7,6,12,14,11,9,8,4,5,10,14,12,15,7,10,14,24,28)
med = median (x)
abs_dev = abs(x-med)
mad = 1.4826*median(abs_dev)

Tmin = med-(3*mad)
Tmax = med+(3*mad)
x[which(x < Tmin | x > Tmax)]

# Dixon's Q Test
library(outliers)
x = c(10,4,6,8,9,8,7,6,12,14,11,9,8,4,5,10,14,12,15,7,10,14,24,28)
dixon.test(x)

# Grubb's Test
x = c(10,4,6,8,9,8,7,6,12,14,11,9,8,4,5,10,14,12,15,7,10,14,24,28)
grubbs.test(x)

# Rosner's test  
library(EnvStats)
rosnerTest(x, k = 2)$all.stats

# Chi-squared test for outliers
chisq.out.test(x)

# Adjusted Boxplot
library(robustbase)
par(mfrow=c(1,2))
boxplot(df$hwy, main = "Original Boxplot")
adjbox(df$hwy, main = "Adjusted Boxplot")

# Sequential Fences
