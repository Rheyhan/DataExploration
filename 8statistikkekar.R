#robust statistik
#[Statistik Kekar untuk Nilai Tengah
  #Median
  #α-Trimmed Mean (Rataan terpangkas α)
  #α-Winsorized Mean (Rataan terwinsor α)
  #Midhinge

#Statistik Kekar untuk Ragam
  #Inter-Quartile Range
  #Median Absolute Deviation(MAD)
  #Trimmed Variance (and Standard Deviation)
  #Winsorized Variance (aand Standard Deviation)
library(ggplot2)
df<- ggplot2::mpg$hwy
#nilai tengah
  #median
median(df)

  #trimmed mean
mean(df,trim=0.05)

  #winsorized
test<-c( 30, 33, 40, 45, 46, 28, 50, 67, 80, 72)
winsorizwinsormean <- function(x, probs = c(0.05, 0.95)) {
  xq <- quantile(x, probs = probs)
  x[x < xq[1]] <- xq[1]
  x[x > xq[2]] <- xq[2]
  return(mean(x))
}
winsorizwinsormean(test)

  #Midhinge
Midhinge <- function(dt){
  as.numeric(0.5*(quantile(dt, 0.75) + quantile(dt,0.25)))
}
Midhinge(df)
test<-c(30, 33, 40, 45, 46, 28, 50, 67, 80, 72)
Midhinge(test)
#Untuk ragam
  #IQR (inter quartile range)
JAK <- function(dt){
  as.numeric(quantile(dt, 0.75) - quantile(dt,0.25))
}
JAK(df)

  #Median Absolute Deviation(MAD)
mad(df)
  #Trimmed Variance (and Standard Deviation)
library("chemometrics")
sd(df)
test<-c(30, 33, 40, 45, 46, 28, 50, 67, 80, 72)
sd_trim(df, trim=0)
sd(test)
  #Winsorized Variance (aand Standard Deviation)
library(datawizard)
winsorize(df,threshold=0.2)

#1
ini1<-c(80, 84, 83, 83, 83, 82, 84, 85, 87, 88, 90, 83)
ini1
winsorize(ini1,threshold=0.2)

#ini2
ini2<-c(12,12,12,13,13,14,16,17,18,18,19,19,19,19, 20,20,21,22,22,67)
win