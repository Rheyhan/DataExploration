#libraries
library(dplyr)

#df import
df<-read.csv("D:/Kuliah/!yes/R/AED/dummydf/mostvaluableplayers.csv")
new.data <- filter(df, position=="Central Midfield")
new.data <- filter(new.data, nationality=="Spain")
new.data <- new.data[,-c(1:3)]
new.data <- new.data[,-c(2:3)]
colnames(new.data) <- c("x","y")

#Mengurutkan data 
new.data <- new.data[order(new.data$x),]

#Menghitung banyak titik anggota jendela
q <- 0.5
n <- 10
nq <- n*q
nq

# Titik Focal 1
#Mencari nilai jarak(d), z, dan bobot(w)---#
#Jarak(d) xi-xp
d1 = new.data$x-new.data$x[1]
d1
#Nilai z
h1 = max(d1[1:5])
h1
z1 = d1/h1
z1
#Nilai bobot
w1 = (1-abs(z1)^3)^3
w1
wm1 <- diag(c(1.0000000,0.9538536,0.1932259,0.1932259,0.0000000,0,0,0,0,0))
wm1

#Menentukan nilai y duga
#Weighted Least Square
#Bduga = (X' W X)^(-1) X' W y
y <- as.matrix(new.data$y)
y
x <- matrix(c(rep(1,10),new.data$x), ncol=2, nrow=10, byrow=F) #ncol=k+1, kolom pertama berisi 1, nfrow=banyak baris
x
b.1 <- solve(t(x)%*%wm1%*%x)%*%t(x)%*%wm1%*%y
b.1
#yduga <- b0+b1x
yduga1 = b.1[1]+b.1[2]*new.data$x
yduga1[1]
y1 <- yduga1[1]
y1


### LOWESS / LOESS BUILT-IN SYNTAX IN R ### (use this instead lmao)

### Local Regression ###
#Cars Dataset
data <- cars
colnames(data) = c('x','y')
plot(data$x, data$y,
     main = 'Plot Between Speed and Distances of Cars',
     xlab = 'Speed',
     ylab = 'Distances')

# Making Model Local Regresion for Dataset Cars
loessmod15 <- loess(y~x, data = data, span = 0.5)
loessmod19 <- loess(y~x, data = data, span = 0.9)

#Get Smooth Output
smoothed15 <- predict(loessmod15)
smoothed19 <- predict(loessmod19)

#Plot
lines(smoothed15, x = data$x, col = 'green', lwd = 3)
lines(smoothed19, x = data$x, col = 'blue', lwd = 3)

