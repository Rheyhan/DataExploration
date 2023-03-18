#Sebaran dan violin
curve(df(x, df1 = 50, df2 = 20), 0, 3.5,
      lwd = 4, col = warna)
  #Bewarna isi
warna<-rgb(1,0,0)
polygon(curve(df(x,50, 20), 0, 3.5, lwd=4),
        col=warna, border=warna)

#histo with rand
  #runif -> uniform
  #rnorm -> normal
  #rchisq -> ki square
  #distribusi f
set.seet(42)
hist(rf(n=100, df1=50,df2= 20), breaks=10)

hist(rf(n=100, df1=50,df2= 20), breaks=20, freq=F)
curve(df(x, df1 = 50, df2 = 20), from = 0, to = 3.5, add = T,
      lwd = 4, col = "blue")

  #penduga naive
data <- c(21, 23, 25, 27, 27, 28, 28, 28, 28,
          29, 29, 30, 30, 31, 33, 33, 33, 34, 36, 38)
h = 2
n = length(data)
x <- seq(20, 40, by=1)
fx <- NULL
for (i in 1:length(x)){          #rumus penduga naive
  fx[i] = sum(ifelse(abs(data - x[i]) < h ,
                     1, 0)) / (2*h*n)
}
hist(data, freq=F, breaks=20, col= "coral")
lines(x, fx, col="blue", lwd=2)

#Density penduga kernel
library(readxl)
df<- read_xlsx("D:/Kuliah/!yes/R/AED/dummydf/data.xlsx")
str(df)
par(mfrow = c(1, 1))
kepekatan <- density(df$percollege, bw=3, kernel= "epanechnikov")
hist(df$percollege, freq=FALSE, breaks=15,
     col="skyblue1", main="", xlab="Area")
lines(kepekatan, col="blue", lwd=1, main="", ylim=c(0, 1))

kepekatan <- density(df$percollege, bw=3, kernel= "gaussian")
hist(df$percollege, freq=FALSE, breaks=15,
     col="skyblue1", main="", xlab="Area")
lines(kepekatan, col="blue", lwd=1, main="", ylim=c(0, 1))

par(mfrow = c(2, 3)) #multiplot dalam satu gambar

plot(density(df$percollege, kernel = "gaussian", bw = .2), lwd = 2, type = "l",
     main = "gaussian")
plot(density(df$percollege, kernel = "epanechnikov", bw = .2), lwd = 2, type = "l",
     main = "epanechnikov")
plot(density(df$percollege, kernel = "triangular", bw = .2), lwd = 2, type = "l",
     main = "triangular")
plot(density(df$percollege, kernel = "biweight", bw = .2), lwd = 2, type = "l",
     main = "biweight")
plot(density(df$percollege, kernel = "cosine", bw = .2), lwd = 2, type = "l",
     main = "cosine")
plot(density(df$percollege, kernel = "optcosine", bw = .2), lwd = 2, type = "l",
     main = "optcosine")

## Data sampel
par(mfrow=c(1,1))
library("ggplot2")
vec1 <- midwest$percbelowpoverty
hist(vec1)

par(mfrow = c(1, 2))
plot(density(vec1, bw = 0.2), col = "blue", lwd = 2)
plot(density(vec1, bw = 5), col = "red", lwd = 2)

par(mfrow = c(1, 1))
plot(density(vec1, bw = .2), type = "l", col = "grey50", lwd = 2)
lines(density(vec1, bw = 1), col = "blue", lwd = 2)
lines(density(vec1, bw = 5), col = "red", lwd = 2)

legend("topright", legend = c("bw = 0.2", "bw = 1", "bw =  5"),
       lwd = 2, col = c("grey50", "blue", "red"))

## semakin besar bw maka kurva semakin halus namun perlu diperhatikan karena informasi penting 
## dalam data dapat hilang

hist(vec1, freq = F, breaks = 50, border = "white", col = rgb(.95, .95, 0, .6))

lines(density(vec1, bw = .2), col = "grey50", lwd = 2)
lines(density(vec1, bw = 1), col = "blue", lwd = 2)
lines(density(vec1, bw = 5), col = "red", lwd = 2)

legend("topright", legend = c("bw = 0.2", "bw =  1", "bw =  5"),
       lwd = 2, col = c("grey50", "blue", "red"))

##
plot(density(vec1, bw = 2), col = "red", type = "l")
polygon(density(vec1, bw = 2), col = "red", border = "red")

curve(dchisq(x, df = mean(vec1)), from = 0, to = 50, add = T,
      lwd = 2, col = "red")

legend("topright", legend = c("Penduga Densitas Kernel", "Chi-Squared"),
       col = c("black", "red"), lwd = 2)

## Uji Formal
set.seed(42)
kSamples::ad.test(vec1, rchisq(n = length(vec1), df = mean(vec1)))

# VIOLIN PLOT
data(midwest)
df<-midwest

ggplot(df, aes(state, percollege)) + 
  geom_violin(alpha = 0.2)

ggplot(df, aes(state, percollege)) + 
  geom_boxplot() + 
  geom_violin(alpha = 0.2)

ggplot(df,aes(state,percollege))+
  geom_violin()+
  geom_boxplot(width=.1,fill="black",outlier.colour=NA)+
  stat_summary(fun.y=median,geom="point",fill="blue",shape=21,size=2.5)

ggplot(df, aes(state, percollege)) + 
  geom_violin(draw_quantiles = 0.5) #kuantil 2

ggplot(df, aes(state, percollege)) + 
  geom_violin(aes(col = state), fill = NA, draw_quantiles = c(0.25,0.5,0.75))  +
  # kuantil 1,2, dan 3
  labs(title = "Persentase Penduduk Berendidikan Perguruan Tinggi",
       subtitle = "Berdasarkan Negara Bagian",
       caption = "Data obtained from the ggplot2 package")

ggplot(df, aes(state, percollege)) + 
  geom_violin(aes(fill = state)) +
  scale_fill_viridis_d(option = "B") + 
  labs(title = "Persentase Penduduk Berendidikan Perguruan Tinggi",
       subtitle = "Berdasarkan Negara Bagian",
       caption = "Data obtained from the ggplot2 package")

ggplot(df, aes(state, percollege))+
  geom_violin(fill='lightblue',alpha=0.5)+
  geom_jitter(position = position_jitter(width = 0.2), col="red", size=1.5)

ggplot(df, aes(state, percollege))+
  geom_violin(fill='lightblue', col="darkred",alpha=0.5, linetype="dotted")
