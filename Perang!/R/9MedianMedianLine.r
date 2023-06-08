#libraries
library("ggplot2")

#dataframe
df<- midwest
x<- df$percwhite
y<- df$percchildbelowpovert

plot(x, y, col = rgb(1, 0, 0, .5), pch = 16, cex = .75,
     main = "Midwest Dataset",
     xlab = "% of white population", ylab = "% below poverty line")


#Regresi Linear OLS
linReg <- lm(y ~ x)
coef(linReg)

plot(x, y, col = rgb(1, 0, 0, .5), pch = 16, cex = 0.75,
     main = "Midwest Dataset",
     xlab = "% of white population", ylab = "% below poverty line")
abline(linReg, lwd = 2)
text(40, 35, "OLS: y = 38.66 - 0.27X", cex = 0.8)

#Algoritma MML
  #1 Urutkan Data
y <- y[order(x)]  # y dirutkan (sesuai dengan urutan dari x)
x <- sort(x)  # x diurutkan
cbind(x,y)

  #2Bagi menjadi 3 kelompok
n <- round(length(x)/3)
group1 <- 1:n
group2 <- (n + 1):(length(x) - n)
group3 <- (length(x) - n + 1):length(x)

data.frame(min = c(1, min(group1), min(group2), min(group3)),
           max = c(length(x), max(group1), max(group2), max(group3)),
           length = c(length(x), length(group1),
                      length(group2), length(group3)),
           row.names = c("All", "Group1", "Group2", "Group3"))

  #3Cari median X&Y setiap kelompol
x1 <- median(x[group1])
y1 <- median(y[group1])

x2 <- median(x[group2])
y2 <- median(y[group2])

x3 <- median(x[group3])
y3 <- median(y[group3])

data.frame(x = c(x1, x2, x3),
           y = c(y1, y2, y3),
           row.names = c("Group 1", "Group 2", "Group 3"))

plot(x, y, col = rgb(0, 0, 0, .5), pch = 16, cex = .5,
     main = "Garis Resisten MML Akan Berusaha
Menghubungkan Ketiga Titik Median (warna merah)")
points(c(x1, x2, x3), c(y1, y2, y3), col = "red", pch = 16)

  #4Cari rataan untuk setiap ,edoam x dan y
xbar <- mean(c(x1, x2, x3))
ybar <- mean(c(y1, y2, y3))

  #5Cari Intercept dan Slope dari Garis MML
b1 <- (y3 - y1)/(x3 - x1)
b0 <- ybar - b1*xbar

round(data.frame(Intercept = c(coef(linReg)[[1]], b0),
                 Slope = c(coef(linReg)[[2]], b1),
                 row.names = c("OLS", "MML")), 4)

plot(x, y, col = rgb(1, 0, 0, .5), pch = 16, cex = .75,
     main = "Midwest Dataset",
     xlab = "% of white population", ylab = "% below poverty line")

abline(linReg, lwd = 2)  # visualisasi OLS
abline(b0, b1, col = "blue", lwd = 2)  # visualisasi MML
legend("bottomleft", legend = c("OLS", "MML"), col = c("black", "blue"),
       lty = 1)
text(40, 31.8, "y = 38.66 - 0.27X", cex = .8)
text(30, 25, "y = 35.99 - 0.25X", cex = .8, col = "blue")

# Visualisasi OLS dan MML Menggunakan GGPlot
  #geom_smooth() untuk OLS
  #geom_abline() untuk MML

ggplot(data = df, mapping = aes(x = percwhite, y = percbelowpoverty)) +
  geom_point(colour = "red", alpha = .5, size = 1) +
  geom_smooth(method = "lm", formula = y ~ x, mapping = aes(col = "black"),
              size = 1.5, se = F) +
  geom_abline(mapping = aes(intercept = b0, slope = b1, col = "blue"),
              size = 1.5, show.legend = F) +
  scale_colour_manual(name = "",
                      labels = c("OLS", "MML"), 
                      values = c("black", "blue")) +
  theme_bw()