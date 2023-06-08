data1 <- read.delim("clipboard")


#MODEL MKT
model <- lm(Longevity~Gestation,data=data1) 
summary(model)
plot(x=data1$Gestation, y=data1$Longevity,
     main='Scatter Plot Gestation vs Longevity',
     xlab = 'Gestation', 
     ylab='Longevity')
abline(model)


#MEDIAN MEDIAN LINE
library(dplyr)

resistance_line <- function(x,y,data){
  #----Mengurutkan data berdasar nilai x-----
  data <- as.data.frame(data1)
  x <- data1[,2] 
  y <- data1[,3] 
  data <- data.frame(x,y) 
  data <- data[with(data,order(x)),] 
  n<-nrow(data)
  #----Membagi data menjadi 3 bagian---------
  if (n%%3==0) {
    data<-data%>%
      mutate(m=c(rep(1,n/3), 
                 rep(2,n/3), 
                 rep(3,n/3)))
  } else if (n%%3==1) { 
    data<-data%>% 
      mutate(m=c(rep(1,(n-1)/3 ), 
                 rep(2,(n-1)/3+1 ), 
                 rep(3,(n-1)/3 ))
      )
  } else if(n%%3==2){ 
    data<-data%>% 
      mutate(m=c(rep(1,(n+1)/3 ), 
                 rep(2,(n+1)/3-1 ), 
                 rep(3,(n+1)/3 ))
      )
  }
  #-----------Menghitung median dari data--------- 
  med <- data%>% 
    group_by(m)%>% 
    summarise(mx=median(x), 
              my=median(y))%>% 
    as.data.frame()
  
  #------Menghitung nilai dugaan parameter------
  mx<-med$mx 
  my<-med$my 
  xbar<-mean(mx) 
  ybar<-mean(my) 
  b1<-(my[3]-my[1])/(mx[3]-mx[1]) 
  b0<-ybar-(b1*xbar)
  
  #------Menjadikan intercept dan slope dalam satu dataframe-------- 
  d<-data.frame(intercept=b0, slope=b1)
  
  #-------Mengembalikan nilai dari intercept dan slope------- 
  return(d) 
}

d <- resistance_line(x,y,data1)

plot(data1$Gestation ,data1$Longevity, 
     xlab='Gestation', 
     ylab='Longevity', 
     main='Scatter Plot Gestation vs Longevity') 
abline(model,col='red', lwd=2) 
abline(a=d$intercept,b=d$slope,col='blue', lwd=2) 

