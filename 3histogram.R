#Histogram (Pertemuan 2)
#import module and setfile
library("ggplot2")
df<-read.csv2("D:/Kuliah/!yes/R/AED/dummydf/data.csv")
View(df)
head(df)

#Histo
  #Bawaan R
hist(df$percollege) #Frequency
hist(df$percollege, freq=F) #Density

hist(df$percollege, freq=F,
     main="Inijudul", xlab="Inix",
     ylab="Iniy", 
     sub = "dibawah legend X",
     col="RED",
     breaks = seq(min(df$percollege), 
                  max(df$percollege), 
                  length.out = 22))
hist(df$percollege, freq=F,
     main="Inijudul", xlab="Inix",
     ylab="Iniy", 
     sub = "dibawah legend X",
     col="RED",
     breaks = seq(min(df$percollege), 
                  max(df$percollege), 
                  length.out = 22))
    #Buat line 
abline(v=median(df$percollege),col="BLUE",lwd=5) #Median percollege, size 5
abline(v=mean(df$percollege), col="green", lwd=5) #ini mean percollege, size 5

  
  #GGPLOT
ggplot(df, aes(df$percollege)) + 
  geom_histogram()

ggplot(df, aes(df$percollege)) + #pake warna dan line
  geom_histogram(color="Black", fill="Red")+
  geom_vline(aes(xintercept=mean(percollege)),
             color="green", linetype="solid", size=1)

ggplot(df, aes(df$percollege, fill=state))+ #ini histo nunjukin per provinsi
    geom_histogram(color="Black")+
    theme(legend.position="top") #ubah legenda

library("plyr")
    #dibuat kaya tadi tapi ada intersep rataan per provinsi
rataan<-ddply(df, "state", summarise, rata2=mean(percollege)
              
ggplot(df, aes(df$percollege, fill=state))+
  geom_histogram(color="Blue")+
  geom_vline(data=rataan,aes(xintercept=rataan$rata2, color=state),
             linetype="dashed")+
  theme(legend.position="bottom")


    #Dibuat masing-masing (ada 5 gambar histo untuk setiap prov)
ggplot(df, aes(df$percollege))+
  geom_histogram(color="black", fill="white")+
  facet_grid(state ~ .)+ 
  geom_vline(data=rataan, aes(xintercept=rata2, color="red"),
             linetype="dashed")

ggplot(df, aes(x=df$percollege)) +
  geom_histogram(aes(fill=state),color="black") +
  scale_fill_brewer(palette="Set2") +
  facet_wrap( ~ state, ncol=1) +
  xlab("Inix") +
  ylab("iniY")  +
  ggtitle("judul")+
  theme_bw()
  
  #gganimate
library("gganimate")
inihistgg<-ggplot(df,aes(df$percollege))+
  geom_histogram(color="black")

yes=inihistgg+transition_states(state, 5,0.5, wrap=F)+
    view_follow(fixed_x = T)
yes