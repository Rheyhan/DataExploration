#boxplot
library("ggplot2")
df<-read.csv2("D:/Kuliah/!yes/R/AED/dummydf/data.csv")

  #bawaan
boxplot(df$percchildbelowpovert)

    #modifikasi
boxplot(df$percchildbelowpovert,
        horizontal = TRUE,  #coord_flip()
        col="red",          #warna
        main="inijudul",
        ylab="iniy",
        xlab="inix")
library(tidyverse)
    #group (x kategorik, y numerik)
boxplot(data=df, percchildbelowpovert~state,
        ylim=c(0,100)) #tampilkan y 0-100

      #colouring
boxplot(data=df, percchildbelowpovert~state,
        col=c("red","blue","green","black","pink"))

      #rename index
boxplot(percchildbelowpovert ~ inmetro, data = df, ylim = c(0, 100),
        col = c("red", "blue"), 
        xaxt = "n")  #hilang legenda x
axis(1, at = 1:2, labels = c("No", "Yes")) # untuk memodifikasi salah satu sumbu

    #fungsi reorder secara median (sorting)
boxplot(df$percchildbelowpovert ~
          reorder(df$state, df$percchildbelowpovert, FUN = median))
boxplot(data=df, percchildbelowpovert~state) #perbandingan

    #abline dan point text
boxplot(data=df, percchildbelowpovert~state,
        main="gak tau",
        ylab="ini y",
        xlab="ini x",
        ylim=c(0,100))
abline(h = 50, lty = "dashed", col = "grey")
text(x = 4, y = 60 + 7, labels = "Menominee, WI", cex = .725)

  #ggplot
iniboxplot<-ggplot(df, aes(y=percchildbelowpovert))+
  geom_boxplot()

    #tema
iniboxplot+theme_light()
iniboxplot+theme_bw()
iniboxplot+theme_classic()
iniboxplot+theme_minimal()
iniboxplot+theme_void()

  #grouping
inigrouperbox<-ggplot(df, aes(x=as.factor(inmetro), 
                              y=percchildbelowpovert,
                              fill=as.factor(inmetro)))
inigrouperbox+geom_boxplot()
    #warna?

inigrouperbox + geom_boxplot(fill = "orange", color = "brown") + theme_classic()
inigrouperbox + geom_boxplot(fill = c("red", "blue")) + theme_classic()
    #flip

inigrouperbox+geom_boxplot()+coord_flip()

  #Category Sorting 
inisorted<-ggplot(df,aes(x=reorder(state, percchildbelowpovert, median),
                         y=percchildbelowpovert))+geom_boxplot()
inisorted+geom_jitter(alpha=.5) #jitter
