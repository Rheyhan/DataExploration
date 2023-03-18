#sorting dataframe
library("ggplot2")
df<-read.csv("D:/Kuliah/!yes/R/AED/dummydf/bayi.csv",)

View(df) #view dataframe keseluruhan tab baru
str(df) #lihat ringkasan daru df
summary(df) #kesimpulan dari df
head(df) #melihat 6 baris data pertama

  #Simple sorting
df[1,]
df[1:5,]
df[1:5,3]
df[c(1,5,7),2:3]
df[,-1] #Show tanpa kolom 1
df[c(1,5), -1] #Show baris 1 dan 5 tanpa kolom 1

  #colnames func
colnames(df)      #Show nama kolom
colnames(df) <- c("Angka", "gede", "Tinggi") #Ganti nama
colnames(df)[c(1,2)]<-c("Angka", "gede") #ganti nama 1,2 saja
colnames(df)[which(colnames(df)=="Angka")] <- "Nomor" #mirip kaya ubah nama di pandas
colnames(df)

  #Dim func
dim(df) #dimensi data i x j
dim(df)[1] #Jumlah kolom
nrow(df)
dim(df)[2] #Jumlah baris
ncol(df)

  #Duplication
arr1<-df["Tinggi"] #Duplikasi satu kolom
arr2<-df$Tinggi #sama kaya atas

z<-as.data.frame(df)
z==df

  #rbind cbing
dummy1<-c("nasgor","Bakso","Susu")
dummy2<-c("Cilok","Cilor","Sate")
rbind(dummy1,dummy2) #Merge dummy1 dan dummy 2 baris
initest<-cbind(dummy1,dummy2) #Merge dummy1 dan dummy 2 kolom
#Serupa
initest<-as.data.frame(initest);initest
data.frame(dummy1,dummy2)

  #Membuat kolom baru
initest["Inidummy3"]<-c("Seblak","Apayaa","Mie5ribuan");initest


  #Simple plot dan hist
df<-df[,-1]
hist(df$Tinggi)
plot(df$gede, df$Tinggi,
     main="Inijudul", xlab="IninamaX",
     ylab="IninamaY")