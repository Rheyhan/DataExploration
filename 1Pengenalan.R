#Pengenalan 
#vector
a<-c(1,2)
rep(a,3) #Repetisi a 3 kali mirip kek kali di arr piton
rep(c("a","b","c"),3)
seq(1,10, 2) #mirip range piton
paste("A", 1:9, sep="-") # ["A"+"-"+str([i]) for i in range(1,9)]
  #sorting
angka <- c(2,3,4,1)
angka[2:3]
angka[c(1,3)]  #mirip kaya atas tapi spesifik
angka[angka<2] #tampilkan yang kurang 2
angka[-2]       #show kecuali angka[2]

#Faktor
z<-c("SD","SMP","SMA")
z1<-rep(z,3)
pendidikan <- factor(z1)
pendidikan
factor(pendidikan, c("SD","SMP","SMA"))

#Matrix
inimatrix1<-matrix(seq(9), 3, 3)
inimatrix2<-matrix(c(1,2,3,4), 2, 2)
inimatrix; inimatrix2

dim(inimatrix1) #checksize
length(inimatrix1) #length rowxcol

iimatrixdim<-matrix(seq(16), 4, 4, TRUE, dimnames = list(c("Ini1","Ini2","Ini3","Ini4"), c("Ntah1","Ntah2","Nta3","Ntah4")))
iimatrixdim
initest<-seq(1:9)
initesttapimatrix<-matrix(initest, 3, 3, TRUE)
#Array
iniarray<-array(0, dim=c(3,2))
iniarray

#Dataframe
d1=c("Initest","Iniapaya","Hi Mom!")
d2=c("I like lean!", "Helloworld!:", "KMS")
dummy<-data.frame(d1,d2)
dummy
dummyDF<-data.frame(iniangka=c(1,2,3),
                    inistring=c("inisatu", "inidua","initiga"))
dummyDF                    
str(dummy) #mirip lihat detail pada df
  

  #Add kolom
dummy$x3<-c("gaktau", "taktau", "Koktanyasaya")
dummy
  #Sorting
dummyDF$iniangka #cek baris
dummyDF[,1]     #cek baris
dummyDF[[1]]   #cek baris
dummyDF[,"iniangka"] #cek baris
dummyDF[1,]     #cek kolom
dummy[,-3]    #cek kolom tanpa kolom 3