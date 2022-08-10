#03.05.2018
#R Vize Ödevi


#Deðiþken atama
isgucu$lucret=log(isgucu$ucret)
isgucu$lkidem=log(isgucu$kidem)
isgucu$ltecrube=log(isgucu$tecrube)
isgucu$legitim=log(isgucu$egitim)

ucret<-isgucu$ucret
egitim<-isgucu$egitim
kadin<-isgucu$kadin
kidem <-isgucu$kidem
Tecrube<-isgucu$tecrube


#Logaritma alýrken 0 Deðerinin oluþturduðu problem

isgucu$legitim[isgucu$egitim==0]<--5
isgucu$lkidem[isgucu$kidem==0]<--5
isgucu$ltecrube[isgucu$tecrube==0]<--5

#yeni veri seti olusturma

data_ucr<-data.frame(isgucu$kadin,isgucu$lucret,isgucu$lkidem,isgucu$ltecrube,isgucu$legitim)

#Grafik oluþturma

plot(ucret~kadin, data=isgucu, col="red",mfrow=c(0,1)))
abline(lm(ucret~kadin))
boxplot(ucret~kadin,data=isgucu , main="Cinsiyet-Ücret Grafiði",col="Red")

#Ýnteraktif Grafik Kullanmak için;
#Ggplot2 ve plotly Paketi aktif olmalý
my_graph=ggplot(data_ucr, aes(x=isgucu$lucret, isgucu$legitim)) + geom_point(shape=1)                     
ggplotly(my_graph)                     

#Scatterplot
pairs(~isgucu$lucret+isgucu$legitim+isgucu$lkidem+isgucu$ltecrube, data = isgucu, main="Ýþgücü Ücret-Egitim-Kýdem-Tecrübe Grafiði")


#Corrplot Grafiði
m<-cor(data_ucr)
corrplot.mixed(m)

#degisken isimlerinin duzenlenmesi

names(data_ucr)[1]<- paste ("kadin")
names(data_ucr)[2]<- paste ("lucr")
names(data_ucr)[3]<- paste ("lkid")      
names(data_ucr)[4]<- paste ("ltec")
names(data_ucr)[5]<- paste ("legi")

#logaritma ile regresyon

ucr_m1<- lm(lucr~legi+lkid+ltec, data=data_ucr)
summary(ucr_m1)
ucr
data_ucr.k<- subset(data_ucr, kadin==1)
ucr_m2<-lm(lucr~legi+lkid+ltec, data=data_ucr.k)
summary(ucr_m2)

data_ucr.e<- subset(data_ucr, kadin==0)
ucr_m3<-lm(lucr~legi+lkid+ltec, data=data_ucr.e)
summary(ucr_m3)

e<-cbind(Toplam=coef(ucr_m1),kadýn=coef(ucr_m2),erkek=coef(ucr_m3))
model_katsayilari<-cbind(Toplam=coef(ucr_m1),kadýn=coef(ucr_m2),erkek=coef(ucr_m3))
model_katsayilari



ucr_m4<-lm(lucr~legi+lkid+ltec+kadin, data=data_ucr)
summary(ucr_m4)

ucr_m5<-lm(lucr~I(legi^2)+I(ltec^2)+legi+lkid+ltec+kadin, data=data_ucr)
summary(ucr_m5)
