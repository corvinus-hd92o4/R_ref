labels(Stat_adatok)

attach(Stat_adatok)

#leíró statisztikák

#általános adatok
summary(Stat_adatok)


#A pest vagy buda atalakitasa
Stat_adatok$Főváros<-factor(Stat_adatok$Főváros, levels=0:1, labels=c("Videk","Fovaros"))

#hany fovarosi es hany videki lakas
table(Stat_adatok$Főváros)

#ugyan ez szazalekosan
prop.table(table(Stat_adatok$Főváros))

#oszlopdiagram
barplot(table(Stat_adatok$Főváros))
barplot(prop.table(table(Stat_adatok$Főváros)))

#ugyan ezek szobaszamra
table(Stat_adatok$Szobaszám)

barplot(table(Stat_adatok$Szobaszám),xlab="Szobaszám")


#mennyiségi változók (nm és ár) logaritmizalasa, igy szep normalis eloszlasokat kaptam
hist(Stat_adatok$`Ár (Ft)`,50)

Stat_adatok$log_ar<-log(Stat_adatok$`Ár (Ft)`)
hist(Stat_adatok$log_ar,20)

hist(Stat_adatok$Négyzetméter)

Stat_adatok$log_nm<-log(Stat_adatok$Négyzetméter)
hist(Stat_adatok$log_nm)

#adatok az árról és a nm ről
summary(`Ár (Ft)`)
summary(Négyzetméter)


range(`Ár (Ft)`)
diff(range(`Ár (Ft)`)) #ez az ár változó terjedelme

#szórás, variancia
sd(`Ár (Ft)`)
var(Stat_adatok$log_ar)

library("moments")

#csúcsosság: ha az eloszlas normalis, akkor a csucsossag 0
#pozitiv: az eloszlas a normalishoz kepest csucsosabb
#nehativ: az eloszlas a normalishoz kepest laposabb

kurtosis(Stat_adatok$`Ár (Ft)`)

#ferdeség: 

skewness(Stat_adatok$`Ár (Ft)`)

kurtosis(Stat_adatok$log_ar)
skewness(Stat_adatok$log_ar)

agostino.test(Stat_adatok$log_ar)
anscombe.test(Stat_adatok$log_ar)


kurtosis(Stat_adatok$log_nm)
skewness(Stat_adatok$log_nm)

agostino.test(Stat_adatok$log_nm)
anscombe.test(Stat_adatok$log_nm)

jarque.test(Stat_adatok$log_nm)
jarque.test(Stat_adatok$log_ar)

jarque.test(Stat_adatok$log_ar)
jarque.test(Stat_adatok$`Ár (Ft)`)

jarque.test(Stat_adatok$log_nm)

#ezek igy mar majdnem nagyon kozel vannak a normalis eloszlashoz, kezelhetjuk oket normalis eloszlaskent


shapiro.test(Stat_adatok$log_ar) #ez is a normalis eloszlast nezi
shapiro.test(Stat_adatok$log_nm)


#standardizalom az arat, hatha jol jon
Stat_adatok$stand_ar<-scale(Stat_adatok$`Ár (Ft)`)


#fuggetlenseg vizsgalat

chisq.test(table(Stat_adatok$Négyzetméter, Stat_adatok$Szobaszám))
chisq.test(table(Stat_adatok$`Ár (Ft)`, Stat_adatok$Négyzetméter))
chisq.test(table(Stat_adatok$log_ar, Stat_adatok$log_nm))
chisq.test(table(Stat_adatok$log_ar, Stat_adatok$Szobaszám))

chisq.test(table(Stat_adatok$`Ár (Ft)`, Stat_adatok$Szobaszám))



chisq.test(table(Stat_adatok$`Ár (Ft)`, Stat_adatok$Főváros))
chisq.test(table(Stat_adatok$Négyzetméter, Stat_adatok$Főváros))
chisq.test(table(Stat_adatok$Szobaszám, Stat_adatok$Főváros))







#ez alapjan nem fuggetlen a ket valtozo

library("vcd")

assocstats(table(Stat_adatok$Négyzetméter, Stat_adatok$Szobaszám))


#kapcsolatelemzes

#két mennyiségi
#scatter plot

plot(`Ár (Ft)`~Négyzetméter, data=Stat_adatok)

#korrelációs mátrix:
Stat_adatok_korr<-Stat_adatok[,c(1,4,6)] #beleteszem az arat, a nmt és a szobaszamot
cor(Stat_adatok_korr)

cor(Stat_adatok_korr)
cor.test(Stat_adatok_korr,method="spearman")




library("PerformanceAnalytics")

chart.Correlation(Stat_adatok_korr, histogram = TRUE,pch=19 )
#ha nem kell normalis eloszlas: spearman,
#ha feltetel a normalis eloszlas: pearson

Stat_adatok_korr2<-Stat_adatok[,c(8,9,6)]
chart.Correlation(Stat_adatok_korr2, histogram = TRUE, method="pearson", pch=19)

cor(Stat_adatok_korr2)

#parciális korreláció

library(ppcor)


parc_korr<-pcor(Stat_adatok_korr,method = c("spearman"))
parc_korr  #itt kiderül, hogy igazából a szobaszám az árra nem hat, csak a nm-en keresztül

library(lattice)

histogram(~`Ár (Ft)`| Főváros, data=Stat_adatok) 

histogram(~Négyzetméter|Főváros, data=Stat_adatok)

Dhistogram(~Szobaszám|Főváros, data=Stat_adatok)


densityplot(Stat_adatok$log_ar, groups=Stat_adatok$Főváros, auto.key=TRUE)
densityplot(Stat_adatok$`Ár (Ft)`, groups=Stat_adatok$Főváros, auto.key=TRUE)

densityplot(Stat_adatok$Négyzetméter, groups=Stat_adatok$Főváros, auto.key=TRUE)
densityplot(Stat_adatok$log_nm, groups=Stat_adatok$Főváros, auto.key=TRUE)

densityplot(Stat_adatok$Szobaszám, groups=Stat_adatok$Főváros, auto.key=TRUE)


boxplot(log_ar ~Főváros, data= Stat_adatok) #a fovarosban dragabbak a lakasok
boxplot(Négyzetméter~Főváros, data=Stat_adatok)
boxplot(Négyzetméter~Főváros, data=Stat_adatok)

#TODO
#meg kéne nézni azt is, hogy a fővárosban szignifikénsan drágábbak-e a lakások??????


#normalis eloszlas feltétel (t test, anova)
#2 csoport(Fovaros vagy videk)

t.test(Stat_adatok$log_ar~Stat_adatok$Főváros)
#a p érték kicsi, tehát a fővárosi lakások SZIGNIFIKÁNSAN drágábbak, mint a fővárosi lakások!! 
#mert elvetjuk a nullhipotéist

tapply(Stat_adatok$`Ár (Ft)`, Stat_adatok$Főváros, mean)#az árak átlaga

tapply(Stat_adatok$Négyzetméter, Stat_adatok$Főváros, mean)

tapply(Stat_adatok$Szobaszám, Stat_adatok$Főváros, mean)

t.test(Stat_adatok$Szobaszám~Stat_adatok$Főváros)
t.test(Stat_adatok$`Ár (Ft)`~Stat_adatok$Főváros)
t.test(Stat_adatok$log_nm~Stat_adatok$Főváros)


#2 vagy több csoport


#anova?
library(car)

lm(Stat_adatok$log_ar~Stat_adatok$Főváros) -> mod1
anova(mod1)

lm(Stat_adatok$log_nm~Stat_adatok$Főváros)->mod2
anova(mod2) #itt az latszik, hogy a nm videken es a fovarosban 5%on kulonbozik

lm(Stat_adatok$Szobaszám~Stat_adatok$Főváros)->mod3
anova(mod3)  #a szobaszámok átlaga nem kulonbozik szignifikansan a fovarosban es videken




library("vcd")

assocstats(table(Stat_adatok$`Ár (Ft)`,Stat_adatok$Főváros))
assocstats(table(Stat_adatok$`Ár (Ft)`,Stat_adatok$Szobaszám))



cor(Stat_adatok$`Ár (Ft)`,Stat_adatok$Főváros)
cor(Stat_adatok$`Ár (Ft)`, Stat_adatok$Szobaszám)
cor(Stat_adatok$log_ar,Stat_adatok$Szobaszám)
cor(Stat_adatok$log_ar,Stat_adatok$Négyzetméter)
cor(Stat_adatok$log_ar,Stat_adatok$log_nm)


cor.test(Stat_adatok$log_ar,Stat_adatok$Szobaszám)


###########################################################
#KLASZTERELEMZES
Stat_adatok_korr<-Stat_adatok[,c(1,4,6)]

Stat_adatok_korr<-na.omit(Stat_adatok_korr)


#sztenderdizaljuk a valtozokat, de szerintem ez most nem is kell

Stat_adatok_korr$Négyzetméter<-scale(Stat_adatok$Négyzetméter)
Stat_adatok_korr$`Ár (Ft)`<-scale(Stat_adatok$`Ár (Ft)`)
Stat_adatok_korr$Szobaszám<-scale(Stat_adatok$Szobaszám)

kmeans(Stat_adatok_korr,Stat_adatok_korr[1:6,]) #♣6 csoportba osztjuk
 #kmeans(Stat_adatok_korr,Stat_adatok_korr[1:4,])

library(cluster)

dist(Stat_adatok_korr[1:3])

silhouette(kmeans(Stat_adatok_korr[1:3],Stat_adatok_korr[1:4,1:3])$cluster, dist(Stat_adatok_korr[,1:3]))
#ez nem jo, meg kene valami sziluettes cucc


plot(silhouette(kmeans(Stat_adatok_korr[1:3],Stat_adatok_korr[1:6,1:3])$cluster, dist(Stat_adatok_korr[,1:3])))





#hierarchikus klaszterezes

hclust(dist(Stat_adatok_korr[1]),method="single") -> hierklaszt
plot(hierklaszt, hang=-1)

rect.hclust(hierklaszt,k=3)	


Stat_adatok_korr$Szobaszám<-as.factor(Stat_adatok_korr$Szobaszám)

hclust(dist(t(Stat_adatok_korr[1:3]),method="maximum"),method="complete") -> hierklaszt
plot(hierklaszt, hang=-1)


###############################################################
#klaszterelemzes ujra

Stat_adatok_clust<-Stat_adatok[,c(1,4,6,7)]
Stat_adatok_clust

Stat_adatok_clust<-na.omit(Stat_adatok_clust)

kmeans(Stat_adatok_korr,4)


hclust(dist(t(Stat_adatok_clust[1:4]),method="maximum"),method="complete") -> hierklaszt
plot(hierklaszt)






#Lineáris regresszió

lm(`Ár (Ft)`~Főváros, data=Stat_adatok)

lm(`Ár (Ft)`~Szobaszám, data=Stat_adatok)
lm(`Ár (Ft)`~Négyzetméter, data=Stat_adatok)

lm(`Ár (Ft)`~as.factor(Szobaszám), data=Stat_adatok)

lm(`Ár (Ft)`~Főváros+Szobaszám+Négyzetméter, data=Stat_adatok)->reg1
summary(reg1)

#r negyzet mutato 60%, tehat egesz jo modellt illesztettunk (az eredmenyvaltozo szorodasanak mekkora reszet mutatja meg a modellunk)
#f teszt nullhipotezise: egyik betanak sincs magyarazo hatasa, ennek az ellenhipotezise: van olyan beta, ami szignifikans

lm(`Ár (Ft)`~Főváros+as.factor(Szobaszám)+Négyzetméter, data=Stat_adatok)->reg2
summary(reg2)


#érdemes megnézni, hogy jobb lesz e a modell, ha kihagyunk valamilyen magyaarazo valtozot?
step(reg2,direction = "both") #tehát semmilyen valozot nem kell kihagynunk


#minden valtozot standerdizalunk, ez ahhoz lehet jo, hogy megnezzuk, hogy melyik valtozonak mekkora a hatasa az eredmenyvalzotora, de ez nem jo, mert van kategoria valtozonk, szoval off

lm(scale(`Ár (Ft)`)~Főváros+scale(Szobaszám)+scale(Négyzetméter), data=Stat_adatok)->reg3
summary(reg3)




#melyek azok az adatpontok,amelyeknek tul nagy a befolyasolo hatasa?

hatvalues(reg1)

#ezt ellenorizhetjuk cook féle d statisztikával, ahol nagyobb mint 1, annak kritikus a hatasa

na.omit(Stat_adatok[c("Ár (Ft)","Főváros","Négyzetméter","Szobaszám")])->reg_input

cooks.distance(reg1)
cooks.distance(reg1) -> reg_input$cookd

cooks.distance(reg1)
max(cooks.distance(reg1))

reg_input[which(reg_input$cookd > (1)),]
reg_input[which(reg_input$cookd < (1)),] -> reg_input_2 #csinalunk egy olyan bazis, ahol kihagyjuk azt az 5 szobas nagy lakast, mert annak tul nagy a befolyasolo tenyezője

#lefuttatjuk erre is a modellt

lm(`Ár (Ft)`~Főváros+Szobaszám+Négyzetméter, data=reg_input_2)->reg4
summary(reg4)

#láthatjuk, hogy a szobaszám még mindig enyhén szignifikáns, de az r négyzet mutató növekedett egy kicsit, igazából nem javított nagyon a modellünkön, hogy azt az egy megfigyelést kihagytuk

#multikollinearitas
#erdemes megvizsgalni, hogy a magyarazo valtozok mennyire mozgnak egyutt
library(car)
vif(reg4) #minel kisebbek, annal jobb, 3 alatt van minden, tehat nincs problema

#mi van, ha a log árat nézzük

lm(log_ar~Főváros+Szobaszám+Négyzetméter, data=Stat_adatok)->reg5
summary(reg5)

#ebben az esetben a szobaszám már nem szignifikáns!! a szobaszam tul nagy hatassal van a negyzetmeterre
#megnezzuk itt is hogy érdemes-e kihagyni a szobaszámot

vif(reg5)

step(reg5,direction = "both")
#igen, hagyjuk ki a szobaszámot!

lm(log_ar~Főváros+Négyzetméter, data=Stat_adatok)->reg6
summary(reg6)




#################################################################################################
#logisztikus regresszió (Fővárosi e egy lakas, vagy sem?)


glm(Főváros~Négyzetméter+`Ár (Ft)`,family=binomial, data=Stat_adatok)->logrer1
summary(logrer1)

glm(Főváros~log_nm+log_ar,family=binomial, data=Stat_adatok)->logrer2
summary(logrer2)

#tehát visszafele is igaz

glm(Főváros~Négyzetméter+`Ár (Ft)`+Szobaszám,family=binomial, data=Stat_adatok)->logrer3
summary(logrer3)

hatvalues(logrer1)
vif(logrer1) #ezek nem feltetlen kellenek sztem


table((sign(logrer1$fitted.values-0.3368056)+1)/2) #a vagasi ertek a sokasagi atlag, igy egesz jo ertek jon ki
table (Stat_adatok$Főváros)

#nezzuk a sokasagi atlagot
as.numeric(Stat_adatok$Főváros)
mean(Stat_adatok$Főváros) #ez 0.3368056, fentebb hasznaltam


#######################################
#DONTESI FAK

install.packages("rpart")
library(rpart)

rpart(Főváros~Négyzetméter+`Ár (Ft)`, data=Stat_adatok)->dec_tree

summary(dec_tree)

plot(dec_tree)
text(dec_tree)

install.packages("rpart.plot")
library("rpart.plot")
prp(dec_tree,type=4)

rpart(Főváros~`Ár (Ft)`, data=Stat_adatok)->dec_tree2
prp(dec_tree2)

plotcp(dec_tree)


#a modell előre jeéző pontosságát ROC görbén ábrézoljuk

library(pROC)
plot(roc(Stat_adatok$Főváros,predict(dec_tree)))
roc(Stat_adatok$Főváros,predict(dec_tree)) #ez nem fut le

table(Stat_adatok$Főváros,predict(dec_tree,type="class"))



rpart(Főváros~Négyzetméter+`Ár (Ft)`+Szobaszám, data=Stat_adatok,method="class",control=rpart.control(cp=0))->dec_tree3
prp(dec_tree3,type=4,extra=104)
summary(dec_tree3)

printcp(dec_tree3)


rpart(Főváros~Négyzetméter+`Ár (Ft)`+Szobaszám, data=Stat_adatok,method="class",control=rpart.control(cp=0.036082))->dec_tree4
prp(dec_tree4,type=4,extra=104)


##########################
#CTREE

library("party")



ctree(Főváros~Négyzetméter+`Ár (Ft)`+Szobaszám, data=Stat_adatok) -> ctree_fa
ctree_fa


plot(ctree_fa)
