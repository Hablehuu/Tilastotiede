load("htdata2022.RData")
library(mice)
data <- htdata2022
md.pattern(data,rotate.names = T)
#t�ydellisten otosten prosentti on 72.97%
1372/1880
#analyysi pelkill� t�ydellisill� riveill� voisi toimia
#Puuttuvuus ainakin silm�ilyll� vaikuttaisi olevan tyyppi� MAR
#puuttuvuutta esiintyy enim�kseen vanhempien tiedoissa
#Tutkimuksessa halutaan selvitt�� voidaanko lukutaitoa ennustaa tutkimuksessa 
#ker�tyill� taustamuuttujilla
#ensiksi toteutetaan t�ydellisten rivien analyysi
taydeldata <- na.omit(data)
taydeldata$ikaV <- floor(taydeldata$ika/12)
malli1 <- lm(data= taydeldata,luksuj ~ ika+sp+lukutaito+nimeamis+aanne+kirjain
             +sanavarasto +lukivanh+kouluaitilk+kouluisalk+jatkoaiti+jatkoisa)
summary(malli1)
#sukupuolella, i�ll�,��nt�misell� ja vanhempien kouluuksella ei vaikuttaisi olevan
#tilastollisesti merkitt�v�� vaikutusta lukusujuvuuteen
malli2 <- lm(data= taydeldata,luksuj ~ nimeamis+kirjain+lukutaito
             +sanavarasto +lukivanh)

summary(malli2)
plot(residuals(malli2))
plot(residuals(malli1))
#moni-impuointi malli
datamalli2 <- subset (data, select = c(luksuj,nimeamis,kirjain,lukutaito,sanavarasto,lukivanh))
#luodaaan ennustusmatriisi
predictorMatrix <- rbind(c(0,1,1,1,1,1),
                         c(1,0,1,1,0,0),
                         c(0,1,0,1,0,0),
                         c(0,0,1,0,1,0),
                         c(0,0,1,1,0,0),
                         c(1,0,1,1,0,0)
)
#malli
monidata <- mice(datamalli2,m = 10, meth="pmm",predictorMatrix=predictorMatrix, maxit = 40,
                 print=F)
#malli2
monidata2 <- mice(datamalli2, meth="norm",predictorMatrix=predictorMatrix, maxit = 40,
                 print=F)

#k�ytet��n metodia pmm koska norm antaa mahdottomia lukuja
plot(monidata)
#molemmat kuvat vaikuttavat n�ytt�v�n samoilta, moni imutointi antaa suurempia
#arvoja kuin mit� datassa on joissakin tilanteissa
xyplot(monidata,luksuj ~nimeamis)
xyplot(monidata2,luksuj ~nimeamis)
plot(taydeldata$nimeamis,taydeldata$luksuj)
xyplot(monidata,luksuj ~sanavarasto)
plot(taydeldata$sanavarasto,taydeldata$luksuj)
xyplot(monidata,luksuj ~kirjain)
plot(taydeldata$kirjain,taydeldata$luksuj)
xyplot(monidata,luksuj ~lukivanh)
plot(taydeldata$lukivanh,taydeldata$luksuj)
densityplot(monidata)

lukusujuminen <- with(monidata,lm(luksuj ~ nimeamis+kirjain+lukutaito
                                  +sanavarasto +lukivanh))
lukusujuminenpooled <- summary(pool(lukusujuminen))
print(lukusujuminenpooled)

#vanhempien lukuvaikeus muuttui merkitt�v�mm�ksi kuin t�ydellisten rivien
#analyysiss�. Aikaisemmin n�htiin, ett� puuttuvuutta esiintyy paljon
#vanhempien tiedoissa.Voi olla, ett� vanhemmat joilla on lukih�iri�
#eiv�t ole vastanneet kyselyyn yht� paljon kuin muut vanhemmat
