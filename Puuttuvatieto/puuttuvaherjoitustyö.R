load("htdata2022.RData")
library(mice)
data <- htdata2022
md.pattern(data,rotate.names = T)
#täydellisten otosten prosentti on 72.97%
1372/1880
#analyysi pelkillä täydellisillä riveillä voisi toimia
#Puuttuvuus ainakin silmäilyllä vaikuttaisi olevan tyyppiä MAR
#puuttuvuutta esiintyy enimäkseen vanhempien tiedoissa
#Tutkimuksessa halutaan selvittää voidaanko lukutaitoa ennustaa tutkimuksessa 
#kerätyillä taustamuuttujilla
#ensiksi toteutetaan täydellisten rivien analyysi
taydeldata <- na.omit(data)
taydeldata$ikaV <- floor(taydeldata$ika/12)
malli1 <- lm(data= taydeldata,luksuj ~ ika+sp+lukutaito+nimeamis+aanne+kirjain
             +sanavarasto +lukivanh+kouluaitilk+kouluisalk+jatkoaiti+jatkoisa)
summary(malli1)
#sukupuolella, iällä,ääntämisellä ja vanhempien kouluuksella ei vaikuttaisi olevan
#tilastollisesti merkittävää vaikutusta lukusujuvuuteen
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

#käytetään metodia pmm koska norm antaa mahdottomia lukuja
plot(monidata)
#molemmat kuvat vaikuttavat näyttävän samoilta, moni imutointi antaa suurempia
#arvoja kuin mitä datassa on joissakin tilanteissa
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

#vanhempien lukuvaikeus muuttui merkittävämmäksi kuin täydellisten rivien
#analyysissä. Aikaisemmin nähtiin, että puuttuvuutta esiintyy paljon
#vanhempien tiedoissa.Voi olla, että vanhemmat joilla on lukihäiriö
#eivät ole vastanneet kyselyyn yhtä paljon kuin muut vanhemmat
