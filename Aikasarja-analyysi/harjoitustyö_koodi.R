#osa 1
#meitä kiinnostaa vain arvot vuodesta 2009 eteenpäin joten, sitä edeltävät arvot
#voidaan pudottaa pois
#D:/kurssit/TILS619/aikasarjaharjoitustyö/
populaatio <- read.csv2(file = "jkl.csv",header = TRUE)
populaatio$vuosi <- as.numeric(substr(populaatio$Kuukausi, 1, 4))
populaatio$Kuukausi <- rep(1:12,times=length(populaatio$Kuukausi)/12)
populaatio <- subset(populaatio, populaatio$vuosi >= 2009)
#tarkastellaan miltä data näyttää ennen kuin sille tehdään mitään
plot(populaatio$JyvÃ.skylÃ., ylab="populaatio",xlab="kuukausi")
#datasta nähdään selvä väestönkasvu vuodesta 2009 vuoteen 2020
#keväällä nähdään väestönlasku. Elo-ja syyskuussa nähdään korkea väestönkasvu.
#Tähän voidaan veikata selityksenä se, että Jyäskylä on opiskelijakaupunki
#ja opiskelijat muuttavat pois valmistuessaan keväällä ja uudet opiskelijat
#muuttavat kaupunkiin elo-ja syyskuussa, kun lukukausi alkaa.
ue <- ts(populaatio$JyvÃ.skylÃ.,start=2009, frequency=12)
plot(ue)
#dataan voidaan lisätä liukuva keskiarvo mutta data on jo aika selvää niin
#se ei tuo hirveästi uutta tietoa
ma_smooth <- function(x, window_size)
{
  win <- rep(1/window_size, window_size) # painot
  stats::filter(x, win, sides=2)
}

ue_smooth <- ma_smooth(ue, window_size=12)
ts.plot(ue, ue_smooth, col=c(1,2))
#nähdään kuinka paljon väestönmuutos on verrattuna liukuvaan keskiarvoon
ue_r <- ue - ue_smooth
plot(ue_r)

m <- month.abb[cycle(ue)]
fit <- lm(ue_r ~ m)
#datan autokorrelaatio
acf(fit$residuals)



par(mfrow=c(4, 1), mar=c(1,1,1,1))
plot(ue)
plot(ue_smooth)
ue_seasonal <- ts(fit$fitted.values, start=2009, frequency=12); plot(ue_seasonal)
ue_residual <- ts(fit$residuals, start=2009, frequency=12); plot(ue_residual, type='l')

#osa 2
#arma mallit sopivat stationaarisille datoille. alkumuodossa data ei kuitenkaan
#näytä stationaariselta
#voidaan kuitenkin tarkistaa, jos differenssi näyttäisi stationaariselta
d_pop <- diff(ue)
ts.plot(d_pop, xlab = "Vuosi", ylab = "populaatio")

#diierenssi on stationaarisen näköinen
#joten voidaan jatkaa differenssin kanssa
#Aloitetaan tarkastelemalla diffin ja aineiston autokorreaatioita ja osittaisia
#autokorrelaatioita

#aineiston auokorrelaatiosta on vaikea sanoa mitään
acf(ue)
#osittaisesta taas saa jotain irti
pacf(ue)
#tämän perusteella voitaisiin käyttää ma(1) mallia sillä viiveen yksi jälkeen ei
#ole nollasta eroavia arvoja ja samoin ar(1) sopisi sillä arvot ovat pieniä ei-
#vätkä kasva sen myöhemmin
acf(d_pop)
#ar malli ei oiikein sovi sillä autokorrelaatio
#ei laske koko ajan vaan tulee takaisin jaksollisesti 
#ma ei myöskään sovi sillä arvot kasvavat merkittäviksi useasti
pacf(d_pop)
#sama kuin edellisessä vaikea sanoa mitään lukuja

#vaikuttaisi siltä, että muuttamattomasta aineistosta saatiin joitain arvoja
#joita voitaisiin mahdollisesti käyttää mutta se ei ole sopiva itsessänsä
#arma malleille joten kokeillaan sovittaa differenssiä arima malleihin

#kokeillaan ensin sarima mallia jossa on kausivaihtelu mallissa mukana
#koska datassa nähdään kausivaihtelua
#laitetaan mallille kausi 12 sillä se sopii vuoden mukaiseksi
mod1 <- arima(ue, order = c(1, 1, 0), seasonal = list(order = c(1, 0, 0), period = 12))
plot(mod1$residuals)
acf(mod1$residuals)
#autokorrelaatio putoaa melkain nollaksi jo ensimäisen jälkeen joten laitetaan
#joka mietittiin sopivaksi edellisissä kokeissa ma=1
mod2 <- arima(ue, order = c(1, 1, 1), seasonal = list(order = c(1, 0, 0), period = 12))
plot(mod2$residuals)
acf(mod2$residuals)
#autokorrelaatio putosiedelleen mutta niin vähän että on vaikea sanoa onko tämä malli
#todellisesti parempi
#kokeilaan vielä ma(2) mallia
mod3 <- arima(ue, order = c(1, 1, 2), seasonal = list(order = c(1, 0, 0), period = 12))
plot(mod3$residuals)
acf(mod3$residuals)
#ei näy suurta muutosta joten sarma(1,1,1)(1,0,0)s12 malli näyttäisi sopivalta
#tässä vaiheessa kiinnostaa kokeilla jos pelkkä arima toimisi,
#joten kokeillaan vielä sitä
mod4 <- arima(ue, order = c(1, 0, 0))
plot(mod4$residuals)
pacf(mod4$residuals)
acf(mod4$residuals)
#alkulukemilla ainakin vaikuttaisi, että arima olisi huonompi mutta kokeillaan
#kuitenkin jos tätä saataaisiin parannettua

#kokeillaan laittaa malliin edelllä löydetyt ar ja ma arvot
mod5 <- arima(ue, order = c(1, 1, 1))
plot(mod5$residuals)
acf(mod5$residuals)
#sanoisin edelleen, että sarima malli olisi paras malli tälle datalle



#osa4
#nyt kun olemme löytäneet sopivan mallin voimme kokeilla ennustaa Jyväskylän
#väestön kasvua seuraavalle 24 kuukaudelle
#mod2 on paras malli joten se sijoitetaan ennustus algoritmiin
pred <- predict(mod2, n.ahead = 2 * 12)
prediction <- ts(pred$pred, start = c(2021, 1), frequency = 12)
upper <- ts(pred$pred + qnorm(0.975) * pred$se, start = c(2021, 1), frequency = 12)
lower <- ts(pred$pred - qnorm(0.975) * pred$se, start = c(2021, 1), frequency = 12)
ts.plot(ue, prediction, lower, upper, col = c("black", rep("blue", 3)), 
        lty = c(1, 1, rep(2, 2)),  ylab = "Jyväskylän asukasmäärä", xlab = "Vuosi")
legend("bottomleft", legend = c("Aineisto", "Ennuste", "95 % luottamusväli"), col = c("black", "blue", "blue"),
       lty = c(1, 1, 2))

#vaikka tulin tulokseen, että sarima oli parempi niin voi olla hyvä kuitenkin
#hyvä tarkistaa millainen ennustus tulisi arima mallilla
pred2 <- predict(mod5, n.ahead = 2 * 12)
prediction <- ts(pred2$pred, start = c(2021, 1), frequency = 12)
upper <- ts(pred2$pred + qnorm(0.975) * pred$se, start = c(2021, 1), frequency = 12)
lower <- ts(pred2$pred - qnorm(0.975) * pred$se, start = c(2021, 1), frequency = 12)
ts.plot(ue, prediction, lower, upper, col = c("black", rep("blue", 3)), 
        lty = c(1, 1, rep(2, 2)),  ylab = "Jyväskylän asukasmäärä", xlab = "Vuosi")
legend("bottomleft", legend = c("Aineisto", "Ennuste", "95 % luottamusväli"), col = c("black", "blue", "blue"),
       lty = c(1, 1, 2))
#sanoisin, että ennustus ei ole hirveän järkevä


#osa 5
#jos kaksi vuotta, joita pyritään ennustamaan olisivat normaaleja vuosia niin
#sanoisin, että ennustus on järkevä.
#vaikka data jatkuu vuoden 2020 loppuun niin ennustaminen ei välttämättä
#ole täysin tarkkaa sillä datasta yritetään ennustaa kaksi korona vuotta (2021-2022)
#jotka eivät ole omasta mielestäni ole nomaaleja vuosia ja siksi eivät ole välttämättä
#verrattavissa normaaleihin vuosiin joita data ennustaa. Voi olla että korona
#on voinut vaikkuttaa muuttamiseen, sillä etätyöskentelystä on tullut normaalia ja 
#oppilaat eivät välttämättä ole muuttaneet Jyväskylään vaan ovat jääneet kotikuntiinsa
#koronan vuoksi muuttaminen pois jyväskylästä voi myös olla heikompaa.