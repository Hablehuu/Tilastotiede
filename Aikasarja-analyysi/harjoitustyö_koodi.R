#osa 1
#meit� kiinnostaa vain arvot vuodesta 2009 eteenp�in joten, sit� edelt�v�t arvot
#voidaan pudottaa pois
#D:/kurssit/TILS619/aikasarjaharjoitusty�/
populaatio <- read.csv2(file = "jkl.csv",header = TRUE)
populaatio$vuosi <- as.numeric(substr(populaatio$Kuukausi, 1, 4))
populaatio$Kuukausi <- rep(1:12,times=length(populaatio$Kuukausi)/12)
populaatio <- subset(populaatio, populaatio$vuosi >= 2009)
#tarkastellaan milt� data n�ytt�� ennen kuin sille tehd��n mit��n
plot(populaatio$Jyv�.skyl�., ylab="populaatio",xlab="kuukausi")
#datasta n�hd��n selv� v�est�nkasvu vuodesta 2009 vuoteen 2020
#kev��ll� n�hd��n v�est�nlasku. Elo-ja syyskuussa n�hd��n korkea v�est�nkasvu.
#T�h�n voidaan veikata selityksen� se, ett� Jy�skyl� on opiskelijakaupunki
#ja opiskelijat muuttavat pois valmistuessaan kev��ll� ja uudet opiskelijat
#muuttavat kaupunkiin elo-ja syyskuussa, kun lukukausi alkaa.
ue <- ts(populaatio$Jyv�.skyl�.,start=2009, frequency=12)
plot(ue)
#dataan voidaan lis�t� liukuva keskiarvo mutta data on jo aika selv�� niin
#se ei tuo hirve�sti uutta tietoa
ma_smooth <- function(x, window_size)
{
  win <- rep(1/window_size, window_size) # painot
  stats::filter(x, win, sides=2)
}

ue_smooth <- ma_smooth(ue, window_size=12)
ts.plot(ue, ue_smooth, col=c(1,2))
#n�hd��n kuinka paljon v�est�nmuutos on verrattuna liukuvaan keskiarvoon
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
#n�yt� stationaariselta
#voidaan kuitenkin tarkistaa, jos differenssi n�ytt�isi stationaariselta
d_pop <- diff(ue)
ts.plot(d_pop, xlab = "Vuosi", ylab = "populaatio")

#diierenssi on stationaarisen n�k�inen
#joten voidaan jatkaa differenssin kanssa
#Aloitetaan tarkastelemalla diffin ja aineiston autokorreaatioita ja osittaisia
#autokorrelaatioita

#aineiston auokorrelaatiosta on vaikea sanoa mit��n
acf(ue)
#osittaisesta taas saa jotain irti
pacf(ue)
#t�m�n perusteella voitaisiin k�ytt�� ma(1) mallia sill� viiveen yksi j�lkeen ei
#ole nollasta eroavia arvoja ja samoin ar(1) sopisi sill� arvot ovat pieni� ei-
#v�tk� kasva sen my�hemmin
acf(d_pop)
#ar malli ei oiikein sovi sill� autokorrelaatio
#ei laske koko ajan vaan tulee takaisin jaksollisesti 
#ma ei my�sk��n sovi sill� arvot kasvavat merkitt�viksi useasti
pacf(d_pop)
#sama kuin edellisess� vaikea sanoa mit��n lukuja

#vaikuttaisi silt�, ett� muuttamattomasta aineistosta saatiin joitain arvoja
#joita voitaisiin mahdollisesti k�ytt�� mutta se ei ole sopiva itsess�ns�
#arma malleille joten kokeillaan sovittaa differenssi� arima malleihin

#kokeillaan ensin sarima mallia jossa on kausivaihtelu mallissa mukana
#koska datassa n�hd��n kausivaihtelua
#laitetaan mallille kausi 12 sill� se sopii vuoden mukaiseksi
mod1 <- arima(ue, order = c(1, 1, 0), seasonal = list(order = c(1, 0, 0), period = 12))
plot(mod1$residuals)
acf(mod1$residuals)
#autokorrelaatio putoaa melkain nollaksi jo ensim�isen j�lkeen joten laitetaan
#joka mietittiin sopivaksi edellisiss� kokeissa ma=1
mod2 <- arima(ue, order = c(1, 1, 1), seasonal = list(order = c(1, 0, 0), period = 12))
plot(mod2$residuals)
acf(mod2$residuals)
#autokorrelaatio putosiedelleen mutta niin v�h�n ett� on vaikea sanoa onko t�m� malli
#todellisesti parempi
#kokeilaan viel� ma(2) mallia
mod3 <- arima(ue, order = c(1, 1, 2), seasonal = list(order = c(1, 0, 0), period = 12))
plot(mod3$residuals)
acf(mod3$residuals)
#ei n�y suurta muutosta joten sarma(1,1,1)(1,0,0)s12 malli n�ytt�isi sopivalta
#t�ss� vaiheessa kiinnostaa kokeilla jos pelkk� arima toimisi,
#joten kokeillaan viel� sit�
mod4 <- arima(ue, order = c(1, 0, 0))
plot(mod4$residuals)
pacf(mod4$residuals)
acf(mod4$residuals)
#alkulukemilla ainakin vaikuttaisi, ett� arima olisi huonompi mutta kokeillaan
#kuitenkin jos t�t� saataaisiin parannettua

#kokeillaan laittaa malliin edelll� l�ydetyt ar ja ma arvot
mod5 <- arima(ue, order = c(1, 1, 1))
plot(mod5$residuals)
acf(mod5$residuals)
#sanoisin edelleen, ett� sarima malli olisi paras malli t�lle datalle



#osa4
#nyt kun olemme l�yt�neet sopivan mallin voimme kokeilla ennustaa Jyv�skyl�n
#v�est�n kasvua seuraavalle 24 kuukaudelle
#mod2 on paras malli joten se sijoitetaan ennustus algoritmiin
pred <- predict(mod2, n.ahead = 2 * 12)
prediction <- ts(pred$pred, start = c(2021, 1), frequency = 12)
upper <- ts(pred$pred + qnorm(0.975) * pred$se, start = c(2021, 1), frequency = 12)
lower <- ts(pred$pred - qnorm(0.975) * pred$se, start = c(2021, 1), frequency = 12)
ts.plot(ue, prediction, lower, upper, col = c("black", rep("blue", 3)), 
        lty = c(1, 1, rep(2, 2)),  ylab = "Jyv�skyl�n asukasm��r�", xlab = "Vuosi")
legend("bottomleft", legend = c("Aineisto", "Ennuste", "95 % luottamusv�li"), col = c("black", "blue", "blue"),
       lty = c(1, 1, 2))

#vaikka tulin tulokseen, ett� sarima oli parempi niin voi olla hyv� kuitenkin
#hyv� tarkistaa millainen ennustus tulisi arima mallilla
pred2 <- predict(mod5, n.ahead = 2 * 12)
prediction <- ts(pred2$pred, start = c(2021, 1), frequency = 12)
upper <- ts(pred2$pred + qnorm(0.975) * pred$se, start = c(2021, 1), frequency = 12)
lower <- ts(pred2$pred - qnorm(0.975) * pred$se, start = c(2021, 1), frequency = 12)
ts.plot(ue, prediction, lower, upper, col = c("black", rep("blue", 3)), 
        lty = c(1, 1, rep(2, 2)),  ylab = "Jyv�skyl�n asukasm��r�", xlab = "Vuosi")
legend("bottomleft", legend = c("Aineisto", "Ennuste", "95 % luottamusv�li"), col = c("black", "blue", "blue"),
       lty = c(1, 1, 2))
#sanoisin, ett� ennustus ei ole hirve�n j�rkev�


#osa 5
#jos kaksi vuotta, joita pyrit��n ennustamaan olisivat normaaleja vuosia niin
#sanoisin, ett� ennustus on j�rkev�.
#vaikka data jatkuu vuoden 2020 loppuun niin ennustaminen ei v�ltt�m�tt�
#ole t�ysin tarkkaa sill� datasta yritet��n ennustaa kaksi korona vuotta (2021-2022)
#jotka eiv�t ole omasta mielest�ni ole nomaaleja vuosia ja siksi eiv�t ole v�ltt�m�tt�
#verrattavissa normaaleihin vuosiin joita data ennustaa. Voi olla ett� korona
#on voinut vaikkuttaa muuttamiseen, sill� et�ty�skentelyst� on tullut normaalia ja 
#oppilaat eiv�t v�ltt�m�tt� ole muuttaneet Jyv�skyl��n vaan ovat j��neet kotikuntiinsa
#koronan vuoksi muuttaminen pois jyv�skyl�st� voi my�s olla heikompaa.