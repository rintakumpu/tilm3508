### Aineisiton lukeminen 
aikasarjat <- read.table('\\utu.fi\verkkolevyt\Tilastotieteen kurssikansio\kalle\TILA08\Harjoitustyot\kevat_2014/sarjat.txt', header = TRUE)

### Oman sarjan valinta: Oma sarake valitaan kirjoittamalla ISO X etuliitteeksi omalle opiskelijanumerolle (huomaa pilkku vasemman 
### hakasulun j�lkeen ja hipsukat). Korvaa numerot 123456 omalla opiskelijanumerollasi.

x <- aikasarjat[,'X123456']
## Luodaan hypoteettinen kuukausisarja
xt <- ts(x, start = c(2000, 1), frequency = 12)

## Sarjan kuvaaja. Kuvan y�puolisessa palkissa on kohta, jossa lukee: Export. 
## Sen alta l�ytyy komento kuvan tallentamiseen esim. PDF-muodossa. Kuvaaja esitet��n raportissa.
ts.plot(xt, main = 'Kuvan otsikko')

## Siihen autokorrelaatiotarkasteluun, jonka pohjalta malli valitaan, liittyv�t 
## auto- ja osittaisautokorrelaatiokuvaajat on liitett�v� raporttiin. 

## Alustava autokorrelaatiotarkastelu
acf(xt, lag = 36, main = 'Kuvan otsikko')
pacf(xt, lag = 36, main = 'Kuvan otsikko')

## Sarjan differointi (jos alkuper�inen sarja ei ole stationaarinen)
dxt <- diff(xt, lag = 1, differences = 1)
ts.plot(dxt, main = 'Kuvan otsikko')
## Erotussarjan auto-ja osittaisautokorrelaatiotarkastelu
acf(dxt, lag = 36, main = 'Kuvan otsikko')
pacf(dxt, lag = 36, main = 'Kuvan otsikko')

## Sarjan erotuksen erotuksen (2. differenssin) laskeminen, mik�li erotussarja dxt ei ole stationaarinen.
dxt2 <- diff(xt, lag = 1, differences = 2)
ts.plot(dxt2, main = 'Kuvan otsikko')

## Erotussarjan dxt2 auto-ja osittaisautokorrelaatiotarkastelu
acf(dxt2, lag = 36, main = 'Kuvan otsikko')
pacf(dxt2, lag = 36, main = 'Kuvan otsikko')


### Mallin sovitus: Huomaa! ALKUPER�INEN sarja xt v�litet��n funktiolle ARIMA. Differointi spesifioidaan order-m��rittelyss�. 
malli1 <- arima(xt, order = c(p,d,q), seasonal = list(order = c(0,0,q)), method ='ML')
## Mallin parametreihin liittyv� informaatio on liitett�v� raporttiin. Kopioi tulokset konsolista. 
malli1
## T�m� kuvasarja on liitett�v� raporttiin 
tsdiag(malli1)

## Havaittu sarja ja sovitesarja samassa kuvassa liitet��n raporttiin.
sovite <- xt - malli1$residuals
ts.plot(xt, col = 'darkgrey', lwd = 2, main = 'Kuvan otsikko')
lines(sovite , col = 'black', lwd = 2)
