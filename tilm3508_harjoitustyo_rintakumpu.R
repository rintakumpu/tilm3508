###################################
# TILM3508 -harjoitustyö, R-koodi #
# Lasse Rintakumpu, 63555         #
# 31.5.2014                       #
###################################

# Asetetaan työhakemisto

wd <- "C:/Users/Lasse Rintakumpu/OneDrive/Dropbox/Edu/Statistics/TILM 3508 Aikasarjat ja indeksit/Harjoitustyö"
wd <- "C:/Users/Lasse-laina/Dropbox/Edu/Statistics/TILM 3508 Aikasarjat ja indeksit/Harjoitustyö"
setwd(wd)

# Funktio kirjastojen asentamiselle / lataamiselle

lataa_kirjasto <- function(kirjasto) {
  if(kirjasto %in% rownames(installed.packages()) == FALSE)
  {install.packages(kirjasto)} 
  library(kirjasto, character.only = TRUE)
}

# Ladataan/asennetaan käytetyt kirjastot

lapply(c("TSA", "timeSeries"), lataa_kirjasto)

# Ladataan havaintoaineisto

# 1: Yo:n verkossa
aikasarjat <- read.table('\\utu.fi\verkkolevyt\Tilastotieteen kurssikansio\kalle\TILA08\Harjoitustyot\kevat_2014/sarjat.txt', header = TRUE)
# 2: Kaikkialla
aikasarjat <- read.table('https://raw.githubusercontent.com/rintakumpu/tilm3508/master/sarjat.txt', header = TRUE)

# Valitaan oma sarja (63555)
x <- aikasarjat[,'X63555']

## Luodaan havaintoaineistosta hypoteettinen kuukausisarja (f=12)
xt <- ts(x, start = c(2000, 1), frequency = 12)
t <- length(xt) # Sarjan pituus == 200

# Edetään varsinaiseen harjoitustyöhön ladatun ja aikasarjaksi muunnetun aineiston pohjalta =>

##################################
# 1. Sarjan kuvaileva tarkastelu #
##################################

plot(stl(xt, s.window = 'periodic'))

pdf('kuvaaja_dekomp.pdf')
dev.off()

# Kuvaajan perusteella sarja sisältää nousevan trendin
# => Yritetään eliminoida trendi 1. kertaluvun differensillä

xt.diff <- diff(xt, differences = 1) # Ei kausidifferointia, so. lag = l
ts.plot(xt.diff, main = 'Differoitu sarja (1. kertaluku)')

pdf('kuvaaja_sarja_diff1.pdf')
dev.off()

# Kuvaajan perusteella sarja sisältää edelleen nousevan trendin
# => Yritetään eliminoida trendi 2. kertaluvun differensillä

xt.diff <- diff(xt, differences = 2)
ts.plot(xt.diff, main = 'Differoitu sarja (2. kertaluku)')

pdf('kuvaaja_sarja_diff2.pdf')
dev.off()

plot(stl(xt.diff, s.window = 'periodic')) # Huom kausivaihtelu!

# => Trendi häviää toisen kertaluvun differoinnilla
# Testataan differoidun sarjan heteroskedastisuutta McLeod.Li.testillä:

McLeod.Li.test(y = xt.diff)


################################
# 2. Stationaarisuustarkastelu #
################################

# Tarkstellaan stationaarisuutta 

acf(xt.diff, lag = 36, main = 'xt.diff Autokorrelaatio')
pdf('kuvaaja_acf.pdf')
dev.off()

# => Otosautokorrelaatiofunktio vaimenee toisella viipeellä => Sarja vaikuttaa stationaariselta.

# Testataan stationarisuutta vielä laajennetulla Dickey-Fuller -testillä

# Valitaan hypoteesipariksi:
# H0: Sarja ei ole stationaarinen
# Hv: Sarja on stationaarinen

# Tehdään testi tasolla 0.05.

adf.test(xt.diff, alternative = c("stationary"))

# Saadaan testisuureen arvoksi -4.5022, joka antaa p-arvon < 0.01. 
# Hylätään H0 tasolla 0.05. 


###############################################################
# 3. Mallin identifiointi, sovitus ja diagnostinen tarkastelu #
###############################################################


pacf(xt.diff, lag = 36, main = 'xt. diff Osittaisautokorrelaatio')
pdf('kuvaaja_pacf.pdf')
dev.off()

# acf Kuolee viipeen kaksi jälkeen
# pacf Kuolee viipeen kaks jälkee
# => Viittaa AR2 tai MA2 -prosessiin

# Sovitetaan sarjaan ARIMA-mallia. Sarjan pituus t > 50, joten mallin sovitus on järkevää.

d <- 2 # 2. kertaluvun differenssi, aiemman päättelyn mukaan
p <- 0
q <- 2

malli1 <- arima(xt, order = c(p,d,q), seasonal = list(order = c(0,0,q-2)), method ='ML') # Parametrit sma1 ja sma2 poistettu
tsdiag(malli1) 

# Mallin parametrien merkitsevyys
(1-pnorm(abs(malli1$coef)/sqrt(diag(malli1$var.coef))))*2

# Jäännösten (osittais)autokorrelaatiokuvaajat

acf(malli1$residual, lag = 36, main = 'Jäännösten autokorrelaatio')
pdf('kuvaaja_acf_residual.pdf')
dev.off()

pacf(malli1$residual, lag = 36, main = 'Jäännösten osittaisautokorrelaatio')
pdf('kuvaaja_pacf_residual.pdf')
dev.off()

sovite <- xt - malli1$residuals # Luodaan sovite poistamalla sarjasta sovitetun mallin jäännökset
ts.plot(xt, col = 'red', lwd = 2, main = 'Havaittu ja sovitesarja')
lines(sovite , col = 'black', lwd = 2, lty = 2)

pdf('kuvaaja_sovite_havaittu.pdf')
dev.off()

Box.test(malli1$residuals, lag = 1, type = c("Ljung-Box"), fitdf = 0)

# Box-Ljung test
# data:  malli1$residuals
# X-squared = 0.0156, df = 1, p-value = 0.9005
