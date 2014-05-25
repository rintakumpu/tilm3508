###################################
# TILM3508 -harjoitustyö, R-koodi #
# Lasse Rintakumpu, 63555         #
# 25.5.2014                       #
###################################

# Asetetaan työhakemisto

wd <- "C:/Users/Lasse Rintakumpu/OneDrive/Dropbox/Edu/Statistics/TILM 3508 Aikasarjat ja indeksit/Harjoitustyö"
setwd(wd)

# Funktio kirjastojen asentamiselle / lataamiselle

lataa_kirjasto <- function(kirjasto) {
  if(kirjasto %in% rownames(installed.packages()) == FALSE)
  {install.packages(kirjasto)} 
  library(kirjasto, character.only = TRUE)
}

# Ladataan/asennetaan käytetyt kirjastot

lapply(c(""), lataa_kirjasto)

# Ladataan havaintoaineisto


