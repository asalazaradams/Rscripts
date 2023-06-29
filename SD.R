library(readr)
encucisd<- read_csv("~/Documents/ESTADISTICAS MEXICO/ENCUCI2020_csv/conjunto_de_datos_ENCUCI_2020_SD/conjunto_de_datos/conjunto_de_datos_ENCUCI_2020_SD.csv

library(stringr)
encuci$FAC_SEL=gsub('\r','',encuci$FAC_SEL)
encuci$FAC_SEL=as.numeric(encuci$FAC_SEL)
