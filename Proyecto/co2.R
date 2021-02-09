#EL IMPACTO DE LA PANDEMIA EN LAS EMISIONES DE CO2

#IMPORTACION DE LOS DATOS
library(readr)
co2<-read_csv("https://raw.githubusercontent.com/wabinai/BEDU-BD-Postworks-Proyecto/main/Proyecto/co2.csv")
View(co2)

#TRANSFORMACION DE VARIABLES
library(lubridate)
co2$date<-dmy(co2$date)
str(co2)

#SEPARACION DE BASE POR PAIS (WORLD, ROW, China, Spain, EU27 & UK, US, Japan, Brazil)
library(dplyr)
paisco2<-split(co2,co2$country)
View(paisco2)

#AGRUPACION POR PAIS, SECTOR, AÑO
#GRAFICOS POR PAIS
library(ggplot2)

#WORLD
world<-paisco2$'WORLD' %>%
  group_by(sector,year(date)) %>%
  summarise(total=sum(mtco2))

ggplot(world,aes(x=sector,y=total, fill=(factor(`year(date)`))))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Accent")+
  xlab("Sector")+ylab("MtCO2")+
  ggtitle("Emisiones de CO2 en el mundo")+labs(fill = "Year")

#ROW
row<-paisco2$'ROW' %>%
  group_by(sector,year(date)) %>%
  summarise(total=sum(mtco2))

ggplot(row,aes(x=sector,y=total, fill=(factor(`year(date)`))))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Set3")+
  xlab("Sector")+ylab("MtCO2")+
  ggtitle("Emisiones de CO2 en el resto del mundo")+labs(fill = "Year")

#CHINA
china<-paisco2$'China' %>%
  group_by(sector,year(date)) %>%
  summarise(total=sum(mtco2))

ggplot(china,aes(x=sector,y=total, fill=(factor(`year(date)`))))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Set2")+
  xlab("Sector")+ylab("MtCO2")+
  ggtitle("Emisiones de CO2 en China")+labs(fill = "Year")

#SPAIN
spain<-paisco2$'Spain' %>%
  group_by(sector,year(date)) %>%
  summarise(total=sum(mtco2))

ggplot(spain,aes(x=sector,y=total, fill=(factor(`year(date)`))))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Set1")+
  xlab("Sector")+ylab("MtCO2")+
  ggtitle("Emisiones de CO2 en España")+labs(fill = "Year")

#EU27 & UK
euuk<-paisco2$'EU27 & UK' %>%
  group_by(sector,year(date)) %>%
  summarise(total=sum(mtco2))

ggplot(euuk,aes(x=sector,y=total, fill=(factor(`year(date)`))))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Dark2")+
  xlab("Sector")+ylab("MtCO2")+
  ggtitle("Emisiones de CO2 en Europa y Reino Unido")+labs(fill = "Year")

#US
us<-paisco2$'US' %>%
  group_by(sector,year(date)) %>%
  summarise(total=sum(mtco2))

ggplot(us,aes(x=sector,y=total, fill=(factor(`year(date)`))))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Paired")+
  xlab("Sector")+ylab("MtCO2")+
  ggtitle("Emisiones de CO2 en Estados Unidos")+labs(fill = "Year")

#JAPAN
japan<-paisco2$'Japan' %>%
  group_by(sector,year(date)) %>%
  summarise(total=sum(mtco2))

ggplot(japan,aes(x=sector,y=total, fill=(factor(`year(date)`))))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Spectral")+
  xlab("Sector")+ylab("MtCO2")+
  ggtitle("Emisiones de CO2 en Japon")+labs(fill = "Year")

#BRAZIL
brazil<-paisco2$'Brazil' %>%
  group_by(sector,year(date)) %>%
  summarise(total=sum(mtco2))

ggplot(brazil,aes(x=sector,y=total, fill=(factor(`year(date)`))))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Reds")+
  xlab("Sector")+ylab("MtCO2")+
  ggtitle("Emisiones de CO2 en Brasil")+labs(fill = "Year")

#SERIES DE TIEMPO
library(forecast)

#WORLD
worldsec<-split(paisco2$'WORLD',paisco2$'WORLD'$sector)
str(worldsec)

worldtsda<-ts(worldsec$`Domestic Aviation`$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(worldtsda, ylab = "Metric Tons of CO2")

acf(worldtsda)
pacf(worldtsda)

auto.arima(worldtsda)
forewda<-(arima(worldtsda, order = c(3, 1, 4))) %>%
  forecast(h = 90) %>%
  plot

worldtsgt<-ts(worldsec$`Ground Transport`$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(worldtsgt, ylab = "Metric Tons of CO2")

auto.arima(worldtsgt)
forewgt<-(arima(worldtsgt, order = c(2, 1, 2))) %>%
  forecast(h = 90) %>%
  plot

worldtsi<-ts(worldsec$Industry$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(worldtsi, ylab = "Metric Tons of CO2")

auto.arima(worldtsi)
forewi <- (arima(worldtsi, order = c(3, 1, 3))) %>%
  forecast(h = 90) %>%
  plot

worldtsp<-ts(worldsec$Power$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(worldtsp, ylab = "Metric Tons of CO2")

auto.arima(worldtsp)
forewp <- (arima(worldtsp, order = c(5, 0, 2))) %>%
  forecast(h = 90) %>%
  plot

worldtsr<-ts(worldsec$Residential$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(worldtsr, ylab = "Metric Tons of CO2")
auto.arima(worldtsr)
forewr <- (arima(worldtsr, order = c(2, 2, 1))) %>%
  forecast(h = 90) %>%
  plot

#ROW
rowsec<-split(paisco2$'ROW',paisco2$'ROW'$sector)
str(rowsec)

rowtsda<-ts(rowsec$`Domestic Aviation`$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(rowtsda, ylab = "Metric Tons of CO2")

rowtsgt<-ts(rowsec$`Ground Transport`$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(rowtsgt, ylab = "Metric Tons of CO2")

rowtsi<-ts(rowsec$Industry$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(rowtsi, ylab = "Metric Tons of CO2")

rowtsp<-ts(rowsec$Power$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(rowtsp, ylab = "Metric Tons of CO2")

rowtsr<-ts(rowsec$Residential$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(rowtsr, ylab = "Metric Tons of CO2")

#EU27 & UK
euuksec<-split(paisco2$'EU27 & UK',paisco2$'EU27 & UK'$sector)
str(euuksec, ylab = "Metric Tons of CO2")

euuktsda<-ts(euuksec$`Domestic Aviation`$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(euuktsda, ylab = "Metric Tons of CO2")

euuktsgt<-ts(euuksec$`Ground Transport`$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(euuktsgt, ylab = "Metric Tons of CO2")

euuktsi<-ts(euuksec$Industry$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(euuktsi, ylab = "Metric Tons of CO2")

euuktsp<-ts(euuksec$Power$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(euuktsp, ylab = "Metric Tons of CO2")

euuktsr<-ts(euuksec$Residential$mtco2, start = c(2019,1), end = c(2020,365), fr = 365)
plot(euuktsr, ylab = "Metric Tons of CO2")

#CONTRASTE DE HIPÓTESIS

#H0: NO HAY DIFRENCIAS SIGNIFICATIVAS EN LAS EMISIONES DE CO2 POR AÑO
#H1: HAY DIFRENCIAS SIGNIFICATIVAS EN LAS EMISIONES DE CO2 POR AÑO
#NIVEL DE SIGNIFICANCIA: ALPHA = 0.05

#ANALISIS DE VARIANZA (PRUEBA ANOVA) -----> NO SE CUMPLEN LOS SUPUESTOS***

#WORLD

#Domestic Aviation
# anovada<-aov(mtco2 ~ year(date), data = worldsec$`Domestic Aviation`)
# summary(anovada)
# shapiro.test(anovada$residuals)
# plot(anovada$fitted.values,anovada$residuals)+abline(0,0)
# 
# #Ground Transport
# anovagt<-aov(mtco2 ~ year(date), data = worldsec$`Ground Transport`)
# summary(anovagt)
# shapiro.test(anovagt$residuals)
# plot(anovada$fitted.values,anovada$residuals)+abline(0,0)

#Industry
# anovai<-aov(mtco2 ~ year(date), data = worldsec$Industry)
# summary(anovai)
# shapiro.test(anovai$residuals)
# plot(anovada$fitted.values,anovada$residuals)+abline(0,0)
# 
# #Power
# anovap<-aov(mtco2 ~ year(date), data = worldsec$Power)
# summary(anovap)
# shapiro.test(anovap$residuals)
# plot(anovada$fitted.values,anovada$residuals)+abline(0,0)
# 
# #Residential
# anovar<-aov(mtco2 ~ year(date), data = worldsec$Residential)
# summary(anovar)
# shapiro.test(anovar$residuals)
# plot(anovada$fitted.values,anovada$residuals)+abline(0,0)

#***SE PROCEDE A HACER UNA PRUEBA NO PARAMÉTRICA KRUSKAL WALLIS

#Domestic Aviation
kruskal.test(mtco2 ~ year(date), data = worldsec$`Domestic Aviation`)

#Ground Transport
kruskal.test(mtco2 ~ year(date), data = worldsec$`Ground Transport`)

#Industry
kruskal.test(mtco2 ~ year(date), data = worldsec$Industry)

#Power
kruskal.test(mtco2 ~ year(date), data = worldsec$Power)

#Residential
kruskal.test(mtco2 ~ year(date), data = worldsec$Residential)
