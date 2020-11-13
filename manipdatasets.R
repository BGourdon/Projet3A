# Title     : Manipulation_datasets
# Objective : Se familiariser avec les data
# Created by: BGourdon
# Created on: 11/11/2020


indicators<-read.csv2("Indicators.csv", dec=".", sep=";", header=T)
economicinfo<-read.csv2("economic_info.csv", dec=".", sep=";", header=T)
datacrop<-read.csv2("MAELIA_crop_raw.csv", dec=".", sep=";", header=T)
datalivestock<-read.csv2("MAELIA_livestock_raw.csv", dec=".", sep=";", header=T)

date <- c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017")
names(indicators) <- c("Scenario", "Level", "Indicator", date)
names(economicinfo) <- c("Group", "Variable", date)
names(datacrop) <- c("Scenario", "Year", "Farm", "Parcel", "Crop", "Yield", "Area", "Revenue", "Variablecost", "Energy", "ProteinkgN", "PDIN", "ProteinN.ton", "Nitrogen", "Phosphorus", "Potassium", "Activeingredient")
names(datalivestock) <- c("Scenario", "Year", "Farm", "Milkrevenue", "Feedcost")


summary(indicators)
summary(economicinfo)
summary(datacrop)
summary(datalivestock)

#étude de l'évolution des indicateurs pour une ferme, un groupe, le territoire, une année

baselinecrop <- subset(datacrop, Scenario == "Baseline situation")
coexistencecrop <- subset(datacrop, Scenario == "Coexistence scenario")
complementarycrop <- subset(datacrop, Scenario == "Complementarity scenario")
synergeticcrop <- subset(datacrop, Scenario == "Synergetic scenario")


#fonction qui crée un dataset avec un scénario, le level d'étude choisi, une culture et un indicateur
#pour le fichier output MAELIA crop
#entrées possibles
#WARNING : bien respecter l'orthographe et les majuscules, bien mettre les guillemets
#scenario : "Baseline situation" ; "Coexistence scenario" ; "Complementarity scenario" ; "Synergetic scenario"
#level : "arable" ; "livestock" ; "territory" ; "AF1" ; "AF2" ; ... ; "AF5" ; "LF1" ; "LF2"
#culture : "BarleyS" ; "BarleyW" ; "Buckwheat" ; "FavaB" ; "Flax" ; "Fodder" ; "gMaize" ; "Gpea" ; "Hay" ; "Hemp" ; "Lucern" ; "Lupin" ; "Mix_CerG" ; "OSR" ; "sMaize" ; "Tritic" ; "WheatW"
#indicateur : "Yield" ; "Area" ; "Revenue" ; "Variablecost" ; "Energy" ; "ProteinkgN" ; "PDIN" ; "ProteinN.ton" ; "Nitrogen" ; "Phosphorus" ; "Potassium" ; "Active ingredient"

dataset <- function(scenario, level, culture, indicateur) {
  if (level == "arable"){
      subset1 <- subset(datacrop, Scenario == scenario)
      subset2 <- subset(subset1, Farm == "AF1" | Farm == "AF2" | Farm == "AF3" | Farm == "AF4" | Farm =="AF5")
      subset3 <- subset(subset2, Crop == culture)
      subset4 <- subset(subset3, select = c("Scenario", "Year", "Farm", "Parcel", "Crop", indicateur))
  }else if (level == "livestock"){
      subset1 <- subset(datacrop, Scenario == scenario)
      subset2 <- subset(subset1, Farm == "LF1" | Farm == "LF2")
      subset3 <- subset(subset2, Crop == culture)
      subset4 <- subset(subset3, select = c("Scenario", "Year", "Farm", "Parcel", "Crop", indicateur))
  }else if (level == "territory"){
      subset1 <- subset(datacrop, Scenario == scenario)
      subset2 <- subset(subset1, Farm == "AF1" | Farm == "AF2" | Farm == "AF3" | Farm == "AF4" | Farm =="AF5" | Farm == "LF1" | Farm == "LF2")
      subset3 <- subset(subset2, Crop == culture)
      subset4 <- subset(subset3, select = c("Scenario", "Year", "Farm", "Parcel", "Crop", indicateur))
  }else{
      subset1 <- subset(datacrop, Scenario == scenario)
      subset2 <- subset(subset1, Farm == level)
      subset3 <- subset(subset2, Crop == culture)
      subset4 <- subset(subset3, select = c("Scenario", "Year", "Farm", "Parcel", "Crop", indicateur))
  }
  return(subset4)
}

#exemple avec l'étude du scénario Baseline, pour le groupement des fermes arable, la culture WheatW, et l'indicateur Yield

fichier <- dataset("Baseline situation", "arable", "WheatW", "Yield")
summary(fichier)
summary(fichier$Yield)


#fonction qui trace l'indicateur en fonction du temps pour plusieurs fermes dans un scenario et pour une culture donnee

tracescenariocultureindicateur <- function(scenario, level, culture, indicateur){


  fichier <- dataset(scenario, level, culture, indicateur)

  if (level == "arable"){

    y1 <- subset(fichier, Farm == "AF1", select = c("Year", indicateur))
    y2 <- subset(fichier, Farm == "AF2", select = c("Year", indicateur))
    y3 <- subset(fichier, Farm == "AF3", select = c("Year", indicateur))
    y4 <- subset(fichier, Farm == "AF4", select = c("Year", indicateur))
    y5 <- subset(fichier, Farm == "AF5", select = c("Year", indicateur))

    plot(y1$Year, y1[,2], col="red", type="l", xlab = "annees", ylab = "indicateur")
    par(new=T)
    plot(y2$Year, y2[,2], col="blue", type="l", xlab = "", ylab = "", axes = F)
    par(new=T)
    plot(y3$Year, y3[,2], col="green", type="l", xlab = "", ylab = "", axes = F)
    par(new=T)
    plot(y4$Year, y4[,2], col="yellow", type="l", xlab = "", ylab = "", axes = F)
    par(new=T)
    plot(y5$Year, y5[,2], col="purple", type="l", xlab = "", ylab = "", axes = F)

    legend("bottomright",legend=c("AF1","AF2", "AF3", "AF4", "AF5"),text.col=c("red","blue", "green", "yellow", "purple"))


  }else if (level == "livestock"){

    y1 <- subset(fichier, Farm == "LF1", select = c("Year", indicateur))
    y2 <- subset(fichier, Farm == "LF2", select = c("Year", indicateur))

    plot(y1$Year, y1[,2], col="red", type="l", xlab = "annees", ylab = "indicateur")
    par(new=T)
    plot(y2$Year, y2[,2], col="blue", type="l", xlab = "", ylab = "", axes = F)

    legend("bottomright",legend=c("LF1","LF2"),text.col=c("red","blue"))

  }else if (level == "territory"){


    y1 <- subset(fichier, Farm == "AF1", select = c("Year", indicateur))
    y2 <- subset(fichier, Farm == "AF2", select = c("Year", indicateur))
    y3 <- subset(fichier, Farm == "AF3", select = c("Year", indicateur))
    y4 <- subset(fichier, Farm == "AF4", select = c("Year", indicateur))
    y5 <- subset(fichier, Farm == "AF5", select = c("Year", indicateur))
    y6 <- subset(fichier, Farm == "LF1", select = c("Year", indicateur))
    y7 <- subset(fichier, Farm == "LF2", select = c("Year", indicateur))

    plot(y1$Year, y1[,2], col="red", type="l", xlab = "annees", ylab = "indicateur")
    par(new=T)
    plot(y2$Year, y2[,2], col="blue", type="l", xlab = "", ylab = "", axes = F)
    par(new=T)
    plot(y3$Year, y3[,2], col="green", type="l", xlab = "", ylab = "", axes = F)
    par(new=T)
    plot(y4$Year, y4[,2], col="yellow", type="l", xlab = "", ylab = "", axes = F)
    par(new=T)
    plot(y5$Year, y5[,2], col="purple", type="l", xlab = "", ylab = "", axes = F)
    par(new=T)
    plot(y6$Year, y6[,2], col="pink", type="l", xlab = "", ylab = "", axes = F)
    par(new=T)
    plot(y7$Year, y7[,2], col="grey", type="l", xlab = "", ylab = "", axes = F)

    legend("bottomright",legend=c("AF1","AF2", "AF3", "AF4", "AF5", "LF1", "LF2"),text.col=c("red","blue", "green", "yellow", "purple", "pink", "grey"))

  }else{

    y1 <- subset(fichier, Farm == level, select = c("Year", indicateur))
    plot(y1$Year, y1[,2], type="l", xlab = "annees", ylab = "indicateur")

  }
}

#x11()
#tracescenariocultureindicateur("Baseline situation", "arable", "WheatW", "Yield")

##utilisation du package ggplot2

install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)


with(datacrop, qplot(Year, Yield))

qplot(Year, Yield, data = datacrop) +
  facet_wrap(~ Farm)

ggplot(data = datacrop, aes(x=Year)) + geom_point(aes(y = Yield), color = "red") + geom_point(aes(y = Revenue), color = "blue")


#fonction qui creee un dataset avec un scenario, une ferme et un indicateur
fichierfarm <- function(scenario, farm, indicateur) {
  subset1 <- subset(datacrop, Scenario == scenario)
  subset2 <- subset(subset1, Farm == farm)
  subset3 <- subset(subset2, select = c("Year", "Crop", indicateur))
  return(subset3)
}

baselineAF1 <- fichierfarm("Baseline situation", "AF1", "Yield")

qplot(Year, Yield, data = baselineAF1) +
  facet_wrap(~ Crop)

#fonction qui cree un dataset avec un scenario, une culture et un indicateur
fichierculture <- function(scenario, crop, indicateur) {
  subset1 <- subset(datacrop, Scenario == scenario)
  subset2 <- subset(subset1, Crop == crop)
  subset3 <- subset(subset2, select = c("Year", "Farm", indicateur))
  return(subset3)
}

baselineWheatW <- fichierculture("Baseline situation", "WheatW", "Yield")

qplot(Year, Yield, data = baselineWheatW) + facet_wrap( ~ Farm)

ggplot(baselineAF1) + geom_boxplot(aes(x = Crop, y = Yield))

ggplot(baselineWheatW) + geom_boxplot(aes(x = Farm, y = Yield))

#fonction qui cree un dataset avec un scenario et un indicateur
fichierindicateur <- function(scenario, indicateur) {
  subset1 <- subset(datacrop, Scenario == scenario)
  subset2 <- subset(subset1, select = c("Year", "Crop", "Farm", indicateur))
  return(subset2)
}

baselineYield <- fichierindicateur("Baseline situation", "Yield")

ggplot(baselineAF1) +
  geom_line(aes(x = Year, y = Yield, color = Crop))

ggplot(baselineWheatW) +
  geom_line(aes(x = Year, y = Yield, color = Farm))

ggplot(baselineYield) +
  geom_line(aes(x = Year, y = Yield, color = Crop)) + facet_wrap( ~ Farm)



#fonction qui cree un dataset avec une ferme et un indicateur
fichierscenario <- function(farm, indicateur) {
  subset1 <- subset(datacrop, Farm == farm)
  subset2 <- subset(subset1, select = c("Scenario", "Year", "Crop", "Farm", indicateur))
  return(subset2)
}

YieldAF1 <- fichierscenario("AF1", "Yield")

ggplot(baselineYield) +
  geom_line(aes(x = Year, y = Yield, color = Crop)) + facet_wrap( ~ Farm)

ggplot(YieldAF1, aes(x=Year)) +
  geom_line(aes(x = Year, y = Yield, color = Crop)) + facet_wrap( ~ Scenario)


#fonction qui cree un dataset avec une culture et un indicateur
fichierscenario2 <- function(crop, indicateur) {
  subset1 <- subset(datacrop, Crop == crop)
  subset2 <- subset(subset1, select = c("Scenario", "Year", "Crop", "Farm", indicateur))
  return(subset2)
}

YieldWheatW <- fichierscenario2("WheatW", "Yield")

ggplot(YieldWheatW, aes(x=Year)) +
  geom_line(aes(x = Year, y = Yield, color = Farm)) + facet_wrap( ~ Scenario)

ggplot(YieldWheatW, aes(x=Year)) +
  geom_boxplot(aes(x = Farm, y = Yield)) + facet_wrap( ~ Scenario)

##Etude du fichier livestock
#on recree toutes les fonctions

#fonction qui cree un dataset avec un scenario et un indicateur


ggplot(datalivestock) +
  geom_line(aes(x = Year, y = Milkrevenue, color = Farm)) + facet_wrap( ~ Scenario)


ggplot(datalivestock) +
  geom_line(aes(x = Year, y = Feedcost, color = Farm)) + facet_wrap( ~ Scenario)

ggplot(datalivestock) +
  geom_boxplot(aes(x = Year, y = Milkrevenue, color = Farm)) + facet_wrap( ~ Scenario)

ggplot(datalivestock) +
  geom_boxplot(aes(x = Year, y = Feedcost, color = Farm)) + facet_wrap( ~ Scenario)