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
names(datacrop) <- c("Scenario", "Year", "Farm", "Parcel", "Crop", "Yield", "Area", "Revenue", "Variablecost", "Energy", "ProteinkgN", "PDIN", "ProteinN.ton", "Nitrogen", "Phosphorus", "Potassium", "Active ingredient")
names(datalivestock) <- c("Scenario", "Year", "Farm", "Milk revenue", "Feed cost")


summary(indicators)
summary(economicinfo)
summary(datacrop)
summary(datalivestock)

#étude de l'évolution des indicateurs pour une ferme, un groupe, le territoire, une année

baselinecrop <- subset(datacrop, Scenario == "Baseline situation")
coexistencecrop <- subset(datacrop, Scenario == "Coexistence scenario")
complementarycrop <- subset(datacrop, Scenario == "Complementarity scenario")
synergeticcrop <- subset(datacrop, Scenario == "Synergetic scenario")


#fonction qui crée un dataset avec un scénario, une ferme, une culture et un indicateur
fichier <- function(scenario, farm, culture, indicateur) {
  subset1 <- subset(datacrop, Scenario == scenario)
  subset2 <- subset(subset1, Farm == farm)
  subset3 <- subset(subset2, Crop == culture)
  subset4 <- subset(subset3, select = c("Scenario", "Year", "Farm", "Parcel", "Crop", indicateur))
  return(subset4)
}

#exemple avec le rendement pour le wheat pour la ferme AF1 pour la baseline situation
choix <- fichier("Baseline situation", "AF1", "WheatW", "Yield")
plot(choix$Year, choix$Yield, type = "l", xlab = "années", ylab = "Yield")