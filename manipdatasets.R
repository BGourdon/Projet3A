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

exdataset <- dataset("Baseline situation", "arable", "WheatW", "Yield")
summary(exdataset)
summary(exdataset$Yield)


#fonction qui trace l'indicateur en fonction du temps