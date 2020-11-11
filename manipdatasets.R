# Title     : Manipulation_datasets
# Objective : Se familiariser avec les data
# Created by: BGourdon
# Created on: 11/11/2020


indicators<-read.csv2("Indicators.csv", dec=".", sep=";", header=T)
economicinfo<-read.csv2("economic_info.csv", dec=".", sep=";", header=T)
datacrop<-read.csv2("MAELIA_crop_raw.csv", dec=".", sep=";", header=T)
datalivestock<-read.csv2("MAELIA_livestock_raw.csv", dec=".", sep=";", header=T)

