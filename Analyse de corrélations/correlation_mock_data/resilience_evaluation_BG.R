rm(list = ls())
#-------------------------------------------------------------------------------------#
#-----------------------------Analysis of resilience----------------------------------#
#-------------------------------------------------------------------------------------#

# Author: M. DARDONVILLE adapted by B. GOURDON
# Date: 2020-12-30

#set working directory
setwd("C:/Users/Utilisateur/Desktop/Mines3A/Projet 3A/Data/Mock_data")

# Load libraries ---------------------------------------------------------------------

library(corrplot)
library(nlme)  
library(lme4)
library(mixOmics)
library(FactoMineR)
#library(mht)
library(gtable)
library(grid)
library(gridExtra)
library(tidyverse)
library(fmsb)
library(car)
library(lmtest)
library(RColorBrewer)
library(reshape2)
library(cowplot)
library(magrittr)
library(ggplot2)
library(slider)
library(mice)
set.seed(123)
library(randomForest)
library(randomForestExplainer)
#library(mvpart)
options( java.parameters = "-Xmx4g" )
require(XLConnect)
require(TeachingDemos)

# Functions ------------------------------------------------------------------------
monplot <- function(X, nom){
  # X est une matrice (pex tune.pls$MSEP), nom est le nom a afficher en ordonnee (pex "MSEP")
  rang <- range(as.vector(X))
  plot(X[1,], ylim=rang, xlab="Number of PLS-components", ylab=nom, col=1, type="b")
  for (i in 2:nrow(X)) {
    lines(X[i,], col=i, type="b")
  }
  legend("topright", rownames(X), col=1:nrow(X), lty=rep(1,nrow(X)))
}

myrescale <- function(x) (x-min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))# normalisation par la moyenne

# Import data ----------------------------------------------------------------------
alldata <- read.csv("C:/Users/Utilisateur/Desktop/Mines3A/Projet 3A/Data/Mock_data/Fichier_indicateurs.csv", sep =";")
data <- subset(alldata, select = -c(1))
colnames(data)[1] <- "agroecosyst"

summary(data) # check NA
data <- data %>%
  filter(Revenue != Inf | VariableCost != Inf) # filter Inf


attach(data)

# On concatène les noms des individus avec les années
nom.ind = paste(agroecosyst, substr(Year,3,4), Scen, SSP, RCP, sep=".")
indiv = agroecosyst

# On crée des tableaux pour visualiser le nombre d'individus pour des variables catégorielles
table(Year)
table(agroecosyst)

# Conversion des individus en facteur puis en numérique pour pouvoir les afficher avec des couleurs différentes
col.indiv = as.numeric(as.factor(indiv))

# Attribution de couleurs aux années et individus
col.annee = Year
col.annee[Year==2005]="lightblue1"
col.annee[Year==2006]="lightblue3"
col.annee[Year==2007]="lightblue4"
col.annee[Year==2008]="mediumpurple1"
col.annee[Year==2009]="mediumpurple3"
col.annee[Year==2010]="mediumpurple4"
col.annee[Year==2011]="blue"
col.annee[Year==2012]="blue1"
col.annee[Year==2013]="blue2"
col.annee[Year==2014]="blue3"
col.annee[Year==2015]="blue4"
col.annee[Year==2016]="darkblue"
col.annee[Year==2017]="gray0"

mycolours <- colorRampPalette(brewer.pal(8, "Set2"))(length(unique(indiv)))

# Création d'un tableau de variables de réponse appelé Y --------------------------------------
# Analyse descriptive des variables de réponse (dynamique, corrélation et ACP)
# Sélection des variables Y 
names(data)
Y= as.data.frame(data[,c(6:10)]) # Revenue (Rev/ha), VariableCost (VCost/ha), EnergyYield (MjKgDM/ha), ProteinYield (kg N/ha), Nefficiency (	
#number of kg of nitrogen applied /ha)
rownames(Y)=nom.ind


# Analyse des corrélations entre variables Y -------------------------------------------
jpeg("pairs_Y.jpg", width = 17, height = 17, units = "cm", res = 300)
pairs(Y, col=col.indiv) #
dev.off()

par(mfrow=c(1,1))
jpeg("corrplot_Y.jpg", width = 17, height = 17, units = "cm", res = 300)
corrplot(cor(Y, use = "pairwise.complete.obs"))
dev.off()

#round(cor(Y, use = "pairwise.complete.obs"),2)
cor.test(Y$VariableCost, Y$Revenue) # corrélées r = 0.922564
cor.test(Y$ProteinYield, Y$EnergyYield)# corrélées r =s 0.799196
cor.test(Y$ProteinYield, Y$VariableCost)

# On enlève les variables corrélées
Yacp = subset(Y, select = c(2, 4, 5)) 
rownames(Yacp) = nom.ind

# ACP des variables Y ------------------------------------------------------------------
pcaY = pca(Yacp, scale=T, ncomp=ncol(Yacp))
par(mfrow=c(1,1))
barplot(pcaY$sdev/sum(pcaY$sdev), main="Scaled PCA of Y")
jpeg("acpY_indiv.jpg", width = 15, height = 15, units = "cm", res = 300)
plotIndiv(pcaY, col = col.indiv)
dev.off()
jpeg("acpY_var.jpg", width = 15, height = 15, units = "cm", res = 300)
plotVar(pcaY)
dev.off()
pcaY$rotation

rm(Yacp, pcaY)

# graphes en ligne
jpeg("nefficiency.jpg", width = 15, height = 13, units = "cm", res = 300)
g2 =  ggplot(data, aes(x=Year, y=Nefficiency)) + geom_line(aes(colour=agroecosyst), size = 1)  +  
  theme_minimal() + scale_y_continuous("Input Nitrogen Efficiency") + theme(axis.title.x = element_text(size = 26)) +
  scale_color_manual(values = mycolours)+
  theme(axis.title.y = element_text(size = 20)) + theme(axis.text.x= element_text(size = 15)) +
  theme(axis.text.y= element_text(size = 19)) + theme(legend.text=element_text(size = 19)) +
  theme(legend.title=element_text(size = 19)) + theme(strip.text.x = element_text(size = 26)) +
  guides(colour = guide_legend(override.aes = list(size=3), title = NULL)) + theme(legend.position="none") +
  xlab("")
g2
dev.off()
jpeg("energyyield.jpg",width = 15, height = 13, units = "cm", res = 300)
g3 =  ggplot(data, aes(x=Year, y=EnergyYield)) + geom_line(aes(colour=agroecosyst), size = 1)  +  
  theme_minimal() + scale_y_continuous("Energy Yield") + theme(axis.title.x = element_text(size = 26)) +
  theme(axis.title.y = element_text(size = 20)) + theme(axis.text.x= element_text(size = 15)) +
  theme(axis.text.y= element_text(size = 19)) + theme(legend.text=element_text(size = 19)) +
  scale_color_manual(values = mycolours)+
  theme(legend.title=element_text(size = 19)) + theme(strip.text.x = element_text(size = 26)) +
  guides(colour = guide_legend(override.aes = list(size=3), title = NULL)) + theme(legend.position="none") +
  xlab("")
g3
dev.off()
jpeg("revenue.jpg", width = 15, height = 13, units = "cm", res = 300)
g4 =  ggplot(data, aes(x=Year, y=Revenue)) + geom_line(aes(colour=agroecosyst), size = 1)  +  
  theme_minimal() + scale_y_continuous("Revenue") + theme(axis.title.x = element_text(size = 26)) +
  theme(axis.title.y = element_text(size = 20)) + theme(axis.text.x= element_text(size = 15)) +
  theme(axis.text.y= element_text(size = 19)) + theme(legend.text=element_text(size = 19)) +
  scale_color_manual(values = mycolours)+
  theme(legend.title=element_text(size = 19)) + theme(strip.text.x = element_text(size = 26)) +
  guides(colour = guide_legend(override.aes = list(size=3), title = NULL)) + theme(legend.position="none") +
  xlab("")
g4
dev.off()

# jpeg("eff.jpg", width = 15, height = 13, units = "cm", res = 300)
# g4 =  ggplot(data, aes(x=Year, y=eff)) + geom_line(aes(colour=agroecosyst), size = 1)  +  
#   theme_minimal() + scale_y_continuous("Economic efficiency") + theme(axis.title.x = element_text(size = 26)) +
#   theme(axis.title.y = element_text(size = 20)) + theme(axis.text.x= element_text(size = 15)) +
#   theme(axis.text.y= element_text(size = 19)) + theme(legend.text=element_text(size = 19)) +
#   scale_color_manual(values = mycolours)+
#   theme(legend.title=element_text(size = 19)) + theme(strip.text.x = element_text(size = 26)) +
#   guides(colour = guide_legend(override.aes = list(size=3), title = NULL)) + theme(legend.position="none") +
#   xlab("")
# g4
# dev.off()


rm(g2,g3,g4)

#Création d'un tableau de variables d'exposition appelé X ---------------------------------------------

# Sélection des variables X
names(data)
X=as.data.frame(data[,c(3:5)])
rownames(X)=nom.ind

#Calcul des critères de dynamique ------------------------------------------------------------------
source("calcul_criteria_BG.R")

criteria_all <- criteriadynamics(agroecosyst, Year, Y)
rm(data_randomreg,data_resist,data_stab,data_temp,data_trend)
names(criteria_all)
criteria <- criteria_all[,-c(2:7,which(str_detect(colnames(criteria_all),
                                                  c("detrend|EI|RESf|RESr"))))]

#Corrplot criteria ####
dir.create(file.path("Criteria plots"), recursive = TRUE)
for(i in colnames(Y)){
  jpeg(paste0("Criteria plots/",i,"_criteria_cor.jpg"), width = 17, height = 17, units = "cm", res = 300)
  corrplot(cor(dplyr::select(criteria, starts_with(paste0(i,"."))), use = "pairwise.complete.obs"))
  dev.off()
}

# Random forest for each criteria ####
criteria_agg <- criteria[, -1] # for aggregated criteria

# criteria_agg <- criteria_agg[,which(str_detect(colnames(criteria_agg),
#                                                c("RES")))]
for(i in 1:dim(criteria_agg)[2]){
  fit <- randomForest(pull(criteria_agg[,i]) ~ ., data = X, na.action = na.exclude, ntree = 500, mtry = 2, importance = TRUE)
  print(colnames(criteria_agg)[i])
  print(fit)
}
  
  criteriaRES <- criteria_all[,which(str_detect(colnames(criteria_all),
                                                c("RESf|RESr")))]# for non aggregated criteria
  
  for(i in 1:dim(criteriaRES)[2]){
    fit <- randomForest(pull(criteriaRES[,i]) ~ ., data = X, na.action = na.exclude, ntree = 500, mtry = 2, importance = TRUE)
    print(colnames(criteriaRES)[i])
    print(fit)
  }

