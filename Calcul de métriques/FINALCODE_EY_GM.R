setwd("C:/Users/Utilisateur/PycharmProjects/Projet3A/Calcul de métriques")

# Importation librairies ####

library(tidyverse)
library(reshape2)
library(cowplot)
library(magrittr)
library(ggplot2)
library(corrplot)
library(slider)
library(nlme)

# Creation datasets ####

datafarm <- read.csv2("PAs_farm_df.csv", dec=".", sep=";", header=T)
datater <- read.csv2("PAs_terr_df.csv", dec=".", sep=";", header=T)
dataEYfarm <- subset(datafarm, select = c(1, 2, 3, 4, 5, 6, 7, 8))
dataGMfarm <- subset(datafarm, select = c(1, 2, 3, 4, 5, 6, 7, 10))
dataEYter <- subset(datater, select = c(1, 2, 3, 4, 5, 6))
dataGMter <- subset(datater, select = c(1, 2, 3, 4, 5, 8))

# Fonctions de calcul de Criteria ####

levelfarm <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 8:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df%>%
      group_by(Farm, RCP, SSP)%>%
      mutate(levelFarm = mean(perf))%>%# mean level
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP, Year)%>%
      mutate(MoyGroup = mean(perf))%>%
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP)%>%
      mutate(levelGroup = mean(MoyGroup))%>%
      ungroup()
    
    df <- select(df, - MoyGroup)
    
    colnames(df)[c((ncol(df)-1): ncol(df))] <- c(paste0(name,".level_Farm"),paste0(name,".level_Group"))
    colnames(df)[i] <- name
  }
  return(df)
}

levelter <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 6:nperf){
    #df <- data
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df %>%
      group_by(RCP, SSP, Year)%>%
      mutate(MoyTer = mean(perf))%>%
      ungroup()
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(levelTerritory = mean(MoyTer))%>%
      ungroup()
    
    df <- select(df, - MoyTer)
    
    colnames(df)[ncol(df)] <- paste0(name,".level_Territory")
    colnames(df)[i] <- name
  }
  return(df)
}

trendfarm <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 8:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df%>%
      group_by(Farm, RCP, SSP)%>%
      mutate(TRENDf_Farm = if(all(is.na(perf))) NA else lm(perf ~ Year, na.action = na.exclude)$coefficients[[2]])%>% # slope of linear regression
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP, Year)%>%
      mutate(MoyGroup = mean(perf))%>%
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP)%>%
      mutate(TRENDf_Group = if(all(is.na(MoyGroup))) NA else lm(MoyGroup ~ Year, na.action = na.exclude)$coefficients[[2]])%>% # slope of linear regression
      ungroup()
    
    df <- select(df, - MoyGroup)
    
    colnames(df)[c((ncol(df)-1): ncol(df))] <- c(paste0(name,".TRENDf_Farm"),paste0(name,".TRENDf_Group"))
    colnames(df)[i] <- name
  }
  return(df)
  
}


trendter <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 6:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df %>%
      group_by(RCP, SSP, Year)%>%
      mutate(MoyTer = mean(perf))%>%
      ungroup()
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(TRENDf_Territory = if(all(is.na(MoyTer))) NA else lm(MoyTer ~ Year, na.action = na.exclude)$coefficients[[2]])%>% # slope of linear regression
      ungroup()
    
    df <- select(df, - MoyTer)
    
    colnames(df)[ncol(df)] <- paste0(name,".TRENDf_Territory")
    colnames(df)[i] <- name
  }
  return(df)
  
}

variabilityfarm_RSD <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 8:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df%>%
      group_by(Farm, RCP, SSP)%>%
      mutate(RSD_Farm = abs(sd(perf)/mean(perf))*100)%>% # relative standard deviation 
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP, Year)%>%
      mutate(MoyGroup = mean(perf))%>%
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP)%>%
      mutate(RSD_Group = abs(sd(MoyGroup)/mean(MoyGroup))*100)%>% # relative standard deviation 
      ungroup()
    
    df <- select(df, - MoyGroup)
    
    colnames(df)[c((ncol(df)-1): ncol(df))] <- c(paste0(name,".RSD_Farm"),paste0(name,".RSD_Group"))
    colnames(df)[i] <- name
  }  
  return(df)
} 

variabilityter_RSD <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 6:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df %>%
      group_by(RCP, SSP, Year)%>%
      mutate(MoyTer = mean(perf))%>%
      ungroup()
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(RSD_Territory = abs(sd(MoyTer)/mean(MoyTer))*100)%>% # relative standard deviation 
      ungroup()
    
    df <- select(df, - MoyTer)
    
    colnames(df)[ncol(df)] <- paste0(name,".RSD_Territory")
    colnames(df)[i] <- name
  }  
  return(df)
} 

variabilityfarm_SSR <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 8:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df%>%
      group_by(Farm, RCP, SSP)%>%
      mutate(SSR_Farm = sum((predict(lm(perf ~ Year, na.action = na.exclude)) - mean(perf))^2))%>% # sum of squared residuals 
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP, Year)%>%
      mutate(MoyGroup = mean(perf))%>%
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP)%>%
      mutate(SSR_Group = sum((predict(lm(MoyGroup ~ Year, na.action = na.exclude)) - mean(MoyGroup))^2))%>% # sum of squared residuals
      ungroup()
    
    df <- select(df, - MoyGroup)
    
    colnames(df)[c((ncol(df)-1): ncol(df))] <- c(paste0(name,".SSR_Farm"),paste0(name,".SSR_Group"))
    colnames(df)[i] <- name
  }  
  return(df)
}   

variabilityter_SSR <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 6:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df %>%
      group_by(RCP, SSP, Year)%>%
      mutate(MoyTer = mean(perf))%>%
      ungroup()
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(SSR_Territory = sum((predict(lm(MoyTer ~ Year, na.action = na.exclude)) - mean(MoyTer))^2))%>% # sum of squared residuals
      ungroup()
    
    df <- select(df, - MoyTer)
    
    colnames(df)[ncol(df)] <- paste0(name,".SSR_Territory")
    colnames(df)[i] <- name
  }  
  return(df)
}   

resistancefarm_disr <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 8:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df%>%
      group_by(Farm, RCP, SSP)%>%
      mutate(NOD_Farm = length(perf[perf < 0.75*slide_dbl(perf, mean, .before = 2)]))%>% # number of disruption 
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP, Year)%>%
      mutate(MoyGroup = mean(perf))%>%
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP)%>%
      mutate(NOD_Group = length(MoyGroup[MoyGroup < 0.75*slide_dbl(MoyGroup, mean, na.action = na.omit, .before = 2)]))%>% # number of disruption
      ungroup()
    
    df <- select(df, - MoyGroup)
    
    colnames(df)[c((ncol(df)-1): ncol(df))] <- c(paste0(name,".NOD_Farm"),paste0(name,".NOD_Group"))
    colnames(df)[i] <- name
  }  
  return(df)
  
}


resistanceter_disr <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 6:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df %>%
      group_by(RCP, SSP, Year)%>%
      mutate(MoyTer = mean(perf))%>%
      ungroup()
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(NOD_Territory = length(MoyTer[MoyTer < 0.75*slide_dbl(MoyTer, mean, na.action = na.omit, .before = 2)]))%>% # number of disruption 
      ungroup()
    
    df <- select(df, - MoyTer)
    
    colnames(df)[ncol(df)] <- paste0(name,".NOD_Territory")
    colnames(df)[i] <- name
  }  
  return(df)
  
}

resistancefarm_probapeak <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 8:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df%>%
      group_by(Farm, RCP, SSP)%>%
      mutate(probahigh_Farm = pnorm((1.25*mean(perf) - mean(perf))/sd(perf)))%>%# probability of high performance level with threshold adapted by individuals Macholdt et al 2020
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP, Year)%>%
      mutate(MoyGroup = mean(perf))%>%
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP)%>%
      mutate(probahigh_Group  = pnorm((1.25*mean(MoyGroup) - mean(MoyGroup))/sd(MoyGroup)))%>%# probability of high performance level with threshold adapted by individuals Macholdt et al 2020
      ungroup()
    
    df <- select(df, - MoyGroup)
    
    colnames(df)[c((ncol(df)-1): ncol(df))] <- c(paste0(name,".probahigh_Farm"),paste0(name,".probahigh_Group"))
    colnames(df)[i] <- name
  }  
  return(df)
  
}


resistanceter_probapeak <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 6:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df %>%
      group_by(RCP, SSP, Year)%>%
      mutate(MoyTer = mean(perf))%>%
      ungroup()
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(probahigh_Territory = pnorm((1.25*mean(MoyTer) - mean(MoyTer))/sd(MoyTer)))%>%# probability of high performance level with threshold adapted by individuals Macholdt et al 2020
      ungroup()
    
    df <- select(df, - MoyTer)
    
    colnames(df)[ncol(df)] <- paste0(name,".probahigh_Territory")
    colnames(df)[i] <- name
  }  
  return(df)
  
}

# Creation fichiers Criteria ####

criteriadynamics_farm <- function(datafarm, mod, nomfichierfarm){
  
  data_levelfarm <- levelfarm(datafarm, ncol(datafarm), mod)
  data_trendfarm <- trendfarm(data_levelfarm, ncol(datafarm), mod)
  data_varRSDfarm <- variabilityfarm_RSD(data_trendfarm, ncol(datafarm), mod)
  data_varSSRfarm <- variabilityfarm_SSR(data_varRSDfarm, ncol(datafarm), mod)
  data_resdisrfarm <- resistancefarm_disr(data_varSSRfarm, ncol(datafarm), mod)
  data_respeakfarm <- resistancefarm_probapeak(data_resdisrfarm, ncol(datafarm), mod)
  
  #Save file
  #write_excel_csv(data_respeakfarm, paste0(nomfichierfarm, mod, "Criteria_Data.csv"), delim = ";", na = "")
  write.table(data_respeakfarm, paste0(nomfichierfarm, mod, "Criteria_Data.csv"), append = FALSE, sep = ";", dec = ".", row.names = FALSE)
  
  return(data_respeakfarm)
}

criteriadynamics_ter <- function(datater, mod, nomfichierter){
  
  data_levelter <- levelter(datater, ncol(datater), mod)
  data_trendter <- trendter(data_levelter, ncol(datater), mod)
  data_varRSDter <- variabilityter_RSD(data_trendter, ncol(datater), mod)
  data_varSSRter <- variabilityter_SSR(data_varRSDter, ncol(datater), mod)
  data_resdisrter <- resistanceter_disr(data_varSSRter, ncol(datater), mod)
  data_respeakter <- resistanceter_probapeak(data_resdisrter, ncol(datater), mod)
  
  #Save file
  #write_excel_csv(data_respeakter, paste0(nomfichierter, mod, "Criteria_Data.csv"), delim = ";", na = "")
  write.table(data_respeakter, paste0(nomfichierter, mod, "Criteria_Data.csv"), append = FALSE, sep = ";", dec = ".", row.names = FALSE)
  
  return(data_respeakter)
}

#Pour le PA EY
EY_BLinefarm <- criteriadynamics_farm(dataEYfarm, "BLine", "EYfarm")
EY_BLineter <- criteriadynamics_ter(dataEYter, "BLine", "EYter")
EY_Synerfarm <- criteriadynamics_farm(dataEYfarm, "Syner", "EYfarm")
EY_Synerter <- criteriadynamics_ter(dataEYter, "Syner", "EYter")

#Pour le PA GM
criteriadynamics_farm(dataGMfarm, "BLine", "GMfarm")
criteriadynamics_ter(dataGMter, "BLine", "GMter")
criteriadynamics_farm(dataGMfarm, "Syner", "GMfarm")
criteriadynamics_ter(dataGMter, "Syner", "GMter")


# Transformation en tableau par m�trique par attribut ####

table <- function(dataPAfarm, dataPAter, mod, metrics){
  
  indicateurfarm <- subset(dataPAfarm, select = which(str_detect(colnames(dataPAfarm), metrics)))
  indicateurter <- subset(dataPAter, select = which(str_detect(colnames(dataPAter), metrics)))
  X <- subset(dataPAfarm, select= c("Farm", "Farm_group", "SSP", "RCP"))
  Y <- subset(dataPAter, select= c("RCP", "SSP"))
  df <- cbind(X, indicateurfarm)
  dg <- cbind(Y, indicateurter)
  
  col1 <- c(subset(df, Farm == "F1" & SSP == "SSP1" & RCP == "RCP2.6")[1,5], subset(df, Farm == "F2" & SSP == "SSP1" & RCP == "RCP2.6")[1,5],
            subset(df, Farm == "F3" & SSP == "SSP1" & RCP == "RCP2.6")[1,5], subset(df, Farm == "F4" & SSP == "SSP1" & RCP == "RCP2.6")[1,5],
            subset(df, Farm == "F5" & SSP == "SSP1" & RCP == "RCP2.6")[1,5], subset(df, Farm == "F6" & SSP == "SSP1" & RCP == "RCP2.6")[1,5],
            subset(df, Farm == "F8" & SSP == "SSP1" & RCP == "RCP2.6")[1,5], subset(df, Farm_group == "AraF_Far" & SSP == "SSP1" & RCP == "RCP2.6")[1,6],
            subset(df, Farm_group == "LivS_Far" & SSP == "SSP1" & RCP == "RCP2.6")[1,6], subset(dg, SSP == "SSP1" & RCP == "RCP2.6")[1,3])
  
  col2 <- c(subset(df, Farm == "F1" & SSP == "SSP2" & RCP == "RCP2.6")[1,5], subset(df, Farm == "F2" & SSP == "SSP2" & RCP == "RCP2.6")[1,5],
            subset(df, Farm == "F3" & SSP == "SSP2" & RCP == "RCP2.6")[1,5], subset(df, Farm == "F4" & SSP == "SSP2" & RCP == "RCP2.6")[1,5],
            subset(df, Farm == "F5" & SSP == "SSP2" & RCP == "RCP2.6")[1,5], subset(df, Farm == "F6" & SSP == "SSP2" & RCP == "RCP2.6")[1,5],
            subset(df, Farm == "F8" & SSP == "SSP2" & RCP == "RCP2.6")[1,5], subset(df, Farm_group == "AraF_Far" & SSP == "SSP2" & RCP == "RCP2.6")[1,6],
            subset(df, Farm_group == "LivS_Far" & SSP == "SSP2" & RCP == "RCP2.6")[1,6], subset(dg, SSP == "SSP2" & RCP == "RCP2.6")[1,3])
  
  col3 <- c(subset(df, Farm == "F1" & SSP == "SSP3" & RCP == "RCP2.6")[1,5], subset(df, Farm == "F2" & SSP == "SSP3" & RCP == "RCP2.6")[1,5],
            subset(df, Farm == "F3" & SSP == "SSP3" & RCP == "RCP2.6")[1,5], subset(df, Farm == "F4" & SSP == "SSP3" & RCP == "RCP2.6")[1,5],
            subset(df, Farm == "F5" & SSP == "SSP3" & RCP == "RCP2.6")[1,5], subset(df, Farm == "F6" & SSP == "SSP3" & RCP == "RCP2.6")[1,5],
            subset(df, Farm == "F8" & SSP == "SSP3" & RCP == "RCP2.6")[1,5], subset(df, Farm_group == "AraF_Far" & SSP == "SSP3" & RCP == "RCP2.6")[1,6],
            subset(df, Farm_group == "LivS_Far" & SSP == "SSP3" & RCP == "RCP2.6")[1,6], subset(dg, SSP == "SSP3" & RCP == "RCP2.6")[1,3])
  
  col4 <- c(subset(df, Farm == "F1" & SSP == "SSP1" & RCP == "RCP4.5")[1,5], subset(df, Farm == "F2" & SSP == "SSP1" & RCP == "RCP4.5")[1,5],
            subset(df, Farm == "F3" & SSP == "SSP1" & RCP == "RCP4.5")[1,5], subset(df, Farm == "F4" & SSP == "SSP1" & RCP == "RCP4.5")[1,5],
            subset(df, Farm == "F5" & SSP == "SSP1" & RCP == "RCP4.5")[1,5], subset(df, Farm == "F6" & SSP == "SSP1" & RCP == "RCP4.5")[1,5],
            subset(df, Farm == "F8" & SSP == "SSP1" & RCP == "RCP4.5")[1,5], subset(df, Farm_group == "AraF_Far" & SSP == "SSP1" & RCP == "RCP4.5")[1,6],
            subset(df, Farm_group == "LivS_Far" & SSP == "SSP1" & RCP == "RCP4.5")[1,6], subset(dg, SSP == "SSP1" & RCP == "RCP4.5")[1,3])
  
  col5 <- c(subset(df, Farm == "F1" & SSP == "SSP2" & RCP == "RCP4.5")[1,5], subset(df, Farm == "F2" & SSP == "SSP2" & RCP == "RCP4.5")[1,5],
            subset(df, Farm == "F3" & SSP == "SSP2" & RCP == "RCP4.5")[1,5], subset(df, Farm == "F4" & SSP == "SSP2" & RCP == "RCP4.5")[1,5],
            subset(df, Farm == "F5" & SSP == "SSP2" & RCP == "RCP4.5")[1,5], subset(df, Farm == "F6" & SSP == "SSP2" & RCP == "RCP4.5")[1,5],
            subset(df, Farm == "F8" & SSP == "SSP2" & RCP == "RCP4.5")[1,5], subset(df, Farm_group == "AraF_Far" & SSP == "SSP2" & RCP == "RCP4.5")[1,6],
            subset(df, Farm_group == "LivS_Far" & SSP == "SSP2" & RCP == "RCP4.5")[1,6], subset(dg, SSP == "SSP2" & RCP == "RCP4.5")[1,3])
  
  col6 <- c(subset(df, Farm == "F1" & SSP == "SSP3" & RCP == "RCP4.5")[1,5], subset(df, Farm == "F2" & SSP == "SSP3" & RCP == "RCP4.5")[1,5],
            subset(df, Farm == "F3" & SSP == "SSP3" & RCP == "RCP4.5")[1,5], subset(df, Farm == "F4" & SSP == "SSP3" & RCP == "RCP4.5")[1,5],
            subset(df, Farm == "F5" & SSP == "SSP3" & RCP == "RCP4.5")[1,5], subset(df, Farm == "F6" & SSP == "SSP3" & RCP == "RCP4.5")[1,5],
            subset(df, Farm == "F8" & SSP == "SSP3" & RCP == "RCP4.5")[1,5], subset(df, Farm_group == "AraF_Far" & SSP == "SSP3" & RCP == "RCP4.5")[1,6],
            subset(df, Farm_group == "LivS_Far" & SSP == "SSP3" & RCP == "RCP4.5")[1,6], subset(dg, SSP == "SSP3" & RCP == "RCP4.5")[1,3])
  
  col7 <- c(subset(df, Farm == "F1" & SSP == "SSP1" & RCP == "RCP8.5")[1,5], subset(df, Farm == "F2" & SSP == "SSP1" & RCP == "RCP8.5")[1,5],
            subset(df, Farm == "F3" & SSP == "SSP1" & RCP == "RCP8.5")[1,5], subset(df, Farm == "F4" & SSP == "SSP1" & RCP == "RCP8.5")[1,5],
            subset(df, Farm == "F5" & SSP == "SSP1" & RCP == "RCP8.5")[1,5], subset(df, Farm == "F6" & SSP == "SSP1" & RCP == "RCP8.5")[1,5],
            subset(df, Farm == "F8" & SSP == "SSP1" & RCP == "RCP8.5")[1,5], subset(df, Farm_group == "AraF_Far" & SSP == "SSP1" & RCP == "RCP8.5")[1,6],
            subset(df, Farm_group == "LivS_Far" & SSP == "SSP1" & RCP == "RCP8.5")[1,6], subset(dg, SSP == "SSP1" & RCP == "RCP8.5")[1,3])
  
  col8 <- c(subset(df, Farm == "F1" & SSP == "SSP2" & RCP == "RCP8.5")[1,5], subset(df, Farm == "F2" & SSP == "SSP2" & RCP == "RCP8.5")[1,5],
            subset(df, Farm == "F3" & SSP == "SSP2" & RCP == "RCP8.5")[1,5], subset(df, Farm == "F4" & SSP == "SSP2" & RCP == "RCP8.5")[1,5],
            subset(df, Farm == "F5" & SSP == "SSP2" & RCP == "RCP8.5")[1,5], subset(df, Farm == "F6" & SSP == "SSP2" & RCP == "RCP8.5")[1,5],
            subset(df, Farm == "F8" & SSP == "SSP2" & RCP == "RCP8.5")[1,5], subset(df, Farm_group == "AraF_Far" & SSP == "SSP2" & RCP == "RCP8.5")[1,6],
            subset(df, Farm_group == "LivS_Far" & SSP == "SSP2" & RCP == "RCP8.5")[1,6], subset(dg, SSP == "SSP2" & RCP == "RCP8.5")[1,3])
  
  col9 <- c(subset(df, Farm == "F1" & SSP == "SSP3" & RCP == "RCP8.5")[1,5], subset(df, Farm == "F2" & SSP == "SSP3" & RCP == "RCP8.5")[1,5],
            subset(df, Farm == "F3" & SSP == "SSP3" & RCP == "RCP8.5")[1,5], subset(df, Farm == "F4" & SSP == "SSP3" & RCP == "RCP8.5")[1,5],
            subset(df, Farm == "F5" & SSP == "SSP3" & RCP == "RCP8.5")[1,5], subset(df, Farm == "F6" & SSP == "SSP3" & RCP == "RCP8.5")[1,5],
            subset(df, Farm == "F8" & SSP == "SSP3" & RCP == "RCP8.5")[1,5], subset(df, Farm_group == "AraF_Far" & SSP == "SSP3" & RCP == "RCP8.5")[1,6],
            subset(df, Farm_group == "LivS_Far" & SSP == "SSP3" & RCP == "RCP8.5")[1,6], subset(dg, SSP == "SSP3" & RCP == "RCP8.5")[1,3])
  
  data <- data.frame(SSP1RCP26=col1, SSP2RCP26=col2, SSP3RCP26=col3, SSP1RCP45=col4, SSP2RCP45=col5, SSP3RCP45=col6, SSP1RCP85=col7, SSP2RCP85=col8, SSP3RCP85=col9)
  rownames(data) <- c("F1", "F2", "F3", "F4", "F5", "F6", "F8", "AraF_Far", "LivS_Far", "Territory")
  #Save file
  #write.csv2(data,paste0(PA, mod, metrics, ".table.csv"), append = FALSE, na = "", row.names = TRUE, col.names = TRUE)
  data
}

