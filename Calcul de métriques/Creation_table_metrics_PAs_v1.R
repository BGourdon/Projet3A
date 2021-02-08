setwd("C:/Users/Utilisateur/Desktop/Mines3A/Projet 3A/Data/PAs")

# Importation librairies 

library(tidyverse)
library(reshape2)
library(cowplot)
library(magrittr)
library(ggplot2)
library(corrplot)
library(slider)
library(nlme)

datafarm <- read.csv2("PAs_farm_df.csv", dec=".", sep=";", header=T)
datafield <- read.csv2("PAs_field_df.csv", dec=".", sep=";", header=T)
datater <- read.csv2("PAs_terr_df.csv", dec=".", sep=";", header=T)

# filter NA

levelfarm <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 8:nperf){

    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df%>%
      group_by(Farm, RCP, SSP)%>%
      mutate(levelFarm = mean(perf, na.rm = T))%>%# mean level
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP)%>%
      mutate(levelGroup = mean(perf, na.rm = T))%>%
      ungroup()
    
    colnames(df)[c((ncol(df)-1): ncol(df))] <- c(paste0(name,".level_Farm"),paste0(name,".level_Group"))
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
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
      group_by(RCP, SSP)%>%
      mutate(levelTerritory = mean(perf, na.rm = T))%>%
      ungroup()
    
    
    colnames(df)[ncol(df)] <- paste0(name,".level_Territory")
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
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
      group_by(Farm_group, RCP, SSP)%>%
      mutate(TRENDf_Group = if(all(is.na(perf))) NA else lm(perf ~ Year, na.action = na.exclude)$coefficients[[2]])%>% # slope of linear regression
      ungroup()
    
    colnames(df)[c((ncol(df)-1): ncol(df))] <- c(paste0(name,".TRENDf_Farm"),paste0(name,".TRENDf_Group"))
    colnames(df)[i] <- name
    
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
  }
  return(df)
  
}
    

trendter <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 6:nperf){

    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(TRENDf_Territory = if(all(is.na(perf))) NA else lm(perf ~ Year, na.action = na.exclude)$coefficients[[2]])%>% # slope of linear regression
      ungroup()
    
    colnames(df)[ncol(df)] <- paste0(name,".TRENDf_Territory")
    colnames(df)[i] <- name
    
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
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
      mutate(RSD_Farm = abs(sd(perf, na.rm = T)/mean(perf, na.rm = T))*100)%>% # relative standard deviation 
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP)%>%
      mutate(RSD_Group = abs(sd(perf, na.rm = T)/mean(perf, na.rm = T))*100)%>% # relative standard deviation 
      ungroup()
    
    colnames(df)[c((ncol(df)-1): ncol(df))] <- c(paste0(name,".RSD_Farm"),paste0(name,".RSD_Group"))
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
  }  
  return(df)
} 

variabilityter_RSD <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 6:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(RSD_Territory = abs(sd(perf, na.rm = T)/mean(perf, na.rm = T))*100)%>% # relative standard deviation 
      ungroup()
    
    colnames(df)[ncol(df)] <- paste0(name,".RSD_Territory")
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
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
      group_by(Farm_group, RCP, SSP)%>%
      mutate(SSR_Group = sum((predict(lm(perf ~ Year, na.action = na.exclude)) - mean(perf))^2))%>% # sum of squared residuals
      ungroup()
    
    colnames(df)[c((ncol(df)-1): ncol(df))] <- c(paste0(name,".SSR_Farm"),paste0(name,".SSR_Group"))
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
  }  
  return(df)
}   

variabilityter_SSR <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 6:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(SSR_Territory = sum((predict(lm(perf ~ Year, na.action = na.exclude)) - mean(perf))^2))%>% # sum of squared residuals
      ungroup()
    
    colnames(df)[ncol(df)] <- paste0(name,".SSR_Territory")
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
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
      mutate(NOD_Farm = length(na.omit(perf[perf < 0.75*slide_dbl(perf, mean, na.action = na.omit, .before = 2)])))%>% # number of disruption 
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP)%>%
      mutate(NOD_Group = length(na.omit(perf[perf < 0.75*slide_dbl(perf, mean, na.action = na.omit, .before = 2)])))%>% # number of disruption
      ungroup()
    
    colnames(df)[c((ncol(df)-1): ncol(df))] <- c(paste0(name,".NOD_Farm"),paste0(name,".NOD_Group"))
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
  }  
  return(df)
  
}
    

resistanceter_disr <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 6:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(NOD_Territory = length(na.omit(perf[perf < 0.75*slide_dbl(perf, mean, na.action = na.omit, .before = 2)])))%>% # number of disruption 
      ungroup()
    
    colnames(df)[ncol(df)] <- paste0(name,".NOD_Territory")
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
  }  
  return(df)
  
}

resistancefarm_probapeak <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 8:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    perchigh <- quantile(df$perf, c(.80), na.rm = T)
    
    df <- df%>%
      group_by(Farm, RCP, SSP)%>%
      mutate(probahigh_Farm = length(na.omit(perf[perf > perchigh]))/length(na.omit(perf)))%>%# probability of high performance level
      ungroup()
    
    df <- df %>%
      group_by(Farm_group, RCP, SSP)%>%
      mutate(probahigh_Group = length(na.omit(perf[perf > perchigh]))/length(na.omit(perf)))%>%# probability of high performance level
      ungroup()
    
    colnames(df)[c((ncol(df)-1): ncol(df))] <- c(paste0(name,".probahigh_Farm"),paste0(name,".probahigh_Group"))
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
  }  
  return(df)
  
}
    

resistanceter_probapeak <- function(data, nperf, mod){
  
  df <- subset(data, Mod == mod)
  
  for(i in 6:nperf){
    
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    perchigh <- quantile(df$perf, c(.80), na.rm = T)
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(probahigh_Territory = length(na.omit(perf[perf > perchigh]))/length(na.omit(perf)))%>%# probability of high performance level
      ungroup()
    
    colnames(df)[ncol(df)] <- paste0(name,".probahigh_Territory")
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
  }  
  return(df)
  
}

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

BLineFarm <- criteriadynamics_farm(datafarm, "BLine", "Farm_and_Group")
BLineTer <- criteriadynamics_ter(datater, "BLine", "Territory")
SynerFarm <- criteriadynamics_farm(datafarm, "Syner", "Farm_and_Group")
SynerTer <- criteriadynamics_ter(datater, "Syner", "Territory")


# Transformation en tableau par métrique par attribut ####
# 
table <- function(datafarm, datater, mod, PA, metrics){
  
  indicateurfarm <- subset(datafarm, select = which(str_detect(colnames(datafarm),(paste(PA, metrics, sep = ".")))))
  indicateurter <- subset(datater, select = which(str_detect(colnames(datater),(paste(PA, metrics, sep = ".")))))
  X <- subset(datafarm, select= c("Farm", "Farm_group", "SSP", "RCP"))
  Y <- subset(datater, select= c("RCP", "SSP"))
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
            subset(df, Farm == "F8" & SSP == "SSP1" & RCP == "RCP4.5")[1,5], subset(df, Farm_group == "AraF_Far" & SSP == "SSP1" & RCP == "RCP45")[1,6],
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
  write.csv2(data,paste0(mod, PA, metrics, ".table.csv"), append = FALSE, na = "", row.names = TRUE, col.names = TRUE)
  data
}

indicateurfarmTEST <- subset(BLineFarm, select = which(str_detect(colnames(BLineFarm),(paste("PA1_EY", "level", sep = ".")))))
indicateurterTEST <- subset(BLineTer, select = which(str_detect(colnames(BLineTer),(paste("PA1_EY", "level", sep = ".")))))
XTEST <- subset(BLineFarm, select= c("Farm", "Farm_group", "SSP", "RCP"))
YTEST <- subset(BLineTer, select= c("RCP", "SSP"))
dfTEST <- cbind(XTEST, indicateurfarmTEST)
dgTEST <- cbind(YTEST, indicateurterTEST)

BLinePA1_EYlevel <- table(BLineFarm, BLineTer, "BLine", "PA1_EY", "level")
BLinePA1_EYTRENDf <- table(BLineFarm, BLineTer, "BLine", "PA1_EY", "TRENDf")
BLinePA1_EYRSD <- table(BLineFarm, BLineTer, "BLine", "PA1_EY", "RSD")
BLinePA1_EYSSR <- table(BLineFarm, BLineTer, "BLine", "PA1_EY", "SSR")
BLinePA1_EYNOD <- table(BLineFarm, BLineTer, "BLine", "PA1_EY", "NOD")
BLinePA1_EYprobahigh <- table(BLineFarm, BLineTer, "BLine", "PA1_EY", "probahigh")

BLinePA2_PYlevel <- table(BLineFarm, BLineTer, "BLine", "PA2_PY", "level")
BLinePA2_PYTRENDf <- table(BLineFarm, BLineTer, "BLine", "PA2_PY", "TRENDf")
BLinePA2_PYRSD<- table(BLineFarm, BLineTer, "BLine", "PA2_PY", "RSD")
BLinePA2_PYSSR <- table(BLineFarm, BLineTer, "BLine", "PA2_PY", "SSR")
BLinePA2_PYNOD <- table(BLineFarm, BLineTer, "BLine", "PA2_PY", "NOD")
BLinePA2_PYprobahigh <- table(BLineFarm, BLineTer, "BLine", "PA2_PY", "probahigh")

BLinePA3_GMlevel <- table(BLineFarm, BLineTer, "BLine", "PA3_GM", "level")
BLinePA3_GMTRENDf <- table(BLineFarm, BLineTer, "BLine", "PA3_GM", "TRENDf")
BLinePA3_GMRSD <- table(BLineFarm, BLineTer, "BLine", "PA3_GM", "RSD")
BLinePA3_GMSSR <- table(BLineFarm, BLineTer, "BLine", "PA3_GM", "SSR")
BLinePA3_GMNOD <- table(BLineFarm, BLineTer, "BLine", "PA3_GM", "NOD")
BLinePA3_GMprobahigh <- table(BLineFarm, BLineTer, "BLine", "PA3_GM", "probahigh")

BLinePA4_EDlevel <- table(BLineFarm, BLineTer, "BLine", "PA4_ED", "level")
BLinePA4_EDTRENDf <- table(BLineFarm, BLineTer, "BLine", "PA4_ED", "TRENDf")
BLinePA4_EDRSD <- table(BLineFarm, BLineTer, "BLine", "PA4_ED", "RSD")
BLinePA4_EDSSR <- table(BLineFarm, BLineTer, "BLine", "PA4_ED", "SSR")
BLinePA4_EDNOD <- table(BLineFarm, BLineTer, "BLine", "PA4_ED", "NOD")
BLinePA4_EDprobahigh <- table(BLineFarm, BLineTer, "BLine", "PA4_ED", "probahigh")

BLinePA5_NSlevel <- table(BLineFarm, BLineTer, "BLine", "PA5_NS", "level")
BLinePA5_NSTRENDf <- table(BLineFarm, BLineTer, "BLine", "PA5_NS", "TRENDf")
BLinePA5_NSRSD <- table(BLineFarm, BLineTer, "BLine", "PA5_NS", "RSD")
BLinePA5_NSSSR <- table(BLineFarm, BLineTer, "BLine", "PA5_NS", "SSR")
BLinePA5_NSNOD <- table(BLineFarm, BLineTer, "BLine", "PA5_NS", "NOD")
BLinePA5_NSprobahigh <- table(BLineFarm, BLineTer, "BLine", "PA5_NS", "probahigh")


BLinePA6_SSlevel <- table(BLineFarm, BLineTer, "BLine", "PA6_SS", "level")
BLinePA6_SSTRENDf <- table(BLineFarm, BLineTer, "BLine", "PA6_SS", "TRENDf")
BLinePA6_SSRSD <- table(BLineFarm, BLineTer, "BLine", "PA6_SS", "RSD")
BLinePA6_SSSSR <- table(BLineFarm, BLineTer, "BLine", "PA6_SS", "SSR")
BLinePA6_SSNOD <- table(BLineFarm, BLineTer, "BLine", "PA6_SS", "NOD")
BLinePA6_SSprobahigh <- table(BLineFarm, BLineTer, "BLine", "PA6_SS", "probahigh")

BLinePA7_CSlevel <- table(BLineFarm, BLineTer, "BLine", "PA7_CS", "level")
BLinePA7_CSTRENDf <- table(BLineFarm, BLineTer, "BLine", "PA7_CS", "TRENDf")
BLinePA7_CSRSD <- table(BLineFarm, BLineTer, "BLine", "PA7_CS", "RSD")
BLinePA7_CSSSR <- table(BLineFarm, BLineTer, "BLine", "PA7_CS", "SSR")
BLinePA7_CSNOD <- table(BLineFarm, BLineTer, "BLine", "PA7_CS", "NOD")
BLinePA7_CSprobahigh <- table(BLineFarm, BLineTer, "BLine", "PA7_CS", "probahigh")

BLinePA8_WQRlevel <- table(BLineFarm, BLineTer, "BLine", "PA8_WQR", "level")
BLinePA8_WQRTRENDf <- table(BLineFarm, BLineTer, "BLine", "PA8_WQR", "TRENDf")
BLinePA8_WQRRSD <- table(BLineFarm, BLineTer, "BLine", "PA8_WQR", "RSD")
BLinePA8_WQRSSR <- table(BLineFarm, BLineTer, "BLine", "PA8_WQR", "SSR")
BLinePA8_WQRNOD <- table(BLineFarm, BLineTer, "BLine", "PA8_WQR", "NOD")
BLinePA8_WQRprobahigh <- table(BLineFarm, BLineTer, "BLine", "PA8_WQR", "probahigh")

BLinePA9_PAIlevel <- table(BLineFarm, BLineTer, "BLine", "PA9_PAI", "level")
BLinePA9_PAITRENDf <- table(BLineFarm, BLineTer, "BLine", "PA9_PAI", "TRENDf")
BLinePA9_PAIRSD <- table(BLineFarm, BLineTer, "BLine", "PA9_PAI", "RSD")
BLinePA9_PAISSR <- table(BLineFarm, BLineTer, "BLine", "PA9_PAI", "SSR")
BLinePA9_PAINOD <- table(BLineFarm, BLineTer, "BLine", "PA9_PAI", "NOD")
BLinePA9_PAIprobahigh <- table(BLineFarm, BLineTer, "BLine", "PA9_PAI", "probahigh")

BLinePA10_NLlevel <- table(BLineFarm, BLineTer, "BLine", "PA10_NL", "level")
BLinePA10_NLTRENDf <- table(BLineFarm, BLineTer, "BLine", "PA10_NL", "TRENDf")
BLinePA10_NLRSD <- table(BLineFarm, BLineTer, "BLine", "PA10_NL", "RSD")
BLinePA10_NLSSR <- table(BLineFarm, BLineTer, "BLine", "PA10_NL", "SSR")
BLinePA10_NLNOD <- table(BLineFarm, BLineTer, "BLine", "PA10_NL", "NOD")
BLinePA10_NLprobahigh <- table(BLineFarm, BLineTer, "BLine", "PA10_NL", "probahigh")

BLinePA11_GHGlevel <- table(BLineFarm, BLineTer, "BLine", "PA11_GHG", "level")
BLinePA11_GHGTRENDf <- table(BLineFarm, BLineTer, "BLine", "PA11_GHG", "TRENDf")
BLinePA11_GHGRSD <- table(BLineFarm, BLineTer, "BLine", "PA11_GHG", "RSD")
BLinePA11_GHGSSR <- table(BLineFarm, BLineTer, "BLine", "PA11_GHG", "SSR")
BLinePA11_GHGNOD <- table(BLineFarm, BLineTer, "BLine", "PA11_GHG", "NOD")
BLinePA11_GHGprobahigh <- table(BLineFarm, BLineTer, "BLine", "PA11_GHG", "probahigh")

BLinePA12_WLlevel <- table(BLineFarm, BLineTer, "BLine", "PA12_WL", "level")
BLinePA12_WLTRENDf <- table(BLineFarm, BLineTer, "BLine", "PA12_WL", "TRENDf")
BLinePA12_WLRSD <- table(BLineFarm, BLineTer, "BLine", "PA12_WL", "RSD")
BLinePA12_WLSSR <- table(BLineFarm, BLineTer, "BLine", "PA12_WL", "SSR")
BLinePA12_WLNOD <- table(BLineFarm, BLineTer, "BLine", "PA12_WL", "NOD")
BLinePA12_WLprobahigh <- table(BLineFarm, BLineTer, "BLine", "PA12_WL", "probahigh")

PA <- names(BLineFarm)[8:19]
metrics <- c("level", "TRENDf", "RSD", "SSR", "NOD", "probahigh")

ggplot(mtcars, aes(x=wt, y=mpg, shape=cyl, color=cyl)) +
  geom_point()
