# Importation librairies 

library(tidyverse)
library(reshape2)
library(cowplot)
library(magrittr)
library(ggplot2)
library(corrplot)
library(slider)
library(nlme)

# Creation du fichier ####
setwd("C:/Users/Utilisateur/Desktop/Mines3A/Projet 3A/Data/Mock_data")

dataRCP26 <- read.csv2("Prod_farm_final_RCP26.csv", dec=".", sep=";", header=T)
dataRCP45 <- read.csv2("Prod_farm_final_RCP45.csv", dec=".", sep=";", header=T) 
dataRCP85 <- read.csv2("Prod_farm_final_RCP85.csv", dec=".", sep=";", header=T)
indicators <- bind_rows(dataRCP26, dataRCP45, dataRCP85)

# Creation des sous fichiers

indRCP26 <- subset(dataRCP26, select = c(1, 2, 3, 4, 6, 8, 11, 13, 14, 16, 20))
colnames(indRCP26) <- c("Scen", "Year", "Farm", "SSP", "Revenue", "VariableCost", "Farm_Group", "EnergyYield", "ProteinYield", "Nefficiency", "RCP")
RCP26 <- indRCP26 %>% mutate(System = paste(indRCP26$Scen, indRCP26$RCP, indRCP26$SSP, indRCP26$Farm, indRCP26$Year, sep=""))
RCP26 <- subset(RCP26, select = c("System", "Farm", "Farm_Group", "Year", "Scen", "SSP", "RCP", "Revenue", "VariableCost", "EnergyYield", "ProteinYield", "Nefficiency"))

indRCP45 <- subset(dataRCP45, select = c(1, 2, 3, 4, 6, 8, 11, 13, 14, 16, 20))
colnames(indRCP45) <- c("Scen", "Year", "Farm", "SSP", "Revenue", "VariableCost", "Farm_Group", "EnergyYield", "ProteinYield", "Nefficiency", "RCP")
RCP45 <- indRCP45 %>% mutate(System = paste(indRCP45$Scen, indRCP45$RCP, indRCP45$SSP, indRCP45$Farm, indRCP45$Year, sep=""))
RCP45 <- subset(RCP45, select = c("System", "Farm", "Farm_Group", "Year", "Scen", "SSP", "RCP", "Revenue", "VariableCost", "EnergyYield", "ProteinYield", "Nefficiency"))

indRCP85 <- subset(dataRCP85, select = c(1, 2, 3, 4, 6, 8, 11, 13, 14, 16, 20))
colnames(indRCP85) <- c("Scen", "Year", "Farm", "SSP", "Revenue", "VariableCost", "Farm_Group", "EnergyYield", "ProteinYield", "Nefficiency", "RCP")
RCP85 <- indRCP85 %>% mutate(System = paste(indRCP85$Scen, indRCP85$RCP, indRCP85$SSP, indRCP85$Farm, indRCP85$Year, sep=""))
RCP85 <- subset(RCP85, select = c("System", "Farm", "Farm_Group", "Year", "Scen", "SSP", "RCP", "Revenue", "VariableCost", "EnergyYield", "ProteinYield", "Nefficiency"))

fichier <- bind_rows(RCP26, RCP45, RCP85)

summary(fichier) # check NA
fichier <- fichier %>%
  filter(Revenue != Inf | VariableCost != Inf) # filter Inf

dataBLine <- subset(fichier, Scen == "BLine")
dataSiner <- subset(fichier, Scen == "Siner")

# Fonctions ####

level <- function(df, nperf){
  for(i in 8:nperf){
    #df <- data
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df%>%
      group_by(Farm, RCP, SSP)%>%
      mutate(levelFarm = mean(perf, na.rm = T))%>%# mean level
      ungroup()
    
    df <- df %>%
      group_by(Farm_Group, RCP, SSP)%>%
      mutate(levelGroup = mean(perf, na.rm = T))%>%
      ungroup()
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(levelTerritory = mean(perf, na.rm = T))%>%
      ungroup()

    
    colnames(df)[c((ncol(df)-2): ncol(df))] <- c(paste0(name,".level_Farm"),paste0(name,".level_Group"),paste0(name,".level_Territory"))
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
  }
  return(df)
}

trend <- function(df, nperf){
  for(i in 8:nperf){
    #df <- data
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df%>%
      group_by(Farm, RCP, SSP)%>%
      mutate(TRENDf_Farm = if(all(is.na(perf))) NA else lm(perf ~ Year, na.action = na.exclude)$coefficients[[2]])%>% # slope of linear regression
      ungroup()
    
    df <- df %>%
      group_by(Farm_Group, RCP, SSP)%>%
      mutate(TRENDf_Group = if(all(is.na(perf))) NA else lm(perf ~ Year, na.action = na.exclude)$coefficients[[2]])%>% # slope of linear regression
      ungroup()
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(TRENDf_Territory = if(all(is.na(perf))) NA else lm(perf ~ Year, na.action = na.exclude)$coefficients[[2]])%>% # slope of linear regression
      ungroup()
    
    colnames(df)[c((ncol(df)-2): ncol(df))] <- c(paste0(name,".TRENDf_Farm"),paste0(name,".TRENDf_Group"),paste0(name,".TRENDf_Territory"))
    colnames(df)[i] <- name
    
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
  }
  return(df)
  
}
    
variability_RSD <- function(df, nperf){
  for(i in 8:nperf){
    #df <- data
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df%>%
      group_by(Farm, RCP, SSP)%>%
      mutate(RSD_Farm = abs(sd(perf, na.rm = T)/mean(perf, na.rm = T))*100)%>% # relative standard deviation 
      ungroup()
    
    df <- df %>%
      group_by(Farm_Group, RCP, SSP)%>%
      mutate(RSD_Group = abs(sd(perf, na.rm = T)/mean(perf, na.rm = T))*100)%>% # relative standard deviation 
      ungroup()
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(RSD_Territory = abs(sd(perf, na.rm = T)/mean(perf, na.rm = T))*100)%>% # relative standard deviation 
      ungroup()
    
    colnames(df)[c((ncol(df)-2): ncol(df))] <- c(paste0(name,".RSD_Farm"),paste0(name,".RSD_Group"),paste0(name,".RSD_Territory"))
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
  }  
  return(df)
}    
    
    

variability_SSR <- function(df, nperf){
  for(i in 8:nperf){
    #df <- data
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df%>%
      group_by(Farm, RCP, SSP)%>%
      mutate(SSR_Farm = sum((predict(lm(perf ~ Year, na.action = na.exclude)) - mean(perf))^2))%>% # sum of squared residuals 
      ungroup()
    
    df <- df %>%
      group_by(Farm_Group, RCP, SSP)%>%
      mutate(SSR_Group = sum((predict(lm(perf ~ Year, na.action = na.exclude)) - mean(perf))^2))%>% # sum of squared residuals
      ungroup()
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(SSR_Territory = sum((predict(lm(perf ~ Year, na.action = na.exclude)) - mean(perf))^2))%>% # sum of squared residuals
      ungroup()
    
    colnames(df)[c((ncol(df)-2): ncol(df))] <- c(paste0(name,".SSR_Farm"),paste0(name,".SSR_Group"),paste0(name,".SSR_Territory"))
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
  }  
  return(df)
}    

resistance_disr <- function(df, nperf){
  for(i in 8:nperf){
    #df <- data
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
   
    df <- df%>%
      group_by(Farm, RCP, SSP)%>%
      mutate(NOD_Farm = length(na.omit(perf[perf < 0.75*slide_dbl(perf, mean, na.action = na.omit, .before = 2)])))%>% # number of disruption 
      ungroup()
    
    df <- df %>%
      group_by(Farm_Group, RCP, SSP)%>%
      mutate(NOD_Group = length(na.omit(perf[perf < 0.75*slide_dbl(perf, mean, na.action = na.omit, .before = 2)])))%>% # number of disruption
      ungroup()
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(NOD_Territory = length(na.omit(perf[perf < 0.75*slide_dbl(perf, mean, na.action = na.omit, .before = 2)])))%>% # number of disruption 
      ungroup()
    
    colnames(df)[c((ncol(df)-2): ncol(df))] <- c(paste0(name,".NOD_Farm"),paste0(name,".NOD_Group"),paste0(name,".NOD_Territory"))
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
  }  
  return(df)

}
    
    
resistance_probapeak <- function(df, nperf){
  for(i in 8:nperf){
    #df <- data
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    perchigh <- quantile(df$perf, c(.80), na.rm = T)
    
    df <- df%>%
      group_by(Farm, RCP, SSP)%>%
      mutate(probahigh_Farm = length(na.omit(perf[perf > perchigh]))/length(na.omit(perf)))%>%# probability of high performance level
      ungroup()
    
    df <- df %>%
      group_by(Farm_Group, RCP, SSP)%>%
      mutate(probahigh_Group = length(na.omit(perf[perf > perchigh]))/length(na.omit(perf)))%>%# probability of high performance level
      ungroup()
    
    df <- df %>%
      group_by(RCP, SSP)%>%
      mutate(probahigh_Territory = length(na.omit(perf[perf > perchigh]))/length(na.omit(perf)))%>%# probability of high performance level
      ungroup()
    
    colnames(df)[c((ncol(df)-2): ncol(df))] <- c(paste0(name,".probahigh_Farm"),paste0(name,".probahigh_Group"),paste0(name,".probahigh_Territory"))
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
  }  
  return(df)
  
}


criteriadynamics <- function(data, nomfichier){
  
  nperf <- ncol(data)
  
  # Criteria calculation
  
  data_level <- level(data, nperf)
  data_trend <- trend(data_level, nperf)
  data_varRSD <- variability_RSD(data_trend, nperf)
  data_varSSR <- variability_SSR(data_varRSD, nperf)
  data_resdisr <- resistance_disr(data_varSSR, nperf)
  data_respeak <- resistance_probapeak(data_resdisr, nperf)
  names(data_respeak)

  #Save file
  write_excel_csv(data_respeak,paste0(nomfichier, "Criteria_Data.csv"), delim = ";", na = "")

  return(data_respeak)
}


# Transformation en tableau par métrique par attribut ####
# 
table <- function(data, PA, metrics){
  
  indicateur <- subset(data, select = which(str_detect(colnames(data),(paste(PA, metrics, sep = ".")))))
  X <- subset(data, select= c("Farm", "Farm_Group", "SSP", "RCP"))
  df <- cbind(X, indicateur)
  
    col1 <- c(subset(df, Farm == "F1" & SSP == "SSP1" & RCP == "RCP26")[1,5], subset(df, Farm == "F2" & SSP == "SSP1" & RCP == "RCP26")[1,5],
              subset(df, Farm == "F3" & SSP == "SSP1" & RCP == "RCP26")[1,5], subset(df, Farm == "F4" & SSP == "SSP1" & RCP == "RCP26")[1,5],
              subset(df, Farm == "F5" & SSP == "SSP1" & RCP == "RCP26")[1,5], subset(df, Farm == "F6" & SSP == "SSP1" & RCP == "RCP26")[1,5],
              subset(df, Farm == "F8" & SSP == "SSP1" & RCP == "RCP26")[1,5], subset(df, Farm_Group == "AraF_Far" & SSP == "SSP1" & RCP == "RCP26")[1,6],
              subset(df, Farm_Group == "LivS_Far" & SSP == "SSP1" & RCP == "RCP26")[1,6], subset(df, SSP == "SSP1" & RCP == "RCP26")[1,7])
    
    col2 <- c(subset(df, Farm == "F1" & SSP == "SSP2" & RCP == "RCP26")[1,5], subset(df, Farm == "F2" & SSP == "SSP2" & RCP == "RCP26")[1,5],
              subset(df, Farm == "F3" & SSP == "SSP2" & RCP == "RCP26")[1,5], subset(df, Farm == "F4" & SSP == "SSP2" & RCP == "RCP26")[1,5],
              subset(df, Farm == "F5" & SSP == "SSP2" & RCP == "RCP26")[1,5], subset(df, Farm == "F6" & SSP == "SSP2" & RCP == "RCP26")[1,5],
              subset(df, Farm == "F8" & SSP == "SSP2" & RCP == "RCP26")[1,5], subset(df, Farm_Group == "AraF_Far" & SSP == "SSP2" & RCP == "RCP26")[1,6],
              subset(df, Farm_Group == "LivS_Far" & SSP == "SSP2" & RCP == "RCP26")[1,6], subset(df, SSP == "SSP2" & RCP == "RCP26")[1,7])
    
    col3 <- c(subset(df, Farm == "F1" & SSP == "SSP3" & RCP == "RCP26")[1,5], subset(df, Farm == "F2" & SSP == "SSP3" & RCP == "RCP26")[1,5],
              subset(df, Farm == "F3" & SSP == "SSP3" & RCP == "RCP26")[1,5], subset(df, Farm == "F4" & SSP == "SSP3" & RCP == "RCP26")[1,5],
              subset(df, Farm == "F5" & SSP == "SSP3" & RCP == "RCP26")[1,5], subset(df, Farm == "F6" & SSP == "SSP3" & RCP == "RCP26")[1,5],
              subset(df, Farm == "F8" & SSP == "SSP3" & RCP == "RCP26")[1,5], subset(df, Farm_Group == "AraF_Far" & SSP == "SSP3" & RCP == "RCP26")[1,6],
              subset(df, Farm_Group == "LivS_Far" & SSP == "SSP3" & RCP == "RCP26")[1,6], subset(df, SSP == "SSP3" & RCP == "RCP26")[1,7])
    
    col4 <- c(subset(df, Farm == "F1" & SSP == "SSP1" & RCP == "RCP45")[1,5], subset(df, Farm == "F2" & SSP == "SSP1" & RCP == "RCP45")[1,5],
              subset(df, Farm == "F3" & SSP == "SSP1" & RCP == "RCP45")[1,5], subset(df, Farm == "F4" & SSP == "SSP1" & RCP == "RCP45")[1,5],
              subset(df, Farm == "F5" & SSP == "SSP1" & RCP == "RCP45")[1,5], subset(df, Farm == "F6" & SSP == "SSP1" & RCP == "RCP45")[1,5],
              subset(df, Farm == "F8" & SSP == "SSP1" & RCP == "RCP45")[1,5], subset(df, Farm_Group == "AraF_Far" & SSP == "SSP1" & RCP == "RCP45")[1,6],
              subset(df, Farm_Group == "LivS_Far" & SSP == "SSP1" & RCP == "RCP45")[1,6], subset(df, SSP == "SSP1" & RCP == "RCP45")[1,7])
    
    col5 <- c(subset(df, Farm == "F1" & SSP == "SSP2" & RCP == "RCP45")[1,5], subset(df, Farm == "F2" & SSP == "SSP2" & RCP == "RCP45")[1,5],
              subset(df, Farm == "F3" & SSP == "SSP2" & RCP == "RCP45")[1,5], subset(df, Farm == "F4" & SSP == "SSP2" & RCP == "RCP45")[1,5],
              subset(df, Farm == "F5" & SSP == "SSP2" & RCP == "RCP45")[1,5], subset(df, Farm == "F6" & SSP == "SSP2" & RCP == "RCP45")[1,5],
              subset(df, Farm == "F8" & SSP == "SSP2" & RCP == "RCP45")[1,5], subset(df, Farm_Group == "AraF_Far" & SSP == "SSP2" & RCP == "RCP45")[1,6],
              subset(df, Farm_Group == "LivS_Far" & SSP == "SSP2" & RCP == "RCP45")[1,6], subset(df, SSP == "SSP2" & RCP == "RCP45")[1,7])
    
    col6 <- c(subset(df, Farm == "F1" & SSP == "SSP3" & RCP == "RCP45")[1,5], subset(df, Farm == "F2" & SSP == "SSP3" & RCP == "RCP45")[1,5],
              subset(df, Farm == "F3" & SSP == "SSP3" & RCP == "RCP45")[1,5], subset(df, Farm == "F4" & SSP == "SSP3" & RCP == "RCP45")[1,5],
              subset(df, Farm == "F5" & SSP == "SSP3" & RCP == "RCP45")[1,5], subset(df, Farm == "F6" & SSP == "SSP3" & RCP == "RCP45")[1,5],
              subset(df, Farm == "F8" & SSP == "SSP3" & RCP == "RCP45")[1,5], subset(df, Farm_Group == "AraF_Far" & SSP == "SSP3" & RCP == "RCP45")[1,6],
              subset(df, Farm_Group == "LivS_Far" & SSP == "SSP3" & RCP == "RCP45")[1,6], subset(df, SSP == "SSP3" & RCP == "RCP45")[1,7])
    
    col7 <- c(subset(df, Farm == "F1" & SSP == "SSP1" & RCP == "RCP85")[1,5], subset(df, Farm == "F2" & SSP == "SSP1" & RCP == "RCP85")[1,5],
              subset(df, Farm == "F3" & SSP == "SSP1" & RCP == "RCP85")[1,5], subset(df, Farm == "F4" & SSP == "SSP1" & RCP == "RCP85")[1,5],
              subset(df, Farm == "F5" & SSP == "SSP1" & RCP == "RCP85")[1,5], subset(df, Farm == "F6" & SSP == "SSP1" & RCP == "RCP85")[1,5],
              subset(df, Farm == "F8" & SSP == "SSP1" & RCP == "RCP85")[1,5], subset(df, Farm_Group == "AraF_Far" & SSP == "SSP1" & RCP == "RCP85")[1,6],
              subset(df, Farm_Group == "LivS_Far" & SSP == "SSP1" & RCP == "RCP85")[1,6], subset(df, SSP == "SSP1" & RCP == "RCP85")[1,7])
    
    col8 <- c(subset(df, Farm == "F1" & SSP == "SSP2" & RCP == "RCP85")[1,5], subset(df, Farm == "F2" & SSP == "SSP2" & RCP == "RCP85")[1,5],
              subset(df, Farm == "F3" & SSP == "SSP2" & RCP == "RCP85")[1,5], subset(df, Farm == "F4" & SSP == "SSP2" & RCP == "RCP85")[1,5],
              subset(df, Farm == "F5" & SSP == "SSP2" & RCP == "RCP85")[1,5], subset(df, Farm == "F6" & SSP == "SSP2" & RCP == "RCP85")[1,5],
              subset(df, Farm == "F8" & SSP == "SSP2" & RCP == "RCP85")[1,5], subset(df, Farm_Group == "AraF_Far" & SSP == "SSP2" & RCP == "RCP85")[1,6],
              subset(df, Farm_Group == "LivS_Far" & SSP == "SSP2" & RCP == "RCP85")[1,6], subset(df, SSP == "SSP2" & RCP == "RCP85")[1,7])
    
    col9 <- c(subset(df, Farm == "F1" & SSP == "SSP3" & RCP == "RCP85")[1,5], subset(df, Farm == "F2" & SSP == "SSP3" & RCP == "RCP85")[1,5],
              subset(df, Farm == "F3" & SSP == "SSP3" & RCP == "RCP85")[1,5], subset(df, Farm == "F4" & SSP == "SSP3" & RCP == "RCP85")[1,5],
              subset(df, Farm == "F5" & SSP == "SSP3" & RCP == "RCP85")[1,5], subset(df, Farm == "F6" & SSP == "SSP3" & RCP == "RCP85")[1,5],
              subset(df, Farm == "F8" & SSP == "SSP3" & RCP == "RCP85")[1,5], subset(df, Farm_Group == "AraF_Far" & SSP == "SSP3" & RCP == "RCP85")[1,6],
              subset(df, Farm_Group == "LivS_Far" & SSP == "SSP3" & RCP == "RCP85")[1,6], subset(df, SSP == "SSP3" & RCP == "RCP85")[1,7])
    
    data <- data.frame(SSP1RCP26=col1, SSP2RCP26=col2, SSP3RCP26=col3, SSP1RCP45=col4, SSP2RCP45=col5, SSP3RCP45=col6, SSP1RCP85=col7, SSP2RCP85=col8, SSP3RCP85=col9)
    rownames(data) <- c("F1", "F2", "F3", "F4", "F5", "F6", "F8", "AraF_Far", "LivS_Far", "Territory")
      #Save file
    write.csv2(data,paste0(PA, metrics, ".table.csv"), append = FALSE, na = "", row.names = TRUE, col.names = TRUE)
    data
}


criteriaBLine <- criteriadynamics(dataBLine, "Bline")
table(criteriaBLine, "Revenue", "level")
table(criteriaBLine, "EnergyYield", "level")
table(criteriaBLine, "ProteinYield", "level")
table(criteriaBLine, "VariableCost", "level")
table(criteriaBLine, "Nefficiency", "level")

table(criteriaBLine, "Revenue", "TRENDf")
table(criteriaBLine, "EnergyYield", "TRENDf")
table(criteriaBLine, "ProteinYield", "TRENDf")
table(criteriaBLine, "VariableCost", "TRENDf")
table(criteriaBLine, "Nefficiency", "TRENDf")

table(criteriaBLine, "Revenue", "RSD")
table(criteriaBLine, "EnergyYield", "RSD")
table(criteriaBLine, "ProteinYield", "RSD")
table(criteriaBLine, "VariableCost", "RSD")
table(criteriaBLine, "Nefficiency", "RSD")

table(criteriaBLine, "Revenue", "SSR")
table(criteriaBLine, "EnergyYield", "SSR")
table(criteriaBLine, "ProteinYield", "SSR")
table(criteriaBLine, "VariableCost", "SSR")
table(criteriaBLine, "Nefficiency", "SSR")

table(criteriaBLine, "Revenue", "NOD")
table(criteriaBLine, "EnergyYield", "NOD")
table(criteriaBLine, "ProteinYield", "NOD")
table(criteriaBLine, "VariableCost", "NOD")
table(criteriaBLine, "Nefficiency", "NOD")

table(criteriaBLine, "Revenue", "probahigh")
table(criteriaBLine, "EnergyYield", "probahigh")
table(criteriaBLine, "ProteinYield", "probahigh")
table(criteriaBLine, "VariableCost", "probahigh")
table(criteriaBLine, "Nefficiency", "probahigh")
