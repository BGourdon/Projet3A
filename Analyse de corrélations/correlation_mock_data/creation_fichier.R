setwd("C:/Users/Utilisateur/Desktop/Mines3A/Projet 3A/Data/Mock_data")

dataRCP26 <- read.csv2("Prod_farm_final_RCP26.csv", dec=".", sep=";", header=T)
dataRCP45 <- read.csv2("Prod_farm_final_RCP45.csv", dec=".", sep=";", header=T) 
dataRCP85 <- read.csv2("Prod_farm_final_RCP85.csv", dec=".", sep=";", header=T)
indicators <- bind_rows(dataRCP26, dataRCP45, dataRCP85)

# Creation des sous fichiers ####

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

write.table(fichier, file = "Fichier_indicateurs.csv",dec=".", sep=";", row.names = FALSE, col.names = TRUE)
