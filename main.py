# Title     : Manipulation_fichier
# Objective : Rendre les fichiers d'entrée utilisables par les script R
# Created by: BGourdon
# Created on: 25/11/2020

# if __name__ == '__main__':

import pandas as pd

indicators = pd.read_csv("Indicators.csv", encoding='latin1', delimiter=";")
indicators_melt = indicators.melt(id_vars=["Scenario", "Level", "Indicator"], var_name="Year", value_name="Value")
print(indicators_melt.head())

indicators_melt.to_csv('C:/Users/Utilisateur/PycharmProjects/pythonProject/export_indicators.csv', sep=';', index=False,
                       header=True)

economic_info = pd.read_csv("economic_info.csv", encoding='latin1', delimiter=";")
economic_info_melt = economic_info.melt(id_vars=["Group", "Variable"], var_name="Year", value_name="Value")
economic_info_melt.to_csv('C:/Users/Utilisateur/PycharmProjects/pythonProject/export_economic_info.csv', sep=';',
                          index=False, header=True)
