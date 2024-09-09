
# Libraries Used

library(tidyverse)
library(coin)
library(scales)
library(readxl)
library(tibble)
library(knitr)
library(kableExtra)


# Set the working directory
#setwd("C:/Users/Serigne Mbacké/Desktop/LVI")

# Load the survey data
Mydata <- read_csv2(file = "data/Enquête_sur_les_ménages_au_BFA_2022-05-10.csv")

# Load the indexes of the 600 households selected in Excel after cleaning missing
# values, outliers, and inconsistencies
Index_households <- read_excel("data/Index_households.xlsx")

# Filter the dataset to include only selected households
Mydata <- subset(Mydata, Index %in% c(Index_households$Mydata_pure.Index))

# Display the distribution of provinces (agro-climatic zones)
table(Mydata$`Province (Zone agro-climatique)`)

attach(Mydata)

# Order the levels of discrete variables
Mydata$`Province (Zone agro-climatique)` <- factor(Mydata$`Province (Zone agro-climatique)`,
                                                   levels = c("BAM (Sahélienne)",
                                                              "SISSILI (Soudano-Sahélienne)",
                                                              "PONI (Soudanienne)"))

Mydata$`Village:` <- factor(Mydata$`Village:`,
                            levels = c("Mogodin", "Kora - Mossi", "Sakou - Mossi",
                                       "Boutiourou", "Ouliansan", "Kayero Tio",
                                       "Balantira", "Djikando", "Doumbou"))

# Compute dependency ratio
Mydata <- Mydata %>%
  mutate(Dependency_ratio = ((Mydata$`Combien de personnes vivent dans ce ménage ?` -
                                (Mydata$`Combien de personnes de plus de soixante ans (+ 65 ans) y a-t-il dans le ménage ?` +
                                   Mydata$`Combien de personnes de moins de quinze ans (- 15 ans) y a-t-il dans le ménage ?`)) /
                               Mydata$`Combien de personnes vivent dans ce ménage ?`))

# Create the household head's gender variable from two columns
Mydata <- Mydata %>%
  mutate(sexe = coalesce(`Sexe du chef de ménage`, `Sexe du repondant`)) # Combine two variables by replacing NA values

# Create the household head's age variable from two columns
Mydata <- Mydata %>%
  mutate(age = coalesce(`Age du chef de ménage`, `Quel âge avez vous ?`))

Mydata$sexe <- gsub("Féminin", "Feminin", Mydata$sexe) # Change "Féminin" to "Feminin" in the entire column

# Average household size by province
Taille_moyen_menage_ZC <- aggregate(Mydata$`Combien de personnes vivent dans ce ménage ?` ~ Mydata$`Province (Zone agro-climatique)`, Mydata, mean)
ET_Taille_moyen_menage_ZC <- aggregate(Mydata$`Combien de personnes vivent dans ce ménage ?` ~ Mydata$`Province (Zone agro-climatique)`, Mydata, sd)

colnames(Taille_moyen_menage_ZC) <- c("Provinces (Agro-climatic Zones)", "Average Household Size")
colnames(ET_Taille_moyen_menage_ZC) <- c("Provinces (Agro-climatic Zones)", "Standard Deviation of Household Size")

# Average field size by agro-climatic zone/village
Taille_moyen_champ_ZC <- aggregate(Mydata$`Nombre d'hectare` ~ Mydata$`Province (Zone agro-climatique)`, Mydata, mean)
ET_Taille_moyen_champ_ZC <- aggregate(Mydata$`Nombre d'hectare` ~ Mydata$`Province (Zone agro-climatique)`, Mydata, sd)

colnames(Taille_moyen_champ_ZC) <- c("Provinces (Agro-climatic Zones)", "Average Field Size")

# Create the Type_cult variable: the number of crops cultivated per household
Mydata <- Mydata %>%
  mutate(Type_cult = select(., `Parmi ces cultures lesquelles avez vous cultivé l'année dernière ?/Arachide` : `Parmi ces cultures lesquelles avez vous cultivé l'année dernière ?/Soja`) %>% 
           rowSums(na.rm = TRUE))
nbre_culture_par_province <- table(Mydata$`Province (Zone agro-climatique)`, Mydata$Type_cult, deparse.level = 1)
nbre_culture_par_province <- data.frame(nbre_culture_par_province)
cultpro <- nbre_culture_par_province %>% 
  pivot_wider(names_from = Var2, values_from = Freq) #Mettre sous forme de tableau au lieu de ligne

# Create the Type_bet variable: The number of types of livestock per household
Mydata <- Mydata %>%
  mutate(Type_bet = select(., `Quels animaux composent votre cheptel de bétail ?/Mouton` : `Quels animaux composent votre cheptel de bétail ?/Dromadaire`) %>% 
           rowSums(na.rm = TRUE))

# Count the number of livestock types by province
nbre_bete_par_province <- table(Mydata$`Province (Zone agro-climatique)`, Mydata$Type_bet)
nbre_bete_par_province <- data.frame(nbre_bete_par_province)

# Reshape the data for easier analysis
bettvil <- nbre_bete_par_province %>% 
  pivot_wider(names_from = Var2, values_from = Freq)

# Assign values for the borrow_lend index
Mydata$Ratio_bor_len[Mydata$`Avez vous emprunté de l'argent au cours du mois dernier ?` == "OUI" & 
                       Mydata$`Avez vous preté de l'argent au cours du mois dernier ?` == "NON"] <- 2
Mydata$Ratio_bor_len[Mydata$`Avez vous emprunté de l'argent au cours du mois dernier ?` == "NON" & 
                       Mydata$`Avez vous preté de l'argent au cours du mois dernier ?` == "OUI"] <- 0.5
Mydata$Ratio_bor_len[Mydata$`Avez vous emprunté de l'argent au cours du mois dernier ?` == "OUI" & 
                       Mydata$`Avez vous preté de l'argent au cours du mois dernier ?` == "OUI"] <- 1
Mydata$Ratio_bor_len[Mydata$`Avez vous emprunté de l'argent au cours du mois dernier ?` == "NON" & 
                       Mydata$`Avez vous preté de l'argent au cours du mois dernier ?` == "NON"] <- 1

# Filter the dataset to include only selected households
Mydata <- subset(Mydata, Index %in% c(Index_households$Mydata_pure.Index))


#########################################
### LVI Components (SDP, LS, SN, etc) ###
#########################################

## SOCIO-DEMOGRAPHIC PROFILE (SDP) ##
Mydata <- Mydata %>%
  mutate(Dependency_ratio = ((Mydata$`Combien de personnes vivent dans ce ménage ?` - 
                                (Mydata$`Combien de personnes de plus de soixante ans (+ 65 ans) y a-t-il dans le ménage ?` + 
                                   Mydata$`Combien de personnes de moins de quinze ans (- 15 ans) y a-t-il dans le ménage ?`)) / 
                               Mydata$`Combien de personnes vivent dans ce ménage ?`))

# Calculate average dependency ratio by province
DR <- aggregate(Mydata$Dependency_ratio ~ Mydata$`Province (Zone agro-climatique)`, Mydata, mean)
DR <- DR$`Mydata$Dependency_ratio`
Dependancy_Ratio <- data.frame(DR)

# Calculate the percentage and standard deviation of gender distribution by province
Sexe_CM <- Mydata %>%
  count(`Province (Zone agro-climatique)`, sexe) %>% 
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pct_Sexe = prop.table(n), ET_Sexe = sd(n))
Sexe_Head_Household <- Sexe_CM[c(1, 3, 5), 4]

# Calculate average household size by province
Taille_moyen_menage_ZC <- Mydata %>%
  group_by(`Province (Zone agro-climatique)`) %>%
  summarise(Mean = mean(`Combien de personnes vivent dans ce ménage ?`))

# Rescale household size to a range between 0 and 1
max_menage <- max(Mydata$`Combien de personnes vivent dans ce ménage ?`)
min_menage <- min(Mydata$`Combien de personnes vivent dans ce ménage ?`) + 1
Household_size <- scales::rescale(Taille_moyen_menage_ZC$Mean, to = c(0, 1), from = c(min_menage, max_menage))

# Calculate percentage distribution of education levels by province
Education_HH <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Quel est votre niveau d'étude ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pctEducation = prop.table(n))

Education_resultats <- Education_HH[c(1, 2, 6, 8, 11), 4]
E_Kongoussi <- sum(Education_resultats$pctEducation[1:2])
E_Leo <- sum(Education_resultats$pctEducation[3:4])
E_Gaoua <- sum(Education_resultats$pctEducation[5])
Education <- data.frame(rbind(E_Kongoussi, E_Leo, E_Gaoua))

# Calculate the Socio-Demographic Profile (SDP) as an average of all metrics
SDP <- (Education + Sexe_Head_Household + Household_size + Dependancy_Ratio) / 4

## Livelihood Strategy (LS) ##
# Calculate the percentage of households using improved seeds by province
Improved_Seed_HH <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Avez vous utilisé des semences améliorées ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pct_Improved_Seed = prop.table(n))

# Aggregate the percentage of improved seed usage by combining relevant categories
Im <- Improved_Seed_HH[-c(2, 5, 8), 4]
Improved_Seed <- data.frame(c(sum(Im$pct_Improved_Seed[1:2]), 
                              sum(Im$pct_Improved_Seed[3:4]),
                              sum(Im$pct_Improved_Seed[5:6])))

# Calculate the percentage of households using fertilizer by province
Fertilizer_HH <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Aviez vous utilisé de l'engrais pendant la campagne agricole ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pct_Fertilizer = prop.table(n))
Fertilizer <- Fertilizer_HH[c(1, 3, 5), 4]

# Calculate the percentage of households that received climate change training by province
Training_CC_HH <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Avez vous une fois reçu une formation / information / préparation pour faire face au changement climatique ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pct_Training_CC = prop.table(n))

Training_CC <- Training_CC_HH[c(1, 3, 6), 4]

# Calculate the percentage of households owning livestock by province
Livestock_HH <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Le ménage possède-t-il du bétail ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pct_Livestock = prop.table(n))

# Create an index for livestock types
Index_livestock <- data.frame("Index", 1, 1/2, 1/3, 1/4, 1/5, 1/6)
colnames(Index_livestock) <- colnames(bettvil)

# Calculate the number of livestock types per household
Mydata <- Mydata %>%
  mutate(Type_bet = select(., `Quels animaux composent votre cheptel de bétail ?/Mouton` : `Quels animaux composent votre cheptel de bétail ?/Dromadaire`) %>% 
           rowSums(na.rm = TRUE))

# Count the number of livestock types by province
nbre_bete_par_province <- table(Mydata$`Province (Zone agro-climatique)`, Mydata$Type_bet)
nbre_bete_par_province <- data.frame(nbre_bete_par_province)
bettvil <- nbre_bete_par_province %>% 
  pivot_wider(names_from = Var2, values_from = Freq)

# Combine livestock index with the livestock data
Livestock_index <- rbind(bettvil, Index_livestock)

# Calculate livestock stock indices for each province
Kongoussi_stock <- sum(Livestock_index[1, 2:7] * Livestock_index[4, 2:7]) / sum(Livestock_index[1, 2:7])
Leo_stock <- sum(Livestock_index[2, 2:7] * Livestock_index[4, 2:7]) / sum(Livestock_index[2, 2:7])
Gaoua_stock <- sum(Livestock_index[3, 2:7] * Livestock_index[4, 2:7]) / sum(Livestock_index[3, 2:7])
Livestock <- data.frame(rbind(Kongoussi_stock, Leo_stock, Gaoua_stock))

# Calculate the Livelihood Strategy (LS) as an average of all metrics
LS <- (Training_CC + Improved_Seed + Fertilizer + Livestock) / 4

## SOCIAL NETWORK (SN) ##

# Calculate the mean ratio of borrowing to lending by province
Ratio_bor_len <- aggregate(Ratio_bor_len ~ `Province (Zone agro-climatique)`, Mydata, mean)

# Normalize the borrowing-lending ratio to a scale from 0 to 1
Max_ratio <- 2
Min_ratio <- 0
Ratio_bor_len[1, 2] <- (Ratio_bor_len[1, 2] - Min_ratio) / (Max_ratio - Min_ratio)
Ratio_bor_len[2, 2] <- (Ratio_bor_len[2, 2] - Min_ratio) / (Max_ratio - Min_ratio)
Ratio_bor_len[3, 2] <- (Ratio_bor_len[3, 2] - Min_ratio) / (Max_ratio - Min_ratio)
Ratio_borrow_lend <- data.frame(Ratio_bor_len$Ratio_bor_len)

# Calculate the percentage of households with a radio by province
Radio_HH <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Avez vous une radio ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pct_Radio = prop.table(n))
Radio <- Radio_HH[c(1, 3, 5), 4]

# Calculate the percentage of households that belong to a community, group, or cooperative by province
Community_HH <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Faites vous partir d'une communauté, d'un groupement ou d'une coopérative ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pct_Community = prop.table(n))
Community_member <- Community_HH[c(1, 3, 5), 4]

# Calculate the percentage of households that participated in village activities last year by province
Activity_HH <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Avez vous participé aux activités du village l'année dernière ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pct_Activity = prop.table(n))
Activity <- Activity_HH[c(1, 3, 5), 4]

# Calculate the Social Network (SN) index as the average of all metrics
SN <- (Activity + Community_member + Radio + Ratio_borrow_lend) / 4

## FOOD (F) ##

# Create an index for crop types
Index_crop <- data.frame("Index", 1/2, 1/3, 1/4, 1/5, 1/6, 1/7, 1/8)
colnames(Index_crop) <- colnames(cultpro)

# Combine crop data with the crop index
Crop_index <- rbind(cultpro, Index_crop)

# Calculate crop index for each province
Kongoussi_crop <- sum(Crop_index[1, 2:8] * Crop_index[4, 2:8]) / sum(Crop_index[1, 2:8])
Leo_crop <- sum(Crop_index[2, 2:8] * Crop_index[4, 2:8]) / sum(Crop_index[2, 2:8])
Gaoua_crop <- sum(Crop_index[3, 2:8] * Crop_index[4, 2:8]) / sum(Crop_index[3, 2:8])
crop <- data.frame(c(Kongoussi_crop, Leo_crop, Gaoua_crop))

# Calculate the percentage of households with less than three types of crops
cultpro1 <- cultpro[, -c(1)]
Third_crop_Kongoussi <- sum(cultpro1[1, 1:2]) / sum(cultpro1[1, ])
Third_crop_Leo <- sum(cultpro1[2, 1:2]) / sum(cultpro1[2, ])
Third_crop_Gaoua <- sum(cultpro1[3, 1:2]) / sum(cultpro1[3, ])
Third_crop <- data.frame(c(Third_crop_Kongoussi, Third_crop_Leo, Third_crop_Gaoua))

# Calculate the percentage of households able to sustainably save their harvest by province
Maintain_harvest_HH <- Mydata %>% 
  count(`Province (Zone agro-climatique)`, `Parvenez vous a économiser votre récolte durablement ?`) %>% 
  group_by(`Province (Zone agro-climatique)`) %>% 
  mutate(pct_Economy = prop.table(n))
Maintain_harvest <- Maintain_harvest_HH[c(1, 3, 5), 4]

# Calculate the overall food-related index (F) as the average of crop index, harvest maintenance, and third crop metrics
F_ <- data.frame(crop + Maintain_harvest + Third_crop) / 3

## WATER (W) ##

# Calculate the percentage of households by water source for each province
Source_of_water_HH <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Quelle est la source d'eau du ménage ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pct_Source_water = prop.table(n))

# Extract and combine water source percentages
Source_water_HH <- Source_of_water_HH[c(seq(2, 8, 2)), 4]
Source_water <- data.frame(c(
  Source_water_HH$pct_Source_water[1], 
  Source_water_HH$pct_Source_water[2],
  Source_water_HH$pct_Source_water[3] + Source_water_HH$pct_Source_water[4]
))

# Calculate the percentage of households with regular access to water
Water_HH <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Avez vous un accès régulier à de l'eau ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pct_water = prop.table(n))
Water_supply <- Water_HH[c(1, 3, 5), 4]

# Calculate the Water index (W) as the average of water supply and source metrics
W <- (Water_supply + Source_water) / 2

## HEALTH (H) ##

# Calculate the percentage of households with a family latrine by province
Toilets_HH <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Disposez vous d'une latrine familiale pour vos besoins ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pct_Toilet = prop.table(n))
Toilets <-Toilets_HH[c(1, 3, 5), 4]

# Calculate the percentage of households with a member suffering from a disability by province
Handicap_HH <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Un membre du ménage souffre-t-il d'un handicap ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pct_Handicap = prop.table(n))
Handicap <- Handicap_HH[c(2, 4, 6), 4]

# Calculate the percentage of households with a member suffering from a chronic illness by province
Chronical_ill_HH <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Un membre du ménage souffre-t-il d'une maladie chronique ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pct_illness = prop.table(n))
Chronical_ill <- Chronical_ill_HH[c(2, 4, 6), 4]
# Calculate the Health index (H) as the average of chronic illness, disability, and toilet access percentages
H <- ( Chronical_ill + Handicap + Toilets) / 3

## LAND (L) ##
# Define constants for land size proportions
ave_farm_size <- Taille_moyen_champ_ZC$`Average Field Size` # Mean
sd_farm_size <- ET_Taille_moyen_champ_ZC$`Mydata$\`Nombre d'hectare\`` # Standard deviation
max_farm <- max(Mydata$`Nombre d'hectare`)
min_farm <- min(Mydata$`Nombre d'hectare`)
Farm_size <- scales::rescale(ave_farm_size, to = c(0, 1), from = c(min_farm, max_farm))

# Less farm size more vulnerabilty
Farm_size <- 1 - Farm_size


# Calculate the proportion of households with less than 2 hectares of land, by province
Small_Ha <- Mydata$`Province (Zone agro-climatique)`[Mydata$`Nombre d'hectare` < 2]
A <- table(Small_Ha)
B <- table(Mydata$`Province (Zone agro-climatique)`)
Less_2_hectares <- A / B

# Calculate the percentage of households that own the land they farm, by province
Land_owner_HH <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Etes vous vous même propriétaire de ces terres ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) %>%
  mutate(pct_Land = prop.table(n))
Land_owner <- Land_owner_HH[c(1, 3, 5), 4]

# Calculate the Land index (L) as the average of land ownership, small land proportion, and inverse land size proportion
L <- (Farm_size + Less_2_hectares + Land_owner) / 3


## INSTITUTIONAL (I) #
#(ACLED DATA on the MAP)

# Load the number of incidents of terrorism and protest by province

Insecurity <- read_csv2("insecurity_events.csv") # terrorism refers to act against civilian in ACLED database
                                                 # protest refers to protest/riots in the database
                                                  
# Define maximum and minimum values for normalization
max_terrorism <- Insecurity$Max_Burkina[1] # The highest number of terrorism event in a province
max_protest <- Insecurity$Max_Burkina[2]   # The highest number of protest event in a province

min_terrorism <- Insecurity$Min_Burkina[1]
min_protest <- Insecurity$Min_Burkina[2]

# Normalize the data for terrorism incidents
Insecurity[1, 2] <- (Insecurity[1, 2] - min_terrorism) / (max_terrorism - min_terrorism)
Insecurity[1, 3] <- (Insecurity[1, 3] - min_terrorism) / (max_terrorism - min_terrorism)
Insecurity[1, 4] <- (Insecurity[1, 4] - min_terrorism) / (max_terrorism - min_terrorism)

# Normalize the data for protest incidents
Insecurity[2, 2] <- (Insecurity[2, 2] - 1) / (max_protest - 1)
Insecurity[2, 3] <- (Insecurity[2, 3] - 1) / (max_protest - 1)
Insecurity[2, 4] <- (Insecurity[2, 4] - 1) / (max_protest - 1)

# Weight the normalized values: 80% for terrorism and 20% for protest
Insecurity[1, 2:4] <- Insecurity[1, 2:4] * 0.8 # The impact of both is different in the livelihood
Insecurity[2, 2:4] <- Insecurity[2, 2:4] * 0.2

# Calculate and display the summed values for each province
I_Kongoussi <- sum(Insecurity[, 2])
I_Leo <- sum(Insecurity[, 3])
I_Gaoua <- sum(Insecurity[, 4])

# Create a data frame with the final values for each province
I <- data.frame(rbind(I_Kongoussi, I_Leo, I_Gaoua))

# NATURAL DISASTER AND CLIMATE VARIABILITY (ND) ##

# Calculate the distribution 
floods <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Quel est en moyenne le nombre d’inondations survenu pendant les 3 dernières années ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) 

drought <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Quel est en moyenne le nombre secheresse / poches de sécheresse survenu pendant les 3 dernières années ?`) %>%       
  group_by(`Province (Zone agro-climatique)`)


Wet_season <- Mydata %>%
  count(`Province (Zone agro-climatique)`, `Comment qualifierez vous la saison des pluies précédente ?`) %>%       
  group_by(`Province (Zone agro-climatique)`) 

type_season <- Wet_season %>% 
  mutate(score = case_match(`Comment qualifierez vous la saison des pluies précédente ?`,
                            "Très bonne" ~ 1,
                            "Bonne" ~ 2,
                            "Normale" ~ 3,
                            "Mauvaise" ~ 4,
                            "Très mauvaise" ~ 5))

# Rename with short names
type_season <- type_season %>%
  rename(province = `Province (Zone agro-climatique)`,
         type = `Comment qualifierez vous la saison des pluies précédente ?`)

floods <- floods %>%
  rename(province = `Province (Zone agro-climatique)`,
         inondations = `Quel est en moyenne le nombre d’inondations survenu pendant les 3 dernières années ?`)

drought <- drought %>%
  rename(province = `Province (Zone agro-climatique)`,
         secheresse = `Quel est en moyenne le nombre secheresse / poches de sécheresse survenu pendant les 3 dernières années ?`)

# Calcul weighted mean
resultats_fl <- floods %>% 
  group_by(province) %>% 
  summarise(moyenne_ponderee = weighted.mean(inondations, n))

resultats_dr <- drought %>% 
  group_by(province) %>% 
  summarise(moyenne_ponderee = weighted.mean(secheresse, n))

resultats_ts <- type_season %>% 
  group_by(province) %>% 
  summarise(moyenne_ponderee = weighted.mean(score, n))

# ND1 number of flood events
ND1 <- data.frame(resultats_fl$moyenne_ponderee)
max_floods <- max(Mydata$`Quel est en moyenne le nombre d’inondations survenu pendant les 3 dernières années ?`)
min_floods <- min(Mydata$`Quel est en moyenne le nombre d’inondations survenu pendant les 3 dernières années ?`)
ND1_rescaled <- scales::rescale(as.numeric(ND1$resultats.moyenne_ponderee), to = c(0, 1), from = c(min_floods, max_floods))

# ND2 number of drought events
ND2 <- data.frame(resultats_dr$moyenne_ponderee)
max_drought <- max(Mydata$`Quel est en moyenne le nombre secheresse / poches de sécheresse survenu pendant les 3 dernières années ?`)
min_drought <- min(Mydata$`Quel est en moyenne le nombre secheresse / poches de sécheresse survenu pendant les 3 dernières années ?`)
ND2_rescaled <- scales::rescale(as.numeric(ND2$resultats_dr.moyenne_ponderee), to = c(0, 1), from = c(min_drought, max_drought))

# ND3 score of type season
ND3 <- data.frame(resultats_ts$moyenne_ponderee)
max_score <- max(type_season$score)
min_score <- min(type_season$score)
ND3_rescaled <- scales::rescale(as.numeric(ND3$resultats_ts.moyenne_ponderee), to = c(0, 1), from = c(min_score, max_score))

# Load climate data
climate_data <- read_csv2("data/climate_data.csv")

ND4 <- data.frame(climate_data[1,2:4])
max_sd_tmax <- max(climate_data$Max[1])
min_sd_tmax <- min(climate_data$Min[1])
ND4_rescaled <- scales::rescale(as.numeric(ND4), to = c(0, 1), from = c(min_sd_tmax, max_sd_tmax))

ND5 <- data.frame(climate_data[2,2:4])
max_sd_tmin <- max(climate_data$Max[2])
min_sd_tmin <- min(climate_data$Min[2])
ND5_rescaled <- scales::rescale(as.numeric(ND5), to = c(0, 1), from = c(min_sd_tmin, max_sd_tmin))

ND6 <- data.frame(climate_data[3,2:4])
max_sd_pr <- max(climate_data$Max[3])
min_sd_pr <- min(climate_data$Min[3])
ND6_rescaled <- scales::rescale(as.numeric(ND6), to = c(0, 1), from = c(min_sd_pr, max_sd_pr))

# Combine the normalized data into a single data frame
ND <- data.frame((ND1_rescaled+ND2_rescaled+ND3_rescaled+ND4_rescaled+ND5_rescaled+ND6_rescaled)/6)

# Combine all LVI components into a single data frame
overall_table <- cbind(SDP, LS, SN, F_, W, H, L, I, ND)

# Set meaningful column names
colnames(overall_table) <- c("SOCIO-DEMOGRAPHIC PROFILE (SDP)", 
                             "LIVELIHOOD STRATEGY (LS)", 
                             "SOCIAL NETWORK (SN)", 
                             "FOOD (F)", 
                             "WATER (W)", 
                             "HEALTH (H)", 
                             "LAND (L)", 
                             "INSTITUTIONAL (I)", 
                             "NATURAL DISASTER AND CLIMATE VARIABILITY (ND)")

# Set row names
row.names(overall_table) <- c("Kongoussi", "Léo", "Gaoua")

# Convert row names to a column
overall_table <- rownames_to_column(overall_table, var = "Province")

# Reshape the data for better presentation
my_table_lvi <- overall_table %>% 
  pivot_longer(cols = -Province, names_to = "components", values_to = "values") %>% 
  pivot_wider(names_from = Province, values_from = values)

# Round numerical values for clarity
my_table_lvi[2:4] <- round(my_table_lvi[2:4], 3)

# Display the styled table using kable and kableExtra
my_table_lvi %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"))
# Save the final result
write_csv2(my_table_lvi, "data/LVI_score.csv")

######################
#Calculation LVI IPCC#
######################
# Calculate adaptive capacity using weighted sum of SDP, LS, and SN
adaptative_capacity <- (SDP * 4 + LS * 4 + SN * 4) / 12

# Calculate sensitivity using weighted sum of F_, W, H, and L
sensitivity <- (F_ * 3 + W * 2 + H * 3 + L * 3) / 11

# Calculate exposure using weighted sum of I and ND
exposure <- (I * 2 + ND * 6) / 8

# Combine the calculated indices into a single data frame
lvi_ipcc_table <- cbind(adaptative_capacity, sensitivity, exposure)

# Set column names for the data frame
colnames(lvi_ipcc_table) <- c('Adaptative Capacity', 'Sensitivity', 'Exposure')

# Add row names as a column to the data frame and remove prefix "E_"
lvi_ipcc_table <- lvi_ipcc_table %>% 
  rownames_to_column() %>% 
  mutate(rowname = str_replace(rowname, "E_", ""))

# Save the data frame to a CSV file
write_csv2(lvi_ipcc_table, "data/LVI_IPCC.csv")

# Calculate the LVI_IPCC using the formula
LVI_IPCC <- (exposure - adaptative_capacity) * sensitivity



# Bigger table
tbSDP <- cbind(Education, Sexe_Head_Household, Household_size, Dependancy_Ratio)
colnames(tbSDP) <- c(paste0(rep("SDP_",4), 1:4))
row.names(tbSDP) <- c("Kongoussi", "Léo", "Gaoua")

tbLS <- cbind(Training_CC , Improved_Seed , Fertilizer , Livestock)
colnames(tbLS) <- c(paste0(rep("LS_",4), 1:4))
row.names(tbLS) <- c("Kongoussi", "Léo", "Gaoua")

tbSN <- cbind(Activity, Community_member, Radio, Ratio_borrow_lend)
colnames(tbSN) <- c(paste0(rep("SN_",4), 1:4))
row.names(tbSN) <- c("Kongoussi", "Léo", "Gaoua")

tbF <- cbind(crop, Maintain_harvest, Third_crop)
colnames(tbF) <- c(paste0(rep("F_",3), 1:3))
row.names(tbF) <- c("Kongoussi", "Léo", "Gaoua")

tbW <- cbind(Water_supply, Source_water)
colnames(tbW) <- c(paste0(rep("W_",2), 1:2))
row.names(tbW) <- c("Kongoussi", "Léo", "Gaoua")

tbH <- cbind(Chronical_ill_HH[c(2, 4, 6), 4], Handicap_HH[c(2, 4, 6), 4], Toilets_HH[c(1, 3, 5), 4])
colnames(tbH) <- c(paste0(rep("H_",3), 1:3))
row.names(tbH) <- c("Kongoussi", "Léo", "Gaoua")

tbL <- cbind(L1, as.numeric(L2), L3)
colnames(tbL) <- c(paste0(rep("L_",3), 1:3))
row.names(tbL) <- c("Kongoussi", "Léo", "Gaoua")

tbI <- cbind(Insecurity[1,], Insecurity[2,])
colnames(tbI) <- c(paste0(rep("I_",2), 1:2))
row.names(tbI) <- c("Kongoussi", "Léo", "Gaoua")

tbND <- cbind(ND1, ND2, ND3, ND4, ND5, ND6)
colnames(tbND) <- c(paste0(rep("ND_",6), 1:6))
row.names(tbND) <- c("Kongoussi", "Léo", "Gaoua")

Global_table <- cbind(tbSDP, SDP, tbLS, LS, tbSN, SN, tbF, F_, tbW, W,
                      tbH, H, tbL, L, tbI, I, tbND,ND)
colnames(Global_table) <- c(
  paste0(rep("SDP_",4), 1:4),
  "SDP",
  paste0(rep("LS_",4), 1:4),
  "LS",
  paste0(rep("SN_",4), 1:4),
  "SN",
  paste0(rep("F_",3), 1:3),
  "F",
  paste0(rep("W_",2), 1:2),
  "W",
  paste0(rep("H_",3), 1:3),
  "H",
  paste0(rep("L_",3), 1:3),
  "L",
  paste0(rep("I_",2), 1:2),
  "I",
  paste0(rep("ND_",6), 1:6),
  "ND"
)

# Reshape the data for better presentation
glo_with_pro <- rownames_to_column(Global_table,var = "Province")

lvi_all <- glo_with_pro %>%
  pivot_longer(cols = -Province, names_to = "components", values_to = "values") %>% 
  pivot_wider(names_from = Province, values_from = values)

# Round numerical values for clarity
lvi_all[2:4] <- round(lvi_all[2:4], 3)

# Display the styled table using kable and kableExtra
lvi_all %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"))
