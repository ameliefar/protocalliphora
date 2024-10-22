#'File with scripts to obtain curated data tables
#'From original datasets, we only keep columns and rows fo interest and create data tables stored in folder "data" in the repository
#'
#'Loading packages
library(tidyverse)
library(openxlsx)
#'
#'PRE-ANALYSIS : dataset to check the correlation between nestling physical condition and brood parasite load

path <- "//Maison/SIE/Projets/Donnees_Mesange/1-BDD  LA PLUS RECENTE/1- Données (Démo, Morpho,Pous, Obs)/7-BDD en cours de saisie/"
color_path <- "//Maison/SIE/Projets/Donnees_Mesange/1-BDD  LA PLUS RECENTE/3-BDD_Couleurs_Plumes/BDD_Col_A_JOUR/"
data_path <- "C:/Users/FARGEVIEILLE/Documents/GitHub/protocalliphora/data"
brood <- openxlsx::read.xlsx(paste0(path, "/SIE DEMO 1976-2024.xlsx"), detectDates = TRUE) #dataset on brood information
chick <- openxlsx::read.xlsx(paste0(path, "/SIE POUS 1976-2024.xlsx"), detectDates = TRUE) #dataset related to chick information 
adult <- openxlsx::read.xlsx(paste0(path, "/SIE MORPH 1976-2024.xlsx"), detectDates = TRUE) #dataset related to breeder information
col <- readRDS(paste0(color_path, "/bdd_col_v2_ble_22102024.rds")) #datset related to color variables

# Selecting targeted variables from brood dataset
sub_brood <- brood %>% 
  dplyr::filter(lieu %in% c("pir", "tua") & espece == "ble") %>% # Select sites corresponding to "PIRIO" population and species Cyanistes caeruleus
  dplyr::filter_at(vars(proto, pulecl), all_vars(!is.na(.))) %>%  # Select rows with information on parasite load and number of hatchlings
  dplyr::filter(is.na(explique) | !(str_detect(explique, "w|p|P"))) %>% # Exclude cross-fostered broods
  dplyr::filter(extnt != "T") %>% # Exclude nests which were experimentally treated for protocalliphora
  tidyr::pivot_longer(starts_with("pulbag"), names_to = "pulnumber", values_to = "bague", values_drop_na = TRUE) %>% 
  dplyr::select(lieu, an, nic, bague, proto, pulecl, expou, experience, explique)
                   

data_cond <- chick %>% 
  dplyr::filter(lieu %in% c("pir", "tua") & espece == "ble" & age_plume > 13) %>% # Select sites, species and chick age (based on feather development)
  dplyr::select(lieu, an, nic, bague, tarsed, poids, age_plume, exp_p) %>% 
  dplyr::left_join(sub_brood, by = "bague") %>% # merge with brood dataset
  dplyr::arrange(dplyr::desc(age_plume)) %>% # the following rows ensure there is no duplicate
  dplyr::group_by(bague) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(an.x %in% c(2004:2022) & !is.na(proto)) %>% # filter targeted years and remove lack of information on parasite load
  dplyr::filter(if_any(c(tarsed, poids), ~(!is.na(.)))) %>% # discard rows with no values for tarsus or mass
  dplyr::filter(is.na(exp_p)) %>% # exclude broods with experience on chicks (antibiotics/cross-fostering)
  dplyr::mutate(indID = as.character(bague),
                tarsus = as.numeric(tarsed),
                mass = as.numeric(poids),
                par_load = as.integer(proto),
                hatch_size = as.numeric(pulecl),
                year = as.character(an.x),
                broodID = paste(lieu.x, nic.x, sep = ""),
                relative_par_load = round(par_load/hatch_size, digits = 2)) %>% # parasite load per nestling
  dplyr::select(indID, broodID, year, hatch_size, par_load, relative_par_load, tarsus, mass) 


write.csv(data_cond, paste0(data_path, "/nestling_condition.csv"), row.names = FALSE)


#'PREDICTION 1
# Select targeted data from color data set
sub_col <- col %>% 
  dplyr::filter(lieu %in% c("pir", "tua") & espece == "ble" & stringr::str_detect(age, "P1")) %>% # Select sites corresponding to "PIRIO" and blue tit species and first-year breeders who were born in the population
  dplyr::select(bague, an, lieu, nic, sex, per, BB, BUVC, YB, YUVC, YC, age) #253 rows

# Get broodID with an antibiotic experiment in 2022 (not appearing (yet) in brood table)
antibio <- chick %>% 
  filter(exp_p == "antibio" & lieu %in% c("pir", "tua")) %>% 
  distinct(nic)

# Select targeted data from brood data set
sub_brood <- brood %>% 
  dplyr::filter(lieu %in% c("pir", "tua") & espece == "ble" & np == 1 & an %in% c(2004:2022) & !grepl("p|P|w", explique)) %>% # Select sites, species and targeted years
  dplyr::filter(extnt != "T") %>% # Exclude nests which were experimentally treated for protocalliphora
  dplyr::filter(is.na(explique) | !(str_detect(explique, "w|p|P"))) %>%  # Exclude cross-fostered broods
  dplyr::filter(!(nic %in% antibio$nic & an == 2022)) %>% # Remove broods with antibiotic experiment in 2022
  dplyr::select(lieu, nic, an, proto, pulecl, starts_with("pulbag")) %>% # Select columns of interest
  tidyr::pivot_longer(cols = starts_with("pulbag"), names_to = NULL, values_to = "bague", values_drop_na = TRUE) %>% # nestling ID columns are transformed into individual rows
  dplyr::filter(!is.na(proto)) # Remove rows without data on parasite load
  
# Merge data tables
recruit <- dplyr::left_join(sub_col, sub_brood, by = c("bague")) %>% # Merge subsets
  dplyr::filter(!is.na(lieu.y)) %>%  # Remove rows that lack brood information (no count of parasite load)
  dplyr::mutate(indID = as.character(bague),
                year = as.character(an.y),
                sex = dplyr::case_when(sex == 1 ~ "M",
                                       sex == 2 ~ "F",
                                       TRUE ~ NA_character_),
                period = dplyr::case_when(per == 2 ~ "C",
                                              per == 3 ~ "F"),
                broodID = paste(lieu.y, nic.y, sep = ""),
                par_load = as.numeric(proto),
                hatch_size = as.numeric(pulecl),
                relative_par_load = round(par_load/hatch_size, digits = 2)) %>% 
  dplyr::filter(!is.na(sex)) %>% # remove individuals with uncertainty on sex (possible when capture outside of reproduction)
  dplyr::select(indID, sex, broodID, year, hatch_size, par_load, relative_par_load, period, BB, BUVC, YB, YUVC, YC) %>% 
  dplyr::arrange(year, indID, period) %>% 
  dplyr::group_by(indID) %>% 
  dplyr::slice(1) %>% # remove duplicates for birds (feather) sampled twice. The function "arrange" above allows to keep the first sample for the analyses
  dplyr::ungroup()
  
write.csv(recruit, paste0(data_path, "/recruit_color.csv"), row.names = FALSE)


#'PREDICTION 2
#'For this one, I need identity of breeders year 1 and capture (with feather collection) year 2. So I need data from color and brood
#'
# Select targeted data from color data set
sub_col <- col %>% 
  dplyr::filter(lieu %in% c("pir", "tua") & espece == "ble") %>% # Select sites corresponding to "PIRIO" and blue tit species
  dplyr::mutate(an = as.integer(an)) %>% 
  dplyr::select(bague, lieu, an, lieu, ,nic, sex, age, per, BB, BUVC, YB, YUVC, YC) 

# Select targeted data from brood data set
sub_brood <- brood %>% 
  dplyr::filter(lieu %in% c("pir", "tua") & espece == "ble" & np == 1 & an %in% c(2004:2023)& !grepl("p|P|w", explique)) %>% # Select sites, species, targeted years and remove broods with experiments
  dplyr::filter(extnt != "T") %>% # Exclude nests which were experimentally treated for protocalliphora
  dplyr::filter(!(nic %in% antibio$nic & an == 2022)) %>% # Remove broods with antibiotic experiment in 2022
  dplyr::select(lieu, nic, an, proto, pulecl, mbag, fbag) %>% # Select columns of interest
  dplyr::mutate(pairID = paste(fbag, mbag, sep = "_")) %>%  # Create a variable pairID (female/male)
  tidyr::pivot_longer(cols = c(mbag, fbag), names_to = NULL, values_to = "bague", values_drop_na = TRUE) %>% # breeder ID columns are transformed into individual rows
  dplyr::filter(!is.na(proto)) %>%  # Remove rows without data on parasite load 
  dplyr::mutate(an = as.integer(an) + 1) # Coin a variable corresponding to "year 2" to match data on color

adult <- dplyr::left_join(sub_col, sub_brood, by = c("bague", "an")) %>% 
  dplyr::filter(!is.na(lieu.y)) %>% # Remove rows with missing data
  dplyr::mutate(broodID = paste(lieu.y, nic.y, sep = ""),
                indID = as.character(bague),
                year = as.character(an),
                sex = dplyr::case_when(sex == 1 ~ "M",
                                       sex == 2 ~ "F",
                                       TRUE ~ NA_character_),
                period = dplyr::case_when(per == 2 ~ "C",
                                          per == 3 ~ "F"),
                par_load = as.numeric(proto),
                hatch_size = as.numeric(pulecl),
                relative_par_load = round(par_load/hatch_size, digits = 2),
                min_age = dplyr::case_when(str_detect(age, "A") ~ as.integer(stringr::str_remove(age, "A")) + 1, #transform age into minimum age (A is for birds first tagged when at least 2 years old)
                                            TRUE ~ as.integer(stringr::str_remove(age, "[PJI]"))), #P is for birds tagged as nestlings, J for birds tagged when one year old, I for birds tagged as breeders but unidentified age (so at least one year old)
                mateID = case_when(sex == "M" ~ str_remove_all(pairID, "_.*"),
                                   sex == "F" ~ str_remove_all(pairID, ".*_"))) %>%  # Create a variable "mate ID" 
  dplyr::filter(!is.na(sex)) %>% # remove individuals with uncertainty on sex (possible when capture outside of reproduction)
  dplyr::select(indID, sex, min_age, broodID, pairID, mateID, year, hatch_size, par_load, relative_par_load, period, BB, BUVC, YB, YUVC, YC) %>% 
  dplyr::arrange(year, indID, period) %>% 
  dplyr::group_by(indID, year) %>% 
  dplyr::slice(1) %>% # remove duplicates for birds (feather) sampled twice. The function "arrange" above allows to keep the first sample for the analyses
  dplyr::ungroup()
  
  
write.csv(adult, paste0(data_path, "/adult_color.csv"), row.names = FALSE)


