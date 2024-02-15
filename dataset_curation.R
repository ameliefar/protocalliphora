#'File with scripts to obtain curated data tables
#'From original datasets, we only keep columns and rows fo interest and create data tables stored in folder "data" in the repository
#'
#'Loading packages
library(tidyverse)
library(openxlsx)
#'
#'PRE-ANALYSIS : dataset to check the correlation between nestling physical condition and brood parasite load

path <- "//Maison/SIE/Projets/Donnees_Mesange/1-BDD  LA PLUS RECENTE/1- Données (Démo, Morpho,Pous, Obs)/8-BDD validé"
data_path <- "C:/Users/FARGEVIEILLE/Documents/GitHub/protocalliphora/data"
brood <- openxlsx::read.xlsx(paste0(path, "/SIE DEMO 1976-2023.xlsx"), detectDates = TRUE) #dataset on brood information
chick <- openxlsx::read.xlsx(paste0(path, "/SIE POUS 1976-2023.xlsx"), detectDates = TRUE) #dataset related to chick informationS 


# Selecting targeted variables from brood dataset
sub_brood <- brood %>% 
  dplyr::filter(lieu %in% c("pir", "tua") & espece == "ble") %>% # Select sites corresponding to "PIRIO" population and species Cyanistes caeruleus
  dplyr::filter_at(vars(proto, pulecl), all_vars(!is.na(.))) %>%  # Select rows with information on parasite load and number of hatchlings
  dplyr::filter(is.na(explique) | !(str_detect(explique, "p|P"))) %>% # Exclude cross-fostered broods
  tidyr::pivot_longer(starts_with("pulbag"), names_to = "pulnumber", values_to = "bague") %>% 
  dplyr::filter(!is.na(bague)) %>% 
  dplyr::select(lieu, an, nic, bague, proto, pulecl, expou, experience, explique)
                   

data_cond <- chick %>% 
  dplyr::filter(lieu %in% c("pir", "tua") & espece == "ble" & age_plume > 13) %>% # Select sites, species and chick age (based on feather development)
  dplyr::select(lieu, an, nic, bague, tarsed, poids, age_plume) %>% 
  dplyr::left_join(sub_brood, by = "bague") %>% # merge with brood dataset
  dplyr::arrange(dplyr::desc(age_plume)) %>% # the following rows ensure there is no duplicate
  dplyr::group_by(bague) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(an.x %in% c(2004:2021) & !is.na(proto)) %>% # filter targeted years and remove lack of information on parasite load
  dplyr::filter_at(vars(c(tarsed, poids)), any_vars(!is.na(.))) %>% # discard rows with no values for tarsus or mass
  dplyr::filter_at(vars(c(tarsed, poids)), any_vars(!is.na(.))) %>% # discard rows with no values for tarsus or mass
  dplyr::mutate(ind_id = as.character(bague),
                tarsus = as.numeric(tarsed),
                mass = as.numeric(poids),
                par_load = as.integer(proto),
                nestling = as.numeric(pulecl),
                year = as.character(an.x),
                nestbox = paste(lieu.x, nic.x, sep = ""),
                relative_par_load = round(par_load/nestling, digits = 2)) %>% # parasite load per nestling
  dplyr::select(year, nestbox, ind_id, par_load, nestling, relative_par_load, tarsus, mass) 


write.csv(data_cond, paste0(data_path, "/nestling_condition.csv"), row.names = FALSE)

