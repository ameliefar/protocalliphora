#'File with scripts to obtain curated data tables
#'From original datasets, we only keep columns and rows fo interest and create data tables stored in folder "data" in the repository
#'
#'Loading packages
library(tidyverse)
library(openxlsx)
#'
#'PRE-ANALYSIS : dataset to check the correlation between nestling physical condition and brood parasite load

path <- "//Maison/SIE/Projets/Donnees_Mesange/1-BDD  LA PLUS RECENTE/1- Données (Démo, Morpho,Pous, Obs)/8-BDD validé"
color_path <- "//Maison/SIE/Projets/Donnees_Mesange/1-BDD  LA PLUS RECENTE/3-BDD_Couleurs_Plumes/BDD_Col_A_JOUR"
data_path <- "C:/Users/FARGEVIEILLE/Documents/GitHub/protocalliphora/data"
brood <- openxlsx::read.xlsx(paste0(path, "/SIE DEMO 1976-2023.xlsx"), detectDates = TRUE) #dataset on brood information
chick <- openxlsx::read.xlsx(paste0(path, "/SIE POUS 1976-2023.xlsx"), detectDates = TRUE) #dataset related to chick information 
adult <- openxlsx::read.xlsx(paste0(path, "/SIE MORPH 1976-2023.xlsx"), detectDates = TRUE) #dataset related to breeder information
col <- openxlsx::read.xlsx(paste0(color_path, "/DB_Colour_2005-2021_19_03_2023.xlsx"), detectDates = TRUE) #datset related to color variables

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

# DATA to analyse correlation between first year breeder coloration and parasite load as a nestling


# Select targeted data from color dataset
sub_col <- col %>% 
  dplyr::filter(place %in% c("pir", "tua") & sp == "Cyanistes_caeruleus" & stringr::str_detect(age, "P1")) %>% # Select sites corresponding to "PIRIO" and blue tit species and first-year breeders who were born in the population
  dplyr::filter(is.na(exp_as_nestling) | exp_as_nestling == 0) %>% # Remove birds coming from a brood with an experiment (reducing/increasing the number of eggs or nestlings, or cross-fostering experiment)
  dplyr::select(ring, yr, place, nest, sex, per, BB, BUVC, YB, YUVC, YC, age, expe, exp_as_nestling) #183 rows (224 without removing birds with experiment)

sub_brood <- brood %>% 
  dplyr::filter(lieu %in% c("pir", "tua") & espece == "ble" & np == 1 & an %in% c(2004:2021)) %>% # Select sites, species and targeted years
  dplyr::select(lieu, nic, an, proto, pulecl, starts_with("pulbag")) %>% # Select columns of interest
  tidyr::pivot_longer(cols = starts_with("pulbag"), names_to = NULL, values_to = "ring", values_drop_na = TRUE) %>% # nestling ID columns are transformed into individual rows
  dplyr::filter(!is.na(proto)) # Remove rows without data on parasite load

recruit <- dplyr::left_join(sub_col, sub_brood, by = c("ring")) %>% # Merge subsets
  dplyr::filter(!is.na(lieu)) %>%  # Remove rows that lack brood information (no count of parasite load)
  dplyr::mutate(indID = as.character(ring),
                year = as.character(an),
                sex = dplyr::case_when(sex == 1 ~ "M",
                                       sex == 2 ~ "F",
                                       TRUE ~ NA_character_),
                period = dplyr::case_when(per == 2 ~ "C",
                                              per == 3 ~ "F"),
                broodID = paste(lieu, nic, sep = ""),
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
