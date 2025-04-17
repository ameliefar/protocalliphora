#'File with scripts to obtain curated data tables
#'From original datasets, we only keep columns and rows fo interest and create data tables stored in folder "data" in the repository
#'
#'Loading packages
library(tidyverse)
library(openxlsx)
#'
#'
#'
data_path <- "~/GitHub/protocalliphora/data"

#' Unavailable datasets
#' Main & long-term datasets from which we extracted the data for the analyses - they are not open 
#' but we provide scripts we used to curate these datasets and obtained the 3 datasets used in the analyses
#' 
brood # long-term dataset with information related to brood (laying date, clutch size, parent ID, nestling ID, etc.)
chick # long-term dataset with information related to nestlings (ID, measurements, etc.)
adult # long-term dataset with information related to parents (ID, date when captured, measurements, age, sex, etc.)
color # long-term dataset with color variables from feather measurements 
nestbag # dataset with information related to parasite sampling protocol


#'PRE-ANALYSIS : dataset to check the correlation between nestling physical condition and brood parasite load

# Extract values related to nestlings and brood
sub_chick <- chick %>% 
  dplyr::filter(lieu %in% c("pir", "tua") & # select sites
                  espece == "ble" & # select species
                  as.numeric(age_plume) > 13 & # select "old" nestlings (based on feather development)
                  an %in% c(2004:2022) & # select targetted years of parasite sampling
                  (exp_p != "antibio" | is.na(exp_p))) %>% # Remove particular experiment on nestlings that may affect nestling condition
  dplyr::mutate(across(c("tarsed", "poids", "age_plume"), ~ as.numeric(.))) %>%  # transform variables to real
  dplyr::arrange(bague) %>% # order by ring
  dplyr::group_by(bague) %>% # nest by ring
  dplyr::mutate(n = n(), # count the number of observations for each individual
                #' To ensure the largest number of observations with tarsus length, I used a trick
                #' I've checked first that there were only one (or no) value for tarsus length for each individual
                tarse = dplyr::case_when(n == 2 ~ mean(tarsed, na.rm = T),
                                  TRUE ~ tarsed)) %>% 
  dplyr::slice(1) %>% #' Keep one observation per individual
  dplyr::ungroup() %>% #' unnest
  dplyr::select(lieu, an, nic, bague, tarse, poids, age_plume, exp_p)  %>% #' select variables of interest
  dplyr::left_join(brood %>% # merge with dataset on brood, only keeping individuals in sub_chick object
                     tidyr::pivot_longer(starts_with("pulbag"), names_to = "pulnumber", values_to = "bague", values_drop_na = TRUE) %>% #' transform columns with nestling ID into different rows
                     dplyr::select(lieu, an, nic, bague, proto, pulecl, expou, experience, explique, extnt, date_ponte, pulenv, mort), #' select variables of interest
                   by = "bague") #' merge both objects by nestling ID

#' Finalize dataset related to nestling condition, excluding broods with experiments or missing important values (e.g. parasite load)
data_cond <- sub_chick %>% 
  dplyr::filter_at(vars(proto, pulecl), all_vars(!is.na(.))) %>%  # Select rows with information on parasite load and number of hatchlings
  dplyr::filter(is.na(explique) | !(str_detect(explique, "w|p|P"))) %>% # Exclude cross-fostered broods
  dplyr::filter(extnt != "T") %>% # Exclude nests which were experimentally treated for protocalliphora
  dplyr::filter(if_any(c(tarse, poids), ~(!is.na(.)))) %>% # discard rows with no values for tarsus or mass
  dplyr::mutate(across(c("proto", "pulecl", "pulenv"), ~ as.integer(.)), # transform variables as integer
                laydate = lubridate::yday(as.Date(date_ponte, format = "%d/%m/%Y")), # transform laying date as the number of days since January 1st (of the year)
                relative_par_load = round(proto/pulecl, digits = 2), # create variable "relative parasite load"
                broodID = paste0(lieu.x, nic.x)) %>% # coin a broodID from site and nestbox number
  dplyr::select(indID = "bague", broodID, year = "an.x", # select and rename variables when necessary
                hatch_size = "pulecl", par_load = "proto",
                relative_par_load, tarsus = "tarse", mass = "poids",
                laydate, fledg_size = "pulenv") %>% 
  #' Include data related to parasite protocol
  dplyr::left_join(nestbag %>% 
                     dplyr::mutate(broodID = stringr::str_remove_all(stringr::str_remove_all(captureLocationID, "_NB"), "_"), #standardize format with the one used for broodID
                                   year = as.character(Year)) %>% 
                     dplyr::select(year, nestbag = "Nestbag", broodID),
                   by = c("year", "broodID"),
                   relationship = "many-to-many") %>% 
  dplyr::mutate(nestbag = replace(nestbag, is.na(nestbag), 1)) %>% # replace missing values with "1" (regular protocol in all other years)
  dplyr::distinct(indID, .keep_all = TRUE) # remove duplicated rows from merging with the nestbag file


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
  dplyr::select(lieu, nic, an, proto, pulecl, date_ponte, starts_with("pulbag")) %>% # Select columns of interest
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
                relative_par_load = round(par_load/hatch_size, digits = 2),
                laydate = lubridate::yday(as.Date(date_ponte, format = "%d/%m/%Y"))) %>%  # transform laying date as the number of days since January 1st (of the year)
  dplyr::filter(!is.na(sex)) %>% # remove individuals with uncertainty on sex (possible when capture outside of reproduction)
  dplyr::select(indID, sex, broodID, year, hatch_size, par_load, relative_par_load, period, BB, BUVC, YB, YUVC, YC, laydate) %>% 
  dplyr::arrange(year, indID, period) %>% 
  dplyr::group_by(indID) %>% 
  dplyr::slice(1) %>% # remove duplicates for birds (feather) sampled twice. The function "arrange" above allows to keep the first sample for the analyses
  dplyr::ungroup() %>%  
  #' Include data related to parasite protocol
  dplyr::left_join(nestbag %>% 
                     dplyr::mutate(broodID = stringr::str_remove_all(stringr::str_remove_all(captureLocationID, "_NB"), "_"), #standardize format with the one used for broodID
                                   year = as.character(Year)) %>% 
                     dplyr::select(year, nestbag = "Nestbag", broodID),
                   by = c("year", "broodID"),
                   relationship = "many-to-many") %>% 
  dplyr::mutate(nestbag = replace(nestbag, is.na(nestbag), 1)) %>% # replace missing values with "1" (regular protocol in all other years)
  dplyr::distinct(indID, .keep_all = TRUE) # remove duplicated rows from merging with the nestbag file

  
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
  dplyr::select(lieu, nic, an, proto, pulecl, date_ponte, mbag, fbag) %>% # Select columns of interest
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
                                   sex == "F" ~ str_remove_all(pairID, ".*_")), # Create a variable "mate ID" 
                laydate = lubridate::yday(as.Date(date_ponte, format = "%d/%m/%Y"))) %>% # transform laying date as the number of days since January 1st (of the year)
  dplyr::filter(!is.na(sex)) %>% # remove individuals with uncertainty on sex (possible when capture outside of reproduction)
  dplyr::select(indID, sex, min_age, broodID, pairID, mateID, year, hatch_size, par_load, relative_par_load, period, BB, BUVC, YB, YUVC, YC, laydate) %>% 
  dplyr::arrange(year, indID, period) %>% 
  dplyr::group_by(indID, year) %>% 
  dplyr::slice(1) %>% # remove duplicates for birds (feather) sampled twice. The function "arrange" above allows to keep the first sample for the analyses
  dplyr::ungroup() %>% 
  #' Include data related to parasite protocol
  dplyr::left_join(nestbag %>% 
                     dplyr::mutate(broodID = stringr::str_remove_all(stringr::str_remove_all(captureLocationID, "_NB"), "_"), #standardize format with the one used for broodID
                                   year = as.character(Year)) %>% 
                     dplyr::select(year, nestbag = "Nestbag", broodID),
                   by = c("year", "broodID"),
                   relationship = "many-to-many") %>% 
  dplyr::mutate(nestbag = replace(nestbag, is.na(nestbag), 1)) %>% # replace missing values with "1" (regular protocol in all other years)
  dplyr::distinct(indID, .keep_all = TRUE) # remove duplicated rows from merging with the nestbag file
  
  
write.csv(adult, paste0(data_path, "/adult_color.csv"), row.names = FALSE)


