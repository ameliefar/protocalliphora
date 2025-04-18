#'Pre-analyses to test correlations between parasite load and nestling body condition

#'Load packages
library(tidyverse)
library(lme4)
library(ggeffects)
library(car)


#'Set working directory
setwd("~/GitHub/protocalliphora/")

#----------------#
# Curate dataset #
#----------------#

#'Load dataset and define variable structure
data_cond <- read.csv("data/nestling_condition.csv") %>% 
  dplyr::mutate(across(c("year", "indID", "nestbag"), ~ as.character(.)),
                across(c("hatch_size", "par_load", "laydate", "fledg_size"), ~ as.integer(.)),
                across(c("relative_par_load", "tarsus", "mass"), ~ as.numeric(.)))

#'Standardize continuous variables
data_cond_sd <- data_cond %>% 
  dplyr::mutate(across(c("relative_par_load", "tarsus", "mass", "laydate"), ~ scale(.)[,1]))

#--------------#
# Build models #
#--------------#

#'Function to organize the outputs from the two models
output_nestling_mod <- function(mod, name) {
  
  #' Extract 95% confidence intervals for estimates along with some distribution around standard deviation for random effect
  
  confint_sd <- as.data.frame(stats::confint(mod)) %>%
    dplyr::mutate(term = rownames(.),
                  effect = dplyr::case_when(stringr::str_detect(term, "sig") ~ "ran_pars",
                                            TRUE ~ "fixed"),
                  group = dplyr::case_when(term == ".sig01" ~ "broodID:year",
                                           term == ".sig02" ~ "year",
                                           term == ".sigma" ~ "Residual",
                                           TRUE ~ NA_character_),
                  term = dplyr::case_when(term == ".sig01" ~ "sd__(Intercept)",
                                          term == ".sig02" ~ "sd__(Intercept)",
                                          term == ".sigma" ~ "sd__Observation",
                                          TRUE ~ term)) %>% 
    dplyr::rename(low95ci = '2.5 %', up95ci = '97.5 %')
  
  #' Extract estimates and merge with values from estimated confidence intervals
  tab_sd <- broom.mixed::tidy(mod) %>% 
    dplyr::left_join(confint_sd,
                     by = c("effect", "group", "term")) %>% 
    #' Add additional information (groups for each random effect & residuals, name for the model)
    dplyr::mutate(nb_groups = dplyr::case_when(group == "broodID:year" ~ lme4::ngrps(mod)[1],
                                               group == "year" ~ lme4::ngrps(mod)[2],  
                                               TRUE ~ NA_integer_),
                  nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(mod),
                                            TRUE ~ NA_integer_),
                  exp_var = print(name))
  
}

#' Linear mixed model with mass as the response variable
#' 
mass_mod <- lme4::lmer(mass ~ relative_par_load*hatch_size + laydate + (1|year/broodID), data = data_cond_sd)
mass <- "mass" 

#'Get the table associated with model for mass with all organized outputs
mass_tab <- output_nestling_mod(mod = mass_mod, name = mass) 


#' Linear mixed model with tarsus length as the response variable
#' 
tars_mod <- lme4::lmer(tarsus ~ relative_par_load*hatch_size + laydate + (1|year/broodID), data = data_cond_sd)
tars <- "tarsus_length" 

#'Get the table associated with model for tarsus length with all organized outputs
tars_tab <- output_nestling_mod(mod = tars_mod, name = tars)

#-----------------------------------------------------------------#
# Table with outputs from both models (Table 1 in the manuscript) #
#-----------------------------------------------------------------#

#'Merge outputs from both models together
nestling_outputs <- dplyr::bind_rows(mass_tab, tars_tab) %>% 
  dplyr::mutate(across(c("estimate", "std.error", "statistic", "low95ci", "up95ci"), ~ round(., digits = 4))) %>% # Round number to only keep four digits
  dplyr::select(model = "exp_var", effect, term, estimate, std_error = "std.error",  # Rename et reorganize variables
                t_value = "statistic", '95CI_low' = "low95ci", '95CI_up' = "up95ci", 
                group, nb_obs, nb_groups)
write.csv(nestling_outputs, "output_tables/Table1.csv", na = "", row.names = FALSE) # Save model outputs in the dedicated folder




#-----------------------------------------------------------#
# Plot for nestling body mass  (Figure 2 in the manuscript) #
#-----------------------------------------------------------#

#' Create a conversion table to display unstandardize values for mass in the plot
conversion_mass <- tibble::tibble(sd_values = seq(-6, 6, 0.01), 
                                  unsd_values = round(mean(data_cond$mass, na.rm = TRUE) + sd_values * sd(data_cond$mass, na.rm = TRUE), 2)) %>% 
  filter(unsd_values %in% seq(6, 12, 1)) # only select standardized values corresponding to mass from 6g to 12g (by 1 g)

#' Get values to get predicted values and confidence intervals in the plot
model_predict <- as.data.frame(ggeffects::ggpredict(mass_mod_sd, terms = c("relative_par_load")))

#' Create plot
bodycond_lm <- ggplot2::ggplot(model_predict,
                               aes(x = x, y = predicted, group = group)) +
  #' Create confidence intervals
  ggplot2::geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray", alpha = 0.4) +
  #' Display raw data as points
  ggplot2::geom_point(data = attr(model_predict, "rawdata"),
                      aes(x = x, y = response),
                      alpha = 0.7, shape = 16, size = 1.2, color = "black") +
  #' Display the linear model
  ggplot2::geom_smooth(method = "lm", se = TRUE, size = 0.8, color = "black") +  
  #' Clarify labels
  ggplot2::labs(x = "Relative parasite load", y = "Nestling mass (g)") +
  #' Clarify values on the y axis as unstandardized mass rather than standardized values
  ggplot2::scale_y_continuous(breaks = conversion_mass$sd_values, labels = conversion_mass$unsd_values) +
  #' Modify overall features of the plot
  ggplot2::theme(plot.background = element_rect(fill = "white"),  
                 panel.background = element_blank(), 
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(), 
                 axis.line = element_line(color = "black", linewidth = 1), 
                 axis.ticks = element_line(color = "black"), 
                 axis.title = element_text(size = 20, face = "bold"),
                 axis.text = element_text(size = 14, face = "bold", color = "black"),
                 plot.margin = margin(10, 10, 10, 10))

#' Save the plot in appropriate folder
ggplot2::ggsave("figures/figure2.png", plot = bodycond_lm, width = 12, height = 6, dpi = 300)


#--------------------------------------------------------------------------------#
# Display values for variance inflation factors (ESM - Table S5 "Nestling' Model)#
#--------------------------------------------------------------------------------#

car::vif(mass_mod_sd)




#----------------------------------------------------------------------------#
# Outputs for models excluding non-significant interactions (ESM - Table S1) #
#----------------------------------------------------------------------------#
alt_mass_mod <- lme4::lmer(mass ~ relative_par_load + hatch_size + laydate + (1|year/broodID), data = data_cond_sd)

alt_tars_mod <- lme4::lmer(tarsus ~ relative_par_load + hatch_size + laydate + (1|year/broodID), data = data_cond_sd)

alt_nestling_outputs <- dplyr::bind_rows(output_nestling_mod(mod = alt_mass_mod, name = mass), 
                                         output_nestling_mod(mod = alt_tars_mod, name = tars)) %>% 
  dplyr::mutate(across(c("estimate", "std.error", "statistic", "low95ci", "up95ci"), ~ round(., digits = 4))) %>% # Round number to only keep four digits
  dplyr::select(model = "exp_var", effect, term, estimate, std_error = "std.error",  # Rename et reorganize variables
                t_value = "statistic", '95CI_low' = "low95ci", '95CI_up' = "up95ci", 
                group, nb_obs, nb_groups)

write.csv(alt_nestling_outputs, "output_tables/TableS1.csv", na = "", row.names = FALSE) # Save model outputs in the dedicated folder
