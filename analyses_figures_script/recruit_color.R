#'Analyses to test correlations between parasite load as a nestling and plumage color as a breeding yearling the next year

#'Load packages
library(tidyverse)
library(lme4)
library(ggeffects)
library(broom.mixed)


#'Set working directory
setwd("~/GitHub/protocalliphora/")

#----------------#
# Curate dataset #
#----------------#

#'Load dataset and define variable structure
recruit <- read.csv("data/recruit_color.csv") %>%
  dplyr::mutate(across(c("year", "indID", "nestbag"), ~ as.character(.)),
                across(c("hatch_size", "laydate"), ~ as.integer(.)),
                across(c("BB", "BUVC", "YB", "YUVC", "YC", "relative_par_load"), ~ as.numeric(.)))

#'Standardize continuous variables and organize each color variable as a row (to automatize building models)
recruit_sd <- recruit %>% 
  dplyr::mutate(across(c("relative_par_load", "hatch_size", "laydate", "BB", "BUVC", "YB", "YUVC", "YC"), ~ scale(.)[,1])) 


#--------------#
# Build models #
#--------------#

#' Linear mixed model with brightness from the blue crown (BB) as the response variable
BB_mod <- lme4::lmer(BB ~ relative_par_load*hatch_size + 
                       relative_par_load*sex +
                       period + laydate + 
                       (1|year/broodID), data = recruit_sd)

#' Linear mixed model with UV-chroma from the blue crown (BUVC) as the response variable
BUVC_mod <- lme4::lmer(BUVC ~ relative_par_load*hatch_size + 
                         relative_par_load*sex +
                         period + laydate + 
                         (1|year/broodID), data = recruit_sd)

#' Linear mixed model with brightness from the yellow breast patch (YB) as the response variable
YB_mod <- lme4::lmer(YB ~ relative_par_load*hatch_size + 
                       relative_par_load*sex +
                       period + laydate + 
                       (1|year/broodID), data = recruit_sd)

#' Linear mixed model with UV-chroma from the yellow breast patch (YUVC) as the response variable
YUVC_mod <- lme4::lmer(YUVC ~ relative_par_load*hatch_size + 
                         relative_par_load*sex +
                         period + laydate + 
                         (1|year/broodID), data = recruit_sd)

#' Linear mixed model with yellow chroma from the yellow breast patch (YC) as the response variable
YC_mod <- lme4::lmer(YC ~ relative_par_load*hatch_size + 
                       relative_par_load*sex +
                       period + laydate + 
                       (1|year/broodID), data = recruit_sd)



#----------------------------------------#
# Function to extract outputs from model #
#----------------------------------------#

output_recruit_mod <- function(mod, name) {
  
  #' Extract 95% confidence intervals for estimates along with some distribution around standard deviation for random effect
  
  confint_sd <- as.data.frame(stats::confint(mod)) %>%
    #' Reorganize names for each group & terms
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
                  exp_var = name)
  
}

#'There are some warnings related to (random effect) singularity issues (probably few recruits from the same brood)

#-----------------------------------------------------------------#
# Table with outputs from all models (Table 2 in the manuscript) #
#-----------------------------------------------------------------#
recruit_outputs <- dplyr::bind_rows(output_recruit_mod(mod = BB_mod, name = "BB"),
                                    output_recruit_mod(mod = BUVC_mod, name = "BUVC"),
                                    output_recruit_mod(mod = YB_mod, name = "YB"),
                                    output_recruit_mod(mod = YUVC_mod, name = "YUVC"),
                                    output_recruit_mod(mod = YC_mod, name = "YC")) %>% 
  dplyr::mutate(across(c("estimate", "std.error", "statistic", "low95ci", "up95ci"), ~ round(., digits = 4))) %>% # Round number to only keep four digits
  dplyr::select(model = "exp_var", effect, term, estimate, std_error = "std.error",  # Rename et reorganize variables
                t_value = "statistic", '95CI_low' = "low95ci", '95CI_up' = "up95ci", 
                group, nb_obs, nb_groups)
#'There are some warnings related to boundary issues (related to singularity issues)

write.csv(recruit_outputs, "output_tables/Table2.csv", na = "", row.names = FALSE) # Save model outputs in the dedicated folder



#---------------------------------------------------------------------------------------------#
# Plot for brightness from the yellow breast patch of yearlings  (Figure 3 in the manuscript) #
#---------------------------------------------------------------------------------------------#

#' 
#' Create a conversion table to display unstandardize values for mass in the plot
conversion_YB <- tibble::tibble(sd_values = seq(-3, 5, 0.01), 
                                unsd_values = round(mean(recruit$YB, na.rm = TRUE) + sd_values * sd(recruit$YB, na.rm = TRUE), 1)) %>% 
  dplyr::filter(unsd_values %in% seq(5, 30, 5)) %>% # only select standardized values corresponding to value for brightness from 5 to 30%
  dplyr::distinct(unsd_values, .keep_all = TRUE)

#' Divide hatch size into small (< 6 or -0.35 as standardized value) & large broods (>=6) to visualize interaction
recruit_sd_cat <- recruit_sd %>% 
  dplyr::mutate(brood_cat = dplyr::case_when(hatch_size < -0.35 ~ "small",
                                             TRUE ~ "large"),
                brood_cat = factor(brood_cat, levels = c("small", "large"))) %>% 
  dplyr::filter(relative_par_load < 3) # remove three outliers for parasite load to enhance visualization

#'Rerun model with categories
yb_mod_cat <- lme4::lmer(YB ~ relative_par_load*brood_cat + 
                           relative_par_load*sex +
                           period + laydate + 
                           (1|year/broodID), data = recruit_sd_cat)

#' Get predicted values and organize levels and their name
yb_predict <- ggeffects::ggpredict(yb_mod_cat, terms = c("relative_par_load", "brood_cat"))

yb_predict$group <- factor(yb_predict$group, levels = c("small", "large"))

label <- ggplot2::as_labeller(c("small" = "Small broods", "large" = "Large broods"))

#' Create plot
fig3 <- ggplot2::ggplot(as.data.frame(yb_predict),
               aes(x = x, y = predicted,
                   group = group)) +
  
  #' Create linear prediction
  ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, color = "black") +  
  
  #' Create confidence intervals around linear prediction
  ggplot2::geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray", alpha = 0.4) +
  
  #' Display raw points
  ggplot2::geom_point(data = attr(yb_predict, "rawdata"),
             aes(x = x, y = response),
             alpha = 0.7, 
             shape = 16,
             size = 1.2,  
             color = "black") +
  
  #' Divide plots by the two categories of brood size
  ggplot2::facet_wrap(~ group, nrow = 1, labeller = label) +
  
  #' Add label and arrange esthetics of plot
  ggplot2::labs( x = "Relative parasite load", y = "Brightness of the yellow breast patch") + 
  ggplot2::scale_y_continuous(breaks = conversion_YB$sd_values, labels = conversion_YB$unsd_values) +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::theme(legend.position = "top",  
                 legend.title = element_text(size = 18, face = "bold"),
                 legend.text = element_text(size = 14),
                 plot.title = element_text(size = 14, face = "bold", vjust = 0.8, hjust = 0.5),
                 plot.background = element_rect(fill = "white", color = NA),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_rect(fill = "white", color = "black", linewidth = 1.2),  
                 strip.text = element_text(face = "bold", size = 18, color = "black"),  
                 panel.background = element_rect(fill = "white"),  
                 panel.border = element_blank(),  
                 axis.line = element_line(color = "black", linewidth = 1),
                 axis.ticks = element_line(color = "black"),  # display tick bars
                 axis.title.x = element_text(size = 20, face = "bold"),
                 axis.title.y = element_text(size = 18, face = "bold"),
                 axis.text = element_text(size = 14, face = "bold", color = "black"),
                 panel.spacing = unit(0, "lines"), # Remove margin between facets
                 plot.margin = margin(10, 10, 10, 10))  # Add margins around plot

#' Save as figure 3
ggsave("figures/figure3.png", plot = fig3, width = 12, height = 6, dpi = 300)



#----------------------------------------------------------------------------#
# Outputs for models excluding non-significant interactions (ESM - Table S2) #
#----------------------------------------------------------------------------#
#' Linear mixed model with brightness from the blue crown (BB) as the response variable
alt_BB_mod <- lme4::lmer(BB ~ relative_par_load + hatch_size + 
                       sex + period + laydate + 
                       (1|year/broodID), data = recruit_sd)

#' Linear mixed model with UV-chroma from the blue crown (BUVC) as the response variable
alt_BUVC_mod <- lme4::lmer(BUVC ~ relative_par_load + hatch_size + 
                         sex + period + laydate + 
                         (1|year/broodID), data = recruit_sd)

#' Linear mixed model with brightness from the yellow breast patch (YB) as the response variable
alt_YB_mod <- lme4::lmer(YB ~ relative_par_load*hatch_size + 
                       sex + period + laydate + 
                       (1|year/broodID), data = recruit_sd)

#' Linear mixed model with UV-chroma from the yellow breast patch (YUVC) as the response variable
alt_YUVC_mod <- lme4::lmer(YUVC ~ relative_par_load + hatch_size + 
                         sex + period + laydate + 
                         (1|year/broodID), data = recruit_sd)

#' Linear mixed model with yellow chroma from the yellow breast patch (YC) as the response variable
alt_YC_mod <- lme4::lmer(YC ~ relative_par_load + hatch_size + 
                       sex + period + laydate + 
                       (1|year/broodID), data = recruit_sd)

alt_recruit_outputs <- dplyr::bind_rows(output_recruit_mod(mod = alt_BB_mod, name = "BB"), 
                                        output_recruit_mod(mod = alt_BUVC_mod, name = "BUVC"),
                                        output_recruit_mod(mod = alt_YB_mod, name = "YB"),
                                        output_recruit_mod(mod = alt_YUVC_mod, name = "YUVC"),
                                        output_recruit_mod(mod = alt_YC_mod, name = "YC")) %>% 
  dplyr::mutate(across(c("estimate", "std.error", "statistic", "low95ci", "up95ci"), ~ round(., digits = 4))) %>% # Round number to only keep four digits
  dplyr::select(model = "exp_var", effect, term, estimate, std_error = "std.error",  # Rename et reorganize variables
                t_value = "statistic", '95CI_low' = "low95ci", '95CI_up' = "up95ci", 
                group, nb_obs, nb_groups)

#' There are warnings related to singularities on random effects (mostly for color variables related to yellow breast patch)

write.csv(alt_recruit_outputs, "output_tables/TableS2.csv", na = "", row.names = FALSE) # Save model outputs in the dedicated folder