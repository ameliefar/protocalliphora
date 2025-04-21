#'Analyses to test correlations between parasite load as a parent one year and own plumage color as a breeder the next year

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
adult <- read.csv("data/adult_color.csv") %>%
  dplyr::mutate(across(c("year", "indID", "nestbag"), ~ as.character(.)),
                across(c("hatch_size", "laydate", "min_age"), ~ as.integer(.)),
                across(c("BB", "BUVC", "YB", "YUVC", "YC", "relative_par_load"), ~ as.numeric(.)))

#'Standardize continuous variables and organize each color variable as a row (to automatize building models)
adult_sd <- adult %>% 
  dplyr::mutate(across(c("relative_par_load", "hatch_size", "min_age", "laydate", "BB", "BUVC", "YB", "YUVC", "YC"), ~ scale(.)[,1])) 



#--------------#
# Build models #
#--------------#

#' Linear mixed model with brightness from the blue crown (BB) as the response variable
BB_mod <- lme4::lmer(BB ~ relative_par_load*hatch_size + 
                       relative_par_load*sex +
                       min_age + period + laydate + 
                       (1|year) + (1|pairID) + (1|indID), data = adult_sd)

#' Linear mixed model with UV-chroma from the blue crown (BUVC) as the response variable
BUVC_mod <- lme4::lmer(BUVC ~ relative_par_load*hatch_size + 
                         relative_par_load*sex +
                         min_age + period + laydate + 
                         (1|year) + (1|pairID) + (1|indID), data = adult_sd)

#' Linear mixed model with brightness from the yellow breast patch (YB) as the response variable
YB_mod <- lme4::lmer(YB ~ relative_par_load*hatch_size + 
                       relative_par_load*sex +
                       min_age + period + laydate + 
                       (1|year) + (1|pairID) + (1|indID), data = adult_sd)

#' Linear mixed model with UV-chroma from the yellow breast patch (YUVC) as the response variable
YUVC_mod <- lme4::lmer(YUVC ~ relative_par_load*hatch_size + 
                         relative_par_load*sex +
                         min_age + period + laydate + 
                         (1|year) + (1|pairID) + (1|indID), data = adult_sd)

#' Linear mixed model with yellow chroma from the yellow breast patch (YC) as the response variable
YC_mod <- lme4::lmer(YC ~ relative_par_load*hatch_size + 
                       relative_par_load*sex +
                       min_age + period + laydate + 
                       (1|year) + (1|pairID) + (1|indID), data = adult_sd)


#----------------------------------------#
# Function to extract outputs from model #
#----------------------------------------#

output_adult_mod <- function(mod, name) {
  
  #' Extract 95% confidence intervals for estimates along with some distribution around standard deviation for random effect
  
  confint_sd <- as.data.frame(stats::confint(mod)) %>%
    #' Reorganize names for each group & terms
    dplyr::mutate(term = rownames(.),
                  effect = dplyr::case_when(stringr::str_detect(term, "sig") ~ "ran_pars",
                                            TRUE ~ "fixed"),
                  group = dplyr::case_when(term == ".sig01" ~ "indID",
                                           term == ".sig02" ~ "pairID",
                                           term == ".sig03" ~ "year",
                                           term == ".sigma" ~ "Residual",
                                           TRUE ~ NA_character_),
                  term = dplyr::case_when(term == ".sig01" ~ "sd__(Intercept)",
                                          term == ".sig02" ~ "sd__(Intercept)",
                                          term == ".sig03" ~ "sd__(Intercept)",
                                          term == ".sigma" ~ "sd__Observation",
                                          TRUE ~ term)) %>% 
    dplyr::rename(low95ci = '2.5 %', up95ci = '97.5 %')
  
  #' Extract estimates and merge with values from estimated confidence intervals
  tab_sd <- broom.mixed::tidy(mod) %>% 
    dplyr::left_join(confint_sd,
                     by = c("effect", "group", "term")) %>% 
    #' Add additional information (groups for each random effect & residuals, name for the model)
    dplyr::mutate(nb_groups = dplyr::case_when(group == "indID" ~ lme4::ngrps(mod)[1],
                                               group == "pairID" ~ lme4::ngrps(mod)[2],
                                               group == "year" ~ lme4::ngrps(mod)[3],  
                                               TRUE ~ NA_integer_),
                  nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(mod),
                                            TRUE ~ NA_integer_),
                  exp_var = name)
  
}


#'There are some warnings related to (random effect) singularity issues (probably related to many individuals with only one observation)

#-----------------------------------------------------------------#
# Table with outputs from all models (Table 2 in the manuscript) #
#-----------------------------------------------------------------#
adult_outputs <- dplyr::bind_rows(output_adult_mod(mod = BB_mod, name = "BB"),
                                    output_adult_mod(mod = BUVC_mod, name = "BUVC"),
                                    output_adult_mod(mod = YB_mod, name = "YB"),
                                    output_adult_mod(mod = YUVC_mod, name = "YUVC"),
                                    output_adult_mod(mod = YC_mod, name = "YC")) %>% 
  dplyr::mutate(across(c("estimate", "std.error", "statistic", "low95ci", "up95ci"), ~ round(., digits = 4))) %>% # Round number to only keep four digits
  dplyr::select(model = "exp_var", effect, term, estimate, std_error = "std.error",  # Rename et reorganize variables
                t_value = "statistic", '95CI_low' = "low95ci", '95CI_up' = "up95ci", 
                group, nb_obs, nb_groups)
#'There are some warnings related to boundary issues (related to singularity issues)


write.csv(adult_outputs, "output_tables/Table3.csv", na = "", row.names = FALSE) # Save model outputs in the dedicated folder




#--------------------------------------------------------------------------------#
# Plot for UV-chroma from the blue crown of adults  (Figure 4 in the manuscript) #
#--------------------------------------------------------------------------------#

#' Create a conversion table to display unstandardize values for mass in the plot
conversion_BUVC <- tibble::tibble(sd_values = seq(-1.5, 3, 0.01), 
                                unsd_values = round(mean(adult$BUVC, na.rm = TRUE) + sd_values * sd(adult$BUVC, na.rm = TRUE), 3)) %>% 
  dplyr::filter(unsd_values %in% seq(0.30, 0.45, 0.05)) %>% # only select standardized values corresponding to value for brightness from 5 to 30%
  dplyr::distinct(unsd_values, .keep_all = TRUE)


buvc_predict = ggeffects::ggpredict(BUVC_mod, terms = c("relative_par_load", "sex")) 
fig4 <- ggplot2::ggplot(as.data.frame(buvc_predict), 
                        aes(x = x, y = predicted, 
                            group = group, 
                            color = group, 
                            fill = group)) +
  #' Trace predicted linear relation
  ggplot2::geom_line(data = subset(as.data.frame(buvc_predict), x <= 4), color = "black") + 
  #' Add confidence intervals
  ggplot2::geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = FALSE, alpha = 0.2) +
  #' Add raw points
  ggplot2::geom_point(data = attr(buvc_predict, "rawdata"), 
                      aes(x = jitter(x),
                          y = jitter(response)),
                      alpha = 0.6,
                      shape = 16,
                      size = 1.2) +
  #' Set y axis values
  ggplot2::scale_y_continuous(breaks = conversion_BUVC$sd_values, labels = conversion_BUVC$unsd_values) +
  #' Set displayed colors
  ggplot2::scale_color_manual(values = c("F" = "grey", "M" = "black"),
                     aesthetics = c("colour", "fill"),
                     labels = c("Female", "Male")) +
  ggplot2::labs(color = "Sex", fill = "Sex", title = "" , x = "Relative parasite load", y = "UV-chroma from the blue crown") +
  #' Set esthetics of the global plot
  ggplot2::theme(plot.title = element_text(size = 14, 
                                           face = "bold", 
                                           vjust = 0.8, 
                                           hjust = 0.5),
                 plot.background = element_rect(fill = "white"),
                 panel.background = element_rect(fill = "white"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 axis.line = element_line(colour = "black", 
                                          linewidth = 1, 
                                          linetype = "solid"),
                 axis.title = element_text(size = 16,
                                           face = "bold"),
                 axis.text = element_text(size = 14, 
                                          face = "bold",
                                          color = "black"),
                 legend.position.inside = c(0.80, 0.8),
                 legend.box = "horizontal",
                 legend.background = element_blank(),     
                 legend.key = element_blank(),
                 legend.justification = c(0.3, 0.15),
                 legend.text = element_text(size = 10, face = "bold"),
                 legend.title = element_text(size = 12, face = "bold", hjust = 0.5),
                 strip.background = element_blank())


#' Save as figure 4
ggplot2::ggsave("figures/figure4.png", plot = fig4, width = 12, height = 6, dpi = 300)


#------------------------------------------------------------------------------------------#
# Plot for brightness from the yellow breast patch of adults  (Figure 5 in the manuscript) #
#------------------------------------------------------------------------------------------#
conversion_YB <- tibble::tibble(sd_values = seq(-1.5, 4, 0.01), 
                                  unsd_values = round(mean(adult$YB, na.rm = TRUE) + sd_values * sd(adult$YB, na.rm = TRUE), 1)) %>% 
  dplyr::filter(unsd_values %in% seq(10, 35, 5)) %>% # only select standardized values corresponding to value for brightness from 5 to 30%
  dplyr::distinct(unsd_values, .keep_all = TRUE)



yb_predict <- ggeffects::ggpredict(YB_mod, terms = c("relative_par_load"))

fig5 <- ggplot2::ggplot(as.data.frame(yb_predict), aes(x = x, y = predicted)) +
  # Add predicted linear model
  ggplot2::geom_line(data = as.data.frame(yb_predict), 
            linewidth = 0.8, color = "black") +  
  # Add predicted confidence intervals
  ggplot2::geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                       fill = "gray", alpha = 0.4) +
  # Add raw points
  ggplot2::geom_point(data = attr(yb_predict, "rawdata"), 
             aes(x = x, y = response), 
             alpha = 0.7, shape = 16, size = 1.2, color = "black") +
  
  ggplot2::labs(x = "Relative parasite load", y = "Brightness of the yellow breast patch") +
  ggplot2::scale_y_continuous(breaks = conversion_YB$sd_values,  
                              labels = conversion_YB$unsd_values) +
  #' Set esthetics of the global plot
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::theme(plot.background = element_rect(fill = "white", color = NA),
                 panel.background = element_rect(fill = "white", color = NA),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 axis.line = element_line(color = "black", linewidth = 1),
                 axis.ticks = element_line(color = "black"),
                 axis.title.x = element_text(size = 20, face = "bold"),
                 axis.title.y = element_text(size = 18, face = "bold"),
                 axis.text = element_text(size = 14, face = "bold", color = "black"),
                 plot.margin = margin(10, 10, 10, 10))

#' Save as figure 5
ggplot2::ggsave("figures/figure5.png", plot = fig5, width = 12, height = 6, dpi = 300)


#----------------------------------------------------------------------------#
# Outputs for models excluding non-significant interactions (ESM - Table S3) #
#----------------------------------------------------------------------------#
#' Linear mixed model with brightness from the blue crown (BB) as the response variable
alt_BB_mod <- lme4::lmer(BB ~ relative_par_load + hatch_size + 
                           min_age + sex + period + laydate + 
                           (1|year) + (1|indID) + (1|pairID), data = adult_sd)

#' Linear mixed model with UV-chroma from the blue crown (BUVC) as the response variable
alt_BUVC_mod <- lme4::lmer(BUVC ~ relative_par_load*sex + hatch_size + 
                             min_age + sex + period + laydate + 
                             (1|year) + (1|indID) + (1|pairID), data = adult_sd)

#' Linear mixed model with brightness from the yellow breast patch (YB) as the response variable
alt_YB_mod <- lme4::lmer(YB ~ relative_par_load + hatch_size + 
                           min_age + sex + period + laydate + 
                           (1|year) + (1|indID) + (1|pairID), data = adult_sd)

#' Linear mixed model with UV-chroma from the yellow breast patch (YUVC) as the response variable
alt_YUVC_mod <- lme4::lmer(YUVC ~ relative_par_load + hatch_size + 
                             min_age + sex + period + laydate + 
                             (1|year) + (1|indID) + (1|pairID), data = adult_sd)

#' Linear mixed model with yellow chroma from the yellow breast patch (YC) as the response variable
alt_YC_mod <- lme4::lmer(YC ~ relative_par_load + hatch_size + 
                           min_age + sex + period + laydate + 
                           (1|year) + (1|indID) + (1|pairID), data = adult_sd)

alt_adult_outputs <- dplyr::bind_rows(output_adult_mod(mod = alt_BB_mod, name = "BB"), 
                                        output_adult_mod(mod = alt_BUVC_mod, name = "BUVC"),
                                        output_adult_mod(mod = alt_YB_mod, name = "YB"),
                                        output_adult_mod(mod = alt_YUVC_mod, name = "YUVC"),
                                        output_adult_mod(mod = alt_YC_mod, name = "YC")) %>% 
  dplyr::mutate(across(c("estimate", "std.error", "statistic", "low95ci", "up95ci"), ~ round(., digits = 4))) %>% # Round number to only keep four digits
  dplyr::select(model = "exp_var", effect, term, estimate, std_error = "std.error",  # Rename et reorganize variables
                t_value = "statistic", '95CI_low' = "low95ci", '95CI_up' = "up95ci", 
                group, nb_obs, nb_groups)

#' There are warnings related to singularities on random effects

write.csv(alt_adult_outputs, "output_tables/TableS3.csv", na = "", row.names = FALSE) # Save model outputs in the dedicated folder