#'Pre-analyses to test correlations between parasite load and nestling body condition
install.packages("tidyverse", "lme4", "parameters", "extrafont", "broom.mixed", "cowplot", "ggeffects")

#'Load packages
library(tidyverse)
library(lme4)
library(parameters)
library(extrafont)

#'Load dataset
data_cond <- read.csv("data/nestling_condition.csv") %>% 
  dplyr::mutate(year = as.character(year),
                indID = as.character(indID))

#'Testing correlation among explanatory variables and covariables
cor.test(data_cond$par_load, data_cond$hatch_size, method = "pearson") #simple correlation test across the whole dataset
data_cor <- data_cond %>% 
  tidyr::nest(.by = year) %>% # Nest data by year
  dplyr::mutate(model = lapply(data,
                               function(df)
                                 cor.test(df$par_load, df$hatch_size, method = "pearson") %>% 
                                 broom::tidy())) %>% # Allow to test correlation within year
  dplyr::select(-(data)) %>%  # Keep columns associated with correlation tests only 
  tidyr::unnest(cols = c(model))  # Display as a data frame with one row for each year


#'Building models
#'Model with nestling mass
mass_mod <- lme4::lmer(mass ~ relative_par_load * hatch_size + (1|year) + (1|broodID), data = data_cond) #model

mass_tab <- broom.mixed::tidy(mass_mod) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
mass_confint <- as.data.frame(stats::confint(mass_mod)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
mass_tab <- mass_tab %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ mass_confint[4, 1], 
                                           term == "relative_par_load" ~ mass_confint[5, 1],
                                           term == "hatch_size" ~ mass_confint[6, 1],
                                           term == "relative_par_load:hatch_size" ~ mass_confint[7, 1],
                                           group == "broodID" ~ mass_confint[1, 1],
                                           group == "year" ~ mass_confint[2, 1],
                                           group == "Residual" ~ mass_confint[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ mass_confint[4, 2],
                                          term == "relative_par_load" ~ mass_confint[5, 2],
                                          term == "hatch_size" ~ mass_confint[6, 2],
                                          term == "relative_par_load:hatch_size" ~ mass_confint[7, 2],
                                          group == "brooodID" ~ mass_confint[1, 2],
                                          group == "year" ~ mass_confint[2, 2],
                                          group == "Residual" ~ mass_confint[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "broodID" ~ lme4::ngrps(mass_mod)[1],
                                             group == "year" ~ lme4::ngrps(mass_mod)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(mass_mod),
                                          TRUE ~ NA_integer_),
                exp_var = "mass")


#'Model with nestling tarsus length
tars_mod <- lme4::lmer(tarsus ~ relative_par_load * hatch_size + (1|year) + (1|broodID), data = data_cond) #model

tars_tab <- broom.mixed::tidy(tars_mod) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
tars_confint <- as.data.frame(stats::confint(tars_mod)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
tars_tab <- tars_tab %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ tars_confint[4, 1], 
                                           term == "relative_par_load" ~ tars_confint[5, 1],
                                           term == "hatch_size" ~ tars_confint[6, 1],
                                           term == "relative_par_load:hatch_size" ~ tars_confint[7, 1],
                                           group == "broodID" ~ tars_confint[1, 1],
                                           group == "year" ~ tars_confint[2, 1],
                                           group == "Residual" ~ tars_confint[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ tars_confint[4, 2],
                                          term == "relative_par_load" ~ tars_confint[5, 2],
                                          term == "hatch_size" ~ tars_confint[6, 2],
                                          term == "relative_par_load:hatch_size" ~ tars_confint[7, 2],
                                          group == "broodID" ~ tars_confint[1, 2],
                                          group == "year" ~ tars_confint[2, 2],
                                          group == "Residual" ~ tars_confint[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "broodID" ~ lme4::ngrps(tars_mod)[1],
                                             group == "year" ~ lme4::ngrps(tars_mod)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(tars_mod),
                                          TRUE ~ NA_integer_), 
                exp_var = "tarsus")

# Create a table enabling the creation of the forest plot

all_tab <- dplyr::bind_rows(mass_tab, tars_tab) %>% 
  dplyr::filter(term != "(Intercept)" & effect != "ran_pars") %>% # Remove intercepts and random effects values
  dplyr::mutate_at(vars(estimate, low95ci, up95ci), ~ round(., digits = 4)) %>% # Round numbers to keep only 4 digits
  dplyr::select(exp_var, term, estimate, low95ci, up95ci) # Reduce table width to the needed columns


#Figures
#'Create a forest plot to display the outputs of the tests
#' extrafont::font_import() 
#' extrafont::loadfonts(device = "win") (code to run to extend the number of available fonts - take time so I added #' to avoid running it everytime)
bodycond_fp <- ggplot(all_tab, aes(x = term, y = estimate, ymin = low95ci, ymax = up95ci)) +
  geom_pointrange(size = 1, linewidth = 1, shape = "¤") +
  coord_flip() +
  labs(title = "" , x = "", y = "") +
  scale_x_discrete(label = c("Number of hatchlings", "Relative parasite load", "Interaction between\nrelative parasite load\nand number of hatchlings"))  +
  scale_y_continuous(limits = c(-0.20, 0.20), breaks = c(-0.10,  0, 0.10), label = c("-0.10", "0", "0.10")) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.4) +
  guides(color = "none", fill = "none") +
  theme(plot.margin = margin(t = 0.25, b = 0.25, l = 0.25, r = 20),
        plot.background = element_rect(fill = "ghostwhite"),
        text = element_text(family = "Noto Sans"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(colour = "black", linewidth = 1, linetype = "solid"),
        axis.line.y = element_blank(),
        panel.spacing.x = unit(3, "lines"),
        panel.spacing.y = unit(1.5, "lines"),
        panel.background = element_rect(fill = "ghostwhite"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 16, face = "bold", color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold", color = "black"),
        axis.text.x = element_text(size = 10, face = "bold", color = "black", hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold", color = "black", hjust = 0)) +
  facet_wrap(. ~ exp_var, 
             nrow = 1, 
             ncol = 2, 
             labeller = labeller(exp_var = c("mass" = "Nestling mass (g)", "tarsus" = "Nestling tarsus length (mm)")))
bodycond_fp

# Plot displaying results from model on mass
bc_predict = ggeffects::ggpredict(mass_mod, terms = c("relative_par_load", "hatch_size")) 

  
moderator_values <- sort(c(as.numeric(as.character(unique(bc_predict$group))),
                           range(c(attr(bc_predict, "rawdata")$group, 
                                   as.numeric(as.character(unique(bc_predict$group)))))))
  
bodycond_lm <- ggplot(as.data.frame(bc_predict), 
       aes(x = x, y = predicted, 
           group = group, 
           color = as.numeric(as.character(group)), 
           fill = as.numeric(as.character(group)))) +
  geom_point(data = attr(bc_predict, "rawdata"), 
               aes(x = jitter(x),
                   y = jitter(response)),
               alpha = 0.6,
             shape = 16,
             size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = FALSE, alpha = 0.2) +
  geom_line() +
  scale_color_gradient(low = "white",
                       high = "black",
                       aesthetics = c("colour", "fill"), 
                       breaks = moderator_values, 
                       limits = range(moderator_values), 
                       labels = c("1", "5", "6.5", "8", "11")) +
  labs(color = "Number of\nhatchlings", fill = "Number of\nhatchlings", title = "" , x = "Relative parasite load", y = "Nestling mass (g)") +
  theme(text = element_text(family = "Noto Sans"),
        plot.title = element_text(size = 14, face = "bold", vjust = 0.8, hjust = 0.5),
        plot.background = element_rect(fill = "ghostwhite"),
        panel.background = element_rect(fill = "ghostwhite"),
        panel.grid.major = element_blank(),
        panel.grid.minor =element_blank(),
        axis.line = element_line(colour = "black", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold", color = "black"),
        legend.position = c(0.95, 0.9),
        legend.box = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.justification = c(0.3, 0.15),
        legend.title.align = 1,
        legend.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        strip.background = element_blank())

# Assembling plots to form "Figure 1"
plot_bc <- cowplot::ggdraw() + 
  theme(plot.background = element_rect(fill = "ghostwhite", color = NA)) +
  cowplot::draw_plot(bodycond_fp, x = 0, y = 0.15, hjust = 0, width = 0.45, height = 0.8) +
  cowplot::draw_plot(bodycond_lm, x = 0.55, y = 0.15, hjust = 0, width = 0.35, height = 0.7)  +
  cowplot::draw_plot_label(label = c("(a)", "(b)"), x = c(0, 0.55), y = c(0.96, 0.96),  hjust = 0, family = "Noto Sans", size = 14)
cowplot::save_plot("figures/fig1.jpg", plot_bc, ncol = 1, nrow = 1, base_height = 8, base_width = 18)


# Using standardized data
data_cond_sd <- data_cond %>% 
  dplyr::mutate(across(4:8, ~ scale(.)[,1]))

#'Building models
#'Model with nestling mass
mass_mod_sd <- lme4::lmer(mass ~ relative_par_load * hatch_size + (1|year) + (1|broodID), data = data_cond_sd) #model

mass_tab_sd <- broom.mixed::tidy(mass_mod_sd) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
mass_confint_sd <- as.data.frame(stats::confint(mass_mod_sd)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
mass_tab_sd <- mass_tab_sd %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ mass_confint_sd[4, 1], 
                                           term == "relative_par_load" ~ mass_confint_sd[5, 1],
                                           term == "hatch_size" ~ mass_confint_sd[6, 1],
                                           term == "relative_par_load:hatch_size" ~ mass_confint_sd[7, 1],
                                           group == "broodID" ~ mass_confint_sd[1, 1],
                                           group == "year" ~ mass_confint_sd[2, 1],
                                           group == "Residual" ~ mass_confint_sd[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ mass_confint_sd[4, 2],
                                          term == "relative_par_load" ~ mass_confint_sd[5, 2],
                                          term == "hatch_size" ~ mass_confint_sd[6, 2],
                                          term == "relative_par_load:hatch_size" ~ mass_confint_sd[7, 2],
                                          group == "brooodID" ~ mass_confint_sd[1, 2],
                                          group == "year" ~ mass_confint_sd[2, 2],
                                          group == "Residual" ~ mass_confint_sd[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "broodID" ~ lme4::ngrps(mass_mod_sd)[1],
                                             group == "year" ~ lme4::ngrps(mass_mod_sd)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(mass_mod_sd),
                                          TRUE ~ NA_integer_),
                exp_var = "mass_sd")


#'Model with nestling tarsus length
tars_mod_sd <- lme4::lmer(tarsus ~ relative_par_load * hatch_size + (1|year) + (1|broodID), data = data_cond_sd) #model

tars_tab_sd <- broom.mixed::tidy(tars_mod_sd) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
tars_confint_sd <- as.data.frame(stats::confint(tars_mod_sd)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
tars_tab_sd <- tars_tab_sd %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ tars_confint_sd[4, 1], 
                                           term == "relative_par_load" ~ tars_confint_sd[5, 1],
                                           term == "hatch_size" ~ tars_confint_sd[6, 1],
                                           term == "relative_par_load:hatch_size" ~ tars_confint_sd[7, 1],
                                           group == "broodID" ~ tars_confint_sd[1, 1],
                                           group == "year" ~ tars_confint_sd[2, 1],
                                           group == "Residual" ~ tars_confint_sd[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ tars_confint_sd[4, 2],
                                          term == "relative_par_load" ~ tars_confint_sd[5, 2],
                                          term == "hatch_size" ~ tars_confint_sd[6, 2],
                                          term == "relative_par_load:hatch_size" ~ tars_confint_sd[7, 2],
                                          group == "broodID" ~ tars_confint_sd[1, 2],
                                          group == "year" ~ tars_confint_sd[2, 2],
                                          group == "Residual" ~ tars_confint_sd[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "broodID" ~ lme4::ngrps(tars_mod_sd)[1],
                                             group == "year" ~ lme4::ngrps(tars_mod_sd)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(tars_mod_sd),
                                          TRUE ~ NA_integer_), 
                exp_var = "tarsus_sd")

# Create a table enabling the creation of the forest plot

all_tab_sd <- dplyr::bind_rows(mass_tab_sd, tars_tab_sd) %>% 
  dplyr::filter(effect != "ran_pars") %>% # Remove  random effects values
  dplyr::mutate_at(vars(estimate, low95ci, up95ci), ~ round(., digits = 4)) %>% # Round numbers to keep only 4 digits
  dplyr::select(exp_var, term, estimate, low95ci, up95ci) # Reduce table width to the needed columns

all_tab_sd$term <- factor(all_tab_sd$term, c("(Intercept)", "relative_par_load", "hatch_size", "relative_par_load:hatch_size"))

#Figures
#'Create a forest plot to display the outputs of the tests
#' extrafont::font_import() 
#' extrafont::loadfonts(device = "win") (code to run to extend the number of available fonts - take time so I added #' to avoid running it everytime)
bodycond_fp_sd <- ggplot(all_tab_sd, aes(x = term, y = estimate, ymin = low95ci, ymax = up95ci)) +
  geom_pointrange(size = 1, linewidth = 1, shape = "¤") +
  coord_flip() +
  labs(title = "" , x = "", y = "") +
  scale_x_discrete(label = c("Intercept", "Relative parasite load", "Number of hatchlings", "Interaction between\nrelative parasite load\nand number of hatchlings"))  +
  scale_y_continuous(limits = c(-0.5, 0.5), breaks = c(-0.50, -0.25, 0, 0.25, 0.5), label = c("-0.5", "-0.25", "0", "0.25", "0.5")) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.4) +
  guides(color = "none", fill = "none") +
  theme(plot.margin = margin(t = 0.25, b = 0.25, l = 0.25, r = 20),
        plot.background = element_rect(fill = "ghostwhite"),
        text = element_text(family = "Noto Sans"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(colour = "black", linewidth = 1, linetype = "solid"),
        axis.line.y = element_blank(),
        panel.spacing.x = unit(3, "lines"),
        panel.spacing.y = unit(1.5, "lines"),
        panel.background = element_rect(fill = "ghostwhite"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 16, face = "bold", color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold", color = "black"),
        axis.text.x = element_text(size = 10, face = "bold", color = "black", hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold", color = "black", hjust = 0)) +
  facet_wrap(. ~ exp_var, 
             nrow = 1, 
             ncol = 2, 
             labeller = labeller(exp_var = c("mass_sd" = "Nestling mass (g)", "tarsus_sd" = "Nestling tarsus length (mm)")))
bodycond_fp_sd


# Plot displaying results from model on mass
bc_predict_sd = ggeffects::ggpredict(mass_mod_sd, terms = c("relative_par_load", "hatch_size")) 


moderator_values_sd <- sort(c(as.numeric(as.character(unique(bc_predict_sd$group))),
                           range(c(attr(bc_predict_sd, "rawdata")$group, 
                                   as.numeric(as.character(unique(bc_predict_sd$group)))))))

bodycond_lm_sd <- ggplot(as.data.frame(bc_predict_sd), 
                      aes(x = x, y = predicted, 
                          group = group, 
                          color = as.numeric(as.character(group)), 
                          fill = as.numeric(as.character(group)))) +
  geom_point(data = attr(bc_predict_sd, "rawdata"), 
             aes(x = jitter(x),
                 y = jitter(response)),
             alpha = 0.6,
             shape = 16,
             size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = FALSE, alpha = 0.2) +
  geom_line() +
  scale_x_continuous(limits = c(-1.457642, 4.923553), )
  scale_color_gradient(low = "white",
                       high = "black",
                       aesthetics = c("colour", "fill"), 
                       breaks = moderator_values_sd, 
                       limits = range(moderator_values_sd), 
                       labels = c("1", "5", "6.5", "8", "11")) +
  labs(color = "Number of\nhatchlings", fill = "Number of\nhatchlings", title = "" , x = "Relative parasite load", y = "Nestling mass (g)") +
  theme(text = element_text(family = "Noto Sans"),
        plot.title = element_text(size = 14, face = "bold", vjust = 0.8, hjust = 0.5),
        plot.background = element_rect(fill = "ghostwhite"),
        panel.background = element_rect(fill = "ghostwhite"),
        panel.grid.major = element_blank(),
        panel.grid.minor =element_blank(),
        axis.line = element_line(colour = "black", linewidth = 1, linetype = "solid"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold", color = "black"),
        legend.position = c(0.95, 0.9),
        legend.box = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.justification = c(0.3, 0.15),
        legend.title.align = 1,
        legend.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        strip.background = element_blank())

# Assembling plots to form "Figure 1"
plot_bc_sd <- cowplot::ggdraw() + 
  theme(plot.background = element_rect(fill = "ghostwhite", color = NA)) +
  cowplot::draw_plot(bodycond_fp_sd, x = 0, y = 0.15, hjust = 0, width = 0.45, height = 0.8) +
  cowplot::draw_plot(bodycond_lm_sd, x = 0.55, y = 0.15, hjust = 0, width = 0.35, height = 0.7)  +
  cowplot::draw_plot_label(label = c("(a)", "(b)"), x = c(0, 0.55), y = c(0.96, 0.96),  hjust = 0, family = "Noto Sans", size = 14)
cowplot::save_plot("figures/fig1_sd.jpg", plot_bc_sd, ncol = 1, nrow = 1, base_height = 8, base_width = 18)


#Printing outputs from models (unscaled and scaled)
##Unscaled
all_tab <- dplyr::bind_rows(mass_tab, tars_tab) %>% 
  dplyr::mutate_at(vars(estimate, low95ci, up95ci), ~ round(., digits = 4)) %>% # Round numbers to keep only 4 digits
  dplyr::select(exp_var, term, estimate, low95ci, up95ci, nb_obs, nb_groups) # Reduce table width to the needed columns
write.csv(all_tab, "output_tables/output_preanalyses_unsc.csv", row.names = FALSE)

##Standardized (scaled)
all_tab_sd <- dplyr::bind_rows(mass_tab_sd, tars_tab_sd) %>% 
  dplyr::filter(effect != "ran_pars") %>% # Remove  random effects values
  dplyr::mutate_at(vars(estimate, low95ci, up95ci), ~ round(., digits = 4)) %>% # Round numbers to keep only 4 digits
  dplyr::select(exp_var, term, estimate, low95ci, up95ci, nb_obs, nb_groups) # Reduce table width to the needed columns
write.csv(all_tab_sd, "output_tables/output_preanalyses_sc.csv", row.names = FALSE)
