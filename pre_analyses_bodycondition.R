#'Pre-analyses to test correlations between parasite load and nestling body condition
#'
#'Load packages
library(tidyverse)
library(lme4)
library(parameters)
library(extrafont)

#'Load dataset
data_cond <- read.csv("data/nestling_condition.csv") %>% 
  dplyr::mutate(year = as.character(year))

#'Testing correlation among explanatory variables and covariables
cor.test(data_cond$par_load, data_cond$nestling, method = "pearson") #simple correlation test across the whole dataset
data_cor <- data_cond %>% 
  tidyr::nest(.by = year) %>% # Nest data by year
  dplyr::mutate(model = lapply(data,
                               function(df)
                                 cor.test(df$par_load, df$nestling, method = "pearson") %>% 
                                 broom::tidy())) %>% # Allow to test correlation within year
  dplyr::select(-(data)) %>%  # Keep columns associated with correlation tests only 
  tidyr::unnest(cols = c(model))  # Display as a data frame with one row for each year


#'Building models
#'Model with nestling mass
mass_mod <- lme4::lmer(mass ~ relative_par_load * nestling + (1|year) + (1|nestbox), data = data_cond) #model

mass_tab <- broom.mixed::tidy(mass_mod) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
mass_confint <- as.data.frame(stats::confint(mass_mod)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
mass_tab <- mass_tab %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ mass_confint[4, 1], 
                                           term == "relative_par_load" ~ mass_confint[5, 1],
                                           term == "nestling" ~ mass_confint[6, 1],
                                           term == "relative_par_load:nestling" ~ mass_confint[7, 1],
                                           group == "nestbox" ~ mass_confint[1, 1],
                                           group == "year" ~ mass_confint[2, 1],
                                           group == "Residual" ~ mass_confint[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ mass_confint[4, 2],
                                          term == "relative_par_load" ~ mass_confint[5, 2],
                                          term == "nestling" ~ mass_confint[6, 2],
                                          term == "relative_par_load:nestling" ~ mass_confint[7, 2],
                                          group == "nestbox" ~ mass_confint[1, 2],
                                          group == "year" ~ mass_confint[2, 2],
                                          group == "Residual" ~ mass_confint[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "nestbox" ~ lme4::ngrps(mass_mod)[1],
                                             group == "year" ~ lme4::ngrps(mass_mod)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(mass_mod),
                                          TRUE ~ NA_integer_))


#'Model with nestling tarsus length
tars_mod <- lme4::lmer(tarsus ~ relative_par_load * nestling + (1|year) + (1|nestbox), data = data_cond) #model

tars_tab <- broom.mixed::tidy(tars_mod) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
tars_confint <- as.data.frame(stats::confint(tars_mod)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
tars_tab <- tars_tab %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ tars_confint[4, 1], 
                                           term == "relative_par_load" ~ tars_confint[5, 1],
                                           term == "nestling" ~ tars_confint[6, 1],
                                           term == "relative_par_load:nestling" ~ tars_confint[7, 1],
                                           group == "nestbox" ~ tars_confint[1, 1],
                                           group == "year" ~ tars_confint[2, 1],
                                           group == "Residual" ~ tars_confint[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ tars_confint[4, 2],
                                          term == "relative_par_load" ~ tars_confint[5, 2],
                                          term == "nestling" ~ tars_confint[6, 2],
                                          term == "relative_par_load:nestling" ~ tars_confint[7, 2],
                                          group == "nestbox" ~ tars_confint[1, 2],
                                          group == "year" ~ tars_confint[2, 2],
                                          group == "Residual" ~ tars_confint[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "nestbox" ~ lme4::ngrps(tars_mod)[1],
                                             group == "year" ~ lme4::ngrps(tars_mod)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(tars_mod),
                                          TRUE ~ NA_integer_))

# Create a table enabling the creation of the forest plot

all_tab <- dplyr::bind_rows(dplyr::mutate(mass_tab, exp_var = "mass"), # Add column to include variables to explain
                           dplyr::mutate(tars_tab, exp_var = "tarsus")) %>% 
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
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.1) +
  guides(color = "none", fill = "none") +
  theme(plot.margin = margin(t = 0.25, b = 0.25, l = 0.25, r = 20),
        text = element_text(family = "Times New Roman"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(colour = "black", linewidth = 1, linetype = "solid"),
        axis.line.y = element_blank(),
        panel.spacing.x = unit(3, "lines"),
        panel.spacing.y = unit(1.5, "lines"),
        panel.background = element_blank(),
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
bc_predict = ggeffects::ggpredict(mass_mod, terms = c("relative_par_load", "nestling")) 

  
moderator_values <- sort(c(as.numeric(as.character(unique(bc_predict$group))),
                           range(c(attr(bc_predict, "rawdata")$group, 
                                   as.numeric(as.character(unique(bc_predict$group)))))))
  
ggplot(as.data.frame(bc_predict), 
       aes(x = x, y = predicted, 
           group = group, 
           color = as.numeric(as.character(group)), 
           fill = as.numeric(as.character(group)))) +
  geom_point(data = attr(bc_predict, "rawdata"), 
               aes(x = jitter(x),
                   y = jitter(response)),
               alpha = 0.6) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = FALSE, alpha = 0.2) +
  geom_line() +
  scale_color_gradient(low = "white",
                       high = "black",
                       aesthetics = c("colour", "fill"), 
                       breaks = moderator_values, 
                       limits = range(moderator_values), 
                       labels = c("1", "5", "6.5", "8", "11")) +
  labs(color = "Number of\nhatchlings", fill = "Number of\nhatchlings", title = "" , x = "Relative parasite load", y = "Nestling mass (g)") +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 14, face = "bold", vjust = 0.8, hjust = 0.5),
        panel.background = element_rect(fill = "transparent", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor =element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold", color = "black"),
        legend.position = c(0.85, 0.72),
        legend.box = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.justification = c(0.3, 0.15),
        legend.title.align = 1,
        legend.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        strip.background = element_rect(linetype = "solid", color = "black", size = 1, fill = "grey95"))
  


