#'Analyses to test correlations between plumage color after molt and parasite load for breeding event before molt
install.packages("tidyverse", "lme4", "parameters", "extrafont", "broom.mixed", "cowplot", "ggeffects")

#'Load packages
library(tidyverse)
library(lme4)
library(parameters)
library(extrafont)

#'Load dataset
adult <- read.csv("data/adult_color.csv") %>% 
  dplyr::mutate(year = as.character(year),
                indID = as.character(indID))


#'Building models
#'Mean brightness from the blue crown
bba_mod <- lme4::lmer(BB ~ relative_par_load * hatch_size + relative_par_load * sex + min_age + period + (1|year) + (1|broodID) + (1|indID), data = adult)
bba_tab <- broom.mixed::tidy(bba_mod) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
bba_confint <- as.data.frame(stats::confint(bba_mod)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
bba_tab <- bba_tab %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ bba_confint[5, 1], 
                                           term == "relative_par_load" ~ bba_confint[6, 1],
                                           term == "hatch_size" ~ bba_confint[7, 1],
                                           term == "sexM" ~ bba_confint[8, 1],
                                           term == "min_age" ~ bba_confint[9, 1],
                                           term == "periodF" ~ bba_confint[10, 1],
                                           term == "relative_par_load:hatch_size" ~ bba_confint[11, 1],
                                           term == "relative_par_load:sexM" ~ bba_confint[12, 1],
                                           group == "indID" ~ bba_confint[1, 1],
                                           group == "broodID" ~ bba_confint[2, 1],
                                           group == "year" ~ bba_confint[3, 1],
                                           group == "Residual" ~ bba_confint[4, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ bba_confint[5, 2], 
                                          term == "relative_par_load" ~ bba_confint[6, 2],
                                          term == "hatch_size" ~ bba_confint[7, 2],
                                          term == "sexM" ~ bba_confint[8, 2],
                                          term == "min_age" ~ bba_confint[9, 2],
                                          term == "periodF" ~ bba_confint[10, 2],
                                          term == "relative_par_load:hatch_size" ~ bba_confint[11, 2],
                                          term == "relative_par_load:sexM" ~ bba_confint[12, 2],
                                          group == "indID" ~ bba_confint[1, 2],
                                          group == "broodID" ~ bba_confint[2, 2],
                                          group == "year" ~ bba_confint[3, 2],
                                          group == "Residual" ~ bba_confint[4, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "indID" ~ lme4::ngrps(bba_mod)[1],
                                             group == "broodID" ~ lme4::ngrps(bba_mod)[2],
                                             group == "year" ~ lme4::ngrps(bba_mod)[3],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(bba_mod),
                                          TRUE ~ NA_integer_),
                exp_var = "BB")


#'UV chroma from the blue crown
buvca_mod <- lme4::lmer(BUVC ~ relative_par_load * hatch_size + relative_par_load * sex + min_age + period + (1|year) + (1|broodID) + (1|indID), data = adult)
buvca_tab <- broom.mixed::tidy(buvca_mod) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
buvca_confint <- as.data.frame(stats::confint(buvca_mod)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
buvca_tab <- buvca_tab %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ buvca_confint[5, 1], 
                                           term == "relative_par_load" ~ buvca_confint[6, 1],
                                           term == "hatch_size" ~ buvca_confint[7, 1],
                                           term == "sexM" ~ buvca_confint[8, 1],
                                           term == "min_age" ~ buvca_confint[9, 1],
                                           term == "periodF" ~ buvca_confint[10, 1],
                                           term == "relative_par_load:hatch_size" ~ buvca_confint[11, 1],
                                           term == "relative_par_load:sexM" ~ buvca_confint[12, 1],
                                           group == "indID" ~ buvca_confint[1, 1],
                                           group == "broodID" ~ buvca_confint[2, 1],
                                           group == "year" ~ buvca_confint[3, 1],
                                           group == "Residual" ~ buvca_confint[4, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ buvca_confint[5, 2], 
                                          term == "relative_par_load" ~ buvca_confint[6, 2],
                                          term == "hatch_size" ~ buvca_confint[7, 2],
                                          term == "sexM" ~ buvca_confint[8, 2],
                                          term == "min_age" ~ buvca_confint[9, 2],
                                          term == "periodF" ~ buvca_confint[10, 2],
                                          term == "relative_par_load:hatch_size" ~ buvca_confint[11, 2],
                                          term == "relative_par_load:sexM" ~ buvca_confint[12, 2],
                                          group == "indID" ~ buvca_confint[1, 2],
                                          group == "broodID" ~ buvca_confint[2, 2],
                                          group == "year" ~ buvca_confint[3, 2],
                                          group == "Residual" ~ buvca_confint[4, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "indID" ~ lme4::ngrps(buvca_mod)[1],
                                             group == "broodID" ~ lme4::ngrps(buvca_mod)[2],
                                             group == "year" ~ lme4::ngrps(buvca_mod)[3],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(buvca_mod),
                                          TRUE ~ NA_integer_),
                exp_var = "BUVC")


#'Mean brightness from the yellow breast patch
yba_mod <- lme4::lmer(YB ~ relative_par_load * hatch_size + relative_par_load * sex + min_age + period + (1|year) + (1|broodID) + (1|indID), data = adult)
yba_tab <- broom.mixed::tidy(yba_mod) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
yba_confint <- as.data.frame(stats::confint(yba_mod)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
yba_tab <- yba_tab %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ yba_confint[5, 1], 
                                           term == "relative_par_load" ~ yba_confint[6, 1],
                                           term == "hatch_size" ~ yba_confint[7, 1],
                                           term == "sexM" ~ yba_confint[8, 1],
                                           term == "min_age" ~ yba_confint[9, 1],
                                           term == "periodF" ~ yba_confint[10, 1],
                                           term == "relative_par_load:hatch_size" ~ yba_confint[11, 1],
                                           term == "relative_par_load:sexM" ~ yba_confint[12, 1],
                                           group == "indID" ~ yba_confint[1, 1],
                                           group == "broodID" ~ yba_confint[2, 1],
                                           group == "year" ~ yba_confint[3, 1],
                                           group == "Residual" ~ yba_confint[4, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ yba_confint[5, 2], 
                                          term == "relative_par_load" ~ yba_confint[6, 2],
                                          term == "hatch_size" ~ yba_confint[7, 2],
                                          term == "sexM" ~ yba_confint[8, 2],
                                          term == "min_age" ~ yba_confint[9, 2],
                                          term == "periodF" ~ yba_confint[10, 2],
                                          term == "relative_par_load:hatch_size" ~ yba_confint[11, 2],
                                          term == "relative_par_load:sexM" ~ yba_confint[12, 2],
                                          group == "indID" ~ yba_confint[1, 2],
                                          group == "broodID" ~ yba_confint[2, 2],
                                          group == "year" ~ yba_confint[3, 2],
                                          group == "Residual" ~ yba_confint[4, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "indID" ~ lme4::ngrps(yba_mod)[1],
                                             group == "broodID" ~ lme4::ngrps(yba_mod)[2],
                                             group == "year" ~ lme4::ngrps(yba_mod)[3],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(yba_mod),
                                          TRUE ~ NA_integer_),
                exp_var = "YB")


#'UV chroma from the yellow breast patch
yuvca_mod <- lme4::lmer(YUVC ~ relative_par_load * hatch_size + relative_par_load * sex + min_age + period + (1|year) + (1|broodID) + (1|indID), data = adult)
yuvca_tab <- broom.mixed::tidy(yuvca_mod) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
yuvca_confint <- as.data.frame(stats::confint(yuvca_mod)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
yuvca_tab <- yuvca_tab %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ yuvca_confint[5, 1], 
                                           term == "relative_par_load" ~ yuvca_confint[6, 1],
                                           term == "hatch_size" ~ yuvca_confint[7, 1],
                                           term == "sexM" ~ yuvca_confint[8, 1],
                                           term == "min_age" ~ yuvca_confint[9, 1],
                                           term == "periodF" ~ yuvca_confint[10, 1],
                                           term == "relative_par_load:hatch_size" ~ yuvca_confint[11, 1],
                                           term == "relative_par_load:sexM" ~ yuvca_confint[12, 1],
                                           group == "indID" ~ yuvca_confint[1, 1],
                                           group == "broodID" ~ yuvca_confint[2, 1],
                                           group == "year" ~ yuvca_confint[3, 1],
                                           group == "Residual" ~ yuvca_confint[4, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ yuvca_confint[5, 2], 
                                          term == "relative_par_load" ~ yuvca_confint[6, 2],
                                          term == "hatch_size" ~ yuvca_confint[7, 2],
                                          term == "sexM" ~ yuvca_confint[8, 2],
                                          term == "min_age" ~ yuvca_confint[9, 2],
                                          term == "periodF" ~ yuvca_confint[10, 2],
                                          term == "relative_par_load:hatch_size" ~ yuvca_confint[11, 2],
                                          term == "relative_par_load:sexM" ~ yuvca_confint[12, 2],
                                          group == "indID" ~ yuvca_confint[1, 2],
                                          group == "broodID" ~ yuvca_confint[2, 2],
                                          group == "year" ~ yuvca_confint[3, 2],
                                          group == "Residual" ~ yuvca_confint[4, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "indID" ~ lme4::ngrps(yuvca_mod)[1],
                                             group == "broodID" ~ lme4::ngrps(yuvca_mod)[2],
                                             group == "year" ~ lme4::ngrps(yuvca_mod)[3],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(yuvca_mod),
                                          TRUE ~ NA_integer_),
                exp_var = "YUVC")


#'Yellow chroma from the yellow breast patch
yca_mod <- lme4::lmer(YC ~ relative_par_load * hatch_size + relative_par_load * sex + min_age + period + (1|year) + (1|broodID) + (1|indID), data = adult)
yca_tab <- broom.mixed::tidy(yca_mod) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
yca_confint <- as.data.frame(stats::confint(yca_mod)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
yca_tab <- yca_tab %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ yca_confint[5, 1], 
                                           term == "relative_par_load" ~ yca_confint[6, 1],
                                           term == "hatch_size" ~ yca_confint[7, 1],
                                           term == "sexM" ~ yca_confint[8, 1],
                                           term == "min_age" ~ yca_confint[9, 1],
                                           term == "periodF" ~ yca_confint[10, 1],
                                           term == "relative_par_load:hatch_size" ~ yca_confint[11, 1],
                                           term == "relative_par_load:sexM" ~ yca_confint[12, 1],
                                           group == "indID" ~ yca_confint[1, 1],
                                           group == "broodID" ~ yca_confint[2, 1],
                                           group == "year" ~ yca_confint[3, 1],
                                           group == "Residual" ~ yca_confint[4, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ yca_confint[5, 2], 
                                          term == "relative_par_load" ~ yca_confint[6, 2],
                                          term == "hatch_size" ~ yca_confint[7, 2],
                                          term == "sexM" ~ yca_confint[8, 2],
                                          term == "min_age" ~ yca_confint[9, 2],
                                          term == "periodF" ~ yca_confint[10, 2],
                                          term == "relative_par_load:hatch_size" ~ yca_confint[11, 2],
                                          term == "relative_par_load:sexM" ~ yca_confint[12, 2],
                                          group == "indID" ~ yca_confint[1, 2],
                                          group == "broodID" ~ yca_confint[2, 2],
                                          group == "year" ~ yca_confint[3, 2],
                                          group == "Residual" ~ yca_confint[4, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "indID" ~ lme4::ngrps(yca_mod)[1],
                                             group == "broodID" ~ lme4::ngrps(yca_mod)[2],
                                             group == "year" ~ lme4::ngrps(yca_mod)[3],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(yca_mod),
                                          TRUE ~ NA_integer_),
                exp_var = "YC")



#'Group all tables together for forestplot purposes
ad_tab <- dplyr::bind_rows(bba_tab, buvca_tab, yba_tab, yuvca_tab, yca_tab) %>% 
  dplyr::filter(effect != "ran_pars") %>% # Remove random effects values
  dplyr::mutate_at(vars(estimate, low95ci, up95ci), ~ round(., digits = 4)) %>% # Round numbers to keep only 4 digits
  dplyr::select(exp_var, term, estimate, low95ci, up95ci) # Reduce table width to the needed columns


#'Order and level factors to organize the forest plot
ad_tab$exp_var <- factor(ad_tab$exp_var, c("BB", "BUVC", "YB", "YUVC", "YC"))
ad_tab$term <- factor(ad_tab$term, c("relative_par_load:hatch_size", "hatch_size", "relative_par_load:sexM",
                                       "relative_par_load", "min_age", "periodF", "sexM", "(Intercept)"))

color_scheme <- c(rep("royalblue", 2), rep("darkorange", 3))


#Figures
#'Create a forest plot to display the outputs of the tests
#' extrafont::font_import() 
#' extrafont::loadfonts(device = "win") (code to run to extend the number of available fonts - take time so I added #' to avoid running it everytime)
ad_fp <- ggplot(ad_tab, aes(x = term, y = estimate, ymin = low95ci, ymax = up95ci, color = exp_var)) +
  geom_pointrange(size = 1, linewidth = 1, shape = "¤", show.legend = FALSE) +
  scale_color_manual(values = color_scheme) +
  coord_flip() +
  labs(title = "" , x = "", y = "") +
  scale_x_discrete(label = c("Interaction between\nrelative parasite load\nand number of hatchlings", "Number of hatchlings", 
                             "Relative parasite\nload for male", "Relative parasite load", "Age", "Sampled when\nfeeding nestlings",
                             "Male", "Female sampled when\nbuilding nest"))  +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.4) +
  theme(plot.margin = margin(t = 20, b = 0.25, l = 0.25, r = 20),
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
             ncol = 5, 
             scales = "free_x",
             labeller = labeller(exp_var = c("BB" = "Mean brightness", "BUVC" = "UV-chroma",
                                             "YB" = "Mean brightness", "YUVC" = "UV-chroma", "YC" = "Yellow chroma"))) 
ad_fp

# Additional text to form "Figure 3"
plot_ad <- cowplot::ggdraw() +
  cowplot::draw_plot(ad_fp, x = 0, y = 0, hjust = 0, width = 1, height = 1) +
  cowplot::draw_text("Blue crown", x = 0.29, y = 0.96, size = 14, fontface = "bold", family = "Noto Sans", color = "royalblue") +
  cowplot::draw_line(x = c(0.2, 0.38), y = c(0.93, 0.93), linewidth = 1, color = "black") +
  cowplot::draw_text("Yellow breast patch", x = 0.75, y = 0.96, size = 14, fontface = "bold", family = "Noto Sans", color = "darkorange") +
  cowplot::draw_line(x = c(0.55, 0.9), y = c(0.93, 0.93), linewidth = 1, color = "black")
cowplot::save_plot("figures/fig3.jpg", plot_ad, ncol = 1, nrow = 1, base_height = 6, base_width = 18)


# Plot displaying results from model on mean brightness from the blue crown
bb_hatch_predict = ggeffects::ggpredict(bba_mod, terms = c("relative_par_load", "hatch_size")) 


moderator_values <- sort(c(as.numeric(as.character(unique(bb_hatch_predict$group))),
                           range(c(attr(bb_hatch_predict, "rawdata")$group, 
                                   as.numeric(as.character(unique(bb_hatch_predict$group)))))))

bb_hatch_lm <- ggplot(as.data.frame(bb_hatch_predict), 
                      aes(x = x, y = predicted, 
                          group = group, 
                          color = as.numeric(as.character(group)), 
                          fill = as.numeric(as.character(group)))) +
  geom_point(data = attr(bb_hatch_predict, "rawdata"), 
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
                       breaks = c("2", "6", "10"), #Issue displaying labels for hatching size
                       limits = range(moderator_values), 
                       labels = c("2", "6",  "10")) +
  labs(color = "Number of\nhatchlings", fill = "Number of\nhatchlings", title = "" , x = "Relative parasite load", y = "Mean brightness from the blue crown") +
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
        legend.position = c(0.65, 0.6),
        legend.box = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.justification = c(0.3, 0.15),
        legend.title.align = 1,
        legend.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        strip.background = element_blank())


bb_sex_predict = ggeffects::ggpredict(bba_mod, terms = c("relative_par_load", "sex")) 


moderator_values <- sort(c(as.numeric(as.character(unique(bb_sex_predict$group))),
                           range(c(attr(bb_sex_predict, "rawdata")$group, 
                                   as.numeric(as.character(unique(bb_sex_predict$group)))))))

bb_sex_lm <- ggplot(as.data.frame(bb_sex_predict), 
                      aes(x = x, y = predicted, 
                          group = group, 
                          color = as.numeric(as.character(group)), 
                          fill = as.numeric(as.character(group)))) +
  geom_point(data = attr(bb_sex_predict, "rawdata"), 
             aes(x = jitter(x),
                 y = jitter(response)),
             alpha = 0.6,
             shape = 16,
             size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = FALSE, alpha = 0.2) +
  geom_line() 


#####STANDARDIZED#####
#With standardized data
adult_sd <- adult %>% 
  dplyr::mutate(across(c(3, 6:8, 10:14), ~ scale(.)[,1]))

#'Building models
#'Mean brightness from the blue crown
bba_mod_sd <- lme4::lmer(BB ~ relative_par_load * hatch_size + relative_par_load * sex + min_age + period + (1|year) + (1|broodID) + (1|indID), data = adult_sd)
bba_tab_sd <- broom.mixed::tidy(bba_mod_sd) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
bba_confint_sd <- as.data.frame(stats::confint(bba_mod_sd)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
bba_tab_sd <- bba_tab_sd %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ bba_confint_sd[5, 1], 
                                           term == "relative_par_load" ~ bba_confint_sd[6, 1],
                                           term == "hatch_size" ~ bba_confint_sd[7, 1],
                                           term == "sexM" ~ bba_confint_sd[8, 1],
                                           term == "min_age" ~ bba_confint_sd[9, 1],
                                           term == "periodF" ~ bba_confint_sd[10, 1],
                                           term == "relative_par_load:hatch_size" ~ bba_confint_sd[11, 1],
                                           term == "relative_par_load:sexM" ~ bba_confint_sd[12, 1],
                                           group == "indID" ~ bba_confint_sd[1, 1],
                                           group == "broodID" ~ bba_confint_sd[2, 1],
                                           group == "year" ~ bba_confint_sd[3, 1],
                                           group == "Residual" ~ bba_confint_sd[4, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ bba_confint_sd[5, 2], 
                                          term == "relative_par_load" ~ bba_confint_sd[6, 2],
                                          term == "hatch_size" ~ bba_confint_sd[7, 2],
                                          term == "sexM" ~ bba_confint_sd[8, 2],
                                          term == "min_age" ~ bba_confint_sd[9, 2],
                                          term == "periodF" ~ bba_confint_sd[10, 2],
                                          term == "relative_par_load:hatch_size" ~ bba_confint_sd[11, 2],
                                          term == "relative_par_load:sexM" ~ bba_confint_sd[12, 2],
                                          group == "indID" ~ bba_confint_sd[1, 2],
                                          group == "broodID" ~ bba_confint_sd[2, 2],
                                          group == "year" ~ bba_confint_sd[3, 2],
                                          group == "Residual" ~ bba_confint_sd[4, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "indID" ~ lme4::ngrps(bba_mod_sd)[1],
                                             group == "broodID" ~ lme4::ngrps(bba_mod_sd)[2],
                                             group == "year" ~ lme4::ngrps(bba_mod_sd)[3],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(bba_mod_sd),
                                          TRUE ~ NA_integer_),
                exp_var = "BB")


#'UV chroma from the blue crown
buvca_mod_sd <- lme4::lmer(BUVC ~ relative_par_load * hatch_size + relative_par_load * sex + min_age + period + (1|year) + (1|broodID) + (1|indID), data = adult_sd)
buvca_tab_sd <- broom.mixed::tidy(buvca_mod_sd) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
buvca_confint_sd <- as.data.frame(stats::confint(buvca_mod_sd)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
buvca_tab_sd <- buvca_tab_sd %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ buvca_confint_sd[5, 1], 
                                           term == "relative_par_load" ~ buvca_confint_sd[6, 1],
                                           term == "hatch_size" ~ buvca_confint_sd[7, 1],
                                           term == "sexM" ~ buvca_confint_sd[8, 1],
                                           term == "min_age" ~ buvca_confint_sd[9, 1],
                                           term == "periodF" ~ buvca_confint_sd[10, 1],
                                           term == "relative_par_load:hatch_size" ~ buvca_confint_sd[11, 1],
                                           term == "relative_par_load:sexM" ~ buvca_confint_sd[12, 1],
                                           group == "indID" ~ buvca_confint_sd[1, 1],
                                           group == "broodID" ~ buvca_confint_sd[2, 1],
                                           group == "year" ~ buvca_confint_sd[3, 1],
                                           group == "Residual" ~ buvca_confint_sd[4, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ buvca_confint_sd[5, 2], 
                                          term == "relative_par_load" ~ buvca_confint_sd[6, 2],
                                          term == "hatch_size" ~ buvca_confint_sd[7, 2],
                                          term == "sexM" ~ buvca_confint_sd[8, 2],
                                          term == "min_age" ~ buvca_confint_sd[9, 2],
                                          term == "periodF" ~ buvca_confint_sd[10, 2],
                                          term == "relative_par_load:hatch_size" ~ buvca_confint_sd[11, 2],
                                          term == "relative_par_load:sexM" ~ buvca_confint_sd[12, 2],
                                          group == "indID" ~ buvca_confint_sd[1, 2],
                                          group == "broodID" ~ buvca_confint_sd[2, 2],
                                          group == "year" ~ buvca_confint_sd[3, 2],
                                          group == "Residual" ~ buvca_confint_sd[4, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "indID" ~ lme4::ngrps(buvca_mod_sd)[1],
                                             group == "broodID" ~ lme4::ngrps(buvca_mod_sd)[2],
                                             group == "year" ~ lme4::ngrps(buvca_mod_sd)[3],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(buvca_mod_sd),
                                          TRUE ~ NA_integer_),
                exp_var = "BUVC")


#'Mean brightness from the yellow breast patch
yba_mod_sd <- lme4::lmer(YB ~ relative_par_load * hatch_size + relative_par_load * sex + min_age + period + (1|year) + (1|broodID) + (1|indID), data = adult_sd)
yba_tab_sd <- broom.mixed::tidy(yba_mod_sd) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
yba_confint_sd <- as.data.frame(stats::confint(yba_mod_sd)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
yba_tab_sd <- yba_tab_sd %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ yba_confint_sd[5, 1], 
                                           term == "relative_par_load" ~ yba_confint_sd[6, 1],
                                           term == "hatch_size" ~ yba_confint_sd[7, 1],
                                           term == "sexM" ~ yba_confint_sd[8, 1],
                                           term == "min_age" ~ yba_confint_sd[9, 1],
                                           term == "periodF" ~ yba_confint_sd[10, 1],
                                           term == "relative_par_load:hatch_size" ~ yba_confint_sd[11, 1],
                                           term == "relative_par_load:sexM" ~ yba_confint_sd[12, 1],
                                           group == "indID" ~ yba_confint_sd[1, 1],
                                           group == "broodID" ~ yba_confint_sd[2, 1],
                                           group == "year" ~ yba_confint_sd[3, 1],
                                           group == "Residual" ~ yba_confint_sd[4, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ yba_confint_sd[5, 2], 
                                          term == "relative_par_load" ~ yba_confint_sd[6, 2],
                                          term == "hatch_size" ~ yba_confint_sd[7, 2],
                                          term == "sexM" ~ yba_confint_sd[8, 2],
                                          term == "min_age" ~ yba_confint_sd[9, 2],
                                          term == "periodF" ~ yba_confint_sd[10, 2],
                                          term == "relative_par_load:hatch_size" ~ yba_confint_sd[11, 2],
                                          term == "relative_par_load:sexM" ~ yba_confint_sd[12, 2],
                                          group == "indID" ~ yba_confint_sd[1, 2],
                                          group == "broodID" ~ yba_confint_sd[2, 2],
                                          group == "year" ~ yba_confint_sd[3, 2],
                                          group == "Residual" ~ yba_confint_sd[4, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "indID" ~ lme4::ngrps(yba_mod_sd)[1],
                                             group == "broodID" ~ lme4::ngrps(yba_mod_sd)[2],
                                             group == "year" ~ lme4::ngrps(yba_mod_sd)[3],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(yba_mod_sd),
                                          TRUE ~ NA_integer_),
                exp_var = "YB")


#'UV chroma from the yellow breast patch
yuvca_mod_sd <- lme4::lmer(YUVC ~ relative_par_load * hatch_size + relative_par_load * sex + min_age + period + (1|year) + (1|broodID) + (1|indID), data = adult_sd)
yuvca_tab_sd <- broom.mixed::tidy(yuvca_mod_sd) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
yuvca_confint_sd <- as.data.frame(stats::confint(yuvca_mod_sd)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
yuvca_tab_sd <- yuvca_tab_sd %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ yuvca_confint_sd[5, 1], 
                                           term == "relative_par_load" ~ yuvca_confint_sd[6, 1],
                                           term == "hatch_size" ~ yuvca_confint_sd[7, 1],
                                           term == "sexM" ~ yuvca_confint_sd[8, 1],
                                           term == "min_age" ~ yuvca_confint_sd[9, 1],
                                           term == "periodF" ~ yuvca_confint_sd[10, 1],
                                           term == "relative_par_load:hatch_size" ~ yuvca_confint_sd[11, 1],
                                           term == "relative_par_load:sexM" ~ yuvca_confint_sd[12, 1],
                                           group == "indID" ~ yuvca_confint_sd[1, 1],
                                           group == "broodID" ~ yuvca_confint_sd[2, 1],
                                           group == "year" ~ yuvca_confint_sd[3, 1],
                                           group == "Residual" ~ yuvca_confint_sd[4, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ yuvca_confint_sd[5, 2], 
                                          term == "relative_par_load" ~ yuvca_confint_sd[6, 2],
                                          term == "hatch_size" ~ yuvca_confint_sd[7, 2],
                                          term == "sexM" ~ yuvca_confint_sd[8, 2],
                                          term == "min_age" ~ yuvca_confint_sd[9, 2],
                                          term == "periodF" ~ yuvca_confint_sd[10, 2],
                                          term == "relative_par_load:hatch_size" ~ yuvca_confint_sd[11, 2],
                                          term == "relative_par_load:sexM" ~ yuvca_confint_sd[12, 2],
                                          group == "indID" ~ yuvca_confint_sd[1, 2],
                                          group == "broodID" ~ yuvca_confint_sd[2, 2],
                                          group == "year" ~ yuvca_confint_sd[3, 2],
                                          group == "Residual" ~ yuvca_confint_sd[4, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "indID" ~ lme4::ngrps(yuvca_mod_sd)[1],
                                             group == "broodID" ~ lme4::ngrps(yuvca_mod_sd)[2],
                                             group == "year" ~ lme4::ngrps(yuvca_mod_sd)[3],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(yuvca_mod_sd),
                                          TRUE ~ NA_integer_),
                exp_var = "YUVC")


#'Yellow chroma from the yellow breast patch
yca_mod_sd <- lme4::lmer(YC ~ relative_par_load * hatch_size + relative_par_load * sex + min_age + period + (1|year) + (1|broodID) + (1|indID), data = adult_sd)
yca_tab_sd <- broom.mixed::tidy(yca_mod_sd) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
yca_confint_sd <- as.data.frame(stats::confint(yca_mod_sd)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
yca_tab_sd <- yca_tab_sd %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ yca_confint_sd[5, 1], 
                                           term == "relative_par_load" ~ yca_confint_sd[6, 1],
                                           term == "hatch_size" ~ yca_confint_sd[7, 1],
                                           term == "sexM" ~ yca_confint_sd[8, 1],
                                           term == "min_age" ~ yca_confint_sd[9, 1],
                                           term == "periodF" ~ yca_confint_sd[10, 1],
                                           term == "relative_par_load:hatch_size" ~ yca_confint_sd[11, 1],
                                           term == "relative_par_load:sexM" ~ yca_confint_sd[12, 1],
                                           group == "indID" ~ yca_confint_sd[1, 1],
                                           group == "broodID" ~ yca_confint_sd[2, 1],
                                           group == "year" ~ yca_confint_sd[3, 1],
                                           group == "Residual" ~ yca_confint_sd[4, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ yca_confint_sd[5, 2], 
                                          term == "relative_par_load" ~ yca_confint_sd[6, 2],
                                          term == "hatch_size" ~ yca_confint_sd[7, 2],
                                          term == "sexM" ~ yca_confint_sd[8, 2],
                                          term == "min_age" ~ yca_confint_sd[9, 2],
                                          term == "periodF" ~ yca_confint_sd[10, 2],
                                          term == "relative_par_load:hatch_size" ~ yca_confint_sd[11, 2],
                                          term == "relative_par_load:sexM" ~ yca_confint_sd[12, 2],
                                          group == "indID" ~ yca_confint_sd[1, 2],
                                          group == "broodID" ~ yca_confint_sd[2, 2],
                                          group == "year" ~ yca_confint_sd[3, 2],
                                          group == "Residual" ~ yca_confint_sd[4, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "indID" ~ lme4::ngrps(yca_mod_sd)[1],
                                             group == "broodID" ~ lme4::ngrps(yca_mod_sd)[2],
                                             group == "year" ~ lme4::ngrps(yca_mod_sd)[3],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(yca_mod_sd),
                                          TRUE ~ NA_integer_),
                exp_var = "YC")



#'Group all tables together for forestplot purposes
ad_tab_sd <- dplyr::bind_rows(bba_tab_sd, buvca_tab_sd, yba_tab_sd, yuvca_tab_sd, yca_tab_sd) %>% 
  dplyr::filter(effect != "ran_pars") %>% # Remove random effects values
  dplyr::mutate_at(vars(estimate, low95ci, up95ci), ~ round(., digits = 4)) %>% # Round numbers to keep only 4 digits
  dplyr::select(exp_var, term, estimate, low95ci, up95ci) # Reduce table width to the needed columns


#'Order and level factors to organize the forest plot
ad_tab_sd$exp_var <- factor(ad_tab_sd$exp_var, c("BB", "BUVC", "YB", "YUVC", "YC"))
ad_tab_sd$term <- factor(ad_tab_sd$term, c("relative_par_load:hatch_size", "hatch_size", "relative_par_load:sexM",
                                     "relative_par_load", "min_age", "periodF", "sexM", "(Intercept)"))

color_scheme <- c(rep("royalblue", 2), rep("darkorange", 3))

#'Create a forest plot to display the outputs of the tests
#' extrafont::font_import() 
#' extrafont::loadfonts(device = "win") (code to run to extend the number of available fonts - take time so I added #' to avoid running it everytime)
ad_fp_sd <- ggplot(ad_tab_sd, aes(x = term, y = estimate, ymin = low95ci, ymax = up95ci, color = exp_var)) +
  geom_pointrange(size = 1, linewidth = 1, shape = "¤", show.legend = FALSE) +
  scale_color_manual(values = color_scheme) +
  coord_flip() +
  labs(title = "" , x = "", y = "") +
  scale_x_discrete(label = c("Interaction between\nrelative parasite load\nand number of hatchlings", "Number of hatchlings", 
                             "Relative parasite\nload for male", "Relative parasite load", "Age", "Sampled when\nfeeding nestlings",
                             "Male", "Intercept:\nFemale sampled when\nbuilding nest"))  +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.4) +
  theme(plot.margin = margin(t = 20, b = 0.25, l = 0.25, r = 20),
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
             ncol = 5, 
             scales = "free_x",
             labeller = labeller(exp_var = c("BB" = "Mean brightness", "BUVC" = "UV-chroma",
                                             "YB" = "Mean brightness", "YUVC" = "UV-chroma", "YC" = "Yellow chroma"))) 
ad_fp_sd

# Additional text to form "Figure 3"
plot_ad_sd <- cowplot::ggdraw() +
  cowplot::draw_plot(ad_fp_sd, x = 0, y = 0, hjust = 0, width = 1, height = 1) +
  cowplot::draw_text("Blue crown", x = 0.29, y = 0.96, size = 14, fontface = "bold", family = "Noto Sans", color = "royalblue") +
  cowplot::draw_line(x = c(0.2, 0.38), y = c(0.93, 0.93), linewidth = 1, color = "black") +
  cowplot::draw_text("Yellow breast patch", x = 0.75, y = 0.96, size = 14, fontface = "bold", family = "Noto Sans", color = "darkorange") +
  cowplot::draw_line(x = c(0.55, 0.9), y = c(0.93, 0.93), linewidth = 1, color = "black")
cowplot::save_plot("figures/fig3_sd.jpg", plot_ad_sd, ncol = 1, nrow = 1, base_height = 6, base_width = 18)



# Plot displaying results from model on mean brightness from the blue crown
bb_hatch_predict_sd = ggeffects::ggpredict(bba_mod_sd, terms = c("relative_par_load", "hatch_size")) 


moderator_values_asd <- sort(c(as.numeric(as.character(unique(bb_hatch_predict_sd$group))),
                           range(c(attr(bb_hatch_predict_sd, "rawdata")$group, 
                                   as.numeric(as.character(unique(bb_hatch_predict_sd$group)))))))

bb_hatch_lm_sd <- ggplot(as.data.frame(bb_hatch_predict_sd), 
                      aes(x = x, y = predicted, 
                          group = group, 
                          color = as.numeric(as.character(group)), 
                          fill = as.numeric(as.character(group)))) +
  geom_point(data = attr(bb_hatch_predict_sd, "rawdata"), 
             aes(x = jitter(x),
                 y = jitter(response)),
             alpha = 0.6,
             shape = 16,
             size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = FALSE, alpha = 0.2) +
  geom_line() +
  labs(color = "Number of\nhatchlings", fill = "Number of\nhatchlings", title = "" , x = "Relative parasite load", y = "Mean brightness from the blue crown") +
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
        legend.position = c(0.65, 0.6),
        legend.box = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.justification = c(0.3, 0.15),
        legend.title.align = 1,
        legend.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        strip.background = element_blank())

ggsave("figures/fig3_lmbb_sd.jpg", bb_hatch_lm_sd, width = 8, height = 6)



# Plot displaying results from model on mean brightness from the blue crown
buvc_sex_predict_sd = ggeffects::ggpredict(buvca_mod_sd, terms = c("relative_par_load", "sex")) 

cols <- c("F" = "grey59", "M" = "black")

buvc_sex_lm_sd <- ggplot(as.data.frame(buvc_sex_predict_sd), 
                         aes(x = x, y = predicted, 
                             group = group, 
                             color = group, 
                             fill = group)) +
  geom_point(data = attr(buvc_sex_predict_sd, "rawdata"), 
             aes(x = jitter(x),
                 y = jitter(response)),
             alpha = 0.6,
             shape = 16,
             size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = FALSE, alpha = 0.2) +
  geom_line() +
  scale_color_manual(values = cols,
                     aesthetics = c("colour", "fill"),
                     labels = c("Female", "Male")) +
  labs(color = "Sex", fill = "Sex", title = "" , x = "Relative parasite load", y = "UV-chroma from the blue crown") +
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
        legend.position = c(0.80, 0.8),
        legend.box = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.justification = c(0.3, 0.15),
        legend.title.align = 0.5,
        legend.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        strip.background = element_blank())

ggsave("figures/fig3_buvcsex_sd.jpg", buvc_sex_lm_sd, width = 8, height = 6)


#Display table with output (unscaled and scaled)
##unscaled
ad_tab <- dplyr::bind_rows(bba_tab, buvca_tab, yba_tab, yuvca_tab, yca_tab) %>% 
  dplyr::mutate_at(vars(estimate, low95ci, up95ci), ~ round(., digits = 4)) %>% # Round numbers to keep only 4 digits
  dplyr::select(exp_var, term, estimate, low95ci, up95ci, nb_obs, nb_groups) # Reduce table width to the needed columns
write.csv(ad_tab, "output_tables/output_prediction2_unsc.csv", row.names = FALSE)

##standardized
ad_tab_sd <- dplyr::bind_rows(bba_tab_sd, buvca_tab_sd, yba_tab_sd, yuvca_tab_sd, yca_tab_sd) %>% 
  dplyr::mutate_at(vars(estimate, low95ci, up95ci), ~ round(., digits = 4)) %>% # Round numbers to keep only 4 digits
  dplyr::select(exp_var, term, estimate, low95ci, up95ci, nb_obs, nb_groups) # Reduce table width to the needed columns
write.csv(ad_tab_sd, "output_tables/output_prediction2_sc.csv", row.names = FALSE)