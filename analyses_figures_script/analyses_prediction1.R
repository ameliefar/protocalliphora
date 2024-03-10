#'Analyses to test correlations between plumage color for first-year breeders and their parasite load as nestlings
#'
#'Load packages
install.packages("tidyverse", "lme4", "parameters", "extrafont", "broom.mixed", "cowplot", "ggeffects")
library(lme4)
library(parameters)
library(extrafont)

#'Load dataset
recruit <- read.csv("data/recruit_color.csv") %>% 
  dplyr::mutate(year = as.character(year),
                indID = as.character(indID))


#'Building models
#'Mean brightness from the blue crown
bbr_mod <- lme4::lmer(BB ~ relative_par_load * hatch_size + relative_par_load * sex + period + (1|year) + (1|broodID), data = recruit)
bbr_tab <- broom.mixed::tidy(bbr_mod) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
bbr_confint <- as.data.frame(stats::confint(bbr_mod)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
bbr_tab <- bbr_tab %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ bbr_confint[4, 1], 
                                           term == "relative_par_load" ~ bbr_confint[5, 1],
                                           term == "hatch_size" ~ bbr_confint[6, 1],
                                           term == "sexM" ~ bbr_confint[7, 1],
                                           term == "periodF" ~ bbr_confint[8, 1],
                                           term == "relative_par_load:hatch_size" ~ bbr_confint[9, 1],
                                           term == "relative_par_load:sexM" ~ bbr_confint[10, 1],
                                           group == "broodID" ~ bbr_confint[1, 1],
                                           group == "year" ~ bbr_confint[2, 1],
                                           group == "Residual" ~ bbr_confint[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ bbr_confint[4, 2], 
                                          term == "relative_par_load" ~ bbr_confint[5, 2],
                                          term == "hatch_size" ~ bbr_confint[6, 2],
                                          term == "sexM" ~ bbr_confint[7, 2],
                                          term == "periodF" ~ bbr_confint[8, 2],
                                          term == "relative_par_load:hatch_size" ~ bbr_confint[9, 2],
                                          term == "relative_par_load:sexM" ~ bbr_confint[10, 2],
                                          group == "broodID" ~ bbr_confint[1, 2],
                                          group == "year" ~ bbr_confint[2, 2],
                                          group == "Residual" ~ bbr_confint[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "broodID" ~ lme4::ngrps(bbr_mod)[1],
                                             group == "year" ~ lme4::ngrps(bbr_mod)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(bbr_mod),
                                          TRUE ~ NA_integer_),
                exp_var = "BB")

#'UV-chroma from the blue crown
buvcr_mod <- lme4::lmer(BUVC ~ relative_par_load * hatch_size + relative_par_load * sex + period + (1|year) + (1|broodID), data = recruit)
buvcr_tab <- broom.mixed::tidy(buvcr_mod) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
buvcr_confint <- as.data.frame(stats::confint(buvcr_mod)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
buvcr_tab <- buvcr_tab %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ buvcr_confint[4, 1], 
                                           term == "relative_par_load" ~ buvcr_confint[5, 1],
                                           term == "hatch_size" ~ buvcr_confint[6, 1],
                                           term == "sexM" ~ buvcr_confint[7, 1],
                                           term == "periodF" ~ buvcr_confint[8, 1],
                                           term == "relative_par_load:hatch_size" ~ buvcr_confint[9, 1],
                                           term == "relative_par_load:sexM" ~ buvcr_confint[10, 1],
                                           group == "broodID" ~ buvcr_confint[1, 1],
                                           group == "year" ~ buvcr_confint[2, 1],
                                           group == "Residual" ~ buvcr_confint[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ buvcr_confint[4, 2], 
                                          term == "relative_par_load" ~ buvcr_confint[5, 2],
                                          term == "hatch_size" ~ buvcr_confint[6, 2],
                                          term == "sexM" ~ buvcr_confint[7, 2],
                                          term == "periodF" ~ buvcr_confint[8, 2],
                                          term == "relative_par_load:hatch_size" ~ buvcr_confint[9, 2],
                                          term == "relative_par_load:sexM" ~ buvcr_confint[10, 2],
                                          group == "broodID" ~ buvcr_confint[1, 2],
                                          group == "year" ~ buvcr_confint[2, 2],
                                          group == "Residual" ~ buvcr_confint[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "broodID" ~ lme4::ngrps(buvcr_mod)[1],
                                             group == "year" ~ lme4::ngrps(buvcr_mod)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(buvcr_mod),
                                          TRUE ~ NA_integer_),
                exp_var = "BUVC")


#'Mean brightness from the yellow breast patch
ybr_mod <- lme4::lmer(YB ~ relative_par_load * hatch_size + relative_par_load * sex + period + (1|year) + (1|broodID), data = recruit)
ybr_tab <- broom.mixed::tidy(ybr_mod) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
ybr_confint <- as.data.frame(stats::confint(ybr_mod)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
ybr_tab <- ybr_tab %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ ybr_confint[4, 1], 
                                           term == "relative_par_load" ~ ybr_confint[5, 1],
                                           term == "hatch_size" ~ ybr_confint[6, 1],
                                           term == "sexM" ~ ybr_confint[7, 1],
                                           term == "periodF" ~ ybr_confint[8, 1],
                                           term == "relative_par_load:hatch_size" ~ ybr_confint[9, 1],
                                           term == "relative_par_load:sexM" ~ ybr_confint[10, 1],
                                           group == "broodID" ~ ybr_confint[1, 1],
                                           group == "year" ~ ybr_confint[2, 1],
                                           group == "Residual" ~ ybr_confint[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ ybr_confint[4, 2], 
                                          term == "relative_par_load" ~ ybr_confint[5, 2],
                                          term == "hatch_size" ~ ybr_confint[6, 2],
                                          term == "sexM" ~ ybr_confint[7, 2],
                                          term == "periodF" ~ ybr_confint[8, 2],
                                          term == "relative_par_load:hatch_size" ~ ybr_confint[9, 2],
                                          term == "relative_par_load:sexM" ~ ybr_confint[10, 2],
                                          group == "broodID" ~ ybr_confint[1, 2],
                                          group == "year" ~ ybr_confint[2, 2],
                                          group == "Residual" ~ ybr_confint[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "broodID" ~ lme4::ngrps(ybr_mod)[1],
                                             group == "year" ~ lme4::ngrps(ybr_mod)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(ybr_mod),
                                          TRUE ~ NA_integer_),
                exp_var = "YB")

#'UV-chroma from the yellow breast patch
yuvcr_mod <- lme4::lmer(YUVC ~ relative_par_load * hatch_size + relative_par_load * sex + period + (1|year) + (1|broodID), data = recruit)
yuvcr_tab <- broom.mixed::tidy(yuvcr_mod) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
yuvcr_confint <- as.data.frame(stats::confint(yuvcr_mod)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
yuvcr_tab <- yuvcr_tab %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ yuvcr_confint[4, 1], 
                                           term == "relative_par_load" ~ yuvcr_confint[5, 1],
                                           term == "hatch_size" ~ yuvcr_confint[6, 1],
                                           term == "sexM" ~ yuvcr_confint[7, 1],
                                           term == "periodF" ~ yuvcr_confint[8, 1],
                                           term == "relative_par_load:hatch_size" ~ yuvcr_confint[9, 1],
                                           term == "relative_par_load:sexM" ~ yuvcr_confint[10, 1],
                                           group == "broodID" ~ yuvcr_confint[1, 1],
                                           group == "year" ~ yuvcr_confint[2, 1],
                                           group == "Residual" ~ yuvcr_confint[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ yuvcr_confint[4, 2], 
                                          term == "relative_par_load" ~ yuvcr_confint[5, 2],
                                          term == "hatch_size" ~ yuvcr_confint[6, 2],
                                          term == "sexM" ~ yuvcr_confint[7, 2],
                                          term == "periodF" ~ yuvcr_confint[8, 2],
                                          term == "relative_par_load:hatch_size" ~ yuvcr_confint[9, 2],
                                          term == "relative_par_load:sexM" ~ yuvcr_confint[10, 2],
                                          group == "broodID" ~ yuvcr_confint[1, 2],
                                          group == "year" ~ yuvcr_confint[2, 2],
                                          group == "Residual" ~ yuvcr_confint[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "broodID" ~ lme4::ngrps(yuvcr_mod)[1],
                                             group == "year" ~ lme4::ngrps(yuvcr_mod)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(yuvcr_mod),
                                          TRUE ~ NA_integer_),
                exp_var = "YUVC")

#'Yellow chroma from the yellow breast patch
ycr_mod <- lme4::lmer(YC ~ relative_par_load * hatch_size + relative_par_load * sex + period + (1|year) + (1|broodID), data = recruit)
ycr_tab <- broom.mixed::tidy(ycr_mod) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
ycr_confint <- as.data.frame(stats::confint(ycr_mod)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
ycr_tab <- ycr_tab %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ ycr_confint[4, 1], 
                                           term == "relative_par_load" ~ ycr_confint[5, 1],
                                           term == "hatch_size" ~ ycr_confint[6, 1],
                                           term == "sexM" ~ ycr_confint[7, 1],
                                           term == "periodF" ~ ycr_confint[8, 1],
                                           term == "relative_par_load:hatch_size" ~ ycr_confint[9, 1],
                                           term == "relative_par_load:sexM" ~ ycr_confint[10, 1],
                                           group == "broodID" ~ ycr_confint[1, 1],
                                           group == "year" ~ ycr_confint[2, 1],
                                           group == "Residual" ~ ycr_confint[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ ycr_confint[4, 2], 
                                          term == "relative_par_load" ~ ycr_confint[5, 2],
                                          term == "hatch_size" ~ ycr_confint[6, 2],
                                          term == "sexM" ~ ycr_confint[7, 2],
                                          term == "periodF" ~ ycr_confint[8, 2],
                                          term == "relative_par_load:hatch_size" ~ ycr_confint[9, 2],
                                          term == "relative_par_load:sexM" ~ ycr_confint[10, 2],
                                          group == "broodID" ~ ycr_confint[1, 2],
                                          group == "year" ~ ycr_confint[2, 2],
                                          group == "Residual" ~ ycr_confint[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "broodID" ~ lme4::ngrps(ycr_mod)[1],
                                             group == "year" ~ lme4::ngrps(ycr_mod)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(ycr_mod),
                                          TRUE ~ NA_integer_),
                exp_var = "YC")


#'Group all tables together for forestplot purposes
rec_tab <- dplyr::bind_rows(bbr_tab, buvcr_tab, ybr_tab, yuvcr_tab, ycr_tab) %>% 
  dplyr::filter(effect != "ran_pars") %>% # Remove random effects values
  dplyr::mutate_at(vars(estimate, low95ci, up95ci), ~ round(., digits = 4)) %>% # Round numbers to keep only 4 digits
  dplyr::select(exp_var, term, estimate, low95ci, up95ci) # Reduce table width to the needed columns


#'Order and level factors to organize the forest plot
rec_tab$exp_var <- factor(rec_tab$exp_var, c("BB", "BUVC", "YB", "YUVC", "YC"))
rec_tab$term <- factor(rec_tab$term, c("relative_par_load:hatch_size", "hatch_size", "relative_par_load:sexM",
                                       "relative_par_load", "periodF", "sexM", "(Intercept)"))

color_scheme <- c(rep("royalblue", 2), rep("darkorange", 3))


#Figures
#'Create a forest plot to display the outputs of the tests
#' extrafont::font_import() 
#' extrafont::loadfonts(device = "win") (code to run to extend the number of available fonts - take time so I added #' to avoid running it everytime)
rec_fp <- ggplot(rec_tab, aes(x = term, y = estimate, ymin = low95ci, ymax = up95ci, color = exp_var)) +
  geom_pointrange(size = 1, linewidth = 1, shape = "¤", show.legend = FALSE) +
  scale_color_manual(values = color_scheme) +
  coord_flip() +
  labs(title = "" , x = "", y = "") +
  scale_x_discrete(label = c("Interaction between\nrelative parasite load\nand number of hatchlings", "Number of hatchlings", 
                             "Relative parasite\nload for male", "Relative parasite load", "Sampled when\nfeeding nestlings",
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
rec_fp

# Additional text to form "Figure 2"
plot_rec <- cowplot::ggdraw() +
  cowplot::draw_plot(rec_fp, x = 0, y = 0, hjust = 0, width = 1, height = 1) +
  cowplot::draw_text("Blue crown", x = 0.29, y = 0.96, size = 14, fontface = "bold", family = "Noto Sans", color = "royalblue") +
  cowplot::draw_line(x = c(0.2, 0.38), y = c(0.93, 0.93), linewidth = 1, color = "black") +
  cowplot::draw_text("Yellow breast patch", x = 0.75, y = 0.96, size = 14, fontface = "bold", family = "Noto Sans", color = "darkorange") +
  cowplot::draw_line(x = c(0.55, 0.9), y = c(0.93, 0.93), linewidth = 1, color = "black")
cowplot::save_plot("figures/not_standardized/fig2.jpg", plot_rec, ncol = 1, nrow = 1, base_height = 6, base_width = 18)




## ~~~~~~ WITH STANDARDIZED DATA
#Data manipulation to add standardized data (centered (substracting the column mean) and scaled (dividing centered value by sd))
recruit_sd <- recruit %>% 
  dplyr::mutate(across(c(5:7, 9:13), ~ scale(.)[,1])) %>% 
  dplyr::left_join(dplyr::select(recruit, 
                                 c("indID", "year", "hatch_size", "par_load", "relative_par_load", "BB", "BUVC", "YB", "YUVC", "YC")), 
                   by = c("indID", "year")) %>% 
  dplyr::select(indID, broodID, year, sex, period,
                hatch_size = "hatch_size.y",
                sd_hatch_size = "hatch_size.x",
                par_load = "par_load.y",
                sd_par_load = "par_load.x",
                relative_par_load = "relative_par_load.y",
                sd_relative_par_load = "relative_par_load.x",
                BB = "BB.y",
                sd_BB = "BB.x",
                BUVC = "BUVC.y",
                sd_BUVC = "BUVC.x",
                YB = "YB.y",
                sd_YB = "YB.x",
                YUVC = "YUVC.y",
                sd_YUVC = "YUVC.x",
                YC = "YC.y",
                sd_YC = "YC.x")



#'Building models
#'Mean brightness from the blue crown
bbr_mod_sd <- lme4::lmer(sd_BB ~ sd_relative_par_load * sd_hatch_size + sd_relative_par_load * sex + period + (1|year) + (1|broodID), data = recruit_sd)
bbr_tab_sd <- broom.mixed::tidy(bbr_mod_sd) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
bbr_confint_sd <- as.data.frame(stats::confint(bbr_mod_sd)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
bbr_tab_sd <- bbr_tab_sd %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ bbr_confint_sd[4, 1], 
                                           term == "sd_relative_par_load" ~ bbr_confint_sd[5, 1],
                                           term == "sd_hatch_size" ~ bbr_confint_sd[6, 1],
                                           term == "sexM" ~ bbr_confint_sd[7, 1],
                                           term == "periodF" ~ bbr_confint_sd[8, 1],
                                           term == "sd_relative_par_load:sd_hatch_size" ~ bbr_confint_sd[9, 1],
                                           term == "sd_relative_par_load:sexM" ~ bbr_confint_sd[10, 1],
                                           group == "broodID" ~ bbr_confint_sd[1, 1],
                                           group == "year" ~ bbr_confint_sd[2, 1],
                                           group == "Residual" ~ bbr_confint_sd[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ bbr_confint_sd[4, 2], 
                                          term == "sd_relative_par_load" ~ bbr_confint_sd[5, 2],
                                          term == "sd_hatch_size" ~ bbr_confint_sd[6, 2],
                                          term == "sexM" ~ bbr_confint_sd[7, 2],
                                          term == "periodF" ~ bbr_confint_sd[8, 2],
                                          term == "sd_relative_par_load:sd_hatch_size" ~ bbr_confint_sd[9, 2],
                                          term == "sd_relative_par_load:sexM" ~ bbr_confint_sd[10, 2],
                                          group == "broodID" ~ bbr_confint_sd[1, 2],
                                          group == "year" ~ bbr_confint_sd[2, 2],
                                          group == "Residual" ~ bbr_confint_sd[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "broodID" ~ lme4::ngrps(bbr_mod_sd)[1],
                                             group == "year" ~ lme4::ngrps(bbr_mod_sd)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(bbr_mod_sd),
                                          TRUE ~ NA_integer_),
                exp_var = "BB_sd")

#'UV-chroma from the blue crown
buvcr_mod_sd <- lme4::lmer(sd_BUVC ~ sd_relative_par_load * sd_hatch_size + sd_relative_par_load * sex + period + (1|year) + (1|broodID), data = recruit_sd)
buvcr_tab_sd <- broom.mixed::tidy(buvcr_mod_sd) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
buvcr_confint_sd <- as.data.frame(stats::confint(buvcr_mod_sd)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
buvcr_tab_sd <- buvcr_tab_sd %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ buvcr_confint_sd[4, 1], 
                                           term == "sd_relative_par_load" ~ buvcr_confint_sd[5, 1],
                                           term == "sd_hatch_size" ~ buvcr_confint_sd[6, 1],
                                           term == "sexM" ~ buvcr_confint_sd[7, 1],
                                           term == "periodF" ~ buvcr_confint_sd[8, 1],
                                           term == "sd_relative_par_load:sd_hatch_size" ~ buvcr_confint_sd[9, 1],
                                           term == "sd_relative_par_load:sexM" ~ buvcr_confint_sd[10, 1],
                                           group == "broodID" ~ buvcr_confint_sd[1, 1],
                                           group == "year" ~ buvcr_confint_sd[2, 1],
                                           group == "Residual" ~ buvcr_confint_sd[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ buvcr_confint_sd[4, 2], 
                                          term == "sd_relative_par_load" ~ buvcr_confint_sd[5, 2],
                                          term == "sd_hatch_size" ~ buvcr_confint_sd[6, 2],
                                          term == "sexM" ~ buvcr_confint_sd[7, 2],
                                          term == "periodF" ~ buvcr_confint_sd[8, 2],
                                          term == "sd_relative_par_load:sd_hatch_size" ~ buvcr_confint_sd[9, 2],
                                          term == "sd_relative_par_load:sexM" ~ buvcr_confint_sd[10, 2],
                                          group == "broodID" ~ buvcr_confint_sd[1, 2],
                                          group == "year" ~ buvcr_confint_sd[2, 2],
                                          group == "Residual" ~ buvcr_confint_sd[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "broodID" ~ lme4::ngrps(buvcr_mod_sd)[1],
                                             group == "year" ~ lme4::ngrps(buvcr_mod_sd)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(buvcr_mod_sd),
                                          TRUE ~ NA_integer_),
                exp_var = "BUVC_sd")


#'Mean brightness from the yellow breast patch
ybr_mod_sd <- lme4::lmer(sd_YB ~ sd_relative_par_load * sd_hatch_size + sd_relative_par_load * sex + period + (1|year) + (1|broodID), data = recruit_sd)
ybr_tab_sd <- broom.mixed::tidy(ybr_mod_sd) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
ybr_confint_sd <- as.data.frame(stats::confint(ybr_mod_sd)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
ybr_tab_sd <- ybr_tab_sd %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ ybr_confint_sd[4, 1], 
                                           term == "sd_relative_par_load" ~ ybr_confint_sd[5, 1],
                                           term == "sd_hatch_size" ~ ybr_confint_sd[6, 1],
                                           term == "sexM" ~ ybr_confint_sd[7, 1],
                                           term == "periodF" ~ ybr_confint_sd[8, 1],
                                           term == "sd_relative_par_load:sd_hatch_size" ~ ybr_confint_sd[9, 1],
                                           term == "sd_relative_par_load:sexM" ~ ybr_confint_sd[10, 1],
                                           group == "broodID" ~ ybr_confint_sd[1, 1],
                                           group == "year" ~ ybr_confint_sd[2, 1],
                                           group == "Residual" ~ ybr_confint_sd[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ ybr_confint_sd[4, 2], 
                                          term == "sd_relative_par_load" ~ ybr_confint_sd[5, 2],
                                          term == "sd_hatch_size" ~ ybr_confint_sd[6, 2],
                                          term == "sexM" ~ ybr_confint_sd[7, 2],
                                          term == "periodF" ~ ybr_confint_sd[8, 2],
                                          term == "sd_relative_par_load:sd_hatch_size" ~ ybr_confint_sd[9, 2],
                                          term == "sd_relative_par_load:sexM" ~ ybr_confint_sd[10, 2],
                                          group == "broodID" ~ ybr_confint_sd[1, 2],
                                          group == "year" ~ ybr_confint_sd[2, 2],
                                          group == "Residual" ~ ybr_confint_sd[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "broodID" ~ lme4::ngrps(ybr_mod_sd)[1],
                                             group == "year" ~ lme4::ngrps(ybr_mod_sd)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(ybr_mod_sd),
                                          TRUE ~ NA_integer_),
                exp_var = "YB_sd")

#'UV-chroma from the yellow breast patch
yuvcr_mod_sd <- lme4::lmer(sd_YUVC ~ sd_relative_par_load * sd_hatch_size + sd_relative_par_load * sex + period + (1|year) + (1|broodID), data = recruit_sd)
yuvcr_tab_sd <- broom.mixed::tidy(yuvcr_mod_sd) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
yuvcr_confint_sd <- as.data.frame(stats::confint(yuvcr_mod_sd)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
yuvcr_tab_sd <- yuvcr_tab_sd %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ yuvcr_confint_sd[4, 1], 
                                           term == "sd_relative_par_load" ~ yuvcr_confint_sd[5, 1],
                                           term == "sd_hatch_size" ~ yuvcr_confint_sd[6, 1],
                                           term == "sexM" ~ yuvcr_confint_sd[7, 1],
                                           term == "periodF" ~ yuvcr_confint_sd[8, 1],
                                           term == "sd_relative_par_load:sd_hatch_size" ~ yuvcr_confint_sd[9, 1],
                                           term == "sd_relative_par_load:sexM" ~ yuvcr_confint_sd[10, 1],
                                           group == "broodID" ~ yuvcr_confint_sd[1, 1],
                                           group == "year" ~ yuvcr_confint_sd[2, 1],
                                           group == "Residual" ~ yuvcr_confint_sd[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ yuvcr_confint_sd[4, 2], 
                                          term == "sd_relative_par_load" ~ yuvcr_confint_sd[5, 2],
                                          term == "sd_hatch_size" ~ yuvcr_confint_sd[6, 2],
                                          term == "sexM" ~ yuvcr_confint_sd[7, 2],
                                          term == "periodF" ~ yuvcr_confint_sd[8, 2],
                                          term == "sd_relative_par_load:sd_hatch_size" ~ yuvcr_confint_sd[9, 2],
                                          term == "sd_relative_par_load:sexM" ~ yuvcr_confint_sd[10, 2],
                                          group == "broodID" ~ yuvcr_confint_sd[1, 2],
                                          group == "year" ~ yuvcr_confint_sd[2, 2],
                                          group == "Residual" ~ yuvcr_confint_sd[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "broodID" ~ lme4::ngrps(yuvcr_mod_sd)[1],
                                             group == "year" ~ lme4::ngrps(yuvcr_mod_sd)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(yuvcr_mod_sd),
                                          TRUE ~ NA_integer_),
                exp_var = "YUVC_sd")

#'Yellow chroma from the yellow breast patch
ycr_mod_sd <- lme4::lmer(sd_YC ~ sd_relative_par_load * sd_hatch_size + sd_relative_par_load * sex + period + (1|year) + (1|broodID), data = recruit_sd)
ycr_tab_sd <- broom.mixed::tidy(ycr_mod_sd) # Extract estimates, statistics for fixed effects, as well as standard-deviation for random effects
ycr_confint_sd <- as.data.frame(stats::confint(ycr_mod_sd)) %>% # Estimate 95% confidence intervals for fixed and random effects
  dplyr::rename("low95ci" = '2.5 %', "up95ci" = '97.5 %')
ycr_tab_sd <- ycr_tab_sd %>% 
  # Add confidence intervals to the data table summarizing results related to the model
  dplyr::mutate(low95ci = dplyr::case_when(term == "(Intercept)" ~ ycr_confint_sd[4, 1], 
                                           term == "sd_relative_par_load" ~ ycr_confint_sd[5, 1],
                                           term == "sd_hatch_size" ~ ycr_confint_sd[6, 1],
                                           term == "sexM" ~ ycr_confint_sd[7, 1],
                                           term == "periodF" ~ ycr_confint_sd[8, 1],
                                           term == "sd_relative_par_load:sd_hatch_size" ~ ycr_confint_sd[9, 1],
                                           term == "sd_relative_par_load:sexM" ~ ycr_confint_sd[10, 1],
                                           group == "broodID" ~ ycr_confint_sd[1, 1],
                                           group == "year" ~ ycr_confint_sd[2, 1],
                                           group == "Residual" ~ ycr_confint_sd[3, 1],
                                           TRUE ~ NA_real_),
                up95ci = dplyr::case_when(term == "(Intercept)" ~ ycr_confint_sd[4, 2], 
                                          term == "sd_relative_par_load" ~ ycr_confint_sd[5, 2],
                                          term == "sd_hatch_size" ~ ycr_confint_sd[6, 2],
                                          term == "sexM" ~ ycr_confint_sd[7, 2],
                                          term == "periodF" ~ ycr_confint_sd[8, 2],
                                          term == "sd_relative_par_load:sd_hatch_size" ~ ycr_confint_sd[9, 2],
                                          term == "sd_relative_par_load:sexM" ~ ycr_confint_sd[10, 2],
                                          group == "broodID" ~ ycr_confint_sd[1, 2],
                                          group == "year" ~ ycr_confint_sd[2, 2],
                                          group == "Residual" ~ ycr_confint_sd[3, 2],
                                          TRUE ~ NA_real_),
                nb_groups = dplyr::case_when(group == "broodID" ~ lme4::ngrps(ycr_mod_sd)[1],
                                             group == "year" ~ lme4::ngrps(ycr_mod_sd)[2],
                                             TRUE ~ NA_integer_),
                nb_obs = dplyr::case_when(group == "Residual" ~ stats::nobs(ycr_mod_sd),
                                          TRUE ~ NA_integer_),
                exp_var = "YC_sd")


#'Group all tables together for forestplot purposes
rec_tab_sd <- dplyr::bind_rows(bbr_tab_sd, buvcr_tab_sd, ybr_tab_sd, yuvcr_tab_sd, ycr_tab_sd) %>% 
  dplyr::filter(effect != "ran_pars") %>% # Remove random effects values
  dplyr::mutate_at(vars(estimate, low95ci, up95ci), ~ round(., digits = 4)) %>% # Round numbers to keep only 4 digits
  dplyr::select(exp_var, term, estimate, low95ci, up95ci) # Reduce table width to the needed columns


#'Order and level factors to organize the forest plot
rec_tab_sd$exp_var <- factor(rec_tab_sd$exp_var, c("BB_sd", "BUVC_sd", "YB_sd", "YUVC_sd", "YC_sd"))
rec_tab_sd$term <- factor(rec_tab_sd$term, c("sd_relative_par_load:sd_hatch_size", "sd_hatch_size", "sd_relative_par_load:sexM",
                                       "sd_relative_par_load", "periodF", "sexM", "(Intercept)"))

color_scheme <- c(rep("royalblue", 2), rep("darkorange", 3))

#'Create a forest plot to display the outputs of the tests
#' extrafont::font_import() 
#' extrafont::loadfonts(device = "win") (code to run to extend the number of available fonts - take time so I added #' to avoid running it everytime)
rec_fp_sd <- ggplot(rec_tab_sd, aes(x = term, y = estimate, ymin = low95ci, ymax = up95ci, color = exp_var)) +
  geom_pointrange(size = 1, linewidth = 1, shape = "¤", show.legend = FALSE) +
  scale_color_manual(values = color_scheme) +
  coord_flip() +
  labs(title = "" , x = "", y = "") +
  scale_x_discrete(label = c("Interaction between\nrelative parasite load\nand number of hatchlings", "Number of hatchlings", 
                             "Relative parasite\nload for male", "Relative parasite load", "Sampled when\nfeeding nestlings",
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
             labeller = labeller(exp_var = c("BB_sd" = "Mean brightness", "BUVC_sd" = "UV-chroma",
                                             "YB_sd" = "Mean brightness", "YUVC_sd" = "UV-chroma", "YC_sd" = "Yellow chroma"))) 
rec_fp_sd

# Additional text to form "Figure 2"
plot_rec_sd <- cowplot::ggdraw() +
  cowplot::draw_plot(rec_fp_sd, x = 0, y = 0, hjust = 0, width = 1, height = 1) +
  cowplot::draw_text("Blue crown", x = 0.29, y = 0.96, size = 14, fontface = "bold", family = "Noto Sans", color = "royalblue") +
  cowplot::draw_line(x = c(0.2, 0.38), y = c(0.93, 0.93), linewidth = 1, color = "black") +
  cowplot::draw_text("Yellow breast patch", x = 0.75, y = 0.96, size = 14, fontface = "bold", family = "Noto Sans", color = "darkorange") +
  cowplot::draw_line(x = c(0.55, 0.9), y = c(0.93, 0.93), linewidth = 1, color = "black")
cowplot::save_plot("figures/standardized/fig2_sd.jpg", plot_rec_sd, ncol = 1, nrow = 1, base_height = 6, base_width = 18)



###Extra plot
# Plot displaying results from model on yellow chroma from the yellow breast patch
yc_hatch_predict_sd = ggeffects::ggpredict(ycr_mod_sd, terms = c("sd_relative_par_load", "sd_hatch_size")) 


moderator_values_rsd <- sort(c(as.numeric(as.character(unique(yc_hatch_predict_sd$group))),
                           range(c(attr(yc_hatch_predict_sd, "rawdata")$group, 
                                   as.numeric(as.character(unique(yc_hatch_predict_sd$group)))))))

yc_hatch_lm_sd <- ggplot(as.data.frame(yc_hatch_predict_sd), 
                      aes(x = x, y = predicted, 
                          group = group, 
                          color = as.numeric(as.character(group)), 
                          fill = as.numeric(as.character(group)))) +
  geom_point(data = attr(yc_hatch_predict_sd, "rawdata"), 
             aes(x = jitter(x),
                 y = jitter(response)),
             alpha = 0.6,
             shape = 16,
             size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = FALSE, alpha = 0.2) +
  geom_line() +
  labs(color = "Number of\nhatchlings", fill = "Number of\nhatchlings", title = "" , x = "Relative parasite load", y = "Yellow chroma from the yellow breast patch") +
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
        legend.position.inside = c(0.65, 0.6),
        legend.box = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.justification = c(0.3, 0.15),
        legend.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 12, face = "bold", hjust = 0.5),
        strip.background = element_blank())

ggsave("figures/standardized/fig2_sd_yc_recruit.jpg", yc_hatch_lm_sd, width = 8, height = 6)



#Outputs from models (not standardized and standardized)
##not standardized
name <- c("model", "effect", "term", "estimate", "std_error", "t_value", "95CI_low", "95CI_up", "group", "nb_groups", "nb_obs")

rec_tab <- dplyr::bind_rows(bbr_tab, buvcr_tab, ybr_tab, yuvcr_tab, ycr_tab) %>% 
  dplyr::mutate_at(vars(estimate, std.error, statistic, low95ci, up95ci), ~ round(., digits = 4)) %>% # Round numbers to keep only 4 digits
  dplyr::select(exp_var, effect, term, estimate, std.error, statistic, low95ci, up95ci, group, nb_obs, nb_groups) # Reduce table width to the needed columns
colnames(rec_tab) <- name
write.csv(rec_tab, "output_tables/output_prediction1_unst.csv", row.names = FALSE)

##Scaled
rec_tab_sd <- dplyr::bind_rows(bbr_tab_sd, buvcr_tab_sd, ybr_tab_sd, yuvcr_tab_sd, ycr_tab_sd) %>% 
  dplyr::mutate_at(vars(estimate, std.error, statistic, low95ci, up95ci), ~ round(., digits = 4)) %>% # Round numbers to keep only 4 digits
  dplyr::select(exp_var, effect, term, estimate, std.error, statistic, low95ci, up95ci, group, nb_obs, nb_groups) # Reduce table width to the needed columns
colnames(rec_tab_sd) <- name
write.csv(rec_tab_sd, "output_tables/output_prediction1_st.csv", row.names = FALSE)
