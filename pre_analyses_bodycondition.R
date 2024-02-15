#'Pre-analyses to test correlations between parasite load and nestling body condition
#'
#'Load packages
library(lme4)
library(merTools)
library(parameters)

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

# Merge both data tables together
all_tab <- dplyr::bind_rows(dplyr::mutate(mass_tab, to_explain = "mass"),
                            dplyr::mutate(tars_tab, to_explain = "tarsus"))
#Figures
#'Create a forest plot to display the outputs of the tests
#'
plot <- ggplot(all_tab, aes(x = term, y = estimate, ymin = low95ci, ymax = up95ci)) +
  geom_pointrange(size = 0.75, shape = 21) +
  coord_flip() +
  labs(title = "" , x = "", y = "") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.5) +
  guides(color = "none", fill = "none") +
  facet_wrap(. ~ to_explain, 
             nrow = 1, 
             ncol = 2, 
             labeller = labeller(to_explain = c("mass" = "Nestling mass (g)", "tarsus" = "Nestling tarsus length (mm)")))



theme(plot.margin = margin(t = 0.25, b = 0.25, l = 0.25, r = 20),
      text = element_text(family = "Times"),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 10, face = "bold", color = "black", hjust = 0.5),
      axis.text.y = element_text(size = 16, face = "bold", color = text, hjust = 0),
      axis.title = element_text(size = 16, face = "bold", color = "black"),
      axis.ticks.y = element_blank(),
      panel.spacing.x = unit(3, "lines"),
      panel.spacing.y = unit(1.5, "lines"),
      panel.background = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 12, face = "bold", color = "black"),
      axis.line.x = element_line(colour = "black", linewidth = 1, linetype = "solid"),
      axis.line.y = element_blank()) +
  #Laying date
ld_pl = filter(es_pl, lht == "ld")


font_add("Times", regular = "times.ttf")

text <- c(rep("black", 4), "#D55E00", "white", rep(c(rep("black", 4), "#009E73", "white"), 2), rep("black", 4), "#009E73")
size = c(rep(c(rep(12, 4), 20, 2), 3), rep(12, 4), 20)


ld_pl_plot<- ggplot(ld_pl, aes(x = pop_col, y = estimate, ymin = low, ymax = up, color = pop_col, fill = pop_col)) +
  geom_pointrange(size = 0.75, shape = 21) +
  coord_flip() +
  labs(title = "" , x = "", y="") +
  scale_y_continuous(limits = c(-0.06, 0.06), breaks = c(-0.05, -0.025, 0, 0.025, 0.05), label = c("-0.05", "-0.025", "0", "0.025", "0.05")) +
  scale_x_discrete(label = c("Yellow chroma", "Mean brightness", "UV chroma", "Mean brightness", "D-Rouvière", "", "Yellow chroma", "Mean brightness", "UV chroma", "Mean brightness", "E-Pirio", "", "Yellow chroma", "Mean brightness", "UV chroma", "Mean brightness", "E-Muro", "", "Yellow chroma", "Mean brightness", "UV chroma", "Mean brightness", "D-Muro"))  +
  scale_color_manual(values = c("darkorange", "darkorange", "royalblue", "royalblue", "white", "white", "darkorange", "darkorange", "royalblue", "royalblue", "white", "white", "darkorange", "darkorange", "royalblue", "royalblue", "white", "white", "darkorange", "darkorange", "royalblue", "royalblue", "white")) +
  scale_fill_manual(values = c("darkorange", "darkorange", "royalblue", "royalblue", "white", "white", "darkorange", "darkorange", "royalblue", "royalblue", "white", "white", "darkorange", "darkorange", "royalblue", "royalblue", "white", "white", "darkorange", "darkorange", "royalblue", "royalblue", "white")) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) +
  theme(plot.margin = margin(t = 0.25, b = 0.25, l = 0.25, r = 20),
        text = element_text(family = "Times"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 10, face = "bold", color = "black", hjust = 0.5),
        axis.text.y = element_text(size = size, face = "bold", color = text, hjust = 0),
        axis.title = element_text(size = 16, face = "bold", color = "black"),
        axis.ticks.y = element_blank(),
        panel.spacing.x = unit(3, "lines"),
        panel.spacing.y = unit(1.5, "lines"),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold", color = "black"),
        axis.line.x = element_line(colour = "black", size = 1, linetype = "solid"),
        axis.line.y = element_blank()) +
  guides(color = "none", fill = "none") +
  facet_wrap(.~trait, nrow = 4, ncol = 5, labeller = labeller(trait = c("bf" = "Female linear\nestimate", "gf" = "Female quadratic\nestimate", "bm" = "Male linear\nestimate", "gm" = "Male quadratic\nestimate", "bmf" = "Interaction between\nmale and female color")))
save_plot("C:/Users/FARGEVIEILLE/Desktop/SelectionGradients_Updated_to2021/ForestPlots/ld_pl.png", ld_pl_plot, ncol = 1, nrow = 1, base_height = 12, base_width = 15)
ggsave("C:/Users/FARGEVIEILLE/Desktop/SelectionGradients_Updated_to2021/ForestPlots/ld_pl.pdf", plot = ld_pl_plot, 
       device = cairo_pdf,
       width = 15, height = 12, dpi = 300) 
