#'Pre-analyses to test correlations between parasite load and nestling body condition
#'
#'Load packages
library(lme4)
library(merTools)
library(parameters)

#'Load dataset
data_cond <- read.csv("data_cond.csv")

#'Testing correlation among explanatory variables and covariables
cor.test(data_cond$par_load, data_cond$nestling, method = "pearson")

data_cond %>% 
  group_by(year) %>% 
  summarise(cor = cor(par_load, nestling), n = n())

#'Building models
mass_mod <- lme4::lmer(mass ~ relative_par_load * nestling + (1|year) + (1|nestbox_raised), data = data_cond)
mass_tab <- tibble(variable = c("intercept", "relative_par_load", "nestling", "relative_par_load:nestling"),
                   estimate = round(coef(summary(mass_mod))[ , 1], 4),
                   se = round(coef(summary(mass_mod))[ , 2], 4),
                   low_95ci = estimate - 1.96*se,
                   up_95ci = estimate + 1.96*se)
rand_mass_tab <- as.data.frame(vc <- VarCorr(mass_mod))