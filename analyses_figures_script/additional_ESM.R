#' Additional tables and figures from ESM


#-------------------#
# Preliminary steps #
#-------------------#

#'Load packages
library(tidyverse)
library(lme4)
library(car)
library(reshape2)
library(janitor)

#'Set working directory
setwd("~/GitHub/protocalliphora/")

#' Load and curate datasets

#'Load datasets and define variable structure
data_cond <- read.csv("data/nestling_condition.csv") %>% 
  dplyr::mutate(across(c("year", "indID", "nestbag"), ~ as.character(.)),
                across(c("hatch_size", "par_load", "laydate", "fledg_size"), ~ as.integer(.)),
                across(c("relative_par_load", "tarsus", "mass"), ~ as.numeric(.)))

recruit <- read.csv("data/recruit_color.csv") %>%
  dplyr::mutate(across(c("year", "indID", "nestbag"), ~ as.character(.)),
                across(c("hatch_size", "par_load", "laydate"), ~ as.integer(.)),
                across(c("BB", "BUVC", "YB", "YUVC", "YC", "relative_par_load"), ~ as.numeric(.)))

adult <- read.csv("data/adult_color.csv")  %>%
  dplyr::mutate(across(c("year", "indID", "nestbag"), ~ as.character(.)),
                across(c("BB", "BUVC", "YB", "YUVC", "YC", "relative_par_load"), ~ as.numeric(.)))





#---------------------------------------------------------------#
# Viusalizing parasite quantification method - Figure S1 in ESM #
#---------------------------------------------------------------#


#' Buiding boxplot

figs1 <- ggplot2::ggplot(data_cond, aes(x = year, y = relative_par_load, fill = nestbag)) +
  ggplot2::geom_boxplot(position = position_dodge(width = 0.8)) +
  ggplot2::scale_fill_manual(values = c("1" = "gray", "2" = "#E69F00"), #' Set colors for each protocol
                             labels = c("protocol 1", "protocol 2")) + #' set names displayed in the legend
  ggplot2::labs(x = "Year", y = "Parasite abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.title = element_blank(), 
        legend.text = element_text(size = 12),
        legend.position = "top")

ggplot2::ggsave("figures/figureS1.png", plot = figs1, 
                width = 10, height = 6, dpi = 300)

#' Testing model (in caption to Figure S1)
mod_protocol <- lme4::lmer(relative_par_load ~ nestbag + (1|year), 
                           data = data_cond %>% 
                             filter(year %in% c("2004", "2005", "2006", "2008", "2009"))) #' Select years when the two protocols were used
car::Anova(mod_protocol) # Compare outputs from both protocols with a Wald Chi square test



#-----------------------------------------------------------------------#
# Viusalizing correlation matrix for color variables - Figure s2 in ESM #
#-----------------------------------------------------------------------#
#' Create an empty matrix to insert correlation values
var_names <- c("BB", "BUVC", "YB", "YUVC", "YC")
combined_cor <- matrix(NA, nrow = 5, ncol = 5)
rownames(combined_cor) <- colnames(combined_cor) <- var_names


#' Estimate correlation matrix for yearlings and adults
cor_juv <- round(cor(recruit %>% 
                       dplyr::select(BB, BUVC, YB, YUVC, YC),
                     use = "pairwise.complete.obs"), 2)
cor_ad <- round(cor(adult %>% 
                      dplyr::select(BB, BUVC, YB, YUVC, YC),
                    use = "pairwise.complete.obs"), 2)


#' Insert values into the created empty matrix
combined_cor[lower.tri(combined_cor)] <- cor_juv[lower.tri(cor_juv)] # Insert values for yearling on the bottom left part
combined_cor[upper.tri(combined_cor)] <- cor_ad[upper.tri(cor_ad)] # Insert values for adults on the top right part
diag(combined_cor) <- 1 #' Add a diagonal of 1


#'Organize as a data frame to plot the matrix
long_cor <- melt(combined_cor)
colnames(long_cor) <- c("Var1", "Var2", "correlation")

#'Organize variables to be displayed in a certain order
long_cor$Var1 <- factor(long_cor$Var1, levels = rev(var_names)) #' set as the y-axis with "last variable" (YC) at the bottom
long_cor$Var2 <- factor(long_cor$Var2, levels = var_names)

#'Generate plot
ggplot2::ggplot(long_cor, aes(x = Var2, y = Var1, fill = correlation)) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::geom_text(aes(label = correlation), color = "black", size = 4.5) +
  ggplot2::scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", #' organize correlation gradient
                                midpoint = 0, limits = c(-1, 1),
                                guide = guide_colorbar(barheight = unit(8, "cm"),   #' set the legend
                                                       barwidth = unit(0.5, "cm"),  
                                                       title.position = "top",
                                                       title.hjust = 0.5), 
                                name = "correlation") +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::theme(plot.background = element_rect(fill = "white"), 
                 axis.text.x = element_text(angle = 0, hjust = 0.5),
                 axis.text.y = element_text(size = 12),
                 axis.title = element_blank(),
                 panel.grid = element_blank(),
                 legend.position = "right",
                 legend.title = element_text(size = 12, face = "bold"),
                 legend.text = element_text(size = 10))

ggplot2::ggsave("figures/figureS2.png", width = 8, height = 6, dpi = 300)



#--------------------------------------------------------#
# Tally number of individuals per year - Table S4 in ESM #
#--------------------------------------------------------#
tables4 <- data_cond %>% #' Tally numbers for individuals used in "nestling condition" model
  dplyr::group_by(year) %>% 
  dplyr::summarise(n_nestling = n()) %>% 
  dplyr::left_join(recruit %>%  #'  Tally numbers for individuals used in "yearling color" model
                     dplyr::group_by(year) %>% 
                     dplyr::summarise(n_yearling = n()),
                   by = "year") %>% 
  dplyr::left_join(adult %>%   #'  Tally numbers for individuals used in "adult color" model
                     dplyr::group_by(year) %>% 
                     dplyr::summarise(n_adult = n()),
                   by = "year") %>% 
  janitor::adorn_totals("row") #' Add totals for the three generated columns

write.csv(tables4, "output_tables/tableS4.csv", row.names = FALSE)



#-----------------------------------------------------------------------------#
# Report values for multicollinearity in all sets of models - Table S5 in ESM #
#-----------------------------------------------------------------------------#

#' Build models 
## Related to nestling condition
#' Linear mixed model with mass as the response variable (removing interaction that would obviously inflate multicollinearity)
mass_mod <- lme4::lmer(mass ~ relative_par_load + hatch_size + laydate + (1|year/broodID), data = data_cond)
nest_vif <- as.data.frame(car::vif(mass_mod))
nest_s5 <- tibble(model = "nestling",
                  terms = rownames(nest_vif),
                  vif_values = nest_vif[,1])




## Related to prediction 1 (yearling color) - (removing interaction that would obviously inflate multicollinearity)
recruit_mod <- lme4::lmer(YB ~ relative_par_load + hatch_size + sex + period + laydate + (1|year/broodID), data = recruit)
recruit_vif <- as.data.frame(car::vif(recruit_mod))
recruit_s5 <- tibble(model = "yearling",
                     terms = rownames(recruit_vif),
                     vif_values = recruit_vif[,1])


## Related to prediction 2 (parent color)
adult_mod <- lme4::lmer(YB ~ relative_par_load + hatch_size + sex + min_age + period + laydate + (1|year) + (1|pairID) +(1|indID), data = adult)
adult_vif <- as.data.frame(car::vif(adult_mod))
adult_s5 <- tibble(model = "adult",
                     terms = rownames(adult_vif),
                     vif_values = adult_vif[,1])

tables5 <- bind_rows(nest_s5, recruit_s5, adult_s5)
write.csv(tables5, "output_tables/tableS5.csv", row.names = FALSE)



