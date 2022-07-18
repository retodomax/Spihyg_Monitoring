#' ---
#' project: Zivi2022R    ##################################################
#' title:   Erscheinungsbild
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2022-07-14 14:48:37
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(tidyverse)
source("02_R/00_fun.R")


# read data ---------------------------------------------------------------

dat <- read_excel("01_data/erschBild_output.xlsx", na = c("NA", "N/A"))

### Remove this as soon as we know what Berufsgruppe 0 means
dat <- dat %>% filter(Berufsgruppe != "0")


# transform ---------------------------------------------------------------

var_sel <- c("Fingernägel_Status", "Schmuck_Status", "Haare_Status",
             "Handgelenke_Status", "PrivateKleidung_Status",
             "Mundschutz_Status", "Alles_Korrekt")

dat <- dat %>% 
  filter(Messperiode %in% rev(year_full_levels(Messperiode))[1:10]) %>% 
  mutate(across(all_of(var_sel), ~ case_when(.x == "korrekt" ~ 1,
                                             .x == "nicht korrekt" ~ 0)))


# Overall -----------------------------------------------------------------

ldat <- dat %>% 
  pivot_longer(any_of(var_sel))

ldat_agg <- ldat %>% 
  filter(name == "Alles_Korrekt")
t_corr <- prop_period_fun(dat = ldat_agg, response = "value", by = "name")
p <- plot_agg(dat = t_corr[[2]],
              fileprefix = "03_figures/05_ErscheinungsBild_all",
              suffix = "_agg")

ldat_by <- ldat %>% 
  filter(name != "Alles_Korrekt")
t_all <- prop_period_fun(dat = ldat_by, response = "value", by = "name")
p <- plot_by(dat = t_all, fileprefix = "03_figures/05_ErscheinungsBild_all",
             facet_var = "name", ncols = 3, gray_area = FALSE,
             width2 = 16, height2 = 10)


# Separate by profession --------------------------------------------------

### Delete this as soon as we have also Observations for Mundschutz
var_sel <- var_sel[!(var_sel %in% c("Mundschutz_Status"))]
######

## loop through all variable
for(i in seq_along(var_sel)){
  vari <- var_sel[i]
  print(vari)
  t_finger <- prop_period_fun(dat = dat, response = vari,
                              by = "Berufsgruppe")
  p <- plot_by(dat = t_finger,
               fileprefix = paste0("03_figures/05_ErschBild_", vari),
               facet_var = "Berufsgruppe", ncols = 4, width2 = 16,
               height2 = 7)
}

