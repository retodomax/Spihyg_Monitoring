#' ---
#' project: Zivi2022R    ##################################################
#' title:   Monitoring VAP
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2022-07-05 16:12:43
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(tidyverse)
source("02_R/00_fun.R")



# read data ---------------------------------------------------------------

dat <- read_excel("01_data/VAP_python_output2_cens.xlsx", na = "NA")

dat <- dat %>% 
  mutate(OKH_Formel = ifelse(OKH_Formel == "korrekt", 1,
                             ifelse(OKH_Formel == "nicht korrekt", 0, NA)),
         Sedationsstopp_Ja_Nein = ifelse(Sedationsstopp_Ja_Nein == "Ja", 1,
                                         ifelse(Sedationsstopp_Ja_Nein == "Nein",
                                                0, NA)),
         Mundpflege = ifelse(Mundpflege == "Ja", 1,
                             ifelse(Mundpflege == "Nein", 0, NA))) %>% 
  filter(Messperiode %in% rev(year_full_levels(Messperiode))[1:10])


# create tables -----------------------------------------------------------

full_grid <- expand_grid(Messperiode = year_full_levels(dat$Messperiode),
                        Station = unique(dat$Station))

t_OKH <- prop_period_fun(dat = dat, response = "OKH_Formel", by = "Station",
                filter_crit = "OKH_istRelevant", full_grid = full_grid)
t_Sed <- prop_period_fun(dat = dat, response = "Sedationsstopp_Ja_Nein",
                         by = "Station",
                         filter_crit = "Sedationsstopp_istRelevant",
                         full_grid = full_grid)
t_Mund <- prop_period_fun(dat = dat, response = "Mundpflege",
                          by = "Station",
                          filter_crit = "Mundpflege_istRelevant",
                          full_grid = full_grid)



# create plots ------------------------------------------------------------

p_OKH <- plot_prop_period(prop_period = t_OKH,
                          fileprefix = "03_figures/01_VAP_OKH")
p_Sed <- plot_prop_period(prop_period = t_Sed,
                          fileprefix = "03_figures/01_VAP_Sed")
p_Mund <- plot_prop_period(prop_period = t_Mund,
                           fileprefix = "03_figures/01_VAP_Mund")
