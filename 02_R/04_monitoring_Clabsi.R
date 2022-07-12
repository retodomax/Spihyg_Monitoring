#' ---
#' project: Zivi2022R    ##################################################
#' title:   CLAPSI
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2022-07-12 09:44:18
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(tidyverse)
source("02_R/00_fun.R")


# read data ---------------------------------------------------------------

dat <- read_excel("01_data/clabsi_output_cens.xlsx", na = "NA")


# transform ---------------------------------------------------------------

var_sel <- c("Buendel_calc", "HD erfolgt?", "Sterile HS?", "Steriler Mantel?",
             "Steriles grosses Tuch?", "Chirurgische Maske und Haube?",
             "Alles korrekt angezogen?", "Asepsis eingehalten?",
             "Hautdesinfektion mit CHG 2%?", "Hautdesinfektion korrekt?",
             "Einwirkzeit eingehalten?", "Pflaster aseptisch angebracht")

dat <- dat %>% 
  filter(Messperiode %in% rev(year_full_levels(Messperiode))[1:10]) %>% 
  mutate(across(all_of(var_sel), ~ case_when(.x == "Ja" ~ 1, .x == "Nein" ~ 0)))


# single plot -------------------------------------------------------------

full_grid <- expand_grid(Messperiode = year_full_levels(dat$Messperiode))
t_Buendel <- prop_period_fun(dat = dat, response = "Buendel_calc", by = NA,
                         filter_crit = NA, full_grid = full_grid)
p_OKH <- plot_prop_period(prop_period = t_Buendel,
                          fileprefix = "03_figures/04_clapsi_01")
