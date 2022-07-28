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

dat <- read_excel_n("01_data/clabsi_monitoring_output.xlsx")


# transform ---------------------------------------------------------------

var_sel <- c("Buendel_calc", "HD erfolgt?", "Sterile HS?", "Steriler Mantel?",
             "Steriles grosses Tuch?", "Chirurgische Maske und Haube?",
             "Alles korrekt angezogen?", "Asepsis eingehalten?",
             "Hautdesinfektion mit CHG 2%?", "Hautdesinfektion korrekt?",
             "Einwirkzeit eingehalten?", "Pflaster aseptisch angebracht")

# recode response variable and only include last 10 years
dat <- dat %>% 
  mutate(across(all_of(var_sel), ~ case_when(.x == "Ja" ~ 1, .x == "Nein" ~ 0)))
ldat <- dat %>% 
  pivot_longer(any_of(var_sel))



# tables and plot ---------------------------------------------------------

t_var <- prop_period_fun(dat = ldat, response = "value", by = "name",
                         filter_crit = NULL)
# redefine separated and aggregated table
t_var[[2]] <- t_var[[1]] %>% 
  filter(name == "Buendel_calc")
t_var[[1]] <- t_var[[1]] %>% 
  filter(name != "Buendel_calc")

p <- plot_prop_period(prop_period = t_var, fileprefix = "04_clapsi",
                 by = "name", ncols = 3, gray_area = FALSE,
                 width1 = 16, height1 = 8)

