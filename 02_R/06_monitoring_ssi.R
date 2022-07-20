#' ---
#' project: Zivi2022R    ##################################################
#' title:   SSI
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2022-07-18 16:10:04
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(tidyverse)
source("02_R/00_fun.R")
library(ggnewscale)



# read data ---------------------------------------------------------------

dat <- read_excel("01_data/ssi_monitoring_output_cens.xlsx",
                  na = c("NA", "N/A"))



# Normothermie ------------------------------------------------------------

max_per <- max(dat$Messperiode)
dat <- dat %>% 
  rename(Normothermie = `Normothermie ok?`,
         Antibiotika = `Antibiotikaabgabe ok?`) %>% 
  mutate(Normotermie2 = ifelse(Normothermie == "Korrekt", 1, 0),
         Antibiotika2 = ifelse(Antibiotika == "Korrekt", 1, 0),
         Messperiode = factor(Messperiode,
                              levels = year_full_levels(dat$Messperiode)),
         Normothermie = factor(Normothermie,
                               levels = c("Zu kühl", "Keine Messung",
                                          "Korrekt")),
         Klinik = Klinik_Akronym)
t_normo <- prop_period_fun(dat = dat, response = "Normotermie2", by = "Klinik")
p_normo <- plot_prop_period(prop_period = t_normo, facet_var = "Klinik", 
                            fileprefix = "03_figures/06_ssi_normo_01",
                            ncols = 4)

## Barplots

pn_agg <- barplot_agg(dat = dat, response = "Normothermie",
                      fileprefix = "03_figures/06_ssi_normo_02")


pn_by <- barplot_by(dat = dat, response = "Normothermie", by = "Klinik"
                    , fileprefix = "03_figures/06_ssi_normo_02"
                    , n_min = 0, gray_area_dat = t_normo[[2]])




# Antibiotika abgabe ------------------------------------------------------


t_AB <- prop_period_fun(dat = dat, response = "Antibiotika2", by = "Klinik")
p_AB <- plot_prop_period(prop_period = t_AB, facet_var = "Klinik", 
                         fileprefix = "03_figures/06_ssi_AB_01", ncols = 4)

## Barplots

pa_agg <- barplot_agg(dat = dat, response = "Antibiotika",
                      fileprefix = "03_figures/06_ssi_AB_02")


pa_by <- barplot_by(dat = dat, response = "Antibiotika", by = "Klinik"
                    , fileprefix = "03_figures/06_ssi_AB_02"
                    , n_min = 0, gray_area_dat = t_normo[[2]])
