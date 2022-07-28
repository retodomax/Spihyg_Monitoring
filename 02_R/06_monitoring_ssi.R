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

dat <- read_excel_n("01_data/ssi_monitoring_output.xlsx")

dat <- dat %>% 
  rename(Normothermie = `Normothermie ok?`,
         Antibiotika = `Antibiotikaabgabe ok?`,
         Normothermie_Beg = `Normothermie Begründung`,
         Antibiotika_Beg = `Antibiotikaabgabe Begründung`) %>% 
  mutate(Klinik = Klinik_Akronym,
         Normotermie2 = ifelse(Normothermie == "korrekt", 1, 0), ## NA stay NA
         Antibiotika2 = ifelse(Antibiotika == "korrekt", 1, 0),
         Messperiode = factor(Messperiode,
                              levels = year_full_levels(dat$Messperiode)),
         Normothermie = factor(Normothermie_Beg,
                               levels = c("Zu kühl", "Keine Messung",
                                          "Korrekt")),
         Antibiotika = factor(Antibiotika_Beg,
                              levels = c("zu früh", "zu spät",
                                         "Keine Verabreichung",
                                         "Keine Angaben", "Korrekt")))



# Normothermie ------------------------------------------------------------


t_normo <- prop_period_fun(dat = dat, response = "Normotermie2", by = "Klinik")
p_normo <- plot_prop_period(prop_period = t_normo, by = "Klinik", 
                            fileprefix = "06_ssi_normo_01",
                            ncols = 4)

## Barplots

pn_agg <- barplot_agg(dat = dat, response = "Normothermie",
                      fileprefix = "06_ssi_normo_02")

pn_by <- barplot_by(dat = dat, response = "Normothermie", by = "Klinik"
                    , fileprefix = "06_ssi_normo_02"
                    , n_min = 0, gray_area_dat = t_normo[[2]])




# Antibiotika abgabe ------------------------------------------------------


t_AB <- prop_period_fun(dat = dat, response = "Antibiotika2", by = "Klinik")
p_AB <- plot_prop_period(prop_period = t_AB, by = "Klinik", 
                         fileprefix = "06_ssi_AB_01", ncols = 4)

## Barplots

pa_agg <- barplot_agg(dat = dat, response = "Antibiotika",
                      fileprefix = "06_ssi_AB_02")

pa_by <- barplot_by(dat = dat, response = "Antibiotika", by = "Klinik"
                    , fileprefix = "06_ssi_AB_02"
                    , n_min = 0, gray_area_dat = t_normo[[2]])
