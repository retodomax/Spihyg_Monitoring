#' ---
#' project: Zivi2022R    ##################################################
#' title:   Monitoring_CAUTI
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2022-07-07 13:46:56
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(tidyverse)
source("02_R/00_fun.R")
library(ggnewscale)


# read data ---------------------------------------------------------------

dat <- read_excel_n("01_data/cauti_monitoring_output.xlsx")

# recode response variable
dat <- dat %>% 
  rename(Tgl_Evaluation = `Tgl. Evaluation`) %>% 
  mutate(Tgl_Evaluation = case_when(Tgl_Evaluation == "ja" ~ 1,
                                    Tgl_Evaluation == "nein" ~ 0))



# Daily Evaluation --------------------------------------------------------

t_cauti <- prop_period_fun(dat = dat, response = "Tgl_Evaluation",
                           by = "Klinik")

p_cauti <- plot_prop_period(prop_period = t_cauti, by = "Klinik",
                 fileprefix = "02_Cauti_01_daily", width1 = 16, height1 = 8,
                 n_min_by = 10, ncols = 3)



# Valid Indication --------------------------------------------------------

f_levels <- c("Keine Indikation", "Urinmonitoring/Bilanzierung",
              "Prolongierte Immobilisation", "Palliation PLUS Komfort",
              "Operation", "Harnverhalt", "Andere", "Bilanz")
dat <- dat %>% 
  mutate(Indikation = factor(Indikation, levels = f_levels),
         Messperiode = factor(Messperiode,
                              levels = year_full_levels(dat$Messperiode)),
         AnyIndi = as.numeric(Indikation != "Keine Indikation"))

t_anyIndi <- prop_period_fun(dat = dat, response = "AnyIndi", by = "Klinik")

p_anyIndi <- plot_prop_period(prop_period = t_anyIndi, by = "Klinik",
                              fileprefix = "02_Cauti_03_valid",
                              width1 = 16, height1 = 8,
                              n_min_by = 10, ncols = 3)



# Indication Barplot ------------------------------------------------------

ag_bar <- barplot_agg(dat = dat, response = "Indikation",
                      fileprefix = "02_Cauti_02")

by_bar <- barplot_by(dat = dat, response = "Indikation", by = "Klinik"
                     , fileprefix = "02_Cauti_02", n_min = 10
                     , gray_area_dat = t_anyIndi[[2]])

