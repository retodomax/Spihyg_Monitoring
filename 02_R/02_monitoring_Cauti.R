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

dat <- read_excel("01_data/cauti_output_cens.xlsx", na = "NA")
dat <- dat %>% 
  rename(Tgl_Evaluation = `Tgl. Evaluation`) %>% 
  mutate(Tgl_Evaluation = case_when(Tgl_Evaluation == "ja" ~ 1,
                                    Tgl_Evaluation == "nein" ~ 0)) %>% 
  filter(Messperiode %in% rev(year_full_levels(Messperiode))[1:10])

max_per <- max(dat$Messperiode)


# Daily Evaluation --------------------------------------------------------

# full_grid <- expand_grid(Messperiode = year_full_levels(dat$Messperiode),
#                          Klinik = unique(dat$Klinik))
# 
# t_cauti <- prop_period_fun(dat = dat, response = "Tgl_Evaluation", by = "Klinik",
#                          filter_crit = NA, full_grid = full_grid)
t_cauti <- prop_period_fun(dat = dat, response = "Tgl_Evaluation",
                           by = "Klinik")

myclin <- t_cauti[[1]] %>% filter(Messperiode == max_per, n > 9) %>% pull(Klinik)
t_cauti[[1]] <- t_cauti[[1]] %>%
  filter(Klinik %in% myclin)

p_cauti <- plot_prop_period(prop_period = t_cauti,
                            fileprefix = "03_figures/02_Cauti_01_daily",
                            facet_var = "Klinik", ncols = 4,
                            width1 = 16, height1 = 8)



# Valid Indication --------------------------------------------------------

f_levels <- c("Keine Indikation", "Urinmonitoring/Bilanzierung",
              "Prolongierte Immobilisation", "Palliation PLUS Komfort",
              "Operation", "Harnverhalt", "Andere", "Bilanz")
dat <- dat %>% 
  mutate(Indikation = factor(Indikation, levels = f_levels),
         Messperiode = factor(Messperiode,
                              levels = year_full_levels(dat$Messperiode)),
         AnyIndi = as.numeric(Indikation != "Keine Indikation"))

# full_grid <- expand_grid(Messperiode = year_full_levels(dat$Messperiode),
#                          Klinik = unique(dat$Klinik))
# t_anyIndi <- prop_period_fun(dat = dat, response = "AnyIndi", by = "Klinik",
#                              filter_crit = NA, full_grid = full_grid)
t_anyIndi <- prop_period_fun(dat = dat, response = "AnyIndi", by = "Klinik")

myclin <- t_anyIndi[[1]] %>% filter(Messperiode == max_per, n > 9) %>% pull(Klinik)
t_anyIndi[[1]] <- t_anyIndi[[1]] %>% 
  filter(Klinik %in% myclin)
p_anyIndi <- plot_prop_period(prop_period = t_anyIndi,
                              fileprefix = "03_figures/02_Cauti_03_valid",
                              facet_var = "Klinik", ncols = 4,
                              width1 = 16, height1 = 8)




# Indication Barplot ------------------------------------------------------


ag_bar <- barplot_agg(dat = dat, response = "Indikation",
                      fileprefix = "03_figures/02_Cauti_02")

barplot_by(dat = dat, response = "Indikation", by = "Klinik"
           , fileprefix = "03_figures/02_Cauti_02", n_min = 10
           , gray_area_dat = t_anyIndi[[2]])
