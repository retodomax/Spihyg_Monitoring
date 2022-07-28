#' ---
#' project: Zivi2022R    ##################################################
#' title:   nvHAP
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2022-07-19 14:39:10
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(tidyverse)
source("02_R/00_fun.R")
library(ggnewscale)



# read data ---------------------------------------------------------------

dat <- read_excel_n("01_data/nvhap_monitoring_output.xlsx") %>% 
  rename(Klinik = FachlicheOE_Kurzbezeichnung)
patPerOe <- read_excel("01_data/patPerOe.xlsx") %>% 
  rename(Messperiode = Periode,
         Klinik = FachlicheOE_Kurzbezeichnung,
         n = Faelle)


# tables and plots --------------------------------------------------------

p_nvHAP <- prop_period_fun(dat = dat, response = "nvHap_Infekt", by = "Klinik",
                binomial = FALSE, patPerOe = patPerOe)
pp <- plot_prop_period(prop_period = p_nvHAP, fileprefix = "07_nvHAP_p"
                 , by = "Klinik", n_min = 10, ncols = 5
                 , ylim = c(0, 0.05), ylab = "Infekte / Austritte"
                 , width1 = 16, height1 = 8, y_perc = TRUE)

r_nvHAP <- prop_period_fun(dat = dat, response = "nvHap_Infekt", by = "Klinik",
                           binomial = FALSE, patPerOe = patPerOe, rate = TRUE)
pr <- plot_prop_period(prop_period = r_nvHAP, fileprefix = "07_nvHAP_r"
                 , by = "Klinik", n_min = 10, ncols = 5
                 , ylim = c(0, 7), ylab = "Infekte / 1000 Patiententage"
                 , width1 = 16, height1 = 8)


# split by_plots ----------------------------------------------------------


## for proportion
pdat <- ggplot_build(pp[[2]])
kl <- unique(pdat$plot$plot_env$.$Klinik)
gr <- split(kl, cut(seq_along(kl), 2, labels = FALSE))
p_nvHAP1 <- p_nvHAP2 <- p_nvHAP
p_nvHAP1[[1]] <- p_nvHAP[[1]] %>% filter(Klinik %in% gr[[1]])
p_nvHAP2[[1]] <- p_nvHAP[[1]] %>% filter(Klinik %in% gr[[2]])

pp1 <- plot_by(dat = p_nvHAP1, by = "Klinik", fileprefix = "07_nvHAP_p_split1"
               , ncols = 3, ylim = c(0, 0.05), ylab = "Infekte / Austritte"
               , y_perc = TRUE)
pp2 <- plot_by(dat = p_nvHAP2, by = "Klinik", fileprefix = "07_nvHAP_p_split2"
               , ncols = 3, ylim = c(0, 0.05), ylab = "Infekte / Austritte"
               , y_perc = TRUE)

## for rate
pdat <- ggplot_build(pr[[2]])
kl <- unique(pdat$plot$plot_env$.$Klinik)
gr <- split(kl, cut(seq_along(kl), 2, labels = FALSE))
r_nvHAP1 <- r_nvHAP2 <- r_nvHAP
r_nvHAP1[[1]] <- r_nvHAP[[1]] %>% filter(Klinik %in% gr[[1]])
r_nvHAP2[[1]] <- r_nvHAP[[1]] %>% filter(Klinik %in% gr[[2]])

pr1 <- plot_by(dat = r_nvHAP1, by = "Klinik", fileprefix = "07_nvHAP_r_split1"
               , ncols = 3, ylim = c(0, 7),
               ylab = "Infekte / 1000 Patiententage")
pr2 <- plot_by(dat = r_nvHAP2, by = "Klinik", fileprefix = "07_nvHAP_r_split2"
               , ncols = 3, ylim = c(0, 7),
               ylab = "Infekte / 1000 Patiententage")



## better solution would use ggforce::facet_wrap_paginate but
## this does currently not work in combination with lemon::facet_rep_wrap()...
## See here: https://stackoverflow.com/q/73138937/6152316



