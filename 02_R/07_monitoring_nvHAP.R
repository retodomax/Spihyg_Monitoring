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

dat <- read_excel("01_data/nvhap_surv_output_cens.xlsx",
                  na = c("NA", "N/A")) %>% 
  rename(Klinik = FachlicheOE_Kurzbezeichnung)
patPerOe <- read_excel("01_data/patPerOe.xlsx") %>% 
  rename(Messperiode = Periode,
         Klinik = FachlicheOE_Kurzbezeichnung,
         n = Faelle)

full_grid <- expand_grid(Messperiode = year_full_levels(dat$Messperiode),
                         Klinik = unique(na.omit(dat$Klinik)))


# pivot -------------------------------------------------------------------

dat %>% 
  group_by(Messperiode, Klinik) %>% 
  summarize(n_infect = sum(nvHap_Infekt == "Ja", na.rm = TRUE)) %>% 
  left_join(patPerOe) %>% 
  # full_join(full_grid) %>% 
  mutate(percent_infect = n_infect/n,
         rate_infect = n_infect/(VWD/1000)) %>% 
  rowwise() %>% 
  mutate(CI_lo_p = binom_CI(n_infect, n, 1),
         CI_up_p = binom_CI(n_infect, n, 2),
         CI_lo_r = poiss_CI(n_infect, VWD/1000, 1),
         CI_up_r = poiss_CI(n_infect, VWD/1000, 2)) %>% 
  ungroup() %>% 
  right_join(full_grid) %>% 
  arrange_at(c("Messperiode", "Klinik")) %>% 
  mutate(n = ifelse(is.na(n), 0, n))

### Hier weiter machen
## 1) Implement Poisson CI for Rate and Binomial CI for ratio
## 2) write everythin in function form (as below) such that it can be used
##    with `prop_period_fun`

p_nvHAP <- prop_period_fun(dat = dat, response = "nvHap_Infekt", by = "Klinik",
                binomial = FALSE, patPerOe = patPerOe)


plot_prop_period(prop_period = p_nvHAP, fileprefix = "03_figures/07_nvHAP_p"
                 , facet_var = "Klinik", n_min = 0, ncols = 5
                 , ylim = c(0, 0.05), ylab = "Infekte / Austritte")


r_nvHAP <- prop_period_fun(dat = dat, response = "nvHap_Infekt", by = "Klinik",
                           binomial = FALSE, patPerOe = patPerOe, rate = TRUE)
plot_prop_period(prop_period = r_nvHAP, fileprefix = "03_figures/07_nvHAP_r"
                 , facet_var = "Klinik", n_min = 0, ncols = 5
                 , ylim = c(0, 7), ylab = "Infekte / 1000 Patiententage")
