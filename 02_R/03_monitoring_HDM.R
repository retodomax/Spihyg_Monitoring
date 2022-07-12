#' ---
#' project: Zivi2022R    ##################################################
#' title:   HDM
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2022-07-08 13:34:10
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(tidyverse)
source("02_R/fun.R")


# read data ---------------------------------------------------------------

dat <- read_excel("01_data/hdm_output.xlsx", na = "NA") %>% 
  filter(Messperiode %in% rev(year_full_levels(Messperiode))[1:10])





# aggregate ---------------------------------------------------------------

dat_main <- dat %>% 
  group_by(Messperiode, Hauptbereich, Subbereich, Station) %>% 
  summarise(desinf_per_careday = mean(`Desinfektionsmittel/Pflegetag [ml/t]`),
            n = n()) %>% 
  {dat_station <<- .} %>% 
  summarise(desinf_per_careday = weighted.mean(desinf_per_careday, n),
            n = sum(n)) %>% 
  {dat_sub <<- .} %>% 
  summarise(desinf_per_careday = weighted.mean(desinf_per_careday, n),
            n = sum(n))


# plot --------------------------------------------------------------------

## Main
p_main <- dat_main %>%
  ggplot(aes(x = Messperiode, y = desinf_per_careday,
             colour = Hauptbereich, group = Hauptbereich,
             label = round(desinf_per_careday))) +
  geom_point() +
  geom_line() +
  geom_text(hjust=0, vjust=-1.3, size = 2.5) +
  ylab("Desinfektionsmittel/Pflegetag [ml/t]") +
  theme_bw() +
  coord_cartesian(ylim = c(0, 400))
ggsave("03_figures/03_HDM_01_main.png", plot = p_main,
       width = 16, height = 8, units = "cm", scale = 1.5)

## IPS
p_IPS <- plot_hdm(dat = dat_station,
                  filter_by = "Intensivpflegestationen (IPS)",
                  filter_level = "Hauptbereich", wrap_by = "Station",
                  filename = "03_figures/03_HDM_02_IPS.png")


## IMC
p_IMC <- plot_hdm(dat = dat_station,
                  filter_by = "Intermediate-Care-Stationen (IMC)",
                  filter_level = "Hauptbereich", wrap_by = "Station",
                  filename = "03_figures/03_HDM_02_IMC.png")


## Normalstationen
p_n <- plot_hdm(dat = dat_sub, filter_by = "Normalstationen (MSB)",
                filter_level = "Hauptbereich", wrap_by = "Subbereich",
                filename = "03_figures/03_HDM_02_Normalstation.png")

## Normalstation PFA
p_nA <- plot_hdm(dat = dat_station, filter_by = "PFA",
                 filter_level = "Subbereich", wrap_by = "Station",
                 filename = "03_figures/03_HDM_03_Normalstation_PFA.png")

## Normalstation PFB
p_nB <- plot_hdm(dat = dat_station, filter_by = "PFB",
                 filter_level = "Subbereich", wrap_by = "Station",
                 filename = "03_figures/03_HDM_03_Normalstation_PFB.png")

## Normalstation PFC
p_nC <- plot_hdm(dat = dat_station, filter_by = "PFC",
                 filter_level = "Subbereich", wrap_by = "Station",
                 filename = "03_figures/03_HDM_03_Normalstation_PFC.png")

## Normalstation PFD
p_nD <- plot_hdm(dat = dat_station, filter_by = "PFD",
                 filter_level = "Subbereich", wrap_by = "Station",
                 filename = "03_figures/03_HDM_03_Normalstation_PFD.png")

