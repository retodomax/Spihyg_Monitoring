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

dat <- read_excel("01_data/ssi_monitoring_output_cens.xlsx", na = c("NA", "N/A"))



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
                          fileprefix = "03_figures/06_ssi_normo_01", ncols = 4)



# Antibiotika abgabe ------------------------------------------------------


t_AB <- prop_period_fun(dat = dat, response = "Antibiotika2", by = "Klinik")
p_AB <- plot_prop_period(prop_period = t_AB, facet_var = "Klinik", 
                          fileprefix = "03_figures/06_ssi_AB_01", ncols = 4)


### Barplot agg (currently for normo)
# ag_bar <- dat %>% 
#   filter(!is.na(Normothermie)) %>% 
#   ggplot(aes(x = Messperiode, fill = Normothermie)) +
#   geom_bar(position = "fill", width = .5) +
#   scale_x_discrete(drop = FALSE) +
#   theme_bw() +
#   ylab("Anteil Beobachtungen") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ag_bar
# ggsave("03_figures/06_ssi_normo_02_agg.png", plot = ag_bar,
#        width = 16, height = 8, units = "cm", scale = 1.5)
barplot_agg(dat = dat, response = "Normothermie",
            fileprefix = "03_figures/06_ssi_normo_02")


barplot_by(dat = dat, response = "Normothermie", by = "Klinik"
           , fileprefix = "03_figures/06_ssi_normo_02"
           , n_min = 0, gray_area_dat = t_normo[[2]])


## by Barplot (currently for normo)

dat_agg <- dat %>% 
  filter(!is.na(Normothermie)) %>% 
  group_by(Messperiode, Klinik) %>% 
  summarise(n = n())


#### Here we have a problem! All 
myclin <- dat_agg %>% filter(Messperiode == max_per, n > 9) %>% pull(Klinik)
myclin <- unique(dat_agg$Klinik)
dat2 <- dat %>%
  filter(Klinik %in% myclin,
         !is.na(Normothermie))
dat_agg <- dat_agg %>% filter(Klinik %in% myclin)



cols2 <- c("USZ_Durchschnitt" = "lightgray")
p <- dat2 %>% 
  ggplot(aes(x = Messperiode)) +
  geom_area(data = t_normo[[2]],
            mapping = aes(x = Messperiode, y = percent_correct, group = 1,
                          fill = "USZ_Durchschnitt")) + 
  scale_fill_manual(name = "", values = cols2) +
  new_scale_fill() +
  geom_bar(aes(fill = Normothermie), position = "fill", width = .5) +
  scale_x_discrete(drop = FALSE) +
  theme_bw() +
  ylab("Anteil Beobachtungen") +
  scale_y_continuous(breaks = seq(0,1,0.25), limits = c(-.3,1)) +
  geom_text(data = dat_agg, aes(label = n, y = -.25, x = Messperiode)) +
  annotate(geom="text", x= 0.8, y=-.1, label="Gesamt N",
           hjust = 0) +
  facet_wrap(~ Klinik) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p
ggsave("03_figures/02_Cauti_02_Bar_by.png", plot = p,
       width = 24.7, height = 20, units = "cm", scale = 1.5)



## ToDo:
## - Fix problem with by Barplot (filtering of Klinik)
## - Write function for barplot plots
## -- continue here: write function for Barplot by in CAUTI
## - Apply function to both SSI and Cauti
