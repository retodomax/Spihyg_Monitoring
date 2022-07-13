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

## Aggregated Barplot
ag_bar <- dat %>% 
  ggplot(aes(x = Messperiode, fill = Indikation)) +
  geom_bar(position = "fill", width = .5) + 
  scale_x_discrete(drop=FALSE) +
  theme_bw() +
  ylab("Anteil Beobachtungen") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("03_figures/02_Cauti_02_Bar_agg.png", plot = ag_bar,
       width = 16, height = 8, units = "cm", scale = 1.5)

## by Barplot
dat_agg <- dat %>% 
  group_by(Messperiode, Klinik) %>% 
  summarise(n = n())

myclin <- dat_agg %>% filter(Messperiode == max_per, n > 9) %>% pull(Klinik)
dat2 <- dat %>% 
  filter(Klinik %in% myclin)
dat_agg <- dat_agg %>% filter(Klinik %in% myclin)

cols2 <- c("USZ_Durchschnitt" = "lightgray")
p <- dat2 %>% 
  ggplot(aes(x = Messperiode)) +
  geom_area(data = t_anyIndi[[2]],
            mapping = aes(x = Messperiode, y = percent_correct, group = 1,
                          fill = "USZ_Durchschnitt")) + 
  scale_fill_manual(name = "", values = cols2) +
  new_scale_fill() +
  geom_bar(aes(fill = Indikation), position = "fill", width = .5) +
  scale_x_discrete(drop=FALSE) +
  theme_bw() +
  ylab("Anteil Beobachtungen") +
  scale_y_continuous(breaks = seq(0,1,0.25), limits = c(-.3,1)) +
  geom_text(data = dat_agg, aes(label = n, y = -.25, x = Messperiode)) +
  annotate(geom="text", x= 0.8, y=-.1, label="Gesamt N",
           hjust = 0) +
  facet_wrap(~ Klinik) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("03_figures/02_Cauti_02_Bar_by.png", plot = p,
       width = 24.7, height = 20, units = "cm", scale = 1.5)
