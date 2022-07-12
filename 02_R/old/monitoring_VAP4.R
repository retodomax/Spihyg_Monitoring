#' ---
#' project: Zivi2022R    ##################################################
#' title:   Monitoring VAP
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2022-07-04 15:19:42
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(tidyverse)



# read data ---------------------------------------------------------------

dat <- read_excel("VAP_python_output2_cens.xlsx", na = "NA")

## Change data type
# dat <- dat %>% 
#   mutate(Station = factor(Station),
#          OKH_Kontraindikation = factor(OKH_Kontraindikation),
#          OKH_Kontraindikation_Vorabfrage = factor(OKH_Kontraindikation_Vorabfrage, levels = c("Nein", "Ja")),
#          OKH_Typ_der_Lagerung = factor(OKH_Typ_der_Lagerung),
#          Mundpflege = factor(Mundpflege, levels = c("Nein", "Ja")),
#          Sedationsstopp_Vorabfrage_Kontraindikation = factor(Sedationsstopp_Vorabfrage_Kontraindikation, levels = c("Nein", "Ja")),
#          Sedationsstopp_Kontraindikation = factor(Sedationsstopp_Kontraindikation, levels = c("Nein", "Ja")),
#          Sedationsstopp_Vorabfrage_Sedation = factor(Sedationsstopp_Vorabfrage_Sedation, levels = c("Nein", "Ja")),
#          Sedationsstopp_Vorabfrage_Intubation = factor(Sedationsstopp_Vorabfrage_Intubation, levels = c("Nein", "Ja")),
#          Sedationsstopp_Vorabfrage_Intubation = factor(Sedationsstopp_Vorabfrage_Intubation, levels = c("Nein", "Ja")),
#          Sedationsstopp_Ja_Nein = factor(Sedationsstopp_Ja_Nein, levels = c("Nein", "Ja")),
#          OKH_Formel = factor(OKH_Formel))

## function to get all Messperiode levels
year_full_levels <- function(x){
  ## 1) extract range
  year_levels <- sort(unique(x))
  year_range <- year_levels[c(1, length(year_levels))]
  
  ## 2) make numeric
  toNum <- function(x){
    x <- as.numeric(x)
    x[1] + ifelse(x[2] == 2, 0.5, 0)
  }
  year_range_num <- sapply(strsplit(year_range, "-"), toNum)
  
  ## 3) make sequence
  year_seq <- seq(year_range_num[1], year_range_num[2], 0.5)
  
  ## 4) transform back
  toChar <- function(x){
    x <- paste0(x[1], "-", ifelse(x[2] == 5, 2, 1))
  }
  sapply(strsplit(format(year_seq, nsmall = 1), "\\."), toChar)
}




# tables ------------------------------------------------------------------

## filter for valid observations
dat2 <- dat %>% 
  filter(OKH_istRelevant == "Ja")

## table for only single Messperiode, separated by Station
t1 <- dat2 %>% 
  filter(Messperiode == "2021-2") %$% 
  table(Station, OKH_Formel)

t1 %>% addmargins()
prop.table(t1, margin = 1)

## function to get binomial confidence interval
binom_CI <- function(x, n, l) {
  CI <- binom.test(x, n, alternative =  "two.sided")$conf.int[l]
  return(CI)
}

prop_period_OKH <- dat2 %>% 
  group_by(Messperiode) %>% 
  summarize(n = sum(!is.na(OKH_Formel)),
            n_correct = sum(OKH_Formel == "korrekt", na.rm = TRUE),
            n_wrong = sum(OKH_Formel == "nicht korrekt", na.rm = TRUE)) %>% 
  mutate(percent_correct = n_correct/n) %>% 
  rowwise() %>% 
  mutate(CI_lo = binom_CI(n_correct, n, 1),
         CI_up = binom_CI(n_correct, n, 2)) %>% 
  ungroup() %>% 
  full_join(tibble(Messperiode = year_full_levels(dat$Messperiode))) %>% 
  arrange(Messperiode) %>% 
  mutate(n = ifelse(is.na(n), 0, n))


prop_period_OKH_by <- dat2 %>% 
  group_by(Messperiode, Station) %>% 
  summarize(n = sum(!is.na(OKH_Formel)),
            n_correct = sum(OKH_Formel == "korrekt", na.rm = TRUE),
            n_wrong = sum(OKH_Formel == "nicht korrekt", na.rm = TRUE)) %>% 
  mutate(percent_correct = n_correct/n) %>% 
  rowwise() %>% 
  mutate(CI_lo = binom_CI(n_correct, n, 1),
         CI_up = binom_CI(n_correct, n, 2)) %>% 
  ungroup() %>% 
  full_join(expand_grid(Messperiode = year_full_levels(dat$Messperiode),
                        Station = unique(dat$Station))) %>% 
  filter(!is.na(Station)) %>% 
  mutate(n = ifelse(is.na(n), 0, n))


# plots -------------------------------------------------------------------

p <- ggplot(prop_period_OKH, aes(x = Messperiode, y = percent_correct, group = 1,
                        label = round(percent_correct, 2))) +
  geom_errorbar(aes(ymin = CI_lo, ymax = CI_up), width = 0.1, col = "darkgray") +
  geom_point() +
  geom_text(hjust=-.3, vjust=-.3, size = 2.5) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(breaks = seq(0,1,0.25), limits = c(-.3,1)) +
  geom_text(aes(label = n, y = -.25)) +
  annotate(geom="text", x= 0.8, y=-.1, label="Gesamt N",
           hjust = 0) +
  ylab("Anteil korrekte Beobachtungen")


cols1 <- c("Spezifische Abteilung" = "black")
cols2 <- c("USZ Durchschnitt" = "lightgray")
p2 <- ggplot(prop_period_OKH_by, aes(x = Messperiode, y = percent_correct, group = 1)) +
  geom_area(data = prop_period_OKH, aes(x = Messperiode, y = percent_correct,
                                    fill = "USZ Durchschnitt")) +
  geom_errorbar(aes(ymin = CI_lo, ymax = CI_up), width = 0.1, col = "darkgray") +
  geom_point(aes(col = "Spezifische Abteilung")) +
  geom_line(aes(col = "Spezifische Abteilung")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0,1,0.25), limits = c(-.3,1)) +
  geom_text(aes(label = n, y = -.25)) +
  annotate(geom="text", x= 0.8, y=-.1, label="Gesamt N",
           hjust = 0) +
  facet_wrap(~ Station, ncol = 2) +
  ylab("Anteil korrekte Beobachtungen") +
  scale_colour_manual(name = "",values = cols1) +
  scale_fill_manual(name = "", values = cols2)

p
p2

ggsave("overall_VAP2.png", p, width = 8, height = 6, units = "cm", scale = 1.5)
ggsave("detail_VAP.png", p2, width = 16, height = 20, units = "cm", scale = 1.5)




