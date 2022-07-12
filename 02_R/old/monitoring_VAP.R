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

dat <- read_excel("VAP_python_output_cens.xlsx", na = "NA")
# dat <- read_excel("test_data.xlsx", na = "NA")

## Differences (VAP instead of test):
# OKH_Kontraindikation empty instead of 0

dat <- dat %>% 
  mutate(Station = factor(Station),
         OKH_Kontraindikation = factor(OKH_Kontraindikation),
         OKH_Kontraindikation_Vorabfrage = factor(OKH_Kontraindikation_Vorabfrage, levels = c("Nein", "Ja")),
         OKH_Typ_der_Lagerung = factor(OKH_Typ_der_Lagerung),
         Mundpflege = factor(Mundpflege, levels = c("Nein", "Ja")),
         Sedationsstopp_Vorabfrage_Kontraindikation = factor(Sedationsstopp_Vorabfrage_Kontraindikation, levels = c("Nein", "Ja")),
         Sedationsstopp_Kontraindikation = factor(Sedationsstopp_Kontraindikation, levels = c("Nein", "Ja")),
         Sedationsstopp_Vorabfrage_Sedation = factor(Sedationsstopp_Vorabfrage_Sedation, levels = c("Nein", "Ja")),
         Sedationsstopp_Vorabfrage_Intubation = factor(Sedationsstopp_Vorabfrage_Intubation, levels = c("Nein", "Ja")),
         Sedationsstopp_Vorabfrage_Intubation = factor(Sedationsstopp_Vorabfrage_Intubation, levels = c("Nein", "Ja")),
         Sedationsstopp_Ja_Nein = factor(Sedationsstopp_Ja_Nein, levels = c("Nein", "Ja")),
         OKH_Formel = factor(OKH_Formel))
sort(unique(dat$Messperiode))




# tables ------------------------------------------------------------------

dat2 <- dat %>% 
  filter(OKH_Kontraindikation_Vorabfrage == "Nein",
         # OKH_Kontraindikation == "0",
         # OKH_Kontraindikation_Anderes == "0"
         is.na(OKH_Kontraindikation),
         is.na(OKH_Kontraindikation_Anderes))

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

prop_period <- dat2 %>% 
  group_by(Messperiode) %>% 
  summarize(n = sum(!is.na(OKH_Formel)),
            n_correct = sum(OKH_Formel == "korrekt", na.rm = TRUE),
            n_wrong = sum(OKH_Formel == "nicht korrekt", na.rm = TRUE)) %>% 
  mutate(percent_correct = n_correct/n) %>% 
  rowwise() %>% 
  mutate(CI_lo = binom_CI(n_correct, n, 1),
         CI_up = binom_CI(n_correct, n, 2)) %>% 
  ungroup()

prop_period_by <- dat2 %>% 
  group_by(Messperiode, Station) %>% 
  summarize(n = sum(!is.na(OKH_Formel)),
            n_correct = sum(OKH_Formel == "korrekt", na.rm = TRUE),
            n_wrong = sum(OKH_Formel == "nicht korrekt", na.rm = TRUE)) %>% 
  mutate(percent_correct = n_correct/n) %>% 
  rowwise() %>% 
  mutate(CI_lo = binom_CI(n_correct, n, 1),
         CI_up = binom_CI(n_correct, n, 2)) %>% 
  ungroup()


# plots -------------------------------------------------------------------


ggplot(prop_period, aes(x = Messperiode, y = percent_correct, group = 1,
                        label = round(percent_correct, 2))) +
  geom_point() +
  geom_text(hjust=-.3, vjust=-.3) +
  geom_line() +
  geom_errorbar(aes(ymin = CI_lo, ymax = CI_up), width = 0.1) +
  theme_bw() +
  coord_cartesian(xlim = c(0, 100), ylim = c(0,1))

  geom_

ggplot(prop_period_by, aes(x = Messperiode, y = percent_correct, group = 1)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = CI_lo, ymax = CI_up), width = 0.1) +
  theme_bw() +
  ylim(0, 1) +
  facet_wrap(~ Station)
