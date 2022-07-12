library(readxl)
library(magrittr)
library(tidyverse)
dat <- read_excel("VAP_python_output_cens.xlsx", na = "NA")
dat <- read_excel("test_data.xlsx", na = "NA")

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

t1 <- dat %>% 
  filter(OKH_Kontraindikation_Vorabfrage == "Nein",
         OKH_Kontraindikation == "0",
         OKH_Kontraindikation_Anderes == "0") %$% 
  table(Station, OKH_Formel)

t1 %>% addmargins()
prop.table(t1, margin = 1)



