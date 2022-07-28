#' ---
#' project: Zivi2022R    ##################################################
#' title:   Tests
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2022-07-20 16:54:37
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(tidyverse)
source("02_R/00_fun.R")



# ErschBild ---------------------------------------------------------------


dat <- read_excel("01_data/erschBild_output.xlsx", na = c("NA", "N/A"))

### Remove this as soon as we know what Berufsgruppe 0 means
dat <- dat %>% filter(Berufsgruppe != "0")

dat$Messperiode
dat %>% 
  filter(Berufsgruppe == "Arzt_Aerztin",
         Messperiode == "2018-1") %>% 
  summarise(share = sum(PrivateKleidung_Status == "korrekt")/n(),
            n = n())

