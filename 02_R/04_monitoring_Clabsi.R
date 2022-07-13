#' ---
#' project: Zivi2022R    ##################################################
#' title:   CLAPSI
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2022-07-12 09:44:18
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(tidyverse)
source("02_R/00_fun.R")


# read data ---------------------------------------------------------------

dat <- read_excel("01_data/clabsi_output_cens.xlsx", na = "NA")


# transform ---------------------------------------------------------------

var_sel <- c("Buendel_calc", "HD erfolgt?", "Sterile HS?", "Steriler Mantel?",
             "Steriles grosses Tuch?", "Chirurgische Maske und Haube?",
             "Alles korrekt angezogen?", "Asepsis eingehalten?",
             "Hautdesinfektion mit CHG 2%?", "Hautdesinfektion korrekt?",
             "Einwirkzeit eingehalten?", "Pflaster aseptisch angebracht")

dat <- dat %>% 
  filter(Messperiode %in% rev(year_full_levels(Messperiode))[1:10]) %>% 
  mutate(across(all_of(var_sel), ~ case_when(.x == "Ja" ~ 1, .x == "Nein" ~ 0)))


# single plot -------------------------------------------------------------



# all plots together ------------------------------------------------------

full_grid <- expand_grid(Messperiode = year_full_levels(dat$Messperiode))


p_var <- vector("list", length = length(var_sel))
i <- 1
for(i in seq_along(var_sel)){
  resp <- var_sel[i]
  t_var <- prop_period_fun(dat = dat, response = resp, by = NA,
                               filter_crit = NA, full_grid = full_grid)
  p_var[[i]] <- plot_prop_period(prop_period = t_var,
                            fileprefix = paste0("03_figures/04_clapsi_",
                                                sprintf("%02d", i)),
                            title = resp)[[1]]
}

library(ggpubr)
ggarrange(p_var[[2]], p_var[[3]], p_var[[4]],
          p_var[[5]], p_var[[6]], p_var[[7]],
          ncol = 3, nrow = 2)

ggarrange(p_var[[8]], p_var[[9]], p_var[[10]],
          p_var[[11]], p_var[[12]],
          ncol = 3, nrow = 2)



### Think about alternative solution:
# 1) make table long (not for each variable a column)
# 2) use facet_grid()


ldat <- dat %>% 
  pivot_longer(any_of(var_sel))
# debugonce(prop_period_fun)
t_var <- prop_period_fun(dat = ldat, response = "value", by = "name",
                         filter_crit = NA, full_grid = full_grid)
plot_prop_period(prop_period = t_var, fileprefix = "03_figures/04_clapsi_all_",
                 facet_var = "name", ncols = 3, gray_area = FALSE)


## To improve
# a) remove Buendel_calc              ==> Manually befor calc
# b) split over two plots             ==> manually select some columns


