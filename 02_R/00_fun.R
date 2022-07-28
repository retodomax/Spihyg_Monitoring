#' ---
#' project: Zivi2022R    ##################################################
#' title:   functions
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2022-07-07 14:10:58
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(tidyverse)
library(lemon)
library(scales)
library(survival)



# Explanation -------------------------------------------------------------

## Usually we have a large data frame which contain the following variables
## 
## - Messperiode:           Half year in which the measurement took place
##                          (format: 20YY-X)
## - Response variable:     Contains 1 if success (usually korrekte Beobachtung)
##                          and 0 if failure
## - by variable:           We want to calculate success rate two times,
##                          once aggregated over all "by" levels and once
##                          separately for each "by" level
## - filter_crit variable:  Indicating if row contains valid observations.
##                          Often called *_istRelevant. Only include
##                          observations which have a "Ja" in this column.
## 
## We want to do the following 2 steps
##
## 1) Calculate 2 proportion table
##    - aggregated: separated by Messperiode
##    - separated: separated by Messperiode and "by" variable)
##    => function prop_period_fun()
## 2) Plot proportion over time (once aggregated and separated)
##    => plot_prop_period()
## 
## Function prop_period_fun()
## 1) filters the data by filter_crit
## 2) calls function prop_table to get aggregated proportion table
## 3) calls function prop_table to get separated proportion table
##
## Function plot_prop_period()
## 1) plots separated proportion table
## 2) plots aggregated proportion table
##
##
## There are some additional plotting function for specific plots
## plot_hdm() to plot HDM data
## barplot_by() to plot separated barplot (response not only success/failure)
## barplot_agg() to plot aggregated barplot
##
##
## Make sure your working directory contains the following folders
## - 01_data/
## - 02_R/
## - 03_fig/
## - 04_csv/




# Helper functions --------------------------------------------------------



#' Get all Messperiode levels
#' 
#' Function to get all Messperiode levels. Messperiode is character string
#' in the form 20YY-X. Function makes sure all levels between min and max
#' are included
#' @param x Character vector for which we want to have all Messperiode levels.
#' @returns Character vector with all levels
year_full_levels <- function(x){
  x <- as.character(x)
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



#' Read in excel data, only include last n years
#' 
#' @param n_year Number of years to include
read_excel_n <- function(path, n_year = 5, na = c("NA", "N/A")){
  read_excel(path = path, na = na) %>%
    # only include last n years
    filter(Messperiode %in% rev(year_full_levels(Messperiode))[1:(n_year*2)])
}





#' Binomial confidence interval
#' 
#' Returns confidence interval (CI) for binomial proportion
#' @param x number of sucesses
#' @param n number of trials
#' @param l which CI bound to return (1 = lower bound, 2 = upper bound)
binom_CI <- function(x, n, l) {
  if(is.na(n) | is.na(x) | n < x | n == 0){
    NA
  } else {
    binom.test(x, n, alternative =  "two.sided")$conf.int[l]
  }
}


#' Poisson confidence interval
#' 
#' Returns confidence interval (CI) for poisson rate parameter
#' @param x number of sucesses
#' @param n number of trials
#' @param l which CI bound to return (1 = lower bound, 2 = upper bound)
poiss_CI <- function(x, t, l){
  if(is.na(t) | is.na(x) | t == 0){
    NA
  } else {
    survival::cipoisson(k = x, time = t)[l]
  }
}






# Proportion tables -------------------------------------------------------



#' Proportion Table
#' 
#' Function to get proportion table
#' @param dat data frame with raw data
#' @param response character, indicates the response column in \code{dat}
#' @param by character, proportions will be calculated separately for each 
#' combination of \code{Messperiode} and \code{by}. Can also be \code{NULL},
#' then proportions are calculated aggregated for each \code{Messperiode}
#' @param full_grid data frame which contains all possible level combinations
#' of \code{Messperiode} and (optionally) \code{by}
prop_table <- function(dat, response, by = NULL, full_grid){
  dat %>% 
    group_by(Messperiode, across(any_of(by))) %>% 
    summarize(n = sum(!is.na(.data[[response]])),
              n_correct = sum(.data[[response]] == 1, na.rm = TRUE),
              n_wrong = sum(.data[[response]] == 0, na.rm = TRUE)) %>% 
    mutate(percent_correct = n_correct/n) %>% 
    rowwise() %>% 
    mutate(CI_lo = binom_CI(n_correct, n, 1),
           CI_up = binom_CI(n_correct, n, 2)) %>% 
    ungroup() %>% 
    right_join(full_grid) %>% # Make sure that all combinations of Messperiode
                              # and "by" variable are included in output
    arrange_at(c("Messperiode", by)) %>% 
    mutate(n = ifelse(is.na(n), 0, n))
}




#' Rate and proportion table for nvHAP data
#' 
#' @param dat data frame with raw data
#' @param response character, indicates the response column in \code{dat}
#' @param by character, proportions will be calculated separately for each 
#' combination of \code{Messperiode} and \code{by}. Can also be \code{NULL},
#' then proportions are calculated aggregated for each \code{Messperiode}
#' @param full_grid data frame which contains all possible level combinations
#' of \code{Messperiode} and (optionally) \code{by}
#' @param patPerOe data frame which contains \code{Messperiode}, \code{by}
#' variable, \code{n} (Austritte) and \code{VWD} (1000 Patiententage).
#' @param rate logical, TRUE calculates a rate (cases per 1000 Patiententage).
rate_table <- function(dat, response, by = NULL, full_grid,
                       patPerOe, rate = FALSE){
  dat <- dat %>% 
    group_by(Messperiode, across(any_of(by))) %>% 
    summarize(n_correct = sum(.data[[response]] == "Ja", na.rm = TRUE)) %>% 
    left_join(patPerOe)
  if(rate){ # calculate Poisson rate and CI (Infektionen/1000Patiententage)
    dat %>% 
      mutate(percent_correct = n_correct/(VWD/1000)) %>% 
      rowwise() %>% 
      mutate(CI_lo = poiss_CI(n_correct, VWD/1000, 1),
             CI_up = poiss_CI(n_correct, VWD/1000, 2)) %>% 
      ungroup() %>% 
      right_join(full_grid) %>% 
      arrange_at(c("Messperiode", by)) %>% 
      mutate(n = ifelse(is.na(n), 0, n))
  } else { # Calculates Binomial proportion and CI (Infektionen/Austritte)
    dat %>% 
      mutate(percent_correct = n_correct/n) %>% 
      rowwise() %>% 
      mutate(CI_lo = binom_CI(n_correct, n, 1),
             CI_up = binom_CI(n_correct, n, 2)) %>% 
      ungroup() %>% 
      right_join(full_grid) %>% 
      arrange_at(c("Messperiode", by)) %>% 
      mutate(n = ifelse(is.na(n), 0, n))
  }
}




#' Proportion table (aggregated and separated by second variable)
#' 
#' Function to get list of proportion table, once aggregated per 
#' \code{Messperiode} and once separated by variable \code{by}.
#' @param dat data frame with raw data
#' @param response character, indicates the response column in \code{dat}
#' @param by character, proportions will be calculated once aggregated per
#' \code{Messperiode} and once separately for each 
#' combination of \code{Messperiode} and \code{by}. Can also be \code{NULL},
#' then proportions are only calculated aggregated for each \code{Messperiode}.

#' @param filter_crit character, column name. Removes all rows where the value
#' is not "Ja"
#' @param binomial logical, set FALSE for nvHAP calculation.
#' @param patPerOe data frame for nvHAP calculations. Contains
#' \code{Messperiode}, \code{by} variable, \code{n} (Austritte) and
#' \code{VWD} (1000 Patiententage).
#' @param rate logical, TRUE calculates a rate (cases per 1000 Patiententage,
#' only for nvHAP)
#' @returns Returns list of two proportion tables (once aggregated and
#' once separated by variable \code{by}) or (if \code{by = NULL}) single
#' aggregated proportion table.
prop_period_fun <- function(dat, response, by = NULL, filter_crit = NULL,
                            binomial = TRUE, patPerOe = NULL, rate = FALSE){
  if(!is.null(filter_crit)){
    dat <- dat %>% filter(.data[[filter_crit]] == "Ja")
  }
  small_grid <- tibble(Messperiode = year_full_levels(dat$Messperiode))
  if(binomial){ # not nvHAP
    out <- dat_agg <- prop_table(dat = dat, response = response,
                                 by = NULL, full_grid = small_grid)
  } else { # nvHAP
    ag_patPerOe <- patPerOe %>% 
      group_by(Messperiode) %>% 
      summarise(n = sum(n, na.rm = TRUE),
                VWD = sum(VWD, na.rm = TRUE))
    out <- dat_agg <- rate_table(dat = dat, response = response,
                                 by = NULL, full_grid = small_grid,
                                 patPerOe = ag_patPerOe, rate = rate)
  }
  if(!is.null(by)){ # if there is a by variable
    full_grid <- expand_grid(year_full_levels(dat$Messperiode),
                             unique(na.omit(dat[[by]])))
    colnames(full_grid) <- c("Messperiode", by)
    if(binomial){ # not nvHAP
      dat_by <- prop_table(dat = dat, response = response,
                           by = by, full_grid = full_grid)
    } else { # nvHAP
      dat_by <- rate_table(dat = dat, response = response,
                           by = by, full_grid = full_grid,
                           patPerOe = patPerOe, rate = rate)
    }
    out <- list(dat_by = dat_by, dat_agg = dat_agg)
  }
  out
}



# Plot functions ----------------------------------------------------------



#' Normalized plot coordinates to absolute plot coordinates
#' 
#' @param ylim numeric vector with two elements giving the lower and upper
#' limit of the plot coordinates
#' @param p numeric, normalized coordinate (0 = lower limit, 1 = upper limit)
#' @returns absolute plot coordinates
rp <- function(ylim, p){ ylim[1] + diff(ylim)*p }



#' Make output directories
#' 
#' Creates directories if they do not exist yet
#' @param paths a character vector containing path names
mk_output_dir <- function(paths){
  lapply(paths, function(x) dir.create(x, showWarnings = FALSE))
}




#' Plot separated proportion table
#' 
#' @param dat output of \code{prop_period_fun()}
#' @param by character, \code{by} variable used in \code{prop_period_fun()}
#' @param fileprefix filename prefix of output (png and csv)
#' @param width2 numeric, width of png output
#' @param height2 numeric, height of png output
#' @param n_min numeric, minimum number of observations such that estimated
#' proportion and CI are included in plot.
#' @param n_min_by integer, minimum number of observations in most recent
#' period such that subplot is included 
#' @param gray_area logical, if USZ average should be included as gray area
#' @param ncols integer, number of columns for facets (subplots)
#' @param ylim numeric vector with 2 elements, y axis limits
#' @param ylab character, y axis title
#' @param y_perc logical, should y-axis be in percent
#' @param title character, plot title
#' @returns Object of class gg. Also saves png and csv in corresponding
#' output folder
plot_by <- function(dat, by = "Station", fileprefix, 
                    width2 = 16, height2 = 20,
                    n_min = 10, n_min_by = 0, gray_area = TRUE,
                    ncols = 2, ylim = c(0,1),
                    ylab = "Anteil korrekte Beobachtungen",
                    y_perc = FALSE, title = NULL){
  # at least 1 Messperiode with n_min observations
  to_include_n_min <- dat[[1]] %>% 
    mutate(point_plotted = n >= n_min) %>% 
    group_by(.data[[by]]) %>% 
    summarise(include = sum(point_plotted) > 1) %>% 
    filter(include) %>% 
    pull(by)
  # at least n_min_by observations in most recent period
  max_per <- max(as.character(dat[[1]]$Messperiode))
  to_include_by <- dat[[1]] %>%
    filter(Messperiode == max_per, n >= n_min_by) %>%
    pull(.data[[by]])
  # filter by above conditions
  dat[[1]] <- dat[[1]] %>% 
    filter(.data[[by]] %in% intersect(to_include_n_min, to_include_by))
  # change scale arguments if y should be in percent
  if(y_perc){scale <- 100; suffix_t <- "%"} else {scale <- 1; suffix_t <- ""}
  # define some colors
  cols1 <- c("Spezifische Abteilung" = "black")
  cols2 <- c("USZ Durchschnitt" = "lightgray")
  # plot
  p2 <- dat[[1]] %>% 
    mutate(across(all_of(c("percent_correct", "CI_lo", "CI_up")),
                  ~ ifelse(n < n_min, NA, .x))) %>% 
    ggplot(aes(x = Messperiode, y = percent_correct, group = 1)) +
    {if(gray_area) geom_area(data = dat[[2]],
                             aes(x = Messperiode, y = percent_correct,
                                 fill = "USZ Durchschnitt"))} +
    geom_errorbar(aes(ymin = CI_lo, ymax = CI_up), width = 0.1,
                  col = "darkgray") +
    geom_point(aes(col = "Spezifische Abteilung")) +
    geom_line(aes(col = "Spezifische Abteilung")) +
    theme_bw() +
    {if(gray_area) theme(legend.position = "bottom")} +
    {if(!gray_area) theme(legend.position="none")} +
    scale_y_continuous(breaks = pretty(ylim, n = 3),
                       labels = label_percent(scale = scale,
                                              suffix = suffix_t)) +
    coord_cartesian(ylim = c(rp(ylim, -.3), ylim[2])) +
    geom_text(aes(label = n, y = rp(ylim, -.2)),
              angle = 90, size = 2, hjust = 1) +
    annotate(geom="text", x= 0.8, y= rp(ylim, -.1), label="Gesamt N",
             hjust = 0, size = 2.5) +
    facet_rep_wrap(~ .data[[by]], ncol = ncols, repeat.tick.labels = 'x') +
    ylab(ylab) +
    scale_colour_manual(name = "",values = cols1) +
    scale_fill_manual(name = "", values = cols2) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(title)
  mk_output_dir(c("03_fig", "04_csv"))
  write_csv(dat[[1]], file = paste0("04_csv/", fileprefix, "_by.csv"))
  ggsave(paste0("03_fig/",fileprefix, "_by.png"), p2,
         width = width2, height = height2,
         units = "cm", scale = 1.5)
  p2
}






#' Plot aggregated proportion table
#' 
#' @param dat output of \code{prop_period_fun()} (only second element of list
#' if argument \code{by} was not set to \code{NULL})
#' @param fileprefix filename prefix of output (png and csv)
#' @param suffix filename suffix of output (png and csv)
#' @param width1 numeric, width of png output
#' @param height1 numeric, height of png output
#' @param n_min numeric, minimum number of observations such that estimated
#' proportion and CI are included in plot.
#' @param ylim numeric vector with 2 elements, y axis limits
#' @param ylab character, y axis title
#' @param y_perc logical, should y-axis be in percent
#' @param title character, plot title
#' @returns Object of class gg. Also saves png and csv in corresponding
#' output folder
plot_agg <- function(dat, fileprefix, suffix = "",
                     width1 = 8, height1 = 6, n_min = 10, 
                     ylim = c(0,1), ylab = "Anteil korrekte Beobachtungen",
                     y_perc = FALSE, title = NULL){
  if(y_perc){scale <- 100; suffix_t <- "%"} else {scale <- 1; suffix_t <- ""}
  p1 <- dat %>% 
    mutate(across(all_of(c("percent_correct", "CI_lo", "CI_up")),
                  ~ ifelse(n < n_min, NA, .x))) %>% 
    ggplot(aes(x = Messperiode, y = percent_correct, group = 1,
               label = round(percent_correct, 2))) +
    geom_errorbar(aes(ymin = CI_lo, ymax = CI_up),
                  width = 0.1, col = "darkgray") +
    geom_point() +
    geom_text(aes(label = scales::label_percent(
      scale = scale, suffix = suffix_t, accuracy = 0.01)(percent_correct)),
              hjust=-.3, vjust=-.3, size = 2.5) +
    geom_line() +
    theme_bw() +
    scale_y_continuous(breaks = pretty(ylim, n = 3),
                       labels = label_percent(scale = scale,
                                              suffix = suffix_t)) +
    coord_cartesian(ylim = c(rp(ylim, -.3), ylim[2])) +
    geom_text(aes(label = n, y = rp(ylim, -.2)),
              angle = 90, size = 2, hjust = 1) +
    annotate(geom="text", x= 0.8, y= rp(ylim, -.1), label="Gesamt N",
             hjust = 0, size = 2.5) +
    ylab(ylab) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(title) + theme(plot.title = element_text(size = 12))
  mk_output_dir(c("03_fig", "04_csv"))
  write_csv(dat, file = paste0("04_csv/", fileprefix, suffix, ".csv"))
  ggsave(paste0("03_fig/", fileprefix, suffix, ".png"), p1,
         width = width1, height = height1,
         units = "cm", scale = 1.5)
  p1
}
  



#' Plot output of \code{prop_period_fun()}
#' 
#' @param prop_period output of \code{prop_period_fun()}
#' @param by character, \code{by} variable used in \code{prop_period_fun()}
#' @param fileprefix filename prefix of output (png and csv)
#' @param width1 numeric, width of aggregated png output
#' @param height1 numeric, height of aggregated png output
#' @param width2 numeric, width of separated png output
#' @param height2 numeric, height of separated png output
#' @param n_min numeric, minimum number of observations such that estimated
#' proportion and CI are included in plot.
#' @param gray_area logical, if USZ average should be included as gray area
#' @param ncols integer, number of columns for facets (subplots)
#' @param ylim numeric vector with 2 elements, y axis limits
#' @param ylab character, y axis title
#' @param y_perc logical, should y-axis be in percent
#' @param title character, plot title
#' @returns list containing two objects of class gg. Also saves png and csv
#' in corresponding output folder
plot_prop_period <- function(prop_period, by = "Station", fileprefix,
                             width1 = 8, height1 = 6,
                             width2 = 16, height2 = 20,
                             n_min = 10, n_min_by = 0, gray_area = TRUE,
                             ncols = 2, ylim = c(0,1),
                             ylab = "Anteil korrekte Beobachtungen",
                             y_perc = FALSE, title = NULL){
  p2 <- NULL
  suffix <- ""
  ag_table <- prop_period
  if(class(prop_period)[1] == "list"){
    p2 <- plot_by(dat = prop_period, fileprefix = fileprefix,
                  by = by, n_min = n_min,
                  ncols = ncols, n_min_by = n_min_by, gray_area = gray_area,
                  width2 = width2, height2 = height2,
                  ylim = ylim, ylab = ylab, y_perc = y_perc)
    suffix <- "_agg"
    ag_table <- prop_period[[2]]
  }
  p1 <- plot_agg(dat = ag_table, fileprefix = fileprefix,
                 n_min = n_min, width1 = width1, height1 = height1,
                 title = title, suffix = suffix,
                 ylim = ylim, ylab = ylab, y_perc = y_perc)
  list(p1, p2)
}



# HDM ---------------------------------------------------------------------


#' Plot HDM data
#' 
#' @param dat data frame with raw data
#' @param by character, plot will be splitted/facetted by this variables
#' @param filename filename of output (png) without filename extension
#' @param filter_level character, column name used for filtering
#' @param filter_by character, only rows where \code{filter_level} takes
#' this value will be included
#' @returns object of class gg. Also saves png in corresponding output folder
plot_hdm <- function(dat = dat_station,
                     by = "Station",
                     filter_by = "Intermediate-Care-Stationen (IMC)",
                     filter_level = "Hauptbereich", 
                     filename){
  dat <- dat %>% 
    filter(.data[[filter_level]] == filter_by)
  p <- dat %>% 
    ggplot(aes(x = Messperiode, y = desinf_per_careday, group = 1,
               label = round(desinf_per_careday))) +
    geom_point() +
    geom_line() +
    geom_text(hjust=0, vjust=-1.3, size = 2.5) +
    ylab("Desinfektionsmittel/Pflegetag [ml/t]") +
    facet_rep_wrap(~ .data[[by]], repeat.tick.labels = 'x') +
    # facet_wrap(~ .data[[by]]) +
    theme_bw() + 
    scale_y_continuous(expand = expansion(mult = c(0.1,0.4))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  mk_output_dir("03_fig")
  ggsave(filename = paste0("03_fig/", filename, ".png"), plot = p,
         width = 16, height = 9, units = "cm", scale = 1.5)
  p
}







# Barplots ----------------------------------------------------------------




#' Aggregated barplot
#' 
#' @param dat data frame with raw data
#' @param response character, indicates the response column in \code{dat}
#' @param fileprefix filename prefix of output (png)
#' @returns object of class gg. Also saves png
#' in corresponding output folder
barplot_agg <- function(dat, response, fileprefix){
  dat <- dat %>%
    filter(!is.na(.data[[response]])) %>%
    mutate(Messperiode = factor(Messperiode,
                                levels = year_full_levels(dat$Messperiode)))
  dat_agg <- dat %>%
    group_by(Messperiode, .drop = FALSE) %>%
    summarise(n = n())
  response <- sym(response)
  ag_bar <- dat %>%
    ggplot(aes(x = Messperiode)) +
    geom_bar(aes(fill = !!response), position = "fill", width = 0.5) +
    scale_x_discrete(drop = FALSE) +
    theme_bw() +
    ylab("Anteil Beobachtungen") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_y_continuous(breaks = seq(0,1,0.25), limits = c(-.3,1)) +
    geom_text(data = dat_agg, aes(label = n, y = -.2, x = Messperiode),
              # inherit.aes = FALSE,
              angle = 90, size = 2, hjust = 1) +
    annotate(geom="text", x= 0.8, y=-.1, label="Gesamt N",
             hjust = 0, size = 2.5) +
    scale_fill_brewer(palette = "Set2")
  mk_output_dir("03_fig")
  ggsave(paste0("03_fig/", fileprefix, "_bar_agg.png"), plot = ag_bar,
         width = 16, height = 8, units = "cm", scale = 1.5)
  ag_bar
}





#' Separated barplot
#' 
#' @param dat data frame with raw data
#' @param response character, indicates the response column in \code{dat}
#' @param by character, proportions will be calculated separately for each 
#' combination of \code{Messperiode} and \code{by}. Can also be \code{NULL},
#' then proportions are calculated aggregated for each \code{Messperiode}
#' @param fileprefix filename prefix of output (png)
#' @param n_min_by numeric, minimum number of observations in most recent period
#' such that subplot is included 
#' @param gray_area_dat data frame containing the USZ average data
#' (columns Messperiode and percent_correct)
#' @returns object of class gg. Also saves png
#' in corresponding output folder
barplot_by <- function(dat, response, by, fileprefix, n_min_by = 10,
                       gray_area_dat){
  dat <- dat %>%
    filter(!is.na(.data[[response]])) %>%
    mutate(Messperiode = factor(Messperiode,
                                levels = year_full_levels(dat$Messperiode)))
  dat_agg <- dat %>%
    group_by(Messperiode, across(any_of(by)), .drop = FALSE) %>%
    summarise(n = n())
  
  # only include subplots which have at least n_min_by observations in most
  # recent period
  max_per <- max(as.character(dat$Messperiode))
  to_include_by <- dat_agg %>%
    filter(Messperiode == max_per, n >= n_min_by) %>% pull(.data[[by]])
  dat2 <- dat %>% filter(.data[[by]] %in% to_include_by)
  dat_agg <- dat_agg %>% filter(.data[[by]] %in% to_include_by)
  
  response <- sym(response)
  cols2 <- c("USZ Durchschnitt" = "lightgray")
  by_bar <- dat2 %>%
    ggplot(aes(x = Messperiode)) +
    geom_area(data = gray_area_dat,
              mapping = aes(x = Messperiode, y = percent_correct, group = 1,
                            fill = "USZ Durchschnitt")) + 
    scale_fill_manual(name = "", values = cols2) +
    new_scale_fill() +
    geom_bar(aes(fill = !!response), position = "fill", width = 0.5) +
    scale_x_discrete(drop = FALSE) +
    theme_bw() +
    ylab("Anteil Beobachtungen") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_y_continuous(breaks = seq(0,1,0.25), limits = c(-.3,1)) +
    geom_text(data = dat_agg, aes(label = n, y = -.2, x = Messperiode),
              inherit.aes = FALSE, angle = 90, size = 2, hjust = 1) +
    annotate(geom="text", x= 0.8, y=-.1, label="Gesamt N",
             hjust = 0, size = 2.5) +
    facet_rep_wrap(eval(expr(~!!ensym(by))), ncol = 3, repeat.tick.labels = 'x') +
    scale_fill_brewer(palette = "Set2", guide = guide_legend(order = 1))
  mk_output_dir("03_fig")
  ggsave(paste0("03_fig/", fileprefix, "_bar_by.png"), plot = by_bar,
         width = 16, height = 20, units = "cm", scale = 1.5)
  by_bar
}