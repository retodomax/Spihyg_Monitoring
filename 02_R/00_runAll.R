#' ---
#' project: Zivi2022R    ##################################################
#' title:   Make all plots
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2022-07-18 16:11:49
#' output:  github_document   #############################################
#' ---



# packages ----------------------------------------------------------------

## loads the necessary packages or installs them if they are not found
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, magrittr, tidyverse, lemon, ggnewscale, scales, survival)



# run all scripts ---------------------------------------------------------

source("02_R/00_fun.R")
source("02_R/01_monitoring_VAP5.R")
source("02_R/02_monitoring_Cauti.R")
source("02_R/03_monitoring_HDM.R")
source("02_R/04_monitoring_Clabsi.R")
source("02_R/05_ErschBild.R")
source("02_R/06_monitoring_ssi.R")
source("02_R/07_monitoring_nvHAP.R")
# source("02_R/")
