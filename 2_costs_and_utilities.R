# Project: Cost and utilities inputs for economic evaluation
# Created by: James Hedley
# Date created: 29th November 2021
# Last updated: 15th December 2021


# Load libraries
library(tidyverse)
library(haven)
library(readxl)

# Set directories
usyd <- '//shared.sydney.edu.au/research-data/'
anzdataloc <- paste0(usyd,'PRJ-MODUS/3 Data Management/ANZDATA useable data')

paper3loc <- 'C:/Users/james.hedley/My Drive/Education/USYD - PhD/Paper 3 PBT kidney donors economic evaluation/'
inputsloc <- paste0(paper3loc,'Model inputs/')
modelsloc <- paste0(paper3loc,'Model inputs/R models/')
dashboardloc <- paste0(paper3loc,'Dashboard/')
costsloc <- paste0(paper3loc,'Costs and utilities/')
utilitiesloc <- paste0(paper3loc,'Costs and utilities/')


# Health state costs ----
hs_costs <- read_excel(path = file.path(costsloc, "cost_inputs.xlsx"),
                       sheet = "Cost of each health state",
                       range = "B5:G11")


cost_dialysis_m1_m3_mean <- hs_costs[1, "Dialysis"] %>% as.numeric()
cost_dialysis_m4_m12_mean <- hs_costs[2, "Dialysis"] %>% as.numeric()
cost_dialysis_m13_m24_mean <- hs_costs[3, "Dialysis"] %>% as.numeric()
cost_dialysis_m25_m36_mean <- hs_costs[4, "Dialysis"] %>% as.numeric()
cost_dialysis_m37_m48_mean <- hs_costs[5, "Dialysis"] %>% as.numeric()
cost_dialysis_m49_plus_mean <- hs_costs[6, "Dialysis"] %>% as.numeric()

cost_tx_m1_m3_mean <- hs_costs[1, "Transplant"] %>% as.numeric()
cost_tx_m4_m12_mean <- hs_costs[2, "Transplant"] %>% as.numeric()
cost_tx_m13_m24_mean <- hs_costs[3, "Transplant"] %>% as.numeric()
cost_tx_m25_m36_mean <- hs_costs[4, "Transplant"] %>% as.numeric()
cost_tx_m37_m48_mean <- hs_costs[5, "Transplant"] %>% as.numeric()
cost_tx_m49_plus_mean <- hs_costs[6, "Transplant"] %>% as.numeric()

cost_cancer_m1_m3_mean <- hs_costs[1, "De-novo cancer"] %>% as.numeric()
cost_cancer_m4_m12_mean <- hs_costs[2, "De-novo cancer"] %>% as.numeric()
cost_cancer_m13_m24_mean <- hs_costs[3, "De-novo cancer"] %>% as.numeric()
cost_cancer_m25_m36_mean <- hs_costs[4, "De-novo cancer"] %>% as.numeric()
cost_cancer_m37_m48_mean <- hs_costs[5, "De-novo cancer"] %>% as.numeric()
cost_cancer_m49_plus_mean <- hs_costs[6, "De-novo cancer"] %>% as.numeric()

cost_transmission_m1_m3_mean <- hs_costs[1, "Transmitted cancer"] %>% as.numeric()
cost_transmission_m4_m12_mean <- hs_costs[2, "Transmitted cancer"] %>% as.numeric()
cost_transmission_m13_m24_mean <- hs_costs[3, "Transmitted cancer"] %>% as.numeric()
cost_transmission_m25_m36_mean <- hs_costs[4, "Transmitted cancer"] %>% as.numeric()
cost_transmission_m37_m48_mean <- hs_costs[5, "Transmitted cancer"] %>% as.numeric()
cost_transmission_m49_plus_mean <- hs_costs[6, "Transmitted cancer"] %>% as.numeric()



# Event costs ----
event_costs <- read_excel(path = file.path(costsloc, "cost_inputs.xlsx"),
                       sheet = "Cost of each transition",
                       range = "B4:C7")

cost_ltx_mean <- event_costs[1, "Cost"] %>% as.numeric()
cost_dtx_mean <- event_costs[2, "Cost"] %>% as.numeric()
cost_nephrectomy_mean <- event_costs[3, "Cost"] %>% as.numeric()


# Utilities ----
hs_utilities <- read_excel(path = file.path(utilitiesloc, "utility_inputs.xlsx"),
                           sheet = "Utility of each health state",
                           range = "B4:F8")

utility_cancer_mean <- hs_utilities[1, "Utility"] %>% as.numeric()
utility_cancer_lower <- hs_utilities[1, "Min"] %>% as.numeric()
utility_cancer_upper <- hs_utilities[1, "Max"] %>% as.numeric()

utility_transmission_mean <- hs_utilities[2, "Utility"] %>% as.numeric()
utility_transmission_lower <- hs_utilities[2, "Min"] %>% as.numeric()
utility_transmission_upper <- hs_utilities[2, "Max"] %>% as.numeric()

utility_dialysis_mean <- hs_utilities[3, "Utility"] %>% as.numeric()
utility_dialysis_se <- hs_utilities[3, "SE"] %>% as.numeric()

utility_transplant_mean <- hs_utilities[4, "Utility"] %>% as.numeric()
utility_transplant_se <- hs_utilities[4, "SE"] %>% as.numeric()


# Save costs and utilites inputs
save(file = file.path(inputsloc,'costs.RData'),
     #
     cost_dialysis_m1_m3_mean,
     cost_dialysis_m4_m12_mean,
     cost_dialysis_m13_m24_mean,
     cost_dialysis_m25_m36_mean,
     cost_dialysis_m37_m48_mean,
     cost_dialysis_m49_plus_mean,
     #
     cost_tx_m1_m3_mean,
     cost_tx_m4_m12_mean,
     cost_tx_m13_m24_mean,
     cost_tx_m25_m36_mean,
     cost_tx_m37_m48_mean,
     cost_tx_m49_plus_mean,
     #
     cost_cancer_m1_m3_mean,
     cost_cancer_m4_m12_mean,
     cost_cancer_m13_m24_mean,
     cost_cancer_m25_m36_mean,
     cost_cancer_m37_m48_mean,
     cost_cancer_m49_plus_mean,
     #
     cost_transmission_m1_m3_mean,
     cost_transmission_m4_m12_mean,
     cost_transmission_m13_m24_mean,
     cost_transmission_m25_m36_mean,
     cost_transmission_m37_m48_mean,
     cost_transmission_m49_plus_mean,
     #
     cost_ltx_mean,
     cost_dtx_mean,
     cost_nephrectomy_mean)


save(file = file.path(inputsloc,'utilities.RData'),
     utility_cancer_mean,
     utility_cancer_lower,
     utility_cancer_upper,
     #
     utility_transmission_mean,
     utility_transmission_lower,
     utility_transmission_upper,
     #
     utility_dialysis_mean,
     utility_dialysis_se,
     #
     utility_transplant_mean,
     utility_transplant_se)

