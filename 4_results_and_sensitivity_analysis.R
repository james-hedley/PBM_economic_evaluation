# Project: Run economic evaluation and sensitivity analyses
# Created by: James Hedley
# Date created: 8th November 2021
# Last updated: 22nd June 2022

# Load libraries ----


# Set directories ----
user <- Sys.getenv("USERNAME")
if (user == 'jhed6231') {user <- paste0(user, '.MCS')}

paper3loc <- paste0('C:/Users/', user, '/My Drive/Education/USYD - PhD/Paper 3 PBT kidney donors economic evaluation/')
inputsloc <- file.path(paper3loc, 'Model inputs')
resultsloc <- file.path(paper3loc, 'Model results')


# Load the statistical inputs
load(file.path(inputsloc, 'statistical_inputs.RData'))

# Load the structure of the eocnomic model ----
source(file.path(paper3loc, '3_economic_model.R'))


# Base case ----
n_patients <- 1500
loops <- 10000

set.seed(12345)
seeds <- floor(runif(loops) * 999999999)
  
evens <- (1:loops)[1:loops %% 2 == 0]
odds <- (1:loops)[1:loops %% 2 == 1]

for (loop in 1:loops) {
    
  ## Display progress
  print(paste0("Loop ", loop, " of ", loops))
  
  ## Simulate transitions
  set.seed(seeds[loop])
  results <- run_model(n_patients = n_patients, 
                       display_progress = FALSE,
                       seeds = floor(runif(n_patients) * 999999999),
                       time_horizon = 25, 
                       cycle_length = 3/12,
                       scenarios = 4,
                       extra_donors_pcts = c(0, 1/340, 2/340, 7/340),
                       baseline_risk = 0.02,
                       new_risks = c(0.02,
                                     mean(c(0.02)),
                                     mean(c(0.001, 0.02)),
                                     mean(c(0.001, 0.02, rep(0.064, 5)))),
                       transmission_cycles = 3,
                       instant_death = 0,
                       utility_method = "Multiply")

      ## Save results
      save(file = file.path(resultsloc, paste0("individual_results_", loop, ".RData")),
           results)
      
} # End of loop



# Probabilistic sensitivity analysis ----
n_patients <- 1500
loops <- 10000

set.seed(12345)
seeds <- floor(runif(loops) * 999999999)

ones <- (1:loops)[1:loops %% 3 == 1]
twos <- (1:loops)[1:loops %% 3 == 2]
threes <- (1:loops)[1:loops %% 3 == 0]
  
for (loop in 1:loops) {
  
  ## Display progress
  print(paste0("Loop ", loop, " of ", loops))
  
  ## Set seeds
  set.seed(seeds[loop])
  patient_seeds <- floor(runif(n_patients) * 999999999)
  
  ## Randomly select parameters
  discount_costs <- runif(1) * 0.05 * 2 
  discount_qalys <- runif(1) * 0.05 * 2 
  
  extra_donors_pcts <- c(0, 
                         runif(1) * (1/340) * 2,
                         runif(1) * (2/340) * 2,
                         runif(1) * (7/340) * 2)
  
  new_risks <- c(0.02,
                 runif(1) * mean(c(0.02)) * 2,
                 runif(1) * mean(c(0.001, 0.02)) * 2,
                 runif(1) * mean(c(0.001, 0.02, rep(0.064, 5))) * 2)
  
  utility_method <- sample(x = c("Floor", "Multiply", "Add"), 
                           size = 1, 
                           prob = c(1/3, 1/3, 1/3))
  
  load_inputs(psa_stats = TRUE, 
              psa_costs = TRUE, 
              psa_utilities = TRUE)
  
  ## Simulate transitions
  results <- run_model(n_patients = n_patients, 
                       display_progress = FALSE,
                       seeds = patient_seeds,
                       time_horizon = 25, 
                       cycle_length = 3/12,
                       scenarios = 4,
                       extra_donors_pcts = extra_donors_pcts,
                       baseline_risk = 0.02,
                       new_risks = new_risks,
                       transmission_cycles = 3,
                       instant_death = 0,
                       utility_method = "Multiply")
  
  psa_results <- results %>% 
    dplyr::select(starts_with("incremental_costs"), starts_with("incremental_qalys")) %>%
    summarise_all(sum)
  
  psa_results$discount_costs <- discount_costs
  psa_results$discount_qalys <- discount_qalys
  
  psa_results$extra_donors_pct1 <- extra_donors_pcts[2]
  psa_results$extra_donors_pct2 <- extra_donors_pcts[3]
  psa_results$extra_donors_pct3 <- extra_donors_pcts[4]
  
  psa_results$prob_transmission1 <- new_risks[2]
  psa_results$prob_transmission2 <- new_risks[3]
  psa_results$prob_transmission3 <- new_risks[4]
  
  psa_results$utility_method <- utility_method
  
  psa_results$coef_prevtxcount_int <- coef_prevtxcount_int
  
  psa_results$coef_bldgrp_bldgrpa_int <- coef_bldgrp_bldgrpa_int
  psa_results$coef_bldgrp_bldgrpa_prevtxcount <- coef_bldgrp_bldgrpa_prevtxcount
  
  psa_results$coef_bldgrp_bldgrpb_int <- coef_bldgrp_bldgrpb_int
  psa_results$coef_bldgrp_bldgrpb_prevtxcount <- coef_bldgrp_bldgrpb_prevtxcount
  
  psa_results$coef_bldgrp_bldgrpab_int <- coef_bldgrp_bldgrpab_int
  psa_results$coef_bldgrp_bldgrpab_prevtxcount <- coef_bldgrp_bldgrpab_prevtxcount
  
  psa_results$coef_female_int <- coef_female_int
  psa_results$coef_female_bldgrpa <- coef_female_bldgrpa
  psa_results$coef_female_bldgrpb <- coef_female_bldgrpb
  psa_results$coef_female_bldgrpab <- coef_female_bldgrpab
  psa_results$coef_female_prevtxcount <- coef_female_prevtxcount
  
  psa_results$coef_agewtlst_int <- coef_agewtlst_int
  psa_results$coef_agewtlst_bldgrpa <- coef_agewtlst_bldgrpa
  psa_results$coef_agewtlst_bldgrpb <- coef_agewtlst_bldgrpb
  psa_results$coef_agewtlst_bldgrpab <- coef_agewtlst_bldgrpab
  psa_results$coef_agewtlst_female <- coef_agewtlst_female
  psa_results$coef_agewtlst_prevtxcount <- coef_agewtlst_prevtxcount
  
  psa_results$coef_comorbs_int <- coef_comorbs_int
  psa_results$coef_comorbs_bldgrpa <- coef_comorbs_bldgrpa
  psa_results$coef_comorbs_bldgrpb <- coef_comorbs_bldgrpb
  psa_results$coef_comorbs_bldgrpab <- coef_comorbs_bldgrpab
  psa_results$coef_comorbs_female <- coef_comorbs_female
  psa_results$coef_comorbs_prevtxcount <- coef_comorbs_prevtxcount
  psa_results$coef_comorbs_agewtlst <- coef_comorbs_agewtlst
  
  psa_results$timetodtx_shape <- timetodtx_shape
  psa_results$timetodtx_scale <- timetodtx_scale
  psa_results$coef_timetodtx_bldgrpa <- coef_timetodtx_bldgrpa
  psa_results$coef_timetodtx_bldgrpb <- coef_timetodtx_bldgrpb
  psa_results$coef_timetodtx_bldgrpab <- coef_timetodtx_bldgrpab
  psa_results$coef_timetodtx_female <- coef_timetodtx_female
  psa_results$coef_timetodtx_agewtlst <- coef_timetodtx_agewtlst
  psa_results$coef_timetodtx_comorbs <- coef_timetodtx_comorbs
  psa_results$coef_timetodtx_prevtxcount <- coef_timetodtx_prevtxcount
  
  psa_results$timetoltx_mu <- timetoltx_mu
  psa_results$timetoltx_sigma <- timetoltx_sigma
  psa_results$timetoltx_Q <- timetoltx_Q
  psa_results$timetoltx_P <- timetoltx_P
  psa_results$coef_timetoltx_bldgrpa <- coef_timetoltx_bldgrpa
  psa_results$coef_timetoltx_bldgrpb <- coef_timetoltx_bldgrpb
  psa_results$coef_timetoltx_bldgrpab <- coef_timetoltx_bldgrpab
  psa_results$coef_timetoltx_female <- coef_timetoltx_female
  psa_results$coef_timetoltx_agewtlst <- coef_timetoltx_agewtlst
  psa_results$coef_timetoltx_comorbs <- coef_timetoltx_comorbs
  psa_results$coef_timetoltx_prevtxcount <- coef_timetoltx_prevtxcount
  
  psa_results$coef_donorpbm_int <- coef_donorpbm_int
  psa_results$coef_donorpbm_donordeceased <- coef_donorpbm_donordeceased
  psa_results$coef_donorpbm_agetx <- coef_donorpbm_agetx
  psa_results$coef_donorpbm_female <- coef_donorpbm_female
  psa_results$coef_donorpbm_bldgrpa <- coef_donorpbm_bldgrpa
  psa_results$coef_donorpbm_bldgrpb <- coef_donorpbm_bldgrpb
  psa_results$coef_donorpbm_bldgrpab <- coef_donorpbm_bldgrpab
  psa_results$coef_donorpbm_comorbs <- coef_donorpbm_comorbs
  psa_results$coef_donorpbm_prevtxcount <- coef_donorpbm_prevtxcount
  
  psa_results$coef_donortype_dbdscd_int <- coef_donortype_dbdscd_int
  psa_results$coef_donortype_dbdscd_agetx <- coef_donortype_dbdscd_agetx
  psa_results$coef_donortype_dbdscd_bldgrpa <- coef_donortype_dbdscd_bldgrpa
  psa_results$coef_donortype_dbdscd_bldgrpb <- coef_donortype_dbdscd_bldgrpb
  psa_results$coef_donortype_dbdscd_bldgrpab <- coef_donortype_dbdscd_bldgrpab
  psa_results$coef_donortype_dbdscd_female <- coef_donortype_dbdscd_female
  psa_results$coef_donortype_dbdscd_comorbs <- coef_donortype_dbdscd_comorbs
  psa_results$coef_donortype_dbdscd_prevtxcount <- coef_donortype_dbdscd_prevtxcount
  psa_results$coef_donortype_dbdscd_donorpbm <- coef_donortype_dbdscd_donorpbm
  
  psa_results$coef_donortype_dbdecd_int <- coef_donortype_dbdecd_int
  psa_results$coef_donortype_dbdecd_agetx <- coef_donortype_dbdecd_agetx
  psa_results$coef_donortype_dbdecd_bldgrpa <- coef_donortype_dbdecd_bldgrpa
  psa_results$coef_donortype_dbdecd_bldgrpb <- coef_donortype_dbdecd_bldgrpb
  psa_results$coef_donortype_dbdecd_bldgrpab <- coef_donortype_dbdecd_bldgrpab
  psa_results$coef_donortype_dbdecd_female <- coef_donortype_dbdecd_female
  psa_results$coef_donortype_dbdecd_comorbs <- coef_donortype_dbdecd_comorbs
  psa_results$coef_donortype_dbdecd_prevtxcount <- coef_donortype_dbdecd_prevtxcount
  psa_results$coef_donortype_dbdecd_donorpbm <- coef_donortype_dbdecd_donorpbm
  
  psa_results$coef_donorfemale_int <- coef_donorfemale_int
  psa_results$coef_donorfemale_donortypedcd <- coef_donorfemale_donortypedcd
  psa_results$coef_donorfemale_donortypedbdscd <- coef_donorfemale_donortypedbdscd
  psa_results$coef_donorfemale_donortypedbdecd <- coef_donorfemale_donortypedbdecd
  psa_results$coef_donorfemale_donorpbm <- coef_donorfemale_donorpbm
  psa_results$coef_donorfemale_agetx <- coef_donorfemale_agetx
  psa_results$coef_donorfemale_bldgrpa <- coef_donorfemale_bldgrpa
  psa_results$coef_donorfemale_bldgrpb <- coef_donorfemale_bldgrpb
  psa_results$coef_donorfemale_bldgrpab <- coef_donorfemale_bldgrpab
  psa_results$coef_donorfemale_female <- coef_donorfemale_female
  psa_results$coef_donorfemale_comorbs <- coef_donorfemale_comorbs
  psa_results$coef_donorfemale_prevtxcount <- coef_donorfemale_prevtxcount
  
  psa_results$coef_donorage_int <- coef_donorage_int
  psa_results$coef_donorage_donorfemale <- coef_donorage_donorfemale
  psa_results$coef_donorage_donortypedcd <- coef_donorage_donortypedcd
  psa_results$coef_donorage_donortypedbdscd <- coef_donorage_donortypedbdscd
  psa_results$coef_donorage_donortypedbdecd <- coef_donorage_donortypedbdecd
  psa_results$coef_donorage_donorpbm <- coef_donorage_donorpbm
  psa_results$coef_donorage_agetx <- coef_donorage_agetx
  psa_results$coef_donorage_bldgrpa <- coef_donorage_bldgrpa
  psa_results$coef_donorage_bldgrpb <- coef_donorage_bldgrpb
  psa_results$coef_donorage_bldgrpab <- coef_donorage_bldgrpab
  psa_results$coef_donorage_female <- coef_donorage_female
  psa_results$coef_donorage_comorbs <- coef_donorage_comorbs
  psa_results$coef_donorage_prevtxcount <- coef_donorage_prevtxcount
  
  psa_results$coef_donorkdpi_int <- coef_donorkdpi_int
  psa_results$coef_donorkdpi_donorfemale <- coef_donorkdpi_donorfemale
  psa_results$coef_donorkdpi_donorage <- coef_donorkdpi_donorage
  psa_results$coef_donorkdpi_donortypedbdscd <- coef_donorkdpi_donortypedbdscd
  psa_results$coef_donorkdpi_donortypedbdecd <- coef_donorkdpi_donortypedbdecd
  psa_results$coef_donorkdpi_donorpbm <- coef_donorkdpi_donorpbm
  psa_results$coef_donorkdpi_agetx <- coef_donorkdpi_agetx
  psa_results$coef_donorkdpi_bldgrpa <- coef_donorkdpi_bldgrpa
  psa_results$coef_donorkdpi_bldgrpb <- coef_donorkdpi_bldgrpb
  psa_results$coef_donorkdpi_bldgrpab <- coef_donorkdpi_bldgrpab
  psa_results$coef_donorkdpi_female <- coef_donorkdpi_female
  psa_results$coef_donorkdpi_comorbs <- coef_donorkdpi_comorbs
  psa_results$coef_donorkdpi_prevtxcount <- coef_donorkdpi_prevtxcount
  
  psa_results$timetotxcancer_shape <- timetotxcancer_shape
  psa_results$timetotxcancer_scale <- timetotxcancer_scale
  psa_results$coef_timetotxcancer_donorage <- coef_timetotxcancer_donorage
  psa_results$coef_timetotxcancer_donorfemale <- coef_timetotxcancer_donorfemale
  psa_results$coef_timetotxcancer_donorkdpi <- coef_timetotxcancer_donorkdpi
  psa_results$coef_timetotxcancer_donortypedbdscd <- coef_timetotxcancer_donortypedbdscd
  psa_results$coef_timetotxcancer_donortypedbdecd <- coef_timetotxcancer_donortypedbdecd
  psa_results$coef_timetotxcancer_bldgrpa <- coef_timetotxcancer_bldgrpa
  psa_results$coef_timetotxcancer_bldgrpb <- coef_timetotxcancer_bldgrpb
  psa_results$coef_timetotxcancer_bldgrpab <- coef_timetotxcancer_bldgrpab
  psa_results$coef_timetotxcancer_female <- coef_timetotxcancer_female
  psa_results$coef_timetotxcancer_agetx <- coef_timetotxcancer_agetx
  psa_results$coef_timetotxcancer_comorbs <- coef_timetotxcancer_comorbs
  psa_results$coef_timetotxcancer_prevtxcount <- coef_timetotxcancer_prevtxcount
  
  psa_results$timetoltxcancer_shape <- timetoltxcancer_shape
  psa_results$timetoltxcancer_scale <- timetoltxcancer_scale
  psa_results$coef_timetoltxcancer_donorage <- coef_timetoltxcancer_donorage
  psa_results$coef_timetoltxcancer_donorfemale <- coef_timetoltxcancer_donorfemale
  psa_results$coef_timetoltxcancer_bldgrpa <- coef_timetoltxcancer_bldgrpa
  psa_results$coef_timetoltxcancer_bldgrpb <- coef_timetoltxcancer_bldgrpb
  psa_results$coef_timetoltxcancer_bldgrpab <- coef_timetoltxcancer_bldgrpab
  psa_results$coef_timetoltxcancer_female <- coef_timetoltxcancer_female
  psa_results$coef_timetoltxcancer_agetx <- coef_timetoltxcancer_agetx
  psa_results$coef_timetoltxcancer_comorbs <- coef_timetoltxcancer_comorbs
  psa_results$coef_timetoltxcancer_prevtxcount <- coef_timetoltxcancer_prevtxcount
  
  psa_results$timetoltxfail_splinegamma0 <- timetoltxfail_splinegamma[1]
  psa_results$timetoltxfail_splinegamma1 <- timetoltxfail_splinegamma[2]
  psa_results$timetoltxfail_splinegamma2 <- timetoltxfail_splinegamma[3]
  psa_results$timetoltxfail_splinegamma3 <- timetoltxfail_splinegamma[4]
  psa_results$timetoltxfail_splinegamma4 <- timetoltxfail_splinegamma[5]
  psa_results$timetoltxfail_splinegamma5 <- timetoltxfail_splinegamma[6]
  psa_results$timetoltxfail_splinegamma6 <- timetoltxfail_splinegamma[7]
  psa_results$coef_timetoltxfail_donorage <- coef_timetoltxfail_donorage
  psa_results$coef_timetoltxfail_donorfemale <- coef_timetoltxfail_donorfemale
  psa_results$coef_timetoltxfail_bldgrpa <- coef_timetoltxfail_bldgrpa
  psa_results$coef_timetoltxfail_bldgrpb <- coef_timetoltxfail_bldgrpb
  psa_results$coef_timetoltxfail_bldgrpab <- coef_timetoltxfail_bldgrpab
  psa_results$coef_timetoltxfail_female <- coef_timetoltxfail_female
  psa_results$coef_timetoltxfail_agetx <- coef_timetoltxfail_agetx
  psa_results$coef_timetoltxfail_comorbs <- coef_timetoltxfail_comorbs
  psa_results$coef_timetoltxfail_prevtxcount <- coef_timetoltxfail_prevtxcount
  
  psa_results$timetotxfail_splinegamma0 <- timetotxfail_splinegamma[1]
  psa_results$timetotxfail_splinegamma1 <- timetotxfail_splinegamma[2]
  psa_results$timetotxfail_splinegamma2 <- timetotxfail_splinegamma[3]
  psa_results$timetotxfail_splinegamma3 <- timetotxfail_splinegamma[4]
  psa_results$timetotxfail_splinegamma4 <- timetotxfail_splinegamma[5]
  psa_results$timetotxfail_splinegamma5 <- timetotxfail_splinegamma[6]
  psa_results$timetotxfail_splinegamma6 <- timetotxfail_splinegamma[7]
  psa_results$coef_timetotxfail_donortypedbdscd <- coef_timetotxfail_donortypedbdscd
  psa_results$coef_timetotxfail_donortypedbdecd <- coef_timetotxfail_donortypedbdecd
  psa_results$coef_timetotxfail_donorage <- coef_timetotxfail_donorage
  psa_results$coef_timetotxfail_donorfemale <- coef_timetotxfail_donorfemale
  psa_results$coef_timetotxfail_donorkdpi <- coef_timetotxfail_donorkdpi
  psa_results$coef_timetotxfail_bldgrpa <- coef_timetotxfail_bldgrpa
  psa_results$coef_timetotxfail_bldgrpb <- coef_timetotxfail_bldgrpb
  psa_results$coef_timetotxfail_bldgrpab <- coef_timetotxfail_bldgrpab
  psa_results$coef_timetotxfail_female <- coef_timetotxfail_female
  psa_results$coef_timetotxfail_agetx <- coef_timetotxfail_agetx
  psa_results$coef_timetotxfail_comorbs <- coef_timetotxfail_comorbs
  psa_results$coef_timetotxfail_prevtxcount <- coef_timetotxfail_prevtxcount
  
  psa_results$timetoltxcfail_mu <- timetoltxcfail_mu
  psa_results$timetoltxcfail_sigma <- timetoltxcfail_sigma
  psa_results$timetoltxcfail_Q <- timetoltxcfail_Q
  psa_results$coef_timetoltxcfail_donorage <- coef_timetoltxcfail_donorage
  psa_results$coef_timetoltxcfail_donorfemale <- coef_timetoltxcfail_donorfemale
  psa_results$coef_timetoltxcfail_bldgrpa <- coef_timetoltxcfail_bldgrpa
  psa_results$coef_timetoltxcfail_bldgrpb <- coef_timetoltxcfail_bldgrpb
  psa_results$coef_timetoltxcfail_bldgrpab <- coef_timetoltxcfail_bldgrpab
  psa_results$coef_timetoltxcfail_female <- coef_timetoltxcfail_female
  psa_results$coef_timetoltxcfail_agetx <- coef_timetoltxcfail_agetx
  psa_results$coef_timetoltxcfail_comorbs <- coef_timetoltxcfail_comorbs
  psa_results$coef_timetoltxcfail_prevtxcount <- coef_timetoltxcfail_prevtxcount
  psa_results$coef_timetoltxcfail_agetxc <- coef_timetoltxcfail_agetxc
  
  psa_results$timetotxcfail_mu <- timetotxcfail_mu
  psa_results$timetotxcfail_sigma <- timetotxcfail_sigma
  psa_results$timetotxcfail_Q <- timetotxcfail_Q
  psa_results$coef_timetotxcfail_donortypedbdscd <- coef_timetotxcfail_donortypedbdscd
  psa_results$coef_timetotxcfail_donortypedbdecd <- coef_timetotxcfail_donortypedbdecd
  psa_results$coef_timetotxcfail_donorage <- coef_timetotxcfail_donorage
  psa_results$coef_timetotxcfail_donorfemale <- coef_timetotxcfail_donorfemale
  psa_results$coef_timetotxcfail_donorkdpi <- coef_timetotxcfail_donorkdpi
  psa_results$coef_timetotxcfail_bldgrpa <- coef_timetotxcfail_bldgrpa
  psa_results$coef_timetotxcfail_bldgrpb <- coef_timetotxcfail_bldgrpb
  psa_results$coef_timetotxcfail_bldgrpab <- coef_timetotxcfail_bldgrpab
  psa_results$coef_timetotxcfail_female <- coef_timetotxcfail_female
  psa_results$coef_timetotxcfail_agetx <- coef_timetotxcfail_agetx
  psa_results$coef_timetotxcfail_comorbs <- coef_timetotxcfail_comorbs
  psa_results$coef_timetotxcfail_prevtxcount <- coef_timetotxcfail_prevtxcount
  psa_results$coef_timetotxcfail_agetxc <- coef_timetotxcfail_agetxc
  
  psa_results$cost_dialysis_m1_m3 <- cost_dialysis_m1_m3
  psa_results$cost_dialysis_m4_m12 <- cost_dialysis_m4_m12
  psa_results$cost_dialysis_m13_m24 <- cost_dialysis_m13_m24
  psa_results$cost_dialysis_m25_m36 <- cost_dialysis_m25_m36
  psa_results$cost_dialysis_m37_m48 <- cost_dialysis_m37_m48
  psa_results$cost_dialysis_m49_plus <- cost_dialysis_m49_plus
  
  psa_results$cost_tx_m1_m3 <- cost_tx_m1_m3
  psa_results$cost_tx_m4_m12 <- cost_tx_m4_m12
  psa_results$cost_tx_m13_m24 <- cost_tx_m13_m24
  psa_results$cost_tx_m25_m36 <- cost_tx_m25_m36
  psa_results$cost_tx_m37_m48 <- cost_tx_m37_m48
  psa_results$cost_tx_m49_plus <- cost_tx_m49_plus
  
  psa_results$cost_cancer_m1_m3 <- cost_cancer_m1_m3
  psa_results$cost_cancer_m4_m12 <- cost_cancer_m4_m12
  psa_results$cost_cancer_m13_m24 <- cost_cancer_m13_m24
  psa_results$cost_cancer_m25_m36 <- cost_cancer_m25_m36
  psa_results$cost_cancer_m37_m48 <- cost_cancer_m37_m48
  psa_results$cost_cancer_m49_plus <- cost_cancer_m49_plus
  
  psa_results$cost_transmission_m1_m3 <- cost_transmission_m1_m3
  psa_results$cost_transmission_m4_m12 <- cost_transmission_m4_m12
  psa_results$cost_transmission_m13_m24 <- cost_transmission_m13_m24
  psa_results$cost_transmission_m25_m36 <- cost_transmission_m25_m36
  psa_results$cost_transmission_m37_m48 <- cost_transmission_m37_m48
  psa_results$cost_transmission_m49_plus <- cost_transmission_m49_plus
  
  psa_results$cost_ltx <- cost_ltx
  psa_results$cost_dtx <- cost_dtx
  psa_results$cost_nephrectomy <- cost_nephrectomy
  
  psa_results$utility_cancer <- utility_cancer
  psa_results$utility_transmission <- utility_transmission
  psa_results$utility_dialysis <- utility_dialysis
  psa_results$utility_transplant <- utility_transplant
  
    
  ## Save results
  save(file = file.path(resultsloc, paste0("psa_results_", loop, ".RData")),
       psa_results)
  
} # End of loop




# Worst-case scenario analysis ----
loops <- 10000

for (loop in 1:loops) {
  
  ## Display progress
  print(paste0("Loop ", loop, " of ", loops))
  
  ## Open previously run results
  load(file = file.path(resultsloc, paste0("individual_results_", loop, ".RData")))
  
  ## Determine which patients need to be re-run
  rerun_patients <- results$patient[results$incremental_qalys1 != 0 | 
                                      results$incremental_qalys2 != 0 | 
                                      results$incremental_qalys3 != 0 | 
                                      results$incremental_costs1 != 0 | 
                                      results$incremental_costs2 != 0 | 
                                      results$incremental_costs3 != 0] 
                                    
  ## Re-run model on required patients only
  seeds <- results$seed[rerun_patients]
  
  n_patients <- length(rerun_patients)
  
  new_results <- run_model(n_patients = n_patients, 
                           display_progress = FALSE,
                           seeds = seeds,
                           time_horizon = 25, 
                           cycle_length = 3/12,
                           scenarios = 4,
                           extra_donors_pcts = c(0, 1/340, 2/340, 7/340),
                           baseline_risk = 0.02,
                           new_risks = c(0.02,
                                         mean(c(0.02)),
                                         mean(c(0.001, 0.02)),
                                         mean(c(0.001, 0.02, rep(0.064, 5)))),
                           transmission_cycles = 3,
                           instant_death = 1,
                           utility_method = "Multiply")
  
  new_results$patient <- results$patient[rerun_patients]
  
  worstcase_results <- bind_rows(new_results, results[-rerun_patients, ]) %>%
    arrange(patient)
  
  ## Save results
  save(file = file.path(resultsloc, paste0("worstcase_results_", loop, ".RData")),
       worstcase_results)
}



# Threshold analysis - varying transmission risk ----
n_patients <- 1500
loops <- 10000

set.seed(12345)
seeds <- floor(runif(loops) * 999999999)

ones <- (1:loops)[1:loops %% 4 == 1]
twos <- (1:loops)[1:loops %% 4 == 2]
threes <- (1:loops)[1:loops %% 4 == 3]
fours <- (1:loops)[1:loops %% 4 == 0]

for (loop in 1:loops) {
  
  ## Display progress
  print(paste0("Loop ", loop, " of ", loops))
  
  ## Simulate transitions
  set.seed(seeds[loop])
  threshold_results <- run_model(n_patients = n_patients,
                                 display_progress = FALSE,
                                 seeds = floor(runif(n_patients) * 999999999),
                                 time_horizon = 25, 
                                 cycle_length = 3/12,
                                 scenarios = 11,
                                 extra_donors_pcts = c(0, rep(7/340, 10)),
                                 baseline_risk = 0.02,
                                 new_risks = c(0.02, seq(0.05, 0.50, 0.05)),
                                 transmission_cycles = 3,
                                 instant_death = 0,
                                 utility_method = "Multiply")
  
  ## Save results
  save(file = file.path(resultsloc, paste0("threshold_results_", loop, ".RData")),
       threshold_results)
  
} # End of loop



