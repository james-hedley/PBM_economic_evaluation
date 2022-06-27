# Project: Model for an economic evaluation of increasing use of kidney donors with primary brain malignancy
# Created by: James Hedley
# Date created: 21st January 2021
# Last updated: 22nd June 2022


# Load libraries
library('tidyverse')
library('dplyr')
library('ggplot2')
library('MASS')
library('stringr')
library('simstudy')
library('flexsurv')
library('pbapply')
library('ggplot2')


# Disable scientific notation
options(scipen=999)


# Set directories
user <- Sys.getenv("USERNAME")
if (user == 'jhed6231') {user <- paste0(user, '.MCS')}

paper3loc <- paste0('C:/Users/', user, '/My Drive/Education/USYD - PhD/Paper 3 PBT kidney donors economic evaluation/')
inputsloc <- file.path(paper3loc, 'Model inputs')


# Create a function to load inputs ----
load_inputs <- function(psa_stats = 0, psa_costs = 0, psa_utilities = 0) {
 
 # Load inputs ----
 load(file.path(inputsloc,'statistical_inputs.RData'))
 load(file.path(inputsloc,'costs.RData'))
 load(file.path(inputsloc,'utilities.RData'))
 
 
 # Select inputs ----
 ## Select statistical inputs ----
 psa_statistical_inputs <- c(
  "coef_prevtxcount_int",
  #
  "coef_bldgrp_bldgrpa_int",
  "coef_bldgrp_bldgrpa_prevtxcount",
  #
  "coef_bldgrp_bldgrpb_int",
  "coef_bldgrp_bldgrpb_prevtxcount",
  #
  "coef_bldgrp_bldgrpab_int",
  "coef_bldgrp_bldgrpab_prevtxcount",
  #
  "coef_female_int",
  "coef_female_bldgrpa",
  "coef_female_bldgrpb",
  "coef_female_bldgrpab",
  "coef_female_prevtxcount",
  #
  "coef_agewtlst_int",
  "coef_agewtlst_bldgrpa",
  "coef_agewtlst_bldgrpb",
  "coef_agewtlst_bldgrpab",
  "coef_agewtlst_female",
  "coef_agewtlst_prevtxcount",
  #
  "coef_comorbs_int",
  "coef_comorbs_bldgrpa",
  "coef_comorbs_bldgrpb",
  "coef_comorbs_bldgrpab",
  "coef_comorbs_female",
  "coef_comorbs_prevtxcount",
  "coef_comorbs_agewtlst",
  #
  "timetodtx_shape",
  "timetodtx_scale",
  "coef_timetodtx_bldgrpa",
  "coef_timetodtx_bldgrpb",
  "coef_timetodtx_bldgrpab",
  "coef_timetodtx_female",
  "coef_timetodtx_agewtlst",
  "coef_timetodtx_comorbs",
  "coef_timetodtx_prevtxcount",
  #
  "timetoltx_mu",
  "timetoltx_sigma",
  "timetoltx_Q",
  "timetoltx_P",
  "coef_timetoltx_bldgrpa",
  "coef_timetoltx_bldgrpb",
  "coef_timetoltx_bldgrpab",
  "coef_timetoltx_female",
  "coef_timetoltx_agewtlst",
  "coef_timetoltx_comorbs",
  "coef_timetoltx_prevtxcount",
  #
  "coef_donorpbm_int",
  "coef_donorpbm_donordeceased",
  "coef_donorpbm_agetx",
  "coef_donorpbm_female",
  "coef_donorpbm_bldgrpa",
  "coef_donorpbm_bldgrpb",
  "coef_donorpbm_bldgrpab",
  "coef_donorpbm_comorbs",
  "coef_donorpbm_prevtxcount",
  #
  "coef_donortype_dbdscd_int",
  "coef_donortype_dbdscd_agetx",
  "coef_donortype_dbdscd_bldgrpa",
  "coef_donortype_dbdscd_bldgrpb",
  "coef_donortype_dbdscd_bldgrpab",
  "coef_donortype_dbdscd_female",
  "coef_donortype_dbdscd_comorbs",
  "coef_donortype_dbdscd_prevtxcount",
  "coef_donortype_dbdscd_donorpbm",
  #
  "coef_donortype_dbdecd_int",
  "coef_donortype_dbdecd_agetx",
  "coef_donortype_dbdecd_bldgrpa",
  "coef_donortype_dbdecd_bldgrpb",
  "coef_donortype_dbdecd_bldgrpab",
  "coef_donortype_dbdecd_female",
  "coef_donortype_dbdecd_comorbs",
  "coef_donortype_dbdecd_prevtxcount",
  "coef_donortype_dbdecd_donorpbm",
  #
  "coef_donorfemale_int",
  "coef_donorfemale_donortypedcd",
  "coef_donorfemale_donortypedbdscd",
  "coef_donorfemale_donortypedbdecd",
  "coef_donorfemale_donorpbm",
  "coef_donorfemale_agetx",
  "coef_donorfemale_bldgrpa",
  "coef_donorfemale_bldgrpb",
  "coef_donorfemale_bldgrpab",
  "coef_donorfemale_female",
  "coef_donorfemale_comorbs",
  "coef_donorfemale_prevtxcount",
  #
  "coef_donorage_int",
  "coef_donorage_donorfemale",
  "coef_donorage_donortypedcd",
  "coef_donorage_donortypedbdscd",
  "coef_donorage_donortypedbdecd",
  "coef_donorage_donorpbm",
  "coef_donorage_agetx",
  "coef_donorage_bldgrpa",
  "coef_donorage_bldgrpb",
  "coef_donorage_bldgrpab",
  "coef_donorage_female",
  "coef_donorage_comorbs",
  "coef_donorage_prevtxcount",
  #
  "coef_donorkdpi_int",
  "coef_donorkdpi_donorfemale",
  "coef_donorkdpi_donorage",
  "coef_donorkdpi_donortypedbdscd",
  "coef_donorkdpi_donortypedbdecd",
  "coef_donorkdpi_donorpbm",
  "coef_donorkdpi_agetx",
  "coef_donorkdpi_bldgrpa",
  "coef_donorkdpi_bldgrpb",
  "coef_donorkdpi_bldgrpab",
  "coef_donorkdpi_female",
  "coef_donorkdpi_comorbs",
  "coef_donorkdpi_prevtxcount",
  #
  "timetotxcancer_shape",
  "timetotxcancer_scale",
  "coef_timetotxcancer_donorage",
  "coef_timetotxcancer_donorfemale",
  "coef_timetotxcancer_donorkdpi",
  "coef_timetotxcancer_donortypedbdscd",
  "coef_timetotxcancer_donortypedbdecd",
  "coef_timetotxcancer_bldgrpa",
  "coef_timetotxcancer_bldgrpb",
  "coef_timetotxcancer_bldgrpab",
  "coef_timetotxcancer_female",
  "coef_timetotxcancer_agetx",
  "coef_timetotxcancer_comorbs",
  "coef_timetotxcancer_prevtxcount",
  #
  "timetoltxcancer_shape",
  "timetoltxcancer_scale",
  "coef_timetoltxcancer_donorage",
  "coef_timetoltxcancer_donorfemale",
  "coef_timetoltxcancer_bldgrpa",
  "coef_timetoltxcancer_bldgrpb",
  "coef_timetoltxcancer_bldgrpab",
  "coef_timetoltxcancer_female",
  "coef_timetoltxcancer_agetx",
  "coef_timetoltxcancer_comorbs",
  "coef_timetoltxcancer_prevtxcount",
  #
  "timetoltxfail_splinegamma",
  "coef_timetoltxfail_donorage",
  "coef_timetoltxfail_donorfemale",
  "coef_timetoltxfail_bldgrpa",
  "coef_timetoltxfail_bldgrpb",
  "coef_timetoltxfail_bldgrpab",
  "coef_timetoltxfail_female",
  "coef_timetoltxfail_agetx",
  "coef_timetoltxfail_comorbs",
  "coef_timetoltxfail_prevtxcount",
  #
  "timetoltxcfail_mu",
  "timetoltxcfail_sigma",
  "timetoltxcfail_Q",
  "coef_timetoltxcfail_donorage",
  "coef_timetoltxcfail_donorfemale",
  "coef_timetoltxcfail_bldgrpa",
  "coef_timetoltxcfail_bldgrpb",
  "coef_timetoltxcfail_bldgrpab",
  "coef_timetoltxcfail_female",
  "coef_timetoltxcfail_agetx",
  "coef_timetoltxcfail_comorbs",
  "coef_timetoltxcfail_prevtxcount",
  "coef_timetoltxcfail_agetxc",
  #
  "timetotxfail_splinegamma",
  "coef_timetotxfail_donortypedbdscd",
  "coef_timetotxfail_donortypedbdecd",
  "coef_timetotxfail_donorage",
  "coef_timetotxfail_donorfemale",
  "coef_timetotxfail_donorkdpi",
  "coef_timetotxfail_bldgrpa",
  "coef_timetotxfail_bldgrpb",
  "coef_timetotxfail_bldgrpab",
  "coef_timetotxfail_female",
  "coef_timetotxfail_agetx",
  "coef_timetotxfail_comorbs",
  "coef_timetotxfail_prevtxcount",
  #
  "timetotxcfail_mu",
  "timetotxcfail_sigma",
  "timetotxcfail_Q",
  "coef_timetotxcfail_donortypedbdscd",
  "coef_timetotxcfail_donortypedbdecd",
  "coef_timetotxcfail_donorage",
  "coef_timetotxcfail_donorfemale",
  "coef_timetotxcfail_donorkdpi",
  "coef_timetotxcfail_bldgrpa",
  "coef_timetotxcfail_bldgrpb",
  "coef_timetotxcfail_bldgrpab",
  "coef_timetotxcfail_female",
  "coef_timetotxcfail_agetx",
  "coef_timetotxcfail_comorbs",
  "coef_timetotxcfail_prevtxcount",
  "coef_timetotxcfail_agetxc")
 
 nonpsa_statistical_inputs <- c(
  "rmse_agewtlst",
  "rmse_donorage",
  "rmse_donorkdpi",
  #
  "timetoltxfail_splineknots",
  #
  "timetotxfail_splineknots",
  #
  "cancer_wtlst",
  #
  "mortality_wtlst",
  "mortality_offlstc",
  "mortality_long_offlstc",
  "mortality_tx",
  "mortality_txc",
  "mortality_long_txc",
  "mortality_tf",
  "mortality_tfc",
  "mortality_long_tfc")
 
 for(input in psa_statistical_inputs) {
  mean <- get(paste0(input, "_mean"))
  se <- get(paste0(input, "_se"))
  assign(x = input, 
         value = mean + (psa_stats * rnorm(1) * se),
         envir = globalenv())
  
  if (input %in% c("timetoltx_P", "timetodtx_scale", "timetotxcancer_scale")) {
    assign(x = input, 
           value = max(mean + (psa_stats * rnorm(1) * se), 0),
           envir = globalenv())
  }
 }
 
 for(input in nonpsa_statistical_inputs) {
  assign(x = input, 
         value = get(input),
         envir = globalenv())
 }
 
 
 ## Select cost inputs ----
 cost_inputs <- c(
  "cost_dialysis_m1_m3",
  "cost_dialysis_m4_m12",
  "cost_dialysis_m13_m24",
  "cost_dialysis_m25_m36",
  "cost_dialysis_m37_m48",
  "cost_dialysis_m49_plus",
  #
  "cost_tx_m1_m3",
  "cost_tx_m4_m12",
  "cost_tx_m13_m24",
  "cost_tx_m25_m36",
  "cost_tx_m37_m48",
  "cost_tx_m49_plus",
  #
  "cost_cancer_m1_m3",
  "cost_cancer_m4_m12",
  "cost_cancer_m13_m24",
  "cost_cancer_m25_m36",
  "cost_cancer_m37_m48",
  "cost_cancer_m49_plus",
  #
  "cost_transmission_m1_m3",
  "cost_transmission_m4_m12",
  "cost_transmission_m13_m24",
  "cost_transmission_m25_m36",
  "cost_transmission_m37_m48",
  "cost_transmission_m49_plus",
  #
  "cost_ltx",
  "cost_dtx",
  "cost_nephrectomy")
 
 for(input in cost_inputs) {
  mean <- get(paste0(input, "_mean"))
  error <- 0.15 # same as Senanayake et al., 2020 (A and B)
  assign(x = input, 
         value = mean + psa_costs * runif(1, min = -error, max = error) * mean,
         envir = globalenv())
 }
 
 
 ## Select utilities inputs
 ### Function to select alpha and beta paramaters for beta distribution
 fit_beta <- function(mean, lower = NULL, upper = NULL, se = NULL) {
  
  if((!is.null(lower) | !is.null(upper)) & !is.null(se)) {return("Only one of lower/upper or SE should be specified")}
  
  if(!is.null(lower) & !is.null(upper)) {
   
   params <- c(1, 1)
   
   objective_function <- function(params) {
    a <- params[1]
    b <- params[2]
    
    target_mean <- mean
    target_plower <- lower
    target_pupper <- upper
    
    actual_mean <- a / (a + b)
    actual_plower <- qbeta(0.0005, shape1 = a, shape2 = b)
    actual_pupper <- qbeta(0.9995, shape1 = a, shape2 = b)
    
    mean_error <- (actual_mean - target_mean)^2
    plower_error <- (actual_plower - target_plower)^2
    pupper_error <- (actual_pupper - target_pupper)^2
    
    error <- 100*mean_error + plower_error + pupper_error
    
    return(error)
   }
   
   final_params <- nlm(f = objective_function, p = params)$estimate
   
   a <- final_params[1]
   b <- final_params[2]
   
   return(c(a, b))
   
  } else {
   a <- (((mean * (1 - mean))/(se^2)) - 1) * mean
   b <- (((mean * (1 - mean))/(se^2)) - 1) * (1 - mean)
   
   return(c(a, b))
  }
 }
 
 ### Beta distribution parameters for utility of de-novo cancer
 params <- fit_beta(mean = utility_cancer_mean, 
                    lower = utility_cancer_lower, 
                    upper = utility_cancer_upper)
 a_cancer <- params[1]
 b_cancer <- params[2]
 
 ### Beta distribution parameters for utility of transmitted cancer
 params <- fit_beta(mean = utility_transmission_mean, 
                    lower = utility_transmission_lower, 
                    upper = utility_transmission_upper)
 a_transmission <- params[1]
 b_transmission <- params[2]
 
 ### Beta distribution parameters for utility of dialysis
 params <- fit_beta(mean = utility_dialysis_mean, 
                    se = utility_dialysis_se)
 a_dialysis <- params[1]
 b_dialysis <- params[2]
 
 
 ### Beta distribution parameters for utility of transplant
 params <- fit_beta(mean = utility_transplant_mean, 
                    se = utility_transplant_se)
 a_transplant <- params[1]
 b_transplant <- params[2]
 
 
 utility_inputs <- c(
  "utility_cancer",
  "utility_transmission",
  "utility_dialysis",
  "utility_transplant")
 
 for(input in utility_inputs) {
  state <- str_sub(input, 9, 999)
  mean <- get(paste0(input, "_mean"))
  a <- get(paste0("a_", state))
  b <- get(paste0("b_", state))
  assign(x = input, 
         value = ((1 - psa_utilities) * mean) + (psa_utilities * rbeta(1, shape1 = a, shape2 = b)),
         envir = globalenv())
 }
 
} # End of load_inputs function


# Load inputs ----
psa_stats <- FALSE
psa_costs <- FALSE
psa_utilities <- FALSE

load_inputs(psa_stats = psa_stats, 
            psa_costs = psa_costs, 
            psa_utilities = psa_utilities)



# Functions for mathematical operations ----
## Create functions for the logistic and inverse logistic functions
logistic <- function(x){exp(x)/(1+exp(x))}
invlogistic <- function(x){-log((1/(x))-1)}

## Define functions to convert between rates and probabilities
prob_to_rate <- function(prob,time=1) {(-log(1-prob))/time}
rate_to_prob <- function(rate,time=1) {1-exp(-(rate*time))}
prob_to_prob <- function(prob,time=1,rr=1) {1-exp(log(1-prob)*rr*time)}


# Patient characteristics ----
sim_patient <- function() {

  ## Number of previous transplants ----
  ### Maximum number
  max_prevtxcount <- 2
  
  ### Model for number of previous transplants
  f_prevtxcount <- 
   coef_prevtxcount_int
   
  ### Simulate number of comorbidities
  prevtxcount <- rpois(1, lambda = exp(replace_na(f_prevtxcount, 0)))
  
  ### Ensure number of previous transplants does not exceed the maximum possible (i.e. 2)
  prevtxcount <- pmin(prevtxcount, max_prevtxcount)
  
  
  ## Bloodgroup ----
  ### Equation for bloodgroup A
  f_bldgrpa <-
   coef_bldgrp_bldgrpa_int + 
   coef_bldgrp_bldgrpa_prevtxcount * prevtxcount
  expf_bldgrpa <- exp(f_bldgrpa)
  
  ### Equation for bloodgroup AB
  f_bldgrpb <-
   coef_bldgrp_bldgrpb_int + 
   coef_bldgrp_bldgrpb_prevtxcount * prevtxcount
  expf_bldgrpb <- exp(f_bldgrpb)
  
  ### Equation for bloodgroup AB
  f_bldgrpab <-
   coef_bldgrp_bldgrpab_int + 
   coef_bldgrp_bldgrpab_prevtxcount * prevtxcount
  expf_bldgrpab <- exp(f_bldgrpab)
  
  ### Sum of all equations
  expf_sum <- expf_bldgrpa + expf_bldgrpb + expf_bldgrpab
  
  ### Probability of each blood type
  p_bldgrpo <- as.numeric(unlist(1/(1+expf_sum)))
  p_bldgrpa <- as.numeric(unlist(expf_bldgrpa/(1+expf_sum)))
  p_bldgrpb <- as.numeric(unlist(expf_bldgrpb/(1+expf_sum)))
  p_bldgrpab <- as.numeric(unlist(expf_bldgrpab/(1+expf_sum)))
  
  ### Combine all blood group probabilities into a single matrix
  p_bldgrp <- c(p_bldgrpo, p_bldgrpa, p_bldgrpb, p_bldgrpab)
  
  ### Simulate blood group
  bldgrp <- sample(x = c('O', 'A', 'B', 'AB'),
                   size = 1,
                   prob = p_bldgrp)
  
  ### Create dummy variables for each blood group
  bldgrpo <- ((bldgrp == 'O') * 1)
  bldgrpa <- ((bldgrp == 'A') * 1)
  bldgrpb <- ((bldgrp == 'B') * 1)
  bldgrpab <- ((bldgrp == 'AB') * 1)
  
  
  ## Sex ----
  ### Model
   f_female <- 
    coef_female_int +
    coef_female_bldgrpa * bldgrpa +
    coef_female_bldgrpb * bldgrpb +
    coef_female_bldgrpab * bldgrpab +
    coef_female_prevtxcount * prevtxcount
   p_female <- logistic(f_female)
   
  ### Simulate female status
  ## Avoid using rbinom() since it will not always increment the random number seed in the same way
  ## https://stackoverflow.com/questions/18907600/erratic-seed-behavior-with-rbinomprob-0-5-in-r
  female <- ifelse(runif(1) <= replace_na(p_female, 0), 1, 0)
  
  
  ## Age at first waitlisting ----
  ### Minimum and maximum ages
  max_agewtlst = 100
  min_agewtlst = 0
  
  ### Model
  f_agewtlst <- 
   coef_agewtlst_int + 
   coef_agewtlst_female * female + 
   coef_agewtlst_bldgrpa * bldgrpa +
   coef_agewtlst_bldgrpb * bldgrpb +
   coef_agewtlst_bldgrpab * bldgrpab +
   coef_agewtlst_prevtxcount * prevtxcount
  
  ### Simulate age at waitlisting
  agewtlst <- rnorm(1, mean = f_agewtlst, sd = rmse_agewtlst)
  
  ### Restrict age to be within specified limits
  agewtlst <- pmax(pmin(agewtlst, max_agewtlst), min_agewtlst)
  
  
  ## Number of comorbidities ----
  ### Maximum number of comorbidities
  max_comorbs = 7
  
  ### Model
  f_comorbs <- 
   coef_comorbs_int + 
   coef_comorbs_agewtlst * agewtlst +
   coef_comorbs_female * female + 
   coef_comorbs_bldgrpa * bldgrpa +
   coef_comorbs_bldgrpb * bldgrpb +
   coef_comorbs_bldgrpab * bldgrpab +
   coef_comorbs_prevtxcount * prevtxcount
  
  ## Simulate number of comorbidities
  comorbs <- rpois(1, lambda = exp(f_comorbs))
                                   
  ### Ensure number of comorbidities does not exceed the maximum possible (i.e. 7)
  comorbs <- pmin(comorbs, max_comorbs)
  
  ## Return patient characteristics
  chars <- list(
    agewtlst = agewtlst,
    female = female,
    bldgrp = bldgrp,
    bldgrpo = bldgrpo,
    bldgrpa = bldgrpa,
    bldgrpb = bldgrpb,
    bldgrpab = bldgrpab,
    comorbs = comorbs,
    prevtxcount = prevtxcount)
  return(chars)
  
} # End of function to simulate patient characteristics


# Transitions from waitlist ----
trans_wtlst <- function(hs = NULL, chars = NULL, extra_donors_pct = NULL,
                        min_extra_donors_pct = NULL, max_extra_donors_pct = NULL,
                        n_cycles = NULL, cycle_length = NULL) {
  
  
  ## Extract patient characteristics
  agewtlst <- chars$agewtlst
  female <- chars$female
  bldgrp <- chars$bldgrp
  bldgrpo <- chars$bldgrpo
  bldgrpa <- chars$bldgrpa
  bldgrpb <- chars$bldgrpb
  bldgrpab <- chars$bldgrpab
  comorbs <- chars$comorbs
  prevtxcount <- chars$prevtxcount
  
  ## Set current and remaining cycles
  current_cycle <- 0
  remaining_cycles <- n_cycles
  
  ## Update cycle start times
  cycle_starttimes <- seq(0, remaining_cycles * cycle_length * 365.25, cycle_length * 365.25)
  age_cyclestart <- agewtlst + (current_cycle * cycle_length) + (cycle_starttimes[-length(cycle_starttimes)] / 365.25)
  years_cyclestart <- seq(0, (remaining_cycles - 1) * cycle_length, cycle_length)
  
  
  ## Probability living donor transplant
  mu <- timetoltx_mu +
   coef_timetoltx_agewtlst * agewtlst +
   coef_timetoltx_female * female +
   coef_timetoltx_bldgrpa * bldgrpa +
   coef_timetoltx_bldgrpb * bldgrpb +
   coef_timetoltx_bldgrpab * bldgrpab +
   coef_timetoltx_comorbs * comorbs + 
   coef_timetoltx_prevtxcount * prevtxcount
  
  cumprob <- pgenf(q = cycle_starttimes,
                   mu = mu, 
                   sigma = timetoltx_sigma, 
                   Q = timetoltx_Q, 
                   P = timetoltx_P)
  
  cumprob0 <- cumprob[-length(cumprob)] 
  cumprob1 <- cumprob[-1]
  
  tp_wtlst_ltx <- (cumprob1 - cumprob0) / (1 - cumprob0)
  
  
  ## Probability deceased donor transplant
  f_lambda <- 
   log(timetodtx_scale) + 
   coef_timetodtx_agewtlst * agewtlst + 
   coef_timetodtx_female * female + 
   coef_timetodtx_bldgrpa * bldgrpa + 
   coef_timetodtx_bldgrpb * bldgrpb + 
   coef_timetodtx_bldgrpab * bldgrpab + 
   coef_timetodtx_comorbs * comorbs +
   coef_timetodtx_prevtxcount * prevtxcount
  lambda <- exp(f_lambda)^(-1/timetodtx_shape)
  
  cumprob <- pweibull(q = cycle_starttimes, shape = timetodtx_shape, scale = lambda)
  
  cumprob0 <- cumprob[-length(cumprob)] 
  cumprob1 <- cumprob[-1]
  
  tp_wtlst_dtx <- (cumprob1 - cumprob0) / (1 - cumprob0)
  
  ### Increase in donation due to intervention
  new_tp_wtlst_dtx <- prob_to_prob(tp_wtlst_dtx, rr = (1 + extra_donors_pct))
  min_tp_wtlst_dtx <- prob_to_prob(tp_wtlst_dtx, rr = (1 + min_extra_donors_pct))
  max_tp_wtlst_dtx <- prob_to_prob(tp_wtlst_dtx, rr = (1 + max_extra_donors_pct))
  
  tp_wtlst_ndtx <- new_tp_wtlst_dtx - tp_wtlst_dtx
   
  if (min_extra_donors_pct == 0) {
    min_tp_wtlst_ndtx <- rep(0, length(tp_wtlst_dtx))
  } else {
    min_tp_wtlst_ndtx <- min_tp_wtlst_dtx - tp_wtlst_dtx
  }

  max_tp_wtlst_ndtx <- max_tp_wtlst_dtx - tp_wtlst_dtx
  
  
  ## Probability off waitlist with cancer
  rate <- sapply(1:remaining_cycles, function(x) {
   cancer_wtlst$annualrate[cancer_wtlst$age == pmin(floor(age_cyclestart), 100)[x] & 
                             cancer_wtlst$female == female]
  })
  
  tp_wtlst_offlstc <- rate_to_prob(rate, cycle_length)
  
  
  ## Probability death
  rate <- sapply(1:remaining_cycles, function(x) {
   mortality_wtlst$annualrate[mortality_wtlst$age == pmin(floor(age_cyclestart), 100)[x] & 
                               mortality_wtlst$female == female]
  })
  
  tp_wtlst_dth <- rate_to_prob(rate, cycle_length)
  
  
  ## Probability remain on waitlist
  tp_wtlst_wtlst <- 1 - tp_wtlst_ltx - tp_wtlst_dtx - tp_wtlst_ndtx - tp_wtlst_offlstc - tp_wtlst_dth
  
  max_tp_wtlst_wtlst <- 1 - tp_wtlst_ltx - tp_wtlst_dtx - max_tp_wtlst_ndtx - tp_wtlst_offlstc - tp_wtlst_dth
  min_tp_wtlst_wtlst <- 1 - tp_wtlst_ltx - tp_wtlst_dtx - min_tp_wtlst_ndtx - tp_wtlst_offlstc - tp_wtlst_dth
  
  
  ## Transitions from waitlist
  tp <- cbind(tp_wtlst_wtlst, tp_wtlst_ndtx, tp_wtlst_dtx, tp_wtlst_ltx, tp_wtlst_offlstc, tp_wtlst_dth)
  tp <- pmax(tp, 0) / rowSums(pmax(tp, 0))
  
  max_tp <- cbind(max_tp_wtlst_wtlst, max_tp_wtlst_ndtx, tp_wtlst_dtx, tp_wtlst_ltx, tp_wtlst_offlstc, tp_wtlst_dth)
  max_tp <- pmax(max_tp, 0) / rowSums(pmax(max_tp, 0))
  
  min_tp <- cbind(min_tp_wtlst_wtlst, min_tp_wtlst_ndtx, tp_wtlst_dtx, tp_wtlst_ltx, tp_wtlst_offlstc, tp_wtlst_dth)
  min_tp <- pmax(min_tp, 0) / rowSums(pmax(min_tp, 0))
  
                              
  health_state <- "wtlst"
  cycle <- 1
  resimulate <- 0
  donordeceased <- NULL
  donornew <- NULL
  while(health_state == "wtlst" & cycle <= remaining_cycles) {
   
    current_tp <- tp[cycle, ]
    
    rand <- runif(1)
    
    new_hs <- case_when(
      rand < cumsum(current_tp)[1] ~ "wtlst",
      rand < cumsum(current_tp)[2] ~ "dtx",
      rand < cumsum(current_tp)[3] ~ "dtx",
      rand < cumsum(current_tp)[4] ~ "ltx",
      rand < cumsum(current_tp)[5] ~ "offlstc",
      rand < cumsum(current_tp)[6] ~ "dth"
    )
    
    if (dplyr::between(rand, cumsum(current_tp)[1], cumsum(current_tp)[4])) { ## dtx or ltx
      donordeceased <- 0
      donornew <- 0
    }
    
    if (dplyr::between(rand, cumsum(current_tp)[1], cumsum(current_tp)[3])) { # ndtx or dtx
      donordeceased <- 1
    }
    
    if (dplyr::between(rand, cumsum(current_tp)[1], cumsum(current_tp)[2])) { # ndtx
      donornew <- 1
    }
    
    if ((!dplyr::between(rand, cumsum(current_tp)[1], cumsum(current_tp)[2]) & 
         dplyr::between(rand, cumsum(max_tp[cycle, ])[1], cumsum(max_tp[cycle, ])[2])) | # Not selected for a new donor, but might have been
        (dplyr::between(rand, cumsum(current_tp)[1], cumsum(current_tp)[2]) & 
         !dplyr::between(rand, cumsum(min_tp[cycle, ])[1], cumsum(min_tp[cycle, ])[2]))) { # Selected for a new donor, but might not have been
      resimulate <- 1
    } 
    
    hs[current_cycle + cycle] <- new_hs
   
    health_state <- new_hs
    cycle <- cycle + 1
  }

  ## If patient died, all remaining health states are death
  last_hs <- hs[length(na.omit(hs))]
  if (last_hs == "dth") {hs[is.na(hs)] <- "dth"}

  ## Return health states
  return(list(
    hs = hs,
    donordeceased = donordeceased,
    donornew = donornew,
    resimulate = resimulate))
  
} # End of transitions from waitlist
  

  
# Transitions from off waitlist with cancer ----
trans_offlstc <- function(hs = NULL, chars = NULL,
                          n_cycles = NULL, cycle_length = NULL) {
  
  ## Extract patient characteristics
  agewtlst <- chars$agewtlst
  female <- chars$female
  bldgrp <- chars$bldgrp
  bldgrpo <- chars$bldgrpo
  bldgrpa <- chars$bldgrpa
  bldgrpb <- chars$bldgrpb
  bldgrpab <- chars$bldgrpab
  comorbs <- chars$comorbs
  prevtxcount <- chars$prevtxcount
  
  ## Current health state
  current_hs <- hs[length(na.omit(hs))]
  
  ## Current and remaining cycles
  current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
  remaining_cycles <- n_cycles - current_cycle
  

  ## Update cycle start times
  cycle_starttimes <- seq(0, remaining_cycles * cycle_length * 365.25, cycle_length * 365.25)
  age_cyclestart <- agewtlst + (current_cycle * cycle_length) + (cycle_starttimes[-length(cycle_starttimes)] / 365.25)
  years_cyclestart <- seq(0, (remaining_cycles - 1) * cycle_length, cycle_length)

  ## Probability death
  rate <- sapply(1:remaining_cycles, function(x) {
   mortality_long_offlstc$annualrate[mortality_long_offlstc$age == pmin(floor(age_cyclestart), 100)[x] & 
                                      mortality_long_offlstc$female == female &
                                      mortality_long_offlstc$years == pmin(floor(years_cyclestart + 1), 5)[x]]
  })
  
  tp_offlstc_dth <- rate_to_prob(rate, cycle_length)

  ## Probability remain off waitlist
  tp_offlstc_offlstc <- 1 - tp_offlstc_dth

  
  ## Transitions from off waitlist
  tp <- cbind(tp_offlstc_dth, tp_offlstc_offlstc)
  tp <- pmax(tp, 0) / rowSums(pmax(tp, 0))
                              
  health_state <- "offlstc"
  cycle <- 1
  while(health_state == "offlstc" & cycle <= remaining_cycles) {
    new_hs <- sample(x = c("dth", "offlstc"),
                     size = 1,
                     prob = tp[cycle, ])
    hs[current_cycle + cycle] <- new_hs
    
    health_state <- new_hs
    cycle <- cycle + 1
  }
  
  ## If patient died, all remaining health states are death
  last_hs <- hs[length(na.omit(hs))]
  if (last_hs == "dth") {hs[is.na(hs)] <- "dth"}
  
  ## Return health states
  return(hs)
  
} # End transitions from off waitlist with cancer
  

# Donor characteristics ----
sim_donor <- function(hs = NULL, chars = NULL, donordeceased = NULL, donornew = NULL,
                      cycle_length = NULL) {
  
  ## Extract patient characteristics
  agewtlst <- chars$agewtlst
  female <- chars$female
  bldgrp <- chars$bldgrp
  bldgrpo <- chars$bldgrpo
  bldgrpa <- chars$bldgrpa
  bldgrpb <- chars$bldgrpb
  bldgrpab <- chars$bldgrpab
  comorbs <- chars$comorbs
  prevtxcount <- chars$prevtxcount
  
  ## Current health state
  current_hs <- hs[length(na.omit(hs))]
  
  ## Current cycle
  current_cycle <- ifelse(sum(is.na(hs)) == 0, length(hs), min(which(is.na(hs))) - 1)
  

  ## Recipient's age at transplant
  agetx <- agewtlst + (current_cycle - 1) * cycle_length
  
  
  ## Donor PBM
  ### Model
  f_donorpbm <- 
   coef_donorpbm_int + 
   coef_donorpbm_prevtxcount * prevtxcount +
   coef_donorpbm_agetx * agetx + 
   coef_donorpbm_female * female +
   coef_donorpbm_bldgrpa * bldgrpa + 
   coef_donorpbm_bldgrpb * bldgrpb + 
   coef_donorpbm_bldgrpab * bldgrpab + 
   coef_donorpbm_comorbs * comorbs + 
   coef_donorpbm_donordeceased * donordeceased
  p_donorpbm <- exp(f_donorpbm)/(1 + exp(f_donorpbm))
   
  
  ### Generate which donors had a PBM
  ## Avoid using rbinom() since it will not always increment the random number seed in the same way
  ## https://stackoverflow.com/questions/18907600/erratic-seed-behavior-with-rbinomprob-0-5-in-r
  if(donornew == 0) {
   donorpbm <- ifelse(runif(1) <= p_donorpbm, 1, 0)
  } else {
   donorpbm <- 1
  }
  
  
  ## Donor type
  ### Equation for DBD SCD donor
  f_dbdscd <- 
   coef_donortype_dbdscd_int + 
   coef_donortype_dbdscd_agetx * agetx + 
   coef_donortype_dbdscd_female * female + 
   coef_donortype_dbdscd_bldgrpa * bldgrpa + 
   coef_donortype_dbdscd_bldgrpb * bldgrpb + 
   coef_donortype_dbdscd_bldgrpab * bldgrpab +
   coef_donortype_dbdscd_comorbs * comorbs + 
   coef_donortype_dbdscd_prevtxcount * prevtxcount + 
   coef_donortype_dbdscd_donorpbm * donorpbm
  expf_dbdscd <- exp(f_dbdscd)
  
  ### Equation for DBD ECD donor
  f_dbdecd <- 
   coef_donortype_dbdecd_int + 
   coef_donortype_dbdecd_agetx * agetx + 
   coef_donortype_dbdecd_female * female + 
   coef_donortype_dbdecd_bldgrpa * bldgrpa + 
   coef_donortype_dbdecd_bldgrpb * bldgrpb + 
   coef_donortype_dbdecd_bldgrpab * bldgrpab +
   coef_donortype_dbdecd_comorbs * comorbs + 
   coef_donortype_dbdecd_prevtxcount * prevtxcount + 
   coef_donortype_dbdecd_donorpbm * donorpbm
  expf_dbdecd <- exp(f_dbdecd)
   
  ### Sum of all equations
  expf_sum <- expf_dbdscd + expf_dbdecd
  
  ### Probability of each donor type
  p_dbdscd <- expf_dbdscd/(1 + expf_sum)
  p_dbdecd <- expf_dbdecd/(1 + expf_sum)
  p_dcd <- 1/(1 + expf_sum)
  
  ### Determine donor type
  if (donordeceased == 0) { ## If donor is not deceased, they must be living
   donortype <- "Living"
  } else if (donorpbm == 1) { ## If donor is deceased with PBM, they must be DBD SCD
   donortype <- "DBDSCD"
  } else {
   p_donortype <- c(p_dcd, p_dbdscd, p_dbdecd)
   donortype <- sample(x = c("DCD", "DBDSCD", "DBDECD"),
                       size = 1,
                       prob = p_donortype)
  }
  
  ### Dummy variables for donor type
  donortypedcd <- ifelse(donortype == "DCD", 1, 0)
  donortypedbdscd <- ifelse(donortype == "DBDSCD", 1, 0)
  donortypedbdecd <- ifelse(donortype == "DBDECD", 1, 0)
  donortypeliving <- ifelse(donortype == "Living", 1, 0)
  
  
  ## Donor sex 
  ### Model
  f_donorfemale <- 
   coef_donorfemale_int + 
   coef_donorfemale_agetx * agetx +
   coef_donorfemale_female * female + 
   coef_donorfemale_bldgrpa * bldgrpa +
   coef_donorfemale_bldgrpb * bldgrpb +
   coef_donorfemale_bldgrpab * bldgrpab +
   coef_donorfemale_comorbs * comorbs + 
   coef_donorfemale_prevtxcount * prevtxcount +
   coef_donorfemale_donortypedcd * donortypedcd + 
   coef_donorfemale_donortypedbdscd * donortypedbdscd + 
   coef_donorfemale_donortypedbdecd * donortypedbdecd + 
   coef_donorfemale_donorpbm * donorpbm
  p_donorfemale <- logistic(f_donorfemale)
  
  ### Simulate donor sex
  ## Avoid using rbinom() since it will not always increment the random number seed in the same way
  ## https://stackoverflow.com/questions/18907600/erratic-seed-behavior-with-rbinomprob-0-5-in-r
  donorfemale <- ifelse(runif(1) <= p_donorfemale, 1, 0)
  
  
  ## Donor age 
  ### Minimum and maximum donor age
  max_donorage = 100
  min_donorage = 0
  
  ### Model
  f_donorage <- 
   coef_donorage_int + 
   coef_donorage_agetx * agetx +
   coef_donorage_female * female + 
   coef_donorage_bldgrpa * bldgrpa +
   coef_donorage_bldgrpb * bldgrpb +
   coef_donorage_bldgrpab * bldgrpab +
   coef_donorage_comorbs * comorbs + 
   coef_donorage_prevtxcount * prevtxcount +
   coef_donorage_donortypedcd * donortypedcd + 
   coef_donorage_donortypedbdscd * donortypedbdscd + 
   coef_donorage_donortypedbdecd * donortypedbdecd + 
   coef_donorage_donorpbm * donorpbm + 
   coef_donorage_donorfemale * donorfemale
  
  ### Simulate donor age
  donorage <- rnorm(n = 1, mean = f_donorage, sd = rmse_donorage)
  
  ### Ensure donor age is within the specified age limits
  donorage <- pmax(pmin(donorage, max_donorage), min_donorage)
  
  
  ## Donor KDPI
  if (donordeceased == 0) {
    donorkdpi <- NA
  } else {
    ### Model
    f_donorkdpi <- 
     coef_donorkdpi_int + 
     coef_donorkdpi_agetx * agetx +
     coef_donorkdpi_female * female + 
     coef_donorkdpi_bldgrpa * bldgrpa +
     coef_donorkdpi_bldgrpb * bldgrpb +
     coef_donorkdpi_bldgrpab * bldgrpab +
     coef_donorkdpi_comorbs * comorbs + 
     coef_donorkdpi_prevtxcount * prevtxcount +
     coef_donorkdpi_donortypedbdscd * donortypedbdscd + 
     coef_donorkdpi_donortypedbdecd * donortypedbdecd + 
     coef_donorkdpi_donorpbm * donorpbm + 
     coef_donorkdpi_donorfemale * donorfemale +
     coef_donorkdpi_donorage * donorage
    
    ### Simulate donor KDPI
    donorkdpi_logodds <- rnorm(n = 1, mean = f_donorkdpi, sd = rmse_donorkdpi)
    
    donorkdpi <- exp(donorkdpi_logodds) / (1 + exp(donorkdpi_logodds))
  }
  
  ## Return donor characteristics
  donorchars <- list(
    agetx = agetx,
    donordeceased = donordeceased,
    donornew = donornew,
    donorpbm = donorpbm,
    donorage = donorage,
    donorfemale = donorfemale,
    donortype = donortype,
    donortypedcd = donortypedcd,
    donortypedbdscd = donortypedbdscd,
    donortypedbdecd = donortypedbdecd,
    donortypeliving = donortypeliving,
    donorkdpi = donorkdpi)
  
  return(donorchars)
    
} # End donor characteristics
  
  
# Determine if transplant was a transmission ----
sim_transmission <- function(hs = NULL, donorchars = NULL, 
                             baseline_risk = NULL, new_risk = NULL,
                             min_new_risk = NULL, max_new_risk = NULL) {
  
  ## Extract donor characteristics
  donordeceased <- donorchars$donordeceased
  donornew <- donorchars$donornew
  donorpbm <- donorchars$donorpbm
  
  ## Current health state
  current_hs <- hs[length(na.omit(hs))]
  
  ## Determine if transplant was a transmission
  rand <- runif(1)
  
  resimulate <- 0
  
  if (donordeceased == 0) {
    transmission <- 0
  } else if (donorpbm == 0) {
    transmission <- 0
  } else if (donornew == 0) {
    transmission <- (rand < baseline_risk) * 1
  } else if (donornew == 1) {
    transmission <- (rand < new_risk) * 1
    
    if ((rand > new_risk & rand < max_new_risk) | # Not a transmission, but might have been
        (rand < new_risk & rand > min_new_risk)) { # Was a transmission, but might not have been
      resimulate <- 1
    }
  }

  ## Update health states if transplant was a transmission
  if(current_hs == "dtx" & transmission == 1) {
    hs[min(which(hs %in% c("dtx")))] <- "dtxt"
    current_hs <- "dtxt"
  }
  
  ## Return health states
  return(list(
    hs = hs,
    resimulate = resimulate))
  
} # End check if transplant was a transmission


# Transitions from living donor transplant ----
trans_ltx <- function(hs = NULL, chars = NULL, donorchars = NULL,
                      n_cycles = NULL, cycle_length = NULL) {
  
  ## Extract patient characteristics
  agewtlst <- chars$agewtlst
  female <- chars$female
  bldgrp <- chars$bldgrp
  bldgrpo <- chars$bldgrpo
  bldgrpa <- chars$bldgrpa
  bldgrpb <- chars$bldgrpb
  bldgrpab <- chars$bldgrpab
  comorbs <- chars$comorbs
  prevtxcount <- chars$prevtxcount
  
  ## Extract donor characteristics
  agetx <- donorchars$agetx
  donordeceased <- donorchars$donordeceased
  donornew <- donorchars$donornew
  donorpbm <- donorchars$donorpbm
  donorage <- donorchars$donorage
  donorfemale <- donorchars$donorfemale
  donortype <- donorchars$donortype
  donortypedcd <- donorchars$donortypedcd
  donortypedbdscd <- donorchars$donortypedbdscd
  donortypedbdecd <- donorchars$donortypedbdecd
  donortypeliving <- donorchars$donortypeliving
  donorkdpi <- donorchars$donorkdpi
  
  ## Current health state
  current_hs <- hs[length(na.omit(hs))]
  
  ## Current and remaining cycles
  current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
  remaining_cycles <- n_cycles - current_cycle


  ## Update cycle start times
  cycle_starttimes <- seq(0, remaining_cycles * cycle_length * 365.25, cycle_length * 365.25)
  age_cyclestart <- agewtlst + ((current_cycle - 1) * cycle_length) + (cycle_starttimes[-length(cycle_starttimes)] / 365.25)
  years_cyclestart <- seq(0, (remaining_cycles - 1) * cycle_length, cycle_length)
  
  ## Probability transplant failure
  log_cumprob <-
    log(psurvspline(q = cycle_starttimes, 
                    gamma = timetoltxfail_splinegamma, 
                    knots = timetoltxfail_splineknots)) +
    coef_timetoltxfail_agetx * agetx + 
    coef_timetoltxfail_female * female + 
    coef_timetoltxfail_bldgrpa * bldgrpa + 
    coef_timetoltxfail_bldgrpb * bldgrpb + 
    coef_timetoltxfail_bldgrpab * bldgrpab + 
    coef_timetoltxfail_comorbs * comorbs +
    coef_timetoltxfail_prevtxcount * prevtxcount +
    coef_timetoltxfail_donorage * donorage +
    coef_timetoltxfail_donorfemale * donorfemale
  cumprob <- exp(log_cumprob)
    
  cumprob0 <- cumprob[-length(cumprob)] 
  cumprob1 <- cumprob[-1]
  
  tp_ltx_ltf <- (cumprob1 - cumprob0) / (1 - cumprob0)
  
  
  ## Probability cancer (with functioning transplant)
  f_lambda <- 
    log(timetoltxcancer_scale) + 
    coef_timetoltxcancer_agetx * agetx + 
    coef_timetoltxcancer_female * female + 
    coef_timetoltxcancer_bldgrpa * bldgrpa + 
    coef_timetoltxcancer_bldgrpb * bldgrpb + 
    coef_timetoltxcancer_bldgrpab * bldgrpab + 
    coef_timetoltxcancer_comorbs * comorbs +
    coef_timetoltxcancer_prevtxcount * prevtxcount +
    coef_timetoltxcancer_donorage * donorage +
    coef_timetoltxcancer_donorfemale * donorfemale
  lambda <- exp(f_lambda)^(-1/timetoltxcancer_shape)
  
  cumprob <- pweibull(q = cycle_starttimes, shape = timetoltxcancer_shape, scale = lambda)
  
  cumprob0 <- cumprob[-length(cumprob)] 
  cumprob1 <- cumprob[-1]
  
  tp_ltx_ltxc <- (cumprob1 - cumprob0) / (1 - cumprob0)
  
  
  ## Probability death
  rate <- sapply(1:remaining_cycles, function(x) {
    mortality_tx$annualrate[mortality_tx$age == pmin(floor(age_cyclestart), 100)[x] & 
                              mortality_tx$female == female]
  })
  
  tp_ltx_dth <- rate_to_prob(rate, cycle_length)
  
  
  ## Probability remain in living donor transplant
  tp_ltx_ltx <- 1 - tp_ltx_ltf - tp_ltx_ltxc - tp_ltx_dth
  
  
  ## Transitions from living donor transplant
  tp <- cbind(tp_ltx_ltf, tp_ltx_ltxc, tp_ltx_dth, tp_ltx_ltx)
  tp <- pmax(tp, 0) / rowSums(pmax(tp, 0))
                              
  health_state <- "ltx"
  cycle <- 1
  while(health_state == "ltx" & cycle <= remaining_cycles) {
    new_hs <- sample(x = c("ltf", "ltxc", "dth", "ltx"),
                     size = 1,
                     prob = tp[cycle, ])
    hs[current_cycle + cycle] <- new_hs
    
    health_state <- new_hs
    cycle <- cycle + 1
  }

  ## If patient died, all remaining health states are death
  last_hs <- hs[length(na.omit(hs))]
  if (last_hs == "dth") {hs[is.na(hs)] <- "dth"}
  
  ## Return health states
  return(hs)
  
} # End transitions from living donor transplant
  

# Transitions from living donor cancer ----
trans_ltxc <- function(hs = NULL, chars = NULL, donorchars = NULL,
                       n_cycles = NULL, cycle_length = NULL) {
  
  ## Extract patient characteristics
  agewtlst <- chars$agewtlst
  female <- chars$female
  bldgrp <- chars$bldgrp
  bldgrpo <- chars$bldgrpo
  bldgrpa <- chars$bldgrpa
  bldgrpb <- chars$bldgrpb
  bldgrpab <- chars$bldgrpab
  comorbs <- chars$comorbs
  prevtxcount <- chars$prevtxcount
  
  ## Extract donor characteristics
  agetx <- donorchars$agetx
  donordeceased <- donorchars$donordeceased
  donornew <- donorchars$donornew
  donorpbm <- donorchars$donorpbm
  donorage <- donorchars$donorage
  donorfemale <- donorchars$donorfemale
  donortype <- donorchars$donortype
  donortypedcd <- donorchars$donortypedcd
  donortypedbdscd <- donorchars$donortypedbdscd
  donortypedbdecd <- donorchars$donortypedbdecd
  donortypeliving <- donorchars$donortypeliving
  donorkdpi <- donorchars$donorkdpi
  
  ## Current health state
  current_hs <- hs[length(na.omit(hs))]
  
  ## Current and remaining cycles
  current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
  remaining_cycles <- n_cycles - current_cycle
  
  
  ## Update cycle start times
  cycle_starttimes <- seq(0, remaining_cycles * cycle_length * 365.25, cycle_length * 365.25)
  age_cyclestart <- agewtlst + (current_cycle * cycle_length) + (cycle_starttimes[-length(cycle_starttimes)] / 365.25)
  years_cyclestart <- seq(0, (remaining_cycles - 1) * cycle_length, cycle_length)
  
  ## Age at post-transplant cancer
  agetxc <- agewtlst + (min(which(hs %in% c("ltxc", "ltxt", "ltfc"))) - 1) * cycle_length
  
  ## Probability transplant failure
  mu <- timetoltxcfail_mu +
    coef_timetoltxcfail_agetx * agetx + 
    coef_timetoltxcfail_agetxc * agetxc + 
    coef_timetoltxcfail_female * female + 
    coef_timetoltxcfail_bldgrpa * bldgrpa + 
    coef_timetoltxcfail_bldgrpb * bldgrpb + 
    coef_timetoltxcfail_bldgrpab * bldgrpab + 
    coef_timetoltxcfail_comorbs * comorbs +
    coef_timetoltxcfail_prevtxcount * prevtxcount +
    coef_timetoltxcfail_donorage * donorage +
    coef_timetoltxcfail_donorfemale * donorfemale
  
  cumprob <- pgengamma(q = cycle_starttimes,
                       mu = mu,
                       sigma = timetoltxcfail_sigma,
                       Q = timetoltxcfail_Q)
  
  cumprob0 <- cumprob[-length(cumprob)] 
  cumprob1 <- cumprob[-1]
  
  tp_ltxc_ltfc <- (cumprob1 - cumprob0) / (1 - cumprob0)
  tp_ltxc_ltfc[is.nan(tp_ltxc_ltfc)] <- 1
  
  ## Probability death
  rate <- sapply(1:remaining_cycles, function(x) {
    mortality_long_txc$annualrate[mortality_long_txc$age == pmin(floor(age_cyclestart), 100)[x] & 
                                    mortality_long_txc$female == female &
                                    mortality_long_txc$years == pmin(floor(years_cyclestart + 1), 5)[x]]
  })
  
  tp_ltxc_dth <- rate_to_prob(rate, cycle_length)
  
  ## Probability remain in living donor transplant cancer
  tp_ltxc_ltxc <- 1 - tp_ltxc_ltfc - tp_ltxc_dth
  
  ## Transitions from deceased donor cancer
  tp <- cbind(tp_ltxc_ltfc, tp_ltxc_dth, tp_ltxc_ltxc)
  tp <- pmax(tp, 0) / rowSums(pmax(tp, 0))
  
  health_state <- "ltxc"
  cycle <- 1
  while(health_state == "ltxc" & cycle <= remaining_cycles) {
    new_hs <- sample(x = c("ltfc", "dth", "ltxc"),
                     size = 1,
                     prob = tp[cycle, ])
    hs[current_cycle + cycle] <- new_hs
    
    health_state <- new_hs
    cycle <- cycle + 1
  }
  
  ## If patient died, all remaining health states are death
  last_hs <- hs[length(na.omit(hs))]
  if (last_hs == "dth") {hs[is.na(hs)] <- "dth"}
  
  ## Return health states
  return(hs)
  
} # End transitions from living donor cancer


# Transitions from living donor transplant failure ----
trans_ltf <- function(hs = NULL, chars = NULL, donorchars = NULL,
                      n_cycles = NULL, cycle_length = NULL) {
  
  ## Extract patient characteristics
  agewtlst <- chars$agewtlst
  female <- chars$female
  bldgrp <- chars$bldgrp
  bldgrpo <- chars$bldgrpo
  bldgrpa <- chars$bldgrpa
  bldgrpb <- chars$bldgrpb
  bldgrpab <- chars$bldgrpab
  comorbs <- chars$comorbs
  prevtxcount <- chars$prevtxcount
  
  ## Extract donor characteristics
  agetx <- donorchars$agetx
  donordeceased <- donorchars$donordeceased
  donornew <- donorchars$donornew
  donorpbm <- donorchars$donorpbm
  donorage <- donorchars$donorage
  donorfemale <- donorchars$donorfemale
  donortype <- donorchars$donortype
  donortypedcd <- donorchars$donortypedcd
  donortypedbdscd <- donorchars$donortypedbdscd
  donortypedbdecd <- donorchars$donortypedbdecd
  donortypeliving <- donorchars$donortypeliving
  donorkdpi <- donorchars$donorkdpi
  
  ## Current health state
  current_hs <- hs[length(na.omit(hs))]
  
  ## Current and remaining cycles
  current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
  remaining_cycles <- n_cycles - current_cycle


  ## Update cycle start times
  cycle_starttimes <- seq(0, remaining_cycles * cycle_length * 365.25, cycle_length * 365.25)
  age_cyclestart <- agewtlst + (current_cycle * cycle_length) + (cycle_starttimes[-length(cycle_starttimes)] / 365.25)
  years_cyclestart <- seq(0, (remaining_cycles - 1) * cycle_length, cycle_length)
  
  ## Probability cancer
  rate <- sapply(1:remaining_cycles, function(x) {
    cancer_wtlst$annualrate[cancer_wtlst$age == pmin(floor(age_cyclestart), 100)[x] & 
                              cancer_wtlst$female == female]
  })
  
  tp_ltf_ltfc <- rate_to_prob(rate, cycle_length)
  
  ## Probability death
  rate <- sapply(1:remaining_cycles, function(x) {
    mortality_tf$annualrate[mortality_tf$age == pmin(floor(age_cyclestart), 100)[x] & 
                              mortality_tf$female == female]
  })
  
  tp_ltf_dth <- rate_to_prob(rate, cycle_length)
  
  ## Probability remain in living donor transplant failure
  tp_ltf_ltf <- 1 - tp_ltf_ltfc - tp_ltf_dth

  ## Transitions from living donor transplant failure
  tp <- cbind(tp_ltf_ltfc, tp_ltf_dth, tp_ltf_ltf)
  tp <- pmax(tp, 0) / rowSums(pmax(tp, 0))
                              
  health_state <- "ltf"
  cycle <- 1
  while(health_state == "ltf" & cycle <= remaining_cycles) {
    new_hs <- sample(x = c("ltfc", "dth", "ltf"),
                     size = 1,
                     prob = tp[cycle, ])
    hs[current_cycle + cycle] <- new_hs
    
    health_state <- new_hs
    cycle <- cycle + 1
  }
  
  ## If patient died, all remaining health states are death
  last_hs <- hs[length(na.omit(hs))]
  if (last_hs == "dth") {hs[is.na(hs)] <- "dth"}
  
  ## Return health states
  return(hs)
  
} # End of transitions from living donor transplant failure
  

# Transitions from living donor transplant failure with cancer ----
trans_ltfc <- function(hs = NULL, chars = NULL, donorchars = NULL,
                       n_cycles = NULL, cycle_length = NULL) {
  
  ## Extract patient characteristics
  agewtlst <- chars$agewtlst
  female <- chars$female
  bldgrp <- chars$bldgrp
  bldgrpo <- chars$bldgrpo
  bldgrpa <- chars$bldgrpa
  bldgrpb <- chars$bldgrpb
  bldgrpab <- chars$bldgrpab
  comorbs <- chars$comorbs
  prevtxcount <- chars$prevtxcount
  
  ## Extract donor characteristics
  agetx <- donorchars$agetx
  donordeceased <- donorchars$donordeceased
  donornew <- donorchars$donornew
  donorpbm <- donorchars$donorpbm
  donorage <- donorchars$donorage
  donorfemale <- donorchars$donorfemale
  donortype <- donorchars$donortype
  donortypedcd <- donorchars$donortypedcd
  donortypedbdscd <- donorchars$donortypedbdscd
  donortypedbdecd <- donorchars$donortypedbdecd
  donortypeliving <- donorchars$donortypeliving
  donorkdpi <- donorchars$donorkdpi
  
  ## Current health state
  current_hs <- hs[length(na.omit(hs))]
  
  ## Current and remaining cycles
  current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
  remaining_cycles <- n_cycles - current_cycle
  
  
  ## Update cycle start times
  cycle_starttimes <- seq(0, remaining_cycles * cycle_length * 365.25, cycle_length * 365.25)
  age_cyclestart <- agewtlst + (current_cycle * cycle_length) + (cycle_starttimes[-length(cycle_starttimes)] / 365.25)
  years_cyclestart <- seq(0, (remaining_cycles - 1) * cycle_length, cycle_length)
  
  ## Probability death
  rate <- sapply(1:remaining_cycles, function(x) {
    mortality_long_tfc$annualrate[mortality_long_tfc$age == pmin(floor(age_cyclestart), 100)[x] & 
                                    mortality_long_tfc$female == female &
                                    mortality_long_tfc$years == pmin(floor(years_cyclestart + 1), 5)[x]]
  })
  
  tp_ltfc_dth <- rate_to_prob(rate, cycle_length)
  
  ## Probability remain in living donor transplant failure with cancer
  tp_ltfc_ltfc <- 1 - tp_ltfc_dth
  
  ## Transitions from living donor transplant failure with cancer
  tp <- cbind(tp_ltfc_dth, tp_ltfc_ltfc)
  tp <- pmax(tp, 0) / rowSums(pmax(tp, 0))
  
  health_state <- "ltfc"
  cycle <- 1
  while(health_state == "ltfc" & cycle <= remaining_cycles) {
    new_hs <- sample(x = c("dth", "ltfc"),
                     size = 1,
                     prob = tp[cycle, ])
    hs[current_cycle + cycle] <- new_hs
    
    health_state <- new_hs
    cycle <- cycle + 1
  }
  
  ## If patient died, all remaining health states are death
  last_hs <- hs[length(na.omit(hs))]
  if (last_hs == "dth") {hs[is.na(hs)] <- "dth"}
  
  ## Return health states
  return(hs)
  
} # End transitions from living donor transplant failure with cancer



# Transitions from deceased donor transplant ----
trans_dtx <- function(hs = NULL, chars = NULL, donorchars = NULL,
                      n_cycles = NULL, cycle_length = NULL) {
  
  ## Extract patient characteristics
  agewtlst <- chars$agewtlst
  female <- chars$female
  bldgrp <- chars$bldgrp
  bldgrpo <- chars$bldgrpo
  bldgrpa <- chars$bldgrpa
  bldgrpb <- chars$bldgrpb
  bldgrpab <- chars$bldgrpab
  comorbs <- chars$comorbs
  prevtxcount <- chars$prevtxcount
  
  ## Extract donor characteristics
  agetx <- donorchars$agetx
  donordeceased <- donorchars$donordeceased
  donornew <- donorchars$donornew
  donorpbm <- donorchars$donorpbm
  donorage <- donorchars$donorage
  donorfemale <- donorchars$donorfemale
  donortype <- donorchars$donortype
  donortypedcd <- donorchars$donortypedcd
  donortypedbdscd <- donorchars$donortypedbdscd
  donortypedbdecd <- donorchars$donortypedbdecd
  donortypeliving <- donorchars$donortypeliving
  donorkdpi <- donorchars$donorkdpi
  
  ## Current health state
  current_hs <- hs[length(na.omit(hs))]
  
  ## Current and remaining cycles
  current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
  remaining_cycles <- n_cycles - current_cycle

  
  ## Update cycle start times
  cycle_starttimes <- seq(0, remaining_cycles * cycle_length * 365.25, cycle_length * 365.25)
  age_cyclestart <- agewtlst + (current_cycle * cycle_length) + (cycle_starttimes[-length(cycle_starttimes)] / 365.25)
  years_cyclestart <- seq(0, (remaining_cycles - 1) * cycle_length, cycle_length)
  
  ## Probability transplant failure
  log_cumprob <-
    log(psurvspline(q = cycle_starttimes, 
                    gamma = timetotxfail_splinegamma, 
                    knots = timetotxfail_splineknots)) +
    coef_timetotxfail_agetx * agetx + 
    coef_timetotxfail_female * female + 
    coef_timetotxfail_bldgrpa * bldgrpa + 
    coef_timetotxfail_bldgrpb * bldgrpb + 
    coef_timetotxfail_bldgrpab * bldgrpab + 
    coef_timetotxfail_comorbs * comorbs +
    coef_timetotxfail_prevtxcount * prevtxcount +
    coef_timetotxfail_donortypedbdscd * donortypedbdscd + 
    coef_timetotxfail_donortypedbdecd * donortypedbdecd + 
    coef_timetotxfail_donorage * donorage +
    coef_timetotxfail_donorfemale * donorfemale +
    coef_timetotxfail_donorkdpi * donorkdpi
  cumprob <- exp(log_cumprob)
  
  cumprob0 <- cumprob[-length(cumprob)] 
  cumprob1 <- cumprob[-1]
  
  tp_dtx_dtf <- (cumprob1 - cumprob0) / (1 - cumprob0)
  
  
  ## Probability cancer (with functioning transplant)
  f_lambda <- 
    log(timetotxcancer_scale) + 
    coef_timetotxcancer_agetx * agetx + 
    coef_timetotxcancer_female * female + 
    coef_timetotxcancer_bldgrpa * bldgrpa + 
    coef_timetotxcancer_bldgrpb * bldgrpb + 
    coef_timetotxcancer_bldgrpab * bldgrpab + 
    coef_timetotxcancer_comorbs * comorbs +
    coef_timetotxcancer_prevtxcount * prevtxcount +
    coef_timetotxcancer_donortypedbdscd * donortypedbdscd + 
    coef_timetotxcancer_donortypedbdecd * donortypedbdecd + 
    coef_timetotxcancer_donorage * donorage +
    coef_timetotxcancer_donorfemale * donorfemale +
    coef_timetotxcancer_donorkdpi * donorkdpi
  lambda <- exp(f_lambda)^(-1/timetotxcancer_shape)
  
  cumprob <- pweibull(q = cycle_starttimes, shape = timetotxcancer_shape, scale = lambda)
  
  cumprob0 <- cumprob[-length(cumprob)] 
  cumprob1 <- cumprob[-1]
  
  tp_dtx_dtxc <- (cumprob1 - cumprob0) / (1 - cumprob0)
  
  
  ## Probability death
  rate <- sapply(1:remaining_cycles, function(x) {
    mortality_tx$annualrate[mortality_tx$age == pmin(floor(age_cyclestart), 100)[x] & 
                              mortality_tx$female == female]
  })
  
  tp_dtx_dth <- rate_to_prob(rate, cycle_length)
  
  
  ## Probability remain in deceased donor transplant
  tp_dtx_dtx <- 1 - tp_dtx_dtf - tp_dtx_dtxc - tp_dtx_dth
  
  
  ## Transitions from deceased donor transplant
  tp <- cbind(tp_dtx_dtf, tp_dtx_dtxc, tp_dtx_dth, tp_dtx_dtx)
  tp <- pmax(tp, 0) / rowSums(pmax(tp, 0))
                              
  health_state <- "dtx"
  cycle <- 1
  while(health_state == "dtx" & cycle <= remaining_cycles) {
    new_hs <- sample(x = c("dtf", "dtxc", "dth", "dtx"),
                     size = 1,
                     prob = tp[cycle, ])
    hs[current_cycle + cycle] <- new_hs
    
    health_state <- new_hs
    cycle <- cycle + 1
  }
  
  ## If patient died, all remaining health states are death
  last_hs <- hs[length(na.omit(hs))]
  if (last_hs == "dth") {hs[is.na(hs)] <- "dth"}
  
  ## Return health states
  return(hs)
  
} # End of transitions from deceased donor transplant
  
  
# Transitions from deceased donor cancer ----
trans_dtxc <- function(hs = NULL, chars = NULL, donorchars = NULL,
                       n_cycles = NULL, cycle_length = NULL) {
  
  ## Extract patient characteristics
  agewtlst <- chars$agewtlst
  female <- chars$female
  bldgrp <- chars$bldgrp
  bldgrpo <- chars$bldgrpo
  bldgrpa <- chars$bldgrpa
  bldgrpb <- chars$bldgrpb
  bldgrpab <- chars$bldgrpab
  comorbs <- chars$comorbs
  prevtxcount <- chars$prevtxcount
  
  ## Extract donor characteristics
  agetx <- donorchars$agetx
  donordeceased <- donorchars$donordeceased
  donornew <- donorchars$donornew
  donorpbm <- donorchars$donorpbm
  donorage <- donorchars$donorage
  donorfemale <- donorchars$donorfemale
  donortype <- donorchars$donortype
  donortypedcd <- donorchars$donortypedcd
  donortypedbdscd <- donorchars$donortypedbdscd
  donortypedbdecd <- donorchars$donortypedbdecd
  donortypeliving <- donorchars$donortypeliving
  donorkdpi <- donorchars$donorkdpi
  
  ## Current health state
  current_hs <- hs[length(na.omit(hs))]
  
  ## Current and remaining cycles
  current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
  remaining_cycles <- n_cycles - current_cycle

  
  ## Update cycle start times
  cycle_starttimes <- seq(0, remaining_cycles * cycle_length * 365.25, cycle_length * 365.25)
  age_cyclestart <- agewtlst + (current_cycle * cycle_length) + (cycle_starttimes[-length(cycle_starttimes)] / 365.25)
  years_cyclestart <- seq(0, (remaining_cycles - 1) * cycle_length, cycle_length)
  
  ## Age at post-transplant cancer
  agetxc <- agewtlst + (min(which(hs %in% c("dtxc", "dtxt", "dtfc"))) - 1) * cycle_length
  
  ## Probability transplant failure
  mu <- timetotxcfail_mu +
    coef_timetotxcfail_agetx * agetx + 
    coef_timetotxcfail_agetxc * agetxc + 
    coef_timetotxcfail_female * female + 
    coef_timetotxcfail_bldgrpa * bldgrpa + 
    coef_timetotxcfail_bldgrpb * bldgrpb + 
    coef_timetotxcfail_bldgrpab * bldgrpab + 
    coef_timetotxcfail_comorbs * comorbs +
    coef_timetotxcfail_prevtxcount * prevtxcount +
    coef_timetotxcfail_donortypedbdscd * donortypedbdscd + 
    coef_timetotxcfail_donortypedbdecd * donortypedbdecd + 
    coef_timetotxcfail_donorage * donorage +
    coef_timetotxcfail_donorfemale * donorfemale +
    coef_timetotxcfail_donorkdpi * donorkdpi
  
  cumprob <- pgengamma(q = cycle_starttimes,
                       mu = mu,
                       sigma = timetotxcfail_sigma,
                       Q = timetotxcfail_Q)
  
  cumprob0 <- cumprob[-length(cumprob)] 
  cumprob1 <- cumprob[-1]
  
  tp_dtxc_dtfc <- (cumprob1 - cumprob0) / (1 - cumprob0)
  tp_dtxc_dtfc[is.nan(tp_dtxc_dtfc)] <- 1
  
  ## Probability death
  rate <- sapply(1:remaining_cycles, function(x) {
    mortality_long_txc$annualrate[mortality_long_txc$age == pmin(floor(age_cyclestart), 100)[x] & 
                                    mortality_long_txc$female == female &
                                    mortality_long_txc$years == pmin(floor(years_cyclestart + 1), 5)[x]]
  })
  
  tp_dtxc_dth <- rate_to_prob(rate, cycle_length)
  
  ## Probability remain in deceased donor transplant cancer
  tp_dtxc_dtxc <- 1 - tp_dtxc_dtfc - tp_dtxc_dth
  
  ## Transitions from deceased donor cancer
  tp <- cbind(tp_dtxc_dtfc, tp_dtxc_dth, tp_dtxc_dtxc)
  tp <- pmax(tp, 0) / rowSums(pmax(tp, 0))
  
  health_state <- "dtxc"
  cycle <- 1
  while(health_state == "dtxc" & cycle <= remaining_cycles) {
    new_hs <- sample(x = c("dtfc", "dth", "dtxc"),
                     size = 1,
                     prob = tp[cycle, ])
    hs[current_cycle + cycle] <- new_hs
    
    health_state <- new_hs
    cycle <- cycle + 1
  }
  
  ## If patient died, all remaining health states are death
  last_hs <- hs[length(na.omit(hs))]
  if (last_hs == "dth") {hs[is.na(hs)] <- "dth"}
  
  ## Return health states
  return(hs)
    
} # End transitions from deceased donor cancer
  
  
# Transitions from deceased donor transplant failure ----
trans_dtf <- function(hs = NULL, chars = NULL, donorchars = NULL,
                      n_cycles = NULL, cycle_length = NULL) {
  
  ## Extract patient characteristics
  agewtlst <- chars$agewtlst
  female <- chars$female
  bldgrp <- chars$bldgrp
  bldgrpo <- chars$bldgrpo
  bldgrpa <- chars$bldgrpa
  bldgrpb <- chars$bldgrpb
  bldgrpab <- chars$bldgrpab
  comorbs <- chars$comorbs
  prevtxcount <- chars$prevtxcount
  
  ## Extract donor characteristics
  agetx <- donorchars$agetx
  donordeceased <- donorchars$donordeceased
  donornew <- donorchars$donornew
  donorpbm <- donorchars$donorpbm
  donorage <- donorchars$donorage
  donorfemale <- donorchars$donorfemale
  donortype <- donorchars$donortype
  donortypedcd <- donorchars$donortypedcd
  donortypedbdscd <- donorchars$donortypedbdscd
  donortypedbdecd <- donorchars$donortypedbdecd
  donortypeliving <- donorchars$donortypeliving
  donorkdpi <- donorchars$donorkdpi
  
  ## Current health state
  current_hs <- hs[length(na.omit(hs))]
  
  ## Current and remaining cycles
  current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
  remaining_cycles <- n_cycles - current_cycle
    
  ## Update cycle start times
  cycle_starttimes <- seq(0, remaining_cycles * cycle_length * 365.25, cycle_length * 365.25)
  age_cyclestart <- agewtlst + (current_cycle * cycle_length) + (cycle_starttimes[-length(cycle_starttimes)] / 365.25)
  years_cyclestart <- seq(0, (remaining_cycles - 1) * cycle_length, cycle_length)
  
  ## Probability cancer
  rate <- sapply(1:remaining_cycles, function(x) {
    cancer_wtlst$annualrate[cancer_wtlst$age == pmin(floor(age_cyclestart), 100)[x] & 
                              cancer_wtlst$female == female]
  })
  
  tp_dtf_dtfc <- rate_to_prob(rate, cycle_length)
  
  ## Probability death
  rate <- sapply(1:remaining_cycles, function(x) {
    mortality_tf$annualrate[mortality_tf$age == pmin(floor(age_cyclestart), 100)[x] &
                              mortality_tf$female == female]
  })
  
  tp_dtf_dth <- rate_to_prob(rate, cycle_length)
  
  ## Probability remain in deceased donor transplant failure
  tp_dtf_dtf <- 1 - tp_dtf_dtfc - tp_dtf_dth
  
  ## Transitions from deceased donor cancer
  tp <- cbind(tp_dtf_dtfc, tp_dtf_dth, tp_dtf_dtf)
  tp <- pmax(tp, 0) / rowSums(pmax(tp, 0))
  
  health_state <- "dtf"
  cycle <- 1
  while(health_state == "dtf" & cycle <= remaining_cycles) {
    new_hs <- sample(x = c("dtfc", "dth", "dtf"),
                     size = 1,
                     prob = tp[cycle, ])
    hs[current_cycle + cycle] <- new_hs
    
    health_state <- new_hs
    cycle <- cycle + 1
  }

  ## If patient died, all remaining health states are death
  last_hs <- hs[length(na.omit(hs))]
  if (last_hs == "dth") {hs[is.na(hs)] <- "dth"}
  
  ## Return health states
  return(hs)
    
} # End transitions from deceased donor transplant failure
  
  
# Transitions from transmission ----
trans_dtxt <- function(hs = NULL, chars = NULL, donorchars = NULL, 
                       instant_death = 0, transmission_cycles = NULL,
                       n_cycles = NULL, cycle_length = NULL) {
  
  ## Extract patient characteristics
  agewtlst <- chars$agewtlst
  female <- chars$female
  bldgrp <- chars$bldgrp
  bldgrpo <- chars$bldgrpo
  bldgrpa <- chars$bldgrpa
  bldgrpb <- chars$bldgrpb
  bldgrpab <- chars$bldgrpab
  comorbs <- chars$comorbs
  prevtxcount <- chars$prevtxcount
  
  ## Extract donor characteristics
  agetx <- donorchars$agetx
  donordeceased <- donorchars$donordeceased
  donornew <- donorchars$donornew
  donorpbm <- donorchars$donorpbm
  donorage <- donorchars$donorage
  donorfemale <- donorchars$donorfemale
  donortype <- donorchars$donortype
  donortypedcd <- donorchars$donortypedcd
  donortypedbdscd <- donorchars$donortypedbdscd
  donortypedbdecd <- donorchars$donortypedbdecd
  donortypeliving <- donorchars$donortypeliving
  donorkdpi <- donorchars$donorkdpi
  
  ## Current health state
  current_hs <- hs[length(na.omit(hs))]
  
  ## Current and remaining cycles
  current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
  remaining_cycles <- n_cycles - current_cycle

  
  ## Update cycle start times
  cycle_starttimes <- seq(0, remaining_cycles * cycle_length * 365.25, cycle_length * 365.25)
  age_cyclestart <- agewtlst + (current_cycle * cycle_length) + (cycle_starttimes[-length(cycle_starttimes)] / 365.25)
  years_cyclestart <- seq(0, (remaining_cycles - 1) * cycle_length, cycle_length)
  
  ## Age at post-transplant cancer
  agetxc <- agewtlst + (min(which(hs %in% c("dtxc", "dtxt", "dtfc"))) - 1) * cycle_length
  
  ## Probability death
  rate <- sapply(1:remaining_cycles, function(x) {
    mortality_long_txc$annualrate[mortality_long_txc$age == pmin(floor(age_cyclestart), 100)[x] & 
                                    mortality_long_txc$female == female &
                                    mortality_long_txc$years == pmin(floor(years_cyclestart + 1), 5)[x]]
  })
  
  tp_dtxt_dth <- rate_to_prob(rate, cycle_length)
  
  tp_dtxt_dth[(transmission_cycles + 1):remaining_cycles] <- 0
  
  
  ## Probability transplant failure (due to nephrectomy)
  mu <- timetotxcfail_mu +
    coef_timetotxcfail_agetx * agetx + 
    coef_timetotxcfail_agetxc * agetxc + 
    coef_timetotxcfail_female * female + 
    coef_timetotxcfail_bldgrpa * bldgrpa + 
    coef_timetotxcfail_bldgrpb * bldgrpb + 
    coef_timetotxcfail_bldgrpab * bldgrpab + 
    coef_timetotxcfail_comorbs * comorbs +
    coef_timetotxcfail_prevtxcount * prevtxcount +
    coef_timetotxcfail_donortypedbdscd * donortypedbdscd + 
    coef_timetotxcfail_donortypedbdecd * donortypedbdecd + 
    coef_timetotxcfail_donorage * donorage +
    coef_timetotxcfail_donorfemale * donorfemale +
    coef_timetotxcfail_donorkdpi * donorkdpi
  
  cumprob <- pgengamma(q = cycle_starttimes,
                       mu = mu,
                       sigma = timetotxcfail_sigma,
                       Q = timetotxcfail_Q)
  
  cumprob0 <- cumprob[-length(cumprob)] 
  cumprob1 <- cumprob[-1]
  
  tp_dtxt_dtft <- (cumprob1 - cumprob0) / (1 - cumprob0)
  tp_dtxt_dtft[is.nan(tp_dtxt_dtft)] <- 1
  
  tp_dtxt_dtft[transmission_cycles:remaining_cycles] <- 1
  
  tp_dtxt_dtft[transmission_cycles] <- 1 - tp_dtxt_dth[transmission_cycles]
  
  
  ## Probability remain in transmission
  tp_dtxt_dtxt <- 1 - tp_dtxt_dtft - tp_dtxt_dth

  
  ## Adjust probabilities if option for instant death is on
  if (instant_death == 1) {
    tp_dtxt_dtft <- rep(0, length(tp_dtxt_dtft))
    tp_dtxt_dtxt <- rep(0, length(tp_dtxt_dtxt))
    tp_dtxt_dth <- rep(1, length(tp_dtxt_dth))
  }
  
  ## Transitions from transmission
  tp <- cbind(tp_dtxt_dth, tp_dtxt_dtft, tp_dtxt_dtxt)
  tp <- pmax(tp, 0) / rowSums(pmax(tp, 0))
  
  health_state <- "dtxt"
  cycle <- 1
  while(health_state == "dtxt" & cycle <= remaining_cycles) {
    new_hs <- sample(x = c("dth", "dtft", "dtxt"),
                     size = 1,
                     prob = tp[cycle, ])
    hs[current_cycle + cycle] <- new_hs
    
    health_state <- new_hs
    cycle <- cycle + 1
  }
  
  ## If patient died, all remaining health states are death
  last_hs <- hs[length(na.omit(hs))]
  if (last_hs == "dth") {hs[is.na(hs)] <- "dth"}
  
  ## Return health states
  return(hs)
  
} # End of transitions from transmission
  
  
# Transitions from deceased donor transplant failure with cancer ----
trans_dtfc <- function(hs = NULL, chars = NULL, donorchars = NULL,
                       n_cycles = NULL, cycle_length = NULL) {
  
  ## Extract patient characteristics
  agewtlst <- chars$agewtlst
  female <- chars$female
  bldgrp <- chars$bldgrp
  bldgrpo <- chars$bldgrpo
  bldgrpa <- chars$bldgrpa
  bldgrpb <- chars$bldgrpb
  bldgrpab <- chars$bldgrpab
  comorbs <- chars$comorbs
  prevtxcount <- chars$prevtxcount
  
  ## Extract donor characteristics
  agetx <- donorchars$agetx
  donordeceased <- donorchars$donordeceased
  donornew <- donorchars$donornew
  donorpbm <- donorchars$donorpbm
  donorage <- donorchars$donorage
  donorfemale <- donorchars$donorfemale
  donortype <- donorchars$donortype
  donortypedcd <- donorchars$donortypedcd
  donortypedbdscd <- donorchars$donortypedbdscd
  donortypedbdecd <- donorchars$donortypedbdecd
  donortypeliving <- donorchars$donortypeliving
  donorkdpi <- donorchars$donorkdpi
  
  ## Current health state
  current_hs <- hs[length(na.omit(hs))]
  
  ## Current and remaining cycles
  current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
  remaining_cycles <- n_cycles - current_cycle

  
  ## Update cycle start times
  cycle_starttimes <- seq(0, remaining_cycles * cycle_length * 365.25, cycle_length * 365.25)
  age_cyclestart <- agewtlst + (current_cycle * cycle_length) + (cycle_starttimes[-length(cycle_starttimes)] / 365.25)
  years_cyclestart <- seq(0, (remaining_cycles - 1) * cycle_length, cycle_length)
  
  ## Probability death
  rate <- sapply(1:remaining_cycles, function(x) {
    mortality_long_tfc$annualrate[mortality_long_tfc$age == pmin(floor(age_cyclestart), 100)[x] & 
                                    mortality_long_tfc$female == female &
                                    mortality_long_tfc$years == pmin(floor(years_cyclestart + 1), 5)[x]]
  })
  
  tp_dtfc_dth <- rate_to_prob(rate, cycle_length)
  
  ## Probability remain in deceased donor transplant failure with cancer
  tp_dtfc_dtfc <- 1 - tp_dtfc_dth
  
  ## Transitions from deceased donor cancer
  tp <- cbind(tp_dtfc_dth, tp_dtfc_dtfc)
  tp <- pmax(tp, 0) / rowSums(pmax(tp, 0))
  
  health_state <- "dtfc"
  cycle <- 1
  while(health_state == "dtfc" & cycle <= remaining_cycles) {
    new_hs <- sample(x = c("dth", "dtfc"),
                     size = 1,
                     prob = tp[cycle, ])
    hs[current_cycle + cycle] <- new_hs
    
    health_state <- new_hs
    cycle <- cycle + 1
  }
  
  ## If patient died, all remaining health states are death
  last_hs <- hs[length(na.omit(hs))]
  if (last_hs == "dth") {hs[is.na(hs)] <- "dth"}
  
  ## Return health states
  return(hs)
    
} # End transitions from deceased donor transplant failure with cancer


# Transitions from deceased donor transplant failure with transmission ----
trans_dtft <- function(hs = NULL, chars = NULL, donorchars = NULL,
                       n_cycles = NULL, cycle_length = NULL) {
  
  ## Extract patient characteristics
  agewtlst <- chars$agewtlst
  female <- chars$female
  bldgrp <- chars$bldgrp
  bldgrpo <- chars$bldgrpo
  bldgrpa <- chars$bldgrpa
  bldgrpb <- chars$bldgrpb
  bldgrpab <- chars$bldgrpab
  comorbs <- chars$comorbs
  prevtxcount <- chars$prevtxcount
  
  ## Extract donor characteristics
  agetx <- donorchars$agetx
  donordeceased <- donorchars$donordeceased
  donornew <- donorchars$donornew
  donorpbm <- donorchars$donorpbm
  donorage <- donorchars$donorage
  donorfemale <- donorchars$donorfemale
  donortype <- donorchars$donortype
  donortypedcd <- donorchars$donortypedcd
  donortypedbdscd <- donorchars$donortypedbdscd
  donortypedbdecd <- donorchars$donortypedbdecd
  donortypeliving <- donorchars$donortypeliving
  donorkdpi <- donorchars$donorkdpi
  
  ## Current health state
  current_hs <- hs[length(na.omit(hs))]
  
  ## Current and remaining cycles
  current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
  remaining_cycles <- n_cycles - current_cycle
  
  
  ## Update cycle start times
  cycle_starttimes <- seq(0, remaining_cycles * cycle_length * 365.25, cycle_length * 365.25)
  age_cyclestart <- agewtlst + (current_cycle * cycle_length) + (cycle_starttimes[-length(cycle_starttimes)] / 365.25)
  years_cyclestart <- seq(0, (remaining_cycles - 1) * cycle_length, cycle_length)
  
  ## Probability death
  rate <- sapply(1:remaining_cycles, function(x) {
    mortality_long_tfc$annualrate[mortality_long_tfc$age == pmin(floor(age_cyclestart), 100)[x] & 
                                    mortality_long_tfc$female == female &
                                    mortality_long_tfc$years == pmin(floor(years_cyclestart + 1), 5)[x]]
  })
  
  tp_dtft_dth <- rate_to_prob(rate, cycle_length)
  
  ## Probability remain in deceased donor transplant failure with cancer
  tp_dtft_dtft <- 1 - tp_dtft_dth
  
  ## Transitions from deceased donor cancer
  tp <- cbind(tp_dtft_dth, tp_dtft_dtft)
  tp <- pmax(tp, 0) / rowSums(pmax(tp, 0))
  
  health_state <- "dtft"
  cycle <- 1
  while(health_state == "dtft" & cycle <= remaining_cycles) {
    new_hs <- sample(x = c("dth", "dtft"),
                     size = 1,
                     prob = tp[cycle, ])
    hs[current_cycle + cycle] <- new_hs
    
    health_state <- new_hs
    cycle <- cycle + 1
  }
  
  ## If patient died, all remaining health states are death
  last_hs <- hs[length(na.omit(hs))]
  if (last_hs == "dth") {hs[is.na(hs)] <- "dth"}
  
  ## Return health states
  return(hs)
  
} # End transitions from deceased donor transplant failure with transmission



# Simulate all transitions ----
sim_transitions <- function(time_horizon = 25, cycle_length = 3/12,
                            scenarios = 4,
                            extra_donors_pcts = c(0, 1/340, 2/340, 7/340),
                            baseline_risk = 0.02,
                            new_risks = c(0.02,
                                          mean(c(0.02)),
                                          mean(c(0.001, 0.02)),
                                          mean(c(0.001, 0.02, rep(0.064, 5)))),
                            transmission_cycles = 3,
                            instant_death = 0) {

  ## Check that options have been specified correctly
  if ((length(extra_donors_pcts) != scenarios) | (length(new_risks) != scenarios)) {
    return("Error")
  }
  
  ## Model settings
  n_cycles <- time_horizon / cycle_length
  extra_donors_pct <- extra_donors_pcts[1]
  new_risk <- new_risks[1]

  
  ## Initialise health states
  hs <- rep(NA, n_cycles)

  
  ## Patient characteristics
  chars <- sim_patient()
  
  
  ## Simulate transitions
  seed <- floor(runif(1) * 999999999)
  set.seed(seed)
  
  trans_wtlst_result <- trans_wtlst(hs = hs, chars = chars,
                                    extra_donors_pct = extra_donors_pct,
                                    min_extra_donors_pct = min(extra_donors_pcts),
                                    max_extra_donors_pct = max(extra_donors_pcts),
                                    n_cycles = n_cycles, cycle_length = cycle_length)
  hs <- trans_wtlst_result$hs
  donordeceased <- trans_wtlst_result$donordeceased
  donornew <- trans_wtlst_result$donornew
  resimulate <- trans_wtlst_result$resimulate
  
  current_hs <- hs[length(na.omit(hs))]
  current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
  remaining_cycles <- n_cycles - current_cycle
  
  if (remaining_cycles > 0) {
    if (current_hs == "offlstc") {
      hs <- trans_offlstc(hs = hs, chars = chars, n_cycles = n_cycles, cycle_length = cycle_length)
    } else if (current_hs %in% c("dtx", "ltx")) {
      donorchars <- sim_donor(hs = hs, chars = chars, 
                              donordeceased = donordeceased, donornew = donornew,
                              cycle_length = cycle_length)
      transmission_results <- sim_transmission(hs = hs, donorchars = donorchars,
                                               baseline_risk = baseline_risk, new_risk = new_risk,
                                               min_new_risk = min(new_risks), max_new_risk = max(new_risks))
      
      hs <- transmission_results$hs
      resimulate <- max(resimulate, transmission_results$resimulate)
      
      current_hs <- hs[length(na.omit(hs))]
      current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
      remaining_cycles <- n_cycles - current_cycle
    }
  }
  
  if (remaining_cycles > 0) {
   if (current_hs == "ltx") {
     hs <- trans_ltx(hs = hs, chars = chars, donorchars = donorchars, 
                     n_cycles = n_cycles, cycle_length = cycle_length)
  
     current_hs <- hs[length(na.omit(hs))]
     current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
     remaining_cycles <- n_cycles - current_cycle
  
     if (current_hs == "ltf" & remaining_cycles > 0) {
       hs <- trans_ltf(hs = hs, chars = chars, donorchars = donorchars,
                       n_cycles = n_cycles, cycle_length = cycle_length)
     
       current_hs <- hs[length(na.omit(hs))]
       current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
       remaining_cycles <- n_cycles - current_cycle
       
       if (current_hs == "ltfc" & remaining_cycles > 0) {
         hs <- trans_ltfc(hs = hs, chars = chars, donorchars = donorchars,
                          n_cycles = n_cycles, cycle_length = cycle_length)
       }
     } else if (current_hs == "ltxc" & remaining_cycles > 0) {
       hs <- trans_ltxc(hs = hs, chars = chars, donorchars = donorchars,
                        n_cycles = n_cycles, cycle_length = cycle_length)
       
       current_hs <- hs[length(na.omit(hs))]
       current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
       remaining_cycles <- n_cycles - current_cycle
       
       if (current_hs == "ltfc" & remaining_cycles > 0) {
         hs <- trans_ltfc(hs = hs, chars = chars, donorchars = donorchars,
                          n_cycles = n_cycles, cycle_length = cycle_length)
       }
     }
   } else if (current_hs == "dtx") {
     hs <- trans_dtx(hs = hs, chars = chars, donorchars = donorchars,
                     n_cycles = n_cycles, cycle_length = cycle_length)
  
     current_hs <- hs[length(na.omit(hs))]
     current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
     remaining_cycles <- n_cycles - current_cycle
  
     if (remaining_cycles > 0) {
       if(current_hs == "dtf") {
         hs <- trans_dtf(hs = hs, chars = chars, donorchars = donorchars,
                         n_cycles = n_cycles, cycle_length = cycle_length)
  
         current_hs <- hs[length(na.omit(hs))]
         current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
         remaining_cycles <- n_cycles - current_cycle
  
         if (current_hs == "dtfc" & remaining_cycles > 0) {
           hs <- trans_dtfc(hs = hs, chars = chars, donorchars = donorchars,
                            n_cycles = n_cycles, cycle_length = cycle_length)
         }
       } else if (current_hs == "dtxc" & remaining_cycles > 0) {
         hs <- trans_dtxc(hs = hs, chars = chars, donorchars = donorchars,
                          n_cycles = n_cycles, cycle_length = cycle_length)
  
         current_hs <- hs[length(na.omit(hs))]
         current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
         remaining_cycles <- n_cycles - current_cycle
  
         if (current_hs == "dtfc" & remaining_cycles > 0) {
           hs <- trans_dtfc(hs = hs, chars = chars, donorchars = donorchars,
                            n_cycles = n_cycles, cycle_length = cycle_length)
         }
       }
     }
   } else if (current_hs == "dtxt") {
     hs <- trans_dtxt(hs = hs, chars = chars, donorchars = donorchars, 
                      instant_death = instant_death, transmission_cycles = transmission_cycles,
                      n_cycles = n_cycles, cycle_length = cycle_length)
  
     current_hs <- hs[length(na.omit(hs))]
     current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
     remaining_cycles <- n_cycles - current_cycle
  
     if (current_hs == "dtft" & remaining_cycles > 0) {
       hs <- trans_dtft(hs = hs, chars = chars, donorchars = donorchars,
                        n_cycles = n_cycles, cycle_length = cycle_length)
     }
   }
  }
  
  ## Store results from scenario 0 (current practice)
  hs0 <- hs
  if (exists("donorchars")) {
    donorchars0 <- donorchars
  } else {
    donorchars0 <- NULL
  }

  
  ## Simulate transitions for remaining scenarios
  if (scenarios == 1) {
    return(list(
      chars = chars,
      hs0 = hs0,
      donorchars0 = donorchars0
    ))
  } else if (resimulate == 0) {
    for (i in 2:scenarios) {
      assign(paste0("hs", i - 1), hs)
      if (exists("donorchars")) {
        assign(paste0("donorchars", i - 1), donorchars)
      } else {
        assign(paste0("donorchars", i - 1), NULL)
      }
    }
  } else {
    for (i in 2:scenarios) {
      
      ## Model settings
      extra_donors_pct <- extra_donors_pcts[i]
      new_risk <- new_risks[i]
      
      ## Initialise health states
      hs <- rep(NA, n_cycles)
      
      ## Simulate transitions
      set.seed(seed)
      
      trans_wtlst_result <- trans_wtlst(hs = hs, chars = chars,
                                        extra_donors_pct = extra_donors_pct,
                                        min_extra_donors_pct = min(extra_donors_pcts),
                                        max_extra_donors_pct = max(extra_donors_pcts),
                                        n_cycles = n_cycles, cycle_length = cycle_length)
      hs <- trans_wtlst_result$hs
      donordeceased <- trans_wtlst_result$donordeceased
      donornew <- trans_wtlst_result$donornew
      resimulate <- trans_wtlst_result$resimulate
      
      current_hs <- hs[length(na.omit(hs))]
      current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
      remaining_cycles <- n_cycles - current_cycle
      
      if (remaining_cycles > 0) {
        if (current_hs == "offlstc") {
          hs <- trans_offlstc(hs = hs, chars = chars,
                              n_cycles = n_cycles, cycle_length = cycle_length)
        } else if (current_hs %in% c("dtx", "ltx")) {
          donorchars <- sim_donor(hs = hs, chars = chars, 
                                  donordeceased = donordeceased, donornew = donornew,
                                  cycle_length = cycle_length)
          transmission_results <- sim_transmission(hs = hs, donorchars = donorchars,
                                                   baseline_risk = baseline_risk, new_risk = new_risk,
                                                   min_new_risk = min(new_risks), max_new_risk = max(new_risks))
          
          hs <- transmission_results$hs
          resimulate <- max(resimulate, transmission_results$resimulate)
          
          current_hs <- hs[length(na.omit(hs))]
          current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
          remaining_cycles <- n_cycles - current_cycle
        }
      }
      
      if (remaining_cycles > 0) {
        if (current_hs == "ltx") {
          hs <- trans_ltx(hs = hs, chars = chars, donorchars = donorchars,
                          n_cycles = n_cycles, cycle_length = cycle_length)
          
          current_hs <- hs[length(na.omit(hs))]
          current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
          remaining_cycles <- n_cycles - current_cycle
          
          if (current_hs == "ltf" & remaining_cycles > 0) {
            hs <- trans_ltf(hs = hs, chars = chars, donorchars = donorchars,
                            n_cycles = n_cycles, cycle_length = cycle_length)
            
            current_hs <- hs[length(na.omit(hs))]
            current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
            remaining_cycles <- n_cycles - current_cycle
            
            if (current_hs == "ltfc" & remaining_cycles > 0) {
              hs <- trans_ltfc(hs = hs, chars = chars, donorchars = donorchars,
                               n_cycles = n_cycles, cycle_length = cycle_length)
            }
          } else if (current_hs == "ltxc" & remaining_cycles > 0) {
            hs <- trans_ltxc(hs = hs, chars = chars, donorchars = donorchars,
                             n_cycles = n_cycles, cycle_length = cycle_length)
            
            current_hs <- hs[length(na.omit(hs))]
            current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
            remaining_cycles <- n_cycles - current_cycle
            
            if (current_hs == "ltfc" & remaining_cycles > 0) {
              hs <- trans_ltfc(hs = hs, chars = chars, donorchars = donorchars,
                               n_cycles = n_cycles, cycle_length = cycle_length)
            }
          }
        } else if (current_hs == "dtx") {
          hs <- trans_dtx(hs = hs, chars = chars, donorchars = donorchars,
                          n_cycles = n_cycles, cycle_length = cycle_length)
          
          current_hs <- hs[length(na.omit(hs))]
          current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
          remaining_cycles <- n_cycles - current_cycle
          
          if (remaining_cycles > 0) {
            if(current_hs == "dtf") {
              hs <- trans_dtf(hs = hs, chars = chars, donorchars = donorchars,
                              n_cycles = n_cycles, cycle_length = cycle_length)
              
              current_hs <- hs[length(na.omit(hs))]
              current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
              remaining_cycles <- n_cycles - current_cycle
              
              if (current_hs == "dtfc" & remaining_cycles > 0) {
                hs <- trans_dtfc(hs = hs, chars = chars, donorchars = donorchars,
                                 n_cycles = n_cycles, cycle_length = cycle_length)
              }
            } else if (current_hs == "dtxc" & remaining_cycles > 0) {
              hs <- trans_dtxc(hs = hs, chars = chars, donorchars = donorchars,
                               n_cycles = n_cycles, cycle_length = cycle_length)
              
              current_hs <- hs[length(na.omit(hs))]
              current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
              remaining_cycles <- n_cycles - current_cycle
              
              if (current_hs == "dtfc" & remaining_cycles > 0) {
                hs <- trans_dtfc(hs = hs, chars = chars, donorchars = donorchars,
                                 n_cycles = n_cycles, cycle_length = cycle_length)
              }
            }
          }
        } else if (current_hs == "dtxt") {
          hs <- trans_dtxt(hs = hs, chars = chars, donorchars = donorchars, 
                           instant_death = instant_death, transmission_cycles = transmission_cycles,
                           n_cycles = n_cycles, cycle_length = cycle_length)
          
          current_hs <- hs[length(na.omit(hs))]
          current_cycle <- ifelse(sum(is.na(hs)) == 0, n_cycles, min(which(is.na(hs))) - 1)
          remaining_cycles <- n_cycles - current_cycle
          
          if (current_hs == "dtft" & remaining_cycles > 0) {
            hs <- trans_dtft(hs = hs, chars = chars, donorchars = donorchars,
                             n_cycles = n_cycles, cycle_length = cycle_length)
          }
        }
      }
      
      assign(paste0("hs", i - 1), hs)
      if (exists("donorchars")) {
        assign(paste0("donorchars", i - 1), donorchars)
      } else {
        assign(paste0("donorchars", i - 1), NULL)
      }
      
    }
  }
  
  
  # Return output
  output <- list(
    chars = chars,
    hs0 = hs0,
    donorchars0 = donorchars0)
  
  if (scenarios != 1) {
    for (i in 2:scenarios) {
      output <- append(output, 
                       list(
                         get(paste0("hs", i - 1)),
                         get(paste0("donorchars", i - 1))
                         )
                       )
      names(output)[3 + (i - 1) * 2 - 1] <- paste0("hs", i - 1)
      names(output)[4 + (i - 1) * 2 - 1] <- paste0("donorchars", i - 1)
    }
  }
  
  # Return output
  return(output)
}


# Apply utilities to health states ----
apply_utilities <- function(hs = NULL, cycle_length = 3/12, discount_qalys = 0.05,
                            utility_method = "Multiply") {
  
  ## Add one cycle in wtlst at model entry
  hs <- c("wtlst", hs)
  
  ## Apply utilities to health state trace
  if (utility_method == "Floor") {
    utilities <- case_when(
      hs %in% c("wtlst", "dtf", "ltf") ~ utility_dialysis,
      hs %in% c("dtx", "ltx") ~ utility_transplant,
      hs %in% c("dtxc", "ltxc") ~ min(utility_transplant, utility_cancer),
      hs %in% c("dtxt") ~ utility_transplant,
      hs %in% c("offlstc", "dtfc", "ltfc") ~ min(utility_dialysis, utility_cancer),
      hs %in% c("dtft") ~ min(utility_dialysis, utility_transmission),
      hs %in% c("dth") ~ 0
    )
  } else if (utility_method == "Multiply") {
    utilities <- case_when(
      hs %in% c("wtlst", "dtf", "ltf") ~ utility_dialysis,
      hs %in% c("dtx", "ltx") ~ utility_transplant,
      hs %in% c("dtxc", "ltxc") ~ utility_transplant * utility_cancer,
      hs %in% c("dtxt") ~ utility_transplant,
      hs %in% c("offlstc", "dtfc", "ltfc") ~ utility_dialysis * utility_cancer,
      hs %in% c("dtft") ~ utility_dialysis * utility_transmission,
      hs %in% c("dth") ~ 0
    )
  } else if (utility_method == "Add") {
    utilities <- case_when(
      hs %in% c("wtlst", "dtf", "ltf") ~ utility_dialysis,
      hs %in% c("dtx", "ltx") ~ utility_transplant,
      hs %in% c("dtxc", "ltxc") ~ 1 - ((1 - utility_transplant) + (1 - utility_cancer)),
      hs %in% c("dtxt") ~ utility_transplant,
      hs %in% c("offlstc", "dtfc", "ltfc") ~ 1 - ((1 - utility_dialysis) + (1 - utility_cancer)),
      hs %in% c("dtft") ~ 1 - ((1 - utility_dialysis) + (1 - utility_transmission)),
      hs %in% c("dth") ~ 0
    )
  }

  ## Apply half cycle correction
  utilities <- as.numeric(na.omit((utilities + lag(utilities)) / 2))
  
  
  ## Multiply utilities by cycle length
  utilities <- utilities * cycle_length
  

  ## Apply discounting to utilities
  utilities_discounted <- sapply(1:length(utilities), function(x) {
    utilities[x] * ((1 - discount_qalys) ^ ((x - 1) * cycle_length))
    })
  
  ## Calculate total QALYs
  total_qalys <- sum(utilities)
  
  total_qalys_discounted <- sum(utilities_discounted)
    
  
  ## Return output
  output <- list(utilities = utilities,
                 utilities_discounted = utilities_discounted,
                 total_qalys = total_qalys,
                 total_qalys_discounted = total_qalys_discounted)
  return(output)
}



# Apply costs to health states and transitions ----
apply_costs <- function(hs = NULL, cycle_length = 3/12, discount_costs = 0.05, 
                        transmission_cycles = NULL) {

  ## Add one cycle in wtlst at model entry
  hs <- c("wtlst", hs)
  
  ## Create health states for each separate cost
  dialysis_hs <- if_else(hs %in% c("wtlst", "offlstc", "ltf", "ltfc", "dtf", "dtfc", "dtft"), 1, 0)
  tx_hs <- if_else(hs %in% c("ltx", "dtx", "dtxt", "dtxc", "ltxc"), 1, 0)
  cancer_hs <- if_else(hs %in% c("offlstc", "dtxc", "dtfc", "ltfc"), 1, 0)
  transmission_hs <- if_else(hs %in% c("dtft"), 1, 0)
  
  ## Determine which period each health state occurs in
  period <- function(hs, cycle_length) {
    x <- 0
    period <- NULL
    for (i in 1:length(hs)) {
      if (i == 1) {
        if (hs[i] == 0) {
          period[i] <- NA
          x <- 0
        } else if (hs[i] == 1) {
          period[i] <- 0
          x <- 0
        }
      } else if (hs[i] == 0) {
        period[i] <- NA
        x <- 0
      } else if (hs[i] == 1) {
        x <- x + 1
        period[i] <- x
      }
    }
    years <- period * cycle_length
    period <- case_when(
      years <= 0.25 ~ "m1_m3",
      years <= 1 ~ "m4_m12",
      years <= 2 ~ "m13_m24",
      years <= 3 ~ "m25_m36",
      years <= 4 ~ "m37_m48",
      !is.na(years) ~ "m49_plus"
    )
    return(period)
  }

  dialysis_period <- period(dialysis_hs, cycle_length = cycle_length)
  tx_period <- period(tx_hs, cycle_length = cycle_length)
  cancer_period <- period(cancer_hs, cycle_length = cycle_length)
  transmission_period <- period(transmission_hs, cycle_length = cycle_length)
  
  
  ## Determine costs based on health states
  dialysis_costs <- case_when(
    dialysis_period == "m1_m3" ~ cost_dialysis_m1_m3 * (cycle_length / 0.25),
    dialysis_period == "m4_m12" ~ cost_dialysis_m4_m12 * (cycle_length / 0.25),
    dialysis_period == "m13_m24" ~ cost_dialysis_m13_m24 * (cycle_length / 0.25),
    dialysis_period == "m25_m36" ~ cost_dialysis_m25_m36 * (cycle_length / 0.25),
    dialysis_period == "m37_m48" ~ cost_dialysis_m37_m48 * (cycle_length / 0.25),
    dialysis_period == "m49_plus" ~ cost_dialysis_m49_plus * (cycle_length / 0.25),
    TRUE ~ 0
  )
    
  tx_costs <- case_when(
    tx_period == "m1_m3" ~ cost_tx_m1_m3 * (cycle_length / 0.25),
    tx_period == "m4_m12" ~ cost_tx_m4_m12 * (cycle_length / 0.25),
    tx_period == "m13_m24" ~ cost_tx_m13_m24 * (cycle_length / 0.25),
    tx_period == "m25_m36" ~ cost_tx_m25_m36 * (cycle_length / 0.25),
    tx_period == "m37_m48" ~ cost_tx_m37_m48 * (cycle_length / 0.25),
    tx_period == "m49_plus" ~ cost_tx_m49_plus * (cycle_length / 0.25),
    TRUE ~ 0
  )
  
  cancer_costs <- case_when(
    cancer_period == "m1_m3" ~ cost_cancer_m1_m3 * (cycle_length / 0.25),
    cancer_period == "m4_m12" ~ cost_cancer_m4_m12 * (cycle_length / 0.25),
    cancer_period == "m13_m24" ~ cost_cancer_m13_m24 * (cycle_length / 0.25),
    cancer_period == "m25_m36" ~ cost_cancer_m25_m36 * (cycle_length / 0.25),
    cancer_period == "m37_m48" ~ cost_cancer_m37_m48 * (cycle_length / 0.25),
    cancer_period == "m49_plus" ~ cost_cancer_m49_plus * (cycle_length / 0.25),
    TRUE ~ 0
  )
  
  transmission_costs <- case_when(
    transmission_period == "m1_m3" ~ cost_transmission_m1_m3 * (cycle_length / 0.25),
    transmission_period == "m4_m12" ~ cost_transmission_m4_m12 * (cycle_length / 0.25),
    transmission_period == "m13_m24" ~ cost_transmission_m13_m24 * (cycle_length / 0.25),
    transmission_period == "m25_m36" ~ cost_transmission_m25_m36 * (cycle_length / 0.25),
    transmission_period == "m37_m48" ~ cost_transmission_m37_m48 * (cycle_length / 0.25),
    transmission_period == "m49_plus" ~ cost_transmission_m49_plus * (cycle_length / 0.25),
    TRUE ~ 0
  )
  
  hs_costs <- dialysis_costs + tx_costs + cancer_costs + transmission_costs
  
  
  ## Apply half cycle correction to costs
  hs_costs <- as.numeric(na.omit((hs_costs + lag(hs_costs)) / 2))
  
  
  ## Determine event/transition costs
  event_cost_ltx <- (sum(hs %in% c("ltx")) > 0) * cost_ltx
  event_cost_dtx <- (sum(hs %in% c("dtx")) > 0) * cost_dtx
  event_cost_dtft <- (sum(hs %in% c("dtft")) > 0) * cost_nephrectomy
  
  event_costs <- event_cost_ltx + event_cost_dtx + event_cost_dtft
    
    
  ## Apply discounting to costs
  hs_costs_discounted <- sapply(1:length(hs_costs), function(x) {
    hs_costs[x] * ((1 - discount_costs) ^ ((x - 1) * cycle_length))
  })
  
  event_cost_ltx_discounted <- 0
  if (sum(hs %in% c("ltx")) > 0) {
    event_cost_ltx_discounted <- event_cost_ltx * 
      ((1 - discount_costs) ^ ((min(which(hs %in% c("ltx"))) - 1) * cycle_length))
  }
  
  event_cost_dtx_discounted <- 0
  if (sum(hs %in% c("dtx")) > 0) {
    event_cost_dtx_discounted <- event_cost_dtx * 
      ((1 - discount_costs) ^ ((min(which(hs %in% c("dtx"))) - 1) * cycle_length))
  }
  
  event_cost_dtft_discounted <- 0
  if (sum(hs %in% c("dtft")) > 0) {
    event_cost_dtft_discounted <- event_cost_dtft * 
      ((1 - discount_costs) ^ ((min(which(hs %in% c("dtft"))) - 1) * cycle_length))
  }
  
  event_costs_discounted <- event_cost_ltx_discounted + event_cost_dtx_discounted + event_cost_dtft_discounted

  ## Calculate total costs
  total_costs <- sum(hs_costs) + event_costs
  
  total_costs_discounted <- sum(hs_costs_discounted) + event_costs_discounted
  
  
  ## Return output
  output <- list(hs_costs = hs_costs,
                 hs_costs_discounted = hs_costs_discounted,
                 event_costs = event_costs,
                 event_costs_discounted = event_costs_discounted,
                 total_costs = total_costs,
                 total_costs_discounted = total_costs_discounted)
  return(output)
}


# Function to run model over multiple patients ----
run_model <- function(n_patients = 1, display_progress = TRUE, 
                      seeds = floor(runif(n_patients) * 999999999),
                      time_horizon = 25, cycle_length = 3/12,
                      scenarios = 4,
                      extra_donors_pcts = c(0, 1/340, 2/340, 7/340),
                      baseline_risk = 0.02,
                      new_risks = c(0.02,
                                    mean(c(0.02)),
                                    mean(c(0.001, 0.02)),
                                    mean(c(0.001, 0.02, rep(0.064, 5)))),
                      transmission_cycles = 3,
                      instant_death = 0,
                      utility_method = "Multiply") {
  
  ## Check the correct number of seeds have been provided
  if (length(seeds) < n_patients) {
    return("Not enough seeds provided")
  }

  ## Create a data frame to store results for each patient
  results <- data.frame(
    patient = 1:n_patients,
    seed = seeds,
    #
    agewtlst = rep(NA, n_patients),
    female = rep(NA, n_patients),
    bldgrpo = rep(NA, n_patients),
    bldgrpa = rep(NA, n_patients),
    bldgrpb = rep(NA, n_patients),
    bldgrpab = rep(NA, n_patients),
    prevtxcount = rep(NA, n_patients),
    comorbs = rep(NA, n_patients),
    #
    costs0 = rep(NA, n_patients),
    qalys0 = rep(NA, n_patients),
    lys0 = rep(NA, n_patients),
    ltx0 = rep(NA, n_patients),
    ltx_lys0 = rep(NA, n_patients),
    dtx0 = rep(NA, n_patients),
    dtx_lys0 = rep(NA, n_patients),
    transmission0 = rep(NA, n_patients)
  )
  
  if(scenarios >= 2) {
    for(scenario in 2:scenarios) {
      results[, paste0("costs", scenario - 1)] <- NA
      results[, paste0("qalys", scenario - 1)] <- NA
      results[, paste0("lys", scenario - 1)] <- NA
      results[, paste0("ltx", scenario - 1)] <- NA
      results[, paste0("ltx_lys", scenario - 1)] <- NA
      results[, paste0("dtx", scenario - 1)] <- NA
      results[, paste0("dtx_lys", scenario - 1)] <- NA
      results[, paste0("transmission", scenario - 1)] <- NA
      
      results[, paste0("incremental_costs", scenario - 1)] <- NA
      results[, paste0("incremental_qalys", scenario - 1)] <- NA
      results[, paste0("worse", scenario - 1)] <- NA
      results[, paste0("same", scenario - 1)] <- NA
      results[, paste0("better", scenario - 1)] <- NA
      results[, paste0("incremental_lys", scenario - 1)] <- NA
      results[, paste0("extra_ltx", scenario - 1)] <- NA
      results[, paste0("incremental_ltx_lys", scenario - 1)] <- NA
      results[, paste0("extra_dtx", scenario - 1)] <- NA
      results[, paste0("incremental_dtx_lys", scenario - 1)] <- NA
      results[, paste0("extra_transmissions", scenario - 1)] <- NA
    }
  }
  
  variables <- colnames(results)[-(1:2)]
  
  
  ## Loop over each patient
  for (patient in 1:n_patients) {
    
    ## Display progress
    if (display_progress == TRUE) {
      print(paste0("Patient ", patient, " of ", n_patients))
    }
    
    ## Simulate transitions
    set.seed(seeds[patient])
    transitions <- sim_transitions(time_horizon = time_horizon, 
                                   cycle_length = cycle_length,
                                   scenarios = scenarios,
                                   extra_donors_pcts = extra_donors_pcts,
                                   baseline_risk = baseline_risk,
                                   new_risks = new_risks,
                                   transmission_cycles = transmission_cycles,
                                   instant_death = instant_death)
    
    chars <- transitions$chars
    agewtlst <- chars$agewtlst
    female <- chars$female
    bldgrpo <- chars$bldgrpo
    bldgrpa <- chars$bldgrpa
    bldgrpb <- chars$bldgrpb
    bldgrpab <- chars$bldgrpab
    prevtxcount <- chars$prevtxcount
    comorbs <- chars$comorbs
    
    hs0 <- transitions$hs0
    qalys0 <- apply_utilities(hs = hs0)$total_qalys_discounted
    costs0 <- apply_costs(hs = hs0, transmission_cycles = transmission_cycles)$total_costs_discounted
    lys0 <- sum(!(hs0 %in% c("dth"))) * cycle_length 
    ltx0 <- (sum(hs0 %in% c("ltx")) > 0) * 1
    ltx_lys0 <- sum(hs0 %in% c("ltx", "ltxc")) * cycle_length 
    dtx0 <- (sum(hs0 %in% c("dtx", "dtxt")) > 0) * 1
    dtx_lys0 <- sum(hs0 %in% c("dtx", "dtxt", "dtxc")) * cycle_length 
    transmission0 <- (sum(hs0 %in% c("dtxt")) > 0) * 1

    if (scenarios >= 2) {
      for(scenario in 2:scenarios) {
        assign(paste0("hs", scenario - 1), 
               transitions[[paste0("hs", scenario - 1)]])
        assign(paste0("qalys", scenario - 1), 
               apply_utilities(hs = get(paste0("hs", scenario - 1)), 
                               utility_method = utility_method)$total_qalys_discounted)
        assign(paste0("costs", scenario - 1), 
               apply_costs(hs = get(paste0("hs", scenario - 1)), transmission_cycles = transmission_cycles)$total_costs_discounted)
        assign(paste0("lys", scenario - 1), 
               sum(!(get(paste0("hs", scenario - 1)) %in% c("dth"))) * cycle_length)
        assign(paste0("ltx", scenario - 1), 
               (sum(get(paste0("hs", scenario - 1)) %in% c("ltx")) > 0) * 1)
        assign(paste0("ltx_lys", scenario - 1), 
               sum(get(paste0("hs", scenario - 1)) %in% c("ltx", "ltxc")) * cycle_length)
        assign(paste0("dtx", scenario - 1), 
               (sum(get(paste0("hs", scenario - 1)) %in% c("dtx", "dtxt")) > 0) * 1)
        assign(paste0("dtx_lys", scenario - 1), 
               sum(get(paste0("hs", scenario - 1)) %in% c("dtx", "dtxt", "dtxc")) * cycle_length)
        assign(paste0("transmission", scenario - 1), 
               (sum(get(paste0("hs", scenario - 1)) %in% c("dtxt")) > 0) * 1)
        assign(paste0("incremental_costs", scenario - 1), 
               get(paste0("costs", scenario - 1)) - costs0)
        assign(paste0("incremental_qalys", scenario - 1), 
               get(paste0("qalys", scenario - 1)) - qalys0)
        assign(paste0("worse", scenario - 1), 
               (get(paste0("qalys", scenario - 1)) < qalys0) * 1)
        assign(paste0("same", scenario - 1), 
               (get(paste0("qalys", scenario - 1)) == qalys0) * 1)
        assign(paste0("better", scenario - 1), 
               (get(paste0("qalys", scenario - 1)) > qalys0) * 1)
        assign(paste0("incremental_lys", scenario - 1), 
               get(paste0("lys", scenario - 1)) - lys0)
        assign(paste0("extra_ltx", scenario - 1), 
               get(paste0("ltx", scenario - 1)) - ltx0)
        assign(paste0("incremental_ltx_lys", scenario - 1), 
               get(paste0("ltx_lys", scenario - 1)) - ltx_lys0)
        assign(paste0("extra_dtx", scenario - 1), 
               get(paste0("dtx", scenario - 1)) - dtx0)
        assign(paste0("incremental_dtx_lys", scenario - 1), 
               get(paste0("dtx_lys", scenario - 1)) - dtx_lys0)
        assign(paste0("extra_transmissions", scenario - 1), 
               get(paste0("transmission", scenario - 1)) - transmission0)
      }
    }

    ## Update table of results
    results[patient, 3:ncol(results)] <- sapply(variables, function(x) get(x))

  } # End of loop over patients
  
  ## Return table of results
  return(results)
  
} # End of function to run model


# Test model ----
# set.seed(123)
# n_patients <- 1000
# seeds <- floor(runif(n_patients) * 999999999)
# results <- run_model(n_patients = n_patients, seeds = seeds, display_progress = TRUE)

