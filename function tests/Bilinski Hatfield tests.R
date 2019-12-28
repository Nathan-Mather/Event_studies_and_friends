

#===============================================#
# ==== Tests Bilinski and Hatfield Function ====
#===============================================#


  # - Keywords  
  #  - #note I will use the tags #note for things I think are important
  #  - #set will be used for things that need to be set for your specific
  
  # - purpose of code:
  # Tests for the Bilinski and Hatfield style pretrends Function
  
  
  # claer objects and code script to get a fresh start to test this 
  rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
  options(scipen = 999)
  cat("\f")

#=====================================#
# ==== Load packages and function ====
#=====================================#

  
  library(data.table)
  library(TOSTER)
  
  # # Note that this functon helped me figure some stuff out 
  # View(TOSTtwo)
  
  # set the path of the function 
  source("c:/Users/Nmath_000/Documents/Code/Event_studies_and_friends/functions/Bilinski_Hatfield_function.R")

#================================#
# ==== create simulated data ====
#================================#

  #===============================#
  # ==== true paralell trends ====
  #===============================#
  
  # start bhy setting parameters for the simulation 
  # later on I can calcualte these for specific test of the funciton but for now just pick some 
  n_treat <- 250 
  n_control <- 250 
  n_periods <- 4
  sig  <- 2
  B     <- 1
  dif     <- 5
  
  
  # create a data set with treatment and control observations 
  t1_dt <- data.table(ID = rep(1:(n_treat + n_control),n_periods))
  t1_dt[, treat := as.numeric(ID <= n_treat)]
  
  # put period indicators in there 
  t1_dt[, period := 1:n_periods, ID]
  
  
  # generate white noise for every observation 
  t1_dt[, epsilon := rnorm(nrow(t1_dt), 0, sig)]
  
  # Now generate simulated observations of y with noise 
  t1_dt[treat == 0, y := B*period + epsilon ]
  t1_dt[treat == 1, y :=dif + B*period + epsilon ]
  
  # set parms for debug 
  in_data = t1_dt
  in_treat_var = "treat"
  in_time_var = "period"
  in_outcome_var = "y"
  in_threshold   = 1
  
  # run function 
  HB_pretrends(in_data        = t1_dt,
               in_treat_var   = "treat",
               in_time_var    = "period",
               in_outcome_var = "y",
               in_threshold   = 3)

  HB_pretrends(in_data        = t1_dt,
               in_treat_var   = "treat",
               in_time_var    = "period",
               in_outcome_var = "y",
               in_threshold   = .01)
  
  
  #============================#
  # ==== unparalell trends ====
  #============================#
  
  B1     <- 1
  B2     <- 5
  
  
  
  # create a data set with treatment and control observations 
  t2_dt <- data.table(ID = rep(1:(n_treat + n_control),n_periods))
  t2_dt[, treat := as.numeric(ID <= n_treat)]
  
  # put period indicators in there 
  t2_dt[, period := 1:n_periods, ID]
  
  
  # generate white noise for every observation 
  t2_dt[, epsilon := rnorm(nrow(t2_dt), 0, sig)]
  
  # Now generate simulated observations of y with noise 
  t2_dt[treat == 0, y := B1*period + epsilon ]
  t2_dt[treat == 1, y :=dif + B2*period + epsilon ]
  
  plot(t2_dt$y ~ t2_dt$period)
  
  # set parms for debug 
  in_data = t2_dt
  in_treat_var = "treat"
  in_time_var = "period"
  in_outcome_var = "y"
  in_threshold   = .5
  
  
  HB_pretrends(in_data        = t2_dt,
               in_treat_var   = "treat",
               in_time_var    = "period",
               in_outcome_var = "y",
               in_threshold   = .5)
  
  HB_pretrends(in_data        = t2_dt,
               in_treat_var   = "treat",
               in_time_var    = "period",
               in_outcome_var = "y",
               in_threshold   = .00000000001)
  
  HB_pretrends(in_data        = t2_dt,
               in_treat_var   = "treat",
               in_time_var    = "period",
               in_outcome_var = "y",
               in_threshold   = 4.2)
  
  
  
  