#=========================================#
# ==== Bilinski and Hatfield Function ====
#=========================================#

# - Keywords  
#  - #note I will use the tags #note for things I think are important
#  - #set will be used for things that need to be set for your specific

# - purpose of code:
# use the method from the bilinski and Hatfield function to impliment their test on a pretrends test
# there extra stuff around the function for debugging. Specifically I make some simulated data in here.
# we can move that to a seperate testing script once we are closer to a final version 

# -requirments to run 
# Data.table package: I love data.table and it's fast. IMO it is worth the dependency. 


# claer objects and code script to get a fresh start to test this 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")


#================================#
# ==== create simulated data ====
#================================#
library(data.table)


  #===============================#
  # ==== true paralell trends ====
  #===============================#
    
    # start bhy setting parameters for the simulation 
    # later on I can calcualte these for specific test of the funciton but for now just pick some 
    n_treat <- 250 
    n_control <- 250 
    n_periods <- 4
    sig  <- .5
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
    
    

#================================#
# ==== Roxygen documentation ====
#================================#

#' HB_pretrends
#' 
#' Impliments a pretrends test in the style of Bilinski & HatfieldSeeking "evidence of absence: Reconsidering tests of model assumptions". 
#' Retrieved from http://arxiv.org/abs/1805.03273
#'
#'@param in_data Your data set entered as a data.table
#'@param in_treat_var Treatment variable name 
#'@param in_time_var Time variable name 
#'@param in_outcome_var outcome variable name 
#'@param in_threshold Acceptable threshold for differences in time coefficient
#'@details See the paper 
#'@examples 


#=========================#
# ==== start function ====
#=========================#

    
 HB_pretrends <- function(in_data, in_treat_var, in_time_var, in_outcome_var, in_threshold ){
   
   # grab out relevent data 
   
   # chage column names for ease 
   
   # create interaction terms 
   
   # run regression 
   
   # Test for coefficients being withing the threshold 
   # could use something like wald.test, or write from scratch 
   
   
   
   
   
 }



