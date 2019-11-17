#==========================#
# ==== Abraham and Sun ====
#==========================#


# - Keywords  
#  - #set will be used for things that need to be set for your specific
#       file structure to get the code to run. Things like data directories 
#  - #fix will be used for things that need to be fixed 
#  - #note I will use the tags #note for things I think are important

# - purpose of code:
# Get working examples of methods from Abraham and Sun 
# this is taken from the Stata code for their paper 

# -requirments to run 
# Data.table package 
# haven package : This is to load the data but not needed for the analysis  
# lfe package 
# HRS_long.dta data set 


#====================#
# ==== load data ====
#====================#

  # clear objects 
  rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
  options(scipen = 999)
  cat("\f")

  library(data.table)
  library(RCurl)
  library(lfe)
  
  
  
  # # trying to get working version of felm that will match reghdfe 
  # script1 <- getURL("https://raw.githubusercontent.com/sgaure/lfe/master/R/utils.R")
  # script <- getURL("https://raw.githubusercontent.com/sgaure/lfe/66f9c0fab184b366ed60b9732475b58a6acee5dd/R/felm.R", ssl.verifypeer = FALSE)
  # eval(parse(text = script1))
  # eval(parse(text = script))
  # 
  
  # library(lfe)
  
#set loaciton of data set 
  file_path <-  "C:/Users/Nmath_000/Documents/MI_school/Event_Study_and_friends/replication_code_Sun_Abraham/replication code/"
  
  # load data 
  hrs_raw <- data.table(haven::read_dta(paste0(file_path, "HRS_long.dta")))

  # cope this for not while im messing with it 
  hrs <- copy(hrs_raw)
  
#========================#
# ==== Preliminaries ====
#========================#

  # keep balanced sasmple for wave 7-11 
  hrs <- hrs[ wave >= 7]
  hrs[, N := .N, hhidpn]
  hrs <- hrs[N == 5]
  
  # Flag minimum event timee
  hrs[, flag := min(evt_time), hhidpn]


  # drop those first hospitalization happened before or during wave 7
  # note this is because event_time is coded odly 
  hrs <- hrs[flag < 0 ]
  
  # get rid of the variable 
  hrs[, flag := NULL]
  
  # fill in the wave of index hosp within an hhidpn
  # this is essentially getting thier first hopsitalization since the authors 
  # are looking at the treatment of "being hospitalize" as a sticky treatment 
  hrs[, wave_hosp := min(wave_hosp, na.rm = TRUE), hhidpn ]
  
  hrs[, .N, c("ever_hospitalized", "wave_hosp", "wave", "evt_time")]
  nrow(hrs[wave == 7])
  hrs[wave == 7, .N, wave_hosp]
  
  # // keep a sample of individuals who were ever hospitalized wave 8-11
  hrs <- hrs[ever_hospitalized == 1]
  
  # funciton to create dummies 
  # just have it edit the input function because that is easier even though not great coding 
   dummy_f <- function(in_data, in_var){
    # get list of value sfor the variable 
    values <- sort(in_data[, unique(get(in_var))])
    
    # for each value, create a dummy
    for(i in 1:length(values)){
      # get value 
      val_i <- values[[i]]
      # create a variable name.
      # I'll do this to match the way they did evt_time
      var_name <- paste0(in_var, "_", i)
      
      in_data[get(in_var) == val_i, (var_name) := 1]
      in_data[get(in_var) != val_i, (var_name) := 0]
      
  
    }
    # return NULL since I am just editing the input data.set in global env
  }
 
   # now make dummies for wave, evt_time, wave_hosp
   lapply(c("wave", "evt_time", "wave_hosp"), dummy_f, in_data = hrs)
  
   # Generate counts for each cohort
   c_7 <- hrs[wave_hosp == 7 & wave ==7, .N]
   c_8 <- hrs[wave_hosp == 8 & wave ==7, .N]
   c_9 <- hrs[wave_hosp == 9 & wave ==7, .N]
   c_10 <- hrs[wave_hosp == 10 & wave ==7, .N]
   
   # subset to under 60 
   hrs <- hrs[age_hosp <= 59]
   
   
#=====================#
# ==== Estimation ====
#=====================#

   # get all the dummy variables we need 
   dum_vars <- grep("evt_time_|wave_[[:digit:]]", colnames(hrs), value = TRUE)
   
   # leave one of each out 
   dum_vars <- setdiff(dum_vars, c("wave_1", "evt_time_1", "evt_time_4"))
   
   # make formula 
   
    <- as.formula(paste0("oop_spend ~ ",
                             paste0(dum_vars, collapse = " + " ),
                             "| hhidpn |0| hhidpn"))
   # run the regression 
   # the results dont match reghdfe. Something to do with degrees of freedom calculcations or soemthing. Don't really follow it but 
   # here is a github thread about it https://github.com/sgaure/lfe/issues/1
   # There is a Pull Request to add a method to equate the standard errors here https://github.com/sgaure/lfe/pull/26
   # the commented out code would work after that request or if I can get that code puled from github to work 
   # for now the standard errors will just be off 
   
   # reg_res <- felm(form,
   #                 data = hrs[ever_hospitalized == 1],
   #                 cmethod = "reghdfe")
   
   # we have controls for the wave that oyu are in. This is your T and is a control not the outcome of interest 
   # Then we have 
   reg_res <- felm(form,
                   data = hrs[ever_hospitalized == 1])
   
   # check attributes 
   attributes(reg_res)
   
   # put results in a data.table 
   data.table(term       = rownames(reg_res$coefficients),
               estimate   = reg_res$coefficients,
               robust_ste = reg_res$cse,
               t          = reg_res$ctval,
               p_val      = reg_res$cpval)
               
   # set evt_time_x to missing if wave_hosp == 11. Not really clear to me yet why but this is what they do
   evt_tim_vars <- grep("evt_time_", colnames(hrs), value = TRUE)
   for(var_i in evt_tim_vars){
     
     hrs[wave_hosp == 11, (var_i) := NA]
   }
   
   # run regressions of 
   
   tab <- setorder(hrs[, .N, wave_hosp],wave_hosp)
   tab
  
  
  
  
  

