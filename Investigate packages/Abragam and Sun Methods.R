#==========================#
# ==== Abraham and Sun ====
#==========================#


# - Keywords  
#  - #set will be used for things that need to be set for your specific
#       file structure to get the code to run. Things like data directories 
#  - #fix will be used for things that need to be fixed 
#  - #note I will use the tags #note for things I think are important
# - #check is where I do a check not present in the original stata code in order to make sense of what they are doing or
# to match some numbers and make sure I did it right 

# - purpose of code:
# Get working examples of methods from Abraham and Sun 
# this is taken from the Stata code for their paper 

# -requirments to run 
# Data.table package 
# haven package : This is to load the data but not needed for the analysis  
# lfe package 
# HRS_long.dta data set 
# systemfit package to run seamingly unrealted regression (suest in stata
# survey # needed tfor the linear combinations 


# Data description: arent super clear about this so here are my notes on how I understand it 

# hhidpn: is the unique identifier per person 

# wave: is what year/wave the survey is in. It is essentailly the time marker. Each person will have multple 
# entries for wave since it is a panal. After we make it balanced everyone should have 7-11 

# event_time: is when the event (first hospitalization) happened relative to current time. it will be zero in the wave/year that the first hospitalization
# happened -1 a year before it, 1 a year after it etc.

#====================#
# ==== load data ====
#====================#

  # clear objects 
  rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
  options(scipen = 999)
  cat("\f")

  library(data.table)
  # library(RCurl)
  library(lfe)  # for regressions 
  library(systemfit) # for suest in stata
  library(lmtest)    # for robust ste 
  library(sandwich)   # for robust ste 
  library(survey)    # for linear combos 
  
  
  
  
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
   
   # rename the wave dummies to match what they did in stata 
   setnames(hrs, paste0("wave_", c(1:5)), paste0("wave_", c(7:11)))
  
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
   dum_vars <- setdiff(dum_vars, c("wave_7", "evt_time_1", "evt_time_4"))
   
   # make formula 
   form <- as.formula(paste0("oop_spend ~ ",
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
   
   # we have controls for the wave that ypu are in. This is your T and is a control not the outcome of interest 
   # Then we have The event time. This is the wave relative to the hospitalizaiton 
   reg_res <- felm(form,
                   data = hrs[ever_hospitalized == 1])
   
   # check attributes 
   attributes(reg_res)
   
   # put results in a data.table 
   #check data with stata. Std errors are wrong like I said earlier 
   data.table(term       = rownames(reg_res$coefficients),
               estimate   = reg_res$coefficients,
               robust_ste = reg_res$cse,
               t          = reg_res$ctval,
               p_val      = reg_res$cpval)
               
   # Firt, set evt_time dummy to zero for any wave_hosp == 11 
   # that is for anyone with a first hospitalization after 10, make them untreated as they are a control group 
   evt_tim_vars <- grep("evt_time_", colnames(hrs), value = TRUE)
   for(var_i in evt_tim_vars){
     
     hrs[wave_hosp == 11, (var_i) := 0]
   }
   
   # now copy hrs. This is the "preserve" in stata 
   hrs_temp <- copy(hrs)
   
   for(var_i in evt_tim_vars){
     
     hrs_temp[wave_hosp == 11, (var_i) := NA]
   }
   
   
#================================#
# ==== compare data to stata ====
#================================#

 
   #check out data 
   # this is where I got a sense pf what all the variables mean. 
   test <- hrs_temp[, c( "hhidpn", "wave", "wave_hosp", "evt_time" )]
   test[, hhidpn := as.character(hhidpn)]
   
   tab <- setorder(hrs_temp[wave < 11, .N, evt_time], evt_time)
   tab 
   
   hrs_temp[wave < 11, .N, evt_time_3]
   hrs_temp[wave < 11, .N, evt_time_4]
   hrs_temp[wave < 11, .N, evt_time_5]
   hrs_temp[wave < 11, .N, evt_time_6]
   hrs_temp[wave < 11, .N, evt_time_7]
   
#==============================#
# ==== Continue estimation ====
#==============================#

   
   # run regressions of first time hospitalized on event time dummies. 
   # pretty confused on why they are doing this because it seems determinisutic. If I have wave_hosp_3 (wave_hosp == 10) for example
   # then I will for sure have event times 0, -1, -2, -3 
   # get lhs vars 
   lhs_list <-  paste0("wave_hosp_", c(1:3))
   # get rhs vars 
   rhs <- paste0(paste0("evt_time_", c(3:7)),collapse = " + ")
   # make the three formulas 
   formula_list <- lapply(paste0(lhs_list, "~", rhs, "-1"), as.formula)
   

   # # run the three regs 
   # reg_res <- lapply(formula_list, lm, data = hrs_temp[ wave < 11]  )
   # lapply(reg_res, broom::tidy)
   
   
   # run system fit 
   #note this is also not matching. Need to figure it out but I can not right now so mocing on 
   names(formula_list) <- c("eq1", "eq2", 'eq3')
   sur_res <- systemfit(formula_list, method = "SUR", data = hrs_temp[wave<11])
   Vcov <- vcov(sur_res)
   
   
   #===========================#
   # ==== restore in stata ====
   #===========================#
   
  # go back to using regular hrs data 
   rm(hrs_temp)
   
  # here they are creating another indicator 
   # it is 1 if anf only if the first number corresponds to the event time relative to the current wave and the second number 
   # corresponds to the actual wave in whiich hospitalization occured 
  for(vv in evt_tim_vars){
    for(i in 8:11){
      print(paste0(vv, "_", i))
      hrs[,  (paste0(vv, "_", i)) := get(vv)*as.numeric(wave_hosp == i)]
      
    }
    
  }
   
  # make a list of the relevent variables like they do 
   rhs_rel_year_i <- c("evt_time_5_8", "evt_time_6_8", "evt_time_7_8", "evt_time_3_9", "evt_time_5_9", "evt_time_6_9", "evt_time_2_10", "evt_time_3_10", "evt_time_5_10")
  
  # try to make sesne of why they are picking only those 
  print(setorder(hrs[wave < 11, .N, c( "evt_time", "wave_hosp")], wave_hosp, evt_time))
  # so you van see they are picking those because that is all of the wave event time combinations that are present in 
  # waves 8-10 and they are leaving  event time 4 ( event_time == -1) 
  # off for each group as the reference gourp. 
   
    
  rhs_vars <- paste0("wave_", c(8:10))
  rhs_vars <- c(rhs_rel_year_i, rhs_vars)
   
  
  # make formula 
  form <- as.formula(paste0("oop_spend ~ ",
                            paste0(rhs_vars, collapse = " + " ),
                            "| hhidpn |0| hhidpn"))
  
  hrs[ever_hospitalized == 1 & wave < 11, .N, rhs_vars]
  
  reg_res2 <- felm(form,
                  data = hrs[ever_hospitalized == 1 & wave < 11])
  
  # check attributes 
  attributes(reg_res)
  
  # put results in a data.table 
  #check data with stata. Std errors are wrong like I said earlier 
  # estimates seem to match though 
  reg_tab_2 <- data.table( term       = rownames(reg_res2$coefficients),
                           estimate   = reg_res2$coefficients,
                           robust_ste = reg_res2$cse,
                           t          = reg_res2$ctval,
                           p_val      = reg_res2$cpval)
                
  svycontrast(reg_res2, c("evt_time_2_10" = 1))

  svycontrast(reg_res2, c("evt_time_3_9" = c_9/(c_9 + c_10), "evt_time_3_10" = c_10/(c_9 + c_10)))
