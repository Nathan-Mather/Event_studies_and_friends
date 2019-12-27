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
# happened, -1 a year before it, 1 a year after it etc.

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
  
  #set a directory to save out data after preliminaries to use as test data for funciton 
  out_path <- "C:/Users/Nmath_000/Documents/MI_school/Event_Study_and_friends/Funciton_test_data/"
  
  #set loaciton of data set 
  file_path <-  "C:/Users/Nmath_000/Documents/MI_school/Event_Study_and_friends/replication_code_Sun_Abraham/replication code/"
  
  # load data 
  hrs_raw <- data.table(haven::read_dta(paste0(file_path, "HRS_long.dta")))

  # copy this for now while im messing with it 
  hrs <- copy(hrs_raw)
  
  # option for if I want to match the paper or fix any mistakes I think they made 
  opt_match_paper <- TRUE

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
  

   # if we arent trying to match the paper we should subset before we get counts 
   if(!opt_match_paper){
   # subset to under 60 
   hrs <- hrs[age_hosp <= 59]
   }
   
   # Generate counts for each cohort
   c_7 <- hrs[wave_hosp == 7 & wave ==7, .N]
   c_8 <- hrs[wave_hosp == 8 & wave ==7, .N]
   c_9 <- hrs[wave_hosp == 9 & wave ==7, .N]
   c_10 <- hrs[wave_hosp == 10 & wave ==7, .N]
   
   # if we are try ing to match the paper subset after the counts liek they do 
   if(opt_match_paper){
     # subset to under 60 
     hrs <- hrs[age_hosp <= 59]
   }
   
   
   
   # # save this as input data for out function
   # vars_to_keep <- c("hhidpn", "oop_spend", "wave", "evt_time", "wave_hosp",
   #                   grep("evt_time_", colnames(hrs),value = TRUE),
   #                   grep("wave_", colnames(hrs),value = TRUE))
   
   vars_to_keep <- c("hhidpn", "oop_spend", "wave", "evt_time", "wave_hosp")
   out_test_data <- hrs[,vars_to_keep, with = FALSE]

   save(out_test_data, file = paste0(out_path, "AS_test_data.R"))
   
#=====================#
# ==== Estimation ====
#=====================#
   
   
   #==============================================#
   # ==== estimate two way FE as a comparison ====
   #==============================================#

  
     # get all the dummy variables we need 
     dum_vars <- grep("evt_time_|wave_[[:digit:]]", colnames(hrs), value = TRUE)
     
     # leave one of each out 
     dum_vars <- setdiff(dum_vars, c("wave_7", "evt_time_1", "evt_time_4"))
     
     # make formula 
     form <- as.formula(paste0("oop_spend ~ ",
                               paste0(dum_vars, collapse = " + " ),
                               "| hhidpn |0| hhidpn"))
     
     # run the regression
     #note this should match stata now with the updated cmethdd 
     reg_res <- felm(form,
                     data = hrs[ever_hospitalized == 1],
                     cmethod = "reghdfe")
     
     
     b2way_hrs
     
     # check attributes 
     attributes(reg_res)
     
     # put results in a data.table 
     #check data with stata. Std errors are wrong like I said earlier 
     # BUT the purpose of this is just as a camparison. It's not part of their estimation technique 
     reg_res_dt <- data.table(term        = rownames(reg_res$coefficients),
                               estimate   = as.numeric(reg_res$coefficients),
                               robust_ste = reg_res$cse,
                               t          = reg_res$ctval,
                               p_val      = reg_res$cpval)
               
     b2way_hrs <- reg_res_dt[term %like% "evt_time", c("term", "estimate", "robust_ste")]
  #=======================================#
  # ==== set up data for their method ====
  #=======================================#

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
   sur_res <- systemfit(formula_list, method = "SUR", data = hrs_temp[wave<11])# the estiamtes match but 
   Vcov <- vcov(sur_res) ##note  vcov matrix does not match. Not sure what is wronge, maybe robust SE 
   Vcov_test <- Vcov
   
#NOTE THIS IS WHERE THE RESTORE HAPPENS IN STATA
   
  # go back to using regular hrs data 
   rm(hrs_temp)
   
  # here they are creating another indicator 
   # it is 1 if anf only if the first number corresponds to the event time relative to the current wave and the second number 
   # corresponds to the actual wave in whiich hospitalization occured 
   # This is so we can estiamte unique cohort time treatment effects 
  for(vv in evt_tim_vars){
    for(i in 8:11){
      print(paste0(vv, "_", i))
      hrs[,  (paste0(vv, "_", i)) := get(vv)*as.numeric(wave_hosp == i)]
      
    }
    
  }
   
  # make a list of the relevent variables like they do 
   rhs_rel_year_i <- c("evt_time_5_8", "evt_time_6_8", "evt_time_7_8", "evt_time_3_9", "evt_time_5_9", "evt_time_6_9", "evt_time_2_10", "evt_time_3_10", "evt_time_5_10")
  
  # this will show why they picked those specifically 
  print(setorder(hrs[wave < 11, .N, c( "evt_time", "wave_hosp")], wave_hosp, evt_time))
  # so you van see they are picking those because that is all of the wave event time combinations that are present in 
  # waves 8-10 and they are leaving  event time 4 ( event_time == -1) 
  # off for each group as the reference gourp. 
   
  # make RHS variabes 
  rhs_vars <- paste0("wave_", c(8:10))
  rhs_vars <- c(rhs_rel_year_i, rhs_vars)
   
  
  # make formula 
  form <- as.formula(paste0("oop_spend ~ ",
                            paste0(rhs_vars, collapse = " + " ),
                            "| hhidpn |0| hhidpn"))
  
  hrs[ever_hospitalized == 1 & wave < 11, .N, rhs_vars]
  
  # use FELM, agin remember the SE wont match 
  # this is the "saturated" regression of equation (11). We get cohort time treatment effects 
  reg_res_11 <- felm(form,
                  data = hrs[ever_hospitalized == 1 & wave < 11],
                  cmethod = "reghdfe")
  

  
  # put results in a data.table 
  #check data with stata. Std errors are wrong like I said earlier 
  # estimates seem to match though 
  reg_tab_11 <- data.table( term       = rownames(reg_res_11$coefficients),
                           estimate   = as.numeric(reg_res_11$coefficients),
                           robust_ste = reg_res_11$cse,
                           t          = reg_res_11$ctval,
                           p_val      = reg_res_11$cpval)
  
  # for now just make the same objects as the stat code even if its weird 
  b <- reg_tab_11$estimate
  V <- diag(vcov(reg_res_11))
  b_8 <- c(NA, NA, 0, b[1:3] )
  V_8 <- c(NA, NA, 0, V[1:3] )
  b_9 <- c(NA, b[4], 0, b[5:6], NA)
  V_9 <- c(NA, V[4], 0, V[5:6], NA)
  b_10 <- c(b[7:8], 0 , b[9], NA, NA)
  V_10 <- c(V[7:8], 0, V[9], NA, NA)
  biw_long <- b
   
  # first linear combo, estimates for event time 2 (-3 periods)
  res <- as.data.frame(svycontrast(reg_res_11, c("evt_time_2_10" = 1)))
  b <- res[,1 ]
  V <- (res[, 2])^2

  
  
  # second linear combo, esitmate for event time 3 (-2 periods)
  tempb <- as.matrix(biw_long[ c(4,8)])
  tempVcov <- as.matrix(Vcov[c(6,11), c(6,11)]  )
  
  temp <-  t(tempb)%*%tempVcov %*% tempb
  
  res <- as.data.frame(svycontrast(reg_res_11, c("evt_time_3_9" = c_9/(c_9 + c_10), "evt_time_3_10" = c_10/(c_9 + c_10))))
  b <- c(b, res[,1])
  V <- c(V, res[,2]^2 + temp)
  
  # evt_time_4 is normalized 
  b <- c(b, 0)
  V <- c(V, 0)
  
  # third linear combo ,estimates for event time 5, (0 periods away)
  tempb <- as.matrix(biw_long[c(1,5,8)])
  tempVcov <- as.matrix(Vcov[c(3,8,13), c(3,8,13)]  )
  temp <-  t(tempb)%*%tempVcov %*% tempb
  
  res <- as.data.frame(svycontrast(reg_res_11, c("evt_time_5_8" = c_8/(c_8 + c_9 + c_10), "evt_time_5_9" = c_9/(c_8 + c_9 + c_10), "evt_time_5_10" = c_10/(c_8 + c_9 + c_10))))
  b <- c(b, res[,1])
  V <- c(V, res[,2]^2 + temp)
  
  # fourth linear combo, event time 6 (1 period away)
  tempb <- as.matrix(biw_long[c(2,6)])
  tempVcov <- as.matrix(Vcov[c(4,9), c(4,9)]  )
  temp <-  t(tempb)%*%tempVcov %*% tempb
  
  res <- as.data.frame(svycontrast(reg_res_11, c("evt_time_6_8" = c_8/(c_8 + c_9), "evt_time_6_9" = c_9/(c_8 + c_9))))
  b <- c(b, res[,1])
  V <- c(V, res[,2]^2 + temp)
  
  # get event time 7 (2 periods away 
  res <- as.data.frame(svycontrast(reg_res_11, c("evt_time_7_8" = 1)))
  b <- c(b, res[,1])
  V <- c(V, res[,2]^2)
  
  # get IW estimates 
  biw_hrs <- cbind(b,V, b_8, V_8, b_9, V_9, b_10, V_10)
  
#=====================#
# ==== Make table ====
#=====================#
# start by making this super wierd table to match the stata code. 
# the variable names and pretty much everything here is very confusing but 
# I am sticking with it for now to match the stata code. 
  
  # start with the table that's in stata 
  stata_tab <- b2way_hrs[, -"term"]
  setnames(stata_tab, colnames(stata_tab), c("b2way_hrs1", "sd1"))
  
  # add in normalizd to zero evt_time_4 
  stata_tab <- rbind(stata_tab[1:2], 
                     data.table(b2way_hrs1 = 0, sd1 =NA),
                     stata_tab[3:6])
  stata_tab[, y := c(-3:3)]
  stata_tab[, b2way_hrs2 :=sd1^2 ]
  stata_tab[, ci1_u := b2way_hrs1 + 1.96*sqrt(b2way_hrs2) ]
  stata_tab[, ci1_l := b2way_hrs1 - 1.96*sqrt(b2way_hrs2) ]
  
  
  # IW estimates 
  biw_hrs2 <- rbind(biw_hrs, rep(NA, 8))
  stata_tab <- cbind(stata_tab, data.table(biw_hrs2))
  stata_tab[, ci2_u := b + sqrt(V)*1.96]
  stata_tab[, ci2_l := b - sqrt(V)*1.96]
  stata_tab[, sd2 := sqrt(V)]
  stata_tab[, sd8 := sqrt(V_8)]
  stata_tab[, sd9 := sqrt(V_9)]
  stata_tab[, sd10 := sqrt(V_10)]
  
  # now put them in this super weird order from the stata table 
  setcolorder(stata_tab, c("y", "b2way_hrs1", "b", "b_8", "b_9", 
                           "b_10", "sd1", "sd2", "sd8", "sd9",
                           "sd10", "b2way_hrs2", "ci1_u", "ci1_l",
                           "V", "V_8", "V_9", "V_10", "ci2_u", "ci2_l"))
  
  #===========================#
  # ==== Make paper table ====
  #===========================#
  # in order to get the estimate over the SE like the paper 
  # I make to tables, stack them, then sort it 
    
    # get estimates 
    paper_tab_est <- stata_tab[, c("y", "b2way_hrs1",  "b", "b_8",
                               "b_9","b_10")]
    paper_tab_est[, statistic := "estimate"]
    
    # get std.error
    paper_tab_se <- stata_tab[, c("y", "sd1","sd2", "sd8","sd9","sd10")]
    paper_tab_se[, statistic := "Std.Error"]
    
    # change the solumn names so we can stack them 
    setnames(paper_tab_se, colnames(paper_tab_se), colnames(paper_tab_est))
    
    # stack them 
    paper_tab <- rbind(paper_tab_est, paper_tab_se)
    
    # set order so that we get estimate then std.error 
    setorder(paper_tab, y,-statistic)
    
    # order the columns 
    setcolorder(paper_tab, c("y", "statistic"))
    
    # round the numeric  columns 
    paper_tab[,3:7] <- round(paper_tab[,3:7])
    
    # rename columns 
    setnames(paper_tab,
             colnames(paper_tab),
             c("Wave Relative to Hospitalizaiton",
              "Statistic", "FE Est", "IW Est",
              "Cohort 1 Est", "Cohort 2 Est", "Cohort 3 Est"))
    
    
    
  