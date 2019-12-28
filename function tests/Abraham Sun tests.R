#========================================#
# ==== test abraham and sun function ====
#========================================#
# - Keywords  
#  - #note I will use the tags #note for things I think are important
#  - #set will be used for things that need to be set for your specific

# - purpose of code:
# tests for  Abraham and Sun function  
# This also loads or generates severa test data.sets to test the functions 



#=================================#
# ==== load function/packages ====
#=================================#

source("C:/Users/Nmath_000/Documents/Code/Event_studies_and_friends/functions/Abraham_Sun_Function.R")


#==============================#
# ==== load example data 1 ====
#==============================#

  # load up some data to have an example for debugging 
  
  #set loaciton of data set 
  file_path <-  "C:/Users/Nmath_000/Documents/MI_school/Event_Study_and_friends/Funciton_test_data/"
  
  # load data 
  hrs <- get(load(paste0(file_path, "AS_test_data.R")))
  
  # make a control cohort
  hrs[wave_hosp ==11, wave_hosp := NA]
  hrs <- hrs[wave < 11]

  # set parameters for debug 
  in_data = data.table(hrs)
  in_id_var        = "hhidpn"
  in_outcome_var   = "oop_spend"
  in_time_var      = "wave"
  in_cohort_var = "wave_hosp"
  
  
  test_result <- AS_IW(in_data          = hrs,
                       in_id_var        = "hhidpn",
                       in_outcome_var   = "oop_spend",
                       in_time_var      = "wave",
                       in_cohort_var = "wave_hosp")
  
  
#============================#
# ==== create group data ====
#============================#

  # going to create random data with n_schools and average m_studs per school 
  # there will be 3 treated cohorts and some control students 
  # years covered will be 0-4
  # years treated are 1,2,3
  
  n_schools <- 100 # number of schools 
  m_stud    <- 500  # mean students per school 
  stud_sd   <- 100  # standard deviation of number of students per school 
  n_treat <- 50
  
  
  # define all treatement effects 
  treat_effects <- data.table(expand.grid(time = c(0:4), cohort =  c(1:3)))
  treat_effects[, rel_treat := time- cohort]
  treat_effects[rel_treat < 0 , treatment_effect := 0]
  treat_effects[rel_treat == 0 , treatment_effect := 1]
  treat_effects[rel_treat > 0 , treatment_effect := .5]
 
  
  # define noise and trents 
  B_trend <- 2         # time trend 
  school_fe_m <- .5    # mean school fixed effect 
  school_fe_sd <- .2   # Standard deviation of school fixed effect 
  stud_fe_m    <- 1.5  # mean student fixed effect
  stud_fe_sd   <- .6   # SD of student FE
  wn_sd        <- .5    # SD of white noise 
  
  # generate studs and schools 
  r_dt <- vector("list", length = n_schools)
  # just do this with a loop because its easy 
  for(i in 1:n_schools){
    
    r_dt[[i]] <- data.table(school = rep(i,  max(round(rnorm(1,  m_stud, stud_sd)), 50)))
    
  }
  
  # add students 
  r_dt <- rbindlist(r_dt)
  r_dt[, stud := .I]
  
  # make it a panel 
  r_dt <- rbindlist(replicate(5, r_dt, simplify = FALSE))
  r_dt[, time := 0:4, stud]
  
  # pick schools to ever be treated 
  treated <- sample(1:n_schools, n_treat)
  r_dt[, treat := as.numeric(school %in% treated)] 
  
  # for each treated school pick a cohort randomly 
  r_dt[treat ==1, cohort := sample(1:3, 1), school ]
  
  # generate cohort variable 
  r_dt[, rel_treat := time - cohort ]
  
  # now generate treatment effects by merging on xwalk 
  r_dt <- merge(r_dt, treat_effects, c("time", "cohort", "rel_treat"), all.x = TRUE)
  r_dt[is.na(treatment_effect), treatment_effect := 0]
  
  # now generate outcome variable 
  r_dt[, school_fe := rnorm(1, school_fe_m ,school_fe_sd), school]   # school Fe
  r_dt[, stud_fe   := rnorm(1, stud_fe_m, stud_fe_sd), stud]          # stud FE 
  r_dt[, wn := rnorm(nrow(r_dt), 0, wn_sd)]                                    # add white noise to everything 
  
  # now generate outcome variable 
  r_dt[, outcome := 0 + B_trend*time + school_fe + stud_fe + treatment_effect + wn]
  
  
  # set parameters for debug 
  in_data = r_dt
  in_id_var        = "stud"
  in_outcome_var   = "outcome"
  in_time_var      = "time"
  in_cohort_var = "cohort"
  in_group_var  = "school"
  
  
  test_result <- AS_IW(in_data          = r_dt,
                       in_id_var        =  "stud",
                       in_outcome_var   = "outcome",
                       in_time_var      = "time",
                       in_cohort_var    = "cohort",
                       in_group_var     = "school")
  