#===================================#
# ==== Abraham And Sun Function ====
#===================================#
  # - Keywords  
  #  - #note I will use the tags #note for things I think are important
  #  - #set will be used for things that need to be set for your specific
  
  # - purpose of code:
  # Put Abraham and Sun Method into a generalizable function 
  
  # -requirments to run 
  # Data.table package: I love data.table and it's fast. IMO it is worth the dependency. 


# claer objects and code script to get a fresh start to test this 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

#===========================#
# ==== helper functions ====
#===========================#
AS_dummy_f <- function(in_data, in_var){
  
                # get list of value for the variable 
                values <- sort(in_data[, unique(get(in_var))])
                
                # for each value, create a dummy
                for(val_i in values){
                  
                  # if it has a minus sign change the name for the new variable 
                  if(val_i < 0){
                    
                    var_name <- paste0(in_var, "_m", abs(val_i))
                    
                  }else{
                    var_name <- paste0(in_var, "_", val_i)
                  }
                  
                  # create variable in the data.table 
                  in_data[get(in_var) == val_i, (var_name) := 1]
                  in_data[get(in_var) != val_i, (var_name) := 0]
                  
                  
  }
  # return NULL since I am just editing the input data.set in global env
  return(NULL)
}


#============================#
# ==== load example data ====
#============================#
  # load up some data to have an example for debugging 
  
  #set loaciton of data set 
  file_path <-  "C:/Users/Nmath_000/Documents/MI_school/Event_Study_and_friends/Funciton_test_data/"
  
  # load data 
  hrs <- get(load(paste0(file_path, "AS_test_data.R")))

  # set parameters for debug 
  in_data = hrs 
  in_id_var        = "hhidpn"
  in_outcome_var   = "oop_spend"
  in_time_var    = "wave"
  in_rel_treat_var = "evt_time"

  

#================================#
# ==== Roxygen documentation ====
#================================#
  
  #' AS_IW
  #' 
  #' impliment methods from "Estimating Dynamic Treatment Effects in Event Studies
  #' with Heterogeneous Treatment Effects" by Abraham and Sun.
  #'
  #'@param in_data Your data set entered as a data.table
  #'@param in_id_var The Variable Name for an id variable identifying individuals in in_data. Entered as a quoted character 
  #'@param in_outcome_var The variable name for the outcome variable of interest. Entered as a quoted character 
  #'@param in_time_var The variable name for a numeric time variable in in_data. This is the period for a given row in the panel data. Entered as a quoted character 
  #'@param in_rel_treat_var The variable name for a numeric variable in in_data indicating the amount of time since treatment. 
  #'Negative indicates time period is before treatmet. Entered as a quoted character 
  #'@details See the working paper http://economics.mit.edu/files/14964
  #'@examples 
  
  
  
#======================#
# ==== AS Function ====
#======================#
require(data.table)

  
  

AS_IW <- function(in_data          = NULL,
                  in_id_var        = NULL,
                  in_outcome_var   = NULL,
                  in_time_var      = NULL,
                  in_rel_treat_var = NULL){
  
  #====================================#
  # ==== do checks and set up data ====
  #====================================#

    
    # check that in data is a data.table and exists 
    
    # Make out working data set 
    w_dt <- in_data[, c(in_id_var, in_outcome_var, in_time_var, in_rel_treat_var), with = FALSE]
  
    # check that the variables exist 
    
    # check that in_cohort_var and in_rel_treat_var are numeric 
    #note not sure if cohort needs to be numeric. Can think about that later. 
    
    # change the variable names in our working data set 
    #note not totally sure about this. We could just work with the in_var names the entire time but it 
    # can get a bit clunky and it's probably easier to just change names and put them back at the end. 
    setnames(w_dt, c(in_time_var, in_rel_treat_var), c("time", "rel_treat") )
    
    # make a cohort variable 
    w_dt[, cohort := time - rel_treat]
  
    # create dummy variables 
    lapply(c("time", "rel_treat", "cohort"), AS_dummy_f, in_data = w_dt)
    
    # Now pick a control cohort, it'l be the last cohort observed 
    control_cohort <- max(unique(w_dt[, cohort]))
    
    # now we make relative treatment dummies 0 for all of these. We are making it as if there were never treated.
    # first grab the vars 
    rel_treat_dums <- grep("rel_treat_", colnames(w_dt), value = TRUE)
    
    # now make the change 
    w_dt[cohort == control_cohort, (rel_treat_dums) := 0]
    
    # now subset to only time periods before the control cohort. We don't want to include that last time period because we have 
    # artifically set it so no one is treated that period 
    #note I need to check on the reasononing behind this. Not sure that this is exactly what you would want if you had periods 
    # past your last cohort where no one was actually treated. 
    #note in Abraham sun code they just continuously exclude wave 11 from regs but I don't see why we shuoldn't just drop it 
    w_dt <- w_dt[time < control_cohort]
    
  #=================#
  # ==== do SUR ====
  #=================#

    # get lhs vars. We need the dummy vars for each cohort except for the excluded control cohort  
    lhs_list <-  grep("cohort_", colnames(w_dt), value = TRUE)
    lhs_list <- setdiff(lhs_list, paste0("cohort_", control_cohort))
    
    # get rhs vars 
    # first check wich relative treatment vars are actually in use for our group of interest 
    rhs_nums <- sort(w_dt[cohort < control_cohort, unique(rel_treat)  ])
    rhs_nums <- gsub("-", "m", rhs_nums)
    rhs <- paste0(paste0("rel_treat_", rhs_nums[-1]),collapse = " + ")
    
     # make the three formulas 
    formula_list <- lapply(paste0(lhs_list, "~", rhs, "-1"), as.formula)
    
    
    
    # run system fit 
    #note this is also not matching. Need to figure it out but I can not right now so mocing on 
    names(formula_list) <- c("eq1", "eq2", 'eq3')
    #note that we exclue the control cohort from this 
    sur_res <- systemfit(formula_list, method = "SUR", data = w_dt[cohort != control_cohort]) # the estiamtes match but 
    Vcov <- vcov(sur_res) ##note  vcov matrix does not match. Not sure what is wronge, maybe robust SE 
    
  
  #================#
  # ==== do IW ====
  #================#

  
  
  
  
  
  
  
  
  
}
