#===================================#
# ==== Abraham And Sun Function ====
#===================================#
  # - Keywords  
  #  - #note I will use the tags #note for things I think are important
  #  - #set will be used for things that need to be set for your specific
  
  # - purpose of code:
  # Put Abraham and Sun Method into a generalizable function 
  # there extra stuff around the function for debugging. We can scrap all that when it's ready for a package 
  
  # -requirments to run 
  # Data.table package: I love data.table and it's fast. IMO it is worth the dependency. 


library(data.table) # for data manipulation 

#===========================#
# ==== helper functions ====
#===========================#
AS_dummy_f <- function(in_data, in_var){
  
                # get list of value for the variable 
                values <- sort(in_data[, unique(get(in_var))])
                
                # for each value, create a dummy
                for(val_i in values){
                  
                  # create a variable name and if it has a minus sign change the name for the new variable 
                  var_name <- paste0(in_var, "_", val_i)
                  var_name <- gsub("-", "m", var_name)
                  
                  # create variable in the data.table 
                  in_data[get(in_var) == val_i, (var_name) := 1]
                  in_data[get(in_var) != val_i, (var_name) := 0]
                  
                  
  }
  # return NULL since I am just editing the input data.set in global env
  return(NULL)
}

# this function takes a vector in_var_split.  It then splits the vector by in_val_delim and returns the in_val_position element of each split. 
ea_scan <- function (in_var_split = NULL, in_val_position = NULL, in_val_delim = NULL) 
{
  out_var <- sapply(strsplit(in_var_split, in_val_delim), function(x) x[in_val_position])
  return(out_var)
}





  

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
  #'@param in_cohort_var The variable name for a numeric variable in in_data indicating the amount of time since treatment. This needs to be in the same scale as the in_time var
  #'Negative indicates time period is before treatmet. Entered as a quoted character 
  #'@details See the working paper http://economics.mit.edu/files/14964
  #'@examples 
  
  
  
#======================#
# ==== AS Function ====
#======================#
# I see people using require a lot inside functions but the only differents is require throws a warning and library throws an error 
# the function is not going to work without these packages so I would rather it throw an error 
library(systemfit) # for Seamingly unrealated estimation 
library(lfe)       # for fixed effects regs 
library(survey)    # for linear combos 
  
  

AS_IW <- function(in_data          = NULL,
                  in_id_var        = NULL,
                  in_outcome_var   = NULL,
                  in_time_var      = NULL,
                  in_cohort_var = NULL){
  
  
  #====================#
  # ==== do checks ====
  #====================#

    # check that in data is a data.table and exists 
    if(!any(class(in_data) == "data.table")){
      
      stop("in_data must be entered as a data.table")
    }
  
    # check that the variables they enetered actually  exist 
    in_cols <- c(in_id_var, in_outcome_var, in_time_var, in_cohort_var)
    
    for(col_i in in_cols){
      
      # check that it is a column
      if(!col_i %in% colnames(in_data)){
        stop(paste0(col_i, " is not a column name in in_data"))
      }

      
    }
    
    # check that in_cohort_var and in_time_var are numeric 
    if(!class(in_data[[in_cohort_var]]) %chin% c("numeric", "integer"))stop(paste0(in_cohort_var, " is not numeric or integer"))
    if(!class(in_data[[in_time_var]])  %chin% c("numeric", "integer"))stop(paste0(in_time_var, " is not numeric or integer"))
      
    
    
  #==========================#
  # ==== and set up data ====
  #==========================#

    
    # Make our working data set 
    w_dt <- in_data[, in_cols, with = FALSE]
  

    # change the variable names in our working data set 
    #note not totally sure about this. We could just work with the in_var names the entire time but it 
    # can get a bit clunky and it's probably easier to just change names and put them back at the end. 
    # I could also do everything by numeric reference but I am not a fan of that. Too easy for things to break silently 
    setnames(w_dt, in_cols, c("ID", "outcome", "time", "cohort") )
    
    # make a relative treatment variable 
    w_dt[, rel_treat := time - cohort]
  
    # create dummy variables 
    lapply(c("time", "rel_treat", "cohort"), AS_dummy_f, in_data = w_dt)
    

    # Now pick a control cohort, it'l be the last cohort observed 
    control_cohort <- max(unique(w_dt[, cohort]))
    
    print(paste0("Using ", control_cohort, " as the control cohort"))
    

    
    # now we make relative treatment dummies 0 for all of these. We are making it as if they were never treated.
    # first grab the vars 
    rel_treat_dums <- grep("rel_treat_", colnames(w_dt), value = TRUE)
    
    # now make the change. If control_cohort == NA it will do nothing 
    w_dt[cohort == control_cohort, (rel_treat_dums) := 0]
    
    # now subset to only time periods before the control cohort. We don't want to include that last time period because we have 
    # artifically set it so no one is treated that period 
    #note I need to check on the reasononing behind this. Not sure that this is exactly what you would want if you had periods 
    # past your last cohort where no one was actually treated. 
    #note in Abraham sun code they just keep excluding wave 11 from regs, but I don't see why we shuoldn't just drop it 
      w_dt <- w_dt[time < control_cohort]

    
  #=================#
  # ==== do SUR ====
  #=================#

    # get lhs vars. We need the dummy vars for each cohort except for the excluded control cohort  
    lhs_list <-  grep("cohort_", colnames(w_dt), value = TRUE)
    lhs_list <- setdiff(lhs_list, paste0("cohort_", control_cohort))
    
    # get rhs vars 
    # first check wich relative treatment vars are actually in use for our group of interest 
    # if there is a control group exclude it
    rhs_nums <- sort(w_dt[cohort != control_cohort, unique(rel_treat)  ])
    
    # create the suffix 
    rhs_nums <- gsub("-", "m", rhs_nums)
    
    # maek the variable names and collapse them for the forumla
    rhs <- paste0(paste0("rel_treat_", rhs_nums[-1]),collapse = " + ")
    
     # make the three formulas 
    formula_list <- lapply(paste0(lhs_list, "~", rhs, "-1"), as.formula)
    
    # name the formula list based on the cohorts i,e, the LHS variable 
    names(formula_list) <- c(gsub("cohort_", "c",lhs_list))
    
    # run system fit 
    #note this is also not matching. Need to figure it out but I can not right now so mocing on
    #note that we exclue the control cohort from this 
    sur_res <- systemfit(formula_list, method = "SUR", data = w_dt[cohort != control_cohort]) # the estiamtes match but 
    Vcov <- vcov(sur_res) ##note  vcov matrix does not match. Not sure what is wronge, maybe robust SE 
    
  
  #========================#
  # ==== Estimate CATT ====
  #========================#
  # CATT is cohort average treatment effects 
  
    
    # create interaction variables for "saturated regression"
    # start by creating a concatinated variable of rel_treat and cohort 
    w_dt[, treat_coh := paste0(rel_treat, "_", cohort)]
  
    # now make dummies for the concatinated variable 
    AS_dummy_f( in_data = w_dt, "treat_coh")
    
    # now set the dummies to zero for the control group 
    # first grab the vars 
    treat_coh_dums <- grep("treat_coh_", colnames(w_dt), value = TRUE)
    
    # now change all those columns to zero for the control cohort 
    w_dt[cohort == control_cohort, (treat_coh_dums) := 0]
    
    # get all the combinations of relative treatment and cohort in the relevant data set 
    relevant_combos <- w_dt[cohort != control_cohort, .N, c("rel_treat", "cohort") ]

    # get a common "relatvie treatment" accross these groups to use as a control (-1 in AS setting)
    control_rel_treat <- relevant_combos[, list(rel_treat_n = .N), rel_treat]
    control_rel_treat <- control_rel_treat[ rel_treat_n == max(rel_treat_n), rel_treat]
    
    # iRemove zero from the list of options. I don't think in most cases people are going to want the time of the event as a control 
    control_rel_treat <- setdiff(control_rel_treat, 0)
    
    # if there is nothing left throw an error 
    #Note Probably this doesn't need to be an error. I think we could probably have a different control group for the different cohorts but 
    # I am not sure. Something to think about 
    if(length(control_rel_treat) == 0) stop( paste0("Your cohorts do not have any relative treatment variable",
                                                     
                                                    " values in common to use as a control. I suggest trying fewer cohorts to get more overlap in treatment effects."))
    
    # Now just pick the frst one of the rest of the options 
    control_rel_treat <- control_rel_treat[[1]]
    
    # remove it from the relevant combos 
    relevant_combos <- relevant_combos[rel_treat != control_rel_treat]
    
    # # use this to put together RHS of regression 
    relevant_combos[, suffix := paste0(gsub("-", "m", rel_treat), "_", cohort)]
    RHS <- paste0("treat_coh_", relevant_combos$suffix)
    
    # add in time fixed effects
    time_FE <- paste0("time_", sort(unique(w_dt$time))[-1])
    RHS <- paste0(c(RHS, time_FE), collapse = " + ")
    
    # make formula 
    form <- as.formula(paste0("outcome ", " ~ ",
                              RHS,
                              "| ",
                              "ID",
                              "|0|",
                              "ID"))
  
    
    # run regression 
    reg_res <- felm(form,
                       data = w_dt,
                    cmethod = "reghdfe")

    
    reg_tab <- data.table( term       = rownames(reg_res$coefficients),
                              estimate   = as.numeric(reg_res$coefficients),
                              robust_ste = reg_res$cse,
                              t          = reg_res$ctval,
                              p_val      = reg_res$cpval)

  #=================#
  # ==== get IW ====
  #=================#
    
    # Generate counts for each cohort
    #note, this works because it is a balanced panal. NOt sure if this works if it isnt, need to think about it 
    c_list <- w_dt[, list(count = uniqueN(ID)), cohort]

  
  # Now we need to do linear combinations of the different estimate and adjust the standard errors 
  #note I suppose let's do this with a loop 
    #note delete this, its for debug 

  # get relative treatment list to loop over 
  rel_treat_list <- relevant_combos[, unique(rel_treat)]
    
  # create a list for results 
  res_list <- vector("list", length = length(rel_treat_list))

  # start for loop 
  for(i in 1:length(rel_treat_list)){
    
    rel_treat_i <- rel_treat_list[[i]]
    
    # Figure out what cohorts have this rel_treat effect 
    rel_coh_i <- relevant_combos[rel_treat == rel_treat_i, cohort]
    
    # make the varibles list that we need 
    vars_i <- paste0("treat_coh_", gsub("-","m",rel_treat_i), "_", rel_coh_i)
    vcov_vars_i <- paste0("c", rel_coh_i, "_rel_treat_", gsub("-","m",rel_treat_i))
   
    # start by calculating the SE adjustment 
    if(length(rel_coh_i) > 1){
      tempb <- as.matrix(reg_tab[ term %chin% vars_i, estimate])
      tempVcov <- as.matrix(Vcov[vcov_vars_i, vcov_vars_i]  )
      temp <-  t(tempb)%*%tempVcov %*% tempb
    }else{
      temp <- 0
    }
    
    # now make list of linear combos and weights 
    weights <- c_list[cohort %in% rel_coh_i]
    weights[, weights := count/sum(count)]
    weights[, name :=  paste0("treat_coh_", gsub("-","m",rel_treat_i), "_", cohort )]
    weight_list <- weights$weights
    names(weight_list) <- weights$name
    
    # Now do linear combo 
    res <- as.data.frame(svycontrast(reg_res, weight_list))
    
    # put it in the result lists
    res_list[[i]] <- data.table(rel_treat = rel_treat_i, coef = res[,1], var = as.numeric(res[,2]^2 + temp))

    
  # end for loop 
  }
  
  # stack results 
  IW_res <- rbindlist(res_list)
  
  # sort 
  setorder(IW_res, rel_treat)
  
  # add SE 
  IW_res[, sd := sqrt(var)]
  IW_res[, var := NULL]
  
  # clean up saturated regression table 
  reg_tab[term %like% "treat_coh_", suffix := gsub("treat_coh_", "",term)]
  reg_tab[, rel_treat := ea_scan(suffix, 1, "_")]
  reg_tab[, rel_treat := as.numeric(gsub("m", "-", rel_treat))]
  reg_tab[, cohort :=  as.numeric(ea_scan(suffix, 2, "_"))]
  reg_tab[, suffix := NULL]
  setcolorder(reg_tab, c("term", "cohort", "rel_treat"))
  setorder(reg_tab, cohort, rel_treat)
  
 
  # put it all in one big table
  # not sure what we want this to look like. have to think about it a bit 
  
  # put everything people could ever want it in a big ol list 
  # for now all I can thik of is the CATT estimates, the time FE from the CATT reg, and the IW estimates 
  out_list <- list() #note should initialize this with the length and names that we eventually decide to use 
  
  
  
  out_list[["CATT"]] <- reg_tab[]
  
  out_list[["IW"]] <- IW_res
  
  # return it 
  return(out_list)
  
# end funciton 
}





