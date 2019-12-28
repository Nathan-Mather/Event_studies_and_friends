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
#'@param in_alpha Size of test 
#'@param opt_return_reg option to aslo return a table of the regression results. Default is FALSE
#'@details See the paper 
#'@examples 


#=========================#
# ==== start function ====
#=========================#

 HB_pretrends <- function(in_data        = NULL,
                          in_treat_var   = NULL,
                          in_time_var    = NULL,
                          in_outcome_var = NULL,
                          in_threshold   = NULL,
                          in_alpha       = .05,
                          opt_return_reg = FALSE){
   
   
  #====================#
  # ==== do checks ====
  #====================#
    # check that in data is a data.table and exists 
    if(!any(class(in_data) == "data.table")){
     
     stop("in_data must be entered as a data.table")
    }
    
    # make list of variables we need 
    in_cols <- c(in_treat_var, in_time_var, in_outcome_var)
    
    # check that columns exist 
    for(col_i in in_cols){
     
       # check that it is a column
       if(!col_i %in% colnames(in_data)){
         stop(paste0(col_i, " is not a column name in in_data"))
       }
    }
  
  #======================#
  # ==== set up data ====
  #======================#
    # make working data with needed vars 
    w_dt <- in_data[, in_cols, with = FALSE]
   
   # chage column names for ease 
    setnames(w_dt, in_cols, c("treat", "period", "outcome"))
   
    # create interaction terms
    w_dt[, p_t := period*treat]
    
    #note could use this for alternate specificaiton. unsure 
    w_dt[, p_ut := period*(1-treat)]
    
   
   # run regression 
    reg_res <- lm(outcome ~ treat + period + p_t, data = w_dt)
    
    # note I could do this then run an F test on p_ut and p_t coeffs? 
    # reg_res2 <- lm(outcome ~ treat + p_ut + p_t, data = w_dt)
    # 
    # reg_sum2 <- summary(reg_res2)
    # reg_dt2 <- as.data.table(reg_sum2$coefficients, keep.rownames = TRUE)
    # B1 <- reg_dt2[rn == "p_t", Estimate ]
    # B2 <- reg_dt2[rn == "p_ut", Estimate ]
    # vcov <- vcov(reg_res2)
    # v1 <- vcov[4,4]
    # v2 <- vcov[3,3]
    # cov12 <- vcov[3,4]
    # 
    # f_test1 <- (B1-B2-in_threshold)/(sqrt(v1+v2-2*cov12))
    # f_test2 <- (B1-B2+in_threshold)/(sqrt(v1+v2-2*cov12))
    # 
  #===========================#
  # ==== equivalence test ====
  #===========================#
    
    # get coefficient 
    reg_sum <- summary(reg_res)
    reg_dt <- as.data.table(reg_sum$coefficients, keep.rownames = TRUE)
    B <- reg_dt[rn == "p_t", Estimate ]
    
    # get standard error 
    se <- reg_dt[rn == "p_t", `Std. Error` ]
    
    # perform first null test (lower bound)
    t1 <- (B+in_threshold)/se
    
    # perform second null (upper bound)
    t2 <- (B-in_threshold)/se
    
    # get degrees of freedom 
    dof <- nrow(w_dt) - 4
    
    # Now convert these to P values 
    #note these are not usual p values becuse the hypothesis test is inverted. We want the probability they are in the narrow acceptance region
    p1 <- pt(t1, dof, lower.tail=FALSE) 
    p2 <- pt(t2, dof, lower.tail=TRUE) 
    

    # take the min of the two t_tests 
    t_out <- ifelse(abs(t1) < abs(t2), t1,t2)
    
    # take the max of the two p values 
    p_out <- max(p1,p2)
    
    # run the test for if the difference is withing the acceptable limit 
    TOSToutcome <- ifelse(p_out<in_alpha,"significant","non-significant")
    
    # put into a table a
    out_tab <- data.table(threshold = in_threshold, t_stat = t_out, p_val = p_out, alpha = in_alpha, test_result = TOSToutcome, DOF = dof)
    
    
    
    if(opt_return_reg){
    
      # make regression table clear
      reg_dt[rn == "p_t", rn := "Period X Treat"]
      setnames(reg_dt, "rn", "variable")
  
      # out list
      out_list <- vector("list", length = 2)
      names(out_list) <- c("EQ_test", "Reg_tab")
      out_list[["EQ_test"]] <- out_tab
      out_list[["Reg_tab"]] <- reg_dt

    
       return(out_list)
    }else{
      return(out_tab)
    }
 
   
 }



