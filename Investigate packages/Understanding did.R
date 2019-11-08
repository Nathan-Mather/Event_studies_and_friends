#============================#
# ==== Understanding DID ====
#============================#

  # - Keywords  
  #  - #set will be used for things that need to be set for your specific
  #       file structure to get the code to run. Things like data directories 
  #  - #fix will be used for things that need to be fixed 
  #  - #note I will use the tags #note for things I think are important
  
  # - purpose of code:
  # Get working examples of functions from the  DID package 
  # package for relavent working paper 
  #	Callaway, Brantly and Sant'Anna, Pedro. "Difference-in-Differences with Multiple Time Periods." 
  #  Working Paper https://ssrn.com/abstract=3148250, 2019.
  
  # -requirments to run 
  # Package DID needs to be installed 
  # install.packages("did")

#==============================#
# ==== load packages/ data ====
#==============================#
  
  library("did")
  library(gridExtra)
  
  data(mpdta)


#=================================#
# ==== mp.spatt main funciton ====
#=================================#

  # funciton descrimption from manual 
  # mp.spatt computes the ATT in the case where there are more than two periods of data and allowing
  # for treatment to occur at different points in time extending the method of Abadie (2005). This
  # method relies on once individuals are treated they remain in the treated state for the duration.
  
  # The following is a simplified example of the effect of states 
  # increasing their minimum wages on county-level teen employment 
  # rates which comes from Callaway and Sant'Anna (2019).
  out <- mp.spatt(lemp ~ treat, xformla=~lpop, data=mpdta,
                  panel=TRUE, first.treat.name="first.treat",
                  idname="countyreal", tname="year",
                  bstrap=FALSE, se=TRUE, cband=FALSE)
  
  
  # here we can see average tretment effects for each group year combo 
  # "group" is defined by the time period when units are first treated
  # per the paper "they are not determined by the estimation method one adopts (e.g., first difference or fixed effect linear regression)"
  # and "they do not directly restrict heterogeneity with respect to observed covariates,  the time one is first treated (group), or the evolution of treatment effects over time"
  # thesee "parameters can be directly used for learning about treatment
  # effect heterogeneity, and/or to construct many other more aggregated causal parameters"
  summary(out)
  
  # check out this object 
  class(out)
  attributes(out)
  
  # There is a built in plotting funciton. 
  # not crazy about these types of plotting functins but it looks nice 
  ggdid(out, ylim=c(-.25,.1))
  
  # these also produce evnt study style results
  summary(out$aggte, type="dynamic")
  
  # can also get this from the ploting function 
  ggdid(out, type="dynamic", ylim=c(-.25,.1))




#========================#
# ==== mp.spatt.test ====
#========================#

# integrated moments test for conditional common trends holding in all pre-treatment time periods
#  across all groups

  mptest <- mp.spatt.test(lemp ~ treat, xformlalist=list(~lpop), data=mpdta,
                          panel=TRUE, first.treat.name="first.treat",
                          idname="countyreal", tname="year", clustervarlist=list(NULL))
  
  summary(mptest[[1]])  
  
  