
#=======================================#
# ==== go through augsynth vignette ====
#=======================================#
# - Keywords  
#  - #set will be used for things that need to be set for your specific
#       file structure to get the code to run. Things like data directories 
#  - #fix will be used for things that need to be fixed 
#  - #note I will use the tags #note for things I think are important

# - purpose of code:
#  The purpose is just to expore the augsynth package and have working examples of it's main funitons. 
# this package actually has a vignette with more details than this code. Authors are also on this project. 
# But, I started with this one, which is well documented, to get a sense of what I will do for other packages. 
# The idea is to just get a funcitonal data set, real or simulated, to run through the main parts of the package. 
# in the comments I'll just note what it is doing and any quirks. 
# Havign something like this for all the functionalityt we want in the final 
# project should make writing the wrapper easier. 

# original vignette is here
# https://github.com/ebenmichael/augsynth/blob/master/vignettes/augsynth-vignette.md



#========================#
# ==== load packages ====
#========================#
  

  library(augsynth)
  library(broom)
  library(magrittr)
  library(dplyr)
  library(Synth)
  library(data.table)

#====================#
# ==== load data ====
#====================#

  # clear everything out 
  rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
  options(scipen = 999)
  cat("\f")
  
  # load basque data which is part of one of these libraries 
  data(basque)
  
  # convert to data.table 
  basque_dt <- data.table(basque)
  
  
  # filter out spain 
  basque_dt <- basque_dt[regionno!= 1 ]
  
  # add in treatment 
  basque_dt[year >= 1975 & regionno == 17, treatment := 1]
  basque_dt[year < 1975 | regionno != 17, treatment := 0]

#============================#
# ==== synthetic control ====
#============================#


  # do augsynth 
  syn <- augsynth(gdpcap ~ treatment, regionno, year, 1975, basque_dt,
                  progfunc="None", weightfunc="SCM")
  
  # summarize results 
  summary(syn)
  
  # check everyting that is in the object  
  attributes(syn)
  
  # access attributes like so
  syn$weights
  
  # do plot 
  plot(syn)

#========================================#
# ==== Augment with ridge regression ====
#========================================#

  
# run with progfunc="Ridge"
 asyn <- augsynth(gdpcap ~ treatment, regionno, year, 1975, basque_dt,
                  progfunc="Ridge", weightfunc="SCM", opts_out=list(lambda=8))
  
  # summarize results 
  summary(asyn)
  
  
  # do plot 
  plot(asyn)
  
  
  # adding auxillary variables using | in the regression equation 
  covsyn <- augsynth(gdpcap ~ treatment | invest + sec.agriculture + sec.energy + gdpcap,
                     regionno, year, 1975, basque,
                     progfunc="Ridge", weightfunc="SCM", opts_out=list(lambda=0))
  
  
  summary(covsyn)
  plot(covsyn)
  

