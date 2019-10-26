
#=======================================#
# ==== go through augsynth vignette ====
#=======================================#

# this package actually has a vignette with more details than this code. Authors are also on this project. 
# but I started with this one, which is well documented, to get a sense of what I will do for other packages 
# The idea is to just get a funcitonal data set, real or simulated, to run through the main parts of the package. 
# in the comments I'll just note what it is doing and any quirks for when we get to the wrapper 
library(augsynth)
library(broom)
library(magrittr)
library(dplyr)
library(Synth)
library(data.table)

# clear everything out 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")


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
  
#==========================#
# ==== list of options ====
#==========================#

# # various types of progfunc 
# Ridge=Ridge regression (allows for standard errors), 
  # None=No outcome model, 
  # EN=Elastic Net, 
  # RF=Random Forest,
  # GSYN=gSynth,
  # MCP=MCPanel,
  # CITS=CITS
  # CausalImpact=Bayesian structural time series with CausalImpact
  # seq2seq=Sequence to sequence learning with feedforward nets
  
