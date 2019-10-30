#====================================#
# ==== Understanding wfe package ====
#====================================#

  # - Keywords  
  #  - #set will be used for things that need to be set for your specific
  #       file structure to get the code to run. Things like data directories 
  #  - #fix will be used for things that need to be fixed 
  #  - #note I will use the tags #note for things I think are important
  
  
  # - purpose of code:
  # Get working exampels of the main funcitons in the "wfe" package 
  # this is all pulled from the replication files, which can be downloaded 
  # here: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/YUM3K8 
  # These files are from their corresponding paper 
  # here: https://imai.fas.harvard.edu/research/files/FEmatch.pdf 
  # the advatage of this over just looking at their code is it is stripped down
  # and has extra comments for clarity. This is specifically to show working examples
  # of the funcitons rather than code creating outpput for a paper 

  # -requirments to run 
  # set directories or optiosn where #set is tagged
  # download data set "TGR2007.dta" from above link and put it in directory 
  # install packages foreign and wfe and data.table 
#========================#
# ==== load packages ====
#========================#
  
  ## library
  library(foreign)
  library(wfe)
  library(data.table)
#===================================#
# ==== load data/set parameters ====
#===================================#

  #set the working directory
  setwd("c:/Users/Nmath_000/Documents/MI_school/Event_Study_and_friends/wfe_rep_data/")

  ## check/create the folder to put outputs
  OUT_DIR <- file.path(getwd())
  
  ## save results #set 
  SAVE_SEP <- TRUE
  APPENDIX <- TRUE # set TRUE to replicate Table 3 in Appendix A.4

  # read data and convert to data.table 
  D <- data.table(read.dta(file.path(getwd(), "TGR2007.dta")))
  
  # excluding years from 1995 to focus on GATT years
  GATT <- D[year < 1995]
  
  # match some numbers from the paper. Should get 196,207 and 10,289
  
  # number of observaitons 
  nrow(GATT)
  
  # nuymber of unique dyads 
  length(GATT[, unique(dyad)])

  
#==================================#
# ==== Run examples from paper ====
#==================================#
# this section replicates some results from  https://imai.fas.harvard.edu/research/files/FEmatch.pdf  and 
# provides a nice example of the functionality of WFE 

  

  # rename data. No clear reason to do this, they just do in their code 
  Data <- GATT
  
  ## Standard: dyad fixed effect
  # this is equation 28 on page 479 of the paper 
  # Or the "standard" column in table 2 
  F2A.U.std <- wfe(ltrade ~ gattff + gsp + lrgdp + lrgdppc + regional+ custrict + curcol,
                   unit.index = "dyad",
                   time.index = "year",
                   treat      = "gattff",
                   method     = "unit",
                   data       = Data,
                   unweighted = TRUE,
                   hetero.se  = TRUE, 
                   auto.se    = TRUE,
                   verbose    = TRUE, 
                   White      = TRUE)
  
  # check summary of results 
  print(summary(F2A.U.std))
  
  # look at all attribures of model output/object 
  # we would need to extrat all these things and convert it to a consistent style for out output 
  attributes(F2A.U.std)
  
  
  ## ATE: dyad fixed effect
  # this is equation 12 on page 475 of the paper or 
  ## this is the "weighted" column of table 2 in the paper 
  F2A.U.ate <- wfe(ltrade ~ gattff + gsp + lrgdp + lrgdppc + regional+ custrict + curcol,
                   unit.index = "dyad", 
                   time.index = "year",
                   treat      = "gattff",
                   method     = "unit",
                   data       = Data,
                   qoi        = "ate",
                   hetero.se  = TRUE, 
                   auto.se    = TRUE,
                   verbose    = TRUE, 
                   White      = TRUE)
  
  print(summary(F2A.U.ate))
  
  
  ## FD: dyad fixed effect
  ## regional should be excluded from the full model with no variation
  # column three, the "firs Diff" column of table 2 in the paper 
  F2A.U.fd <- wfe(ltrade ~ gattff + gsp + lrgdp + lrgdppc + custrict + curcol,
                  unit.index = "dyad", 
                  time.index = "year",
                  treat      = "gattff", 
                  method     = "unit",
                  data       = Data,
                  qoi        = "ate", 
                  estimator  = "fd",
                  hetero.se  = TRUE,
                  auto.se    = TRUE,
                  verbose    = TRUE,
                  White      = TRUE)
  
  print(summary(F2A.U.fd))
  
  
  