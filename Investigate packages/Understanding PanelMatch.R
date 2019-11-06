#====================================#
# ==== UNderstanding PanelMatch ====
#====================================#


# - Keywords  
#  - #set will be used for things that need to be set for your specific
#       file structure to get the code to run. Things like data directories 
#  - #fix will be used for things that need to be fixed 
#  - #note I will use the tags #note for things I think are important

# - purpose of code:
# Get working examples of functions from the PanelMatch package 
# put out by	Imai, Kosuke. It is related to their paper 
# : https://imai.fas.harvard.edu/research/FEmatch.html
# and is only available on github, not yet on CRAN 

# -requirments to run 
# Package needs to be installed from github which is most easilt done 
# with devtools. Can be done by uncommenting and running the following code 
# if(!require(devtools)) install.packages("devtools")
# library(devtools)
# install_github("insongkim/PanelMatch", dependencies=TRUE)
# 
# also need to data from the folder on dropbox and to set the directory below to find it 

#===========================#
# ==== package and data ====
#===========================#

library(PanelMatch)

# set directory where data is stored 
in_dir <- "c:/Users/Nmath_000/Documents/MI_school/Event_Study_and_friends"
load(paste0(in_dir, "/PanelMatch/dem.rda"))
     

#===============#
# ==== plot ====
#===============#

# pretty cool tretment plot in theory but the axis is kin of messed up
DisplayTreatment(unit.id = "wbcode2",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "dem", data = dem)


#=====================#
# ==== PanelMatch ====
#=====================#

# refinement.method -- Users may choose between standard propensity score weighting or matching (ps.weight, ps.match),
# covariate balanced propensity score weighting or matching (CBPS.weight, CBPS.match), and mahalanobis 
# distance matching (mahalanobis).
# size.match -- This sets the maximum number of control units that can be included in a matched set.
# covs.formula -- This sets which variables are considered in creating the refined matched sets. 
# This can be set to include lagged versions of any variable as well. See the PanelMatch documentation
# for more information about this parameter.

head(dem)

# This creates matches. 
PM.results <- PanelMatch(lag                       = 4,  # how long to make sure treatment matches                               
                         time.id                   = "year",  #variable name of time variables 
                         unit.id                   = "wbcode2", # name of unit identifier in data 
                         treatment                 = "dem",     # treatment variable 
                         refinement.method         = "mahalanobis", 
                         data                      = dem, 
                         match.missing             = T, 
                         covs.formula              = ~ I(lag(tradewb, 1:4)) + I(lag(y, 1:4)), # lag function tells it to match last four years of tradewb and y
                         size.match                = 5, 
                         qoi                       = "att" ,
                         outcome.var               = "y",
                         lead                      = 0:4, 
                         forbid.treatment.reversal = FALSE)

# get actual estimates from the matches above 
PE.results <- PanelEstimate(inference = "bootstrap", sets = PM.results, 
                            data = dem)
PE.results$coefficients

# summarize results 
summary(PE.results)

# plot results 
plot(PE.results)
