#=================================#
# ==== Understand PLM package ====
#=================================#

  
  # - Keywords  
  #  - #set will be used for things that need to be set for your specific
  #       file structure to get the code to run. Things like data directories 
  #  - #fix will be used for things that need to be fixed 
  #  - #note I will use the tags #note for things I think are important
  
  
  
  # - purpose of code:
  # Get working exampels of the main funcitons in the "plm" Package 
  # This is a huge package with a lot of little helper functions as well as several test for various 
  # aspects of panel data. For now I am juts focusing in on the PLM function as a substitute for LM on fixed effects models 
  # can explor other aspects of this as project evolves 
  
  # -requirments to run 
  # all data necessary is from the package  
  # the shel of this code is oming from the package vigette so check that out 
  # for more detailed comments 
  # https://cran.r-project.org/web/packages/plm/vignettes/plmPackage.html
  

#=================================#
# ==== load packages and data ====
#=================================#

library("plm")
library(data.table)


data("EmplUK", package="plm")
data("Produc", package="plm")
data("Grunfeld", package="plm")
data("Wages", package="plm")


#===============================#
# ==== data transformations ====
#===============================#

class(EmplUK)
head(EmplUK)

# make their special data class pdata.frame 
E <- pdata.frame(EmplUK, index=c("firm","year"), drop.index=TRUE, row.names=TRUE)
head(E)

# esentially all this does is take the fixed effects variables and make them a rowname

# it provides some moderatly usefull functionality but probably nother we will want for 
# our wrapper. For example 
head(as.matrix(E$emp))

# it can be used to create lags by groups 
test1 <- lag(E$emp, 0:2)

# but this is easily done with data.table. It's more verbose 
# but also more general and doesn't requre creting a pdata.frame 
dt_exp <- data.table(EmplUK)
setorder(dt_exp, firm, year)
dt_exp[, lag_1 := shift(emp, 1, type = "lag"), firm]
dt_exp[, lag_2 := shift(emp, 2, type = "lag"),  firm]


# check that they are equal 
all.equal(as.numeric(test1[, 2]), dt_exp$lag_1)
all.equal(as.numeric(test1[, 3]), dt_exp$lag_2)

#====================#
# ==== basic PLM ====
#====================#


  # fixed effects model 
  grun.fe <- plm(inv~value+capital, data = Grunfeld, model = "within")
  
  # check out the attributes of what it returns
  attributes(grun.fe)

  # comparable random effects model 
  grun.re <- plm(inv~value+capital, data = Grunfeld, model = "random")
  
  # we can get summary of random effects 
  summary(grun.re)
  
  # get value of fixd effecs 
  fe_val <- fixef(grun.fe, type = "dmean", vcov = grun.fe$vcov)
  # see what it returns 
  fe_val
  # check what attributes it has
  attributes(fe_val)
  # acess the Se 
  attr(fe_val, "se")
  
  #note fore the record this seems like an annoying way to return data and 
  # I would vote we not do it this way 
  
  
#============================#
# ==== More advanced PLM ====
#============================#

  # Two way effects 
  grun.tways <- plm(inv~value+capital, data = Grunfeld, effect = "twoways",
                    model = "random", random.method = "amemiya")
  summary(grun.tways)
  
  # intrumental variables estimators 
  # just ad the | sign and the variables you want
  # should work with all above methods 
  
  # load data for example 
  data("Crime", package = "plm")
  
  # convert data to data.table 
  Crime <- data.table(Crime)
  
  # the data doesn't actually have the variables we need, so make them 
  # Below is copy pasted variables we need from vignette 
  to_log <-  "lcrmrte + lprbarr + lpolpc + lprbconv + lprbpris + lavgsen + ldensity + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed + lwsta + lwloc + lpctymle + lpctmin + ltaxpc + lmix"
  # split these out by the plus sign 
  to_log <-  strsplit(to_log, " + ", fixed = TRUE)[[1]]
  # sub out the leading l to get the unlogged variables that are in the data 
  to_log <- gsub("^l", "", to_log)

  # log all the variables and create lvar column of logged var 
  Crime[, paste0("l", to_log)] <- lapply(Crime[,to_log, with = FALSE],  log)
  
  # run model 
  cr <- plm(lcrmrte ~ lprbarr + lpolpc + lprbconv + lprbpris + lavgsen +
              ldensity + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed +
              lwsta + lwloc + lpctymle + lpctmin + region + smsa + factor(year)
            | . - lprbarr - lpolpc + ltaxpc + lmix,
            data = Crime, model = "random")
  
  # match results from vignette 
  summary(cr)
  
  
# bunch of other stuff in this package but unsure if it is relevent 
#================================#
# ==== variable coefficients ====
#================================#
  
#========================#
# ==== GMM estimator ====
#========================#

#==============================#
# ==== General FGLS models ====
#==============================#

#=============================#
# ==== other tests in PLM ====
#=============================#





