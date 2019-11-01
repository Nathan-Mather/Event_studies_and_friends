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

    
  
#=================================#
# ==== exploring more options ====
#=================================#


# havnt done this yet but as we start developing the wrapper this may become more relevent. 


#==========================#
# ==== other functions ====
#==========================#


