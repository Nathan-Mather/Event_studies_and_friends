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

  # requirments to run 
  # - set directories or optiosn where #set is tagged
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
  setwd("c:/Users/Nmath_000/Documents/MI_school/Case_Study_and_friends/wfe_rep_data/")

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

  
#================================================================#
# ==== I. Both formal (participant) vs. (either one or none) ====
#================================================================#

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
  
  

  #==================================#
  # ==== Before and after design ====
  #==================================#

  
  LEADS <- 5 
  lags <- c(3,5,7)

    cov <- c(FALSE)
    tre <- 2

  
  ## running models
  covariates <- cov
      TRENDS <- tre
      
      RESULTS <- vector("list", length(lags))
      
      ######################################################################
      ## Analyzing before/after over time: formal membership
      ######################################################################
      
      for(lag in 1:length(lags)){
        LAGS <- lags[lag]
        
        if(covariates){
          fname <- paste("forward-", LEADS,
                         "-lags-", LAGS, "-cv-trends", TRENDS, sep="")
        } else {
          fname <- paste("forward-", LEADS,
                         "-lags-", LAGS, "-trends", TRENDS, sep="")
        }
        
        ## remove dyads with no variation in treatment
        df.data <- summarize(group_by(Data, dyad),
                             nyears = n(), ntreatyr = sum(gattff, na.rm=T))
        sub <- filter(df.data, nyears==ntreatyr | ntreatyr==0)
        rm <- unique(as.character(sub$dyad))
        df <- Data[which(!Data$dyad %in% rm),]
        
        ## Remove all dyads that are under treatment in less then LEADS years.
        a <- summarize(group_by(df, dyad),
                       ntr = sum(gattff))
        dyad.rm <- a$dyad[a$ntr<LEADS]
        df <- df[which(!df$dyad %in% dyad.rm),]
        
        ## identifying first treated year for each dyad
        df.dyad <- summarize(group_by(df, dyad),
                             firsttreat = min(year[gattff==1], na.rm=T))
        uniq.dyad <- as.character(unique(df.dyad$dyad))
        
        ## strorages
        coefs <- vector("list", LEADS+1)
        ses <- vector("list", LEADS+1)
        nobs <- vector("list", LEADS+1)
        pvalues <- vector("list", LEADS+1)
        
        for(l in 0:LEADS){
          ## creating treatment variable
          df$treat <- rep(0, nrow(df))
          df$newyear <- df$year
          
          print(l)
          lead <- l
          keep.idx <- c()
          treat.idx <- c()
          rm.dyad <- c()
          ## removing all observations since year t except for t+lead
          for(i in 1:length(uniq.dyad)){
            if(i %% 100 == 0){
              print(i)
            }
            id <- uniq.dyad[i]
            fyear <- as.numeric(as.character(df.dyad[df.dyad$dyad == id, "firsttreat"]))
            
            idx1 <- which(df$dyad == id & df$year < fyear & df$year >= fyear-LAGS)
            idx2 <- which(df$dyad == id & df$year == fyear+lead)
            
            ## checking whether lag=7 exists
            check7 <- which(df$dyad == id & df$year == fyear-7)
            if(length(check7)==0){
              ## if(length(idx1)!=LAGS){ ## making sure that LAGS years prior to treatment exists
              keep.idx <- c(keep.idx)
              treat.idx <- c(treat.idx)
            } else {
              ## changing the treatment
              df$treat[idx2] <- 1
              ## changing the year
              df$newyear[idx2] <- fyear
              
              if(length(idx2)==0){ ## year treat+lead does not exist
                rm.dyad <- c(rm.dyad,id)
              }
              keep.idx <- c(keep.idx, idx1, idx2)
              treat.idx <- c(treat.idx, idx2)
            }
          }
          ## subset
          dat <- df[keep.idx,]
          
          ## remove dyads whose t+lead observation does not exist
          if(length(rm.dyad)>0){
            dat <- dat[which(!dat$dyad %in% rm.dyad),]
          }
          
          ## estimation
          dat$trend <- as.numeric(as.character(dat$year))-1947
          dat$trend2 <- as.numeric(dat$trend)^2
          dat$trend3 <- as.numeric(dat$trend)^3
          
          if(!covariates){
            if(TRENDS == 1){
              mod <- wfe(ltrade ~ treat + trend,
                         unit.index = "dyad",
                         treat="treat", method = "unit", data = dat,
                         qoi = "ate", store.wdm = FALSE,
                         hetero.se= TRUE, auto.se= FALSE,
                         verbose = TRUE, White = FALSE)
            } else if(TRENDS == 2){
              mod <- wfe(ltrade ~ treat + trend + trend2,
                         unit.index = "dyad",
                         treat="treat", method = "unit", data = dat,
                         qoi = "ate", store.wdm = FALSE,
                         hetero.se= TRUE, auto.se= FALSE,
                         verbose = TRUE, White = FALSE)
            }
            
          }
          if(covariates){
            if(TRENDS == 1){
              mod <- wfe(ltrade ~ treat + gsp + lrgdp + lrgdppc +
                           custrict + curcol + trend,
                         unit.index = "dyad", 
                         treat="treat", method = "unit", data = dat,
                         qoi = "ate",  store.wdm = FALSE,
                         hetero.se= TRUE, auto.se= FALSE,
                         verbose = TRUE, White = FALSE)
              
            } else if (TRENDS == 2){
              mod <- wfe(ltrade ~ treat + gsp + lrgdp + lrgdppc +
                           custrict + curcol + trend + trend2,
                         unit.index = "dyad", 
                         treat="treat", method = "unit", data = dat,
                         qoi = "ate",  store.wdm = FALSE,
                         hetero.se= TRUE, auto.se= FALSE,
                         verbose = TRUE, White = FALSE)
            }
          }
          
          print(summary(mod))
          
          if(save.mod){
            fname1 <- paste("bothff-forward-", LEADS, "-trends", TRENDS, ".RData", sep="")
            e <- environment()
            save(file = fname1, list=ls(), env=e)
          }
          
          coefs[[l+1]] <- mod$coef
          ses[[l+1]] <- mod$se
          nobs[[l+1]] <- nrow(dat)
          pvalues[[l+1]] <- mod$White.pvalue
          
          
        }
        
        RESULTS[[lag]]$bothffPE <- coefs
        RESULTS[[lag]]$bothffSE <- ses
        RESULTS[[lag]]$bothffNobs <- nobs
        RESULTS[[lag]]$bothffpvalues <- pvalues
        
        
        pe <- rep(NA, LEADS+1)
        effect.year.95up <- rep(NA, LEADS+1)
        effect.year.95lo <- rep(NA, LEADS+1)
        for(i in 1:(LEADS+1)){
          pe[i] <- as.numeric(coefs[[i]][1])
          se.i <- as.numeric(ses[[i]][1])
          effect.year.95up[i] <- pe[i] + qnorm(.975)*se.i
          effect.year.95lo[i] <- pe[i] - qnorm(.975)*se.i    
        }
        
        
        ######################################################################
        ## Analyzing before/after over time: participant membership
        ######################################################################
        
        
        ## remove dyads with no variation in treatment
        df.data <- summarize(group_by(Data, dyad),
                             nyears = n(), ntreatyr = sum(bothp, na.rm=T))
        sub <- filter(df.data, nyears==ntreatyr | ntreatyr==0)
        rm <- unique(as.character(sub$dyad))
        df <- Data[which(!Data$dyad %in% rm),]
        
        ## Remove all dyads that are under treatment in less then LEADS years.
        a <- summarize(group_by(df, dyad),
                       ntr = sum(bothp))
        dyad.rm <- a$dyad[a$ntr<LEADS]
        df <- df[which(!df$dyad %in% dyad.rm),]
        
        ## identifying first treated year for each dyad
        df.dyad <- summarize(group_by(df, dyad),
                             firsttreat = min(year[bothp==1], na.rm=T))
        uniq.dyad <- as.character(unique(df.dyad$dyad))
        
        
        ## storage
        coefs <- vector("list", LEADS+1)
        ses <- vector("list", LEADS+1)
        nobs <- vector("list", LEADS+1)
        pvalues <- vector("list", LEADS+1)
        
        for(l in 0:LEADS){
          ## creating treatment variable
          df$treat <- rep(0, nrow(df))
          df$newyear <- df$year
          
          print(l)
          lead <- l
          keep.idx <- c()
          treat.idx <- c()
          rm.dyad <- c()
          ## removing all observations since year t except for t+lead
          for(i in 1:length(uniq.dyad)){
            if(i %% 100 == 0){
              print(i)
            }
            id <- uniq.dyad[i]
            fyear <- as.numeric(as.character(df.dyad[df.dyad$dyad == id, "firsttreat"]))
            
            idx1 <- which(df$dyad == id & df$year < fyear & df$year >= fyear-LAGS)
            idx2 <- which(df$dyad == id & df$year == fyear+lead)
            
            ## checking whether lag=7 exists
            check7 <- which(df$dyad == id & df$year == fyear-7)
            if(length(check7)==0){
              ## if(length(idx1)!=LAGS){ ## making sure that LAGS years prior to treatment exists
              keep.idx <- c(keep.idx)
              treat.idx <- c(treat.idx)
            } else {
              
              ## changing the treatment
              df$treat[idx2] <- 1
              ## changing the year
              df$newyear[idx2] <- fyear
              
              if(length(idx2)==0){ ## year treat+lead does not exist
                rm.dyad <- c(rm.dyad,id)
              }
              keep.idx <- c(keep.idx, idx1, idx2)
              treat.idx <- c(treat.idx, idx2)
            }
          }
          ## subset
          dat <- df[keep.idx,]
          
          ## remove dyads whose t+lead observation does not exist
          if(length(rm.dyad)>0){
            dat <- dat[which(!dat$dyad %in% rm.dyad),]
          }
          
          ## estimation
          
          dat$trend <- as.numeric(as.character(dat$year))-1947
          dat$trend2 <- as.numeric(dat$trend)^2
          dat$trend3 <- as.numeric(dat$trend)^3
          
          if(!covariates){
            if(TRENDS == 1){
              mod <- wfe(ltrade ~ treat + trend,
                         unit.index = "dyad",
                         treat="treat", method = "unit", data = dat,
                         qoi = "ate",  store.wdm = FALSE,
                         hetero.se= TRUE, auto.se= FALSE,
                         verbose = TRUE, White = FALSE)
              
            } else if (TRENDS == 2){
              
              mod <- wfe(ltrade ~ treat + trend + trend2,
                         unit.index = "dyad",
                         treat="treat", method = "unit", data = dat,
                         qoi = "ate",  store.wdm = FALSE,
                         hetero.se= TRUE, auto.se= FALSE,
                         verbose = TRUE, White = FALSE)
            }
          }
          
          if(covariates){
            if(TRENDS==1){
              mod <- wfe(ltrade ~ treat + gsp + lrgdp + lrgdppc +
                           custrict + curcol + trend,
                         unit.index = "dyad", 
                         treat="treat", method = "unit", data = dat,
                         qoi = "ate",  store.wdm = FALSE,
                         hetero.se= TRUE, auto.se= FALSE,
                         verbose = TRUE, White = FALSE)
            } else if (TRENDS==2){
              mod <- wfe(ltrade ~ treat + gsp + lrgdp + lrgdppc +
                           custrict + curcol + trend + trend2,
                         unit.index = "dyad", 
                         treat="treat", method = "unit", data = dat,
                         qoi = "ate",  store.wdm = FALSE,
                         hetero.se= TRUE, auto.se= FALSE,
                         verbose = TRUE, White = FALSE)
            }
          }
          
          print(summary(mod))
          
          if(save.mod){
            fname2 <- paste("bothp-forward-", LEADS, "-trends", TRENDS, ".RData", sep="")
            e <- environment()
            save(file = fname2, list=ls(), env=e)
          }
          
          coefs[[l+1]] <- mod$coef
          ses[[l+1]] <- mod$se
          nobs[[l+1]] <- nrow(dat)
          pvalues[[l+1]] <- mod$White.pvalue
          
          
        }
        
        RESULTS[[lag]]$bothpPE <- coefs
        RESULTS[[lag]]$bothpSE <- ses
        RESULTS[[lag]]$bothpNobs <- nobs
        RESULTS[[lag]]$bothppvalues <- pvalues
        
        
        pe <- rep(NA, LEADS+1)
        effect.year.95up <- rep(NA, LEADS+1)
        effect.year.95lo <- rep(NA, LEADS+1)
        for(i in 1:(LEADS+1)){
          pe[i] <- as.numeric(coefs[[i]][1])
          se.i <- as.numeric(ses[[i]][1])
          effect.year.95up[i] <- pe[i] + qnorm(.975)*se.i
          effect.year.95lo[i] <- pe[i] - qnorm(.975)*se.i    
        }
        
        
      }
      
      if(covariates){ 
        fname4 <- paste("lag7-BA-forward-", LEADS, "-cv-trends", TRENDS, ".RData", sep="")
        save(RESULTS,file= file.path(OUT_DIR, fname4))
      } else {
        fname4 <- paste("lag7-BA-forward-", LEADS, "-trends", TRENDS, ".RData", sep="")
        save(RESULTS,file= file.path(OUT_DIR, fname4))
        
      }
      
    }
  }
  sessionInfo()
  
  

  