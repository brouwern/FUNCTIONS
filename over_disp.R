

#source("C:/Users/lisanjie2/Documents/1_R/2_Fx_FUNCTIONS/Fx_over_disp.R")

########################################
### Bolker's overdispersion function #c
# http://glmm.wikidot.com/faq

# I think this is designed for POISSOn data, not sure...
overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}


print("overdisp_fun(model)")

### Bolker's overdispersion function ####
########################################




### ACHTUNG: there is a package with an overdispersion function also!

        #################################
        #### overdispersion with aod3 ###
        #  Check for overdispersion: you can do this by hand by computing
        #  `sum(residuals(gmod_lme4_L,"pearson")^2))`, 
        #  but the `gof()` function
        #  from the `aods3` package is a handy shortcut (it computes overdispersion
        #  based on the deviance (`D` below) and Pearson residuals (`X2`): when
        #  they disagree, use the latter:
        
        # library(aod3)
        # 
        # gof(gmod_lme4_L)