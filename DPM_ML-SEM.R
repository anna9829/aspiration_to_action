install.packages(c("haven", "lavaan"))
library(haven)
library(lavaan)

lavaan.datafile <- read_dta("Directory address")



## Variable list ##
# eduasp2-5 = educational aspirations at waves 2-5
# selfhour1-5 = Self-study time at waves 1-5
# tutorhour1-5 = Tutoring time at waves 1-5
# peduasp2-5 = Parental aspiration at waves 2-5
# stdperform2-5 = School performance at waves 2-5
# emoeng2-5 = School engagement at waves 2-5
# Alpha = Latent variable for individual fixed effects
# weight = Sampling weights
# highedu = binary indicator of parental education


## Self-study models with varying constraints on coefficients ##

# 1. Unconstrained model

self_m1 <- '
  # Structural Equations
    selfhour2 ~ selfhour1 + eduasp2
    selfhour3 ~ selfhour2 + eduasp3
    selfhour4 ~ selfhour3 + eduasp4
    selfhour5 ~ selfhour4 + eduasp5

  # Alpha loadings equal 1 for all times
    Alpha =~ 1*selfhour2 + 1*selfhour3 + 1*selfhour4 + 1*selfhour5 

  # Fixed Effects Model - Alpha correlated with Time-Varying Exogenous Vars
    Alpha ~~ selfhour1 + eduasp2 + eduasp3 + eduasp4 + eduasp5 

  # Correlations between Ys and predetermined variables
    eduasp3 ~~ selfhour2
    eduasp4 ~~ selfhour2 + selfhour3
    eduasp5 ~~ selfhour2 + selfhour3 + selfhour4

  # Constants free to vary across time
    selfhour2 ~ 1
    selfhour3 ~ 1
    selfhour4 ~ 1
    selfhour5 ~ 1
  
  # Error variances free to vary across time
    selfhour2 ~~ selfhour2
    selfhour3 ~~ selfhour3
    selfhour4 ~~ selfhour4
    selfhour5 ~~ selfhour5
     
  # Exogenous variable covariances
    selfhour1 ~~ eduasp2 + eduasp3 + eduasp4 + eduasp5
    eduasp2 ~~ eduasp3 + eduasp4 + eduasp5
    eduasp3 ~~ eduasp4 + eduasp5
    eduasp4 ~~ eduasp5
    
 '

self_m1.results <- lavaan::sem(self_m1, 
                               data = lavaan.datafile,
                               missing = "fiml",           # Full information maximum likelihood for missing data
                               estimator = "MLR",          # Changed from ML to MLR for robust estimation
                               sampling.weights = "weight" # Adding sampling weights
)

lavaan::summary(self_m1.results, fit.measures=TRUE)


# 2. Equivalent autoregression

self_m2 <- '
  # Structural Equations
    selfhour2 ~ c1*selfhour1 + eduasp2
    selfhour3 ~ c1*selfhour2 + eduasp3
    selfhour4 ~ c1*selfhour3 + eduasp4
    selfhour5 ~ c1*selfhour4 + eduasp5

  # Alpha loadings equal 1 for all times
    Alpha =~ 1*selfhour2 + 1*selfhour3 + 1*selfhour4 + 1*selfhour5 

  # Fixed Effects Model - Alpha correlated with Time-Varying Exogenous Vars
    Alpha ~~ selfhour1 + eduasp2 + eduasp3 + eduasp4 + eduasp5 

  # Correlations between Ys and predetermined variables
    eduasp3 ~~ selfhour2
    eduasp4 ~~ selfhour2 + selfhour3
    eduasp5 ~~ selfhour2 + selfhour3 + selfhour4

  # Constants free to vary across time
    selfhour2 ~ 1
    selfhour3 ~ 1
    selfhour4 ~ 1
    selfhour5 ~ 1
  
  # Error variances free to vary across time
    selfhour2 ~~ selfhour2
    selfhour3 ~~ selfhour3
    selfhour4 ~~ selfhour4
    selfhour5 ~~ selfhour5
     
  # Exogenous variable covariances
    selfhour1 ~~ eduasp2 + eduasp3 + eduasp4 + eduasp5
    eduasp2 ~~ eduasp3 + eduasp4 + eduasp5
    eduasp3 ~~ eduasp4 + eduasp5
    eduasp4 ~~ eduasp5
    
 '

self_m2.results <- lavaan::sem(self_m2, 
                               data = lavaan.datafile,
                               missing = "fiml",           # Full information maximum likelihood for missing data
                               estimator = "MLR",          # Changed from ML to MLR for robust estimation
                               sampling.weights = "weight" # Adding sampling weights
)

lavaan::summary(self_m2.results, fit.measures=TRUE)


# 3. Eequivalent coefficients

self_m3 <- '
  # Structural Equations
    selfhour2 ~ selfhour1 + c2*eduasp2
    selfhour3 ~ selfhour2 + c2*eduasp3
    selfhour4 ~ selfhour3 + c2*eduasp4
    selfhour5 ~ selfhour4 + c2*eduasp5

  # Alpha loadings equal 1 for all times
    Alpha =~ 1*selfhour2 + 1*selfhour3 + 1*selfhour4 + 1*selfhour5 

  # Fixed Effects Model - Alpha correlated with Time-Varying Exogenous Vars
    Alpha ~~ selfhour1 + eduasp2 + eduasp3 + eduasp4 + eduasp5 

  # Correlations between Ys and predetermined variables
    eduasp3 ~~ selfhour2
    eduasp4 ~~ selfhour2 + selfhour3
    eduasp5 ~~ selfhour2 + selfhour3 + selfhour4

  # Constants free to vary across time
    selfhour2 ~ 1
    selfhour3 ~ 1
    selfhour4 ~ 1
    selfhour5 ~ 1
  
  # Error variances free to vary across time
    selfhour2 ~~ selfhour2
    selfhour3 ~~ selfhour3
    selfhour4 ~~ selfhour4
    selfhour5 ~~ selfhour5
     
  # Exogenous variable covariances
    selfhour1 ~~ eduasp2 + eduasp3 + eduasp4 + eduasp5
    eduasp2 ~~ eduasp3 + eduasp4 + eduasp5
    eduasp3 ~~ eduasp4 + eduasp5
    eduasp4 ~~ eduasp5
    
 '

self_m3.results <- lavaan::sem(self_m3, 
                               data = lavaan.datafile,
                               missing = "fiml",           # Full information maximum likelihood for missing data
                               estimator = "MLR",          # Changed from ML to MLR for robust estimation
                               sampling.weights = "weight" # Adding sampling weights
)

lavaan::summary(self_m3.results, fit.measures=TRUE)


# 4. Eequivalent autoregression + equivalent coefficients

self_m4 <- '
  # Structural Equations
    selfhour2 ~ c1*selfhour1 + c2*eduasp2
    selfhour3 ~ c1*selfhour2 + c2*eduasp3
    selfhour4 ~ c1*selfhour3 + c2*eduasp4
    selfhour5 ~ c1*selfhour4 + c2*eduasp5

  # Alpha loadings equal 1 for all times
    Alpha =~ 1*selfhour2 + 1*selfhour3 + 1*selfhour4 + 1*selfhour5 

  # Fixed Effects Model - Alpha correlated with Time-Varying Exogenous Vars
    Alpha ~~ selfhour1 + eduasp2 + eduasp3 + eduasp4 + eduasp5 

  # Correlations between Ys and predetermined variables
    eduasp3 ~~ selfhour2
    eduasp4 ~~ selfhour2 + selfhour3
    eduasp5 ~~ selfhour2 + selfhour3 + selfhour4

  # Constants free to vary across time
    selfhour2 ~ 1
    selfhour3 ~ 1
    selfhour4 ~ 1
    selfhour5 ~ 1
  
  # Error variances free to vary across time
    selfhour2 ~~ selfhour2
    selfhour3 ~~ selfhour3
    selfhour4 ~~ selfhour4
    selfhour5 ~~ selfhour5
     
  # Exogenous variable covariances
    selfhour1 ~~ eduasp2 + eduasp3 + eduasp4 + eduasp5
    eduasp2 ~~ eduasp3 + eduasp4 + eduasp5
    eduasp3 ~~ eduasp4 + eduasp5
    eduasp4 ~~ eduasp5
    
 '

self_m4.results <- lavaan::sem(self_m4, 
                               data = lavaan.datafile,
                               missing = "fiml",           # Full information maximum likelihood for missing data
                               estimator = "MLR",          # Changed from ML to MLR for robust estimation
                               sampling.weights = "weight" # Adding sampling weights
)

lavaan::summary(self_m4.results, fit.measures=TRUE)

# Comparing fit indices

fitMeasures(self_m1.results, 
            c("chisq.scaled","df.scaled","pvalue.scaled", 
              "cfi.robust", "AIC", "BIC", "rmsea.robust","rmsea.ci.lower.robust","rmsea.ci.upper.robust",
              "srmr"))#BEST MODEL
fitMeasures(self_m2.results, 
            c("chisq.scaled","df.scaled","pvalue.scaled", 
              "cfi.robust", "AIC", "BIC","rmsea.robust","rmsea.ci.lower.robust","rmsea.ci.upper.robust",
              "srmr"))
fitMeasures(self_m3.results, 
            c("chisq.scaled","df.scaled","pvalue.scaled", 
              "cfi.robust", "AIC", "BIC","rmsea.robust","rmsea.ci.lower.robust","rmsea.ci.upper.robust",
              "srmr"))
fitMeasures(self_m4.results,
            c("chisq.scaled","df.scaled","pvalue.scaled", 
              "cfi.robust", "AIC", "BIC","rmsea.robust","rmsea.ci.lower.robust","rmsea.ci.upper.robust",
              "srmr"))



## Self-study with controls (Model 3 in Table 1) ##

self_m3_control <- '
  # Structural Equations
    selfhour2 ~ selfhour1 + c2*eduasp2 + c3*peduasp2 + c4*tutorhour2
              + c5*stdperform2 + c6*emoeng2
    selfhour3 ~ selfhour2 + c2*eduasp3 + c3*peduasp3 + c4*tutorhour3
              + c5*stdperform3 + c6*emoeng3
    selfhour4 ~ selfhour3 + c2*eduasp4 + c3*peduasp4 + c4*tutorhour4
              + c5*stdperform4 + c6*emoeng4
    selfhour5 ~ selfhour4 + c2*eduasp5 + c3*peduasp5 + c4*tutorhour5
              + c5*stdperform5 + c6*emoeng5

  # Alpha loadings equal 1 for all times
    Alpha =~ 1*selfhour2 + 1*selfhour3 + 1*selfhour4 + 1*selfhour5 

  # Fixed Effects Model - Alpha correlated with Time-Varying Exogenous Vars
     Alpha ~~ selfhour1  + eduasp2  + peduasp2  + tutorhour2  + stdperform2 
           + emoeng2  + eduasp3  + peduasp3  + tutorhour3  + stdperform3 
           + emoeng3  + eduasp4  + peduasp4  + tutorhour4  + stdperform4 
           + emoeng4  + eduasp5  + peduasp5  + tutorhour5  + stdperform5 
           + emoeng5 

  # Correlations between Ys and predetermined variables
    eduasp3 ~~ selfhour2
    peduasp3 ~~ selfhour2
    tutorhour3 ~~ selfhour2
    stdperform3 ~~ selfhour2
    emoeng3 ~~ selfhour2
    
    eduasp4 ~~ selfhour2 + selfhour3
    peduasp4 ~~ selfhour2 + selfhour3
    tutorhour4 ~~ selfhour2 + selfhour3
    stdperform4 ~~ selfhour2 + selfhour3
    emoeng4 ~~ selfhour2 + selfhour3
    
    eduasp5 ~~ selfhour2 + selfhour3 + selfhour4
    peduasp5 ~~ selfhour2 + selfhour3 + selfhour4
    tutorhour5 ~~ selfhour2 + selfhour3 + selfhour4
    stdperform5 ~~ selfhour2 + selfhour3 + selfhour4
    emoeng5 ~~ selfhour2 + selfhour3 + selfhour4

  # Constants free to vary across time
    selfhour2 ~ 1
    selfhour3 ~ 1
    selfhour4 ~ 1
    selfhour5 ~ 1
  
  # Error variances free to vary across time
    selfhour2 ~~ selfhour2
    selfhour3 ~~ selfhour3
    selfhour4 ~~ selfhour4
    selfhour5 ~~ selfhour5
     
  # Exogenous variable covariances
    selfhour1 ~~
        + eduasp2 + peduasp2 + tutorhour2 + stdperform2
        + emoeng2 + eduasp3 + peduasp3 + tutorhour3
        + stdperform3 + emoeng3 + eduasp4 + peduasp4
        + tutorhour4 + stdperform4 + emoeng4 + eduasp5
        + peduasp5 + tutorhour5 + stdperform5 + emoeng5

    eduasp2 ~~
        + peduasp2 + tutorhour2 + stdperform2 + emoeng2
        + eduasp3 + peduasp3 + tutorhour3 + stdperform3
        + emoeng3 + eduasp4 + peduasp4 + tutorhour4
        + stdperform4 + emoeng4 + eduasp5 + peduasp5
        + tutorhour5 + stdperform5 + emoeng5

    peduasp2 ~~
        + tutorhour2 + stdperform2 + emoeng2 + eduasp3
        + peduasp3 + tutorhour3 + stdperform3 + emoeng3
        + eduasp4 + peduasp4 + tutorhour4 + stdperform4
        + emoeng4 + eduasp5 + peduasp5 + tutorhour5
        + stdperform5 + emoeng5

    tutorhour2 ~~
        + stdperform2 + emoeng2 + eduasp3
        + peduasp3 + tutorhour3 + stdperform3
        + emoeng3 + eduasp4 + peduasp4
        + tutorhour4 + stdperform4 + emoeng4
        + eduasp5 + peduasp5 + tutorhour5
        + stdperform5 + emoeng5

    stdperform2 ~~
        + emoeng2 + eduasp3 + peduasp3
        + tutorhour3 + stdperform3 + emoeng3
        + eduasp4 + peduasp4 + tutorhour4
        + stdperform4 + emoeng4 + eduasp5
        + peduasp5 + tutorhour5 + stdperform5
        + emoeng5

    emoeng2 ~~
        + eduasp3 + peduasp3 + tutorhour3 + stdperform3
        + emoeng3 + eduasp4 + peduasp4 + tutorhour4
        + stdperform4 + emoeng4 + eduasp5 + peduasp5
        + tutorhour5 + stdperform5 + emoeng5

    eduasp3 ~~
        + peduasp3 + tutorhour3 + stdperform3 + emoeng3
        + eduasp4 + peduasp4 + tutorhour4 + stdperform4
        + emoeng4 + eduasp5 + peduasp5 + tutorhour5
        + stdperform5 + emoeng5

    peduasp3 ~~
        + tutorhour3 + stdperform3 + emoeng3 + eduasp4
        + peduasp4 + tutorhour4 + stdperform4 + emoeng4
        + eduasp5 + peduasp5 + tutorhour5 + stdperform5
        + emoeng5

    tutorhour3 ~~
        + stdperform3 + emoeng3 + eduasp4
        + peduasp4 + tutorhour4 + stdperform4
        + emoeng4 + eduasp5 + peduasp5
        + tutorhour5 + stdperform5 + emoeng5

    stdperform3 ~~
        + emoeng3 + eduasp4 + peduasp4
        + tutorhour4 + stdperform4 + emoeng4
        + eduasp5 + peduasp5 + tutorhour5
        + stdperform5 + emoeng5

    emoeng3 ~~
        + eduasp4 + peduasp4 + tutorhour4 + stdperform4
        + emoeng4 + eduasp5 + peduasp5 + tutorhour5
        + stdperform5 + emoeng5

    eduasp4 ~~
        + peduasp4 + tutorhour4 + stdperform4 + emoeng4
        + eduasp5 + peduasp5 + tutorhour5 + stdperform5
        + emoeng5

    peduasp4 ~~
        + tutorhour4 + stdperform4 + emoeng4 + eduasp5
        + peduasp5 + tutorhour5 + stdperform5 + emoeng5

    tutorhour4 ~~
        + stdperform4 + emoeng4 + eduasp5
        + peduasp5 + tutorhour5 + stdperform5
        + emoeng5

    stdperform4 ~~
        + emoeng4 + eduasp5 + peduasp5
        + tutorhour5 + stdperform5 + emoeng5

    emoeng4 ~~
        + eduasp5 + peduasp5 + tutorhour5 + stdperform5
        + emoeng5

    eduasp5 ~~
        + peduasp5 + tutorhour5 + stdperform5 + emoeng5

    peduasp5 ~~
        + tutorhour5 + stdperform5 + emoeng5

    tutorhour5 ~~
        + stdperform5 + emoeng5

    stdperform5 ~~
        + emoeng5
  '

self_m3_control.results <- sem(self_m3_control, 
                               data = lavaan.datafile,
                               missing = "fiml",            # Full information maximum likelihood for missing data
                               estimator = "MLR",           # Changed from ML to MLR for robust estimation
                               sampling.weights = "weight"  # Adding sampling weights
)

summary(self_m3_control.results, fit.measures=TRUE)



## Self-study with controls + Multigroup analysis (Models 1-2 in Table 2) ##

self_m3_control_group <- '
  # Structural Equations
    selfhour2 ~ c(auto1_low, auto1_high)*selfhour1 + c(asp_low, asp_high)*eduasp2 + c(pasp_low, pasp_high)*peduasp2 + c(tut_low, tut_high)*tutorhour2
              + c(per_low, per_high)*stdperform2 + c(emo_low, emo_high)*emoeng2
    selfhour3 ~ c(auto2_low, auto2_high)*selfhour2 + c(asp_low, asp_high)*eduasp3 + c(pasp_low, pasp_high)*peduasp3 + c(tut_low, tut_high)*tutorhour3
              + c(per_low, per_high)*stdperform3 + c(emo_low, emo_high)*emoeng3
    selfhour4 ~ c(auto3_low, auto3_high)*selfhour3 + c(asp_low, asp_high)*eduasp4 + c(pasp_low, pasp_high)*peduasp4 + c(tut_low, tut_high)*tutorhour4
              + c(per_low, per_high)*stdperform4 + c(emo_low, emo_high)*emoeng4
    selfhour5 ~ c(auto4_low, auto4_high)*selfhour4 + c(asp_low, asp_high)*eduasp5 + c(pasp_low, pasp_high)*peduasp5 + c(tut_low, tut_high)*tutorhour5
              + c(per_low, per_high)*stdperform5 + c(emo_low, emo_high)*emoeng5

  # Alpha loadings equal 1 for all times
    Alpha =~ 1*selfhour2 + 1*selfhour3 + 1*selfhour4 + 1*selfhour5 

  # Fixed Effects Model - Alpha correlated with Time-Varying Exogenous Vars
     Alpha ~~ selfhour1  + eduasp2  + peduasp2  + tutorhour2  + stdperform2 
           + emoeng2  + eduasp3  + peduasp3  + tutorhour3  + stdperform3 
           + emoeng3  + eduasp4  + peduasp4  + tutorhour4  + stdperform4 
           + emoeng4  + eduasp5  + peduasp5  + tutorhour5  + stdperform5 
           + emoeng5 

  # Correlations between Ys and predetermined variables
    eduasp3 ~~ selfhour2
    peduasp3 ~~ selfhour2
    tutorhour3 ~~ selfhour2
    stdperform3 ~~ selfhour2
    emoeng3 ~~ selfhour2
    
    eduasp4 ~~ selfhour2 + selfhour3
    peduasp4 ~~ selfhour2 + selfhour3
    tutorhour4 ~~ selfhour2 + selfhour3
    stdperform4 ~~ selfhour2 + selfhour3
    emoeng4 ~~ selfhour2 + selfhour3
    
    eduasp5 ~~ selfhour2 + selfhour3 + selfhour4
    peduasp5 ~~ selfhour2 + selfhour3 + selfhour4
    tutorhour5 ~~ selfhour2 + selfhour3 + selfhour4
    stdperform5 ~~ selfhour2 + selfhour3 + selfhour4
    emoeng5 ~~ selfhour2 + selfhour3 + selfhour4

  # Constants free to vary across time
    selfhour2 ~ 1
    selfhour3 ~ 1
    selfhour4 ~ 1
    selfhour5 ~ 1
  
  # Error variances free to vary across time
    selfhour2 ~~ selfhour2
    selfhour3 ~~ selfhour3
    selfhour4 ~~ selfhour4
    selfhour5 ~~ selfhour5
     
  # Exogenous variable covariances
    selfhour1 ~~
        + eduasp2 + peduasp2 + tutorhour2 + stdperform2
        + emoeng2 + eduasp3 + peduasp3 + tutorhour3
        + stdperform3 + emoeng3 + eduasp4 + peduasp4
        + tutorhour4 + stdperform4 + emoeng4 + eduasp5
        + peduasp5 + tutorhour5 + stdperform5 + emoeng5

    eduasp2 ~~
        + peduasp2 + tutorhour2 + stdperform2 + emoeng2
        + eduasp3 + peduasp3 + tutorhour3 + stdperform3
        + emoeng3 + eduasp4 + peduasp4 + tutorhour4
        + stdperform4 + emoeng4 + eduasp5 + peduasp5
        + tutorhour5 + stdperform5 + emoeng5

    peduasp2 ~~
        + tutorhour2 + stdperform2 + emoeng2 + eduasp3
        + peduasp3 + tutorhour3 + stdperform3 + emoeng3
        + eduasp4 + peduasp4 + tutorhour4 + stdperform4
        + emoeng4 + eduasp5 + peduasp5 + tutorhour5
        + stdperform5 + emoeng5

    tutorhour2 ~~
        + stdperform2 + emoeng2 + eduasp3
        + peduasp3 + tutorhour3 + stdperform3
        + emoeng3 + eduasp4 + peduasp4
        + tutorhour4 + stdperform4 + emoeng4
        + eduasp5 + peduasp5 + tutorhour5
        + stdperform5 + emoeng5

    stdperform2 ~~
        + emoeng2 + eduasp3 + peduasp3
        + tutorhour3 + stdperform3 + emoeng3
        + eduasp4 + peduasp4 + tutorhour4
        + stdperform4 + emoeng4 + eduasp5
        + peduasp5 + tutorhour5 + stdperform5
        + emoeng5

    emoeng2 ~~
        + eduasp3 + peduasp3 + tutorhour3 + stdperform3
        + emoeng3 + eduasp4 + peduasp4 + tutorhour4
        + stdperform4 + emoeng4 + eduasp5 + peduasp5
        + tutorhour5 + stdperform5 + emoeng5

    eduasp3 ~~
        + peduasp3 + tutorhour3 + stdperform3 + emoeng3
        + eduasp4 + peduasp4 + tutorhour4 + stdperform4
        + emoeng4 + eduasp5 + peduasp5 + tutorhour5
        + stdperform5 + emoeng5

    peduasp3 ~~
        + tutorhour3 + stdperform3 + emoeng3 + eduasp4
        + peduasp4 + tutorhour4 + stdperform4 + emoeng4
        + eduasp5 + peduasp5 + tutorhour5 + stdperform5
        + emoeng5

    tutorhour3 ~~
        + stdperform3 + emoeng3 + eduasp4
        + peduasp4 + tutorhour4 + stdperform4
        + emoeng4 + eduasp5 + peduasp5
        + tutorhour5 + stdperform5 + emoeng5

    stdperform3 ~~
        + emoeng3 + eduasp4 + peduasp4
        + tutorhour4 + stdperform4 + emoeng4
        + eduasp5 + peduasp5 + tutorhour5
        + stdperform5 + emoeng5

    emoeng3 ~~
        + eduasp4 + peduasp4 + tutorhour4 + stdperform4
        + emoeng4 + eduasp5 + peduasp5 + tutorhour5
        + stdperform5 + emoeng5

    eduasp4 ~~
        + peduasp4 + tutorhour4 + stdperform4 + emoeng4
        + eduasp5 + peduasp5 + tutorhour5 + stdperform5
        + emoeng5

    peduasp4 ~~
        + tutorhour4 + stdperform4 + emoeng4 + eduasp5
        + peduasp5 + tutorhour5 + stdperform5 + emoeng5

    tutorhour4 ~~
        + stdperform4 + emoeng4 + eduasp5
        + peduasp5 + tutorhour5 + stdperform5
        + emoeng5

    stdperform4 ~~
        + emoeng4 + eduasp5 + peduasp5
        + tutorhour5 + stdperform5 + emoeng5

    emoeng4 ~~
        + eduasp5 + peduasp5 + tutorhour5 + stdperform5
        + emoeng5

    eduasp5 ~~
        + peduasp5 + tutorhour5 + stdperform5 + emoeng5

    peduasp5 ~~
        + tutorhour5 + stdperform5 + emoeng5

    tutorhour5 ~~
        + stdperform5 + emoeng5

    stdperform5 ~~
        + emoeng5
'

self_m3_control_group.results <- sem(self_m3_control_group, 
                                     data = lavaan.datafile,
                                     missing = "fiml",             # Full information maximum likelihood for missing data
                                     estimator = "MLR",            # Changed from ML to MLR for robust estimation
                                     sampling.weights = "weight",  # Adding sampling weights
                                     group = "highedu"             # Group indicator
)

summary(self_m3_control_group.results, fit.measures=TRUE)


## Wald tests for coefficient comparison ##

lavTestWald(self_m3_control_group.results, constraints = "asp_low == asp_high")
lavTestWald(self_m3_control_group.results, constraints = "auto1_low == auto1_high")
lavTestWald(self_m3_control_group.results, constraints = "auto2_low == auto2_high")
lavTestWald(self_m3_control_group.results, constraints = "auto3_low == auto3_high")
lavTestWald(self_m3_control_group.results, constraints = "auto4_low == auto4_high")
lavTestWald(self_m3_control_group.results, constraints = "pasp_low == pasp_high")
lavTestWald(self_m3_control_group.results, constraints = "per_low == per_high")
lavTestWald(self_m3_control_group.results, constraints = "tut_low == tut_high")
