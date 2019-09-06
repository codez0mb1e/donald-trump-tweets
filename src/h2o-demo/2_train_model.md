@realDonaldTrump Tweets for Dow Jones Index Direction Prediction
================
Dmitry Petukhov

**Train ML-model that predict Dow Jones Index direction by
@realDonaldTrump tweets.**

Import dependencies and read config:

``` r
options(max.print = 1e3, scipen = 999, width = 1e2)
options(stringsAsFactors = F)
```

``` r
suppressPackageStartupMessages({
  library(h2o)
  
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(magrittr)
  
  library(lubridate)
})
```

``` r
secrets <- config::get(file = "secrets.yml")
```

## Load dataset

``` r
tweets <- readRDS("cache/2019-09-06.rds") #! set your cache here

min(tweets$created_date)
```

    ## [1] "2017-01-03"

``` r
max(tweets$created_date)
```

    ## [1] "2019-08-29"

``` r
tweets %>% count(direction)
```

    ## # A tibble: 2 x 2
    ##   direction     n
    ##       <int> <int>
    ## 1         0   288
    ## 2         1   363

## Init H2O cluster

``` r
h2o.init(nthreads = -1, # use all cores
         max_mem_size = "64G") # max mem size
```

    ##  Connection successful!
    ## 
    ## R is connected to the H2O cluster: 
    ##     H2O cluster uptime:         56 minutes 9 seconds 
    ##     H2O cluster timezone:       Etc/UTC 
    ##     H2O data parsing timezone:  UTC 
    ##     H2O cluster version:        3.24.0.4 
    ##     H2O cluster version age:    3 months and 8 days  
    ##     H2O cluster name:           H2O_started_from_R_dp_vfe873 
    ##     H2O cluster total nodes:    1 
    ##     H2O cluster total memory:   63.83 GB 
    ##     H2O cluster total cores:    12 
    ##     H2O cluster allowed cores:  12 
    ##     H2O cluster healthy:        TRUE 
    ##     H2O Connection ip:          localhost 
    ##     H2O Connection port:        54321 
    ##     H2O Connection proxy:       NA 
    ##     H2O Internal Security:      FALSE 
    ##     H2O API Extensions:         Amazon S3, XGBoost, Algos, AutoML, Core V3, Core V4 
    ##     R Version:                  R version 3.5.3 (2019-03-11)

## Prepare dataset

Set metadata:

``` r
label <- "direction"
features <- setdiff(names(tweets), c("created_date", "text_total", label))
```

Split dataset:

``` r
tweets %<>% 
  mutate(direction = as.factor(direction)) 

train_dt <- tweets %>% 
  filter(created_date < Sys.Date() - months(9)) %>%
  as.h2o
```

    ## 
      |                                                                                                
      |                                                                                          |   0%
      |                                                                                                
      |==========================================================================================| 100%

``` r
dim(train_dt)
```

    ## [1] 467  11

``` r
valid_dt <- tweets %>% 
  filter(created_date >= Sys.Date() - months(9) & created_date < Sys.Date() - months(3)) %>% 
  as.h2o
```

    ## 
      |                                                                                                
      |                                                                                          |   0%
      |                                                                                                
      |==========================================================================================| 100%

``` r
dim(valid_dt)
```

    ## [1] 124  11

``` r
test_dt <- tweets %>% 
  filter(created_date >= Sys.Date() - months(3)) %>% 
  as.h2o()
```

    ## 
      |                                                                                                
      |                                                                                          |   0%
      |                                                                                                
      |==========================================================================================| 100%

``` r
dim(test_dt)
```

    ## [1] 60 11

## Train Model

Start AutoML models training:

``` r
auto <- h2o.automl(x = features, y = label,
                   training_frame = train_dt,
                   validation_frame = valid_dt,
                   leaderboard_frame = test_dt,
                   max_runtime_secs = 60*1, #! set appropriate duration of searching model
                   seed = 314)
```

    ## 
      |                                                                                                
      |                                                                                          |   0%
      |                                                                                                
      |==                                                                                        |   2%
      |                                                                                                
      |===                                                                                       |   3%
      |                                                                                                
      |=====                                                                                     |   5%
      |                                                                                                
      |=======                                                                                   |   8%
      |                                                                                                
      |============                                                                              |  13%
      |                                                                                                
      |==============                                                                            |  16%
      |                                                                                                
      |=================                                                                         |  18%
      |                                                                                                
      |===================                                                                       |  21%
      |                                                                                                
      |=====================                                                                     |  24%
      |                                                                                                
      |========================                                                                  |  26%
      |                                                                                                
      |==========================                                                                |  29%
      |                                                                                                
      |============================                                                              |  32%
      |                                                                                                
      |===============================                                                           |  34%
      |                                                                                                
      |================================                                                          |  36%
      |                                                                                                
      |=================================                                                         |  37%
      |                                                                                                
      |===================================                                                       |  38%
      |                                                                                                
      |====================================                                                      |  40%
      |                                                                                                
      |=====================================                                                     |  41%
      |                                                                                                
      |======================================                                                    |  43%
      |                                                                                                
      |========================================                                                  |  44%
      |                                                                                                
      |=========================================                                                 |  46%
      |                                                                                                
      |==========================================                                                |  47%
      |                                                                                                
      |===========================================                                               |  48%
      |                                                                                                
      |=============================================                                             |  50%
      |                                                                                                
      |==============================================                                            |  51%
      |                                                                                                
      |===============================================                                           |  52%
      |                                                                                                
      |=================================================                                         |  54%
      |                                                                                                
      |==================================================                                        |  55%
      |                                                                                                
      |===================================================                                       |  57%
      |                                                                                                
      |======================================================                                    |  61%
      |                                                                                                
      |========================================================                                  |  62%
      |                                                                                                
      |=====================================================================                     |  76%
      |                                                                                                
      |=====================================================================                     |  77%
      |                                                                                                
      |======================================================================                    |  78%
      |                                                                                                
      |=======================================================================                   |  79%
      |                                                                                                
      |========================================================================                  |  80%
      |                                                                                                
      |=========================================================================                 |  81%
      |                                                                                                
      |=========================================================================                 |  82%
      |                                                                                                
      |==========================================================================                |  83%
      |                                                                                                
      |===========================================================================               |  83%
      |                                                                                                
      |============================================================================              |  84%
      |                                                                                                
      |=============================================================================             |  85%
      |                                                                                                
      |==============================================================================            |  86%
      |                                                                                                
      |==============================================================================            |  87%
      |                                                                                                
      |===============================================================================           |  88%
      |                                                                                                
      |================================================================================          |  89%
      |                                                                                                
      |=================================================================================         |  90%
      |                                                                                                
      |==================================================================================        |  91%
      |                                                                                                
      |===================================================================================       |  92%
      |                                                                                                
      |======================================================================================    |  96%
      |                                                                                                
      |==========================================================================================| 100%

View leaderboard:

``` r
lb <- auto@leaderboard
lb
```

    ##                                              model_id       auc   logloss mean_per_class_error
    ## 1    StackedEnsemble_AllModels_AutoML_20190906_145158 0.6383929 0.6912364            0.4464286
    ## 2           GBM_grid_1_AutoML_20190906_145158_model_4 0.6383929 0.6759472            0.4464286
    ## 3 StackedEnsemble_BestOfFamily_AutoML_20190906_145158 0.6383929 0.6912354            0.4464286
    ## 4                        GBM_3_AutoML_20190906_145158 0.6372768 0.6701865            0.3906250
    ## 5       XGBoost_grid_1_AutoML_20190906_145158_model_3 0.6328125 0.6763071            0.4084821
    ## 6           GBM_grid_1_AutoML_20190906_145158_model_1 0.6194196 1.0705779            0.3928571
    ##        rmse       mse
    ## 1 0.4990476 0.2490485
    ## 2 0.4916330 0.2417030
    ## 3 0.4990471 0.2490480
    ## 4 0.4877097 0.2378607
    ## 5 0.4915495 0.2416209
    ## 6 0.5626825 0.3166116
    ## 
    ## [38 rows x 6 columns]

View best model:

``` r
best <- auto@leader
best
```

    ## Model Details:
    ## ==============
    ## 
    ## H2OBinomialModel: stackedensemble
    ## Model ID:  StackedEnsemble_AllModels_AutoML_20190906_145158 
    ## NULL
    ## 
    ## 
    ## H2OBinomialMetrics: stackedensemble
    ## ** Reported on training data. **
    ## 
    ## MSE:  0.2405689
    ## RMSE:  0.4904783
    ## LogLoss:  0.6740809
    ## Mean Per-Class Error:  0.03300953
    ## AUC:  0.9949187
    ## pr_auc:  0.9925833
    ## Gini:  0.9898375
    ## 
    ## Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
    ##          0   1    Error     Rate
    ## 0      194   8 0.039604   =8/202
    ## 1        7 258 0.026415   =7/265
    ## Totals 201 266 0.032120  =15/467
    ## 
    ## Maximum Metrics: Maximum metrics at their respective thresholds
    ##                         metric threshold    value idx
    ## 1                       max f1  0.566918 0.971751 220
    ## 2                       max f2  0.565395 0.980539 230
    ## 3                 max f0point5  0.568982 0.976917 193
    ## 4                 max accuracy  0.566918 0.967880 220
    ## 5                max precision  0.575024 1.000000   0
    ## 6                   max recall  0.562378 1.000000 284
    ## 7              max specificity  0.575024 1.000000   0
    ## 8             max absolute_mcc  0.566918 0.934540 220
    ## 9   max min_per_class_accuracy  0.567106 0.960396 217
    ## 10 max mean_per_class_accuracy  0.566918 0.966990 220
    ## 
    ## Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
    ## H2OBinomialMetrics: stackedensemble
    ## ** Reported on validation data. **
    ## 
    ## MSE:  0.2498526
    ## RMSE:  0.4998525
    ## LogLoss:  0.6928704
    ## Mean Per-Class Error:  0.4903344
    ## AUC:  0.5378788
    ## pr_auc:  0.5609876
    ## Gini:  0.07575758
    ## 
    ## Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
    ##        0   1    Error     Rate
    ## 0      2  56 0.965517   =56/58
    ## 1      1  65 0.015152    =1/66
    ## Totals 3 121 0.459677  =57/124
    ## 
    ## Maximum Metrics: Maximum metrics at their respective thresholds
    ##                         metric threshold    value idx
    ## 1                       max f1  0.558693 0.695187 120
    ## 2                       max f2  0.556331 0.850515 123
    ## 3                 max f0point5  0.561064 0.602410 107
    ## 4                 max accuracy  0.567227 0.564516  55
    ## 5                max precision  0.575636 1.000000   0
    ## 6                   max recall  0.556331 1.000000 123
    ## 7              max specificity  0.575636 1.000000   0
    ## 8             max absolute_mcc  0.567227 0.136197  55
    ## 9   max min_per_class_accuracy  0.566455 0.551724  62
    ## 10 max mean_per_class_accuracy  0.567227 0.567921  55
    ## 
    ## Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
    ## H2OBinomialMetrics: stackedensemble
    ## ** Reported on cross-validation data. **
    ## ** 5-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
    ## 
    ## MSE:  0.2454597
    ## RMSE:  0.4954389
    ## LogLoss:  0.6840392
    ## Mean Per-Class Error:  0.4969363
    ## AUC:  0.4904633
    ## pr_auc:  0.5339254
    ## Gini:  -0.01907342
    ## 
    ## Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
    ##        0   1    Error      Rate
    ## 0      2 200 0.990099  =200/202
    ## 1      1 264 0.003774    =1/265
    ## Totals 3 464 0.430407  =201/467
    ## 
    ## Maximum Metrics: Maximum metrics at their respective thresholds
    ##                         metric threshold    value idx
    ## 1                       max f1  0.559949 0.724280  94
    ## 2                       max f2  0.558100 0.867714  97
    ## 3                 max f0point5  0.562154 0.623513  89
    ## 4                 max accuracy  0.562154 0.571734  89
    ## 5                max precision  0.569552 1.000000   0
    ## 6                   max recall  0.558100 1.000000  97
    ## 7              max specificity  0.569552 1.000000   0
    ## 8             max absolute_mcc  0.568888 0.070151   6
    ## 9   max min_per_class_accuracy  0.567452 0.168317  14
    ## 10 max mean_per_class_accuracy  0.564360 0.512479  62
    ## 
    ## Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`

View feature importance:

``` r
#h2o.varimp_plot(best, num_of_features = 10)
```
