# Coursera: Practical Machine Learning Prediction Assignment
Benjamin Chan  
Tuesday, October 07, 2014  


> **Background**

> Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 


> **Data **

> The training data for this project are available here: 

> https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

> The test data are available here: 

> https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

> The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment. 

> **What you should submit**

> The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases. 

> 1. Your submission should consist of a link to a Github repo with your R markdown and compiled HTML file describing your analysis. Please constrain the text of the writeup to < 2000 words and the number of figures to be less than 5. It will make it easier for the graders if you submit a repo with a gh-pages branch so the HTML page can be viewed online (and you always want to make it easy on graders :-).
> 2. You should also apply your machine learning algorithm to the 20 test cases available in the test data above. Please submit your predictions in appropriate format to the programming assignment for automated grading. See the programming assignment for additional details. 

> **Reproducibility **

> Due to security concerns with the exchange of R code, your code will not be run during the evaluation by your classmates. Please be sure that if they download the repo, they will be able to view the compiled HTML version of your analysis. 


# Get the data

Read the training data into a data table.


```r
require(data.table)
```

```
## Loading required package: data.table
```

```r
setInternet2(TRUE)
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
DTrain <- fread(url)
```

Make `classe` into a factor.


```r
DTrain <- DTrain[, classe := factor(classe)]
is.factor(DTrain[, classe])
```

```
## [1] TRUE
```

```r
DTrain[, .N, classe]
```

```
##    classe    N
## 1:      A 5580
## 2:      B 3797
## 3:      C 3422
## 4:      D 3216
## 5:      E 3607
```

Get a *feel* for the training data.


```r
names(DTrain)
```

```
##   [1] "V1"                       "user_name"               
##   [3] "raw_timestamp_part_1"     "raw_timestamp_part_2"    
##   [5] "cvtd_timestamp"           "new_window"              
##   [7] "num_window"               "roll_belt"               
##   [9] "pitch_belt"               "yaw_belt"                
##  [11] "total_accel_belt"         "kurtosis_roll_belt"      
##  [13] "kurtosis_picth_belt"      "kurtosis_yaw_belt"       
##  [15] "skewness_roll_belt"       "skewness_roll_belt.1"    
##  [17] "skewness_yaw_belt"        "max_roll_belt"           
##  [19] "max_picth_belt"           "max_yaw_belt"            
##  [21] "min_roll_belt"            "min_pitch_belt"          
##  [23] "min_yaw_belt"             "amplitude_roll_belt"     
##  [25] "amplitude_pitch_belt"     "amplitude_yaw_belt"      
##  [27] "var_total_accel_belt"     "avg_roll_belt"           
##  [29] "stddev_roll_belt"         "var_roll_belt"           
##  [31] "avg_pitch_belt"           "stddev_pitch_belt"       
##  [33] "var_pitch_belt"           "avg_yaw_belt"            
##  [35] "stddev_yaw_belt"          "var_yaw_belt"            
##  [37] "gyros_belt_x"             "gyros_belt_y"            
##  [39] "gyros_belt_z"             "accel_belt_x"            
##  [41] "accel_belt_y"             "accel_belt_z"            
##  [43] "magnet_belt_x"            "magnet_belt_y"           
##  [45] "magnet_belt_z"            "roll_arm"                
##  [47] "pitch_arm"                "yaw_arm"                 
##  [49] "total_accel_arm"          "var_accel_arm"           
##  [51] "avg_roll_arm"             "stddev_roll_arm"         
##  [53] "var_roll_arm"             "avg_pitch_arm"           
##  [55] "stddev_pitch_arm"         "var_pitch_arm"           
##  [57] "avg_yaw_arm"              "stddev_yaw_arm"          
##  [59] "var_yaw_arm"              "gyros_arm_x"             
##  [61] "gyros_arm_y"              "gyros_arm_z"             
##  [63] "accel_arm_x"              "accel_arm_y"             
##  [65] "accel_arm_z"              "magnet_arm_x"            
##  [67] "magnet_arm_y"             "magnet_arm_z"            
##  [69] "kurtosis_roll_arm"        "kurtosis_picth_arm"      
##  [71] "kurtosis_yaw_arm"         "skewness_roll_arm"       
##  [73] "skewness_pitch_arm"       "skewness_yaw_arm"        
##  [75] "max_roll_arm"             "max_picth_arm"           
##  [77] "max_yaw_arm"              "min_roll_arm"            
##  [79] "min_pitch_arm"            "min_yaw_arm"             
##  [81] "amplitude_roll_arm"       "amplitude_pitch_arm"     
##  [83] "amplitude_yaw_arm"        "roll_dumbbell"           
##  [85] "pitch_dumbbell"           "yaw_dumbbell"            
##  [87] "kurtosis_roll_dumbbell"   "kurtosis_picth_dumbbell" 
##  [89] "kurtosis_yaw_dumbbell"    "skewness_roll_dumbbell"  
##  [91] "skewness_pitch_dumbbell"  "skewness_yaw_dumbbell"   
##  [93] "max_roll_dumbbell"        "max_picth_dumbbell"      
##  [95] "max_yaw_dumbbell"         "min_roll_dumbbell"       
##  [97] "min_pitch_dumbbell"       "min_yaw_dumbbell"        
##  [99] "amplitude_roll_dumbbell"  "amplitude_pitch_dumbbell"
## [101] "amplitude_yaw_dumbbell"   "total_accel_dumbbell"    
## [103] "var_accel_dumbbell"       "avg_roll_dumbbell"       
## [105] "stddev_roll_dumbbell"     "var_roll_dumbbell"       
## [107] "avg_pitch_dumbbell"       "stddev_pitch_dumbbell"   
## [109] "var_pitch_dumbbell"       "avg_yaw_dumbbell"        
## [111] "stddev_yaw_dumbbell"      "var_yaw_dumbbell"        
## [113] "gyros_dumbbell_x"         "gyros_dumbbell_y"        
## [115] "gyros_dumbbell_z"         "accel_dumbbell_x"        
## [117] "accel_dumbbell_y"         "accel_dumbbell_z"        
## [119] "magnet_dumbbell_x"        "magnet_dumbbell_y"       
## [121] "magnet_dumbbell_z"        "roll_forearm"            
## [123] "pitch_forearm"            "yaw_forearm"             
## [125] "kurtosis_roll_forearm"    "kurtosis_picth_forearm"  
## [127] "kurtosis_yaw_forearm"     "skewness_roll_forearm"   
## [129] "skewness_pitch_forearm"   "skewness_yaw_forearm"    
## [131] "max_roll_forearm"         "max_picth_forearm"       
## [133] "max_yaw_forearm"          "min_roll_forearm"        
## [135] "min_pitch_forearm"        "min_yaw_forearm"         
## [137] "amplitude_roll_forearm"   "amplitude_pitch_forearm" 
## [139] "amplitude_yaw_forearm"    "total_accel_forearm"     
## [141] "var_accel_forearm"        "avg_roll_forearm"        
## [143] "stddev_roll_forearm"      "var_roll_forearm"        
## [145] "avg_pitch_forearm"        "stddev_pitch_forearm"    
## [147] "var_pitch_forearm"        "avg_yaw_forearm"         
## [149] "stddev_yaw_forearm"       "var_yaw_forearm"         
## [151] "gyros_forearm_x"          "gyros_forearm_y"         
## [153] "gyros_forearm_z"          "accel_forearm_x"         
## [155] "accel_forearm_y"          "accel_forearm_z"         
## [157] "magnet_forearm_x"         "magnet_forearm_y"        
## [159] "magnet_forearm_z"         "classe"
```

```r
DTrain
```

```
##           V1 user_name raw_timestamp_part_1 raw_timestamp_part_2
##     1:     1  carlitos           1323084231               788290
##     2:     2  carlitos           1323084231               808298
##     3:     3  carlitos           1323084231               820366
##     4:     4  carlitos           1323084232               120339
##     5:     5  carlitos           1323084232               196328
##    ---                                                          
## 19618: 19618    adelmo           1322832937               588376
## 19619: 19619    adelmo           1322832937               596287
## 19620: 19620    adelmo           1322832937               636283
## 19621: 19621    adelmo           1322832937               964299
## 19622: 19622    adelmo           1322832937               972293
##          cvtd_timestamp new_window num_window roll_belt pitch_belt
##     1: 05/12/2011 11:23         no         11      1.41       8.07
##     2: 05/12/2011 11:23         no         11      1.41       8.07
##     3: 05/12/2011 11:23         no         11      1.42       8.07
##     4: 05/12/2011 11:23         no         12      1.48       8.05
##     5: 05/12/2011 11:23         no         12      1.48       8.07
##    ---                                                            
## 19618: 02/12/2011 13:35         no        864    147.00     -34.80
## 19619: 02/12/2011 13:35         no        864    145.00     -35.30
## 19620: 02/12/2011 13:35         no        864    145.00     -35.50
## 19621: 02/12/2011 13:35         no        864    143.00     -35.90
## 19622: 02/12/2011 13:35        yes        864    143.00     -36.00
##        yaw_belt total_accel_belt kurtosis_roll_belt kurtosis_picth_belt
##     1:    -94.4                3                                       
##     2:    -94.4                3                                       
##     3:    -94.4                3                                       
##     4:    -94.4                3                                       
##     5:    -94.4                3                                       
##    ---                                                                 
## 19618:    129.0               21                                       
## 19619:    130.0               19                                       
## 19620:    130.0               19                                       
## 19621:    131.0               18                                       
## 19622:    132.0               18          -1.175902           -1.063259
##        kurtosis_yaw_belt skewness_roll_belt skewness_roll_belt.1
##     1:                                                          
##     2:                                                          
##     3:                                                          
##     4:                                                          
##     5:                                                          
##    ---                                                          
## 19618:                                                          
## 19619:                                                          
## 19620:                                                          
## 19621:                                                          
## 19622:           #DIV/0!           0.196860            -0.572396
##        skewness_yaw_belt max_roll_belt max_picth_belt max_yaw_belt
##     1:                              NA             NA             
##     2:                              NA             NA             
##     3:                              NA             NA             
##     4:                              NA             NA             
##     5:                              NA             NA             
##    ---                                                            
## 19618:                              NA             NA             
## 19619:                              NA             NA             
## 19620:                              NA             NA             
## 19621:                              NA             NA             
## 19622:           #DIV/0!           132             25         -1.2
##        min_roll_belt min_pitch_belt min_yaw_belt amplitude_roll_belt
##     1:            NA             NA                               NA
##     2:            NA             NA                               NA
##     3:            NA             NA                               NA
##     4:            NA             NA                               NA
##     5:            NA             NA                               NA
##    ---                                                              
## 19618:            NA             NA                               NA
## 19619:            NA             NA                               NA
## 19620:            NA             NA                               NA
## 19621:            NA             NA                               NA
## 19622:           123             18         -1.2                   9
##        amplitude_pitch_belt amplitude_yaw_belt var_total_accel_belt
##     1:                   NA                                      NA
##     2:                   NA                                      NA
##     3:                   NA                                      NA
##     4:                   NA                                      NA
##     5:                   NA                                      NA
##    ---                                                             
## 19618:                   NA                                      NA
## 19619:                   NA                                      NA
## 19620:                   NA                                      NA
## 19621:                   NA                                      NA
## 19622:                    7               0.00                5.627
##        avg_roll_belt stddev_roll_belt var_roll_belt avg_pitch_belt
##     1:            NA               NA            NA             NA
##     2:            NA               NA            NA             NA
##     3:            NA               NA            NA             NA
##     4:            NA               NA            NA             NA
##     5:            NA               NA            NA             NA
##    ---                                                            
## 19618:            NA               NA            NA             NA
## 19619:            NA               NA            NA             NA
## 19620:            NA               NA            NA             NA
## 19621:            NA               NA            NA             NA
## 19622:         151.1            4.753         22.59         -33.63
##        stddev_pitch_belt var_pitch_belt avg_yaw_belt stddev_yaw_belt
##     1:                NA             NA           NA              NA
##     2:                NA             NA           NA              NA
##     3:                NA             NA           NA              NA
##     4:                NA             NA           NA              NA
##     5:                NA             NA           NA              NA
##    ---                                                              
## 19618:                NA             NA           NA              NA
## 19619:                NA             NA           NA              NA
## 19620:                NA             NA           NA              NA
## 19621:                NA             NA           NA              NA
## 19622:             1.395          1.947        126.9            2.75
##        var_yaw_belt gyros_belt_x gyros_belt_y gyros_belt_z accel_belt_x
##     1:           NA         0.00         0.00        -0.02          -21
##     2:           NA         0.02         0.00        -0.02          -22
##     3:           NA         0.00         0.00        -0.02          -20
##     4:           NA         0.02         0.00        -0.03          -22
##     5:           NA         0.02         0.02        -0.02          -21
##    ---                                                                 
## 19618:           NA         0.37        -0.02        -0.67           50
## 19619:           NA         0.39        -0.02        -0.67           47
## 19620:           NA         0.37         0.00        -0.64           47
## 19621:           NA         0.37        -0.02        -0.59           46
## 19622:        7.564         0.35        -0.02        -0.57           42
##        accel_belt_y accel_belt_z magnet_belt_x magnet_belt_y magnet_belt_z
##     1:            4           22            -3           599          -313
##     2:            4           22            -7           608          -311
##     3:            5           23            -2           600          -305
##     4:            3           21            -6           604          -310
##     5:            2           24            -6           600          -302
##    ---                                                                    
## 19618:           26         -193           190           552          -412
## 19619:           15         -179           192           558          -389
## 19620:           13         -177           191           560          -386
## 19621:           18         -172           190           565          -370
## 19622:           25         -171           194           566          -349
##        roll_arm pitch_arm yaw_arm total_accel_arm var_accel_arm
##     1:   -128.0      22.5  -161.0              34            NA
##     2:   -128.0      22.5  -161.0              34            NA
##     3:   -128.0      22.5  -161.0              34            NA
##     4:   -128.0      22.1  -161.0              34            NA
##     5:   -128.0      22.1  -161.0              34            NA
##    ---                                                         
## 19618:    -99.4     -33.8    79.0              47            NA
## 19619:    -99.6     -34.5    77.3              45            NA
## 19620:    -99.6     -35.1    76.3              44            NA
## 19621:    -98.6     -36.7    73.5              41            NA
## 19622:    -97.6     -37.7    71.5              41         54.26
##        avg_roll_arm stddev_roll_arm var_roll_arm avg_pitch_arm
##     1:           NA              NA           NA            NA
##     2:           NA              NA           NA            NA
##     3:           NA              NA           NA            NA
##     4:           NA              NA           NA            NA
##     5:           NA              NA           NA            NA
##    ---                                                        
## 19618:           NA              NA           NA            NA
## 19619:           NA              NA           NA            NA
## 19620:           NA              NA           NA            NA
## 19621:           NA              NA           NA            NA
## 19622:       -91.65           9.169        84.06        -37.65
##        stddev_pitch_arm var_pitch_arm avg_yaw_arm stddev_yaw_arm
##     1:               NA            NA          NA             NA
##     2:               NA            NA          NA             NA
##     3:               NA            NA          NA             NA
##     4:               NA            NA          NA             NA
##     5:               NA            NA          NA             NA
##    ---                                                          
## 19618:               NA            NA          NA             NA
## 19619:               NA            NA          NA             NA
## 19620:               NA            NA          NA             NA
## 19621:               NA            NA          NA             NA
## 19622:            3.616         13.08       66.31          15.48
##        var_yaw_arm gyros_arm_x gyros_arm_y gyros_arm_z accel_arm_x
##     1:          NA        0.00        0.00       -0.02        -288
##     2:          NA        0.02       -0.02       -0.02        -290
##     3:          NA        0.02       -0.02       -0.02        -289
##     4:          NA        0.02       -0.03        0.02        -289
##     5:          NA        0.00       -0.03        0.00        -289
##    ---                                                            
## 19618:          NA        0.55       -0.51        0.25          75
## 19619:          NA        0.88       -0.71        0.21          52
## 19620:          NA        0.98       -0.82        0.23          62
## 19621:          NA        1.35       -1.00        0.49          70
## 19622:       239.6        1.51       -1.06        0.59          58
##        accel_arm_y accel_arm_z magnet_arm_x magnet_arm_y magnet_arm_z
##     1:         109        -123         -368          337          516
##     2:         110        -125         -369          337          513
##     3:         110        -126         -368          344          513
##     4:         111        -123         -372          344          512
##     5:         111        -123         -374          337          506
##    ---                                                               
## 19618:        -184        -415          272         -134         -562
## 19619:        -163        -406          288         -112         -559
## 19620:        -167        -391          309         -103         -541
## 19621:        -164        -359          339          -91         -543
## 19622:        -152        -365          362          -84         -539
##        kurtosis_roll_arm kurtosis_picth_arm kurtosis_yaw_arm
##     1:                                                      
##     2:                                                      
##     3:                                                      
##     4:                                                      
##     5:                                                      
##    ---                                                      
## 19618:                                                      
## 19619:                                                      
## 19620:                                                      
## 19621:                                                      
## 19622:          -1.32631            0.50959         -0.62736
##        skewness_roll_arm skewness_pitch_arm skewness_yaw_arm max_roll_arm
##     1:                                                                 NA
##     2:                                                                 NA
##     3:                                                                 NA
##     4:                                                                 NA
##     5:                                                                 NA
##    ---                                                                   
## 19618:                                                                 NA
## 19619:                                                                 NA
## 19620:                                                                 NA
## 19621:                                                                 NA
## 19622:          -0.51721           -1.26872         -0.77150        -33.7
##        max_picth_arm max_yaw_arm min_roll_arm min_pitch_arm min_yaw_arm
##     1:            NA          NA           NA            NA          NA
##     2:            NA          NA           NA            NA          NA
##     3:            NA          NA           NA            NA          NA
##     4:            NA          NA           NA            NA          NA
##     5:            NA          NA           NA            NA          NA
##    ---                                                                 
## 19618:            NA          NA           NA            NA          NA
## 19619:            NA          NA           NA            NA          NA
## 19620:            NA          NA           NA            NA          NA
## 19621:            NA          NA           NA            NA          NA
## 19622:          79.5          49        -43.5          27.5          25
##        amplitude_roll_arm amplitude_pitch_arm amplitude_yaw_arm
##     1:                 NA                  NA                NA
##     2:                 NA                  NA                NA
##     3:                 NA                  NA                NA
##     4:                 NA                  NA                NA
##     5:                 NA                  NA                NA
##    ---                                                         
## 19618:                 NA                  NA                NA
## 19619:                 NA                  NA                NA
## 19620:                 NA                  NA                NA
## 19621:                 NA                  NA                NA
## 19622:                9.8                  52                24
##        roll_dumbbell pitch_dumbbell yaw_dumbbell kurtosis_roll_dumbbell
##     1:         13.05         -70.49       -84.87                       
##     2:         13.13         -70.64       -84.71                       
##     3:         12.85         -70.28       -85.14                       
##     4:         13.43         -70.39       -84.87                       
##     5:         13.38         -70.43       -84.85                       
##    ---                                                                 
## 19618:         36.41         -22.86      -113.50                       
## 19619:         35.15         -22.97      -114.53                       
## 19620:         30.06         -20.99      -120.03                       
## 19621:         22.86         -21.76      -125.25                       
## 19622:         20.80         -19.70      -128.20                -1.1322
##        kurtosis_picth_dumbbell kurtosis_yaw_dumbbell
##     1:                                              
##     2:                                              
##     3:                                              
##     4:                                              
##     5:                                              
##    ---                                              
## 19618:                                              
## 19619:                                              
## 19620:                                              
## 19621:                                              
## 19622:                 -0.7225               #DIV/0!
##        skewness_roll_dumbbell skewness_pitch_dumbbell
##     1:                                               
##     2:                                               
##     3:                                               
##     4:                                               
##     5:                                               
##    ---                                               
## 19618:                                               
## 19619:                                               
## 19620:                                               
## 19621:                                               
## 19622:                 0.0955                  0.1057
##        skewness_yaw_dumbbell max_roll_dumbbell max_picth_dumbbell
##     1:                                      NA                 NA
##     2:                                      NA                 NA
##     3:                                      NA                 NA
##     4:                                      NA                 NA
##     5:                                      NA                 NA
##    ---                                                           
## 19618:                                      NA                 NA
## 19619:                                      NA                 NA
## 19620:                                      NA                 NA
## 19621:                                      NA                 NA
## 19622:               #DIV/0!             -19.7                -92
##        max_yaw_dumbbell min_roll_dumbbell min_pitch_dumbbell
##     1:                                 NA                 NA
##     2:                                 NA                 NA
##     3:                                 NA                 NA
##     4:                                 NA                 NA
##     5:                                 NA                 NA
##    ---                                                      
## 19618:                                 NA                 NA
## 19619:                                 NA                 NA
## 19620:                                 NA                 NA
## 19621:                                 NA                 NA
## 19622:             -1.1             -33.1             -128.2
##        min_yaw_dumbbell amplitude_roll_dumbbell amplitude_pitch_dumbbell
##     1:                                       NA                       NA
##     2:                                       NA                       NA
##     3:                                       NA                       NA
##     4:                                       NA                       NA
##     5:                                       NA                       NA
##    ---                                                                  
## 19618:                                       NA                       NA
## 19619:                                       NA                       NA
## 19620:                                       NA                       NA
## 19621:                                       NA                       NA
## 19622:             -1.1                   13.41                     36.2
##        amplitude_yaw_dumbbell total_accel_dumbbell var_accel_dumbbell
##     1:                                          37                 NA
##     2:                                          37                 NA
##     3:                                          37                 NA
##     4:                                          37                 NA
##     5:                                          37                 NA
##    ---                                                               
## 19618:                                          19                 NA
## 19619:                                          18                 NA
## 19620:                                          19                 NA
## 19621:                                          19                 NA
## 19622:                   0.00                   19             0.4217
##        avg_roll_dumbbell stddev_roll_dumbbell var_roll_dumbbell
##     1:                NA                   NA                NA
##     2:                NA                   NA                NA
##     3:                NA                   NA                NA
##     4:                NA                   NA                NA
##     5:                NA                   NA                NA
##    ---                                                         
## 19618:                NA                   NA                NA
## 19619:                NA                   NA                NA
## 19620:                NA                   NA                NA
## 19621:                NA                   NA                NA
## 19622:             37.34                9.783              95.7
##        avg_pitch_dumbbell stddev_pitch_dumbbell var_pitch_dumbbell
##     1:                 NA                    NA                 NA
##     2:                 NA                    NA                 NA
##     3:                 NA                    NA                 NA
##     4:                 NA                    NA                 NA
##     5:                 NA                    NA                 NA
##    ---                                                            
## 19618:                 NA                    NA                 NA
## 19619:                 NA                    NA                 NA
## 19620:                 NA                    NA                 NA
## 19621:                 NA                    NA                 NA
## 19622:             -26.82                  4.01              16.08
##        avg_yaw_dumbbell stddev_yaw_dumbbell var_yaw_dumbbell
##     1:               NA                  NA               NA
##     2:               NA                  NA               NA
##     3:               NA                  NA               NA
##     4:               NA                  NA               NA
##     5:               NA                  NA               NA
##    ---                                                      
## 19618:               NA                  NA               NA
## 19619:               NA                  NA               NA
## 19620:               NA                  NA               NA
## 19621:               NA                  NA               NA
## 19622:             -110               9.748            95.01
##        gyros_dumbbell_x gyros_dumbbell_y gyros_dumbbell_z accel_dumbbell_x
##     1:             0.00            -0.02             0.00             -234
##     2:             0.00            -0.02             0.00             -233
##     3:             0.00            -0.02             0.00             -232
##     4:             0.00            -0.02            -0.02             -232
##     5:             0.00            -0.02             0.00             -233
##    ---                                                                    
## 19618:             0.32            -0.26            -0.36              -42
## 19619:             0.24            -0.24             0.05              -41
## 19620:             0.22            -0.27             0.21              -38
## 19621:             0.13            -0.14             0.34              -40
## 19622:             0.02             0.02             0.36              -36
##        accel_dumbbell_y accel_dumbbell_z magnet_dumbbell_x
##     1:               47             -271              -559
##     2:               47             -269              -555
##     3:               46             -270              -561
##     4:               48             -269              -552
##     5:               48             -270              -554
##    ---                                                    
## 19618:               66             -168              -618
## 19619:               62             -164              -618
## 19620:               54             -170              -621
## 19621:               42             -176              -628
## 19622:               38             -176              -627
##        magnet_dumbbell_y magnet_dumbbell_z roll_forearm pitch_forearm
##     1:               293               -65         28.4         -63.9
##     2:               296               -64         28.3         -63.9
##     3:               298               -63         28.3         -63.9
##     4:               303               -60         28.1         -63.9
##     5:               292               -68         28.0         -63.9
##    ---                                                               
## 19618:               134                 0          0.0           0.0
## 19619:               116                 7          0.0           0.0
## 19620:               113                -9          0.0           0.0
## 19621:               116                 0          0.0           0.0
## 19622:               119                 2          0.0           0.0
##        yaw_forearm kurtosis_roll_forearm kurtosis_picth_forearm
##     1:        -153                                             
##     2:        -153                                             
##     3:        -152                                             
##     4:        -152                                             
##     5:        -152                                             
##    ---                                                         
## 19618:           0                                             
## 19619:           0                                             
## 19620:           0                                             
## 19621:           0                                             
## 19622:           0               #DIV/0!                #DIV/0!
##        kurtosis_yaw_forearm skewness_roll_forearm skewness_pitch_forearm
##     1:                                                                  
##     2:                                                                  
##     3:                                                                  
##     4:                                                                  
##     5:                                                                  
##    ---                                                                  
## 19618:                                                                  
## 19619:                                                                  
## 19620:                                                                  
## 19621:                                                                  
## 19622:              #DIV/0!               #DIV/0!                #DIV/0!
##        skewness_yaw_forearm max_roll_forearm max_picth_forearm
##     1:                                    NA                NA
##     2:                                    NA                NA
##     3:                                    NA                NA
##     4:                                    NA                NA
##     5:                                    NA                NA
##    ---                                                        
## 19618:                                    NA                NA
## 19619:                                    NA                NA
## 19620:                                    NA                NA
## 19621:                                    NA                NA
## 19622:              #DIV/0!                0                 0
##        max_yaw_forearm min_roll_forearm min_pitch_forearm min_yaw_forearm
##     1:                               NA                NA                
##     2:                               NA                NA                
##     3:                               NA                NA                
##     4:                               NA                NA                
##     5:                               NA                NA                
##    ---                                                                   
## 19618:                               NA                NA                
## 19619:                               NA                NA                
## 19620:                               NA                NA                
## 19621:                               NA                NA                
## 19622:         #DIV/0!                0                 0         #DIV/0!
##        amplitude_roll_forearm amplitude_pitch_forearm
##     1:                     NA                      NA
##     2:                     NA                      NA
##     3:                     NA                      NA
##     4:                     NA                      NA
##     5:                     NA                      NA
##    ---                                               
## 19618:                     NA                      NA
## 19619:                     NA                      NA
## 19620:                     NA                      NA
## 19621:                     NA                      NA
## 19622:                      0                       0
##        amplitude_yaw_forearm total_accel_forearm var_accel_forearm
##     1:                                        36                NA
##     2:                                        36                NA
##     3:                                        36                NA
##     4:                                        36                NA
##     5:                                        36                NA
##    ---                                                            
## 19618:                                        29                NA
## 19619:                                        29                NA
## 19620:                                        29                NA
## 19621:                                        32                NA
## 19622:               #DIV/0!                  33             30.11
##        avg_roll_forearm stddev_roll_forearm var_roll_forearm
##     1:               NA                  NA               NA
##     2:               NA                  NA               NA
##     3:               NA                  NA               NA
##     4:               NA                  NA               NA
##     5:               NA                  NA               NA
##    ---                                                      
## 19618:               NA                  NA               NA
## 19619:               NA                  NA               NA
## 19620:               NA                  NA               NA
## 19621:               NA                  NA               NA
## 19622:                0                   0                0
##        avg_pitch_forearm stddev_pitch_forearm var_pitch_forearm
##     1:                NA                   NA                NA
##     2:                NA                   NA                NA
##     3:                NA                   NA                NA
##     4:                NA                   NA                NA
##     5:                NA                   NA                NA
##    ---                                                         
## 19618:                NA                   NA                NA
## 19619:                NA                   NA                NA
## 19620:                NA                   NA                NA
## 19621:                NA                   NA                NA
## 19622:                 0                    0                 0
##        avg_yaw_forearm stddev_yaw_forearm var_yaw_forearm gyros_forearm_x
##     1:              NA                 NA              NA            0.03
##     2:              NA                 NA              NA            0.02
##     3:              NA                 NA              NA            0.03
##     4:              NA                 NA              NA            0.02
##     5:              NA                 NA              NA            0.02
##    ---                                                                   
## 19618:              NA                 NA              NA            1.73
## 19619:              NA                 NA              NA            1.59
## 19620:              NA                 NA              NA            1.54
## 19621:              NA                 NA              NA            1.48
## 19622:               0                  0               0            1.38
##        gyros_forearm_y gyros_forearm_z accel_forearm_x accel_forearm_y
##     1:            0.00           -0.02             192             203
##     2:            0.00           -0.02             192             203
##     3:           -0.02            0.00             196             204
##     4:           -0.02            0.00             189             206
##     5:            0.00           -0.02             189             206
##    ---                                                                
## 19618:           -1.75           -0.25            -271             -68
## 19619:           -1.36            0.00            -271             -91
## 19620:           -1.20            0.05            -263             -99
## 19621:           -0.90            0.05            -270            -141
## 19622:           -0.64            0.08            -278            -159
##        accel_forearm_z magnet_forearm_x magnet_forearm_y magnet_forearm_z
##     1:            -215              -17              654              476
##     2:            -216              -18              661              473
##     3:            -213              -18              658              469
##     4:            -214              -16              658              469
##     5:            -214              -17              655              473
##    ---                                                                   
## 19618:             -37             -205             -587                6
## 19619:             -43             -151             -635              -36
## 19620:             -45             -116             -654              -70
## 19621:             -51              -68             -678              -98
## 19622:             -52              -60             -686             -110
##        classe
##     1:      A
##     2:      A
##     3:      A
##     4:      A
##     5:      A
##    ---       
## 19618:      E
## 19619:      E
## 19620:      E
## 19621:      E
## 19622:      E
```

Do some minor data tidying.

* Drop the `V1` variable (it's just a row number)


```r
DTrain <- DTrain[, V1 := NULL]
```
