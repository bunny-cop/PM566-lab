Lab 05
================
Xiaofan Zhu
2022-09-23

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ lubridate::as.difftime() masks base::as.difftime()
    ## ✖ lubridate::date()        masks base::date()
    ## ✖ dplyr::filter()          masks stats::filter()
    ## ✖ lubridate::intersect()   masks base::intersect()
    ## ✖ dplyr::lag()             masks stats::lag()
    ## ✖ lubridate::setdiff()     masks base::setdiff()
    ## ✖ lubridate::union()       masks base::union()

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year

``` r
library(dtplyr)
```

## Step 1. Read in the data

``` r
if(!file.exists("../lab03/met_all.gz")){
 download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz", "met_all.gz", method="libcurl", timeout = 60)
}

met <- data.table::fread("../lab03/met_all.gz")
```

Read in the stations data

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

Merge met data as we did during the lecture

``` r
met <-  
  merge(
  # Data
  x     = met,      
  y     = stations, 
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
  ) 

nrow(met)
```

    ## [1] 2377343

## Q1

``` r
station_averages <- 
     met[ , .(
       temp      = mean(temp, na.rm=T),
       wind.sp   = mean(wind.sp, na.rm=T),
       atm.press = mean(atm.press,na.rm=T)
     ), by = USAFID]
```

``` r
stmeds <- station_averages[ , .(
          temp50     = median(temp, na.rm=T),
          windsp50   = median(wind.sp,na.rm=T),
          atmpress50 = median(atm.press,na.rm=T)
)]
stmeds
```

    ##      temp50 windsp50 atmpress50
    ## 1: 23.68406 2.461838   1014.691

``` r
station_averages[ , 
              temp_dist50 := abs(temp - stmeds$temp50)][order(temp_dist50)]
```

    ##       USAFID     temp  wind.sp atm.press temp_dist50
    ##    1: 720458 23.68173 1.209682       NaN 0.002328907
    ##    2: 725515 23.68639 2.709164       NaN 0.002328907
    ##    3: 725835 23.67835 2.652381       NaN 0.005712423
    ##    4: 724509 23.67510 4.066833  1013.863 0.008959632
    ##    5: 720538 23.66593 1.907897       NaN 0.018127186
    ##   ---                                               
    ## 1591: 723400      NaN      NaN       NaN         NaN
    ## 1592: 723825      NaN 3.482819       NaN         NaN
    ## 1593: 723895      NaN      NaN       NaN         NaN
    ## 1594: 724347      NaN      NaN       NaN         NaN
    ## 1595: 725786      NaN      NaN       NaN         NaN

``` r
station_averages[ which.min(temp_dist50)]
```

    ##    USAFID     temp  wind.sp atm.press temp_dist50
    ## 1: 720458 23.68173 1.209682       NaN 0.002328907

## Q2

``` r
station_averages <- 
     met[ , .(
       temp      = mean(temp, na.rm=T),
       wind.sp   = mean(wind.sp, na.rm=T),
       atm.press = mean(atm.press,na.rm=T)
     ), by = .(USAFID,STATE)]
head(station_averages)
```

    ##    USAFID STATE     temp  wind.sp atm.press
    ## 1: 690150    CA 33.18763 3.483560  1010.379
    ## 2: 720110    TX 31.22003 2.138348       NaN
    ## 3: 720113    MI 23.29317 2.470298       NaN
    ## 4: 720120    SC 27.01922 2.504692       NaN
    ## 5: 720137    IL 21.88823 1.979335       NaN
    ## 6: 720151    TX 27.57686 2.998428       NaN

``` r
statemeds<- station_averages[ , .(
           temp50    = median(temp, na.rm=T), 
           wind.sp50 = median(wind.sp, na.rm=T)
            ), by = STATE]
statemeds
```

    ##     STATE   temp50 wind.sp50
    ##  1:    CA 22.66268  2.565445
    ##  2:    TX 29.75188  3.413737
    ##  3:    MI 20.51970  2.273423
    ##  4:    SC 25.80545  1.696119
    ##  5:    IL 22.43194  2.237622
    ##  6:    MO 23.95109  2.453547
    ##  7:    AR 26.24296  1.938625
    ##  8:    OR 17.98061  2.011436
    ##  9:    WA 19.24684  1.268571
    ## 10:    GA 26.70404  1.495596
    ## 11:    MN 19.63017  2.617071
    ## 12:    AL 26.33664  1.662132
    ## 13:    IN 22.25059  2.344333
    ## 14:    NC 24.72953  1.627306
    ## 15:    VA 24.37799  1.653032
    ## 16:    IA 21.33461  2.680875
    ## 17:    PA 21.69177  1.784167
    ## 18:    NE 21.87354  3.192539
    ## 19:    ID 20.56798  2.568944
    ## 20:    WI 18.85524  2.053283
    ## 21:    WV 21.94446  1.633487
    ## 22:    MD 24.89883  1.883499
    ## 23:    AZ 30.32372  3.074359
    ## 24:    OK 27.14427  3.852697
    ## 25:    WY 19.80699  3.873392
    ## 26:    LA 27.87430  1.592840
    ## 27:    KY 23.88844  1.895486
    ## 28:    FL 27.57325  2.705069
    ## 29:    CO 21.49638  3.098777
    ## 30:    OH 22.02062  2.554397
    ## 31:    NJ 23.47238  2.148606
    ## 32:    NM 24.94447  3.776083
    ## 33:    KS 24.21220  3.680613
    ## 34:    ND 18.52849  3.956459
    ## 35:    VT 18.61379  1.408247
    ## 36:    MS 26.69258  1.636392
    ## 37:    CT 22.36880  2.101801
    ## 38:    NV 24.56293  3.035050
    ## 39:    UT 24.35182  3.145427
    ## 40:    SD 20.35662  3.665638
    ## 41:    TN 24.88657  1.576035
    ## 42:    NY 20.40674  2.304075
    ## 43:    RI 22.53551  2.583469
    ## 44:    MA 21.30662  2.710944
    ## 45:    DE 24.56026  2.752929
    ## 46:    NH 19.55054  1.563826
    ## 47:    ME 18.79016  2.237210
    ## 48:    MT 19.15492  4.151737
    ##     STATE   temp50 wind.sp50

``` r
station_averages <- 
  merge(
  x = station_averages,
  y = statemeds,
  by.x = "STATE",
  by.y = "STATE",
  all.x = TRUE,
  all.y = FALSE
)
```

``` r
station_averages[ , temp_dist_state50   := temp - temp50]
station_averages[ , windsp_dist_state50 := wind.sp - wind.sp50] 
station_averages
```

    ##       STATE USAFID     temp  wind.sp atm.press   temp50 wind.sp50
    ##    1:    AL 720265 26.22064 1.136691       NaN 26.33664  1.662132
    ##    2:    AL 720307 25.14605 1.624349       NaN 26.33664  1.662132
    ##    3:    AL 720361 26.62228 1.343410  1015.275 26.33664  1.662132
    ##    4:    AL 720362 27.26504 1.746168  1014.559 26.33664  1.662132
    ##    5:    AL 720376 24.97884 1.296044       NaN 26.33664  1.662132
    ##   ---                                                            
    ## 1591:    WY 726667 23.10219 3.290873  1012.276 19.80699  3.873392
    ## 1592:    WY 726690 20.51681 4.242981  1013.000 19.80699  3.873392
    ## 1593:    WY 726700 19.97665 3.066306  1015.219 19.80699  3.873392
    ## 1594:    WY 726710 16.86569 3.500389  1014.944 19.80699  3.873392
    ## 1595:    WY 726720 21.70287 3.800334  1012.771 19.80699  3.873392
    ##       temp_dist_state50 windsp_dist_state50
    ##    1:        -0.1159996         -0.52544171
    ##    2:        -1.1905914         -0.03778375
    ##    3:         0.2856450         -0.31872221
    ##    4:         0.9284033          0.08403570
    ##    5:        -1.3577997         -0.36608819
    ##   ---                                      
    ## 1591:         3.2951940         -0.58251934
    ## 1592:         0.7098198          0.36958901
    ## 1593:         0.1696556         -0.80708670
    ## 1594:        -2.9412986         -0.37300376
    ## 1595:         1.8958786         -0.07305791

``` r
station_averages[ , eucdist := temp_dist_state50^2 +
                                windsp_dist_state50^2]
```

``` r
repstation <- station_averages[ , .(
                    eucdist = min(eucdist, na.rm=T))
                  , by=STATE]
```

``` r
repweather <- 
  merge(
  x = station_averages,
  y = repstation,
  by.x = c("eucdist","STATE"),
  by.y = c("eucdist","STATE"),
  all.x = FALSE,
  all.y = TRUE
)
repweather
```

    ##          eucdist STATE USAFID     temp  wind.sp atm.press   temp50 wind.sp50
    ##  1: 0.0000000000    DE 724180 24.56026 2.752929  1015.046 24.56026  2.752929
    ##  2: 0.0000000000    MD 722218 24.89883 1.883499       NaN 24.89883  1.883499
    ##  3: 0.0000000000    NJ 724090 23.47238 2.148606  1015.095 23.47238  2.148606
    ##  4: 0.0000000000    WA 720254 19.24684 1.268571       NaN 19.24684  1.268571
    ##  5: 0.0002593427    WV 720328 21.94820 1.617823       NaN 21.94446  1.633487
    ##  6: 0.0002593427    WV 724176 21.94072 1.649151  1015.982 21.94446  1.633487
    ##  7: 0.0006410156    AL 722286 26.35793 1.675828  1014.909 26.33664  1.662132
    ##  8: 0.0009745369    FL 722011 27.56952 2.674074  1016.063 27.57325  2.705069
    ##  9: 0.0020160519    IA 725464 21.37948 2.679227       NaN 21.33461  2.680875
    ## 10: 0.0023558504    GA 722197 26.70404 1.544133  1015.574 26.70404  1.495596
    ## 11: 0.0037719616    VA 724006 24.31662 1.650539       NaN 24.37799  1.653032
    ## 12: 0.0069584784    OK 720625 27.06188 3.865717       NaN 27.14427  3.852697
    ## 13: 0.0077716366    IL 722076 22.34403 2.244115       NaN 22.43194  2.237622
    ## 14: 0.0081925142    WI 726413 18.94233 2.028610       NaN 18.85524  2.053283
    ## 15: 0.0090257850    NE 725565 21.86100 3.098367  1015.068 21.87354  3.192539
    ## 16: 0.0091213936    NC 720864 24.82394 1.612864       NaN 24.72953  1.627306
    ## 17: 0.0093580816    NY 724988 20.44142 2.394383  1016.233 20.40674  2.304075
    ## 18: 0.0122411211    WY 726654 19.85844 3.775443  1014.107 19.80699  3.873392
    ## 19: 0.0132319539    MI 725395 20.44096 2.357275  1015.245 20.51970  2.273423
    ## 20: 0.0142107341    LA 722041 27.84758 1.476664       NaN 27.87430  1.592840
    ## 21: 0.0144022476    MA 725088 21.20391 2.773018  1013.718 21.30662  2.710944
    ## 22: 0.0153227818    TX 722598 29.81293 3.521417       NaN 29.75188  3.413737
    ## 23: 0.0159151165    IN 724386 22.32575 2.243013  1014.797 22.25059  2.344333
    ## 24: 0.0243927262    UT 725750 24.23571 3.040962  1011.521 24.35182  3.145427
    ## 25: 0.0327455867    SC 723107 25.95831 1.599275       NaN 25.80545  1.696119
    ## 26: 0.0339881655    PA 725204 21.87141 1.825605       NaN 21.69177  1.784167
    ## 27: 0.0348669121    ND 720911 18.34248 3.940128       NaN 18.52849  3.956459
    ## 28: 0.0353278560    MS 722358 26.54093 1.747426  1014.722 26.69258  1.636392
    ## 29: 0.0375202070    TN 720974 24.71645 1.483411       NaN 24.88657  1.576035
    ## 30: 0.0416560272    MO 720479 24.14775 2.508153       NaN 23.95109  2.453547
    ## 31: 0.0427670273    NM 723658 24.94447 3.569281  1013.917 24.94447  3.776083
    ## 32: 0.0432891328    CT 725087 22.57539 2.126514  1014.534 22.36880  2.101801
    ## 33: 0.0520031063    MN 726553 19.67552 2.393582       NaN 19.63017  2.617071
    ## 34: 0.0544857836    AZ 722745 30.31538 3.307632  1010.144 30.32372  3.074359
    ## 35: 0.0574383290    KS 724550 24.14958 3.449278  1013.315 24.21220  3.680613
    ## 36: 0.0644182645    OH 724295 21.97211 2.803524  1015.742 22.02062  2.554397
    ## 37: 0.0668411712    RI 725079 22.27697 2.583469  1014.620 22.53551  2.583469
    ## 38: 0.0668933328    CA 722970 22.76040 2.325982  1012.710 22.66268  2.565445
    ## 39: 0.0777385965    ID 725867 20.81272 2.702517  1012.802 20.56798  2.568944
    ## 40: 0.0942851270    VT 726115 18.60548 1.101301  1014.985 18.61379  1.408247
    ## 41: 0.0943820541    ME 726077 18.49969 2.337241  1014.475 18.79016  2.237210
    ## 42: 0.1065472295    NH 726116 19.23920 1.465766  1013.840 19.55054  1.563826
    ## 43: 0.1710841308    SD 726590 19.95928 3.550722  1014.284 20.35662  3.665638
    ## 44: 0.1736376706    AR 722054 26.58944 1.707136  1014.127 26.24296  1.938625
    ## 45: 0.1858292309    MT 726798 19.47014 4.445783  1014.072 19.15492  4.151737
    ## 46: 0.2129599434    KY 720448 23.52994 1.604905       NaN 23.88844  1.895486
    ## 47: 0.2380402433    NV 724885 24.78430 2.600266  1013.855 24.56293  3.035050
    ## 48: 0.2636953688    CO 724699 21.94228 2.844072       NaN 21.49638  3.098777
    ## 49: 0.7014979292    OR 720202 17.16329 1.828437       NaN 17.98061  2.011436
    ##          eucdist STATE USAFID     temp  wind.sp atm.press   temp50 wind.sp50
    ##     temp_dist_state50 windsp_dist_state50
    ##  1:       0.000000000         0.000000000
    ##  2:       0.000000000         0.000000000
    ##  3:       0.000000000         0.000000000
    ##  4:       0.000000000         0.000000000
    ##  5:       0.003739818        -0.015663860
    ##  6:      -0.003739818         0.015663860
    ##  7:       0.021294174         0.013695758
    ##  8:      -0.003722957        -0.030994782
    ##  9:       0.044870212        -0.001648046
    ## 10:       0.000000000         0.048537104
    ## 11:      -0.061365678        -0.002493015
    ## 12:      -0.082395169         0.013019775
    ## 13:      -0.087917451         0.006492950
    ## 14:       0.087084812        -0.024672854
    ## 15:      -0.012543905        -0.094172371
    ## 16:       0.094407682        -0.014442410
    ## 17:       0.034676267         0.090308571
    ## 18:       0.051448939        -0.097949619
    ## 19:      -0.078745377         0.083851771
    ## 20:      -0.026716512        -0.116176426
    ## 21:      -0.102708664         0.062073971
    ## 22:       0.061055783         0.107679957
    ## 23:       0.075162869        -0.101319591
    ## 24:      -0.116102741        -0.104464729
    ## 25:       0.152861989        -0.096844200
    ## 26:       0.179641362         0.041438466
    ## 27:      -0.186011382        -0.016330272
    ## 28:      -0.151654911         0.111034426
    ## 29:      -0.170120726        -0.092623677
    ## 30:       0.196657661         0.054605785
    ## 31:       0.000000000        -0.206801904
    ## 32:       0.206587513         0.024712997
    ## 33:       0.045341660        -0.223488792
    ## 34:      -0.008341255         0.233272817
    ## 35:      -0.062628803        -0.231335172
    ## 36:      -0.048515527         0.249127494
    ## 37:      -0.258536596         0.000000000
    ## 38:       0.097726713        -0.239463614
    ## 39:       0.244738817         0.133572108
    ## 40:      -0.008312123        -0.306946307
    ## 41:      -0.290475361         0.100030588
    ## 42:      -0.311337973        -0.098060676
    ## 43:      -0.397339331        -0.114915562
    ## 44:       0.346482848        -0.231489323
    ## 45:       0.315223976         0.294046043
    ## 46:      -0.358501256        -0.290580097
    ## 47:       0.221366082        -0.434784201
    ## 48:       0.445893266        -0.254704857
    ## 49:      -0.817318253        -0.182999458
    ##     temp_dist_state50 windsp_dist_state50

## Q3

``` r
met_stations <- unique(met[, .(USAFID, STATE, lon, lat)])
nrow(met_stations)
```

    ## [1] 2884

``` r
length(unique(met$USAFID))
```

    ## [1] 1595

``` r
met_stations <- unique(met[order(USAFID,day,hour)][, .(USAFID, STATE, lon, lat)])
met_stations[, n := 1:.N, by=USAFID]
met_stations <- met_stations[n == 1][, n:= NULL]
nrow(met_stations)
```

    ## [1] 1595

``` r
met_stations[, lat_mid := quantile(lat, probs = .5, na.rm = TRUE), by=STATE]
met_stations[, lon_mid := quantile(lon, probs = .5, na.rm = TRUE), by=STATE]

met_stations[, dist := sqrt((lat-lat_mid)^2+(lon-lon_mid)^2)]
met_stations[, mindist := which.min(dist), by=STATE]
```

``` r
met_stations[, n := 1:.N, by=STATE]
met_stations[order(STATE,dist)]
```

    ##       USAFID STATE      lon    lat lat_mid   lon_mid      dist mindist  n
    ##    1: 722265    AL  -86.350 32.383 32.7655  -86.5840 0.4483997      17 17
    ##    2: 722300    AL  -86.782 33.178 32.7655  -86.5840 0.4575590      17 29
    ##    3: 722260    AL  -86.400 32.300 32.7655  -86.5840 0.5005460      17 16
    ##    4: 720265    AL  -85.963 32.915 32.7655  -86.5840 0.6387419      17  1
    ##    5: 722280    AL  -86.745 33.566 32.7655  -86.5840 0.8165300      17 24
    ##   ---                                                                    
    ## 1591: 725645    WY -105.683 41.317 42.8055 -108.2355 2.9548077      26  9
    ## 1592: 726650    WY -105.541 44.339 42.8055 -108.2355 3.1003149      26 17
    ## 1593: 725775    WY -111.030 41.273 42.8055 -108.2355 3.1871283      26 15
    ## 1594: 725640    WY -104.800 41.150 42.8055 -108.2355 3.8135732      26  8
    ## 1595: 725763    WY -104.158 42.061 42.8055 -108.2355 4.1449109      26 14

``` r
met_location <- met_stations[n == mindist, .(USAFID, STATE, lon, lat)][order(STATE)]
met_location
```

    ##     USAFID STATE      lon    lat
    ##  1: 722265    AL  -86.350 32.383
    ##  2: 720401    AR  -92.450 35.600
    ##  3: 722783    AZ -111.733 33.467
    ##  4: 723898    CA -119.628 36.319
    ##  5: 726396    CO -105.516 39.050
    ##  6: 725027    CT  -72.828 41.510
    ##  7: 724088    DE  -75.467 39.133
    ##  8: 722011    FL  -81.437 28.290
    ##  9: 722175    GA  -83.600 32.633
    ## 10: 725466    IA  -93.566 41.691
    ## 11: 725865    ID -114.300 43.500
    ## 12: 724397    IL  -88.950 40.483
    ## 13: 725335    IN  -86.152 40.648
    ## 14: 724509    KS  -97.275 38.058
    ## 15: 720448    KY  -84.770 37.578
    ## 16: 720468    LA  -92.099 30.558
    ## 17: 725068    MA  -71.021 41.876
    ## 18: 724067    MD  -76.414 39.326
    ## 19: 726073    ME  -69.667 44.533
    ## 20: 725405    MI  -84.688 43.322
    ## 21: 726569    MN  -94.382 44.859
    ## 22: 724453    MO  -93.183 38.704
    ## 23: 722350    MS  -90.083 32.317
    ## 24: 726770    MT -108.533 45.800
    ## 25: 722201    NC  -79.101 35.582
    ## 26: 720867    ND -100.024 48.390
    ## 27: 725520    NE  -98.317 40.967
    ## 28: 726155    NH  -71.433 43.567
    ## 29: 724090    NJ  -74.350 40.033
    ## 30: 722677    NM -105.662 35.003
    ## 31: 724770    NV -116.005 39.601
    ## 32: 725145    NY  -74.795 41.702
    ## 33: 720928    OH  -83.115 40.280
    ## 34: 723540    OK  -97.383 35.417
    ## 35: 725970    OR -122.867 42.367
    ## 36: 725118    PA  -76.851 40.217
    ## 37: 725074    RI  -71.412 41.597
    ## 38: 720603    SC  -80.567 34.283
    ## 39: 726560    SD -100.285 44.381
    ## 40: 723273    TN  -86.520 36.009
    ## 41: 722575    TX  -97.683 31.083
    ## 42: 725724    UT -111.723 40.219
    ## 43: 720498    VA  -77.517 37.400
    ## 44: 726114    VT  -72.614 44.534
    ## 45: 720388    WA -122.287 47.104
    ## 46: 726452    WI  -89.837 44.359
    ## 47: 720328    WV  -80.274 39.000
    ## 48: 726720    WY -108.450 43.067
    ##     USAFID STATE      lon    lat

``` r
repweather <-
  merge(
  x =   repweather[, .(USAFID, STATE)],
  y = met_stations[, .(USAFID, STATE, lat, lon)],
  by.x = c("USAFID","STATE"),
  by.y = c("USAFID","STATE"),
  all.x = TRUE,
  all.y = FALSE
) 

repweather[, station_type := "met"]
met_location <- met_location[, .(USAFID, STATE, lat, lon)][, station_type := "loc"]
all_stations <- rbind(repweather, met_location, use.names=TRUE)
```

``` r
ggplot(map_data("state"), aes(x = long, y = lat)) +
  geom_map(aes(map_id = region), map = map_data("state"), col = "lightgrey", fill = "gray") +
  geom_point(
    data    = all_stations,
    mapping = aes(x = lon, y = lat, col = station_type),
    inherit.aes = FALSE, cex = 2, alpha = 0.5
    )
```

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- --> \## Q4

``` r
met[, state_temp := mean(temp, na.rm = TRUE), by = STATE]
met[, temp_cat := fifelse(
  state_temp < 20, "low-temp", 
  fifelse(state_temp < 25, "mid-temp", "high-temp"))
  ]
head(met)
```

    ##    USAFID  WBAN year month day hour min  lat      lon elev wind.dir wind.dir.qc
    ## 1: 690150 93121 2019     8   1    0  56 34.3 -116.166  696      220           5
    ## 2: 690150 93121 2019     8   1    1  56 34.3 -116.166  696      230           5
    ## 3: 690150 93121 2019     8   1    2  56 34.3 -116.166  696      230           5
    ## 4: 690150 93121 2019     8   1    3  56 34.3 -116.166  696      210           5
    ## 5: 690150 93121 2019     8   1    4  56 34.3 -116.166  696      120           5
    ## 6: 690150 93121 2019     8   1    5  56 34.3 -116.166  696       NA           9
    ##    wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc ceiling.ht.method
    ## 1:              N     5.7          5      22000             5                 9
    ## 2:              N     8.2          5      22000             5                 9
    ## 3:              N     6.7          5      22000             5                 9
    ## 4:              N     5.1          5      22000             5                 9
    ## 5:              N     2.1          5      22000             5                 9
    ## 6:              C     0.0          5      22000             5                 9
    ##    sky.cond vis.dist vis.dist.qc vis.var vis.var.qc temp temp.qc dew.point
    ## 1:        N    16093           5       N          5 37.2       5      10.6
    ## 2:        N    16093           5       N          5 35.6       5      10.6
    ## 3:        N    16093           5       N          5 34.4       5       7.2
    ## 4:        N    16093           5       N          5 33.3       5       5.0
    ## 5:        N    16093           5       N          5 32.8       5       5.0
    ## 6:        N    16093           5       N          5 31.1       5       5.6
    ##    dew.point.qc atm.press atm.press.qc       rh CTRY STATE state_temp temp_cat
    ## 1:            5    1009.9            5 19.88127   US    CA   22.36199 mid-temp
    ## 2:            5    1010.3            5 21.76098   US    CA   22.36199 mid-temp
    ## 3:            5    1010.6            5 18.48212   US    CA   22.36199 mid-temp
    ## 4:            5    1011.6            5 16.88862   US    CA   22.36199 mid-temp
    ## 5:            5    1012.7            5 17.38410   US    CA   22.36199 mid-temp
    ## 6:            5    1012.7            5 20.01540   US    CA   22.36199 mid-temp

``` r
table(met$temp_cat, useNA = "always")
```

    ## 
    ## high-temp  low-temp  mid-temp      <NA> 
    ##    811126    430794   1135423         0
