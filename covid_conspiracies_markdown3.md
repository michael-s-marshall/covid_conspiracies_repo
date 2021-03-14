covid\_conspiracies\_markdown2
================
Michael Marshall
14/03/2021

## Loading Packages and Data

``` r
pacman::p_load(tidyverse, stringr, ggridges, forcats, labelled, leaps,
               psych, corrr, cowplot, expss, haven, interplot,
               interactions, jtools,labelled, pscl, psych, 
               sjPlot, skimr)

load("COVID W1_W2_W3 Cleaned 2878.RData") # needs to be in your wd
```

## Rescaling variable

``` r
## [rescale01] Function to rescale a variable from 0 to 1
rescale01 <- function(x, ...) {
  (x - min(x, ...)) / ((max(x, ...)) - min(x, ...))
}
```

## Summary and distribution of different COVID specific conspiracies

``` r
# plotting density of different covid conspiracies
df %>% 
  dplyr::select(W2_Conspiracy_Theory1:W2_Conspiracy_Theory5) %>% 
  gather(conspiracy_code, belief,
         W2_Conspiracy_Theory1:W2_Conspiracy_Theory5) %>%
  mutate(conspiracy_code = as.factor(conspiracy_code)) %>%
  filter(!conspiracy_code %in% c("W2_Conspiracy_Theory4",
                                 "W2_Conspiracy_Theory5")) %>% 
  ggplot(aes(x = belief, y = conspiracy_code, height = ..density..)) +
  geom_density_ridges(aes(rel_min_height = 0.005),
                      stat = "density",
                      fill = get_colors("CUD Bright",num.colors = 1)) +
  theme_nice() +
  scale_y_discrete(labels = c("Wuhan laboratory","Meat market","5G")) +
  labs(y = NULL, x = "Belief scale (0-100)",
       caption = "Distribution of origin theory belief") +
  theme(plot.caption = element_text(hjust = 0.61,
                                    face = "bold",
                                    size = 10),
        axis.title.x = element_text(size = 8,
                                    hjust = 0.5)
  )
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#pacman::p_load(patchwork)
df %>% 
  dplyr::select(W2_Conspiracy_Theory1:W2_Conspiracy_Theory5) %>% 
  gather(conspiracy_code, belief,
         W2_Conspiracy_Theory1:W2_Conspiracy_Theory5) %>%
  mutate(conspiracy_code = fct_recode(
    as.factor(conspiracy_code),
    "Wuhan laboratory" = "W2_Conspiracy_Theory1",
    "Meat market" = "W2_Conspiracy_Theory2",
    "5G" = "W2_Conspiracy_Theory3",
    "4" = "W2_Conspiracy_Theory4",
    "5" = "W2_Conspiracy_Theory5")
    ) %>%
  filter(!conspiracy_code %in% c("4","5")) %>% 
  mutate(
    order_var = ifelse(conspiracy_code == "Meat market", 1,
                       ifelse(conspiracy_code == "Wuhan laboratory",2,3))
  ) %>% 
  ggplot(aes(x = belief)) +
  geom_density(fill = get_colors("CUD Bright",num.colors = 1),
               alpha = 0.8) +
  theme_nice() +
  facet_wrap(~fct_reorder(conspiracy_code,order_var), 
             scales = "free_y", ncol = 1) +
  labs(y = "Density", 
       x = "Distribution of origin theory belief")
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Warning: Removed 4416 rows containing non-finite values (stat_density).

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Cleaning dataset

The following code filters down to just those observations that have
completed the battery of questions relating to COVID specific
conspiracies. It also creates a tibble counting the missing
observations, which can be useful to have as an object.

``` r
# filtering for completed dependent variable
conspiracies <- df %>% 
  filter(!is.na(W2_Conspiracy_Theory1) |
           !is.na(W2_Conspiracy_Theory2) |
           !is.na(W2_Conspiracy_Theory3) |
           !is.na(W2_Conspiracy_Theory3) |
           !is.na(W2_Conspiracy_Theory4) |
           !is.na(W2_Conspiracy_Theory5)) %>% 
  rename(W1_Housing_tenure = W1_Hosuing_tenure)

# function to count NAs
count_na <- function(x){
  sum(is.na(x))
}

conspiracies %>% 
  dplyr::select(W2_Conspiracy_Theory1:W2_Conspiracy_Theory5) %>% 
  map_int(count_na)
```

    ## W2_Conspiracy_Theory1 W2_Conspiracy_Theory2 W2_Conspiracy_Theory3 
    ##                     0                     0                     0 
    ## W2_Conspiracy_Theory4 W2_Conspiracy_Theory5 
    ##                     0                     0

``` r
missing <- tibble(
  variable = names(conspiracies),
  NAs = conspiracies %>% map_int(count_na)
)

#View(missing)
```

Due to some measurement error in original *W1\_Education\_binary*
variable, the code below overwrites the variable, and creates a dummy
for degree educated respondents (undergrad OR postgrad = 1).

``` r
conspiracies <- conspiracies %>% 
  mutate(
    W1_Education_binary = ifelse(
      W1_Education %in% c(5,7), 1, 0
    )
  )
count(conspiracies, W1_Education, W1_Education_binary)
```

    ## # A tibble: 8 x 3
    ##                  W1_Education W1_Education_binary     n
    ##                     <dbl+lbl>               <dbl> <int>
    ## 1 1 [No qualifications]                         0    47
    ## 2 2 [O-Level/GCSE or similar]                   0   263
    ## 3 3 [A-Level or similar]                        0   241
    ## 4 4 [Technical qualification]                   0   138
    ## 5 5 [Undergraduate degree]                      1   399
    ## 6 6 [Diploma]                                   0    76
    ## 7 7 [Postgraduate degree]                       1   225
    ## 8 8 [Other qualifications]                      0    17

The code below turns the *preferred newspaper* variables into dummy
variables, as they were previously coded as *1=Yes* and everything else
as *NA*.

``` r
# making preferred newspaper dummy variable (i.e. replacing NA with 0)
na_to_zero <- function(x){
  x[is.na(x)] <- 0
  x <- as.numeric(x)
  return(x)
} 

paper_vars <- rep(str_c("W2_Newspaper_prefer",seq(1,11,1)))

conspiracies[paper_vars] <- conspiracies[paper_vars] %>% 
  map_df(na_to_zero)

conspiracies <- conspiracies %>% 
  mutate(
    red_top_tabloid = ifelse(
      W2_Newspaper_prefer3 == 1 | W2_Newspaper_prefer2 == 1 |
        W2_Newspaper_prefer7 == 1 | W2_Newspaper_prefer8 == 1 |
        W2_Newspaper_prefer9 == 1, 1, 0),
    mid_level_news = ifelse(
      W2_Newspaper_prefer1 == 1 | W2_Newspaper_prefer4 == 1, 1, 0),
    elite_news = ifelse(
      W2_Newspaper_prefer5 == 1 | W2_Newspaper_prefer6 == 1 |
        W2_Newspaper_prefer10 == 1 | 
        W2_Newspaper_prefer11 == 1, 1, 0)
  )

conspiracies %>% 
  count(red_top_tabloid, mid_level_news, elite_news)
```

    ## # A tibble: 8 x 4
    ##   red_top_tabloid mid_level_news elite_news     n
    ##             <dbl>          <dbl>      <dbl> <int>
    ## 1               0              0          0   423
    ## 2               0              0          1   317
    ## 3               0              1          0   219
    ## 4               0              1          1    75
    ## 5               1              0          0   163
    ## 6               1              0          1    54
    ## 7               1              1          0    73
    ## 8               1              1          1    82

``` r
# Creating DVs
# [ethno] ethnocentrism
eth_keys <- list(ethno = cs(W2_Nationalism1,W2_Nationalism2))
eth_test <- scoreItems(eth_keys, conspiracies, min = 1, max = 5)
head(eth_test$scores)
```

    ##      ethno
    ## [1,]     4
    ## [2,]     4
    ## [3,]     4
    ## [4,]     3
    ## [5,]     3
    ## [6,]     3

``` r
eth_test$alpha  # Scale alpha
```

    ##           ethno
    ## alpha 0.8213221

``` r
conspiracies$ethno <- rescale01(eth_test$scores, na.rm = TRUE)
conspiracies$ethno <- c(conspiracies$ethno)  # Ensure variable is numeric and not matrix class
describe(conspiracies$ethno)
```

    ##    vars    n mean   sd median trimmed  mad min max range  skew kurtosis   se
    ## X1    1 1406 0.57 0.25   0.62    0.58 0.19   0   1     1 -0.33    -0.18 0.01

``` r
# Right wing authoritarianism
rwa_keys <- list(rwa = cs(W1_Authoritarianism1_R,
                          W1_Authoritarianism2,
                          W1_Authoritarianism3,
                          W1_Authoritarianism4_R, 
                          W1_Authoritarianism5_R,
                          W1_Authoritarianism6))

rwa_test <- scoreItems(rwa_keys, conspiracies, min = 1, max = 5)
head(rwa_test$scores)
```

    ##           rwa
    ## [1,] 2.833333
    ## [2,] 2.666667
    ## [3,] 3.166667
    ## [4,] 3.333333
    ## [5,] 3.000000
    ## [6,] 3.500000

``` r
summary(rwa_test$alpha)  # Scale alpha
```

    ##       rwa        
    ##  Min.   :0.6837  
    ##  1st Qu.:0.6837  
    ##  Median :0.6837  
    ##  Mean   :0.6837  
    ##  3rd Qu.:0.6837  
    ##  Max.   :0.6837

``` r
conspiracies$RWA <- rescale01(rwa_test$scores, na.rm = TRUE)
conspiracies$RWA <- c(conspiracies$RWA)  # Ensure variable is numeric and not matrix class

describe(conspiracies$RWA)
```

    ##    vars    n mean   sd median trimmed  mad min max range  skew kurtosis se
    ## X1    1 1406 0.51 0.17    0.5    0.52 0.12   0   1     1 -0.25      0.3  0

``` r
## [SDO] Social Dominance Orientation
sdo_keys <- list(sdo = cs(W1_Social_Dominance1,
                          W1_Social_Dominance2_R,
                          W1_Social_Dominance3_R,
                          W1_Social_Dominance4,
                          W1_Social_Dominance5_R,
                          W1_Social_Dominance6, 
                          W1_Social_Dominance7,
                          W1_Social_Dominance8_R))
sdo_test <- scoreItems(sdo_keys, conspiracies, min = 1, max = 5)
head(sdo_test$scores)
```

    ##        sdo
    ## [1,] 1.500
    ## [2,] 3.000
    ## [3,] 3.000
    ## [4,] 2.625
    ## [5,] 3.000
    ## [6,] 2.500

``` r
summary(sdo_test$alpha)  # Scale alpha
```

    ##       sdo        
    ##  Min.   :0.8404  
    ##  1st Qu.:0.8404  
    ##  Median :0.8404  
    ##  Mean   :0.8404  
    ##  3rd Qu.:0.8404  
    ##  Max.   :0.8404

``` r
conspiracies$SDO <- rescale01(sdo_test$scores, na.rm = TRUE)
conspiracies$SDO <- c(conspiracies$SDO)  # Ensure variable is numeric and not matrix class

summary(conspiracies$SDO)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.2258  0.3871  0.3620  0.5161  1.0000

``` r
## [threat] Covid-19 related Threat
summary(conspiracies$W2_COVID19_anxiety)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00   50.00   65.00   61.25   80.75  100.00

``` r
conspiracies$threat <- rescale01(conspiracies$W2_COVID19_anxiety)
conspiracies %>% 
  dplyr::select(W2_COVID19_anxiety, threat) %>%
  correlate()
```

    ## 
    ## Correlation method: 'pearson'
    ## Missing treated using: 'pairwise.complete.obs'

    ## # A tibble: 2 x 3
    ##   rowname            W2_COVID19_anxiety threat
    ##   <chr>                           <dbl>  <dbl>
    ## 1 W2_COVID19_anxiety                 NA      1
    ## 2 threat                              1     NA

``` r
## [right] Right-Wing political views
table(conspiracies$W1_Political_Scale)
```

    ## 
    ##   1   2   3   4   5   6   7   8   9  10 
    ##  35  44 116 143 488 221 182 102  36  39

``` r
conspiracies$right <- rescale01(conspiracies$W1_Political_Scale)
conspiracies %>% 
  dplyr::select(W1_Political_Scale, right) %>%
  correlate()
```

    ## 
    ## Correlation method: 'pearson'
    ## Missing treated using: 'pairwise.complete.obs'

    ## # A tibble: 2 x 3
    ##   rowname            W1_Political_Scale right
    ##   <chr>                           <dbl> <dbl>
    ## 1 W1_Political_Scale                 NA     1
    ## 2 right                               1    NA

``` r
## [soc.con] Social conservatism
table(conspiracies$W1_Political_Abortion_SSM)
```

    ## 
    ##   1   2   3   4   5   6   7   8   9  10 
    ## 262 123 175 129 330 124 109  67  31  56

``` r
conspiracies$soc_con <- rescale01(
  conspiracies$W1_Political_Abortion_SSM)
conspiracies %>% 
  dplyr::select(W1_Political_Abortion_SSM, soc_con) %>%
  correlate()
```

    ## 
    ## Correlation method: 'pearson'
    ## Missing treated using: 'pairwise.complete.obs'

    ## # A tibble: 2 x 3
    ##   rowname                   W1_Political_Abortion_SSM soc_con
    ##   <chr>                                         <dbl>   <dbl>
    ## 1 W1_Political_Abortion_SSM                        NA       1
    ## 2 soc_con                                           1      NA

``` r
## [fis.con] Fiscal conservatism
table(conspiracies$W1_Political_Fiscal)
```

    ## 
    ##   1   2   3   4   5   6   7   8   9  10 
    ##  55  43 112 133 430 201 226 121  39  46

``` r
conspiracies$fis_con <- rescale01(conspiracies$W1_Political_Fiscal)
conspiracies %>% 
  dplyr::select(W1_Political_Fiscal, fis_con) %>%
  correlate()
```

    ## 
    ## Correlation method: 'pearson'
    ## Missing treated using: 'pairwise.complete.obs'

    ## # A tibble: 2 x 3
    ##   rowname             W1_Political_Fiscal fis_con
    ##   <chr>                             <dbl>   <dbl>
    ## 1 W1_Political_Fiscal                  NA       1
    ## 2 fis_con                               1      NA

``` r
## [age.c] Age (in years)
summary(conspiracies$W2_Age_year)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   18.00   37.00   50.00   49.32   61.00   88.00

``` r
conspiracies$age_sc <- rescale01(conspiracies$W2_Age_year)
```

``` r
# conspiracy ideation
consp_keys <- list(consp = cs(W1_Conspiracy_1,
                          W1_Conspiracy_2,
                          W1_Conspiracy_3,
                          W1_Conspiracy_4,
                          W1_Conspiracy_5))
consp_test <- scoreItems(consp_keys, conspiracies, min = 1, max = 11)
```

    ## Number of categories should be increased  in order to count frequencies.

``` r
head(consp_test$scores)
```

    ##      consp
    ## [1,]   5.8
    ## [2,]   9.0
    ## [3,]   7.0
    ## [4,]   8.6
    ## [5,]   6.0
    ## [6,]   8.8

``` r
consp_test$alpha  # Scale alpha
```

    ##           consp
    ## alpha 0.8537269

``` r
cor(consp_test$scores, conspiracies$W1_Conspiracy_Total)
```

    ##       [,1]
    ## consp    1

``` r
mean(
  round(rescale01(consp_test$scores, na.rm = T), 4) ==
    round(rescale01(conspiracies$W1_Conspiracy_Total, na.rm = T), 4)
) # scale is equal to pre-existing variable in dataset
```

    ## [1] 1

``` r
# intolerance of uncertainty
iou_keys <- list(iou = cs(W2_IOU1, W2_IOU2,W2_IOU3, W2_IOU4,
                          W2_IOU5, W2_IOU5,W2_IOU6,W2_IOU7,
                          W2_IOU7,W2_IOU8,W2_IOU9,W2_IOU10,
                          W2_IOU11,W2_IOU12))
iou_test <- scoreItems(iou_keys, conspiracies, min = 1, max = 5)
head(iou_test$scores)
```

    ##           iou
    ## [1,] 3.416667
    ## [2,] 3.916667
    ## [3,] 4.250000
    ## [4,] 3.083333
    ## [5,] 3.000000
    ## [6,] 2.500000

``` r
iou_test$alpha  # Scale alpha
```

    ##             iou
    ## alpha 0.9097412

``` r
cor(iou_test$scores, conspiracies$W2_IOU_Total)
```

    ##     [,1]
    ## iou    1

``` r
mean(
  round(rescale01(iou_test$scores, na.rm = T), 4) ==
    round(rescale01(conspiracies$W2_IOU_Total, na.rm = T), 4)
) # scale is equal to pre-existing variable in dataset
```

    ## [1] 1

``` r
factors <- c("W1_Ethnicity",
             "W2_Gender_binary")
```

``` r
# turning to factors
conspiracies[factors] <- conspiracies[factors] %>% 
  map_df(as.factor)
```

``` r
# renaming trust in science
conspiracies <- conspiracies %>% 
  rename(distrust_science = W2_Trust_Body6)
```

``` r
# rescaling the remaing numeric variables
numerics <- c("W1_Conspiracy_Total","W2_IOU_Total", "W2_INFO_5",
              "W2_INFO_9","distrust_science","W1_Income_2019")

conspiracies[numerics] <- conspiracies[numerics] %>% 
  map_df(rescale01, na.rm = TRUE)
```

``` r
# creating scaled versions of each conspiracy belief
conspiracies$conspiracy1_sc <- rescale01(
  conspiracies$W2_Conspiracy_Theory1, na.rm = TRUE
)

conspiracies$conspiracy2_sc <- rescale01(
  conspiracies$W2_Conspiracy_Theory2, na.rm = TRUE
)

conspiracies$conspiracy3_sc <- rescale01(
  conspiracies$W2_Conspiracy_Theory3, na.rm = TRUE
)

conspiracies$conspiracy4_sc <- rescale01(
  conspiracies$W2_Conspiracy_Theory4, na.rm = TRUE
)

conspiracies$conspiracy5_sc <- rescale01(
  conspiracies$W2_Conspiracy_Theory5, na.rm = TRUE
)
```

## Distribution of variables

A for loop to look at distribution of potential independent variables
(numeric only).

``` r
plot_vars <- conspiracies %>% 
  dplyr::select(
    one_of(numerics),ethno,RWA,SDO,threat,right,soc_con,fis_con
    ) %>%
  names()

for(i in seq_along(plot_vars)){
  
  x1 <- conspiracies[plot_vars][i] %>% as_vector()
  
  print(
    ggplot(data = NULL, aes(x = x1)) +
      geom_vline(aes(xintercept = mean(x1, na.rm =TRUE)), 
                 colour = "black",
                 linetype = "dashed") +
      geom_vline(aes(xintercept = median(x1, na.rm =TRUE)), 
                 colour = "red",
                 linetype = "dashed") +
      geom_density(fill = "lightblue", alpha = 0.7) +
      labs(x = plot_vars[i],
           caption = "Black = Mean, Red = Median")  
  )
  
}
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-23-2.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-23-3.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-23-4.png)<!-- -->

    ## Warning: Removed 4 rows containing non-finite values (stat_density).

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-23-5.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-23-6.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-23-7.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-23-8.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-23-9.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-23-10.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-23-11.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-23-12.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-23-13.png)<!-- -->

# Modelling for belief in Chinese lab origin

## DV Chinese lab conspiracy - singular IV models

``` r
sdo_lab <- lm(conspiracy1_sc ~ SDO,
              data = conspiracies)

summ(sdo_lab)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: conspiracy1_sc
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 65.28, p = 0.00
    ## R² = 0.04
    ## Adj. R² = 0.04 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------
    ##                     Est.   S.E.   t val.      p
    ## ----------------- ------ ------ -------- ------
    ## (Intercept)         0.24   0.02    12.34   0.00
    ## SDO                 0.39   0.05     8.08   0.00
    ## -----------------------------------------------

``` r
rwa_lab <- lm(conspiracy1_sc ~ RWA,
              data = conspiracies)

summ(rwa_lab)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: conspiracy1_sc
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 87.72, p = 0.00
    ## R² = 0.06
    ## Adj. R² = 0.06 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------
    ##                     Est.   S.E.   t val.      p
    ## ----------------- ------ ------ -------- ------
    ## (Intercept)         0.14   0.03     5.07   0.00
    ## RWA                 0.47   0.05     9.37   0.00
    ## -----------------------------------------------

## DV Chinese lab conspiracy - socio-economic variables

``` r
se_lab <- lm(conspiracy1_sc ~ W2_Gender_binary +
               W1_Education_binary +
               W1_Income_2019 +
               age_sc,
              data = conspiracies)

summary(se_lab)$adj.r.squared
```

    ## [1] 0.04693264

``` r
AIC(se_lab)
```

    ## [1] 827.0941

## DV Chinese lab conspiracy - socio-economic variables + political/media

``` r
se_pol_lab <- update(se_lab, ~ . +
                   #political and media variables
                   right +
                   ethno +
                   distrust_science + #distrust of scientists
                   red_top_tabloid + 
                   mid_level_news + 
                   elite_news + 
                   W2_INFO_5 + #social media
                   W2_INFO_9 #family and friends
                 )

summary(se_pol_lab)$adj.r.squared
```

    ## [1] 0.1971395

``` r
AIC(se_pol_lab)
```

    ## [1] 591.6542

## DV Chinese lab conspiracy - socio-economic variables + political/media + pol-psych

``` r
se_polpsych_lab <- update(se_pol_lab, ~ . +
                        #political-psychology variables
                        SDO +
                        RWA +
                        W2_IOU_Total)

summary(se_polpsych_lab)$adj.r.squared
```

    ## [1] 0.2128012

``` r
AIC(se_polpsych_lab)
```

    ## [1] 567.0623

``` r
car::vif(se_polpsych_lab)
```

    ## Registered S3 methods overwritten by 'car':
    ##   method                          from
    ##   influence.merMod                lme4
    ##   cooks.distance.influence.merMod lme4
    ##   dfbeta.influence.merMod         lme4
    ##   dfbetas.influence.merMod        lme4

    ##    W2_Gender_binary W1_Education_binary      W1_Income_2019              age_sc 
    ##            1.087265            1.200203            1.131525            1.366633 
    ##               right               ethno    distrust_science     red_top_tabloid 
    ##            1.487366            1.302714            1.076368            1.089356 
    ##      mid_level_news          elite_news           W2_INFO_5           W2_INFO_9 
    ##            1.126469            1.139595            1.381617            1.221180 
    ##                 SDO                 RWA        W2_IOU_Total 
    ##            1.446343            1.397784            1.136058

## DV Chinese lab conspiracy - socio-economic variables + political/media + pol-psych + covid-threat

``` r
multi_lab <- update(se_polpsych_lab, ~ . +
                  #covid-anxety
                  threat)

summary(multi_lab)$adj.r.squared
```

    ## [1] 0.2207157

``` r
AIC(multi_lab)
```

    ## [1] 553.9136

## DV Chinese lab conspiracy - full model incl. conspiracy ideation

``` r
full_lab <- update(multi_lab, ~ . + 
                  #conspiracies
                  W1_Conspiracy_Total +
                  conspiracy2_sc +
                  conspiracy3_sc)

summary(full_lab)$adj.r.squared
```

    ## [1] 0.2794628

``` r
AIC(full_lab)
```

    ## [1] 447.2214

``` r
summ(full_lab, vifs = T)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: conspiracy1_sc
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(19,1379) = 29.54, p = 0.00
    ## R² = 0.29
    ## Adj. R² = 0.28 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------------------
    ##                              Est.   S.E.   t val.      p    VIF
    ## ------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                 -0.09   0.05    -1.80   0.07       
    ## W2_Gender_binary2            0.02   0.02     1.27   0.20   1.09
    ## W1_Education_binary         -0.05   0.02    -3.07   0.00   1.21
    ## W1_Income_2019              -0.04   0.02    -1.88   0.06   1.15
    ## age_sc                      -0.01   0.04    -0.18   0.86   1.41
    ## right                        0.02   0.05     0.49   0.63   1.49
    ## ethno                        0.12   0.03     3.51   0.00   1.32
    ## distrust_science             0.11   0.03     3.33   0.00   1.19
    ## red_top_tabloid              0.05   0.02     2.54   0.01   1.11
    ## mid_level_news               0.10   0.02     5.82   0.00   1.13
    ## elite_news                  -0.01   0.02    -0.79   0.43   1.14
    ## W2_INFO_5                    0.05   0.03     1.76   0.08   1.41
    ## W2_INFO_9                    0.10   0.03     3.49   0.00   1.26
    ## SDO                          0.16   0.05     3.00   0.00   1.53
    ## RWA                          0.17   0.05     3.11   0.00   1.43
    ## W2_IOU_Total                -0.03   0.04    -0.60   0.55   1.23
    ## threat                       0.13   0.03     4.05   0.00   1.15
    ## W1_Conspiracy_Total          0.26   0.04     6.39   0.00   1.10
    ## conspiracy2_sc              -0.13   0.03    -4.62   0.00   1.07
    ## conspiracy3_sc               0.27   0.04     6.79   0.00   1.31
    ## ---------------------------------------------------------------

## DV Chinese lab conspiracy - interaction RWA and COVID-19 anxiety

``` r
int_lab <- lm(conspiracy1_sc ~ W2_Gender_binary +
                W1_Education_binary +
                W1_Income_2019 +
                age_sc +
                right +
                ethno +
                distrust_science + #distrust of scientists
                red_top_tabloid +
                mid_level_news +
                elite_news +
                W2_INFO_5 + #social media
                W2_INFO_9 + # family and friends
                SDO +
                W2_IOU_Total +
                W1_Conspiracy_Total +
                conspiracy2_sc +
                conspiracy3_sc +
                (RWA * threat), # interaction term
              data = conspiracies) 

summ(int_lab)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: conspiracy1_sc
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(20,1378) = 28.05, p = 0.00
    ## R² = 0.29
    ## Adj. R² = 0.28 
    ## 
    ## Standard errors: OLS
    ## --------------------------------------------------------
    ##                              Est.   S.E.   t val.      p
    ## ------------------------- ------- ------ -------- ------
    ## (Intercept)                 -0.08   0.07    -1.03   0.30
    ## W2_Gender_binary2            0.02   0.02     1.26   0.21
    ## W1_Education_binary         -0.05   0.02    -3.08   0.00
    ## W1_Income_2019              -0.04   0.02    -1.88   0.06
    ## age_sc                      -0.01   0.04    -0.19   0.85
    ## right                        0.02   0.05     0.48   0.63
    ## ethno                        0.12   0.03     3.51   0.00
    ## distrust_science             0.11   0.03     3.33   0.00
    ## red_top_tabloid              0.05   0.02     2.54   0.01
    ## mid_level_news               0.10   0.02     5.83   0.00
    ## elite_news                  -0.01   0.02    -0.77   0.44
    ## W2_INFO_5                    0.05   0.03     1.77   0.08
    ## W2_INFO_9                    0.10   0.03     3.47   0.00
    ## SDO                          0.16   0.05     3.01   0.00
    ## W2_IOU_Total                -0.03   0.04    -0.59   0.55
    ## W1_Conspiracy_Total          0.26   0.04     6.40   0.00
    ## conspiracy2_sc              -0.13   0.03    -4.61   0.00
    ## conspiracy3_sc               0.27   0.04     6.77   0.00
    ## RWA                          0.13   0.12     1.12   0.26
    ## threat                       0.10   0.09     1.07   0.29
    ## RWA:threat                   0.05   0.17     0.30   0.76
    ## --------------------------------------------------------

``` r
plot_coefs(int_lab)
```

    ## Loading required namespace: broom.mixed

    ## Registered S3 method overwritten by 'broom.mixed':
    ##   method      from 
    ##   tidy.gamlss broom

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
summary(full_lab)$adj.r.squared
```

    ## [1] 0.2794628

``` r
AIC(full_lab)
```

    ## [1] 447.2214

``` r
summary(int_lab)$adj.r.squared
```

    ## [1] 0.2789871

``` r
AIC(int_lab)
```

    ## [1] 449.1298

# Modelling for belief in 5G origin conspiracy

## Plots of DV, incl. inverse hyperbolic sine transformation

``` r
# functions for IHS transformation
ihs <- function(x, theta){  # IHS transformation
  asinh(theta * x)/theta
}

ihs_loglik <- function(theta,x){
  
  ihs <- function(x, theta){  # function to IHS transform
    asinh(theta * x)/theta
  }
  
  n <- length(x)
  xt <- ihs(x, theta)
  
  log_lik <- -n*log(sum((xt - mean(xt))^2))- sum(log(1+theta^2*x^2))
  return(log_lik)
}     
```

``` r
best_theta <- optimize(ihs_loglik, 
                       lower = 0.00001, upper = 1e+06,
                       x = conspiracies$W2_Conspiracy_Theory3, 
                       maximum = TRUE)$maximum
best_theta # continues to rise indefinitely
```

    ## [1] 1e+06

``` r
# trying different starting point
best_theta <- optimize(ihs_loglik, 
                       lower = 0.001, upper = 1e+08,
                       x = conspiracies$W2_Conspiracy_Theory3, 
                       maximum = TRUE)$maximum
best_theta # continues to rise indefinitely
```

    ## [1] 1e+08

``` r
# raw data 
conspiracies %>% 
  ggplot(aes(x = W2_Conspiracy_Theory3)) +
  geom_histogram(aes(y = ..density..),
                 colour = "darkgrey", fill = "lightblue") +
  geom_density()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
# ihs transformation where theta = 1
conspiracies %>% 
  ggplot(aes(x = asinh(W2_Conspiracy_Theory3))) +
  geom_histogram(aes(y = ..density..),
                 colour = "darkgrey", fill = "lightblue") +
  geom_density()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-36-2.png)<!-- -->

``` r
# ihs transformation where theta = 1e+08
conspiracies %>% 
  ggplot(aes(x = ihs(W2_Conspiracy_Theory3, best_theta))) +
  geom_histogram(aes(y = ..density..),
                 colour = "darkgrey", fill = "lightblue") +
  geom_density()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-36-3.png)<!-- -->

``` r
# transforming using theta = 1
conspiracies <- conspiracies %>% 
  mutate(w2_conspiracy3_ihs = asinh(W2_Conspiracy_Theory3))
```

## DV 5G conspiracy - singular IV models

``` r
sdo_5g <- lm(conspiracy3_sc ~ SDO,
              data = conspiracies)

summ(sdo_5g)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: conspiracy3_sc
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 82.37, p = 0.00
    ## R² = 0.06
    ## Adj. R² = 0.05 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------
    ##                     Est.   S.E.   t val.      p
    ## ----------------- ------ ------ -------- ------
    ## (Intercept)         0.01   0.01     0.57   0.57
    ## SDO                 0.29   0.03     9.08   0.00
    ## -----------------------------------------------

``` r
rwa_5g <- lm(conspiracy3_sc ~ RWA,
              data = conspiracies)

summ(rwa_5g)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: conspiracy3_sc
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 2.80, p = 0.09
    ## R² = 0.00
    ## Adj. R² = 0.00 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------
    ##                     Est.   S.E.   t val.      p
    ## ----------------- ------ ------ -------- ------
    ## (Intercept)         0.08   0.02     4.40   0.00
    ## RWA                 0.06   0.03     1.67   0.09
    ## -----------------------------------------------

## DV 5G conspiracy - socio-economic variables

``` r
se_5g <- lm(conspiracy3_sc ~ W2_Gender_binary +
               W1_Education_binary +
               W1_Income_2019 +
               age_sc,
              data = conspiracies)

#function for adj.r.squared
adj_rsq <- function(mod){
  out <- summary(mod)$adj.r.squared
  return(out)
}

adj_rsq(se_5g)
```

    ## [1] 0.05624114

``` r
AIC(se_5g)
```

    ## [1] -355.7797

## DV 5G conspiracy - socio-economic variables + political/media

``` r
se_pol_5g <- update(se_5g, ~ . +
                   #political and media variables
                   right +
                   ethno +
                   distrust_science + 
                   red_top_tabloid + 
                   mid_level_news + 
                   elite_news + 
                   W2_INFO_5 + #social media
                   W2_INFO_9) # family and friends

adj_rsq(se_pol_5g)
```

    ## [1] 0.1972151

``` r
AIC(se_pol_5g)
```

    ## [1] -579.9208

## DV 5G conspiracy - socio-economic variables + political/media + pol-psych

``` r
se_polpsych_5g <- update(se_pol_5g, ~ . +
                        #political-psychology variables
                        SDO +
                        RWA +
                        W2_IOU_Total)

adj_rsq(se_polpsych_5g)
```

    ## [1] 0.2230283

``` r
AIC(se_polpsych_5g)
```

    ## [1] -622.6755

## DV 5G conspiracy - socio-economic variables + political/media + pol-psych + covid-threat

``` r
multi_5g <- update(se_polpsych_5g, ~ . +
                  #covid-anxiety
                  threat)

adj_rsq(multi_5g)
```

    ## [1] 0.2231347

``` r
AIC(multi_5g)
```

    ## [1] -621.8791

## DV 5G conspiracy - socio-economic variables + political/media + pol-psych + covid-threat and CRT + conspiracies

``` r
full_5g <- update(multi_5g, ~ . +
                #conspiracies
                W1_Conspiracy_Total +
                conspiracy1_sc +
                conspiracy2_sc)

adj_rsq(full_5g)
```

    ## [1] 0.2499203

``` r
AIC(full_5g)
```

    ## [1] -668.0066

``` r
summ(full_5g, vifs = T)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: conspiracy3_sc
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(19,1379) = 25.52, p = 0.00
    ## R² = 0.26
    ## Adj. R² = 0.25 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------------------
    ##                              Est.   S.E.   t val.      p    VIF
    ## ------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                 -0.08   0.03    -2.25   0.02       
    ## W2_Gender_binary2           -0.00   0.01    -0.05   0.96   1.09
    ## W1_Education_binary         -0.01   0.01    -0.78   0.43   1.21
    ## W1_Income_2019              -0.04   0.02    -2.46   0.01   1.14
    ## age_sc                      -0.08   0.03    -2.86   0.00   1.40
    ## right                        0.02   0.03     0.53   0.59   1.49
    ## ethno                        0.03   0.02     1.39   0.16   1.33
    ## distrust_science             0.18   0.02     8.22   0.00   1.14
    ## red_top_tabloid              0.06   0.01     4.64   0.00   1.10
    ## mid_level_news              -0.01   0.01    -0.68   0.49   1.15
    ## elite_news                   0.01   0.01     0.70   0.49   1.14
    ## W2_INFO_5                    0.07   0.02     3.83   0.00   1.40
    ## W2_INFO_9                    0.07   0.02     3.51   0.00   1.26
    ## SDO                          0.22   0.03     6.43   0.00   1.50
    ## RWA                         -0.12   0.04    -3.44   0.00   1.43
    ## W2_IOU_Total                 0.02   0.03     0.63   0.53   1.23
    ## threat                       0.01   0.02     0.31   0.76   1.17
    ## W1_Conspiracy_Total          0.03   0.03     1.17   0.24   1.13
    ## conspiracy1_sc               0.12   0.02     6.79   0.00   1.36
    ## conspiracy2_sc               0.01   0.02     0.30   0.76   1.09
    ## ---------------------------------------------------------------

## DV 5G IHS conspiracy - singular IV models

``` r
sdo_5g_ihs <- lm(w2_conspiracy3_ihs ~ SDO,
              data = conspiracies)

summ(sdo_5g_ihs)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 107.63, p = 0.00
    ## R² = 0.07
    ## Adj. R² = 0.07 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------
    ##                     Est.   S.E.   t val.      p
    ## ----------------- ------ ------ -------- ------
    ## (Intercept)         0.56   0.10     5.56   0.00
    ## SDO                 2.58   0.25    10.37   0.00
    ## -----------------------------------------------

``` r
rwa_5g_ihs <- lm(w2_conspiracy3_ihs ~ RWA,
              data = conspiracies)

summ(rwa_5g_ihs)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 23.23, p = 0.00
    ## R² = 0.02
    ## Adj. R² = 0.02 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------
    ##                     Est.   S.E.   t val.      p
    ## ----------------- ------ ------ -------- ------
    ## (Intercept)         0.82   0.15     5.64   0.00
    ## RWA                 1.30   0.27     4.82   0.00
    ## -----------------------------------------------

## DV 5G IHS conspiracy - socio-economic variables

``` r
se_5g_ihs <- lm(w2_conspiracy3_ihs ~ W2_Gender_binary +
               W1_Education_binary +
               W1_Income_2019 +
               age_sc,
              data = conspiracies)

adj_rsq(se_5g_ihs)
```

    ## [1] 0.05804877

``` r
AIC(se_5g_ihs)
```

    ## [1] 5443

## DV 5G IHS conspiracy - socio-economic variables + political/media

``` r
se_pol_5g_ihs <- update(se_5g_ihs, ~ . +
                   #political and media variables
                   right +
                   ethno +
                   distrust_science + 
                   red_top_tabloid + 
                   mid_level_news + 
                   elite_news + 
                   W2_INFO_5 + #social media
                   W2_INFO_9)

adj_rsq(se_pol_5g_ihs)
```

    ## [1] 0.2308266

``` r
AIC(se_pol_5g_ihs)
```

    ## [1] 5147.385

## DV 5G IHS conspiracy - socio-economic variables + political/media + pol-psych

``` r
se_polpsych_5g_ihs <- update(se_pol_5g_ihs, ~ . +
                        #political-psychology variables
                        SDO +
                        RWA +
                        W2_IOU_Total)

adj_rsq(se_polpsych_5g_ihs)
```

    ## [1] 0.2586094

``` r
AIC(se_polpsych_5g_ihs)
```

    ## [1] 5098.886

## DV 5G IHS conspiracy - socio-economic variables + political/media + pol-psych + covid-threat

``` r
multi_5g_ihs <- update(se_polpsych_5g_ihs, ~ . +
                  #covid-anxiety
                  threat)

adj_rsq(multi_5g_ihs)
```

    ## [1] 0.2591528

``` r
AIC(multi_5g_ihs)
```

    ## [1] 5098.848

## DV 5G IHS conspiracy - socio-economic variables + political/media + pol-psych + covid-threat and CRT + conspiracies

``` r
full_5g_ihs <- update(multi_5g_ihs, ~ . +
                #conspiracies
                W1_Conspiracy_Total +
                conspiracy1_sc +
                conspiracy2_sc)

adj_rsq(full_5g_ihs)
```

    ## [1] 0.2958343

``` r
AIC(full_5g_ihs)
```

    ## [1] 5030.766

``` r
summ(full_5g_ihs, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(19,1379) = 31.91, p = 0.00
    ## R² = 0.31
    ## Adj. R² = 0.30 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------------------
    ##                              Est.   S.E.   t val.      p    VIF
    ## ------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                 -0.44   0.27    -1.65   0.10       
    ## W2_Gender_binary2            0.15   0.08     1.90   0.06   1.09
    ## W1_Education_binary         -0.09   0.09    -1.02   0.31   1.21
    ## W1_Income_2019              -0.32   0.12    -2.78   0.01   1.14
    ## age_sc                      -0.34   0.21    -1.59   0.11   1.40
    ## right                        0.03   0.23     0.11   0.91   1.49
    ## ethno                        0.24   0.18     1.35   0.18   1.33
    ## distrust_science             1.59   0.17     9.61   0.00   1.14
    ## red_top_tabloid              0.39   0.09     4.20   0.00   1.10
    ## mid_level_news              -0.04   0.09    -0.50   0.62   1.15
    ## elite_news                  -0.04   0.09    -0.52   0.60   1.14
    ## W2_INFO_5                    0.69   0.14     4.84   0.00   1.40
    ## W2_INFO_9                    0.43   0.15     2.82   0.00   1.26
    ## SDO                          1.77   0.27     6.69   0.00   1.50
    ## RWA                         -0.34   0.27    -1.24   0.21   1.43
    ## W2_IOU_Total                 0.17   0.22     0.79   0.43   1.23
    ## threat                       0.10   0.16     0.61   0.54   1.17
    ## W1_Conspiracy_Total          0.16   0.21     0.75   0.45   1.13
    ## conspiracy1_sc               1.12   0.14     8.19   0.00   1.36
    ## conspiracy2_sc              -0.09   0.14    -0.63   0.53   1.09
    ## ---------------------------------------------------------------

## DV 5G conspiracy poisson regression model

``` r
# looking at mean and variance to check for overdispersion
conspiracies %>% 
  mutate(
    sdo_deciles = cut_number(SDO,10)
  ) %>%
  group_by(sdo_deciles) %>% 
  summarise(
    mean_5g = mean(W2_Conspiracy_Theory3),
    var_5g = sd(W2_Conspiracy_Theory3) ^ 2
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 10 x 3
    ##    sdo_deciles    mean_5g var_5g
    ##    <fct>            <dbl>  <dbl>
    ##  1 [0,0.0968]        3.77   171.
    ##  2 (0.0968,0.194]    5.95   285.
    ##  3 (0.194,0.258]     5.11   250.
    ##  4 (0.258,0.323]     6.52   228.
    ##  5 (0.323,0.387]     8.72   316.
    ##  6 (0.387,0.452]    11.6    479.
    ##  7 (0.452,0.484]    11.2    418.
    ##  8 (0.484,0.516]    26.1    894.
    ##  9 (0.516,0.548]    18.2    749.
    ## 10 (0.548,1]        13.2    544.

``` r
conspiracies %>% 
  mutate(
    social_media = cut_interval(W2_INFO_5,5)
  ) %>%
  group_by(social_media) %>% 
  summarise(
    mean_5g = mean(W2_Conspiracy_Theory3),
    var_5g = sd(W2_Conspiracy_Theory3) ^ 2
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 4 x 3
    ##   social_media mean_5g var_5g
    ##   <fct>          <dbl>  <dbl>
    ## 1 [0,0.2]         6.11   252.
    ## 2 (0.2,0.4]      10.9    405.
    ## 3 (0.6,0.8]      19.1    786.
    ## 4 (0.8,1]        20.3    886.

The tables above suggest overdispersion of the data. As a result,
running quasipoisson model.

``` r
full_5g_poiss <- glm(W2_Conspiracy_Theory3 ~
                           #socio-economic variables
                           W2_Gender_binary +
                           W1_Education_binary +
                           W1_Income_2019 +
                           age_sc +
                           
                           #political and media variables
                           right +
                           ethno +
                           distrust_science + #distrust of scientists
                           red_top_tabloid + 
                           mid_level_news + 
                           elite_news + 
                           W2_INFO_5 + #social media
                           W2_INFO_9 + #family and friends
                           
                           #political-psychology variables
                           SDO +
                           RWA +
                           W2_IOU_Total +
                           
                           #covid-anxiety
                           threat +
                           
                           #conspiracies
                           W1_Conspiracy_Total +
                           conspiracy1_sc +
                           conspiracy2_sc,
                         family = "quasipoisson", data = conspiracies)

summ(full_5g_poiss)
```

    ## Note: Pseudo-R2 for quasibinomial/quasipoisson families is calculated by
    ## refitting the fitted and null models as binomial/poisson.

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: Generalized linear model
    ##   Family: quasipoisson 
    ##   Link function: log 
    ## 
    ## MODEL FIT:
    ## <U+03C7>²(19) = 14481.03, p = 0.00
    ## Pseudo-R² (Cragg-Uhler) = 1.00
    ## Pseudo-R² (McFadden) = 0.33
    ## AIC =  NA, BIC =  NA 
    ## 
    ## Standard errors: MLE
    ## --------------------------------------------------------
    ##                              Est.   S.E.   t val.      p
    ## ------------------------- ------- ------ -------- ------
    ## (Intercept)                  0.47   0.32     1.46   0.14
    ## W2_Gender_binary2            0.06   0.09     0.67   0.50
    ## W1_Education_binary         -0.13   0.10    -1.31   0.19
    ## W1_Income_2019              -0.32   0.14    -2.30   0.02
    ## age_sc                      -0.93   0.24    -3.82   0.00
    ## right                       -0.05   0.23    -0.23   0.82
    ## ethno                        0.22   0.21     1.06   0.29
    ## distrust_science             1.18   0.17     7.09   0.00
    ## red_top_tabloid              0.40   0.09     4.28   0.00
    ## mid_level_news              -0.05   0.10    -0.53   0.59
    ## elite_news                   0.03   0.10     0.28   0.78
    ## W2_INFO_5                    0.44   0.16     2.70   0.01
    ## W2_INFO_9                    0.48   0.18     2.72   0.01
    ## SDO                          2.11   0.31     6.82   0.00
    ## RWA                         -0.61   0.33    -1.84   0.07
    ## W2_IOU_Total                 0.07   0.23     0.32   0.75
    ## threat                       0.07   0.19     0.37   0.71
    ## W1_Conspiracy_Total          0.35   0.23     1.51   0.13
    ## conspiracy1_sc               1.17   0.16     7.08   0.00
    ## conspiracy2_sc              -0.02   0.17    -0.11   0.91
    ## --------------------------------------------------------
    ## 
    ## Estimated dispersion parameter = 30.45

# Modelling for belief in Chinese meat market origin

## DV Chinese meat market - singular IV models

``` r
sdo_meat <- lm(conspiracy2_sc ~ SDO,
              data = conspiracies)

summ(sdo_meat)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: conspiracy2_sc
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 0.01, p = 0.91
    ## R² = 0.00
    ## Adj. R² = -0.00 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------
    ##                     Est.   S.E.   t val.      p
    ## ----------------- ------ ------ -------- ------
    ## (Intercept)         0.64   0.02    36.94   0.00
    ## SDO                 0.00   0.04     0.11   0.91
    ## -----------------------------------------------

``` r
rwa_meat <- lm(conspiracy2_sc ~ RWA,
              data = conspiracies)

summ(rwa_meat)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: conspiracy2_sc
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 0.91, p = 0.34
    ## R² = 0.00
    ## Adj. R² = -0.00 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------
    ##                     Est.   S.E.   t val.      p
    ## ----------------- ------ ------ -------- ------
    ## (Intercept)         0.62   0.02    25.31   0.00
    ## RWA                 0.04   0.05     0.96   0.34
    ## -----------------------------------------------

## DV Chinese meat market belief - socio-economic variables

``` r
se_meat <- lm(conspiracy2_sc ~ W2_Gender_binary +
               W1_Education_binary +
               W1_Income_2019 +
               age_sc,
              data = conspiracies)

adj_rsq(se_meat)
```

    ## [1] 0.009737859

``` r
AIC(se_meat)
```

    ## [1] 468.9749

## DV Chinese meat market belief - socio-economic variables + political/media

``` r
se_pol_meat <- update(se_meat, ~ . +
                   #political and media variables
                   right +
                   ethno +
                   distrust_science + 
                   red_top_tabloid + 
                   mid_level_news + 
                   elite_news + 
                   W2_INFO_5 + #social media
                   W2_INFO_9) #family and friends
                 
adj_rsq(se_pol_meat)
```

    ## [1] 0.03730106

``` r
AIC(se_pol_meat)
```

    ## [1] 436.1755

## DV Chinese meat market belief - socio-economic variables + political/media + pol-psych

``` r
se_polpsych_meat <- update(se_pol_meat, ~ . +
                        #political-psychology variables
                        SDO +
                        RWA +
                        W2_IOU_Total)

adj_rsq(se_polpsych_meat)
```

    ## [1] 0.04870178

``` r
AIC(se_polpsych_meat)
```

    ## [1] 422.4775

## DV Chinese meat market belief - socio-economic variables + political/media + pol-psych + covid-threat + CRT

``` r
multi_meat <- update(se_polpsych_meat, ~ . +
                  #covid-anxiety
                  threat)

adj_rsq(multi_meat)
```

    ## [1] 0.05758646

``` r
AIC(multi_meat)
```

    ## [1] 410.3382

## DV Chinese meat market belief - full model incl. conspiracy ideation

``` r
full_meat <- update(multi_meat, ~ . +
                  #conspiracies
                  W1_Conspiracy_Total +
                  conspiracy1_sc +
                  conspiracy3_sc)

adj_rsq(full_meat)
```

    ## [1] 0.07016276

``` r
AIC(full_meat)
```

    ## [1] 394.503

``` r
summ(full_meat, vifs = T)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: conspiracy2_sc
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(19,1379) = 6.55, p = 0.00
    ## R² = 0.08
    ## Adj. R² = 0.07 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------------------
    ##                              Est.   S.E.   t val.      p    VIF
    ## ------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  0.42   0.05     8.53   0.00       
    ## W2_Gender_binary2           -0.00   0.02    -0.08   0.94   1.09
    ## W1_Education_binary         -0.04   0.02    -2.24   0.03   1.21
    ## W1_Income_2019               0.06   0.02     2.69   0.01   1.14
    ## age_sc                       0.05   0.04     1.28   0.20   1.40
    ## right                        0.05   0.04     1.07   0.29   1.49
    ## ethno                        0.16   0.03     4.62   0.00   1.32
    ## distrust_science            -0.11   0.03    -3.28   0.00   1.19
    ## red_top_tabloid              0.01   0.02     0.37   0.72   1.12
    ## mid_level_news               0.02   0.02     1.03   0.30   1.15
    ## elite_news                   0.02   0.02     1.05   0.30   1.14
    ## W2_INFO_5                   -0.01   0.03    -0.50   0.62   1.41
    ## W2_INFO_9                   -0.01   0.03    -0.19   0.85   1.27
    ## SDO                          0.00   0.05     0.04   0.97   1.54
    ## RWA                         -0.04   0.05    -0.76   0.45   1.44
    ## W2_IOU_Total                 0.13   0.04     3.20   0.00   1.22
    ## threat                       0.13   0.03     4.22   0.00   1.15
    ## W1_Conspiracy_Total          0.02   0.04     0.57   0.57   1.13
    ## conspiracy1_sc              -0.12   0.03    -4.62   0.00   1.39
    ## conspiracy3_sc               0.01   0.04     0.30   0.76   1.35
    ## ---------------------------------------------------------------

# Summary of final models

``` r
# setting up groups -----------------------------------------
socio_econ <- c("W2_Gender_binary2","W1_Education_binary",
                "W1_Income_2019","age_sc")
names(socio_econ) <- c("Gender","Education","Income","Age")
socio_econ <- socio_econ[sort(names(socio_econ))]

dispositions <- c("RWA","SDO")
names(dispositions) <- c("RWA","SDO")
dispositions <- dispositions[sort(names(dispositions))]

conspiracy <- c("W1_Conspiracy_Total","conspiracy2_sc","conspiracy1_sc",
                "conspiracy3_sc")
names(conspiracy) <- c("Conspiracy ideation","Meat market belief",
                       "Wuhan lab belief","5G belief")
conspiracy <- conspiracy[sort(names(conspiracy))]

inform <- c("W2_INFO_9","elite_news","mid_level_news",
            "red_top_tabloid","W2_INFO_5")
names(inform) <- c("Family and friends","News: elite",
                   "News: mid-level","News: tabloid","Social media")
inform <- inform[sort(names(inform))]

controls <- c("threat","distrust_science",
              "W2_IOU_Total","ethno","right")
names(controls) <- c("COVID-19 anxiety","Distrust scientists",
                     "Uncertainty intolerance",
                     "Ethnocentrism","Left-right scale")
controls <- controls[sort(names(controls))]

full_list <- c(socio_econ, dispositions, conspiracy, inform, controls)
```

``` r
# chinese lab figure 
plot_coefs(
  full_lab,
  coefs = c(dispositions, conspiracy, inform, controls),
  groups = list(
    Dispositions = names(dispositions), 
    Conspiracy = names(conspiracy),
    Information = names(inform),
    Controls = names(controls) 
  ),
  #facet.label.pos = "left",
  facet.cols = 2
) +
  labs(x = "Coefficient estimate: Wuhan laboratory belief",
       caption = "Note: OLS estimates with 95% confidence intervals, numerical predictors scaled 0-1") +
  theme(strip.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(hjust = 1),
        plot.caption = element_text(hjust = 0))
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-64-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_lab)
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-64-2.png)<!-- -->

``` r
# 5G belief - full set of variables, raw data is DV
plot_coefs(
  full_5g,
  coefs = c(dispositions, conspiracy, inform, controls),
  groups = list(
    Dispositions = names(dispositions), 
    Conspiracy = names(conspiracy),
    Information = names(inform),
    Controls = names(controls) 
  ),
  #facet.label.pos = "left",
  facet.cols = 2
) +
  labs(x = "Coefficient estimate: 5G belief",
       caption = "Note: OLS estimates with 95% confidence intervals, numerical predictors scaled 0-1") +
  theme(strip.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(hjust = 1),
        plot.caption = element_text(hjust = 0))
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_5g)
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-65-2.png)<!-- -->

``` r
# Chinese meat market model - full set of variables
plot_coefs(
  full_meat,
  coefs = c(dispositions, conspiracy, inform, controls),
  groups = list(
    Dispositions = names(dispositions), 
    Conspiracy = names(conspiracy),
    Information = names(inform),
    Controls = names(controls) 
  ),
  #facet.label.pos = "left",
  facet.cols = 2
) +
  labs(x = "Coefficient estimate: Meat market belief",
       caption = "Note: OLS estimates with 95% confidence intervals, numerical predictors scaled 0-1") +
  theme(strip.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(hjust = 1),
        plot.caption = element_text(hjust = 0))
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_meat)
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-66-2.png)<!-- -->

## Combined plot of models

``` r
model_vars <- c(dispositions,conspiracy)
model_vars <- model_vars[sort(names(model_vars))]

plot_coefs(
  full_lab,full_5g,full_meat,
  model.names = c("Wuhan lab","5G","Meat market"),
  coefs = model_vars,
  groups = list(
    Dispositions = names(dispositions),
    Conspiracy = names(conspiracy)
    ),
  facet.label.pos = "left" 
  ) +
  labs(
    x = "Coefficient estimate: OLS model comparison",
    caption = "Note: OLS estimates with 95% confidence intervals, numerical predictors scaled 0-1"
  ) + 
  theme(legend.position = "top",
        legend.margin=margin(t = 0, b = 0, unit='cm'),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(hjust = 1),
        plot.caption = element_text(hjust = 0))
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

``` r
control_vars <- c(controls,inform)
control_vars <- control_vars[sort(names(control_vars))]

plot_coefs(
  full_lab,full_5g,full_meat,
  model.names = c("Wuhan lab","5G","Meat market"),
  coefs = control_vars,
  groups = list(
    Information = names(inform),
    `Pol-psych controls` = names(controls)
    ),
  facet.label.pos = "left" 
  ) +
  labs(
    x = "Coefficient estimate: OLS model comparison",
    caption = "Note: OLS estimates with 95% confidence intervals, numerical predictors scaled 0-1"
  ) + 
  theme(legend.position = "top",
        legend.margin=margin(t = 0, b = 0, unit='cm'),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(hjust = 1),
        plot.caption = element_text(hjust = 0))
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-68-1.png)<!-- -->

``` r
plot_coefs(
  full_5g_ihs,full_5g_poiss,
  model.names = c("5G IHS transformed","5G poisson"),
  coefs = model_vars,
  groups = list(
    Conspiracy = names(conspiracy),
    Dispositions = names(dispositions)
    ),
  facet.label.pos = "left" 
  ) +
  labs(
    x = "Coefficient estimate",
    caption = "Note: Comparison of supplementary models. IHS transformed model estimated using OLS.\nPoisson regression estimated using maximum likelihood method, with overdispersion parameter (=30.45).\n95% confidence intervals presented. Numerical predictors scaled 0-1."
  ) + 
  theme(legend.position = "top",
        legend.margin=margin(t = 0, b = 0, unit='cm'),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(hjust = 1),
        plot.caption = element_text(hjust = 0))
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-69-1.png)<!-- -->

## Conspiracies and social distancing

``` r
# loading package for multi nom
pacman::p_load(nnet)
```

``` r
## making a dataset of variables included in the models above
## plus DV's for social distancing and vaccination

vars <- model.matrix(full_lab)[,-1] %>% as.data.frame() %>% names()
vars <- c(vars,"w2_conspiracy3_ihs","W2_Conspiracy_Theory1",
          "W2_Conspiracy_Theory2","W2_Conspiracy_Theory3",
          "conspiracy1_sc","pid")
vars[1] <- str_sub(vars[1],1,str_length(vars[1])-1)
#vars[2] <- str_sub(vars[2],1,str_length(vars[2])-1)

conspiracies2 <- conspiracies %>% 
  dplyr::select(one_of(vars)) %>% 
  na.omit()

conspiracies2 <- merge(
  conspiracies2,
  conspiracies %>% dplyr::select(pid,W2_SocialDistance10,
                                 W2_SocialDistance11,
                                 W2_SocialDistance12,
                                 W2_SocialDistance14,
                                 W2_C19_Vax_Self,
                                 W2_C19_Vax_Child),
  by = "pid", all.x = TRUE
)
```

``` r
# social distance scale
sd_keys <- list(sd = cs(W2_SocialDistance10,
                        W2_SocialDistance11,
                        W2_SocialDistance12,
                        W2_SocialDistance14))

sd_test <- scoreItems(sd_keys, conspiracies2, min = 1, max = 5)
head(sd_test$scores)
```

    ##        sd
    ## [1,] 5.00
    ## [2,] 3.25
    ## [3,] 4.00
    ## [4,] 4.75
    ## [5,] 3.00
    ## [6,] 3.75

``` r
summary(sd_test$alpha)  # Scale alpha
```

    ##        sd        
    ##  Min.   :0.8735  
    ##  1st Qu.:0.8735  
    ##  Median :0.8735  
    ##  Mean   :0.8735  
    ##  3rd Qu.:0.8735  
    ##  Max.   :0.8735

``` r
conspiracies2$social_distance <- rescale01(sd_test$scores, na.rm = TRUE)
conspiracies2$social_distance <- c(conspiracies2$social_distance)  # Ensure variable is numeric and not matrix class

describe(conspiracies2$social_distance)
```

    ##    vars    n mean   sd median trimmed  mad min max range  skew kurtosis se
    ## X1    1 1399 0.82 0.19   0.88    0.85 0.19   0   1     1 -1.05     1.17  0

``` r
ggplot(conspiracies2, aes(x = social_distance)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1, colour = "black", fill = "lightblue") +
  geom_density()
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-72-1.png)<!-- -->

``` r
dist_full <- lm(social_distance ~ 
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  right +
                  ethno +
                  distrust_science + 
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news +
                  W2_INFO_5 + #social media
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  RWA +
                  W2_IOU_Total +
                  
                  #covid-anxiety
                  threat +
                 
                  #conspiracies
                  W1_Conspiracy_Total +
                  conspiracy1_sc +
                  conspiracy2_sc +
                  conspiracy3_sc,
                data = conspiracies2)
summ(dist_full, vifs = T)
```

    ## MODEL INFO:
    ## Observations: 1399
    ## Dependent Variable: social_distance
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(20,1378) = 24.97, p = 0.00
    ## R² = 0.27
    ## Adj. R² = 0.26 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------------------
    ##                              Est.   S.E.   t val.      p    VIF
    ## ------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  0.66   0.03    22.24   0.00       
    ## W2_Gender_binary2            0.04   0.01     4.02   0.00   1.09
    ## W1_Education_binary          0.00   0.01     0.42   0.68   1.22
    ## W1_Income_2019               0.03   0.01     1.99   0.05   1.15
    ## age_sc                       0.17   0.02     7.26   0.00   1.41
    ## right                        0.02   0.03     0.72   0.47   1.49
    ## ethno                        0.03   0.02     1.37   0.17   1.34
    ## distrust_science            -0.15   0.02    -7.98   0.00   1.20
    ## red_top_tabloid             -0.01   0.01    -0.83   0.40   1.12
    ## mid_level_news               0.00   0.01     0.14   0.89   1.15
    ## elite_news                   0.01   0.01     1.20   0.23   1.14
    ## W2_INFO_5                   -0.01   0.02    -0.92   0.36   1.41
    ## W2_INFO_9                    0.06   0.02     3.37   0.00   1.27
    ## SDO                         -0.21   0.03    -7.23   0.00   1.54
    ## RWA                          0.12   0.03     3.96   0.00   1.44
    ## W2_IOU_Total                 0.02   0.02     0.67   0.51   1.23
    ## threat                       0.02   0.02     1.37   0.17   1.17
    ## W1_Conspiracy_Total          0.05   0.02     1.99   0.05   1.13
    ## conspiracy1_sc               0.01   0.02     0.46   0.64   1.41
    ## conspiracy2_sc               0.07   0.02     4.33   0.00   1.09
    ## conspiracy3_sc              -0.14   0.02    -6.21   0.00   1.35
    ## ---------------------------------------------------------------

``` r
# social distance model plot
dist_controls <- c("age_sc","W2_Gender_binary2","distrust_science",
                   "W2_INFO_9")
names(dist_controls) <- c("Age","Gender","Distrust scientists",
                          "Family and friends")
dist_controls <- dist_controls[sort(names(dist_controls))]

plot_coefs(
  dist_full,
  coefs = c(dist_controls,dispositions,conspiracy),
  groups = list(
    Controls = names(dist_controls), 
    Dispositions = names(dispositions),
    Conspiracy = names(conspiracy)
  ),
  facet.label.pos = "left"
) +
  labs(x = "Coefficient estimate: Social distancing",
       caption = "Note: OLS estimates with 95% confidence intervals, numerical predictors scaled 0-1") +
  theme(strip.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(hjust = 1),
        plot.caption = element_text(hjust = 0))
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-74-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(dist_full)
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-74-2.png)<!-- -->

## Multinomial model for vaccine acceptance

``` r
count(conspiracies2,sum()) %>%  mutate(`%` = n / sum(n))
```

    ##   sum()    n %
    ## 1     0 1399 1

``` r
# multinomial model
vax_full <- multinom(W2_C19_Vax_Self ~ 
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  right +
                  ethno +
                  distrust_science + 
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news +
                  W2_INFO_5 + #social media
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  RWA +
                  W2_IOU_Total +
                  
                  #covid-anxiety
                  threat +
                 
                  #conspiracies
                  W1_Conspiracy_Total +
                  conspiracy1_sc +
                  conspiracy2_sc +
                  conspiracy3_sc,
                data = conspiracies2)
```

    ## # weights:  66 (42 variable)
    ## initial  value 1527.071081 
    ## iter  10 value 998.446091
    ## iter  20 value 985.751324
    ## iter  30 value 985.330108
    ## final  value 985.328575 
    ## converged

``` r
# functions to inspect multinom
z_score <- function(model){
  z <- summary(model)$coefficients/summary(model)$standard.errors
  return(z)
}

p_value <- function(model){
  z <- z_score(model)
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  return(p)
}
```

``` r
p_value(vax_full)
```

    ##   (Intercept) W2_Gender_binary2 W1_Education_binary W1_Income_2019       age_sc
    ## 2   0.1141417        0.34144364           0.5272841   0.2273037510 3.414586e-06
    ## 3   0.8325006        0.07229696           0.5372921   0.0003227875 1.722345e-03
    ##       right      ethno distrust_science red_top_tabloid mid_level_news
    ## 2 0.7964977 0.09957509     4.405816e-09       0.3502299      0.4410809
    ## 3 0.1139373 0.07268913     1.214443e-03       0.4188975      0.5659722
    ##   elite_news W2_INFO_5 W2_INFO_9       SDO       RWA W2_IOU_Total       threat
    ## 2  0.7776279 0.3498732 0.4836841 0.4539986 0.1881196    0.4729139 5.451681e-05
    ## 3  0.1735154 0.4261363 0.8848474 0.3215399 0.1577487    0.2856241 3.290162e-06
    ##   W1_Conspiracy_Total conspiracy1_sc conspiracy2_sc conspiracy3_sc
    ## 2          0.77077324     0.01285167    0.009089015   8.064897e-07
    ## 3          0.05695333     0.68879217    0.049152755   9.721378e-01

``` r
# table for plot of unstandardised coefficients
vax_vars <- c("distrust_science","threat","age_sc","W1_Income_2019",
               "RWA","SDO","W1_Conspiracy_Total","conspiracy1_sc",
               "conspiracy2_sc","conspiracy3_sc")

vax_plots <- full_list[full_list %in% vax_vars]
vax_plots <- vax_plots[sort(names(vax_plots))]

tidies <- tidy(vax_full) %>%
  filter(term %in% vax_plots) %>% 
  mutate(
    odds_ratio = exp(estimate),
    conf.low = estimate - std.error * qnorm(0.975),
    conf.high = estimate + std.error * qnorm(0.975),
    conf.low.exp = exp(estimate - std.error * qnorm(0.975)),
    conf.high.exp = exp(estimate + std.error * qnorm(0.975)),
    y.level = fct_recode(as.factor(y.level),
                         "No" = "2",
                         "Maybe" = "3"),
    term = fct_rev(fct_drop(fct_relevel(term, vax_plots))),
    group_facet = ifelse(term %in% dispositions, 
                         "Dispositions",
                         ifelse(term %in% conspiracy, "Conspiracy",
                                       "Controls")),
    term = fct_recode(term,
                      "Age" = "age_sc",
                      "Income" = "W1_Income_2019",
                      "COVID-19 anxiety" = "threat",
                      "Distrust scientists" = "distrust_science",
                      "RWA" = "RWA",
                      "SDO" = "SDO",
                      "Conspiracy ideation" = "W1_Conspiracy_Total",
                      "Meat market belief" = "conspiracy2_sc",
                      "Wuhan lab belief" = "conspiracy1_sc",
                      "5G belief" = "conspiracy3_sc")
  ) %>%
  rename(Level = y.level)  

# plotting beta coefficients
ggplot(data = tidies, 
       aes(y = term, 
           x = estimate, xmin = conf.low,
           xmax = conf.high, colour = Level)) + 
  geom_vline(xintercept = 0, linetype = 2, size = .25) +
  ggstance::geom_linerangeh(
    aes(y = term, xmin = conf.low,
        xmax = conf.high, colour = Level),
    position = ggstance::position_dodgev(height = 0.62), size = 0.8) +
  geom_point(
    aes(y = term, 
        x = estimate, colour = Level, shape = Level),
    position = ggstance::position_dodgev(height = 0.62),
    fill = "white", size = 3, stroke = 1, show.legend = TRUE) +
  scale_colour_manual(values = get_colors("CUD Bright",num.colors = 2)) +
  theme_nice(legend.pos = "right") +
  scale_shape_manual(values = c(21,22)) +
  drop_y_gridlines() +
  facet_wrap(~group_facet, ncol = 1, scales = "free_y",
             strip.position = "left") +
  theme(axis.title.y = element_blank(),
        legend.margin=margin(t = 0, b = 0, unit='cm'),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 10,
                                   hjust = 1),
        panel.grid.major.x = element_line(linetype = "solid"),
        strip.text.x = element_text(size = 8),
        legend.position = "top",
        plot.caption = element_text(hjust = 0)) +
  labs(
    x = "Coefficient estimate: Vaccine acceptance",
    caption = "Note: Multinomial logit estimated using maximum likelihood.\nUnstardised coefficients and 95% confidence intervals presented. Numerical predictors scaled 0-1."
  ) 
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-78-1.png)<!-- -->

Below is a plot of the average marginal effects for key variables.

``` r
pacman::p_load(margins, ggstance)

# average marginal effect for "No", with bootstrap for variance
set.seed(123)
margin_no <- margins_summary(
  vax_full, category = "2",
  vce = "bootstrap"
)

# average marginal effect for "Maybe", with bootstrap for variance
set.seed(123)
margin_maybe <- margins_summary(
  vax_full, category = "3",
  vce = "bootstrap"
)
```

``` r
# plotting average marginal effect
rbind(
  margin_no %>% as_tibble() %>% mutate(level = "No"),
  margin_maybe %>% as_tibble() %>% mutate(level = "Maybe")
) %>% 
  rename(term = factor) %>% 
  filter(term %in% vax_plots) %>%
  mutate(
    term = fct_rev(fct_drop(fct_relevel(term, vax_plots))),
    group_facet = ifelse(term %in% dispositions, 
                         "Dispositions",
                         ifelse(term %in% conspiracy, "Conspiracy",
                                       "Controls")),
    term = fct_recode(term,
                      "Age" = "age_sc",
                      "Income" = "W1_Income_2019",
                      "COVID-19 anxiety" = "threat",
                      "Distrust scientists" = "distrust_science",
                      "RWA" = "RWA",
                      "SDO" = "SDO",
                      "Conspiracy ideation" = "W1_Conspiracy_Total",
                      "Meat market belief" = "conspiracy2_sc",
                      "Wuhan lab belief" = "conspiracy1_sc",
                      "5G belief" = "conspiracy3_sc")
  ) %>% 
  ggplot(aes(y = term, 
           x = AME, xmin = lower,
           xmax = upper, colour = level)) + 
  geom_vline(xintercept = 0, linetype = 2, size = .25) +
  ggstance::geom_linerangeh(
    aes(y = term, xmin = lower,
        xmax = upper, colour = level),
    position = ggstance::position_dodgev(height = 0.62), size = 0.8) +
  geom_point(
    aes(y = term, 
        x = AME, colour = level, shape = level),
    position = ggstance::position_dodgev(height = 0.62),
    fill = "white", size = 3, stroke = 1, show.legend = TRUE) +
  scale_colour_manual(values = get_colors("CUD Bright",num.colors = 2)) +
  theme_nice(legend.pos = "right") +
  scale_shape_manual(values = c(21,22)) +
  drop_y_gridlines() +
  facet_wrap(~group_facet, ncol = 1, scales = "free_y",
             strip.position = "left") +
  theme(axis.title.y = element_blank(),
        legend.margin=margin(t = 0, b = 0, unit='cm'),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 10,
                                   hjust = 1),
        panel.grid.major.x = element_line(linetype = "solid"),
        strip.text.x = element_text(size = 8),
        legend.position = "top",
        plot.caption = element_text(hjust = 0)) +
  labs(
    x = "Average marginal effect: Vaccine acceptance",
    colour = "Level",
    shape = "Level",
    caption = "Note: Multinomial logit estimated using maximum likelihood method.  Bootstrapped 95% confidence intervals.\nNumerical predictors scaled 0-1."
  )
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-80-1.png)<!-- -->

## Table summarising variables

``` r
all_vars <- model.matrix(dist_full)[,-1] %>% as_tibble()
var_summaries <- tibble(
  variable = names(all_vars),
  mean = all_vars %>% map_dbl(mean, na.rm = TRUE),
  sd = all_vars %>% map_dbl(sd, na.rm = TRUE)
)

#write.csv(var_summaries,"variable_summaries_randr.csv")
```

## Correlation Matrix

``` r
fuller_list <- c(
  full_list %>%
    str_replace("W2_Gender_binary2","W2_Gender_binary"),
  "social_distance","W2_C19_Vax_Self")

names(fuller_list) <- c(names(full_list),
                        "Social distancing motivation",
                        "Vaccination attitudes")

fuller_list <- fuller_list[sort(names(fuller_list))]

cor_df <- conspiracies2 %>% 
  dplyr::select(one_of(fuller_list)) %>% 
  na.omit()

make_dbl <- c("W2_C19_Vax_Self","W2_Gender_binary")

cor_df[make_dbl] <- cor_df[make_dbl] %>% 
  map_df(as.numeric)

names(cor_df) <- names(fuller_list)

cor_df <- cor_df %>% dplyr::select(one_of(sort(names(fuller_list))))
```

``` r
# function to get lower triangle in correlation matrix
get_lower_tri <- function(df){
  cormat <- cor(df)
  cormat[upper.tri(cormat)]<- NA
  return(cormat)
}

lower_tri <- get_lower_tri(cor_df)

var_1 <- sort(names(cor_df))

# plotting correlation matrix
cbind(
  lower_tri,
  var_1
) %>%
  as_tibble() %>%
  gather(var_2,correlation,`5G belief`:`Wuhan lab belief`, na.rm = T) %>%
  mutate(correlation = parse_number(correlation)) %>%
  arrange(var_1) %>%
  ggplot(aes(x = var_1, y = var_2, fill = correlation)) +
  geom_tile(colour = "white") +
  scale_fill_gradientn(breaks = c(-0.2,0,0.2,0.5),
                       colours = c("red","white",
                                   "lightblue",
                                   "dodgerblue4",
                                   "midnightblue")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = NULL, y = NULL)
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-83-1.png)<!-- -->

``` r
# table for correlation matrix
pacman::p_load(stargazer, knitr)

lower_tri <- lower_tri %>% as_tibble() %>% 
  mutate(` ` = names(cor_df)) %>% 
  dplyr::select(` `, everything())

kable(lower_tri,
      caption = "Correlation Matrix")
```

|                              |   5G belief |         Age | Conspiracy ideation | COVID-19 anxiety | Distrust scientists |   Education | Ethnocentrism | Family and friends |      Gender |      Income | Left-right scale | Meat market belief | News: elite | News: mid-level | News: tabloid |         RWA |         SDO | Social distancing motivation | Social media | Uncertainty intolerance | Vaccination attitudes | Wuhan lab belief |
| :--------------------------- | ----------: | ----------: | ------------------: | ---------------: | ------------------: | ----------: | ------------: | -----------------: | ----------: | ----------: | ---------------: | -----------------: | ----------: | --------------: | ------------: | ----------: | ----------: | ---------------------------: | -----------: | ----------------------: | --------------------: | ---------------: |
| 5G belief                    |   1.0000000 |          NA |                  NA |               NA |                  NA |          NA |            NA |                 NA |          NA |          NA |               NA |                 NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Age                          | \-0.1968411 |   1.0000000 |                  NA |               NA |                  NA |          NA |            NA |                 NA |          NA |          NA |               NA |                 NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Conspiracy ideation          |   0.1303185 | \-0.0484166 |           1.0000000 |               NA |                  NA |          NA |            NA |                 NA |          NA |          NA |               NA |                 NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| COVID-19 anxiety             |   0.0390187 |   0.0556909 |           0.0871699 |        1.0000000 |                  NA |          NA |            NA |                 NA |          NA |          NA |               NA |                 NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Distrust scientists          |   0.2993469 | \-0.0757459 |           0.1710566 |      \-0.0424669 |           1.0000000 |          NA |            NA |                 NA |          NA |          NA |               NA |                 NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Education                    | \-0.0744352 | \-0.1655707 |         \-0.0865678 |      \-0.0112353 |         \-0.0940102 |   1.0000000 |            NA |                 NA |          NA |          NA |               NA |                 NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Ethnocentrism                |   0.1019370 |   0.1343591 |           0.0163442 |        0.0570581 |         \-0.0325778 | \-0.1508601 |     1.0000000 |                 NA |          NA |          NA |               NA |                 NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Family and friends           |   0.1853119 | \-0.1909207 |           0.0660756 |        0.1725282 |         \-0.0041932 |   0.0102841 |     0.0708626 |          1.0000000 |          NA |          NA |               NA |                 NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Gender                       |   0.0151136 | \-0.1457747 |           0.0701525 |        0.0476996 |           0.0262593 | \-0.0129786 |   \-0.0515851 |          0.0723318 |   1.0000000 |          NA |               NA |                 NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Income                       | \-0.1120980 |   0.0075214 |         \-0.0911093 |      \-0.0210025 |         \-0.0848267 |   0.2327198 |   \-0.0182942 |        \-0.0302430 | \-0.0871241 |   1.0000000 |               NA |                 NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Left-right scale             |   0.0971868 |   0.1338143 |         \-0.0233279 |      \-0.0101604 |           0.0588338 | \-0.1027946 |     0.3784391 |        \-0.0035331 | \-0.0639928 |   0.1126077 |        1.0000000 |                 NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Meat market belief           | \-0.0411027 |   0.0694473 |         \-0.0126038 |        0.1327156 |         \-0.1260939 | \-0.0335189 |     0.1435770 |          0.0064934 | \-0.0235138 |   0.0782161 |        0.0726111 |          1.0000000 |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| News: elite                  | \-0.0310280 | \-0.0480979 |         \-0.0680059 |        0.0481917 |         \-0.0864244 |   0.2576219 |   \-0.1238120 |          0.0740565 | \-0.0587591 |   0.1434613 |      \-0.1095518 |          0.0351403 |   1.0000000 |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| News: mid-level              |   0.0984832 |   0.0478379 |           0.0346567 |        0.0629015 |           0.0885137 | \-0.0978202 |     0.1989451 |          0.1155816 |   0.0177856 |   0.0045211 |        0.2395652 |          0.0381964 | \-0.0424565 |       1.0000000 |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| News: tabloid                |   0.2088942 | \-0.1203675 |           0.0598785 |        0.0640629 |           0.0857823 | \-0.1236667 |     0.0894869 |          0.1015062 | \-0.0576030 | \-0.1110322 |        0.0227170 |          0.0051788 | \-0.0153704 |       0.1212012 |     1.0000000 |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| RWA                          |   0.0402643 |   0.1482799 |           0.0989850 |        0.0842022 |           0.0673202 | \-0.2067672 |     0.3220561 |          0.0018120 |   0.0913433 | \-0.0408217 |        0.3774242 |          0.0240614 | \-0.2320842 |       0.2118870 |     0.0507231 |   1.0000000 |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| SDO                          |   0.2330242 | \-0.0403077 |         \-0.0599814 |      \-0.0889019 |           0.1762634 | \-0.0724244 |     0.2919805 |        \-0.0529470 | \-0.1015043 |   0.0533128 |        0.4436087 |          0.0057042 | \-0.1508152 |       0.1472083 |     0.0199403 |   0.3580471 |   1.0000000 |                           NA |           NA |                      NA |                    NA |               NA |
| Social distancing motivation | \-0.2915813 |   0.2587121 |           0.0141793 |        0.1170175 |         \-0.3066986 |   0.0015106 |     0.0500811 |          0.0441123 |   0.0905687 |   0.0711845 |      \-0.0145833 |          0.1674625 |   0.0521132 |       0.0012076 |   \-0.0888182 |   0.0750816 | \-0.2510765 |                    1.0000000 |           NA |                      NA |                    NA |               NA |
| Social media                 |   0.2398268 | \-0.3936570 |           0.1029357 |        0.1055366 |           0.0407408 |   0.0507616 |     0.0055942 |          0.3875101 |   0.0856987 | \-0.0620442 |      \-0.0228397 |        \-0.0243246 |   0.0402119 |       0.0448742 |     0.1353432 | \-0.0225222 |   0.0262917 |                  \-0.1108653 |    1.0000000 |                      NA |                    NA |               NA |
| Uncertainty intolerance      |   0.1340517 | \-0.2640647 |           0.1557813 |        0.2404121 |           0.0340551 |   0.0361928 |     0.0415397 |          0.1588673 |   0.0846793 | \-0.1146742 |      \-0.0131687 |          0.0879339 |   0.0095418 |       0.0461810 |     0.0852039 |   0.0294074 |   0.0270955 |                  \-0.0314238 |    0.2592348 |               1.0000000 |                    NA |               NA |
| Vaccination attitudes        |   0.0959488 | \-0.1674974 |           0.0909080 |      \-0.1384933 |           0.1644502 | \-0.0066770 |   \-0.0735038 |          0.0147415 |   0.0894715 | \-0.1345180 |      \-0.0540847 |        \-0.1168862 | \-0.0683376 |       0.0087821 |     0.0098751 |   0.0227439 |   0.0488148 |                  \-0.1237813 |    0.0861778 |               0.0583917 |             1.0000000 |               NA |
| Wuhan lab belief             |   0.3299842 | \-0.0554126 |           0.2340217 |        0.1356206 |           0.2220164 | \-0.1834670 |     0.2083188 |          0.1909797 |   0.0617540 | \-0.1306903 |        0.1634907 |        \-0.0966866 | \-0.1116579 |       0.2502842 |     0.1763378 |   0.2402447 |   0.2050590 |                  \-0.0908347 |    0.1695001 |               0.1006033 |             0.0727313 |                1 |

Correlation Matrix

``` r
#write.csv(lower_tri,"correlation_matrix_randr.csv")
```

## Comparison against British Election Survey (BES) benchmarks

Demographic characteristics are compared to BES Wave 19, conducted in
December 2019, where n = 32,177 for BES.

BES W19 data downloaded from the following
[link](https://www.britishelectionstudy.com/data-objects/panel-study-data/).

Demographics are only displayed for n=1,406 study participants who
answered questions on conspiracy theories and are included in the
regression analyses.

``` r
# reading in BES data
bes <- read_sav("BES data/BES2019_W19_v1.0-2.sav")
```

Our sample has a slightly higher number of male respondents.

``` r
gender_df <- bes %>% 
  mutate(
    gender = to_factor(gender)
  ) %>% 
  count(gender) %>% 
  mutate(`BES W19 Proportion` = n / sum(n)) %>% 
  rename(`BES W19 n` = n)

gender_df_full <- conspiracies %>% 
  mutate(
    gender = fct_recode(
      W2_Gender_binary,
      "Male" = "1",
      "Female" = "2") %>% fct_explicit_na("Other/Prefer not to say")
  ) %>%
  count(gender) %>% 
  mutate(Proportion = n/sum(n)) %>% 
  left_join(gender_df, by = "gender")
gender_df_full
```

    ## # A tibble: 3 x 5
    ##   gender                      n Proportion `BES W19 n` `BES W19 Proportion`
    ##   <fct>                   <int>      <dbl>       <int>                <dbl>
    ## 1 Male                      727    0.517         15049                0.468
    ## 2 Female                    676    0.481         17128                0.532
    ## 3 Other/Prefer not to say     3    0.00213          NA               NA

Our sample has slightly more participants who identify as an ethnicity
other than White. Note: the wording and classification of ethnicity
given below uses the same [conventions as
gov.uk](https://www.ethnicity-facts-figures.service.gov.uk/style-guide/writing-about-ethnicity).

``` r
ethnicity_df <- bes %>% 
  mutate(
    ethnicity = fct_collapse(
      as.factor(p_ethnicity),
      "White" = c("1","2"),
      "ethnic minorities (excluding White minorities)" = 
        c("3","4","5","6","7","8","9","10","11","12","13","14","15"),
      "Prefer not to say" = "16"
    )
  ) %>% 
  count(ethnicity) %>% 
  mutate(`BES W19 Proportion` = n/sum(n)) %>% 
  rename(`BES W19 n` = n)
  
ethnicity_df_full <- conspiracies %>% 
  mutate(
    ethnicity = fct_collapse(
      W1_Ethnicity,
      "White" = c("1","2"),
      "ethnic minorities (excluding White minorities)" = 
        c("3","4","5","6","7","8","9","10","11")
    )
  ) %>% 
  count(ethnicity) %>% 
  mutate(Proportion = n/sum(n)) %>% 
  right_join(ethnicity_df, by = "ethnicity")
ethnicity_df_full
```

    ## # A tibble: 4 x 5
    ##   ethnicity                          n Proportion `BES W19 n` `BES W19 Proporti~
    ##   <fct>                          <int>      <dbl>       <int>              <dbl>
    ## 1 White                           1307     0.930        30308            0.942  
    ## 2 ethnic minorities (excluding ~    99     0.0704        1408            0.0438 
    ## 3 Prefer not to say                 NA    NA              314            0.00976
    ## 4 <NA>                              NA    NA              147            0.00457

Our sample is a bit younger than BES, with more respondents under 34 and
less respondents over 55.

``` r
age_df <- bes %>% 
  mutate(
    age_bands = cut(
      age,
      breaks = c(-Inf,17,24,34,44,54,64,74,Inf),
      labels = c("Under 18","18-24","25-34","35-44","45-54",
                 "55-64","65-74","75+")
    )
  ) %>%
  count(age_bands) %>%
  mutate(`BES W19 Proportion` = n / sum(n)) %>%
  rename(`BES W19 n` = n)

age_df_full <- conspiracies %>% 
  mutate(
    age_bands = cut(
      W2_Age_year,
      breaks = c(-Inf,17,24,34,44,54,64,74,Inf),
      labels = c("Under 18","18-24","25-34","35-44","45-54",
                 "55-64","65-74","75+")
    )
  ) %>%
  count(age_bands) %>%
  mutate(Proportion = n / sum(n)) %>% 
  left_join(age_df, by = "age_bands")
age_df_full
```

    ## # A tibble: 7 x 5
    ##   age_bands     n Proportion `BES W19 n` `BES W19 Proportion`
    ##   <fct>     <int>      <dbl>       <int>                <dbl>
    ## 1 18-24        78     0.0555        1364               0.0424
    ## 2 25-34       213     0.151         2856               0.0888
    ## 3 35-44       244     0.174         4184               0.130 
    ## 4 45-54       307     0.218         5462               0.170 
    ## 5 55-64       311     0.221         7420               0.231 
    ## 6 65-74       215     0.153         8394               0.261 
    ## 7 75+          38     0.0270        2497               0.0776

Our sample has more people with a degree (undergraduate and/or
postgraduate).

``` r
education_df <- bes %>% 
  mutate(
    Education = fct_collapse(
      as.factor(p_education),
      "Degree education" = c("16","17")) %>% 
      fct_lump_n(1) %>% 
      fct_recode(
        "Other qualification or no qualifications" = "Other"
        )
    ) %>% 
  count(Education) %>% 
  mutate(`BES W19 Proportion` = n/sum(n)) %>% 
  rename(`BES W19 n` = n)

education_df_full <- conspiracies %>% 
  mutate(
    Education = fct_recode(
      as.factor(W1_Education_binary),
      "Degree education" = "1",
      "Other qualification or no qualifications" = "0") 
    ) %>% 
  count(Education) %>% 
  mutate(Proportion = n/sum(n)) %>% 
  left_join(education_df, by = "Education")
education_df_full
```

    ## # A tibble: 2 x 5
    ##   Education                          n Proportion `BES W19 n` `BES W19 Proporti~
    ##   <fct>                          <int>      <dbl>       <int>              <dbl>
    ## 1 Other qualification or no qua~   782      0.556       22020              0.684
    ## 2 Degree education                 624      0.444       10157              0.316

The factor levels we use for gross household are not directly comparable
to those used in BES W19. However, the table below uses the best
approximation for comparisons and suggests our sample has slightly more
respondents on low incomes, but also more respondents on high incomes.

``` r
income_df <- bes %>% 
  mutate(
    gross_hh_income = fct_collapse(
      as.factor(p_gross_household),
      "£0 - £14,999" = c("1","2","3"),
      "£15,000 - £24,999" = c("4","5"),
      "£25,000 - £39,999" = c("6","7","8"),
      "£40,000 - £59,999" = c("9","10","11"),
      "£60,000 or more" = c("12","13","14","15"),
      "Other" = c("16","17")
    )
  ) %>% 
  filter(gross_hh_income != "Other") %>% 
  count(gross_hh_income) %>% 
  mutate(`BES W19 Proportion` = n/sum(n)) %>% 
  rename(`BES W19 n` = n)

income_df_full <- conspiracies %>% 
  mutate(
    gross_hh_income = fct_recode(
      as.factor(W1_Income_2019),
      "£0 - 15,490" = "0",
      "£15,491 - £25,340" = "0.25",
      "£25,341 - £38,740" = "0.5",
      "£38,741 - £57,930" = "0.75",
      "£57,931 or more" = "1"
    )
  ) %>% 
  count(gross_hh_income) %>% 
  mutate(Proportion = n/sum(n)) %>% 
  full_join(income_df, by = "gross_hh_income")
income_df_full
```

    ## # A tibble: 10 x 5
    ##    gross_hh_income       n Proportion `BES W19 n` `BES W19 Proportion`
    ##    <fct>             <int>      <dbl>       <int>                <dbl>
    ##  1 £0 - 15,490         279      0.198          NA               NA    
    ##  2 £15,491 - £25,340   252      0.179          NA               NA    
    ##  3 £25,341 - £38,740   259      0.184          NA               NA    
    ##  4 £38,741 - £57,930   311      0.221          NA               NA    
    ##  5 £57,931 or more     305      0.217          NA               NA    
    ##  6 £0 - £14,999         NA     NA            4402                0.184
    ##  7 £15,000 - £24,999    NA     NA            4920                0.206
    ##  8 £25,000 - £39,999    NA     NA            6317                0.264
    ##  9 £40,000 - £59,999    NA     NA            4456                0.186
    ## 10 £60,000 or more      NA     NA            3799                0.159

``` r
# combined table of all demographic comparisons
demographics <- rbind(
  age_df_full %>% rename(Category = age_bands) %>% 
    mutate(Variable = "Age"),
  gender_df_full %>% rename(Category = gender) %>% 
    mutate(Variable = "Gender"), 
  ethnicity_df_full %>% rename(Category = ethnicity) %>% 
    mutate(Variable = "Ethnicity"),
  education_df_full %>% rename(Category = Education) %>% 
    mutate(Variable = "Education"),
  income_df_full %>% rename(Category = gross_hh_income) %>% 
    mutate(Variable = "Gross household income")
) %>% 
  dplyr::select(Variable,everything())

kable(demographics)
```

| Variable               | Category                                       |    n | Proportion | BES W19 n | BES W19 Proportion |
| :--------------------- | :--------------------------------------------- | ---: | ---------: | --------: | -----------------: |
| Age                    | 18-24                                          |   78 |  0.0554765 |      1364 |          0.0423905 |
| Age                    | 25-34                                          |  213 |  0.1514936 |      2856 |          0.0887591 |
| Age                    | 35-44                                          |  244 |  0.1735420 |      4184 |          0.1300308 |
| Age                    | 45-54                                          |  307 |  0.2183499 |      5462 |          0.1697486 |
| Age                    | 55-64                                          |  311 |  0.2211949 |      7420 |          0.2305995 |
| Age                    | 65-74                                          |  215 |  0.1529161 |      8394 |          0.2608696 |
| Age                    | 75+                                            |   38 |  0.0270270 |      2497 |          0.0776020 |
| Gender                 | Male                                           |  727 |  0.5170697 |     15049 |          0.4676943 |
| Gender                 | Female                                         |  676 |  0.4807966 |     17128 |          0.5323057 |
| Gender                 | Other/Prefer not to say                        |    3 |  0.0021337 |        NA |                 NA |
| Ethnicity              | White                                          | 1307 |  0.9295875 |     30308 |          0.9419150 |
| Ethnicity              | ethnic minorities (excluding White minorities) |   99 |  0.0704125 |      1408 |          0.0437580 |
| Ethnicity              | Prefer not to say                              |   NA |         NA |       314 |          0.0097585 |
| Ethnicity              | NA                                             |   NA |         NA |       147 |          0.0045685 |
| Education              | Other qualification or no qualifications       |  782 |  0.5561878 |     22020 |          0.6843397 |
| Education              | Degree education                               |  624 |  0.4438122 |     10157 |          0.3156603 |
| Gross household income | £0 - 15,490                                    |  279 |  0.1984353 |        NA |                 NA |
| Gross household income | £15,491 - £25,340                              |  252 |  0.1792319 |        NA |                 NA |
| Gross household income | £25,341 - £38,740                              |  259 |  0.1842105 |        NA |                 NA |
| Gross household income | £38,741 - £57,930                              |  311 |  0.2211949 |        NA |                 NA |
| Gross household income | £57,931 or more                                |  305 |  0.2169275 |        NA |                 NA |
| Gross household income | £0 - £14,999                                   |   NA |         NA |      4402 |          0.1842304 |
| Gross household income | £15,000 - £24,999                              |   NA |         NA |      4920 |          0.2059094 |
| Gross household income | £25,000 - £39,999                              |   NA |         NA |      6317 |          0.2643760 |
| Gross household income | £40,000 - £59,999                              |   NA |         NA |      4456 |          0.1864903 |
| Gross household income | £60,000 or more                                |   NA |         NA |      3799 |          0.1589939 |

``` r
#write.csv(demographics, "demographic_table.csv")
```

## Regression tables

``` r
# setting up variable orders
to_plot <- c("age_sc","W1_Education_binary","W2_Gender_binary2",
             "W1_Income_2019","elite_news","W2_INFO_9","mid_level_news",
             "W2_INFO_5","red_top_tabloid","threat","ethno","right",
             "distrust_science","W2_IOU_Total",
             "RWA","SDO","W1_Conspiracy_Total",
             "conspiracy1_sc","conspiracy2_sc","conspiracy3_sc")
names(to_plot) <- c("Age","Education","Gender","Income","Elite news",
              "Family and friends","Mid-level news","Social media",
              "Tabloid news","COVID-19 anxiety","Ethnocentrism",
              "Left-right scale","Distrust scientists",
              "Intolerance of uncertainty","RWA",
              "SDO","Conspiracy ideation",
              "Wuhan lab belief","Meat market belief","5G belief")

to_plot <- to_plot[sort(names(to_plot))]
```

``` r
stargazer(full_meat, full_lab, full_5g, full_5g_ihs, full_5g_poiss, 
          title="Table A2: Regression Results Origin Theory Belief",
          dep.var.labels=c("Meat market", "Wuhan lab","5G","5G","5G"),
          column.labels = c("OLS","OLS","OLS","OLS (IHS)","Poisson"),
          order = to_plot,
          covariate.labels = names(to_plot),
          #omit.stat=c("LL","ser","f","bic"),
          keep.stat = c("n","rsq","adj.rsq"),
          model.names = FALSE,
          single.row=TRUE,
          model.numbers = FALSE,
          align = TRUE,
          report = "vcs*",
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "html")
```

<table style="text-align:center">

<caption>

<strong>Table A2: Regression Results Origin Theory Belief</strong>

</caption>

<tr>

<td colspan="6" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="5">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Meat market

</td>

<td>

Wuhan lab

</td>

<td>

5G

</td>

<td>

5G

</td>

<td>

5G

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

OLS

</td>

<td>

OLS

</td>

<td>

OLS

</td>

<td>

OLS (IHS)

</td>

<td>

Poisson

</td>

</tr>

<tr>

<td colspan="6" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

5G belief

</td>

<td>

0.012 (0.039)

</td>

<td>

0.268 (0.039)<sup>\*\*\*</sup>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Age

</td>

<td>

0.052 (0.041)

</td>

<td>

\-0.008 (0.042)

</td>

<td>

\-0.080 (0.028)<sup>\*\*</sup>

</td>

<td>

\-0.341 (0.214)

</td>

<td>

\-0.928 (0.243)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Conspiracy ideation

</td>

<td>

0.023 (0.040)

</td>

<td>

0.255 (0.040)<sup>\*\*\*</sup>

</td>

<td>

0.032 (0.027)

</td>

<td>

0.156 (0.208)

</td>

<td>

0.353 (0.234)

</td>

</tr>

<tr>

<td style="text-align:left">

COVID-19 anxiety

</td>

<td>

0.130 (0.031)<sup>\*\*\*</sup>

</td>

<td>

0.127 (0.031)<sup>\*\*\*</sup>

</td>

<td>

0.007 (0.021)

</td>

<td>

0.099 (0.163)

</td>

<td>

0.072 (0.194)

</td>

</tr>

<tr>

<td style="text-align:left">

Distrust scientists

</td>

<td>

\-0.106 (0.032)<sup>\*\*</sup>

</td>

<td>

0.110 (0.033)<sup>\*\*\*</sup>

</td>

<td>

0.178 (0.022)<sup>\*\*\*</sup>

</td>

<td>

1.594 (0.166)<sup>\*\*\*</sup>

</td>

<td>

1.179 (0.166)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Education

</td>

<td>

\-0.037 (0.016)<sup>\*</sup>

</td>

<td>

\-0.051 (0.017)<sup>\*\*</sup>

</td>

<td>

\-0.009 (0.011)

</td>

<td>

\-0.088 (0.086)

</td>

<td>

\-0.131 (0.100)

</td>

</tr>

<tr>

<td style="text-align:left">

Elite news

</td>

<td>

0.017 (0.016)

</td>

<td>

\-0.013 (0.017)

</td>

<td>

0.008 (0.011)

</td>

<td>

\-0.045 (0.086)

</td>

<td>

0.028 (0.098)

</td>

</tr>

<tr>

<td style="text-align:left">

Ethnocentrism

</td>

<td>

0.157 (0.034)<sup>\*\*\*</sup>

</td>

<td>

0.122 (0.035)<sup>\*\*\*</sup>

</td>

<td>

0.033 (0.023)

</td>

<td>

0.244 (0.180)

</td>

<td>

0.224 (0.210)

</td>

</tr>

<tr>

<td style="text-align:left">

Family and friends

</td>

<td>

\-0.005 (0.029)

</td>

<td>

0.104 (0.030)<sup>\*\*\*</sup>

</td>

<td>

0.070 (0.020)<sup>\*\*\*</sup>

</td>

<td>

0.433 (0.153)<sup>\*\*</sup>

</td>

<td>

0.483 (0.177)<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Gender

</td>

<td>

\-0.001 (0.015)

</td>

<td>

0.020 (0.016)

</td>

<td>

\-0.001 (0.011)

</td>

<td>

0.154 (0.081)

</td>

<td>

0.062 (0.092)

</td>

</tr>

<tr>

<td style="text-align:left">

Income

</td>

<td>

0.059 (0.022)<sup>\*\*</sup>

</td>

<td>

\-0.042 (0.022)

</td>

<td>

\-0.037 (0.015)<sup>\*</sup>

</td>

<td>

\-0.322 (0.116)<sup>\*\*</sup>

</td>

<td>

\-0.325 (0.141)<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Intolerance of uncertainty

</td>

<td>

0.132 (0.041)<sup>\*\*</sup>

</td>

<td>

\-0.026 (0.042)

</td>

<td>

0.018 (0.028)

</td>

<td>

0.173 (0.218)

</td>

<td>

0.075 (0.233)

</td>

</tr>

<tr>

<td style="text-align:left">

Left-right scale

</td>

<td>

0.047 (0.044)

</td>

<td>

0.022 (0.045)

</td>

<td>

0.016 (0.030)

</td>

<td>

0.025 (0.232)

</td>

<td>

\-0.052 (0.225)

</td>

</tr>

<tr>

<td style="text-align:left">

Meat market belief

</td>

<td>

</td>

<td>

\-0.126 (0.027)<sup>\*\*\*</sup>

</td>

<td>

0.006 (0.018)

</td>

<td>

\-0.089 (0.141)

</td>

<td>

\-0.018 (0.168)

</td>

</tr>

<tr>

<td style="text-align:left">

Mid-level news

</td>

<td>

0.017 (0.017)

</td>

<td>

0.100 (0.017)<sup>\*\*\*</sup>

</td>

<td>

\-0.008 (0.012)

</td>

<td>

\-0.045 (0.089)

</td>

<td>

\-0.051 (0.096)

</td>

</tr>

<tr>

<td style="text-align:left">

RWA

</td>

<td>

\-0.040 (0.052)

</td>

<td>

0.166 (0.053)<sup>\*\*</sup>

</td>

<td>

\-0.123 (0.036)<sup>\*\*\*</sup>

</td>

<td>

\-0.340 (0.274)

</td>

<td>

\-0.609 (0.330)

</td>

</tr>

<tr>

<td style="text-align:left">

SDO

</td>

<td>

0.002 (0.051)

</td>

<td>

0.156 (0.052)<sup>\*\*</sup>

</td>

<td>

0.222 (0.035)<sup>\*\*\*</sup>

</td>

<td>

1.772 (0.265)<sup>\*\*\*</sup>

</td>

<td>

2.110 (0.310)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Social media

</td>

<td>

\-0.014 (0.027)

</td>

<td>

0.049 (0.028)

</td>

<td>

0.072 (0.019)<sup>\*\*\*</sup>

</td>

<td>

0.694 (0.143)<sup>\*\*\*</sup>

</td>

<td>

0.442 (0.164)<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Tabloid news

</td>

<td>

0.006 (0.018)

</td>

<td>

0.046 (0.018)<sup>\*</sup>

</td>

<td>

0.056 (0.012)<sup>\*\*\*</sup>

</td>

<td>

0.386 (0.092)<sup>\*\*\*</sup>

</td>

<td>

0.403 (0.094)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Wuhan lab belief

</td>

<td>

\-0.121 (0.026)<sup>\*\*\*</sup>

</td>

<td>

</td>

<td>

0.121 (0.018)<sup>\*\*\*</sup>

</td>

<td>

1.117 (0.136)<sup>\*\*\*</sup>

</td>

<td>

1.165 (0.165)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

0.424 (0.050)<sup>\*\*\*</sup>

</td>

<td>

\-0.093 (0.052)

</td>

<td>

\-0.079 (0.035)<sup>\*</sup>

</td>

<td>

\-0.440 (0.267)

</td>

<td>

0.469 (0.321)

</td>

</tr>

<tr>

<td colspan="6" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

1,399

</td>

<td>

1,399

</td>

<td>

1,399

</td>

<td>

1,399

</td>

<td>

1,399

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.083

</td>

<td>

0.289

</td>

<td>

0.260

</td>

<td>

0.305

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.070

</td>

<td>

0.279

</td>

<td>

0.250

</td>

<td>

0.296

</td>

<td>

</td>

</tr>

<tr>

<td colspan="6" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="5" style="text-align:right">

<sup>*</sup>p\<0.05; <sup>**</sup>p\<0.01; <sup>***</sup>p\<0.001

</td>

</tr>

</table>

``` r
stargazer(dist_full, vax_full,
          title="Table A3: Public Health Measures Model Results",
          dep.var.labels=c("Social distancing (OLS)",
                           "Vaccine (multi logit)",
                           "Vaccine (multi logit)"),
          multicolumn = TRUE,
          column.labels = c(" ", "No", "Maybe"),
          model.names = FALSE,
          model.numbers = FALSE,
          order = to_plot,
          covariate.labels = names(to_plot),
          #omit.stat=c("LL","ser","f","bic"),
          keep.stat = c("n","rsq","adj.rsq","aic"),
          single.row=TRUE,
          align = TRUE,
          report = "vcs*",
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "html")
```

<table style="text-align:center">

<caption>

<strong>Table A3: Public Health Measures Model Results</strong>

</caption>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="3">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Social distancing (OLS)

</td>

<td>

Vaccine (multi logit)

</td>

<td>

Vaccine (multi logit)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

No

</td>

<td>

Maybe

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

5G belief

</td>

<td>

\-0.141 (0.023)<sup>\*\*\*</sup>

</td>

<td>

2.147 (0.435)<sup>\*\*\*</sup>

</td>

<td>

\-0.013 (0.375)

</td>

</tr>

<tr>

<td style="text-align:left">

Age

</td>

<td>

0.172 (0.024)<sup>\*\*\*</sup>

</td>

<td>

\-2.867 (0.617)<sup>\*\*\*</sup>

</td>

<td>

\-1.166 (0.372)<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Conspiracy ideation

</td>

<td>

0.046 (0.023)<sup>\*</sup>

</td>

<td>

0.173 (0.595)

</td>

<td>

0.704 (0.370)

</td>

</tr>

<tr>

<td style="text-align:left">

COVID-19 anxiety

</td>

<td>

0.025 (0.018)

</td>

<td>

\-1.816 (0.450)<sup>\*\*\*</sup>

</td>

<td>

\-1.282 (0.276)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Distrust scientists

</td>

<td>

\-0.150 (0.019)<sup>\*\*\*</sup>

</td>

<td>

2.581 (0.440)<sup>\*\*\*</sup>

</td>

<td>

0.954 (0.295)<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Education

</td>

<td>

0.004 (0.009)

</td>

<td>

0.149 (0.236)

</td>

<td>

0.093 (0.150)

</td>

</tr>

<tr>

<td style="text-align:left">

Elite news

</td>

<td>

0.011 (0.009)

</td>

<td>

0.066 (0.234)

</td>

<td>

\-0.207 (0.152)

</td>

</tr>

<tr>

<td style="text-align:left">

Ethnocentrism

</td>

<td>

0.027 (0.020)

</td>

<td>

\-0.801 (0.487)

</td>

<td>

\-0.557 (0.310)

</td>

</tr>

<tr>

<td style="text-align:left">

Family and friends

</td>

<td>

0.057 (0.017)<sup>\*\*\*</sup>

</td>

<td>

\-0.310 (0.443)

</td>

<td>

\-0.039 (0.269)

</td>

</tr>

<tr>

<td style="text-align:left">

Gender

</td>

<td>

0.036 (0.009)<sup>\*\*\*</sup>

</td>

<td>

0.214 (0.225)

</td>

<td>

0.253 (0.141)

</td>

</tr>

<tr>

<td style="text-align:left">

Income

</td>

<td>

0.025 (0.013)<sup>\*</sup>

</td>

<td>

\-0.397 (0.329)

</td>

<td>

\-0.720 (0.200)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Intolerance of uncertainty

</td>

<td>

0.016 (0.024)

</td>

<td>

\-0.426 (0.594)

</td>

<td>

0.401 (0.376)

</td>

</tr>

<tr>

<td style="text-align:left">

Left-right scale

</td>

<td>

0.018 (0.026)

</td>

<td>

\-0.156 (0.603)

</td>

<td>

\-0.640 (0.405)

</td>

</tr>

<tr>

<td style="text-align:left">

Meat market belief

</td>

<td>

0.068 (0.016)<sup>\*\*\*</sup>

</td>

<td>

\-1.019 (0.391)<sup>\*\*</sup>

</td>

<td>

\-0.468 (0.238)<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Mid-level news

</td>

<td>

0.001 (0.010)

</td>

<td>

0.183 (0.238)

</td>

<td>

0.090 (0.156)

</td>

</tr>

<tr>

<td style="text-align:left">

RWA

</td>

<td>

0.120 (0.030)<sup>\*\*\*</sup>

</td>

<td>

1.012 (0.769)

</td>

<td>

0.676 (0.479)

</td>

</tr>

<tr>

<td style="text-align:left">

SDO

</td>

<td>

\-0.215 (0.030)<sup>\*\*\*</sup>

</td>

<td>

0.579 (0.774)

</td>

<td>

0.463 (0.467)

</td>

</tr>

<tr>

<td style="text-align:left">

Social media

</td>

<td>

\-0.015 (0.016)

</td>

<td>

0.379 (0.405)

</td>

<td>

0.197 (0.247)

</td>

</tr>

<tr>

<td style="text-align:left">

Tabloid news

</td>

<td>

\-0.009 (0.010)

</td>

<td>

\-0.229 (0.245)

</td>

<td>

\-0.131 (0.162)

</td>

</tr>

<tr>

<td style="text-align:left">

Wuhan lab belief

</td>

<td>

0.007 (0.015)

</td>

<td>

0.996 (0.400)<sup>\*</sup>

</td>

<td>

0.096 (0.240)

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

0.657 (0.030)<sup>\*\*\*</sup>

</td>

<td>

\-1.210 (0.766)

</td>

<td>

\-0.098 (0.463)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

1,399

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.266

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.255

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Akaike Inf. Crit.

</td>

<td>

</td>

<td>

2,054.657

</td>

<td>

2,054.657

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="3" style="text-align:right">

<sup>*</sup>p\<0.05; <sup>**</sup>p\<0.01; <sup>***</sup>p\<0.001

</td>

</tr>

</table>

``` r
tab <- full_join(
  margin_no %>% dplyr::select(factor:SE,p),
  margin_maybe %>% dplyr::select(factor:SE,p),
  by = "factor",
  suffix = c("_no","_maybe")
) %>% 
  as_tibble()

tab2 <- tibble(factor = to_plot, variable = names(to_plot))

ames <- left_join(
    tab,
    tab2,
    by = "factor"
    ) %>%
  dplyr::select(variable,everything())
```

``` r
kable(ames)
```

| variable                   | factor                |     AME\_no |    SE\_no |     p\_no |  AME\_maybe | SE\_maybe |  p\_maybe |
| :------------------------- | :-------------------- | ----------: | --------: | --------: | ----------: | --------: | --------: |
| Age                        | age\_sc               | \-0.1615372 | 0.0378285 | 0.0000195 | \-0.1252339 | 0.0700101 | 0.0736474 |
| Wuhan lab belief           | conspiracy1\_sc       |   0.0637148 | 0.0277392 | 0.0216233 | \-0.0083908 | 0.0436422 | 0.8475365 |
| Meat market belief         | conspiracy2\_sc       | \-0.0561299 | 0.0323444 | 0.0826735 | \-0.0534145 | 0.0370903 | 0.1498331 |
| 5G belief                  | conspiracy3\_sc       |   0.1427990 | 0.0270727 | 0.0000001 | \-0.0550133 | 0.0618257 | 0.3735667 |
| Distrust scientists        | distrust\_science     |   0.1477947 | 0.0306252 | 0.0000014 |   0.0967139 | 0.0448466 | 0.0310409 |
| Elite news                 | elite\_news           |   0.0094896 | 0.0162488 | 0.5592087 | \-0.0364152 | 0.0210080 | 0.0830253 |
| Ethnocentrism              | ethno                 | \-0.0394840 | 0.0333169 | 0.2359752 | \-0.0737707 | 0.0509726 | 0.1478233 |
| Mid-level news             | mid\_level\_news      |   0.0099706 | 0.0162411 | 0.5392719 |   0.0105213 | 0.0256511 | 0.6816812 |
| Tabloid news               | red\_top\_tabloid     | \-0.0119851 | 0.0149767 | 0.4235657 | \-0.0163570 | 0.0257439 | 0.5251840 |
| Left-right scale           | right                 |   0.0054075 | 0.0491898 | 0.9124631 | \-0.1035326 | 0.0574798 | 0.0716714 |
| RWA                        | RWA                   |   0.0505452 | 0.0602685 | 0.4016559 |   0.0885985 | 0.0728023 | 0.2236143 |
| SDO                        | SDO                   |   0.0270746 | 0.0545978 | 0.6199705 |   0.0633904 | 0.0751473 | 0.3989215 |
| COVID-19 anxiety           | threat                | \-0.0889479 | 0.0367432 | 0.0154865 | \-0.1705003 | 0.0405062 | 0.0000256 |
| Conspiracy ideation        | W1\_Conspiracy\_Total | \-0.0058160 | 0.0325771 | 0.8583072 |   0.1139518 | 0.0602896 | 0.0587481 |
| Education                  | W1\_Education\_binary |   0.0076020 | 0.0166450 | 0.6478762 |   0.0118811 | 0.0219310 | 0.5879913 |
| Income                     | W1\_Income\_2019      | \-0.0086331 | 0.0279366 | 0.7573017 | \-0.1110991 | 0.0356810 | 0.0018477 |
| Gender                     | W2\_Gender\_binary2   |   0.0080295 | 0.0153910 | 0.6018807 |   0.0372956 | 0.0236473 | 0.1147584 |
| Social media               | W2\_INFO\_5           |   0.0203137 | 0.0275113 | 0.4602861 |   0.0236931 | 0.0461687 | 0.6078218 |
| Family and friends         | W2\_INFO\_9           | \-0.0196362 | 0.0326718 | 0.5478312 |   0.0011001 | 0.0459693 | 0.9809075 |
| Intolerance of uncertainty | W2\_IOU\_Total        | \-0.0381565 | 0.0443563 | 0.3896640 |   0.0778272 | 0.0631957 | 0.2181255 |

## Missing Values

There are 3 missing values from the W2\_Gender\_binary variable, 4
missing values from the distrust\_science variable, and 9 missing values
from the W2\_C19\_Vax\_Self variable. Missing values were removed with
listwise deletion when the respective variable is included in a model.
Therefore, for the conspiracy theory and the social distancing models
n=1399. And for the vaccination model n=1390.

``` r
select_these <- names(conspiracies2)

conspiracies3 <- conspiracies %>%
  dplyr::select(one_of(select_these[-length(select_these)]))

conspiracies3 %>% map_int(count_na) %>% sort()
```

    ##                   pid   W1_Education_binary        W1_Income_2019 
    ##                     0                     0                     0 
    ##                age_sc                 right                 ethno 
    ##                     0                     0                     0 
    ##       red_top_tabloid        mid_level_news            elite_news 
    ##                     0                     0                     0 
    ##             W2_INFO_5             W2_INFO_9                   SDO 
    ##                     0                     0                     0 
    ##                   RWA          W2_IOU_Total                threat 
    ##                     0                     0                     0 
    ##   W1_Conspiracy_Total        conspiracy2_sc        conspiracy3_sc 
    ##                     0                     0                     0 
    ##    w2_conspiracy3_ihs W2_Conspiracy_Theory1 W2_Conspiracy_Theory2 
    ##                     0                     0                     0 
    ## W2_Conspiracy_Theory3        conspiracy1_sc   W2_SocialDistance10 
    ##                     0                     0                     0 
    ##   W2_SocialDistance11   W2_SocialDistance12   W2_SocialDistance14 
    ##                     0                     0                     0 
    ##      W2_C19_Vax_Child      W2_Gender_binary      distrust_science 
    ##                     0                     3                     4 
    ##       W2_C19_Vax_Self 
    ##                     9

``` r
count_na(conspiracies2$social_distance)
```

    ## [1] 0
