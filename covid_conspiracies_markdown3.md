covid\_conspiracies\_markdown3
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
# rescaling the remaining numeric variables
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

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-22-2.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-22-3.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-22-4.png)<!-- -->

    ## Warning: Removed 4 rows containing non-finite values (stat_density).

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-22-5.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-22-6.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-22-7.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-22-8.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-22-9.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-22-10.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-22-11.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-22-12.png)<!-- -->![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-22-13.png)<!-- -->

# Correlation between the origin theories

Correlation between the origin theories below. Statistical significance
tested via spearman’s rank, with bonferroni correction for 3 tests.

``` r
test_1 <- cor.test(
  conspiracies$conspiracy1_sc,
  conspiracies$conspiracy2_sc,
  alternative = "two.sided",
  method = "kendall"
  )
test_1
```

    ## 
    ##  Kendall's rank correlation tau
    ## 
    ## data:  conspiracies$conspiracy1_sc and conspiracies$conspiracy2_sc
    ## z = -5.2025, p-value = 1.966e-07
    ## alternative hypothesis: true tau is not equal to 0
    ## sample estimates:
    ##         tau 
    ## -0.09571348

``` r
test_1$p.value < (0.05 / 3)
```

    ## [1] TRUE

``` r
test_2 <- cor.test(
  conspiracies$conspiracy1_sc,
  conspiracies$conspiracy3_sc,
  alternative = "two.sided",
  method = "kendall"
  )
test_2
```

    ## 
    ##  Kendall's rank correlation tau
    ## 
    ## data:  conspiracies$conspiracy1_sc and conspiracies$conspiracy3_sc
    ## z = 16.486, p-value < 2.2e-16
    ## alternative hypothesis: true tau is not equal to 0
    ## sample estimates:
    ##       tau 
    ## 0.3238067

``` r
test_2$p.value < (0.05 / 3)
```

    ## [1] TRUE

``` r
test_3 <- cor.test(
  conspiracies$conspiracy2_sc,
  conspiracies$conspiracy3_sc,
  alternative = "two.sided",
  method = "kendall"
  )
test_3
```

    ## 
    ##  Kendall's rank correlation tau
    ## 
    ## data:  conspiracies$conspiracy2_sc and conspiracies$conspiracy3_sc
    ## z = -3.9302, p-value = 8.489e-05
    ## alternative hypothesis: true tau is not equal to 0
    ## sample estimates:
    ##         tau 
    ## -0.07656141

``` r
test_3$p.value < (0.05 / 3)
```

    ## [1] TRUE

``` r
pacman::p_load(knitr)
tibble(
  `Origin Belief` = c("Meat Market","Wuhan laboratory","5G"),
  Mean = c(round(mean(conspiracies$conspiracy2_sc, na.rm = T),3),
           round(mean(conspiracies$conspiracy1_sc, na.rm = T),3),
           round(mean(conspiracies$conspiracy3_sc, na.rm = T),3)),
  SD = c(round(sd(conspiracies$conspiracy2_sc, na.rm = T),3),
           round(sd(conspiracies$conspiracy1_sc, na.rm = T),3),
           round(sd(conspiracies$conspiracy3_sc, na.rm = T),3)),
  `1.` = c(" ",round(test_1$estimate,3),round(test_3$estimate,3)),
  `2.` = c(" "," ",round(test_2$estimate,3))
) %>% 
  kable()
```

| Origin Belief    |  Mean |    SD | 1\.     | 2\.   |
| :--------------- | ----: | ----: | :------ | :---- |
| Meat Market      | 0.640 | 0.287 |         |       |
| Wuhan laboratory | 0.383 | 0.332 | \-0.096 |       |
| 5G               | 0.111 | 0.219 | \-0.077 | 0.324 |

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
                  W1_Conspiracy_Total)

summary(full_lab)$adj.r.squared
```

    ## [1] 0.2448091

``` r
AIC(full_lab)
```

    ## [1] 510.9649

``` r
summ(full_lab, vifs = T)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: conspiracy1_sc
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(17,1381) = 27.66, p = 0.00
    ## R² = 0.25
    ## Adj. R² = 0.24 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------------------
    ##                              Est.   S.E.   t val.      p    VIF
    ## ------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                 -0.18   0.05    -3.40   0.00       
    ## W2_Gender_binary2            0.02   0.02     1.30   0.19   1.09
    ## W1_Education_binary         -0.05   0.02    -3.02   0.00   1.20
    ## W1_Income_2019              -0.06   0.02    -2.74   0.01   1.13
    ## age_sc                      -0.04   0.04    -0.87   0.38   1.39
    ## right                        0.02   0.05     0.46   0.64   1.49
    ## ethno                        0.12   0.04     3.31   0.00   1.30
    ## distrust_science             0.18   0.03     5.49   0.00   1.11
    ## red_top_tabloid              0.06   0.02     3.43   0.00   1.09
    ## mid_level_news               0.10   0.02     5.72   0.00   1.13
    ## elite_news                  -0.01   0.02    -0.81   0.42   1.14
    ## W2_INFO_5                    0.07   0.03     2.58   0.01   1.39
    ## W2_INFO_9                    0.13   0.03     4.27   0.00   1.24
    ## SDO                          0.23   0.05     4.31   0.00   1.48
    ## RWA                          0.14   0.05     2.67   0.01   1.42
    ## W2_IOU_Total                -0.04   0.04    -0.91   0.37   1.22
    ## threat                       0.12   0.03     3.70   0.00   1.14
    ## W1_Conspiracy_Total          0.27   0.04     6.71   0.00   1.10
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
    ## F(18,1380) = 26.13, p = 0.00
    ## R² = 0.25
    ## Adj. R² = 0.24 
    ## 
    ## Standard errors: OLS
    ## --------------------------------------------------------
    ##                              Est.   S.E.   t val.      p
    ## ------------------------- ------- ------ -------- ------
    ## (Intercept)                 -0.14   0.08    -1.84   0.07
    ## W2_Gender_binary2            0.02   0.02     1.27   0.20
    ## W1_Education_binary         -0.05   0.02    -3.03   0.00
    ## W1_Income_2019              -0.06   0.02    -2.72   0.01
    ## age_sc                      -0.04   0.04    -0.89   0.38
    ## right                        0.02   0.05     0.45   0.66
    ## ethno                        0.12   0.04     3.31   0.00
    ## distrust_science             0.18   0.03     5.47   0.00
    ## red_top_tabloid              0.06   0.02     3.45   0.00
    ## mid_level_news               0.10   0.02     5.74   0.00
    ## elite_news                  -0.01   0.02    -0.78   0.44
    ## W2_INFO_5                    0.07   0.03     2.60   0.01
    ## W2_INFO_9                    0.13   0.03     4.23   0.00
    ## SDO                          0.23   0.05     4.33   0.00
    ## W2_IOU_Total                -0.04   0.04    -0.88   0.38
    ## W1_Conspiracy_Total          0.27   0.04     6.72   0.00
    ## RWA                          0.07   0.12     0.61   0.54
    ## threat                       0.06   0.10     0.62   0.54
    ## RWA:threat                   0.11   0.17     0.65   0.51
    ## --------------------------------------------------------

``` r
plot_coefs(int_lab)
```

    ## Loading required namespace: broom.mixed

    ## Registered S3 method overwritten by 'broom.mixed':
    ##   method      from 
    ##   tidy.gamlss broom

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

No evidence for an interaction between RWA and threat.

``` r
summary(full_lab)$adj.r.squared
```

    ## [1] 0.2448091

``` r
AIC(full_lab)
```

    ## [1] 510.9649

``` r
summary(int_lab)$adj.r.squared
```

    ## [1] 0.2444946

``` r
AIC(int_lab)
```

    ## [1] 512.534

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

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
# ihs transformation where theta = 1
conspiracies %>% 
  ggplot(aes(x = asinh(W2_Conspiracy_Theory3))) +
  geom_histogram(aes(y = ..density..),
                 colour = "darkgrey", fill = "lightblue") +
  geom_density()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-37-2.png)<!-- -->

``` r
# ihs transformation where theta = 1e+08
conspiracies %>% 
  ggplot(aes(x = ihs(W2_Conspiracy_Theory3, best_theta))) +
  geom_histogram(aes(y = ..density..),
                 colour = "darkgrey", fill = "lightblue") +
  geom_density()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-37-3.png)<!-- -->

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
                W1_Conspiracy_Total)

adj_rsq(full_5g)
```

    ## [1] 0.2257733

``` r
AIC(full_5g)
```

    ## [1] -625.6515

``` r
summ(full_5g, vifs = T)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: conspiracy3_sc
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(17,1381) = 24.98, p = 0.00
    ## R² = 0.24
    ## Adj. R² = 0.23 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------------------
    ##                              Est.   S.E.   t val.      p    VIF
    ## ------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                 -0.10   0.03    -2.83   0.00       
    ## W2_Gender_binary2            0.00   0.01     0.18   0.85   1.09
    ## W1_Education_binary         -0.02   0.01    -1.34   0.18   1.20
    ## W1_Income_2019              -0.04   0.02    -2.91   0.00   1.13
    ## age_sc                      -0.08   0.03    -2.96   0.00   1.39
    ## right                        0.02   0.03     0.62   0.54   1.49
    ## ethno                        0.05   0.02     2.02   0.04   1.30
    ## distrust_science             0.20   0.02     9.17   0.00   1.11
    ## red_top_tabloid              0.06   0.01     5.21   0.00   1.09
    ## mid_level_news               0.00   0.01     0.36   0.72   1.13
    ## elite_news                   0.01   0.01     0.55   0.58   1.14
    ## W2_INFO_5                    0.08   0.02     4.24   0.00   1.39
    ## W2_INFO_9                    0.09   0.02     4.24   0.00   1.24
    ## SDO                          0.25   0.03     7.15   0.00   1.48
    ## RWA                         -0.11   0.04    -2.92   0.00   1.42
    ## W2_IOU_Total                 0.01   0.03     0.48   0.63   1.22
    ## threat                       0.02   0.02     1.01   0.31   1.14
    ## W1_Conspiracy_Total          0.06   0.03     2.39   0.02   1.10
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
                W1_Conspiracy_Total)

adj_rsq(full_5g_ihs)
```

    ## [1] 0.2612123

``` r
AIC(full_5g_ihs)
```

    ## [1] 5095.941

``` r
summ(full_5g_ihs, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(17,1381) = 30.08, p = 0.00
    ## R² = 0.27
    ## Adj. R² = 0.26 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------------------
    ##                              Est.   S.E.   t val.      p    VIF
    ## ------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                 -0.67   0.27    -2.54   0.01       
    ## W2_Gender_binary2            0.18   0.08     2.14   0.03   1.09
    ## W1_Education_binary         -0.14   0.09    -1.63   0.10   1.20
    ## W1_Income_2019              -0.40   0.12    -3.37   0.00   1.13
    ## age_sc                      -0.39   0.22    -1.77   0.08   1.39
    ## right                        0.05   0.24     0.19   0.85   1.49
    ## ethno                        0.36   0.18     1.99   0.05   1.30
    ## distrust_science             1.80   0.17    10.78   0.00   1.11
    ## red_top_tabloid              0.46   0.09     4.86   0.00   1.09
    ## mid_level_news               0.07   0.09     0.74   0.46   1.13
    ## elite_news                  -0.06   0.09    -0.70   0.48   1.14
    ## W2_INFO_5                    0.78   0.15     5.31   0.00   1.39
    ## W2_INFO_9                    0.58   0.16     3.71   0.00   1.24
    ## SDO                          2.03   0.27     7.51   0.00   1.48
    ## RWA                         -0.17   0.28    -0.62   0.54   1.42
    ## W2_IOU_Total                 0.12   0.22     0.53   0.60   1.22
    ## threat                       0.22   0.16     1.34   0.18   1.14
    ## W1_Conspiracy_Total          0.46   0.21     2.20   0.03   1.10
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
                           W1_Conspiracy_Total,
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
    ## <U+03C7>²(17) = 12893.93, p = 0.00
    ## Pseudo-R² (Cragg-Uhler) = 1.00
    ## Pseudo-R² (McFadden) = 0.29
    ## AIC =  NA, BIC =  NA 
    ## 
    ## Standard errors: MLE
    ## --------------------------------------------------------
    ##                              Est.   S.E.   t val.      p
    ## ------------------------- ------- ------ -------- ------
    ## (Intercept)                  0.27   0.31     0.87   0.39
    ## W2_Gender_binary2            0.09   0.09     0.92   0.36
    ## W1_Education_binary         -0.15   0.10    -1.52   0.13
    ## W1_Income_2019              -0.40   0.14    -2.77   0.01
    ## age_sc                      -0.95   0.25    -3.81   0.00
    ## right                        0.01   0.23     0.03   0.98
    ## ethno                        0.37   0.21     1.74   0.08
    ## distrust_science             1.40   0.17     8.30   0.00
    ## red_top_tabloid              0.48   0.10     4.95   0.00
    ## mid_level_news               0.01   0.10     0.11   0.91
    ## elite_news                   0.00   0.10     0.03   0.97
    ## W2_INFO_5                    0.52   0.17     3.16   0.00
    ## W2_INFO_9                    0.60   0.18     3.29   0.00
    ## SDO                          2.36   0.31     7.63   0.00
    ## RWA                         -0.48   0.34    -1.43   0.15
    ## W2_IOU_Total                 0.02   0.24     0.07   0.94
    ## threat                       0.29   0.20     1.49   0.14
    ## W1_Conspiracy_Total          0.68   0.24     2.89   0.00
    ## --------------------------------------------------------
    ## 
    ## Estimated dispersion parameter = 31.86

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
                  W1_Conspiracy_Total)

adj_rsq(full_meat)
```

    ## [1] 0.0569456

``` r
AIC(full_meat)
```

    ## [1] 412.2766

``` r
summ(full_meat, vifs = T)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: conspiracy2_sc
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(17,1381) = 5.97, p = 0.00
    ## R² = 0.07
    ## Adj. R² = 0.06 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------------------
    ##                              Est.   S.E.   t val.      p    VIF
    ## ------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  0.44   0.05     8.92   0.00       
    ## W2_Gender_binary2           -0.00   0.02    -0.24   0.81   1.09
    ## W1_Education_binary         -0.03   0.02    -1.86   0.06   1.20
    ## W1_Income_2019               0.07   0.02     3.01   0.00   1.13
    ## age_sc                       0.06   0.04     1.36   0.17   1.39
    ## right                        0.04   0.04     1.01   0.31   1.49
    ## ethno                        0.14   0.03     4.21   0.00   1.30
    ## distrust_science            -0.13   0.03    -3.99   0.00   1.11
    ## red_top_tabloid             -0.00   0.02    -0.02   0.98   1.09
    ## mid_level_news               0.01   0.02     0.32   0.75   1.13
    ## elite_news                   0.02   0.02     1.15   0.25   1.14
    ## W2_INFO_5                   -0.02   0.03    -0.79   0.43   1.39
    ## W2_INFO_9                   -0.02   0.03    -0.69   0.49   1.24
    ## SDO                         -0.02   0.05    -0.44   0.66   1.48
    ## RWA                         -0.06   0.05    -1.12   0.26   1.42
    ## W2_IOU_Total                 0.14   0.04     3.30   0.00   1.22
    ## threat                       0.12   0.03     3.75   0.00   1.14
    ## W1_Conspiracy_Total         -0.01   0.04    -0.25   0.81   1.10
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

conspiracy <- c("W1_Conspiracy_Total")
names(conspiracy) <- c("Conspiracy ideation")

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

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_lab)
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-65-2.png)<!-- -->

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

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_5g)
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-66-2.png)<!-- -->

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

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_meat)
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-67-2.png)<!-- -->

## Combined plot of models

``` r
model_vars <- c(dispositions,conspiracy)
model_vars <- model_vars[sort(names(model_vars))]

plot_coefs(
  full_lab,full_5g,full_meat,
  model.names = c("Wuhan lab","5G","Meat market"),
  coefs = model_vars#,
  #groups = list(
  #  Dispositions = names(dispositions),
  #  Conspiracy = names(conspiracy)
  #  ),
  #facet.label.pos = "left" 
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

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-69-1.png)<!-- -->

``` r
plot_coefs(
  full_5g_ihs,full_5g_poiss,
  model.names = c("5G IHS transformed","5G poisson"),
  coefs = model_vars#,
  #groups = list(
  #  Conspiracy = names(conspiracy),
  #  Dispositions = names(dispositions)
  #  ),
  #facet.label.pos = "left" 
  ) +
  labs(
    x = "Coefficient estimate",
    caption = "Note: Comparison of supplementary models. IHS transformed model estimated using OLS.\nPoisson regression estimated using maximum likelihood method, with overdispersion parameter (=31.86).\n95% confidence intervals presented. Numerical predictors scaled 0-1."
  ) + 
  theme(legend.position = "top",
        legend.margin=margin(t = 0, b = 0, unit='cm'),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(hjust = 1),
        plot.caption = element_text(hjust = 0))
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-70-1.png)<!-- -->

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
          "conspiracy1_sc","conspiracy2_sc","conspiracy3_sc","pid")
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

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-73-1.png)<!-- -->

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

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-75-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(dist_full)
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-75-2.png)<!-- -->

## Multinomial model for vaccine acceptance

``` r
# making binary for vaccine reluctance
conspiracies2 <- conspiracies2 %>% 
  mutate(
    vax_reluctance = ifelse(W2_C19_Vax_Self == 1, 0, 1)
  )

# multinomial model
vax_full <- glm(vax_reluctance ~ 
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
                data = conspiracies2, family = "binomial")
```

``` r
# summary of vaccine model
summ(vax_full)
```

    ## MODEL INFO:
    ## Observations: 1390 (9 missing obs. deleted)
    ## Dependent Variable: vax_reluctance
    ## Type: Generalized linear model
    ##   Family: binomial 
    ##   Link function: logit 
    ## 
    ## MODEL FIT:
    ## <U+03C7>²(20) = 218.33, p = 0.00
    ## Pseudo-R² (Cragg-Uhler) = 0.20
    ## Pseudo-R² (McFadden) = 0.12
    ## AIC = 1575.58, BIC = 1685.56 
    ## 
    ## Standard errors: MLE
    ## --------------------------------------------------------
    ##                              Est.   S.E.   z val.      p
    ## ------------------------- ------- ------ -------- ------
    ## (Intercept)                  0.24   0.43     0.55   0.58
    ## W2_Gender_binary2            0.24   0.13     1.84   0.07
    ## W1_Education_binary          0.11   0.14     0.78   0.44
    ## W1_Income_2019              -0.66   0.19    -3.55   0.00
    ## age_sc                      -1.57   0.34    -4.55   0.00
    ## right                       -0.49   0.37    -1.34   0.18
    ## ethno                       -0.63   0.29    -2.19   0.03
    ## distrust_science             1.35   0.27     5.04   0.00
    ## red_top_tabloid             -0.16   0.15    -1.08   0.28
    ## mid_level_news               0.12   0.14     0.84   0.40
    ## elite_news                  -0.12   0.14    -0.89   0.37
    ## W2_INFO_5                    0.21   0.23     0.92   0.36
    ## W2_INFO_9                   -0.08   0.25    -0.33   0.74
    ## SDO                          0.46   0.43     1.07   0.29
    ## RWA                          0.72   0.45     1.62   0.11
    ## W2_IOU_Total                 0.21   0.35     0.60   0.55
    ## threat                      -1.40   0.26    -5.42   0.00
    ## W1_Conspiracy_Total          0.61   0.34     1.77   0.08
    ## conspiracy1_sc               0.27   0.22     1.21   0.23
    ## conspiracy2_sc              -0.57   0.22    -2.60   0.01
    ## conspiracy3_sc               0.79   0.31     2.52   0.01
    ## --------------------------------------------------------

Below is a plot of the average marginal effects for key variables.

``` r
pacman::p_load(margins, ggstance)

# average marginal effect for vaccine model
set.seed(123)
margin_vax <- margins_summary(
  vax_full,
  type = "response",
  vce = "bootstrap"
)
margin_vax
```

``` r
# plotting average marginal effect
vax_plots <- c("W1_Income_2019","age_sc","ethno","distrust_science",
               "threat","W1_Conspiracy_Total","conspiracy1_sc",
               "conspiracy2_sc","conspiracy3_sc","RWA","SDO")

conspiracy2 <- c(conspiracy,"conspiracy1_sc","conspiracy2_sc",
                 "conspiracy3_sc")

margin_vax %>% 
  rename(term = factor) %>% 
  filter(term %in% vax_plots) %>%
  mutate(
    term = fct_rev(fct_drop(fct_relevel(term, vax_plots))),
    group_facet = ifelse(term %in% dispositions, 
                         "Dispositions",
                         ifelse(term %in% conspiracy2, "Conspiracy",
                                       "Controls")),
    term = fct_recode(term,
                      "Age" = "age_sc",
                      "Income" = "W1_Income_2019",
                      "COVID-19 anxiety" = "threat",
                      "Distrust scientists" = "distrust_science",
                      "Ethnocentrism" = "ethno",
                      "RWA" = "RWA",
                      "SDO" = "SDO",
                      "Conspiracy ideation" = "W1_Conspiracy_Total",
                      "Meat market belief" = "conspiracy2_sc",
                      "Wuhan lab belief" = "conspiracy1_sc",
                      "5G belief" = "conspiracy3_sc")
  ) %>% 
  ggplot(aes(y = term, 
           x = AME, xmin = lower,
           xmax = upper)) + 
  geom_vline(xintercept = 0, linetype = 2, size = .25) +
  ggstance::geom_linerangeh(
    aes(y = term, xmin = lower,
        xmax = upper),
    position = ggstance::position_dodgev(height = 0.62), size = 0.8,
    colour = get_colors("CUD Bright",num.colors = 1)) +
  geom_point(
    aes(y = term, 
        x = AME),
    position = ggstance::position_dodgev(height = 0.62),
    colour = get_colors("CUD Bright",num.colors = 1),
    fill = "white", size = 3, stroke = 1, shape = 21) +
  #scale_colour_manual(values = get_colors("CUD Bright",num.colors = 2)) +
  theme_nice() +
  drop_y_gridlines() +
  facet_wrap(~group_facet, ncol = 1, scales = "free_y",
             strip.position = "left") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 10,
                                   hjust = 1),
        panel.grid.major.x = element_line(linetype = "solid"),
        strip.text.x = element_text(size = 8),
        plot.caption = element_text(hjust = 0)) +
  labs(
    x = "Average marginal effect: Vaccine reluctance",
    caption = "Note: Binomial logit estimated using maximum likelihood method.  Bootstrapped 95% confidence intervals.\nNumerical predictors scaled 0-1."
  )
```

![](covid_conspiracies_markdown3_files/figure-gfm/unnamed-chunk-79-1.png)<!-- -->

``` r
tidies <- tidy(vax_full)

#presenting odds ratios for vaccine reluctance
tidies %>% 
  mutate(
    odds_ratio = exp(estimate),
    or_lower = exp(estimate - qnorm(0.975) * std.error),
    or_upper = exp(estimate + qnorm(0.975) * std.error)
  ) %>% 
  filter(term %in% vax_plots) %>%
  mutate(
    term = fct_rev(fct_drop(fct_relevel(term, vax_plots))),
    group_facet = ifelse(term %in% dispositions, 
                         "Dispositions",
                         ifelse(term %in% conspiracy2, "Conspiracy",
                                       "Controls")),
    term = fct_recode(term,
                      "Age" = "age_sc",
                      "Income" = "W1_Income_2019",
                      "COVID-19 anxiety" = "threat",
                      "Distrust scientists" = "distrust_science",
                      "Ethnocentrism" = "ethno",
                      "RWA" = "RWA",
                      "SDO" = "SDO",
                      "Conspiracy ideation" = "W1_Conspiracy_Total",
                      "Meat market belief" = "conspiracy2_sc",
                      "Wuhan lab belief" = "conspiracy1_sc",
                      "5G belief" = "conspiracy3_sc")
  ) %>% 
  ggplot(aes(y = term, 
           x = odds_ratio, xmin = or_lower,
           xmax = or_upper)) + 
  geom_vline(xintercept = 1, linetype = 2, size = .25) +
  ggstance::geom_linerangeh(
    aes(y = term, xmin = or_lower,
        xmax = or_upper),
    position = ggstance::position_dodgev(height = 0.62), size = 0.8,
    colour = get_colors("CUD Bright",num.colors = 1)) +
  geom_point(
    aes(y = term, 
        x = odds_ratio),
    position = ggstance::position_dodgev(height = 0.62),
    colour = get_colors("CUD Bright",num.colors = 1),
    fill = "white", size = 3, stroke = 1, shape = 21) +
  #scale_colour_manual(values = get_colors("CUD Bright",num.colors = 2)) +
  theme_nice() +
  drop_y_gridlines() +
  facet_wrap(~group_facet, ncol = 1, scales = "free_y",
             strip.position = "left") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 10,
                                   hjust = 1),
        panel.grid.major.x = element_line(linetype = "solid"),
        strip.text.x = element_text(size = 8),
        plot.caption = element_text(hjust = 0)) +
  labs(
    x = "Odds ratios: Vaccine reluctance",
    caption = "Note: Binomial logit estimated using maximum likelihood method.  Odds ratios with 95% confidence intervals.\nNumerical predictors scaled 0-1."
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
  "social_distance","W2_C19_Vax_Self","conspiracy1_sc","conspiracy2_sc",
  "conspiracy3_sc")

names(fuller_list) <- c(names(full_list),
                        "Social distancing motivation",
                        "Vaccination attitudes",
                        "Meat market","Wuhan laboratory","5G")

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
```

``` r
# table for correlation matrix
pacman::p_load(stargazer)

lower_tri <- lower_tri %>% as_tibble() %>% 
  mutate(` ` = names(cor_df)) %>% 
  dplyr::select(` `, everything())

kable(lower_tri,
      caption = "Correlation Matrix")
```

|                              |          5G |         Age | Conspiracy ideation | COVID-19 anxiety | Distrust scientists |   Education | Ethnocentrism | Family and friends |      Gender |      Income | Left-right scale | Meat market | News: elite | News: mid-level | News: tabloid |         RWA |         SDO | Social distancing motivation | Social media | Uncertainty intolerance | Vaccination attitudes | Wuhan laboratory |
| :--------------------------- | ----------: | ----------: | ------------------: | ---------------: | ------------------: | ----------: | ------------: | -----------------: | ----------: | ----------: | ---------------: | ----------: | ----------: | --------------: | ------------: | ----------: | ----------: | ---------------------------: | -----------: | ----------------------: | --------------------: | ---------------: |
| 5G                           |   1.0000000 |          NA |                  NA |               NA |                  NA |          NA |            NA |                 NA |          NA |          NA |               NA |          NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Age                          | \-0.1968411 |   1.0000000 |                  NA |               NA |                  NA |          NA |            NA |                 NA |          NA |          NA |               NA |          NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Conspiracy ideation          |   0.1303185 | \-0.0484166 |           1.0000000 |               NA |                  NA |          NA |            NA |                 NA |          NA |          NA |               NA |          NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| COVID-19 anxiety             |   0.0390187 |   0.0556909 |           0.0871699 |        1.0000000 |                  NA |          NA |            NA |                 NA |          NA |          NA |               NA |          NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Distrust scientists          |   0.2993469 | \-0.0757459 |           0.1710566 |      \-0.0424669 |           1.0000000 |          NA |            NA |                 NA |          NA |          NA |               NA |          NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Education                    | \-0.0744352 | \-0.1655707 |         \-0.0865678 |      \-0.0112353 |         \-0.0940102 |   1.0000000 |            NA |                 NA |          NA |          NA |               NA |          NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Ethnocentrism                |   0.1019370 |   0.1343591 |           0.0163442 |        0.0570581 |         \-0.0325778 | \-0.1508601 |     1.0000000 |                 NA |          NA |          NA |               NA |          NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Family and friends           |   0.1853119 | \-0.1909207 |           0.0660756 |        0.1725282 |         \-0.0041932 |   0.0102841 |     0.0708626 |          1.0000000 |          NA |          NA |               NA |          NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Gender                       |   0.0151136 | \-0.1457747 |           0.0701525 |        0.0476996 |           0.0262593 | \-0.0129786 |   \-0.0515851 |          0.0723318 |   1.0000000 |          NA |               NA |          NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Income                       | \-0.1120980 |   0.0075214 |         \-0.0911093 |      \-0.0210025 |         \-0.0848267 |   0.2327198 |   \-0.0182942 |        \-0.0302430 | \-0.0871241 |   1.0000000 |               NA |          NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Left-right scale             |   0.0971868 |   0.1338143 |         \-0.0233279 |      \-0.0101604 |           0.0588338 | \-0.1027946 |     0.3784391 |        \-0.0035331 | \-0.0639928 |   0.1126077 |        1.0000000 |          NA |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| Meat market                  |   0.3299842 | \-0.0554126 |           0.2340217 |        0.1356206 |           0.2220164 | \-0.1834670 |     0.2083188 |          0.1909797 |   0.0617540 | \-0.1306903 |        0.1634907 |   1.0000000 |          NA |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| News: elite                  | \-0.0310280 | \-0.0480979 |         \-0.0680059 |        0.0481917 |         \-0.0864244 |   0.2576219 |   \-0.1238120 |          0.0740565 | \-0.0587591 |   0.1434613 |      \-0.1095518 | \-0.1116579 |   1.0000000 |              NA |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| News: mid-level              |   0.0984832 |   0.0478379 |           0.0346567 |        0.0629015 |           0.0885137 | \-0.0978202 |     0.1989451 |          0.1155816 |   0.0177856 |   0.0045211 |        0.2395652 |   0.2502842 | \-0.0424565 |       1.0000000 |            NA |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| News: tabloid                |   0.2088942 | \-0.1203675 |           0.0598785 |        0.0640629 |           0.0857823 | \-0.1236667 |     0.0894869 |          0.1015062 | \-0.0576030 | \-0.1110322 |        0.0227170 |   0.1763378 | \-0.0153704 |       0.1212012 |     1.0000000 |          NA |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| RWA                          |   0.0402643 |   0.1482799 |           0.0989850 |        0.0842022 |           0.0673202 | \-0.2067672 |     0.3220561 |          0.0018120 |   0.0913433 | \-0.0408217 |        0.3774242 |   0.2402447 | \-0.2320842 |       0.2118870 |     0.0507231 |   1.0000000 |          NA |                           NA |           NA |                      NA |                    NA |               NA |
| SDO                          |   0.2330242 | \-0.0403077 |         \-0.0599814 |      \-0.0889019 |           0.1762634 | \-0.0724244 |     0.2919805 |        \-0.0529470 | \-0.1015043 |   0.0533128 |        0.4436087 |   0.2050590 | \-0.1508152 |       0.1472083 |     0.0199403 |   0.3580471 |   1.0000000 |                           NA |           NA |                      NA |                    NA |               NA |
| Social distancing motivation | \-0.2915813 |   0.2587121 |           0.0141793 |        0.1170175 |         \-0.3066986 |   0.0015106 |     0.0500811 |          0.0441123 |   0.0905687 |   0.0711845 |      \-0.0145833 | \-0.0908347 |   0.0521132 |       0.0012076 |   \-0.0888182 |   0.0750816 | \-0.2510765 |                    1.0000000 |           NA |                      NA |                    NA |               NA |
| Social media                 |   0.2398268 | \-0.3936570 |           0.1029357 |        0.1055366 |           0.0407408 |   0.0507616 |     0.0055942 |          0.3875101 |   0.0856987 | \-0.0620442 |      \-0.0228397 |   0.1695001 |   0.0402119 |       0.0448742 |     0.1353432 | \-0.0225222 |   0.0262917 |                  \-0.1108653 |    1.0000000 |                      NA |                    NA |               NA |
| Uncertainty intolerance      |   0.1340517 | \-0.2640647 |           0.1557813 |        0.2404121 |           0.0340551 |   0.0361928 |     0.0415397 |          0.1588673 |   0.0846793 | \-0.1146742 |      \-0.0131687 |   0.1006033 |   0.0095418 |       0.0461810 |     0.0852039 |   0.0294074 |   0.0270955 |                  \-0.0314238 |    0.2592348 |               1.0000000 |                    NA |               NA |
| Vaccination attitudes        |   0.0959488 | \-0.1674974 |           0.0909080 |      \-0.1384933 |           0.1644502 | \-0.0066770 |   \-0.0735038 |          0.0147415 |   0.0894715 | \-0.1345180 |      \-0.0540847 |   0.0727313 | \-0.0683376 |       0.0087821 |     0.0098751 |   0.0227439 |   0.0488148 |                  \-0.1237813 |    0.0861778 |               0.0583917 |             1.0000000 |               NA |
| Wuhan laboratory             | \-0.0411027 |   0.0694473 |         \-0.0126038 |        0.1327156 |         \-0.1260939 | \-0.0335189 |     0.1435770 |          0.0064934 | \-0.0235138 |   0.0782161 |        0.0726111 | \-0.0966866 |   0.0351403 |       0.0381964 |     0.0051788 |   0.0240614 |   0.0057042 |                    0.1674625 |  \-0.0243246 |               0.0879339 |           \-0.1168862 |                1 |

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
             "RWA","SDO","W1_Conspiracy_Total")
names(to_plot) <- c("Age","Education","Gender","Income","Elite news",
              "Family and friends","Mid-level news","Social media",
              "Tabloid news","COVID-19 anxiety","Ethnocentrism",
              "Left-right scale","Distrust scientists",
              "Intolerance of uncertainty","RWA",
              "SDO","Conspiracy ideation")

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

Age

</td>

<td>

0.056 (0.041)

</td>

<td>

\-0.037 (0.043)

</td>

<td>

\-0.084 (0.028)<sup>\*\*</sup>

</td>

<td>

\-0.387 (0.219)

</td>

<td>

\-0.945 (0.248)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Conspiracy ideation

</td>

<td>

\-0.010 (0.039)

</td>

<td>

0.274 (0.041)<sup>\*\*\*</sup>

</td>

<td>

0.065 (0.027)<sup>\*</sup>

</td>

<td>

0.463 (0.210)<sup>\*</sup>

</td>

<td>

0.682 (0.236)<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

COVID-19 anxiety

</td>

<td>

0.116 (0.031)<sup>\*\*\*</sup>

</td>

<td>

0.119 (0.032)<sup>\*\*\*</sup>

</td>

<td>

0.022 (0.021)

</td>

<td>

0.222 (0.165)

</td>

<td>

0.290 (0.195)

</td>

</tr>

<tr>

<td style="text-align:left">

Distrust scientists

</td>

<td>

\-0.125 (0.031)<sup>\*\*\*</sup>

</td>

<td>

0.179 (0.033)<sup>\*\*\*</sup>

</td>

<td>

0.199 (0.022)<sup>\*\*\*</sup>

</td>

<td>

1.805 (0.167)<sup>\*\*\*</sup>

</td>

<td>

1.403 (0.169)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Education

</td>

<td>

\-0.031 (0.016)

</td>

<td>

\-0.051 (0.017)<sup>\*\*</sup>

</td>

<td>

\-0.015 (0.011)

</td>

<td>

\-0.143 (0.088)

</td>

<td>

\-0.154 (0.102)

</td>

</tr>

<tr>

<td style="text-align:left">

Elite news

</td>

<td>

0.019 (0.016)

</td>

<td>

\-0.014 (0.017)

</td>

<td>

0.006 (0.011)

</td>

<td>

\-0.062 (0.088)

</td>

<td>

0.003 (0.100)

</td>

</tr>

<tr>

<td style="text-align:left">

Ethnocentrism

</td>

<td>

0.144 (0.034)<sup>\*\*\*</sup>

</td>

<td>

0.117 (0.035)<sup>\*\*\*</sup>

</td>

<td>

0.048 (0.024)<sup>\*</sup>

</td>

<td>

0.362 (0.182)<sup>\*</sup>

</td>

<td>

0.371 (0.213)

</td>

</tr>

<tr>

<td style="text-align:left">

Family and friends

</td>

<td>

\-0.020 (0.029)

</td>

<td>

0.129 (0.030)<sup>\*\*\*</sup>

</td>

<td>

0.086 (0.020)<sup>\*\*\*</sup>

</td>

<td>

0.579 (0.156)<sup>\*\*\*</sup>

</td>

<td>

0.597 (0.182)<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Gender

</td>

<td>

\-0.004 (0.016)

</td>

<td>

0.021 (0.016)

</td>

<td>

0.002 (0.011)

</td>

<td>

0.178 (0.083)<sup>\*</sup>

</td>

<td>

0.086 (0.094)

</td>

</tr>

<tr>

<td style="text-align:left">

Income

</td>

<td>

0.066 (0.022)<sup>\*\*</sup>

</td>

<td>

\-0.063 (0.023)<sup>\*\*</sup>

</td>

<td>

\-0.044 (0.015)<sup>\*\*</sup>

</td>

<td>

\-0.397 (0.118)<sup>\*\*\*</sup>

</td>

<td>

\-0.397 (0.143)<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Intolerance of uncertainty

</td>

<td>

0.137 (0.042)<sup>\*\*</sup>

</td>

<td>

\-0.039 (0.043)

</td>

<td>

0.014 (0.029)

</td>

<td>

0.117 (0.222)

</td>

<td>

0.017 (0.239)

</td>

</tr>

<tr>

<td style="text-align:left">

Left-right scale

</td>

<td>

0.045 (0.045)

</td>

<td>

0.021 (0.046)

</td>

<td>

0.019 (0.031)

</td>

<td>

0.045 (0.237)

</td>

<td>

0.007 (0.232)

</td>

</tr>

<tr>

<td style="text-align:left">

Mid-level news

</td>

<td>

0.005 (0.017)

</td>

<td>

0.100 (0.018)<sup>\*\*\*</sup>

</td>

<td>

0.004 (0.012)

</td>

<td>

0.067 (0.090)

</td>

<td>

0.011 (0.099)

</td>

</tr>

<tr>

<td style="text-align:left">

RWA

</td>

<td>

\-0.059 (0.052)

</td>

<td>

0.145 (0.054)<sup>\*\*</sup>

</td>

<td>

\-0.106 (0.036)<sup>\*\*</sup>

</td>

<td>

\-0.173 (0.280)

</td>

<td>

\-0.481 (0.337)

</td>

</tr>

<tr>

<td style="text-align:left">

SDO

</td>

<td>

\-0.022 (0.051)

</td>

<td>

0.226 (0.052)<sup>\*\*\*</sup>

</td>

<td>

0.249 (0.035)<sup>\*\*\*</sup>

</td>

<td>

2.027 (0.270)<sup>\*\*\*</sup>

</td>

<td>

2.359 (0.309)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Social media

</td>

<td>

\-0.022 (0.027)

</td>

<td>

0.074 (0.028)<sup>\*\*</sup>

</td>

<td>

0.080 (0.019)<sup>\*\*\*</sup>

</td>

<td>

0.778 (0.146)<sup>\*\*\*</sup>

</td>

<td>

0.523 (0.165)<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Tabloid news

</td>

<td>

\-0.0004 (0.018)

</td>

<td>

0.063 (0.018)<sup>\*\*\*</sup>

</td>

<td>

0.063 (0.012)<sup>\*\*\*</sup>

</td>

<td>

0.456 (0.094)<sup>\*\*\*</sup>

</td>

<td>

0.478 (0.097)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

0.444 (0.050)<sup>\*\*\*</sup>

</td>

<td>

\-0.175 (0.052)<sup>\*\*\*</sup>

</td>

<td>

\-0.097 (0.034)<sup>\*\*</sup>

</td>

<td>

\-0.675 (0.266)<sup>\*</sup>

</td>

<td>

0.273 (0.315)

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

0.068

</td>

<td>

0.254

</td>

<td>

0.235

</td>

<td>

0.270

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.057

</td>

<td>

0.245

</td>

<td>

0.226

</td>

<td>

0.261

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
to_plot2 <- c(to_plot,"conspiracy1_sc","conspiracy2_sc","conspiracy3_sc")
names(to_plot2) <- c(names(to_plot),
                     "Wuhan lab belief", "Meat market belief", "5G belief")
to_plot2 <- to_plot2[sort(names(to_plot2))]

stargazer(dist_full, vax_full,
          title="Table A3: Public Health Measures Model Results",
          dep.var.labels=c("Social distancing (OLS)",
                           "Vaccine (multi logit)",
                           "Vaccine (multi logit)"),
          multicolumn = TRUE,
          column.labels = c(" ", "No", "Maybe"),
          model.names = FALSE,
          model.numbers = FALSE,
          order = to_plot2,
          covariate.labels = names(to_plot2),
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

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="2">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="2" style="border-bottom: 1px solid black">

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

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

No

</td>

</tr>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

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

0.787 (0.313)<sup>\*</sup>

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

\-1.567 (0.345)<sup>\*\*\*</sup>

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

0.605 (0.342)

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

\-1.396 (0.258)<sup>\*\*\*</sup>

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

1.353 (0.268)<sup>\*\*\*</sup>

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

0.107 (0.138)

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

\-0.124 (0.139)

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

\-0.629 (0.287)<sup>\*</sup>

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

\-0.082 (0.250)

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

0.239 (0.130)

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

\-0.659 (0.186)<sup>\*\*\*</sup>

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

0.209 (0.348)

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

\-0.493 (0.369)

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

\-0.575 (0.221)<sup>\*\*</sup>

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

0.119 (0.143)

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

0.721 (0.446)

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

0.463 (0.434)

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

0.210 (0.230)

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

\-0.161 (0.149)

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

0.269 (0.222)

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

0.236 (0.429)

</td>

</tr>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

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

1,390

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

</tr>

<tr>

<td style="text-align:left">

Akaike Inf. Crit.

</td>

<td>

</td>

<td>

1,575.584

</td>

</tr>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="2" style="text-align:right">

<sup>*</sup>p\<0.05; <sup>**</sup>p\<0.01; <sup>***</sup>p\<0.001

</td>

</tr>

</table>

``` r
tab <- margin_vax %>% 
  as_tibble()

tab2 <- tibble(factor = to_plot2, variable = names(to_plot2))

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

| variable                   | factor                |         AME |        SE |           z |         p |       lower |       upper |
| :------------------------- | :-------------------- | ----------: | --------: | ----------: | --------: | ----------: | ----------: |
| Age                        | age\_sc               | \-0.2907836 | 0.0594590 | \-4.8904881 | 0.0000010 | \-0.4073212 | \-0.1742461 |
| Wuhan lab belief           | conspiracy1\_sc       |   0.0498416 | 0.0417081 |   1.1950093 | 0.2320834 | \-0.0319048 |   0.1315880 |
| Meat market belief         | conspiracy2\_sc       | \-0.1067233 | 0.0373903 | \-2.8543012 | 0.0043132 | \-0.1800070 | \-0.0334396 |
| 5G belief                  | conspiracy3\_sc       |   0.1460884 | 0.0546744 |   2.6719719 | 0.0075407 |   0.0389286 |   0.2532483 |
| Distrust scientists        | distrust\_science     |   0.2512008 | 0.0488373 |   5.1436277 | 0.0000003 |   0.1554815 |   0.3469201 |
| Elite news                 | elite\_news           | \-0.0230339 | 0.0208487 | \-1.1048142 | 0.2692401 | \-0.0638966 |   0.0178287 |
| Ethnocentrism              | ethno                 | \-0.1167466 | 0.0550446 | \-2.1209458 | 0.0339264 | \-0.2246321 | \-0.0088612 |
| Mid-level news             | mid\_level\_news      |   0.0221711 | 0.0262931 |   0.8432278 | 0.3991010 | \-0.0293625 |   0.0737047 |
| Tabloid news               | red\_top\_tabloid     | \-0.0298840 | 0.0272256 | \-1.0976431 | 0.2723604 | \-0.0832452 |   0.0234772 |
| Left-right scale           | right                 | \-0.0915025 | 0.0718118 | \-1.2741984 | 0.2025931 | \-0.2322511 |   0.0492461 |
| RWA                        | RWA                   |   0.1337664 | 0.0747922 |   1.7885067 | 0.0736943 | \-0.0128237 |   0.2803565 |
| SDO                        | SDO                   |   0.0859249 | 0.0801025 |   1.0726857 | 0.2834122 | \-0.0710733 |   0.2429230 |
| COVID-19 anxiety           | threat                | \-0.2591414 | 0.0485674 | \-5.3357094 | 0.0000001 | \-0.3543317 | \-0.1639511 |
| Conspiracy ideation        | W1\_Conspiracy\_Total |   0.1123754 | 0.0626658 |   1.7932508 | 0.0729328 | \-0.0104472 |   0.2351981 |
| Education                  | W1\_Education\_binary |   0.0199485 | 0.0257786 |   0.7738394 | 0.4390258 | \-0.0305766 |   0.0704736 |
| Income                     | W1\_Income\_2019      | \-0.1224070 | 0.0336256 | \-3.6402885 | 0.0002723 | \-0.1883120 | \-0.0565020 |
| Gender                     | W2\_Gender\_binary2   |   0.0445809 | 0.0243419 |   1.8314475 | 0.0670338 | \-0.0031283 |   0.0922901 |
| Social media               | W2\_INFO\_5           |   0.0390443 | 0.0528541 |   0.7387178 | 0.4600784 | \-0.0645479 |   0.1426364 |
| Family and friends         | W2\_INFO\_9           | \-0.0151959 | 0.0466927 | \-0.3254450 | 0.7448443 | \-0.1067118 |   0.0763200 |
| Intolerance of uncertainty | W2\_IOU\_Total        |   0.0388067 | 0.0655845 |   0.5917051 | 0.5540481 | \-0.0897365 |   0.1673499 |

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
```

    ## Warning: Unknown columns: `social_distance`

``` r
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
    ##   W1_Conspiracy_Total    w2_conspiracy3_ihs W2_Conspiracy_Theory1 
    ##                     0                     0                     0 
    ## W2_Conspiracy_Theory2 W2_Conspiracy_Theory3        conspiracy1_sc 
    ##                     0                     0                     0 
    ##        conspiracy2_sc        conspiracy3_sc   W2_SocialDistance10 
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
