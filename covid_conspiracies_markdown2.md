covid\_conspiracies\_markdown2
================
Michael Marshall
21/10/2020

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
  mutate(
    conspiracy_code = as.factor(conspiracy_code),
    conspiracy = ifelse(
      conspiracy_code == "W2_Conspiracy_Theory1",
      "Chinese lab",
      ifelse(conspiracy_code == "W2_Conspiracy_Theory2",
             "Chinese meat market",
             ifelse(conspiracy_code == "W2_Conspiracy_Theory3",
                    "5G",
                    ifelse(conspiracy_code == "W2_Conspiracy_Theory4",
                           "No worse than flu",
                           "Vitamin C treatment"))))
  ) %>% 
  ggplot(aes(x = belief, y = conspiracy, height = ..density..)) +
  geom_density_ridges(aes(fill = conspiracy,
                          rel_min_height = 0.005),
                      stat = "density",
                      #bins = 20,
                      show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  theme_ridges()
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Warning: Removed 7360 rows containing non-finite values (stat_density).

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

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

The code below combines the two variables on the 2019 general election
into a single variable that combines whether a respondent voted, and who
they voted for. It also turns the *preferred newspaper* variables into
dummy variables, as they were previously coded as *1=Yes* and everything
else as *NA*.

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
# [nat] nationalism
nat_keys <- list(nationalism = cs(W2_Nationalism1,W2_Nationalism2))
nat_test <- scoreItems(nat_keys, conspiracies, min = 1, max = 5)
head(nat_test$scores)
```

    ##      nationalism
    ## [1,]           4
    ## [2,]           4
    ## [3,]           4
    ## [4,]           3
    ## [5,]           3
    ## [6,]           3

``` r
nat_test$alpha  # Scale alpha
```

    ##       nationalism
    ## alpha   0.8213221

``` r
conspiracies$nat <- rescale01(nat_test$scores, na.rm = TRUE)
conspiracies$nat <- c(conspiracies$nat)  # Ensure variable is numeric and not matrix class
describe(conspiracies$nat)
```

    ##    vars    n mean   sd median trimmed  mad min max range  skew kurtosis   se
    ## X1    1 1406 0.57 0.25   0.62    0.58 0.19   0   1     1 -0.33    -0.18 0.01

``` r
## [imm.econ] Anti-immigrant sentiment - Economy 
table(conspiracies$W1_MigrantAttitudes1)
```

    ## 
    ##   1   2   3   4   5   6   7   8   9  10 
    ##  71  38  80 105 188 198 264 255  87 120

``` r
conspiracies$imm_econ <- rescale01(abs(
  conspiracies$W1_MigrantAttitudes1 - 11))

conspiracies %>% 
  dplyr::select(W1_MigrantAttitudes1, imm_econ) %>%
  correlate()
```

    ## 
    ## Correlation method: 'pearson'
    ## Missing treated using: 'pairwise.complete.obs'

    ## # A tibble: 2 x 3
    ##   rowname              W1_MigrantAttitudes1 imm_econ
    ##   <chr>                               <dbl>    <dbl>
    ## 1 W1_MigrantAttitudes1                   NA       -1
    ## 2 imm_econ                               -1       NA

``` r
## [imm.res] Anti-immigrant sentiment - Resources
table(conspiracies$W1_MigrantAttitudes3)
```

    ## 
    ##   1   2   3   4   5 
    ##  68 101 575 431 231

``` r
conspiracies$imm_res <- rescale01(conspiracies$W1_MigrantAttitudes3)
conspiracies %>% 
  dplyr::select(W1_MigrantAttitudes3, imm_res) %>%
  correlate()
```

    ## 
    ## Correlation method: 'pearson'
    ## Missing treated using: 'pairwise.complete.obs'

    ## # A tibble: 2 x 3
    ##   rowname              W1_MigrantAttitudes3 imm_res
    ##   <chr>                               <dbl>   <dbl>
    ## 1 W1_MigrantAttitudes3                   NA       1
    ## 2 imm_res                                 1      NA

``` r
## [imm.cul] Anti-immigrant sentiment - Culture
table(conspiracies$W1_MigrantAttitudes2)
```

    ## 
    ##   1   2   3   4   5   6   7   8   9  10 
    ## 101  57  88 120 202 177 225 219  80 137

``` r
conspiracies$imm_cul <- rescale01(abs(
  conspiracies$W1_MigrantAttitudes2 - 11))

conspiracies %>% 
  dplyr::select(W1_MigrantAttitudes2, imm_cul) %>%
  correlate()
```

    ## 
    ## Correlation method: 'pearson'
    ## Missing treated using: 'pairwise.complete.obs'

    ## # A tibble: 2 x 3
    ##   rowname              W1_MigrantAttitudes2 imm_cul
    ##   <chr>                               <dbl>   <dbl>
    ## 1 W1_MigrantAttitudes2                   NA      -1
    ## 2 imm_cul                                -1      NA

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
factors <- c("W1_Ethnicity","W1_C19_Infected","W1_BornUK","W1_EURef",
             "W2_Gender_binary","W2_Living_alone","W2_Employment",
             "W1_Education_binary","W1_Housing_tenure")
```

``` r
# turning the above list into factors with the levels as the spss labels
#for(i in seq_along(conspiracies[factors])){
#  conspiracies[factors][,i] <- to_factor(
#    conspiracies[factors][,i], nolabel_to_na = TRUE)
#}
```

``` r
# turning to factors
conspiracies[factors] <- conspiracies[factors] %>% 
  map_df(as.factor)
```

``` r
# Making a binary for each CRT scale, baseline = incorrect answer
conspiracies$CRT1 <- ifelse(
  to_factor(conspiracies$W1_CRT1) == "5 pence", 1, 0
)

conspiracies$CRT2 <- ifelse(
  to_factor(conspiracies$W1_CRT2) == "5 minutes", 1, 0
)

conspiracies$CRT3 <- ifelse(
  to_factor(conspiracies$W1_CRT3) == "47 days", 1, 0
)

conspiracies$CRT4 <- ifelse(
  to_factor(conspiracies$W1_CRT4) == "2nd", 1, 0
)

conspiracies$CRT5 <- ifelse(
  to_factor(conspiracies$W1_CRT5) == "8", 1, 0
)

conspiracies$CRT_test <- ifelse(
  to_factor(conspiracies$W1_CRT_test) == "None of them.", 1, 0
) # baseline = heard of some of them OR all of them
```

``` r
# CRT
crt_keys <- list(crt = cs(CRT1,
                          CRT2,
                          CRT3,
                          CRT4, 
                          CRT5
                          ))

crt_test <- scoreItems(crt_keys, conspiracies, min = 0, max = 1)
head(crt_test$scores)
```

    ##      crt
    ## [1,] 0.2
    ## [2,] 0.4
    ## [3,] 0.0
    ## [4,] 0.2
    ## [5,] 0.0
    ## [6,] 0.0

``` r
summary(crt_test$alpha)  # Scale alpha
```

    ##       crt        
    ##  Min.   :0.7332  
    ##  1st Qu.:0.7332  
    ##  Median :0.7332  
    ##  Mean   :0.7332  
    ##  3rd Qu.:0.7332  
    ##  Max.   :0.7332

``` r
conspiracies$crt <- rescale01(crt_test$scores, na.rm = TRUE)
conspiracies$crt <- c(conspiracies$crt)  # Ensure variable is numeric and not matrix class

describe(conspiracies$crt)
```

    ##    vars    n mean   sd median trimmed mad min max range skew kurtosis   se
    ## X1    1 1406 0.39 0.33    0.4    0.36 0.3   0   1     1 0.41    -1.06 0.01

``` r
# do CRT scores come from different distributions
# dependent on whether they've seen the test

conspiracies %>% 
  group_by(CRT_test) %>% 
  summarise(
    mean_crt = mean(crt),
    median_crt = median(crt),
    std_crt = sd(crt)
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 4
    ##   CRT_test mean_crt median_crt std_crt
    ##      <dbl>    <dbl>      <dbl>   <dbl>
    ## 1        0    0.478        0.4   0.335
    ## 2        1    0.334        0.2   0.314

``` r
kruskal.test(crt ~ CRT_test, data = conspiracies)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  crt by CRT_test
    ## Kruskal-Wallis chi-squared = 61.72, df = 1, p-value = 3.96e-15

``` r
# renaming trust in science
conspiracies <- conspiracies %>% 
  rename(distrust_science = W2_Trust_Body6)
```

``` r
# rescaling the remaing numeric variables
numerics <- c("W1_Conspiracy_Total","W2_Paranoia_Total",
              "W2_Internal_Total","W2_Chance_Total","W2_PO_Total",
              "W2_DAI_Total","W2_IOU_Total", "W2_INFO_5",
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
    one_of(numerics),nat,RWA,SDO,threat,right,soc_con,fis_con
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

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-3.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-4.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-5.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-6.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-7.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-8.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-9.png)<!-- -->

    ## Warning: Removed 4 rows containing non-finite values (stat_density).

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-10.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-11.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-12.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-13.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-14.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-15.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-16.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-17.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-18.png)<!-- -->

# Principal Components Analysis - PCA

``` r
# pca on conspiracy theory variables
pca_df <- conspiracies %>% 
  dplyr::select(W2_Conspiracy_Theory1:W2_Conspiracy_Theory5)

pca_fit <- prcomp(pca_df,scale = TRUE)
```

``` r
# looking at biplots for components 1:4
biplot(pca_fit,
       col = c("lightgrey","red"))
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
biplot(pca_fit, choices = 3:4,
       col = c("lightgrey","red"))
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-26-2.png)<!-- -->

``` r
pca_fit$rotation[,1:4]
```

    ##                               PC1         PC2        PC3          PC4
    ## W2_Conspiracy_Theory1  0.38903347  0.24539595 -0.8871956  0.005348538
    ## W2_Conspiracy_Theory2 -0.06373755 -0.95279476 -0.2915166  0.055301539
    ## W2_Conspiracy_Theory3  0.56176975 -0.06995748  0.2010846  0.389643958
    ## W2_Conspiracy_Theory4  0.46001469 -0.12890497  0.1587345 -0.862444840
    ## W2_Conspiracy_Theory5  0.56337529 -0.10223712  0.2495403  0.318244587

``` r
components <- pca_fit$x[,1:4] %>% as_tibble()
```

First four principal components are as follows:  
1\. general conspiracy ideation, with belief in 5G, ‘it’s no worse than
flu’ and vitamin C treatment highly loaded to this component, and belief
in Chinese lab less so  
2\. disbelief in Chinese meat market origin, with mild positive loading
for Chinese lab origin  
3\. strong conspiracy ideation (non-nationalistic), strong loading for
belig 5g, ‘it’s no worse than flu’ and vitamin C treatment, with
negative loading for both Chinese lab origin and Chinese meat market  
4\. strong loading for belief in 5G conspiracy and vitamin C treatment,
but strong negative loading for belief ‘it’s no worse than flu’

``` r
pc <- cbind(conspiracies,components)

pc <- pc %>% 
  mutate(
    pc1_ihs = asinh(PC1)
  )

pc %>% 
  ggplot(aes(x = pc1_ihs)) +
  geom_density() +
  labs(x = "Principal Component 1 (IHS)")
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
pc %>% 
  ggplot(aes(x = PC2)) +
  geom_density() +
  labs(x = "Principal Component 2")
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-27-2.png)<!-- -->

``` r
pc %>% 
  ggplot(aes(x = PC3)) +
  geom_density() +
  labs(x = "Principal Component 3")
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-27-3.png)<!-- -->

``` r
pc %>% 
  ggplot(aes(x = PC4)) +
  geom_density()  +
  labs(x = "Principal Component 4")
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-27-4.png)<!-- -->

Principal components 1 to 3 explain most of the variation in the data.

``` r
# modelling principal components against general conspiracy ideation
conspiracy_mod <- lm(W1_Conspiracy_Total ~ pc1_ihs + PC2 + PC3 + PC4,
                     data = pc)

par(mfrow = c(2,2))
plot(conspiracy_mod)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
summ(conspiracy_mod, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W1_Conspiracy_Total
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(4,1401) = 20.56, p = 0.00
    ## R² = 0.06
    ## Adj. R² = 0.05 
    ## 
    ## Standard errors: OLS
    ## -------------------------------------------------------
    ##                      Est.   S.E.   t val.      p    VIF
    ## ----------------- ------- ------ -------- ------ ------
    ## (Intercept)          0.58   0.01   111.45   0.00       
    ## pc1_ihs              0.03   0.01     6.56   0.00   1.01
    ## PC2                  0.01   0.01     1.23   0.22   1.00
    ## PC3                 -0.03   0.01    -5.78   0.00   1.00
    ## PC4                  0.00   0.01     0.05   0.96   1.00
    ## -------------------------------------------------------

``` r
source("diagnostic_plots.R")
```

Only principal components 1 and 3 associated with general conspiracy
ideation.

# Modelling for belief in Chinese lab origin

## DV Chinese lab conspiracy - singular IV models

``` r
sdo_lab <- lm(W2_Conspiracy_Theory1 ~ SDO,
              data = conspiracies)

summ(sdo_lab)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 65.28, p = 0.00
    ## R² = 0.04
    ## Adj. R² = 0.04 
    ## 
    ## Standard errors: OLS
    ## ------------------------------------------------
    ##                      Est.   S.E.   t val.      p
    ## ----------------- ------- ------ -------- ------
    ## (Intercept)         24.11   1.95    12.34   0.00
    ## SDO                 39.08   4.84     8.08   0.00
    ## ------------------------------------------------

``` r
rwa_lab <- lm(W2_Conspiracy_Theory1 ~ RWA,
              data = conspiracies)

summ(rwa_lab)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 87.72, p = 0.00
    ## R² = 0.06
    ## Adj. R² = 0.06 
    ## 
    ## Standard errors: OLS
    ## ------------------------------------------------
    ##                      Est.   S.E.   t val.      p
    ## ----------------- ------- ------ -------- ------
    ## (Intercept)         13.88   2.74     5.07   0.00
    ## RWA                 47.40   5.06     9.37   0.00
    ## ------------------------------------------------

``` r
mid_news_lab <- lm(W2_Conspiracy_Theory1 ~ mid_level_news,
              data = conspiracies)

summ(mid_news_lab)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 96.79, p = 0.00
    ## R² = 0.06
    ## Adj. R² = 0.06 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------
    ##                         Est.   S.E.   t val.      p
    ## -------------------- ------- ------ -------- ------
    ## (Intercept)            32.48   1.04    31.29   0.00
    ## mid_level_news         18.08   1.84     9.84   0.00
    ## ---------------------------------------------------

``` r
social_m_lab <- lm(W2_Conspiracy_Theory1 ~ W2_INFO_5,
              data = conspiracies)

summ(social_m_lab)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 43.67, p = 0.00
    ## R² = 0.03
    ## Adj. R² = 0.03 
    ## 
    ## Standard errors: OLS
    ## ------------------------------------------------
    ##                      Est.   S.E.   t val.      p
    ## ----------------- ------- ------ -------- ------
    ## (Intercept)         32.81   1.20    27.33   0.00
    ## W2_INFO_5           18.00   2.72     6.61   0.00
    ## ------------------------------------------------

## DV Chinese lab conspiracy - socio-economic variables

``` r
se_lab <- lm(W2_Conspiracy_Theory1 ~ W2_Gender_binary +
               W1_Education_binary +
               W1_Income_2019 +
               age_sc,
              data = conspiracies)

summ(se_lab, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1403 (3 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(4,1398) = 13.24, p = 0.00
    ## R² = 0.04
    ## Adj. R² = 0.03 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  50.87   2.87    17.74   0.00       
    ## W2_Gender_binary2             2.76   1.77     1.56   0.12   1.03
    ## W1_Education_binary1         -8.28   1.84    -4.50   0.00   1.06
    ## W1_Income_2019               -9.34   2.50    -3.73   0.00   1.06
    ## age_sc                       -8.94   4.14    -2.16   0.03   1.03
    ## ----------------------------------------------------------------

## DV Chinese lab conspiracy - socio-economic variables + political/media

``` r
se_pol_lab <- lm(W2_Conspiracy_Theory1 ~ 
                   
                   #socio-economic variables
                   W2_Gender_binary +
                   W1_Education_binary +
                   W1_Income_2019 +
                   age_sc +
                   
                   #political and media variables
                   right +
                   soc_con +
                   fis_con +
                   nat +
                   distrust_science + #distrust of scientists
                   red_top_tabloid + 
                   mid_level_news + 
                   elite_news + 
                   W2_INFO_5 + #social media
                   W2_INFO_9, #family and friends
                 data = conspiracies)

summ(se_pol_lab, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(14,1384) = 25.53, p = 0.00
    ## R² = 0.21
    ## Adj. R² = 0.20 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                   8.00   4.10     1.95   0.05       
    ## W2_Gender_binary2             3.12   1.64     1.90   0.06   1.06
    ## W1_Education_binary1         -5.00   1.72    -2.91   0.00   1.12
    ## W1_Income_2019               -6.41   2.36    -2.72   0.01   1.13
    ## age_sc                       -0.73   4.18    -0.18   0.86   1.26
    ## right                         6.37   5.48     1.16   0.25   1.97
    ## soc_con                       8.88   3.57     2.49   0.01   1.51
    ## fis_con                      -0.20   5.23    -0.04   0.97   2.08
    ## nat                          16.16   3.62     4.47   0.00   1.28
    ## distrust_science             22.61   3.30     6.84   0.00   1.08
    ## red_top_tabloid               6.71   1.87     3.58   0.00   1.08
    ## mid_level_news               10.95   1.80     6.07   0.00   1.12
    ## elite_news                   -2.94   1.73    -1.70   0.09   1.11
    ## W2_INFO_5                     9.73   2.91     3.35   0.00   1.36
    ## W2_INFO_9                    13.39   3.09     4.33   0.00   1.21
    ## ----------------------------------------------------------------

``` r
AIC(se_lab)
```

    ## [1] 13768.48

``` r
AIC(se_pol_lab)
```

    ## [1] 13478.77

## DV Chinese lab conspiracy - socio-economic variables + political/media + pol-psych

``` r
se_polpsych_lab <- lm(W2_Conspiracy_Theory1 ~
                        
                        #socio-economic variables
                        W2_Gender_binary +
                        W1_Education_binary +
                        W1_Income_2019 +
                        age_sc +
                        
                        #political and media variables
                        right +
                        soc_con +
                        fis_con +
                        nat +
                        distrust_science + #distrust of scientists
                        red_top_tabloid + 
                        mid_level_news + 
                        elite_news + 
                        W2_INFO_5 + #social media
                        W2_INFO_9 + #family and friends
                     
                        #political-psychology variables
                        SDO +
                        RWA +
                        W2_DAI_Total +
                        W2_IOU_Total +
                        W2_Paranoia_Total +
                        W2_Internal_Total +
                        W2_Chance_Total +
                        W2_PO_Total,
                      data = conspiracies)

summ(se_polpsych_lab, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(22,1376) = 19.90, p = 0.00
    ## R² = 0.24
    ## Adj. R² = 0.23 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -9.53   5.92    -1.61   0.11       
    ## W2_Gender_binary2              2.60   1.66     1.56   0.12   1.14
    ## W1_Education_binary1          -4.08   1.70    -2.41   0.02   1.13
    ## W1_Income_2019                -5.00   2.39    -2.09   0.04   1.21
    ## age_sc                         5.02   4.42     1.14   0.26   1.47
    ## right                          2.72   5.51     0.49   0.62   2.08
    ## soc_con                        1.73   3.70     0.47   0.64   1.69
    ## fis_con                       -1.44   5.17    -0.28   0.78   2.12
    ## nat                            9.94   3.66     2.72   0.01   1.36
    ## distrust_science              17.81   3.39     5.26   0.00   1.18
    ## red_top_tabloid                6.19   1.84     3.36   0.00   1.09
    ## mid_level_news                10.30   1.78     5.80   0.00   1.13
    ## elite_news                    -1.74   1.71    -1.02   0.31   1.13
    ## W2_INFO_5                      7.61   2.89     2.64   0.01   1.40
    ## W2_INFO_9                     13.07   3.06     4.27   0.00   1.24
    ## SDO                           11.41   5.34     2.13   0.03   1.51
    ## RWA                           20.15   5.69     3.54   0.00   1.53
    ## W2_DAI_Total                  17.13   4.38     3.91   0.00   1.54
    ## W2_IOU_Total                 -12.31   4.98    -2.47   0.01   1.59
    ## W2_Paranoia_Total             13.55   4.28     3.16   0.00   1.79
    ## W2_Internal_Total              5.77   5.07     1.14   0.26   1.26
    ## W2_Chance_Total               -1.30   5.32    -0.24   0.81   1.93
    ## W2_PO_Total                    6.32   4.96     1.27   0.20   2.23
    ## -----------------------------------------------------------------

Looking at mulitcollinearity below for the following variables:

  - right  
  - soc\_con  
  - fis\_con  
  - paranoia

<!-- end list -->

``` r
# looking for potential multicollinearity with right
cor_mat <- model.matrix(se_polpsych_lab) %>% as.data.frame()

cor_vec <- rep(NA,ncol(cor_mat))
for(i in seq_along(cor_mat)){
  cor_vec[i] <- cor(cor_mat$right,cor_mat[,i])
}
```

    ## Warning in cor(cor_mat$right, cor_mat[, i]): the standard deviation is zero

``` r
tibble(cor_vec,
       names(cor_mat)) %>% 
  arrange(desc(cor_vec))
```

    ## # A tibble: 23 x 2
    ##    cor_vec `names(cor_mat)` 
    ##      <dbl> <chr>            
    ##  1   1     right            
    ##  2   0.668 fis_con          
    ##  3   0.460 soc_con          
    ##  4   0.445 SDO              
    ##  5   0.379 nat              
    ##  6   0.376 RWA              
    ##  7   0.239 mid_level_news   
    ##  8   0.149 W2_Internal_Total
    ##  9   0.135 age_sc           
    ## 10   0.112 W1_Income_2019   
    ## # ... with 13 more rows

Fiscal conservatism positively correlated with right-left scale.

``` r
# looking for multicollinearity with soc_con
cor_vec2 <- rep(NA,ncol(cor_mat))
for(i in seq_along(cor_mat)){
  cor_vec2[i] <- cor(cor_mat$soc_con,cor_mat[,i])
}
```

    ## Warning in cor(cor_mat$soc_con, cor_mat[, i]): the standard deviation is zero

``` r
tibble(cor_vec2,
       names(cor_mat)) %>% 
  arrange(desc(cor_vec2))
```

    ## # A tibble: 23 x 2
    ##    cor_vec2 `names(cor_mat)` 
    ##       <dbl> <chr>            
    ##  1    1     soc_con          
    ##  2    0.503 fis_con          
    ##  3    0.460 right            
    ##  4    0.448 RWA              
    ##  5    0.380 SDO              
    ##  6    0.261 nat              
    ##  7    0.199 distrust_science 
    ##  8    0.192 mid_level_news   
    ##  9    0.151 W2_DAI_Total     
    ## 10    0.114 W2_Paranoia_Total
    ## # ... with 13 more rows

Fiscal conservatism, right-wing scale and RWA correlated with soc\_con.

``` r
# looking for multicollinearity with fis_con
cor_vec3 <- rep(NA,ncol(cor_mat))
for(i in seq_along(cor_mat)){
  cor_vec3[i] <- cor(cor_mat$fis_con,cor_mat[,i])
}
```

    ## Warning in cor(cor_mat$fis_con, cor_mat[, i]): the standard deviation is zero

``` r
tibble(cor_vec3,
       names(cor_mat)) %>% 
  arrange(desc(cor_vec3))
```

    ## # A tibble: 23 x 2
    ##    cor_vec3 `names(cor_mat)` 
    ##       <dbl> <chr>            
    ##  1    1.00  fis_con          
    ##  2    0.668 right            
    ##  3    0.503 soc_con          
    ##  4    0.404 SDO              
    ##  5    0.381 nat              
    ##  6    0.362 RWA              
    ##  7    0.216 mid_level_news   
    ##  8    0.157 W2_Internal_Total
    ##  9    0.131 W1_Income_2019   
    ## 10    0.128 age_sc           
    ## # ... with 13 more rows

Right, soc\_con correlated with fis\_con.

``` r
# looking for multicollinearity with paranoia
cor_vec4 <- rep(NA,ncol(cor_mat))
for(i in seq_along(cor_mat)){
  cor_vec4[i] <- cor(cor_mat$W2_Paranoia_Total,cor_mat[,i])
}
```

    ## Warning in cor(cor_mat$W2_Paranoia_Total, cor_mat[, i]): the standard deviation
    ## is zero

``` r
tibble(cor_vec4,
       names(cor_mat)) %>% 
  arrange(desc(cor_vec4))
```

    ## # A tibble: 23 x 2
    ##    cor_vec4 `names(cor_mat)` 
    ##       <dbl> <chr>            
    ##  1    1     W2_Paranoia_Total
    ##  2    0.491 W2_PO_Total      
    ##  3    0.463 W2_IOU_Total     
    ##  4    0.441 W2_DAI_Total     
    ##  5    0.418 W2_Chance_Total  
    ##  6    0.247 W2_INFO_5        
    ##  7    0.237 distrust_science 
    ##  8    0.158 SDO              
    ##  9    0.137 W2_INFO_9        
    ## 10    0.125 red_top_tabloid  
    ## # ... with 13 more rows

Potential multicollinearity between right, soc\_con and fis\_con. From
hereon, fis\_con is included in the models, as of the three, it shows
the least positive correlation between SDO and RWA. In addition, the
variables relating to locus of control (W2\_Internal\_Total,
W2\_Chance\_Total) are dropped due to non-significance.

## DV Chinese lab belief - model above minus the aforementioned variables

``` r
se_polpsych_lab_2 <- lm(W2_Conspiracy_Theory1 ~
                        
                        #socio-economic variables
                        W2_Gender_binary +
                        W1_Education_binary +
                        W1_Income_2019 +
                        age_sc +
                        
                        #political and media variables
                        fis_con +
                        nat +
                        distrust_science + #distrust of scientists
                        red_top_tabloid + 
                        mid_level_news + 
                        elite_news + 
                        W2_INFO_5 + #social media
                        W2_INFO_9 + #family and friends
                     
                        #political-psychology variables
                        SDO +
                        RWA +
                        W2_DAI_Total +
                        W2_IOU_Total +
                        W2_Paranoia_Total,
                      data = conspiracies)

summ(se_polpsych_lab_2, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(17,1381) = 25.61, p = 0.00
    ## R² = 0.24
    ## Adj. R² = 0.23 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -5.30   4.82    -1.10   0.27       
    ## W2_Gender_binary2              2.23   1.63     1.37   0.17   1.10
    ## W1_Education_binary1          -4.05   1.69    -2.40   0.02   1.12
    ## W1_Income_2019                -4.86   2.35    -2.07   0.04   1.18
    ## age_sc                         5.32   4.37     1.22   0.22   1.44
    ## fis_con                        0.77   4.26     0.18   0.86   1.44
    ## nat                           10.70   3.60     2.97   0.00   1.33
    ## distrust_science              18.04   3.31     5.45   0.00   1.13
    ## red_top_tabloid                6.27   1.84     3.41   0.00   1.09
    ## mid_level_news                10.42   1.77     5.90   0.00   1.12
    ## elite_news                    -1.81   1.71    -1.06   0.29   1.13
    ## W2_INFO_5                      7.76   2.88     2.69   0.01   1.40
    ## W2_INFO_9                     13.48   3.04     4.43   0.00   1.23
    ## SDO                           12.06   5.20     2.32   0.02   1.43
    ## RWA                           20.58   5.42     3.80   0.00   1.39
    ## W2_DAI_Total                  17.95   4.33     4.15   0.00   1.51
    ## W2_IOU_Total                 -11.52   4.86    -2.37   0.02   1.52
    ## W2_Paranoia_Total             14.52   4.10     3.54   0.00   1.64
    ## -----------------------------------------------------------------

``` r
AIC(se_pol_lab)
```

    ## [1] 13478.77

``` r
AIC(se_polpsych_lab_2)
```

    ## [1] 13422.8

## DV Chinese lab conspiracy - socio-economic variables + political/media + pol-psych + covid-threat + CRT

``` r
multi_lab <- lm(W2_Conspiracy_Theory1 ~
                  
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  fis_con +
                  nat +
                  distrust_science + #distrust of scientists
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news + 
                  W2_INFO_5 + #social media
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  
                  #covid-anxety
                  threat +
                  
                  #CRT
                  crt +
                  CRT_test,
                data = conspiracies)

summ(multi_lab, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(20,1378) = 23.63, p = 0.00
    ## R² = 0.26
    ## Adj. R² = 0.24 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -1.65   5.30    -0.31   0.76       
    ## W2_Gender_binary2              1.24   1.63     0.76   0.45   1.12
    ## W1_Education_binary1          -3.74   1.68    -2.23   0.03   1.13
    ## W1_Income_2019                -3.93   2.36    -1.67   0.10   1.20
    ## age_sc                         0.98   4.42     0.22   0.82   1.50
    ## fis_con                        1.76   4.22     0.42   0.68   1.44
    ## nat                           10.37   3.57     2.90   0.00   1.33
    ## distrust_science              17.89   3.30     5.43   0.00   1.14
    ## red_top_tabloid                5.50   1.83     3.00   0.00   1.10
    ## mid_level_news                10.13   1.75     5.78   0.00   1.12
    ## elite_news                    -1.70   1.69    -1.00   0.32   1.13
    ## W2_INFO_5                      6.36   2.88     2.21   0.03   1.42
    ## W2_INFO_9                     11.62   3.04     3.82   0.00   1.25
    ## SDO                           13.62   5.20     2.62   0.01   1.46
    ## RWA                           16.26   5.43     3.00   0.00   1.42
    ## W2_DAI_Total                  13.57   4.39     3.09   0.00   1.58
    ## W2_IOU_Total                 -11.14   4.86    -2.29   0.02   1.55
    ## W2_Paranoia_Total             12.89   4.08     3.16   0.00   1.65
    ## threat                         7.72   3.30     2.34   0.02   1.21
    ## crt                           -9.96   2.67    -3.73   0.00   1.30
    ## CRT_test                       3.12   1.67     1.87   0.06   1.10
    ## -----------------------------------------------------------------

``` r
AIC(se_polpsych_lab_2)
```

    ## [1] 13422.8

``` r
AIC(multi_lab)
```

    ## [1] 13399.58

## DV Chinese lab belief - interactions

``` r
int_lab <- lm(W2_Conspiracy_Theory1 ~
                  
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  fis_con +
                  nat +
                  distrust_science + #distrust of scientists
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news + 
                  W2_INFO_5 +
                  W2_INFO_9 +
                  
                  #political-psychology variables
                  #SDO +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  
                  #covid-anxiety
                  threat +
                  
                  #CRT
                  #crt +
                  CRT_test +
                
                  #interactions
                  (crt*RWA) +
                  (crt*SDO),
                data = conspiracies)

summ(int_lab, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(22,1376) = 21.47, p = 0.00
    ## R² = 0.26
    ## Adj. R² = 0.24 
    ## 
    ## Standard errors: OLS
    ## -------------------------------------------------------------------
    ##                                Est.    S.E.   t val.      p     VIF
    ## -------------------------- -------- ------- -------- ------ -------
    ## (Intercept)                   -3.42    6.49    -0.53   0.60        
    ## W2_Gender_binary2              1.21    1.64     0.74   0.46    1.12
    ## W1_Education_binary1          -3.74    1.68    -2.22   0.03    1.13
    ## W1_Income_2019                -3.88    2.36    -1.64   0.10    1.21
    ## age_sc                         0.89    4.43     0.20   0.84    1.51
    ## fis_con                        1.92    4.24     0.45   0.65    1.45
    ## nat                           10.34    3.57     2.89   0.00    1.33
    ## distrust_science              17.82    3.31     5.38   0.00    1.15
    ## red_top_tabloid                5.47    1.84     2.98   0.00    1.10
    ## mid_level_news                10.12    1.75     5.77   0.00    1.12
    ## elite_news                    -1.67    1.70    -0.98   0.33    1.13
    ## W2_INFO_5                      6.31    2.88     2.19   0.03    1.42
    ## W2_INFO_9                     11.69    3.05     3.83   0.00    1.25
    ## W2_DAI_Total                  13.44    4.40     3.05   0.00    1.59
    ## W2_IOU_Total                 -11.06    4.87    -2.27   0.02    1.56
    ## W2_Paranoia_Total             12.92    4.09     3.16   0.00    1.66
    ## threat                         7.74    3.30     2.35   0.02    1.21
    ## CRT_test                       3.14    1.68     1.87   0.06    1.11
    ## crt                           -6.48    8.03    -0.81   0.42   11.73
    ## RWA                           17.90    8.18     2.19   0.03    3.23
    ## SDO                           15.86    7.77     2.04   0.04    3.24
    ## crt:RWA                       -3.09   15.36    -0.20   0.84   12.47
    ## crt:SDO                       -5.28   14.75    -0.36   0.72    7.96
    ## -------------------------------------------------------------------

``` r
AIC(multi_lab)
```

    ## [1] 13399.58

``` r
AIC(int_lab) # no support for interactions
```

    ## [1] 13403.32

## DV Chinese lab conspiracy - full model incl. conspiracy ideation

Re-adding in LOC variables for comprehensiveness and transparency.

``` r
full_lab <- lm(W2_Conspiracy_Theory1 ~
                  
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  fis_con +
                  nat +
                  distrust_science + #distrust of scientists
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news +
                  W2_INFO_5 + #social media
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  W2_Chance_Total +
                  W2_PO_Total +
                  W2_Internal_Total +
                  
                  #covid-anxiety
                  threat +
                 
                  #CRT
                  crt +
                  CRT_test +
                  
                  #conspiracies
                  W1_Conspiracy_Total +
                  conspiracy2_sc +
                  conspiracy3_sc,
                data = conspiracies)

summ(full_lab, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(26,1372) = 22.94, p = 0.00
    ## R² = 0.30
    ## Adj. R² = 0.29 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                  -11.10   6.32    -1.76   0.08       
    ## W2_Gender_binary2              1.21   1.59     0.76   0.45   1.14
    ## W1_Education_binary1          -3.82   1.63    -2.34   0.02   1.13
    ## W1_Income_2019                -3.20   2.32    -1.38   0.17   1.23
    ## age_sc                         1.55   4.33     0.36   0.72   1.53
    ## fis_con                        2.16   4.12     0.53   0.60   1.46
    ## nat                           10.63   3.51     3.03   0.00   1.36
    ## distrust_science              10.37   3.34     3.10   0.00   1.25
    ## red_top_tabloid                4.21   1.79     2.35   0.02   1.12
    ## mid_level_news                10.12   1.70     5.95   0.00   1.12
    ## elite_news                    -1.29   1.65    -0.78   0.43   1.14
    ## W2_INFO_5                      3.96   2.81     1.41   0.16   1.44
    ## W2_INFO_9                      9.35   2.99     3.13   0.00   1.28
    ## SDO                           13.32   5.14     2.59   0.01   1.51
    ## RWA                           14.94   5.34     2.80   0.01   1.46
    ## W2_DAI_Total                  11.23   4.37     2.57   0.01   1.66
    ## W2_IOU_Total                 -10.26   4.88    -2.10   0.04   1.66
    ## W2_Paranoia_Total              8.71   4.15     2.10   0.04   1.83
    ## W2_Chance_Total                0.59   5.12     0.12   0.91   1.94
    ## W2_PO_Total                    0.04   4.82     0.01   0.99   2.28
    ## W2_Internal_Total              6.99   4.93     1.42   0.16   1.30
    ## threat                         9.60   3.23     2.97   0.00   1.23
    ## crt                           -6.95   2.62    -2.65   0.01   1.33
    ## CRT_test                       2.31   1.63     1.42   0.16   1.11
    ## W1_Conspiracy_Total           25.02   4.04     6.19   0.00   1.14
    ## conspiracy2_sc               -12.38   2.73    -4.53   0.00   1.10
    ## conspiracy3_sc                21.52   4.09     5.26   0.00   1.43
    ## -----------------------------------------------------------------

``` r
par(mfrow = c(2,2))
plot(full_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

``` r
AIC(multi_lab)
```

    ## [1] 13399.58

``` r
AIC(full_lab)
```

    ## [1] 13319.14

# Interaction model

``` r
full_int_lab <- lm(W2_Conspiracy_Theory1 ~
                  
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  fis_con +
                  nat +
                  distrust_science + #distrust of scientists
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news + 
                  W2_INFO_5 + #social media
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  #RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  W2_Chance_Total +
                  W2_PO_Total +
                  W2_Internal_Total +
                  
                  #covid-anxiety
                  threat +
                 
                  #CRT
                  #crt +
                  CRT_test +
                  
                  #conspiracies
                  W1_Conspiracy_Total +
                  conspiracy2_sc +
                  conspiracy3_sc +
                    
                  # interaction
                  (SDO*crt),
                data = conspiracies)

summ(full_int_lab, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(26,1372) = 22.52, p = 0.00
    ## R² = 0.30
    ## Adj. R² = 0.29 
    ## 
    ## Standard errors: OLS
    ## ------------------------------------------------------------------
    ##                                Est.    S.E.   t val.      p    VIF
    ## -------------------------- -------- ------- -------- ------ ------
    ## (Intercept)                   -8.49    6.69    -1.27   0.20       
    ## W2_Gender_binary2              1.76    1.59     1.11   0.27   1.12
    ## W1_Education_binary1          -3.93    1.63    -2.41   0.02   1.13
    ## W1_Income_2019                -3.35    2.32    -1.44   0.15   1.23
    ## age_sc                         2.79    4.32     0.64   0.52   1.52
    ## fis_con                        4.42    4.07     1.09   0.28   1.42
    ## nat                           11.94    3.49     3.42   0.00   1.34
    ## distrust_science              10.27    3.36     3.06   0.00   1.25
    ## red_top_tabloid                4.34    1.80     2.41   0.02   1.12
    ## mid_level_news                10.57    1.70     6.23   0.00   1.11
    ## elite_news                    -1.92    1.64    -1.17   0.24   1.12
    ## W2_INFO_5                      3.90    2.82     1.39   0.17   1.44
    ## W2_INFO_9                      9.41    3.00     3.14   0.00   1.28
    ## SDO                           18.88    7.51     2.51   0.01   3.21
    ## W2_DAI_Total                  11.46    4.38     2.62   0.01   1.66
    ## W2_IOU_Total                  -9.99    4.90    -2.04   0.04   1.67
    ## W2_Paranoia_Total              9.19    4.16     2.21   0.03   1.82
    ## W2_Chance_Total                0.34    5.14     0.07   0.95   1.94
    ## W2_PO_Total                   -0.94    4.83    -0.20   0.84   2.27
    ## W2_Internal_Total              6.82    4.94     1.38   0.17   1.30
    ## threat                        10.27    3.24     3.17   0.00   1.23
    ## CRT_test                       2.50    1.63     1.53   0.13   1.11
    ## W1_Conspiracy_Total           26.44    4.04     6.55   0.00   1.13
    ## conspiracy2_sc               -12.59    2.75    -4.59   0.00   1.10
    ## conspiracy3_sc                20.43    4.10     4.99   0.00   1.42
    ## crt                           -5.89    5.41    -1.09   0.28   5.63
    ## SDO:crt                       -4.92   13.19    -0.37   0.71   6.74
    ## ------------------------------------------------------------------

``` r
par(mfrow = c(2,2))
plot(full_int_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

``` r
AIC(full_lab)
```

    ## [1] 13319.14

``` r
AIC(full_int_lab) # no support for interaction effect
```

    ## [1] 13326.97

# Modelling for belief in 5G origin conspiracy

## Plots of DV, incl. inverse hyperbolic sine transformation

``` r
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


# alternative using shapiro-wik test
shapiro_test_pvalue <- function(theta, x){ 
  x <- ihs(x, theta) 
  shapiro.test(x)$p.value 
}
```

``` r
best_theta <- optimize(shapiro_test_pvalue, 
                       lower = 0.00001, upper = 50,
                       x = conspiracies$W2_Conspiracy_Theory3, 
                       maximum = TRUE)$maximum
best_theta
```

    ## [1] 5.450961

``` r
conspiracies %>% 
  ggplot(aes(x = W2_Conspiracy_Theory3)) +
  geom_histogram(colour = "darkgrey", fill = "lightblue")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

``` r
conspiracies <- conspiracies %>% 
  mutate(w2_conspiracy3_ihs = ihs(W2_Conspiracy_Theory3, best_theta))

conspiracies %>% 
  ggplot(aes(x = w2_conspiracy3_ihs)) +
  geom_histogram(colour = "darkgrey", fill = "lightblue")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-53-2.png)<!-- -->

## DV 5G conspiracy - singular IV models

``` r
sdo_5g <- lm(W2_Conspiracy_Theory3 ~ SDO,
              data = conspiracies)

summ(sdo_5g)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 82.37, p = 0.00
    ## R² = 0.06
    ## Adj. R² = 0.05 
    ## 
    ## Standard errors: OLS
    ## ------------------------------------------------
    ##                      Est.   S.E.   t val.      p
    ## ----------------- ------- ------ -------- ------
    ## (Intercept)          0.72   1.28     0.57   0.57
    ## SDO                 28.79   3.17     9.08   0.00
    ## ------------------------------------------------

``` r
rwa_5g <- lm(W2_Conspiracy_Theory3 ~ RWA,
              data = conspiracies)

summ(rwa_5g)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory3
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
    ## (Intercept)         8.19   1.86     4.40   0.00
    ## RWA                 5.75   3.44     1.67   0.09
    ## -----------------------------------------------

``` r
mid_news_5g <- lm(W2_Conspiracy_Theory3 ~ mid_level_news,
              data = conspiracies)

summ(mid_news_5g)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 13.82, p = 0.00
    ## R² = 0.01
    ## Adj. R² = 0.01 
    ## 
    ## Standard errors: OLS
    ## --------------------------------------------------
    ##                        Est.   S.E.   t val.      p
    ## -------------------- ------ ------ -------- ------
    ## (Intercept)            9.67   0.70    13.72   0.00
    ## mid_level_news         4.64   1.25     3.72   0.00
    ## --------------------------------------------------

``` r
social_m_5g <- lm(W2_Conspiracy_Theory3 ~ W2_INFO_5,
              data = conspiracies)

summ(social_m_5g)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 90.16, p = 0.00
    ## R² = 0.06
    ## Adj. R² = 0.06 
    ## 
    ## Standard errors: OLS
    ## ------------------------------------------------
    ##                      Est.   S.E.   t val.      p
    ## ----------------- ------- ------ -------- ------
    ## (Intercept)          6.06   0.78     7.78   0.00
    ## W2_INFO_5           16.79   1.77     9.50   0.00
    ## ------------------------------------------------

## DV 5G conspiracy - socio-economic variables

``` r
se_5g <- lm(W2_Conspiracy_Theory3 ~ W2_Gender_binary +
               W1_Education_binary +
               W1_Income_2019 +
               age_sc,
              data = conspiracies)

summ(se_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1403 (3 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(4,1398) = 19.94, p = 0.00
    ## R² = 0.05
    ## Adj. R² = 0.05 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   25.69   1.87    13.71   0.00       
    ## W2_Gender_binary2             -1.11   1.16    -0.96   0.34   1.03
    ## W1_Education_binary1          -2.41   1.20    -2.01   0.04   1.06
    ## W1_Income_2019                -6.58   1.64    -4.02   0.00   1.06
    ## age_sc                       -20.39   2.70    -7.54   0.00   1.03
    ## -----------------------------------------------------------------

## DV 5G conspiracy - socio-economic variables + political/media

As with modelling above, fis\_con is included instead of soc\_con and
right to avoid multicollinearity problem.

``` r
se_pol_5g <- lm(W2_Conspiracy_Theory3 ~ 
                   #socio-economic variables
                   W2_Gender_binary +
                   W1_Education_binary +
                   W1_Income_2019 +
                   age_sc +
                   
                   #political and media variables
                   fis_con +
                   nat +
                   distrust_science + #distrust of scientists
                   red_top_tabloid + 
                   mid_level_news + 
                   elite_news + 
                   W2_INFO_5 + #social media
                   W2_INFO_9,
                 data = conspiracies)

summ(se_pol_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(12,1386) = 29.22, p = 0.00
    ## R² = 0.20
    ## Adj. R² = 0.20 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -2.93   2.67    -1.10   0.27       
    ## W2_Gender_binary2             -0.85   1.07    -0.79   0.43   1.05
    ## W1_Education_binary1          -1.05   1.13    -0.93   0.35   1.12
    ## W1_Income_2019                -4.20   1.54    -2.72   0.01   1.12
    ## age_sc                       -10.95   2.75    -3.99   0.00   1.26
    ## fis_con                        4.80   2.67     1.80   0.07   1.25
    ## nat                            7.36   2.35     3.13   0.00   1.25
    ## distrust_science              23.35   2.14    10.92   0.00   1.04
    ## red_top_tabloid                6.15   1.23     4.99   0.00   1.08
    ## mid_level_news                 0.67   1.18     0.57   0.57   1.10
    ## elite_news                     0.15   1.13     0.13   0.90   1.09
    ## W2_INFO_5                      9.23   1.91     4.83   0.00   1.36
    ## W2_INFO_9                      7.64   2.04     3.75   0.00   1.21
    ## -----------------------------------------------------------------

``` r
AIC(se_5g)
```

    ## [1] 12573.69

``` r
AIC(se_pol_5g)
```

    ## [1] 12309.17

## DV 5G conspiracy - socio-economic variables + political/media + pol-psych

``` r
se_polpsych_5g <- lm(W2_Conspiracy_Theory3 ~
                        #socio-economic variables
                        W2_Gender_binary +
                        W1_Education_binary +
                        W1_Income_2019 +
                        age_sc +
                        
                        #political and media variables
                        fis_con +
                        nat +
                        distrust_science + #distrust of scientists
                        red_top_tabloid + 
                        mid_level_news + 
                        elite_news + 
                        W2_INFO_5 + #social media
                        W2_INFO_9 + #family and friends
                     
                        #political-psychology variables
                        SDO +
                        RWA +
                        W2_DAI_Total +
                        W2_IOU_Total +
                        W2_Paranoia_Total +
                        W2_Internal_Total +
                        W2_Chance_Total +
                        W2_PO_Total,
                     data = conspiracies)

summ(se_polpsych_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(20,1378) = 27.16, p = 0.00
    ## R² = 0.28
    ## Adj. R² = 0.27 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                  -11.14   3.77    -2.96   0.00       
    ## W2_Gender_binary2              0.50   1.05     0.48   0.63   1.11
    ## W1_Education_binary1          -0.54   1.08    -0.50   0.62   1.13
    ## W1_Income_2019                -2.77   1.52    -1.82   0.07   1.20
    ## age_sc                        -1.63   2.82    -0.58   0.56   1.46
    ## fis_con                        1.68   2.74     0.61   0.54   1.45
    ## nat                            2.97   2.32     1.28   0.20   1.35
    ## distrust_science              16.49   2.14     7.69   0.00   1.15
    ## red_top_tabloid                5.86   1.18     4.98   0.00   1.09
    ## mid_level_news                 0.53   1.13     0.47   0.64   1.12
    ## elite_news                     0.56   1.09     0.51   0.61   1.13
    ## W2_INFO_5                      7.12   1.84     3.86   0.00   1.40
    ## W2_INFO_9                      7.77   1.96     3.97   0.00   1.24
    ## SDO                           19.42   3.33     5.83   0.00   1.43
    ## RWA                           -8.94   3.48    -2.57   0.01   1.40
    ## W2_DAI_Total                  18.19   2.79     6.51   0.00   1.53
    ## W2_IOU_Total                 -13.35   3.18    -4.20   0.00   1.59
    ## W2_Paranoia_Total             12.02   2.74     4.39   0.00   1.79
    ## W2_Internal_Total              1.23   3.24     0.38   0.70   1.26
    ## W2_Chance_Total               -2.94   3.40    -0.87   0.39   1.93
    ## W2_PO_Total                    8.63   3.17     2.72   0.01   2.23
    ## -----------------------------------------------------------------

``` r
AIC(se_pol_5g)
```

    ## [1] 12309.17

``` r
AIC(se_polpsych_5g)
```

    ## [1] 12175.88

## DV 5G conspiracy - socio-economic variables + political/media + pol-psych + covid-threat

Dropping locus of control variables, internal and chance.

``` r
multi_5g <- lm(W2_Conspiracy_Theory3 ~ 
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  fis_con +
                  nat +
                  distrust_science + #distrust of scientists
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news + 
                  W2_INFO_5 + #social media
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  W2_PO_Total +
                  
                  #covid-anxiety
                  threat +
                 
                  # crt
                  crt +
                  CRT_test,
                 data = conspiracies)

summ(multi_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(21,1377) = 27.61, p = 0.00
    ## R² = 0.30
    ## Adj. R² = 0.29 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -4.20   3.44    -1.22   0.22       
    ## W2_Gender_binary2             -0.26   1.05    -0.25   0.80   1.13
    ## W1_Education_binary1          -0.21   1.07    -0.20   0.85   1.13
    ## W1_Income_2019                -1.52   1.51    -1.00   0.32   1.21
    ## age_sc                        -2.32   2.84    -0.82   0.41   1.51
    ## fis_con                        2.11   2.71     0.78   0.44   1.45
    ## nat                            2.98   2.29     1.30   0.19   1.33
    ## distrust_science              15.60   2.12     7.36   0.00   1.15
    ## red_top_tabloid                5.33   1.17     4.54   0.00   1.10
    ## mid_level_news                 0.34   1.12     0.30   0.76   1.12
    ## elite_news                     0.87   1.09     0.80   0.42   1.13
    ## W2_INFO_5                      6.05   1.84     3.28   0.00   1.42
    ## W2_INFO_9                      7.20   1.95     3.69   0.00   1.25
    ## SDO                           18.62   3.33     5.60   0.00   1.46
    ## RWA                          -10.53   3.48    -3.02   0.00   1.43
    ## W2_DAI_Total                  16.89   2.83     5.98   0.00   1.60
    ## W2_IOU_Total                 -11.99   3.16    -3.79   0.00   1.60
    ## W2_Paranoia_Total             10.99   2.71     4.05   0.00   1.79
    ## W2_PO_Total                    7.08   2.55     2.77   0.01   1.47
    ## threat                        -2.50   2.11    -1.19   0.24   1.21
    ## crt                           -8.73   1.71    -5.11   0.00   1.30
    ## CRT_test                      -0.29   1.07    -0.27   0.78   1.10
    ## -----------------------------------------------------------------

``` r
AIC(se_polpsych_5g)
```

    ## [1] 12175.88

``` r
AIC(multi_5g)
```

    ## [1] 12151.07

As with the Chinese lab belief, the locus of control variables
(W2\_Internal\_Total and W2\_Chance\_Total) appear unrelated to the DV,
and are omitted in subsequent models.

## DV 5G conspiracy - multivariate and interaction model

``` r
int_5g <- lm(W2_Conspiracy_Theory3 ~
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  fis_con +
                  nat +
                  distrust_science + #distrust of scientists
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news + 
                  W2_INFO_5 +
                  W2_INFO_9 +
                  
                  #political-psychology variables
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  W2_PO_Total +
                  
                  #covid-anxiety
                  threat +
               
                  #crt
                  #crt +
                  CRT_test +
                  
                  #interactions
                  (crt * SDO),
             data = conspiracies)

summ(int_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(22,1376) = 26.57, p = 0.00
    ## R² = 0.30
    ## Adj. R² = 0.29 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -7.37   3.82    -1.93   0.05       
    ## W2_Gender_binary2             -0.30   1.05    -0.28   0.78   1.13
    ## W1_Education_binary1          -0.16   1.07    -0.15   0.88   1.13
    ## W1_Income_2019                -1.46   1.51    -0.97   0.33   1.21
    ## age_sc                        -2.45   2.83    -0.87   0.39   1.51
    ## fis_con                        2.37   2.71     0.87   0.38   1.45
    ## nat                            2.96   2.28     1.30   0.20   1.33
    ## distrust_science              15.33   2.12     7.23   0.00   1.15
    ## red_top_tabloid                5.25   1.17     4.48   0.00   1.10
    ## mid_level_news                 0.28   1.12     0.25   0.80   1.12
    ## elite_news                     0.94   1.08     0.86   0.39   1.14
    ## W2_INFO_5                      5.92   1.84     3.21   0.00   1.42
    ## W2_INFO_9                      7.37   1.95     3.78   0.00   1.25
    ## RWA                           -9.43   3.53    -2.67   0.01   1.47
    ## W2_DAI_Total                  16.66   2.83     5.90   0.00   1.60
    ## W2_IOU_Total                 -11.70   3.16    -3.70   0.00   1.61
    ## W2_Paranoia_Total             10.97   2.71     4.05   0.00   1.79
    ## W2_PO_Total                    6.90   2.55     2.70   0.01   1.47
    ## threat                        -2.39   2.11    -1.13   0.26   1.21
    ## CRT_test                      -0.31   1.07    -0.29   0.77   1.10
    ## crt                           -2.67   3.59    -0.74   0.46   5.76
    ## SDO                           25.37   4.84     5.24   0.00   3.09
    ## crt:SDO                      -16.72   8.73    -1.92   0.06   6.83
    ## -----------------------------------------------------------------

``` r
AIC(multi_5g)
```

    ## [1] 12151.07

``` r
AIC(int_5g) # very weak support for interaction term at this point
```

    ## [1] 12149.34

## DV 5G conspiracy - socio-economic variables + political/media + pol-psych + covid-threat and CRT + conspiracies

Re-adding in LOC variables for comprehensiveness and transparency.

``` r
full_5g <- lm(W2_Conspiracy_Theory3 ~
                #socio-economic variables
                W2_Gender_binary +
                W1_Education_binary +
                W1_Income_2019 +
                age_sc +
                  
                #political and media variables
                fis_con +
                nat +
                distrust_science + #distrust of scientists
                red_top_tabloid + 
                mid_level_news + 
                elite_news + 
                W2_INFO_5 + #social media
                W2_INFO_9 + #family and friends
                  
                #political-psychology variables
                SDO +
                RWA +
                W2_DAI_Total +
                W2_IOU_Total +
                W2_Paranoia_Total +
                W2_PO_Total +
                W2_Chance_Total +
                W2_Internal_Total +
                
                #covid-anxiety
                threat +
                
                #crt
                crt +
                CRT_test +
                  
                #conspiracies
                W1_Conspiracy_Total +
                conspiracy1_sc +
                conspiracy2_sc,
              data = conspiracies)

summ(full_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(26,1372) = 23.99, p = 0.00
    ## R² = 0.31
    ## Adj. R² = 0.30 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -4.81   4.13    -1.17   0.24       
    ## W2_Gender_binary2             -0.38   1.04    -0.37   0.71   1.14
    ## W1_Education_binary1           0.13   1.07     0.12   0.91   1.14
    ## W1_Income_2019                -1.28   1.51    -0.84   0.40   1.24
    ## age_sc                        -2.40   2.83    -0.85   0.40   1.53
    ## fis_con                        1.94   2.69     0.72   0.47   1.46
    ## nat                            1.95   2.30     0.85   0.40   1.37
    ## distrust_science              13.65   2.16     6.32   0.00   1.22
    ## red_top_tabloid                4.76   1.17     4.08   0.00   1.11
    ## mid_level_news                -0.60   1.12    -0.54   0.59   1.15
    ## elite_news                     1.04   1.08     0.96   0.33   1.14
    ## W2_INFO_5                      5.35   1.83     2.92   0.00   1.43
    ## W2_INFO_9                      6.25   1.95     3.20   0.00   1.28
    ## SDO                           17.65   3.33     5.29   0.00   1.49
    ## RWA                          -12.46   3.48    -3.58   0.00   1.46
    ## W2_DAI_Total                  16.00   2.83     5.66   0.00   1.63
    ## W2_IOU_Total                 -10.93   3.18    -3.44   0.00   1.66
    ## W2_Paranoia_Total             10.22   2.71     3.78   0.00   1.81
    ## W2_PO_Total                    7.97   3.14     2.54   0.01   2.27
    ## W2_Chance_Total               -3.29   3.35    -0.98   0.33   1.94
    ## W2_Internal_Total             -0.00   3.22    -0.00   1.00   1.30
    ## threat                        -3.41   2.12    -1.61   0.11   1.24
    ## crt                           -7.77   1.70    -4.56   0.00   1.31
    ## CRT_test                      -0.65   1.06    -0.61   0.54   1.11
    ## W1_Conspiracy_Total            2.60   2.68     0.97   0.33   1.17
    ## conspiracy1_sc                 9.19   1.75     5.26   0.00   1.41
    ## conspiracy2_sc                 1.04   1.80     0.58   0.56   1.11
    ## -----------------------------------------------------------------

``` r
par(mfrow = c(2,2))
plot(full_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

``` r
AIC(multi_5g)
```

    ## [1] 12151.07

``` r
AIC(full_5g)
```

    ## [1] 12128.51

``` r
full_int_5g <- lm(W2_Conspiracy_Theory3 ~
                #socio-economic variables
                W2_Gender_binary +
                W1_Education_binary +
                W1_Income_2019 +
                age_sc +
                  
                #political and media variables
                fis_con +
                nat +
                distrust_science + #distrust of scientists
                red_top_tabloid + 
                mid_level_news + 
                elite_news + 
                W2_INFO_5 + #social media
                W2_INFO_9 + #family and friends
                  
                #political-psychology variables
                #SDO +
                RWA +
                W2_DAI_Total +
                W2_IOU_Total +
                W2_Paranoia_Total +
                W2_PO_Total +
                W2_Internal_Total +
                W2_Chance_Total +
                  
                #covid-anxety
                threat +
                
                #crt
                #crt +
                CRT_test +
                    
                #conspiracies
                W1_Conspiracy_Total +
                conspiracy1_sc +
                conspiracy2_sc +
                
                # interactions
                (crt*SDO),
              data = conspiracies)

summ(full_int_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(27,1371) = 23.28, p = 0.00
    ## R² = 0.31
    ## Adj. R² = 0.30 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -8.17   4.49    -1.82   0.07       
    ## W2_Gender_binary2             -0.42   1.04    -0.40   0.69   1.14
    ## W1_Education_binary1           0.17   1.07     0.16   0.87   1.14
    ## W1_Income_2019                -1.23   1.51    -0.81   0.42   1.24
    ## age_sc                        -2.56   2.83    -0.90   0.37   1.53
    ## fis_con                        2.20   2.69     0.82   0.41   1.46
    ## nat                            1.94   2.30     0.84   0.40   1.37
    ## distrust_science              13.37   2.16     6.18   0.00   1.22
    ## red_top_tabloid                4.69   1.17     4.02   0.00   1.11
    ## mid_level_news                -0.64   1.12    -0.57   0.57   1.15
    ## elite_news                     1.11   1.08     1.03   0.30   1.14
    ## W2_INFO_5                      5.22   1.83     2.85   0.00   1.43
    ## W2_INFO_9                      6.42   1.95     3.29   0.00   1.28
    ## RWA                          -11.40   3.52    -3.24   0.00   1.49
    ## W2_DAI_Total                  15.80   2.83     5.59   0.00   1.63
    ## W2_IOU_Total                 -10.67   3.18    -3.35   0.00   1.66
    ## W2_Paranoia_Total             10.21   2.70     3.78   0.00   1.81
    ## W2_PO_Total                    7.76   3.14     2.47   0.01   2.27
    ## W2_Internal_Total              0.24   3.22     0.08   0.94   1.30
    ## W2_Chance_Total               -3.22   3.34    -0.96   0.34   1.94
    ## threat                        -3.26   2.12    -1.54   0.12   1.24
    ## CRT_test                      -0.68   1.06    -0.64   0.52   1.11
    ## W1_Conspiracy_Total            2.92   2.68     1.09   0.28   1.18
    ## conspiracy1_sc                 9.09   1.75     5.21   0.00   1.41
    ## conspiracy2_sc                 0.87   1.80     0.48   0.63   1.12
    ## crt                           -1.77   3.58    -0.50   0.62   5.82
    ## SDO                           24.39   4.86     5.02   0.00   3.17
    ## crt:SDO                      -16.54   8.68    -1.91   0.06   6.88
    ## -----------------------------------------------------------------

``` r
par(mfrow = c(2,2))
plot(full_int_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-69-1.png)<!-- -->

``` r
# comparing AIC with and without interaction
AIC(full_5g)
```

    ## [1] 12128.51

``` r
AIC(full_int_5g) # very weak support for interaction effect
```

    ## [1] 12126.81

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
    ## F(1,1404) = 104.57, p = 0.00
    ## R² = 0.07
    ## Adj. R² = 0.07 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------
    ##                     Est.   S.E.   t val.      p
    ## ----------------- ------ ------ -------- ------
    ## (Intercept)         0.21   0.03     8.20   0.00
    ## SDO                 0.65   0.06    10.23   0.00
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
    ## F(1,1404) = 35.56, p = 0.00
    ## R² = 0.02
    ## Adj. R² = 0.02 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------
    ##                     Est.   S.E.   t val.      p
    ## ----------------- ------ ------ -------- ------
    ## (Intercept)         0.24   0.04     6.34   0.00
    ## RWA                 0.41   0.07     5.96   0.00
    ## -----------------------------------------------

``` r
mid_news_5g_ihs <- lm(w2_conspiracy3_ihs ~ mid_level_news,
              data = conspiracies)

summ(mid_news_5g_ihs)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 22.71, p = 0.00
    ## R² = 0.02
    ## Adj. R² = 0.02 
    ## 
    ## Standard errors: OLS
    ## --------------------------------------------------
    ##                        Est.   S.E.   t val.      p
    ## -------------------- ------ ------ -------- ------
    ## (Intercept)            0.41   0.01    28.75   0.00
    ## mid_level_news         0.12   0.03     4.77   0.00
    ## --------------------------------------------------

``` r
social_m_5g_ihs <- lm(w2_conspiracy3_ihs ~ W2_INFO_5,
              data = conspiracies)

summ(social_m_5g_ihs)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 85.20, p = 0.00
    ## R² = 0.06
    ## Adj. R² = 0.06 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------
    ##                     Est.   S.E.   t val.      p
    ## ----------------- ------ ------ -------- ------
    ## (Intercept)         0.35   0.02    21.95   0.00
    ## W2_INFO_5           0.33   0.04     9.23   0.00
    ## -----------------------------------------------

## DV 5G IHS conspiracy - socio-economic variables

``` r
se_5g_ihs <- lm(w2_conspiracy3_ihs ~ W2_Gender_binary +
               W1_Education_binary +
               W1_Income_2019 +
               age_sc,
              data = conspiracies)

summ(se_5g_ihs, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1403 (3 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(4,1398) = 15.27, p = 0.00
    ## R² = 0.04
    ## Adj. R² = 0.04 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                   0.65   0.04    16.93   0.00       
    ## W2_Gender_binary2             0.05   0.02     2.07   0.04   1.03
    ## W1_Education_binary1         -0.04   0.02    -1.83   0.07   1.06
    ## W1_Income_2019               -0.15   0.03    -4.61   0.00   1.06
    ## age_sc                       -0.26   0.06    -4.67   0.00   1.03
    ## ----------------------------------------------------------------

## DV 5G IHS conspiracy - socio-economic variables + political/media

As with modelling above, fis\_con is included instead of soc\_con and
right to avoid multicollinearity problem.

``` r
se_pol_5g_ihs <- lm(w2_conspiracy3_ihs ~ 
                   #socio-economic variables
                   W2_Gender_binary +
                   W1_Education_binary +
                   W1_Income_2019 +
                   age_sc +
                   
                   #political and media variables
                   fis_con +
                   nat +
                   distrust_science + #distrust of scientists
                   red_top_tabloid + 
                   mid_level_news + 
                   elite_news + 
                   W2_INFO_5 + #social media
                   W2_INFO_9,
                 data = conspiracies)

summ(se_pol_5g_ihs, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(12,1386) = 32.04, p = 0.00
    ## R² = 0.22
    ## Adj. R² = 0.21 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                   0.04   0.05     0.73   0.46       
    ## W2_Gender_binary2             0.05   0.02     2.38   0.02   1.05
    ## W1_Education_binary1         -0.01   0.02    -0.34   0.73   1.12
    ## W1_Income_2019               -0.10   0.03    -3.13   0.00   1.12
    ## age_sc                       -0.06   0.06    -1.14   0.25   1.26
    ## fis_con                       0.10   0.05     1.77   0.08   1.25
    ## nat                           0.18   0.05     3.87   0.00   1.25
    ## distrust_science              0.52   0.04    12.01   0.00   1.04
    ## red_top_tabloid               0.11   0.02     4.59   0.00   1.08
    ## mid_level_news                0.03   0.02     1.33   0.18   1.10
    ## elite_news                   -0.05   0.02    -2.02   0.04   1.09
    ## W2_INFO_5                     0.22   0.04     5.73   0.00   1.36
    ## W2_INFO_9                     0.13   0.04     3.15   0.00   1.21
    ## ----------------------------------------------------------------

``` r
AIC(se_5g_ihs)
```

    ## [1] 1646.507

``` r
AIC(se_pol_5g_ihs)
```

    ## [1] 1371.774

## DV 5G IHS conspiracy - socio-economic variables + political/media + pol-psych

``` r
se_polpsych_5g_ihs <- lm(w2_conspiracy3_ihs ~
                        #socio-economic variables
                        W2_Gender_binary +
                        W1_Education_binary +
                        W1_Income_2019 +
                        age_sc +
                        
                        #political and media variables
                        fis_con +
                        nat +
                        distrust_science + #distrust of scientists
                        red_top_tabloid + 
                        mid_level_news + 
                        elite_news + 
                        W2_INFO_5 + #social media
                        W2_INFO_9 + #family and friends
                     
                        #political-psychology variables
                        SDO +
                        RWA +
                        W2_DAI_Total +
                        W2_IOU_Total +
                        W2_Paranoia_Total +
                        W2_Internal_Total +
                        W2_Chance_Total +
                        W2_PO_Total,
                     data = conspiracies)

summ(se_polpsych_5g_ihs, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(20,1378) = 29.20, p = 0.00
    ## R² = 0.30
    ## Adj. R² = 0.29 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  -0.13   0.08    -1.69   0.09       
    ## W2_Gender_binary2             0.07   0.02     3.13   0.00   1.11
    ## W1_Education_binary1          0.00   0.02     0.19   0.85   1.13
    ## W1_Income_2019               -0.06   0.03    -2.12   0.03   1.20
    ## age_sc                        0.10   0.06     1.81   0.07   1.46
    ## fis_con                      -0.01   0.05    -0.20   0.84   1.45
    ## nat                           0.08   0.05     1.66   0.10   1.35
    ## distrust_science              0.37   0.04     8.63   0.00   1.15
    ## red_top_tabloid               0.11   0.02     4.49   0.00   1.09
    ## mid_level_news                0.02   0.02     0.79   0.43   1.12
    ## elite_news                   -0.02   0.02    -1.02   0.31   1.13
    ## W2_INFO_5                     0.17   0.04     4.71   0.00   1.40
    ## W2_INFO_9                     0.14   0.04     3.57   0.00   1.24
    ## SDO                           0.39   0.07     5.88   0.00   1.43
    ## RWA                           0.07   0.07     1.01   0.31   1.40
    ## W2_DAI_Total                  0.39   0.06     6.89   0.00   1.53
    ## W2_IOU_Total                 -0.23   0.06    -3.66   0.00   1.59
    ## W2_Paranoia_Total             0.18   0.05     3.25   0.00   1.79
    ## W2_Internal_Total            -0.08   0.06    -1.31   0.19   1.26
    ## W2_Chance_Total              -0.09   0.07    -1.39   0.16   1.93
    ## W2_PO_Total                   0.17   0.06     2.74   0.01   2.23
    ## ----------------------------------------------------------------

``` r
AIC(se_pol_5g_ihs)
```

    ## [1] 1371.774

``` r
AIC(se_polpsych_5g_ihs)
```

    ## [1] 1236.01

## DV 5G IHS conspiracy - socio-economic variables + political/media + pol-psych + covid-threat

``` r
multi_5g_ihs <- lm(w2_conspiracy3_ihs ~ 
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  fis_con +
                  nat +
                  distrust_science + #distrust of scientists
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news + 
                  W2_INFO_5 + #social media
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  W2_PO_Total +
                  
                  #covid-anxiety
                  threat +
                 
                  # crt
                  crt +
                  CRT_test,
                 data = conspiracies)

summ(multi_5g_ihs, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(21,1377) = 31.57, p = 0.00
    ## R² = 0.32
    ## Adj. R² = 0.31 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  -0.03   0.07    -0.49   0.63       
    ## W2_Gender_binary2             0.04   0.02     2.15   0.03   1.13
    ## W1_Education_binary1          0.02   0.02     0.73   0.47   1.13
    ## W1_Income_2019               -0.04   0.03    -1.29   0.20   1.21
    ## age_sc                        0.06   0.06     1.00   0.32   1.51
    ## fis_con                      -0.00   0.05    -0.08   0.94   1.45
    ## nat                           0.07   0.05     1.44   0.15   1.33
    ## distrust_science              0.36   0.04     8.55   0.00   1.15
    ## red_top_tabloid               0.09   0.02     3.91   0.00   1.10
    ## mid_level_news                0.01   0.02     0.60   0.55   1.12
    ## elite_news                   -0.01   0.02    -0.67   0.50   1.13
    ## W2_INFO_5                     0.14   0.04     3.96   0.00   1.42
    ## W2_INFO_9                     0.12   0.04     2.98   0.00   1.25
    ## SDO                           0.38   0.07     5.82   0.00   1.46
    ## RWA                           0.02   0.07     0.25   0.80   1.43
    ## W2_DAI_Total                  0.34   0.06     6.03   0.00   1.60
    ## W2_IOU_Total                 -0.20   0.06    -3.13   0.00   1.60
    ## W2_Paranoia_Total             0.15   0.05     2.82   0.00   1.79
    ## W2_PO_Total                   0.15   0.05     2.94   0.00   1.47
    ## threat                       -0.06   0.04    -1.37   0.17   1.21
    ## crt                          -0.24   0.03    -7.04   0.00   1.30
    ## CRT_test                      0.03   0.02     1.53   0.13   1.10
    ## ----------------------------------------------------------------

``` r
AIC(se_polpsych_5g_ihs)
```

    ## [1] 1236.01

``` r
AIC(multi_5g_ihs)
```

    ## [1] 1182.477

As with the Chinese lab belief, the locus of control variables
(W2\_Internal\_Total and W2\_Chance\_Total) appear unrelated to the DV,
and are omitted in subsequent models.

## DV 5G IHS conspiracy - multivariate and interaction model

``` r
int_5g_ihs <- lm(w2_conspiracy3_ihs ~
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  fis_con +
                  nat +
                  distrust_science + #distrust of scientists
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news + 
                  W2_INFO_9 +
                  
                  #political-psychology variables
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  W2_PO_Total +
                   
                  #covid-anxiety
                  threat +
               
                  #crt
                  crt +
                  CRT_test +
                  
                  #interactions
                  (W2_INFO_5 * SDO),
             data = conspiracies)

summ(int_5g_ihs, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(22,1376) = 30.41, p = 0.00
    ## R² = 0.33
    ## Adj. R² = 0.32 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                   0.00   0.07     0.01   0.99       
    ## W2_Gender_binary2             0.05   0.02     2.19   0.03   1.13
    ## W1_Education_binary1          0.02   0.02     0.71   0.48   1.13
    ## W1_Income_2019               -0.04   0.03    -1.27   0.20   1.21
    ## age_sc                        0.06   0.06     1.06   0.29   1.51
    ## fis_con                      -0.01   0.05    -0.12   0.90   1.45
    ## nat                           0.06   0.05     1.39   0.17   1.33
    ## distrust_science              0.36   0.04     8.58   0.00   1.15
    ## red_top_tabloid               0.09   0.02     3.89   0.00   1.10
    ## mid_level_news                0.01   0.02     0.61   0.54   1.12
    ## elite_news                   -0.01   0.02    -0.65   0.52   1.14
    ## W2_INFO_9                     0.11   0.04     2.83   0.00   1.26
    ## RWA                           0.03   0.07     0.38   0.70   1.44
    ## W2_DAI_Total                  0.33   0.06     5.90   0.00   1.60
    ## W2_IOU_Total                 -0.19   0.06    -3.11   0.00   1.60
    ## W2_Paranoia_Total             0.15   0.05     2.79   0.01   1.79
    ## W2_PO_Total                   0.14   0.05     2.82   0.00   1.48
    ## threat                       -0.05   0.04    -1.21   0.23   1.21
    ## crt                          -0.24   0.03    -6.98   0.00   1.30
    ## CRT_test                      0.03   0.02     1.56   0.12   1.10
    ## W2_INFO_5                     0.02   0.07     0.27   0.79   5.28
    ## SDO                           0.28   0.08     3.32   0.00   2.33
    ## W2_INFO_5:SDO                 0.36   0.17     2.10   0.04   6.22
    ## ----------------------------------------------------------------

``` r
AIC(multi_5g_ihs)
```

    ## [1] 1182.477

``` r
AIC(int_5g_ihs) # very weak support for interaction term at this point
```

    ## [1] 1180.01

## DV 5G IHS conspiracy - socio-economic variables + political/media + pol-psych + covid-threat and CRT + conspiracies

Re-adding in LOC variables for comprehensiveness and transparency.

``` r
full_5g_ihs <- lm(w2_conspiracy3_ihs ~
                #socio-economic variables
                W2_Gender_binary +
                W1_Education_binary +
                W1_Income_2019 +
                age_sc +
                  
                #political and media variables
                fis_con +
                nat +
                distrust_science + #distrust of scientists
                red_top_tabloid + 
                mid_level_news + 
                elite_news + 
                W2_INFO_5 + #social media
                W2_INFO_9 + #family and friends
                  
                #political-psychology variables
                SDO +
                RWA +
                W2_DAI_Total +
                W2_IOU_Total +
                W2_Paranoia_Total +
                W2_PO_Total +
                W2_Internal_Total +
                W2_Chance_Total +
                
                #covid-anxiety
                threat +
                
                #crt
                crt +
                CRT_test +
                  
                #conspiracies
                W1_Conspiracy_Total +
                conspiracy1_sc +
                conspiracy2_sc,
              data = conspiracies)

summ(full_5g_ihs, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(26,1372) = 28.23, p = 0.00
    ## R² = 0.35
    ## Adj. R² = 0.34 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                   0.04   0.08     0.52   0.60       
    ## W2_Gender_binary2             0.04   0.02     2.01   0.04   1.14
    ## W1_Education_binary1          0.02   0.02     1.03   0.30   1.14
    ## W1_Income_2019               -0.03   0.03    -0.84   0.40   1.24
    ## age_sc                        0.07   0.06     1.22   0.22   1.53
    ## fis_con                       0.00   0.05     0.00   1.00   1.46
    ## nat                           0.05   0.05     1.21   0.23   1.37
    ## distrust_science              0.30   0.04     7.10   0.00   1.22
    ## red_top_tabloid               0.08   0.02     3.42   0.00   1.11
    ## mid_level_news               -0.01   0.02    -0.41   0.68   1.15
    ## elite_news                   -0.01   0.02    -0.44   0.66   1.14
    ## W2_INFO_5                     0.13   0.04     3.60   0.00   1.43
    ## W2_INFO_9                     0.10   0.04     2.58   0.01   1.28
    ## SDO                           0.35   0.07     5.36   0.00   1.49
    ## RWA                          -0.03   0.07    -0.43   0.67   1.46
    ## W2_DAI_Total                  0.32   0.06     5.81   0.00   1.63
    ## W2_IOU_Total                 -0.16   0.06    -2.60   0.01   1.66
    ## W2_Paranoia_Total             0.13   0.05     2.45   0.01   1.81
    ## W2_PO_Total                   0.16   0.06     2.55   0.01   2.27
    ## W2_Internal_Total            -0.11   0.06    -1.71   0.09   1.30
    ## W2_Chance_Total              -0.10   0.07    -1.48   0.14   1.94
    ## threat                       -0.08   0.04    -1.94   0.05   1.24
    ## crt                          -0.22   0.03    -6.42   0.00   1.31
    ## CRT_test                      0.02   0.02     1.18   0.24   1.11
    ## W1_Conspiracy_Total           0.05   0.05     0.91   0.36   1.17
    ## conspiracy1_sc                0.22   0.03     6.32   0.00   1.41
    ## conspiracy2_sc               -0.01   0.04    -0.17   0.86   1.11
    ## ----------------------------------------------------------------

``` r
par(mfrow = c(2,2))
plot(full_5g_ihs)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-84-1.png)<!-- -->

``` r
AIC(multi_5g_ihs)
```

    ## [1] 1182.477

``` r
AIC(full_5g_ihs)
```

    ## [1] 1142.894

``` r
full_int_5g_ihs <- lm(w2_conspiracy3_ihs ~
                #socio-economic variables
                W2_Gender_binary +
                W1_Education_binary +
                W1_Income_2019 +
                age_sc +
                  
                #political and media variables
                fis_con +
                nat +
                distrust_science + #distrust of scientists
                red_top_tabloid + 
                mid_level_news + 
                elite_news + 
                W2_INFO_5 + #social media
                W2_INFO_9 + #family and friends
                  
                #political-psychology variables
                #SDO +
                RWA +
                W2_DAI_Total +
                W2_IOU_Total +
                W2_Paranoia_Total +
                W2_PO_Total +
                W2_Chance_Total +
                W2_Internal_Total +
                
                #covid-anxety
                threat +
                
                #crt
                #crt +
                CRT_test +
                    
                #conspiracies
                W1_Conspiracy_Total +
                conspiracy1_sc +
                conspiracy2_sc +
                
                # interactions
                (crt*SDO),
              data = conspiracies)

summ(full_int_5g_ihs, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(27,1371) = 27.18, p = 0.00
    ## R² = 0.35
    ## Adj. R² = 0.34 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                   0.02   0.09     0.27   0.78       
    ## W2_Gender_binary2             0.04   0.02     2.00   0.05   1.14
    ## W1_Education_binary1          0.02   0.02     1.04   0.30   1.14
    ## W1_Income_2019               -0.02   0.03    -0.83   0.40   1.24
    ## age_sc                        0.07   0.06     1.20   0.23   1.53
    ## fis_con                       0.00   0.05     0.03   0.98   1.46
    ## nat                           0.05   0.05     1.21   0.23   1.37
    ## distrust_science              0.30   0.04     7.05   0.00   1.22
    ## red_top_tabloid               0.08   0.02     3.40   0.00   1.11
    ## mid_level_news               -0.01   0.02    -0.42   0.68   1.15
    ## elite_news                   -0.01   0.02    -0.43   0.67   1.14
    ## W2_INFO_5                     0.13   0.04     3.57   0.00   1.43
    ## W2_INFO_9                     0.10   0.04     2.60   0.01   1.28
    ## RWA                          -0.02   0.07    -0.34   0.73   1.49
    ## W2_DAI_Total                  0.32   0.06     5.78   0.00   1.63
    ## W2_IOU_Total                 -0.16   0.06    -2.57   0.01   1.66
    ## W2_Paranoia_Total             0.13   0.05     2.45   0.01   1.81
    ## W2_PO_Total                   0.16   0.06     2.53   0.01   2.27
    ## W2_Chance_Total              -0.10   0.07    -1.47   0.14   1.94
    ## W2_Internal_Total            -0.11   0.06    -1.69   0.09   1.30
    ## threat                       -0.08   0.04    -1.92   0.06   1.24
    ## CRT_test                      0.02   0.02     1.17   0.24   1.11
    ## W1_Conspiracy_Total           0.05   0.05     0.94   0.35   1.18
    ## conspiracy1_sc                0.22   0.03     6.30   0.00   1.41
    ## conspiracy2_sc               -0.01   0.04    -0.20   0.84   1.12
    ## crt                          -0.18   0.07    -2.58   0.01   5.82
    ## SDO                           0.39   0.10     4.06   0.00   3.17
    ## crt:SDO                      -0.09   0.17    -0.53   0.59   6.88
    ## ----------------------------------------------------------------

``` r
# comparing AIC with and without interaction
AIC(full_5g_ihs)
```

    ## [1] 1142.894

``` r
AIC(full_int_5g_ihs) # no support for interaction effect
```

    ## [1] 1144.605

## DV 5G conspiracy poisson regression model

``` r
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
running quasipoisson models, as suggested by [Building Your Statistical
Horizons](https://bookdown.org/roback/bookdown-bysh/ch-poissonreg.html).

``` r
full_5g_poiss <- glm(W2_Conspiracy_Theory3 ~
                           #socio-economic variables
                           W2_Gender_binary +
                           W1_Education_binary +
                           W1_Income_2019 +
                           age_sc +
                           
                           #political and media variables
                           fis_con +
                           nat +
                           distrust_science + #distrust of scientists
                           red_top_tabloid + 
                           mid_level_news + 
                           elite_news + 
                           W2_INFO_5 + #social media
                           W2_INFO_9 + #family and friends
                           
                           #political-psychology variables
                           SDO +
                           RWA +
                           W2_DAI_Total +
                           W2_IOU_Total +
                           W2_Paranoia_Total +
                           W2_PO_Total +
                           W2_Internal_Total +
                           W2_Chance_Total +
                           
                           #covid-anxiety
                           threat +
                           
                           #crt
                           crt +
                           CRT_test +
                           
                           #conspiracies
                           W1_Conspiracy_Total +
                           conspiracy1_sc +
                           conspiracy2_sc,
                         family = "quasipoisson", data = conspiracies)

summary(full_5g_poiss)
```

    ## 
    ## Call:
    ## glm(formula = W2_Conspiracy_Theory3 ~ W2_Gender_binary + W1_Education_binary + 
    ##     W1_Income_2019 + age_sc + fis_con + nat + distrust_science + 
    ##     red_top_tabloid + mid_level_news + elite_news + W2_INFO_5 + 
    ##     W2_INFO_9 + SDO + RWA + W2_DAI_Total + W2_IOU_Total + W2_Paranoia_Total + 
    ##     W2_PO_Total + W2_Internal_Total + W2_Chance_Total + threat + 
    ##     crt + CRT_test + W1_Conspiracy_Total + conspiracy1_sc + conspiracy2_sc, 
    ##     family = "quasipoisson", data = conspiracies)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -12.4537   -3.0761   -1.9588   -0.2049   26.6448  
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           0.987932   0.368253   2.683 0.007390 ** 
    ## W2_Gender_binary2     0.015536   0.094722   0.164 0.869739    
    ## W1_Education_binary1  0.015873   0.095795   0.166 0.868417    
    ## W1_Income_2019        0.001470   0.148267   0.010 0.992092    
    ## age_sc               -0.409192   0.255033  -1.604 0.108841    
    ## fis_con              -0.045686   0.227150  -0.201 0.840628    
    ## nat                   0.094290   0.218270   0.432 0.665816    
    ## distrust_science      0.765721   0.174155   4.397 1.18e-05 ***
    ## red_top_tabloid       0.361509   0.095460   3.787 0.000159 ***
    ## mid_level_news       -0.022181   0.095936  -0.231 0.817186    
    ## elite_news            0.054157   0.098525   0.550 0.582629    
    ## W2_INFO_5             0.268096   0.167876   1.597 0.110499    
    ## W2_INFO_9             0.482919   0.180712   2.672 0.007622 ** 
    ## SDO                   1.662306   0.319092   5.209 2.18e-07 ***
    ## RWA                  -0.431112   0.342524  -1.259 0.208376    
    ## W2_DAI_Total          1.408242   0.272879   5.161 2.82e-07 ***
    ## W2_IOU_Total         -1.052544   0.281114  -3.744 0.000188 ***
    ## W2_Paranoia_Total     0.789727   0.253217   3.119 0.001854 ** 
    ## W2_PO_Total           0.607514   0.309928   1.960 0.050177 .  
    ## W2_Internal_Total    -0.722844   0.281772  -2.565 0.010413 *  
    ## W2_Chance_Total      -0.289134   0.368635  -0.784 0.432979    
    ## threat               -0.279818   0.201949  -1.386 0.166099    
    ## crt                  -0.858397   0.183773  -4.671 3.29e-06 ***
    ## CRT_test              0.009522   0.096090   0.099 0.921078    
    ## W1_Conspiracy_Total   0.492431   0.240021   2.052 0.040397 *  
    ## conspiracy1_sc        0.905331   0.169105   5.354 1.01e-07 ***
    ## conspiracy2_sc       -0.094651   0.171920  -0.551 0.582031    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for quasipoisson family taken to be 30.49383)
    ## 
    ##     Null deviance: 41421  on 1398  degrees of freedom
    ## Residual deviance: 23945  on 1372  degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## AIC: NA
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# plotting coefficients for poisson model
cons <- as_tibble(
  exp(confint(full_5g_poiss))
)
```

    ## Waiting for profiling to be done...

``` r
names(cons) <- c("lower","upper")

cons$estimate <- exp(coef(full_5g_poiss))
cons$variable <- names(coef(full_5g_poiss))

cons %>% 
  filter(variable != "(Intercept)") %>%
  gather(
    level,ci,lower:upper
  ) %>% 
  ggplot(aes(x = fct_reorder(variable,estimate), y = estimate)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1, 
             colour = "darkgrey") +
  geom_point() +
  geom_line(aes(x = fct_reorder(variable,estimate), y = ci)) +
  coord_flip() +
  theme_classic()
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-90-1.png)<!-- -->

## DV 5G belief - poisson regression, interaction term

``` r
## interaction term added
full_int_5g_poiss <- glm(W2_Conspiracy_Theory3 ~
                           #socio-economic variables
                           W2_Gender_binary +
                           W1_Education_binary +
                           W1_Income_2019 +
                           age_sc +
                           
                           #political and media variables
                           fis_con +
                           nat +
                           distrust_science + #distrust of scientists
                           red_top_tabloid + 
                           mid_level_news + 
                           elite_news + 
                           W2_INFO_5 + #social media
                           W2_INFO_9 + #family and friends
                           
                           #political-psychology variables
                           #SDO +
                           RWA +
                           W2_DAI_Total +
                           W2_IOU_Total +
                           W2_Paranoia_Total +
                           W2_PO_Total +
                           W2_Internal_Total +
                           W2_Chance_Total +
                           
                           #covid-anxiety
                           threat +
                           
                           #crt
                           #crt +
                           CRT_test +
                           
                           #conspiracies
                           W1_Conspiracy_Total +
                           conspiracy1_sc +
                           conspiracy2_sc +
                           
                           # interactions
                           (crt*SDO),
                         family = "quasipoisson", data = conspiracies)

summary(full_int_5g_poiss)
```

    ## 
    ## Call:
    ## glm(formula = W2_Conspiracy_Theory3 ~ W2_Gender_binary + W1_Education_binary + 
    ##     W1_Income_2019 + age_sc + fis_con + nat + distrust_science + 
    ##     red_top_tabloid + mid_level_news + elite_news + W2_INFO_5 + 
    ##     W2_INFO_9 + RWA + W2_DAI_Total + W2_IOU_Total + W2_Paranoia_Total + 
    ##     W2_PO_Total + W2_Internal_Total + W2_Chance_Total + threat + 
    ##     CRT_test + W1_Conspiracy_Total + conspiracy1_sc + conspiracy2_sc + 
    ##     (crt * SDO), family = "quasipoisson", data = conspiracies)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -11.8042   -3.1042   -1.8767   -0.1929   26.5364  
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           1.385427   0.400559   3.459 0.000559 ***
    ## W2_Gender_binary2     0.012802   0.095245   0.134 0.893095    
    ## W1_Education_binary1  0.003856   0.096323   0.040 0.968076    
    ## W1_Income_2019        0.004980   0.148561   0.034 0.973261    
    ## age_sc               -0.385052   0.256565  -1.501 0.133639    
    ## fis_con              -0.068845   0.228331  -0.302 0.763068    
    ## nat                   0.094325   0.218485   0.432 0.666012    
    ## distrust_science      0.797316   0.174549   4.568 5.37e-06 ***
    ## red_top_tabloid       0.373520   0.095946   3.893 0.000104 ***
    ## mid_level_news       -0.029307   0.096268  -0.304 0.760850    
    ## elite_news            0.045190   0.099063   0.456 0.648338    
    ## W2_INFO_5             0.300960   0.168277   1.788 0.073919 .  
    ## W2_INFO_9             0.470125   0.181711   2.587 0.009778 ** 
    ## RWA                  -0.566118   0.348578  -1.624 0.104589    
    ## W2_DAI_Total          1.414789   0.273251   5.178 2.58e-07 ***
    ## W2_IOU_Total         -1.054336   0.282216  -3.736 0.000195 ***
    ## W2_Paranoia_Total     0.776753   0.253837   3.060 0.002256 ** 
    ## W2_PO_Total           0.616055   0.310823   1.982 0.047677 *  
    ## W2_Internal_Total    -0.726633   0.283348  -2.564 0.010440 *  
    ## W2_Chance_Total      -0.314463   0.370202  -0.849 0.395786    
    ## threat               -0.283025   0.201903  -1.402 0.161206    
    ## CRT_test              0.009499   0.096480   0.098 0.921581    
    ## W1_Conspiracy_Total   0.494554   0.241154   2.051 0.040478 *  
    ## conspiracy1_sc        0.890966   0.169764   5.248 1.78e-07 ***
    ## conspiracy2_sc       -0.104243   0.172898  -0.603 0.546667    
    ## crt                  -2.020666   0.514889  -3.924 9.12e-05 ***
    ## SDO                   0.976705   0.419586   2.328 0.020068 *  
    ## crt:SDO               2.606171   1.054157   2.472 0.013546 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for quasipoisson family taken to be 30.70942)
    ## 
    ##     Null deviance: 41421  on 1398  degrees of freedom
    ## Residual deviance: 23753  on 1371  degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## AIC: NA
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# plotting poiss coefficients (not incl. interaction due to scale size)
cons <- as_tibble(
  exp(confint(full_int_5g_poiss))
)
```

    ## Waiting for profiling to be done...

``` r
names(cons) <- c("lower","upper")

cons$estimate <- exp(coef(full_int_5g_poiss))
cons$variable <- names(coef(full_int_5g_poiss))

cons %>% 
  filter(variable != "(Intercept)" & variable != "crt:SDO") %>%
  gather(
    level,ci,lower:upper
  ) %>% 
  ggplot(aes(x = fct_reorder(variable,estimate), y = estimate)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1, 
             colour = "darkgrey") +
  geom_point() +
  geom_line(aes(x = fct_reorder(variable,estimate), y = ci)) +
  coord_flip() +
  theme_classic()
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-92-1.png)<!-- -->

``` r
# drop in deviance test
phi <- sum(resid(full_int_5g_poiss, type='pearson')^2) / full_int_5g_poiss$df.residual
drop_in_dev <- full_5g_poiss$deviance - full_int_5g_poiss$deviance
diff_in_df <- full_5g_poiss$df.residual - full_int_5g_poiss$df.residual
f_stat <- drop_in_dev / summary(full_int_5g_poiss)$dispersion
f_stat
```

    ## [1] 6.247821

``` r
1-pf(f_stat, diff_in_df, full_int_5g_poiss$df.residual)
```

    ## [1] 0.01255076

## Random forest for 5G belief

``` r
pacman::p_load(randomForest)

conspiracies2 <- model.matrix(full_5g_ihs)[,-1] %>% as.data.frame()
y <- model.matrix(full_lab) %>% as.data.frame %>%
  dplyr::select(conspiracy3_sc) %>% as_vector() %>% 
  ihs(theta = best_theta)

set.seed(123)
rf_mod_5g <- randomForest(y ~ .,
                          data = conspiracies2, ntree = 750,
                          mtry = 8, importance = TRUE)

varImpPlot(rf_mod_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-94-1.png)<!-- -->

# Modelling for belief in Chinese meat market origin

## DV Chinese meat market - singular IV models

``` r
sdo_meat <- lm(W2_Conspiracy_Theory2 ~ SDO,
              data = conspiracies)

summ(sdo_meat)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory2
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 0.01, p = 0.91
    ## R² = 0.00
    ## Adj. R² = -0.00 
    ## 
    ## Standard errors: OLS
    ## ------------------------------------------------
    ##                      Est.   S.E.   t val.      p
    ## ----------------- ------- ------ -------- ------
    ## (Intercept)         63.79   1.73    36.94   0.00
    ## SDO                  0.47   4.28     0.11   0.91
    ## ------------------------------------------------

``` r
rwa_meat <- lm(W2_Conspiracy_Theory2 ~ RWA,
              data = conspiracies)

summ(rwa_meat)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory2
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 0.91, p = 0.34
    ## R² = 0.00
    ## Adj. R² = -0.00 
    ## 
    ## Standard errors: OLS
    ## ------------------------------------------------
    ##                      Est.   S.E.   t val.      p
    ## ----------------- ------- ------ -------- ------
    ## (Intercept)         61.75   2.44    25.31   0.00
    ## RWA                  4.31   4.51     0.96   0.34
    ## ------------------------------------------------

``` r
mid_news_meat <- lm(W2_Conspiracy_Theory2 ~ mid_level_news,
              data = conspiracies)

summ(mid_news_meat)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory2
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 1.75, p = 0.19
    ## R² = 0.00
    ## Adj. R² = 0.00 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------
    ##                         Est.   S.E.   t val.      p
    ## -------------------- ------- ------ -------- ------
    ## (Intercept)            63.27   0.93    68.25   0.00
    ## mid_level_news          2.17   1.64     1.32   0.19
    ## ---------------------------------------------------

``` r
social_m_meat <- lm(W2_Conspiracy_Theory2 ~ W2_INFO_5,
              data = conspiracies)

summ(social_m_meat)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory2
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 0.57, p = 0.45
    ## R² = 0.00
    ## Adj. R² = -0.00 
    ## 
    ## Standard errors: OLS
    ## ------------------------------------------------
    ##                      Est.   S.E.   t val.      p
    ## ----------------- ------- ------ -------- ------
    ## (Intercept)         64.51   1.05    61.26   0.00
    ## W2_INFO_5           -1.80   2.39    -0.75   0.45
    ## ------------------------------------------------

## DV Chinese meat market belief - socio-economic variables

``` r
se_meat <- lm(W2_Conspiracy_Theory2 ~ W2_Gender_binary +
               W1_Education_binary +
               W1_Income_2019 +
               age_sc,
              data = conspiracies)

summ(se_meat, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1403 (3 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory2
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(4,1398) = 4.06, p = 0.00
    ## R² = 0.01
    ## Adj. R² = 0.01 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  57.89   2.51    23.08   0.00       
    ## W2_Gender_binary2            -0.59   1.55    -0.38   0.70   1.03
    ## W1_Education_binary1         -1.56   1.61    -0.97   0.33   1.06
    ## W1_Income_2019                6.59   2.19     3.01   0.00   1.06
    ## age_sc                        8.71   3.62     2.41   0.02   1.03
    ## ----------------------------------------------------------------

## DV Chinese meat market belief - socio-economic variables + political/media

``` r
se_pol_meat <- lm(W2_Conspiracy_Theory2 ~ 
                   
                   #socio-economic variables
                   W2_Gender_binary +
                   W1_Education_binary +
                   W1_Income_2019 +
                   age_sc +
                   
                   #political and media variables
                   right +
                   soc_con +
                   fis_con +
                   nat +
                   distrust_science + #distrust of scientists
                   red_top_tabloid + 
                   mid_level_news + 
                   elite_news + 
                   W2_INFO_5 + #social media
                   W2_INFO_9, #family and friends
                 data = conspiracies)

summ(se_pol_meat, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory2
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(14,1384) = 4.59, p = 0.00
    ## R² = 0.04
    ## Adj. R² = 0.03 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   52.95   3.88    13.64   0.00       
    ## W2_Gender_binary2              0.03   1.55     0.02   0.98   1.06
    ## W1_Education_binary1          -1.20   1.63    -0.74   0.46   1.12
    ## W1_Income_2019                 5.52   2.24     2.47   0.01   1.13
    ## age_sc                         5.63   3.96     1.42   0.16   1.26
    ## right                          1.29   5.19     0.25   0.80   1.97
    ## soc_con                        1.74   3.38     0.52   0.61   1.51
    ## fis_con                       -0.09   4.96    -0.02   0.99   2.08
    ## nat                           14.66   3.43     4.28   0.00   1.28
    ## distrust_science             -13.38   3.13    -4.28   0.00   1.08
    ## red_top_tabloid                0.56   1.77     0.32   0.75   1.08
    ## mid_level_news                 0.75   1.71     0.44   0.66   1.12
    ## elite_news                     2.31   1.64     1.41   0.16   1.11
    ## W2_INFO_5                     -0.21   2.75    -0.08   0.94   1.36
    ## W2_INFO_9                      0.02   2.93     0.01   1.00   1.21
    ## -----------------------------------------------------------------

``` r
AIC(se_meat)
```

    ## [1] 13392.6

``` r
AIC(se_pol_meat)
```

    ## [1] 13327.15

## DV Chinese meat market belief - socio-economic variables + political/media + pol-psych

``` r
se_polpsych_meat <- lm(W2_Conspiracy_Theory2 ~
                        
                        #socio-economic variables
                        W2_Gender_binary +
                        W1_Education_binary +
                        W1_Income_2019 +
                        age_sc +
                        
                        #political and media variables
                        fis_con +
                        nat +
                        distrust_science + #distrust of scientists
                        red_top_tabloid + 
                        mid_level_news + 
                        elite_news + 
                        W2_INFO_5 + #social media
                        W2_INFO_9 + #family and friends
                     
                        #political-psychology variables
                        SDO +
                        RWA +
                        W2_DAI_Total +
                        W2_IOU_Total +
                        W2_Paranoia_Total +
                        W2_Internal_Total +
                        W2_Chance_Total +
                        W2_PO_Total,
                      data = conspiracies)

summ(se_polpsych_meat, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory2
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(20,1378) = 5.77, p = 0.00
    ## R² = 0.08
    ## Adj. R² = 0.06 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  33.71   5.61     6.01   0.00       
    ## W2_Gender_binary2            -0.74   1.57    -0.47   0.64   1.11
    ## W1_Education_binary1         -1.07   1.61    -0.66   0.51   1.13
    ## W1_Income_2019                4.74   2.27     2.09   0.04   1.20
    ## age_sc                        4.78   4.20     1.14   0.26   1.46
    ## fis_con                       1.40   4.08     0.34   0.73   1.45
    ## nat                          13.29   3.46     3.84   0.00   1.35
    ## distrust_science             -9.60   3.19    -3.01   0.00   1.15
    ## red_top_tabloid               0.26   1.75     0.15   0.88   1.09
    ## mid_level_news                0.83   1.68     0.50   0.62   1.12
    ## elite_news                    1.84   1.63     1.13   0.26   1.13
    ## W2_INFO_5                    -1.82   2.75    -0.66   0.51   1.40
    ## W2_INFO_9                    -2.11   2.92    -0.72   0.47   1.24
    ## SDO                          -2.29   4.96    -0.46   0.65   1.43
    ## RWA                          -3.06   5.18    -0.59   0.56   1.40
    ## W2_DAI_Total                  5.23   4.16     1.26   0.21   1.53
    ## W2_IOU_Total                 16.48   4.74     3.48   0.00   1.59
    ## W2_Paranoia_Total            -5.98   4.08    -1.47   0.14   1.79
    ## W2_Internal_Total            18.87   4.82     3.91   0.00   1.26
    ## W2_Chance_Total              15.10   5.06     2.98   0.00   1.93
    ## W2_PO_Total                  -7.85   4.72    -1.66   0.10   2.23
    ## ----------------------------------------------------------------

``` r
AIC(se_pol_meat)
```

    ## [1] 13327.15

``` r
AIC(se_polpsych_meat)
```

    ## [1] 13290.1

## DV Chinese meat market belief - socio-economic variables + political/media + pol-psych + covid-threat + CRT

``` r
multi_meat <- lm(W2_Conspiracy_Theory2 ~
                  
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  fis_con +
                  nat +
                  distrust_science + #distrust of scientists
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news + 
                  W2_INFO_5 + #social media
                  W2_INFO_9 + #family and friends
                  W2_Internal_Total +
                  W2_Chance_Total +
                  
                  #political-psychology variables
                  SDO +
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  W2_Internal_Total +
                  W2_Chance_Total +
                  W2_PO_Total +
                  
                  #covid-anxiety
                  threat +
                  
                  #CRT
                  crt +
                  CRT_test,
                data = conspiracies)

summ(multi_meat, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory2
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(23,1375) = 5.89, p = 0.00
    ## R² = 0.09
    ## Adj. R² = 0.07 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  28.16   6.01     4.68   0.00       
    ## W2_Gender_binary2            -0.58   1.57    -0.37   0.71   1.13
    ## W1_Education_binary1         -1.36   1.61    -0.85   0.40   1.13
    ## W1_Income_2019                4.02   2.28     1.76   0.08   1.23
    ## age_sc                        2.27   4.27     0.53   0.59   1.53
    ## fis_con                       1.50   4.06     0.37   0.71   1.46
    ## nat                          13.06   3.44     3.79   0.00   1.35
    ## distrust_science             -8.54   3.19    -2.68   0.01   1.17
    ## red_top_tabloid               0.18   1.76     0.10   0.92   1.11
    ## mid_level_news                0.75   1.68     0.45   0.65   1.12
    ## elite_news                    1.43   1.62     0.88   0.38   1.14
    ## W2_INFO_5                    -1.93   2.76    -0.70   0.48   1.43
    ## W2_INFO_9                    -3.33   2.93    -1.14   0.26   1.27
    ## W2_Internal_Total            20.60   4.82     4.28   0.00   1.27
    ## W2_Chance_Total              15.26   5.03     3.03   0.00   1.93
    ## SDO                           0.28   4.98     0.06   0.95   1.46
    ## RWA                          -4.07   5.21    -0.78   0.43   1.43
    ## W2_DAI_Total                  2.48   4.24     0.59   0.56   1.61
    ## W2_IOU_Total                 13.85   4.76     2.91   0.00   1.63
    ## W2_Paranoia_Total            -6.66   4.07    -1.64   0.10   1.80
    ## W2_PO_Total                  -7.68   4.70    -1.63   0.10   2.23
    ## threat                       13.35   3.16     4.22   0.00   1.22
    ## crt                           2.43   2.55     0.95   0.34   1.30
    ## CRT_test                     -1.18   1.60    -0.73   0.46   1.10
    ## ----------------------------------------------------------------

``` r
AIC(se_polpsych_meat)
```

    ## [1] 13290.1

``` r
AIC(multi_meat)
```

    ## [1] 13277.23

## DV Chinese meat market belief - full model incl. conspiracy ideation

Re-adding in LOC variables for comprehensiveness and transparency.

``` r
full_meat <- lm(W2_Conspiracy_Theory2 ~
                  
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  fis_con +
                  nat +
                  distrust_science + #distrust of scientists
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news + 
                  W2_INFO_5 + #social media
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  W2_Internal_Total +
                  W2_Chance_Total +
                  W2_PO_Total +
                  
                  #covid-anxety
                  threat +
                 
                  #CRT
                  crt +
                  CRT_test +
                  
                  #conspiracies
                  W1_Conspiracy_Total +
                  conspiracy1_sc +
                  conspiracy3_sc,
                data = conspiracies)

summ(full_meat, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory2
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(26,1372) = 6.07, p = 0.00
    ## R² = 0.10
    ## Adj. R² = 0.09 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   26.61   6.15     4.32   0.00       
    ## W2_Gender_binary2             -0.43   1.56    -0.27   0.79   1.14
    ## W1_Education_binary1          -1.79   1.60    -1.12   0.26   1.13
    ## W1_Income_2019                 3.58   2.27     1.58   0.12   1.23
    ## age_sc                         2.42   4.25     0.57   0.57   1.53
    ## fis_con                        1.72   4.04     0.43   0.67   1.46
    ## nat                           14.13   3.43     4.12   0.00   1.36
    ## distrust_science              -7.08   3.28    -2.16   0.03   1.25
    ## red_top_tabloid                0.69   1.76     0.39   0.69   1.13
    ## mid_level_news                 1.95   1.69     1.15   0.25   1.15
    ## elite_news                     1.25   1.61     0.78   0.44   1.14
    ## W2_INFO_5                     -1.40   2.75    -0.51   0.61   1.44
    ## W2_INFO_9                     -2.16   2.94    -0.74   0.46   1.29
    ## SDO                            1.83   5.05     0.36   0.72   1.52
    ## RWA                           -2.18   5.24    -0.41   0.68   1.47
    ## W2_DAI_Total                   3.78   4.29     0.88   0.38   1.67
    ## W2_IOU_Total                  12.45   4.78     2.61   0.01   1.66
    ## W2_Paranoia_Total             -5.51   4.08    -1.35   0.18   1.83
    ## W2_Internal_Total             21.17   4.80     4.41   0.00   1.28
    ## W2_Chance_Total               15.11   5.00     3.02   0.00   1.93
    ## W2_PO_Total                   -7.46   4.72    -1.58   0.11   2.28
    ## threat                        14.30   3.15     4.53   0.00   1.22
    ## crt                            1.53   2.57     0.59   0.55   1.33
    ## CRT_test                      -0.87   1.60    -0.55   0.59   1.11
    ## W1_Conspiracy_Total            2.45   4.01     0.61   0.54   1.17
    ## conspiracy1_sc               -11.89   2.63    -4.53   0.00   1.41
    ## conspiracy3_sc                 2.35   4.05     0.58   0.56   1.45
    ## -----------------------------------------------------------------

``` r
full_int_meat <- lm(W2_Conspiracy_Theory2 ~
                  
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  fis_con +
                  nat +
                  distrust_science + #distrust of scientists
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news + 
                  W2_INFO_5 + #social media
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  #SDO +
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  W2_Internal_Total +
                  W2_Chance_Total +
                  W2_PO_Total +
                  
                  #covid-anxety
                  threat +
                 
                  #CRT
                  #crt +
                  CRT_test +
                  
                  #conspiracies
                  W1_Conspiracy_Total +
                  conspiracy1_sc +
                  conspiracy3_sc +
                  
                  #interaction
                  (SDO*crt),
                data = conspiracies)

summ(full_int_meat, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory2
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(27,1371) = 5.99, p = 0.00
    ## R² = 0.11
    ## Adj. R² = 0.09 
    ## 
    ## Standard errors: OLS
    ## ------------------------------------------------------------------
    ##                                Est.    S.E.   t val.      p    VIF
    ## -------------------------- -------- ------- -------- ------ ------
    ## (Intercept)                   21.48    6.71     3.20   0.00       
    ## W2_Gender_binary2             -0.48    1.56    -0.31   0.76   1.14
    ## W1_Education_binary1          -1.72    1.60    -1.08   0.28   1.14
    ## W1_Income_2019                 3.63    2.27     1.60   0.11   1.23
    ## age_sc                         2.17    4.24     0.51   0.61   1.53
    ## fis_con                        2.10    4.04     0.52   0.60   1.46
    ## nat                           14.09    3.43     4.11   0.00   1.36
    ## distrust_science              -7.42    3.28    -2.26   0.02   1.25
    ## red_top_tabloid                0.60    1.76     0.34   0.73   1.13
    ## mid_level_news                 1.88    1.68     1.11   0.27   1.15
    ## elite_news                     1.36    1.61     0.84   0.40   1.14
    ## W2_INFO_5                     -1.58    2.75    -0.57   0.57   1.44
    ## W2_INFO_9                     -1.88    2.94    -0.64   0.52   1.29
    ## RWA                           -0.63    5.30    -0.12   0.90   1.50
    ## W2_DAI_Total                   3.53    4.29     0.82   0.41   1.67
    ## W2_IOU_Total                  12.77    4.78     2.67   0.01   1.66
    ## W2_Paranoia_Total             -5.47    4.07    -1.34   0.18   1.83
    ## W2_Internal_Total             21.49    4.80     4.48   0.00   1.28
    ## W2_Chance_Total               15.16    5.00     3.03   0.00   1.93
    ## W2_PO_Total                   -7.72    4.72    -1.64   0.10   2.28
    ## threat                        14.47    3.15     4.59   0.00   1.22
    ## CRT_test                      -0.91    1.59    -0.57   0.57   1.11
    ## W1_Conspiracy_Total            2.93    4.02     0.73   0.47   1.18
    ## conspiracy1_sc               -11.97    2.62    -4.56   0.00   1.41
    ## conspiracy3_sc                 1.95    4.05     0.48   0.63   1.46
    ## SDO                           12.02    7.35     1.64   0.10   3.23
    ## crt                           10.50    5.36     1.96   0.05   5.80
    ## SDO:crt                      -24.82   13.01    -1.91   0.06   6.88
    ## ------------------------------------------------------------------

``` r
AIC(multi_meat)
```

    ## [1] 13277.23

``` r
AIC(full_meat)
```

    ## [1] 13262.45

``` r
AIC(full_int_meat) # very weak support for interactions
```

    ## [1] 13260.74

# Summary of final models

``` r
# Chinese lab belief - full set of variables
to_plot <- c("age_sc","W1_Education_binary1","W2_Gender_binary2",
             "W1_Income_2019","elite_news","W2_INFO_9","mid_level_news",
             "W2_INFO_5","red_top_tabloid","threat","W2_DAI_Total",
             "distrust_science","fis_con","W2_IOU_Total","W2_Chance_Total",
             "W2_Internal_Total","W2_PO_Total","nat","W2_Paranoia_Total",
             "RWA","SDO","crt","CRT_test","W1_Conspiracy_Total")
names(to_plot) <- c("Age","Education","Gender","Income","Elite news",
              "Family and friends","Mid-level news","Social media",
              "Tabloid news","COVID-19 anxiety","Death anxiety",
              "Distrust scientists","Fiscal conservatism",
              "Intolerance of uncertainty","LOC: chance","LOC: internal",
              "LOC: powerful others","Nationalism","Paranoia","RWA",
              "SDO","CRT","CRT pre-exposure","Conspiracy ideation")

lab_vars <- c(to_plot, "conspiracy3_sc","conspiracy2_sc")
names(lab_vars) <- c(names(to_plot), "5G belief", "Meat market belief")

plot_coefs(
  full_lab,
  coefs = lab_vars) +
  theme(legend.position = "top") +
  labs(
    caption = "Figure 1 - Model 1 coefficients: belief in Wuhan laboratory origin"
  ) + 
  theme(plot.caption = element_text(hjust = 0.5,
                                    face = "bold",
                                    size = 10),
        axis.title.x = element_text(size = 8,
                                    hjust = 0.36)
  )
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-109-1.png)<!-- -->

``` r
summary(full_lab)$r.squared
```

    ## [1] 0.3030154

``` r
summary(full_lab)$adj.r.squared
```

    ## [1] 0.2898072

``` r
# 5G belief - full set of variables, raw data is DV
fiveg_vars <- c(to_plot, "conspiracy2_sc","conspiracy1_sc")
names(fiveg_vars) <- c(names(to_plot),"Meat market belief",
                     "Wuhan lab belief")

plot_coefs(
  full_5g,
  coefs = fiveg_vars) +
  theme(legend.position = "top") +
  labs(
    caption = "Figure 3 - Model 3 coefficients: belief in 5G origin"
  ) + 
  theme(plot.caption = element_text(hjust = 0.5,
                                    face = "bold",
                                    size = 10),
        axis.title.x = element_text(size = 8,
                                    hjust = 0.44)
  )
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-110-1.png)<!-- -->

``` r
summary(full_5g)$r.squared
```

    ## [1] 0.3125074

``` r
summary(full_5g)$adj.r.squared
```

    ## [1] 0.2994791

``` r
# 5G belief - full set of variables and interaction term
summ(full_int_5g_poiss)
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
    ## <U+03C7>²(27) = 17667.78, p = 0.00
    ## Pseudo-R² (Cragg-Uhler) = 1.00
    ## Pseudo-R² (McFadden) = 0.40
    ## AIC =  NA, BIC =  NA 
    ## 
    ## Standard errors: MLE
    ## ---------------------------------------------------------
    ##                               Est.   S.E.   t val.      p
    ## -------------------------- ------- ------ -------- ------
    ## (Intercept)                   1.39   0.40     3.46   0.00
    ## W2_Gender_binary2             0.01   0.10     0.13   0.89
    ## W1_Education_binary1          0.00   0.10     0.04   0.97
    ## W1_Income_2019                0.00   0.15     0.03   0.97
    ## age_sc                       -0.39   0.26    -1.50   0.13
    ## fis_con                      -0.07   0.23    -0.30   0.76
    ## nat                           0.09   0.22     0.43   0.67
    ## distrust_science              0.80   0.17     4.57   0.00
    ## red_top_tabloid               0.37   0.10     3.89   0.00
    ## mid_level_news               -0.03   0.10    -0.30   0.76
    ## elite_news                    0.05   0.10     0.46   0.65
    ## W2_INFO_5                     0.30   0.17     1.79   0.07
    ## W2_INFO_9                     0.47   0.18     2.59   0.01
    ## RWA                          -0.57   0.35    -1.62   0.10
    ## W2_DAI_Total                  1.41   0.27     5.18   0.00
    ## W2_IOU_Total                 -1.05   0.28    -3.74   0.00
    ## W2_Paranoia_Total             0.78   0.25     3.06   0.00
    ## W2_PO_Total                   0.62   0.31     1.98   0.05
    ## W2_Internal_Total            -0.73   0.28    -2.56   0.01
    ## W2_Chance_Total              -0.31   0.37    -0.85   0.40
    ## threat                       -0.28   0.20    -1.40   0.16
    ## CRT_test                      0.01   0.10     0.10   0.92
    ## W1_Conspiracy_Total           0.49   0.24     2.05   0.04
    ## conspiracy1_sc                0.89   0.17     5.25   0.00
    ## conspiracy2_sc               -0.10   0.17    -0.60   0.55
    ## crt                          -2.02   0.51    -3.92   0.00
    ## SDO                           0.98   0.42     2.33   0.02
    ## crt:SDO                       2.61   1.05     2.47   0.01
    ## ---------------------------------------------------------
    ## 
    ## Estimated dispersion parameter = 30.71

``` r
plot_coefs(full_int_5g_poiss)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-111-1.png)<!-- -->

``` r
# Chinese meat market model - full set of variables
meat_vars <- c(to_plot, "conspiracy3_sc","conspiracy1_sc")
names(meat_vars) <- c(names(to_plot),"5G belief",
                     "Wuhan lab belief")

plot_coefs(
  full_meat,
  coefs = meat_vars) +
  labs(
    caption = "Figure 2 - Model 2 coefficients: belief in meat market origin"
  ) + 
  theme(plot.caption = element_text(hjust = 0.5,
                                    face = "bold",
                                    size = 10),
        axis.title.x = element_text(size = 8,
                                    hjust = 0.36)
  )
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-112-1.png)<!-- -->

``` r
summary(full_meat)$r.squared
```

    ## [1] 0.1031069

``` r
summary(full_meat)$adj.r.squared
```

    ## [1] 0.08611041

## Combined plot of models

``` r
model_vars <- c("W1_Education_binary1","crt",
                "red_top_tabloid","mid_level_news",
                "W2_INFO_5","W2_INFO_9",
                "distrust_science","SDO",
                "RWA","W1_Conspiracy_Total","conspiracy1_sc",
                "conspiracy2_sc","conspiracy3_sc")
names(model_vars) <- c("Post-secondary education","CRT",
                       "Tabloid news","Mid-level news",
                       "Social media",
                       "Family and friends","Distrusts science",
                       "SDO","RWA","Conspiracy ideation","Wuhan lab",
                       "Meat market","5G")

plot_coefs(
  full_lab,full_5g,
  model.names = c("Wuhan lab","5G"),
  coefs = model_vars) +
  theme(legend.position = "top") +
  labs(
    caption = "Model Coefficients: Multivariate Linear Regression"
  ) +
  theme(plot.caption = element_text(hjust = 0.5,
                                    face = "bold",
                                    size = 10),
        axis.title.x = element_text(size = 8,
                                    hjust = 0.36),
        legend.title = element_text(size = 9),
        legend.position = c(1,1), legend.direction = "vertical",
        legend.justification = c(1,1),
        legend.background = element_rect(colour = "darkgrey", fill = NA))
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-113-1.png)<!-- -->

## Conspiracies and social distancing

``` r
# loading paackage for multi nom
pacman::p_load(nnet)
```

``` r
## making a dataset of variables included in the models above
## plus DV's for social distancing and vaccination

vars <- model.matrix(full_lab)[,-1] %>% as.data.frame() %>% names()
vars <- c(vars,"w2_conspiracy3_ihs","W2_Internal_Total",
          "W2_Chance_Total","W2_Conspiracy_Theory1",
          "W2_Conspiracy_Theory2","W2_Conspiracy_Theory3",
          "W2_Conspiracy_Theory4","W2_Conspiracy_Theory5",
          "conspiracy4_sc","conspiracy5_sc","conspiracy1_sc",
          "W2_PO_Total","pid")
vars[1] <- str_sub(vars[1],1,str_length(vars[1])-1)
vars[2] <- str_sub(vars[2],1,str_length(vars[2])-1)

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
                                 W2_C19_Vax_Child, W2_Trust_Body1,
                                 W2_Trust_Body2,W2_Trust_Body5),
  by = "pid", all.x = TRUE
)
```

``` r
# functions to inspact multinomial logit model
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
ggplot(conspiracies2, aes(x = social_distance, y = ..density..)) +
  geom_histogram(binwidth = 0.1, colour = "black", fill = "lightblue")
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-117-1.png)<!-- -->

``` r
dist_full <- lm(social_distance ~ 
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  fis_con +
                  nat +
                  distrust_science + #distrust of scientists
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news +
                  W2_INFO_5 + #social media
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  W2_Chance_Total +
                  W2_PO_Total +
                  W2_Internal_Total +
                  
                  #covid-anxiety
                  threat +
                 
                  #CRT
                  crt +
                  CRT_test +
                  
                  #conspiracies
                  W1_Conspiracy_Total +
                  conspiracy1_sc +
                  conspiracy2_sc +
                  conspiracy3_sc,
                data = conspiracies2)

summ(dist_full, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399
    ## Dependent Variable: social_distance
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(27,1371) = 25.58, p = 0.00
    ## R² = 0.33
    ## Adj. R² = 0.32 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                   0.49   0.03    14.19   0.00       
    ## W2_Gender_binary2             0.04   0.01     4.41   0.00   1.14
    ## W1_Education_binary1          0.01   0.01     1.61   0.11   1.14
    ## W1_Income_2019               -0.00   0.01    -0.03   0.98   1.24
    ## age_sc                        0.12   0.02     5.01   0.00   1.53
    ## fis_con                      -0.01   0.02    -0.56   0.58   1.46
    ## nat                           0.01   0.02     0.76   0.45   1.37
    ## distrust_science             -0.11   0.02    -6.24   0.00   1.25
    ## red_top_tabloid              -0.01   0.01    -0.95   0.34   1.13
    ## mid_level_news                0.00   0.01     0.30   0.77   1.15
    ## elite_news                    0.01   0.01     0.68   0.50   1.14
    ## W2_INFO_5                    -0.01   0.02    -0.96   0.34   1.44
    ## W2_INFO_9                     0.04   0.02     2.51   0.01   1.29
    ## SDO                          -0.18   0.03    -6.47   0.00   1.52
    ## RWA                           0.13   0.03     4.40   0.00   1.47
    ## W2_DAI_Total                 -0.11   0.02    -4.44   0.00   1.67
    ## W2_IOU_Total                  0.08   0.03     3.04   0.00   1.67
    ## W2_Paranoia_Total            -0.03   0.02    -1.53   0.13   1.83
    ## W2_Chance_Total               0.04   0.03     1.46   0.14   1.94
    ## W2_PO_Total                   0.00   0.03     0.16   0.87   2.28
    ## W2_Internal_Total             0.28   0.03    10.45   0.00   1.30
    ## threat                        0.06   0.02     3.44   0.00   1.24
    ## crt                           0.01   0.01     0.90   0.37   1.33
    ## CRT_test                      0.00   0.01     0.38   0.70   1.11
    ## W1_Conspiracy_Total           0.03   0.02     1.36   0.17   1.17
    ## conspiracy1_sc                0.01   0.01     0.70   0.48   1.43
    ## conspiracy2_sc                0.05   0.02     3.12   0.00   1.11
    ## conspiracy3_sc               -0.11   0.02    -4.88   0.00   1.45
    ## ----------------------------------------------------------------

``` r
plot_coefs(dist_full)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-118-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(dist_full)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-118-2.png)<!-- -->

## Multinomial model for vaccine acceptance

``` r
count(conspiracies2,W2_C19_Vax_Self) %>%  mutate(`%` = n / sum(n))
```

    ##   W2_C19_Vax_Self   n           %
    ## 1               1 939 0.671193710
    ## 2               2 126 0.090064332
    ## 3               3 325 0.232308792
    ## 4              NA   9 0.006433167

``` r
vax_full <- multinom(W2_C19_Vax_Self ~ 
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  fis_con +
                  nat +
                  distrust_science + #distrust of scientists
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news +
                  W2_INFO_5 + #social media
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  W2_Chance_Total +
                  W2_PO_Total +
                  W2_Internal_Total +
                  
                  #covid-anxiety
                  threat +
                 
                  #CRT
                  crt +
                  CRT_test +
                  
                  #conspiracies
                  W1_Conspiracy_Total +
                  conspiracy1_sc +
                  conspiracy2_sc +
                  conspiracy3_sc,
                data = conspiracies2)
```

    ## # weights:  87 (56 variable)
    ## initial  value 1527.071081 
    ## iter  10 value 1046.421450
    ## iter  20 value 988.509252
    ## iter  30 value 981.374880
    ## iter  40 value 981.146324
    ## final  value 981.145956 
    ## converged

``` r
sum_table <- summary(vax_full)

results <- left_join(
  coef(vax_full) %>% as_tibble() %>% 
    gather(variable,coefficient,`(Intercept)`:conspiracy3_sc) %>% 
    mutate(response = rep(c("No","Maybe"),28)),
  sum_table$standard.errors %>% as_tibble() %>% 
    gather(variable, std_error,`(Intercept)`:conspiracy3_sc) %>% 
    mutate(response = rep(c("No","Maybe"),28)),
  by = c("variable","response")
) %>% 
  dplyr::select(variable,response,everything()) %>% 
  mutate(
    upper_ci = coefficient + std_error * qnorm(0.975),
    lower_ci = coefficient - std_error * qnorm(0.975)
  )

results %>% 
  filter(variable != "(Intercept)") %>% 
  ggplot(aes(x = variable, y = coefficient)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  geom_line(data = results %>% filter(variable != "(Intercept)") %>% 
              gather(interval,estimate,upper_ci:lower_ci),
            aes(x = variable, y = estimate), colour = "black") + 
  geom_point(size = 2, colour = "black") +
  geom_point(size = 1.5, colour = "white") +
  facet_wrap(~response) +
  coord_flip() +
  theme_bw() +
  labs(title = "Vaccine acceptance (baseline = Yes)")
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-120-1.png)<!-- -->

## Conspiracy belief and trust in government institutions

``` r
# governmental trust scale
trust_keys <- list(sd = cs(W2_Trust_Body1,W2_Trust_Body2,
                           W2_Trust_Body5
                           ))

trust_test <- scoreItems(trust_keys, conspiracies2, min = 1, max = 5)
head(trust_test$scores)
```

    ##            sd
    ## [1,] 3.000000
    ## [2,] 4.666667
    ## [3,] 4.000000
    ## [4,] 5.000000
    ## [5,] 3.000000
    ## [6,] 2.000000

``` r
trust_test$alpha  # Scale alpha
```

    ##              sd
    ## alpha 0.8742177

``` r
conspiracies2$govt_distrust <- rescale01(trust_test$scores, na.rm = TRUE)
conspiracies2$govt_distrust <- c(conspiracies2$govt_distrust)  # Ensure variable is numeric and not matrix class
conspiracies2$govt_trust <- 0 - conspiracies2$govt_distrust

describe(conspiracies2$govt_trust)
```

    ##    vars    n  mean   sd median trimmed  mad min max range skew kurtosis   se
    ## X1    1 1399 -0.63 0.24  -0.67   -0.63 0.25  -1   0     1 0.11    -0.81 0.01

``` r
ggplot(conspiracies2, aes(x = govt_trust, y = ..density..)) +
  geom_histogram(binwidth = 0.1, colour = "black", fill = "lightblue")
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-121-1.png)<!-- -->

Note: trust in science (distrust\_science) removed, as may be tapping
into a latent ‘distrustful’ disposition invalidating the exogeneity
assumption.

``` r
trust_full <- lm(govt_trust ~ 
                  #socio-economic variables
                  W2_Gender_binary +
                  W1_Education_binary +
                  W1_Income_2019 +
                  age_sc +
                  
                  #political and media variables
                  fis_con +
                  nat +
                  #distrust_science + #distrust of scientists
                  red_top_tabloid + 
                  mid_level_news + 
                  elite_news +
                  W2_INFO_5 + #social media
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  W2_Chance_Total +
                  W2_PO_Total +
                  W2_Internal_Total +
                  
                  #covid-anxiety
                  threat +
                 
                  #CRT
                  crt +
                  CRT_test +
                  
                  #conspiracies
                  W1_Conspiracy_Total +
                  conspiracy1_sc +
                  conspiracy2_sc +
                  conspiracy3_sc,
                 data = conspiracies2)

summ(trust_full, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399
    ## Dependent Variable: govt_trust
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(26,1372) = 17.27, p = 0.00
    ## R² = 0.25
    ## Adj. R² = 0.23 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  -0.84   0.05   -18.08   0.00       
    ## W2_Gender_binary2             0.01   0.01     0.69   0.49   1.13
    ## W1_Education_binary1         -0.00   0.01    -0.04   0.97   1.14
    ## W1_Income_2019                0.01   0.02     0.39   0.69   1.24
    ## age_sc                       -0.03   0.03    -0.99   0.32   1.53
    ## fis_con                       0.13   0.03     4.22   0.00   1.46
    ## nat                           0.25   0.03     9.63   0.00   1.36
    ## red_top_tabloid               0.01   0.01     0.80   0.43   1.13
    ## mid_level_news               -0.01   0.01    -0.57   0.57   1.15
    ## elite_news                    0.00   0.01     0.31   0.76   1.14
    ## W2_INFO_5                     0.02   0.02     0.95   0.34   1.44
    ## W2_INFO_9                     0.05   0.02     2.24   0.03   1.29
    ## SDO                           0.01   0.04     0.31   0.76   1.51
    ## RWA                           0.13   0.04     3.17   0.00   1.47
    ## W2_DAI_Total                  0.01   0.03     0.38   0.70   1.67
    ## W2_IOU_Total                  0.02   0.04     0.62   0.54   1.66
    ## W2_Paranoia_Total            -0.18   0.03    -5.82   0.00   1.80
    ## W2_Chance_Total               0.09   0.04     2.24   0.03   1.94
    ## W2_PO_Total                  -0.04   0.04    -1.03   0.30   2.28
    ## W2_Internal_Total             0.09   0.04     2.48   0.01   1.28
    ## threat                        0.05   0.02     1.96   0.05   1.24
    ## crt                          -0.01   0.02    -0.76   0.45   1.33
    ## CRT_test                      0.02   0.01     1.94   0.05   1.11
    ## W1_Conspiracy_Total          -0.30   0.03    -9.80   0.00   1.15
    ## conspiracy1_sc               -0.01   0.02    -0.62   0.54   1.42
    ## conspiracy2_sc                0.01   0.02     0.28   0.78   1.11
    ## conspiracy3_sc                0.03   0.03     1.01   0.31   1.41
    ## ----------------------------------------------------------------

``` r
plot_coefs(trust_full)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-122-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(trust_full)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-122-2.png)<!-- -->

## Table summarising variables

``` r
all_vars <- model.matrix(dist_full)[,-1] %>% as_tibble()
var_summaries <- tibble(
  variable = names(all_vars),
  mean = all_vars %>% map_dbl(mean, na.rm = TRUE),
  sd = all_vars %>% map_dbl(sd, na.rm = TRUE)
) %>% 
  filter(!variable %in% c("conspiracy1_sc","conspiracy2_sc",
                          "conspiracy3_sc"))
write.csv(
  rbind(
    var_summaries,
    tibble(
      variable = c("Wuhan lab belief","Chinese meat market belief",
                 "5G belief"),
      mean = c(
        mean(conspiracies$W2_Conspiracy_Theory1[conspiracies$pid %in% 
                                         conspiracies2$pid], na.rm = TRUE),
        mean(conspiracies$W2_Conspiracy_Theory2[conspiracies$pid %in% 
                                         conspiracies2$pid], na.rm = TRUE),
        mean(conspiracies$W2_Conspiracy_Theory3[conspiracies$pid %in% 
                                         conspiracies2$pid], na.rm = TRUE)
        ),
      sd = c(
        sd(conspiracies$W2_Conspiracy_Theory1[conspiracies$pid %in% 
                                         conspiracies2$pid], na.rm = TRUE),
        sd(conspiracies$W2_Conspiracy_Theory2[conspiracies$pid %in% 
                                         conspiracies2$pid], na.rm = TRUE),
        sd(conspiracies$W2_Conspiracy_Theory3[conspiracies$pid %in% 
                                         conspiracies2$pid], na.rm = TRUE)
        )
      )
    ), 
  "variable_summaries.csv"
)
```

``` r
conspiracies2 %>% 
  count(
    to_factor(W2_C19_Vax_Self)
  )
```

    ##   to_factor(W2_C19_Vax_Self)   n
    ## 1                        Yes 939
    ## 2                         No 126
    ## 3                      Maybe 325
    ## 4                       <NA>   9
