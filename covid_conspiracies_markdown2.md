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
```

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
# Making a binary for each CRT scale, baseline = correct answer
conspiracies$CRT1 <- ifelse(
  to_factor(conspiracies$W1_CRT1) != "5 pence", 1, 0
)

conspiracies$CRT2 <- ifelse(
  to_factor(conspiracies$W1_CRT2) != "5 minutes", 1, 0
)

conspiracies$CRT3 <- ifelse(
  to_factor(conspiracies$W1_CRT3) != "47 days", 1, 0
)

conspiracies$CRT4 <- ifelse(
  to_factor(conspiracies$W1_CRT4) != "2nd", 1, 0
)

conspiracies$CRT5 <- ifelse(
  to_factor(conspiracies$W1_CRT5) != "8", 1, 0
)

conspiracies$CRT_test <- ifelse(
  to_factor(conspiracies$W1_CRT_test) == "None of them.", 1, 0
) # baseline = heard of some of them OR all of them
```

``` r
# CRT
crt_keys <- list(crt = cs(CRT1,
                          CRT2,
                          CRT3#,
                          #CRT4, 
                          #CRT5
                          ))

crt_test <- scoreItems(crt_keys, conspiracies, min = 0, max = 1)
head(crt_test$scores)
```

    ##            crt
    ## [1,] 0.6666667
    ## [2,] 1.0000000
    ## [3,] 1.0000000
    ## [4,] 1.0000000
    ## [5,] 1.0000000
    ## [6,] 1.0000000

``` r
summary(crt_test$alpha)  # Scale alpha
```

    ##       crt        
    ##  Min.   :0.6948  
    ##  1st Qu.:0.6948  
    ##  Median :0.6948  
    ##  Mean   :0.6948  
    ##  3rd Qu.:0.6948  
    ##  Max.   :0.6948

``` r
conspiracies$crt <- rescale01(crt_test$scores, na.rm = TRUE)
conspiracies$crt <- c(conspiracies$crt)  # Ensure variable is numeric and not matrix class

describe(conspiracies$crt)
```

    ##    vars    n mean   sd median trimmed  mad min max range  skew kurtosis   se
    ## X1    1 1406 0.68 0.36   0.67    0.73 0.49   0   1     1 -0.72    -0.88 0.01

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
    ## 1        0    0.599      0.667   0.382
    ## 2        1    0.734      1       0.339

``` r
kruskal.test(crt ~ CRT_test, data = conspiracies)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  crt by CRT_test
    ## Kruskal-Wallis chi-squared = 44.001, df = 1, p-value = 3.283e-11

``` r
# Making a binary for each CRT scale, 1 = most common wrong answer
conspiracies$CRT1_wrong <- ifelse(
  to_factor(conspiracies$W1_CRT1) == "10 pence", 1, 0
)

conspiracies$CRT2_wrong <- ifelse(
  to_factor(conspiracies$W1_CRT2) == "100 minutes", 1, 0
)

conspiracies$CRT3_wrong <- ifelse(
  to_factor(conspiracies$W1_CRT3) == "24 days", 1, 0
)

conspiracies$CRT4_wrong <- ifelse(
  to_factor(conspiracies$W1_CRT4) == "1st", 1, 0
)

conspiracies$CRT5_wrong <- ifelse(
  to_factor(conspiracies$W1_CRT5) == "7", 1, 0
)
```

``` r
# rescaling the remaing numeric variables
numerics <- c("W1_Conspiracy_Total","W2_Paranoia_Total",
              "W2_Internal_Total","W2_Chance_Total","W2_PO_Total",
              "W2_DAI_Total","W2_IOU_Total", "W2_INFO_5",
              "W2_INFO_9","W2_Trust_Body6","W1_Income_2019")

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
av_ggplot(conspiracy_mod)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-28-2.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-28-3.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-28-4.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-28-5.png)<!-- -->

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
daily_m_lab <- lm(W2_Conspiracy_Theory1 ~ W2_Newspaper_prefer1,
              data = conspiracies)

summ(daily_m_lab)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 87.24, p = 0.00
    ## R² = 0.06
    ## Adj. R² = 0.06 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------------
    ##                               Est.   S.E.   t val.      p
    ## -------------------------- ------- ------ -------- ------
    ## (Intercept)                  32.96   1.03    32.02   0.00
    ## W2_Newspaper_prefer1         17.47   1.87     9.34   0.00
    ## ---------------------------------------------------------

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

``` r
plot_coefs(se_lab)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-33-2.png)<!-- -->

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
                   W2_Trust_Body6 + #distrust of scientists
                   W2_Newspaper_prefer1 + #daily_mail
                   W2_Newspaper_prefer5 + #guardian
                   W2_Newspaper_prefer9 + #sun
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
    ## F(14,1384) = 25.55, p = 0.00
    ## R² = 0.21
    ## Adj. R² = 0.20 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                   9.42   4.12     2.28   0.02       
    ## W2_Gender_binary2             2.85   1.64     1.74   0.08   1.06
    ## W1_Education_binary1         -4.52   1.72    -2.62   0.01   1.12
    ## W1_Income_2019               -6.66   2.35    -2.83   0.00   1.12
    ## age_sc                       -0.15   4.19    -0.03   0.97   1.27
    ## right                         5.59   5.49     1.02   0.31   1.98
    ## soc_con                       9.59   3.56     2.70   0.01   1.50
    ## fis_con                      -2.23   5.24    -0.43   0.67   2.09
    ## nat                          16.11   3.62     4.44   0.00   1.29
    ## W2_Trust_Body6               22.83   3.30     6.91   0.00   1.08
    ## W2_Newspaper_prefer1         10.52   1.83     5.76   0.00   1.11
    ## W2_Newspaper_prefer5         -5.12   2.11    -2.42   0.02   1.16
    ## W2_Newspaper_prefer9         10.50   2.54     4.13   0.00   1.11
    ## W2_INFO_5                    10.24   2.91     3.52   0.00   1.37
    ## W2_INFO_9                    13.30   3.09     4.31   0.00   1.21
    ## ----------------------------------------------------------------

``` r
plot_coefs(se_pol_lab)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_pol_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-34-2.png)<!-- -->

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
                        W2_Trust_Body6 + #distrust of scientists
                        W2_Newspaper_prefer1 + #daily_mail
                        W2_Newspaper_prefer5 + #guardian
                        W2_Newspaper_prefer9 + #sun
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
    ## F(22,1376) = 19.64, p = 0.00
    ## R² = 0.24
    ## Adj. R² = 0.23 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -7.97   6.01    -1.33   0.18       
    ## W2_Gender_binary2              2.34   1.66     1.41   0.16   1.13
    ## W1_Education_binary1          -3.76   1.70    -2.22   0.03   1.13
    ## W1_Income_2019                -5.18   2.39    -2.17   0.03   1.20
    ## age_sc                         5.36   4.44     1.21   0.23   1.48
    ## right                          2.85   5.52     0.52   0.61   2.08
    ## soc_con                        2.63   3.69     0.71   0.48   1.68
    ## fis_con                       -2.82   5.19    -0.54   0.59   2.13
    ## nat                           10.48   3.66     2.86   0.00   1.36
    ## W2_Trust_Body6                18.26   3.39     5.39   0.00   1.17
    ## W2_Newspaper_prefer1           9.58   1.80     5.31   0.00   1.13
    ## W2_Newspaper_prefer5          -2.56   2.13    -1.20   0.23   1.23
    ## W2_Newspaper_prefer9           9.23   2.51     3.68   0.00   1.12
    ## W2_INFO_5                      8.03   2.90     2.77   0.01   1.41
    ## W2_INFO_9                     12.95   3.07     4.22   0.00   1.24
    ## SDO                            9.54   5.39     1.77   0.08   1.53
    ## RWA                           20.67   5.72     3.61   0.00   1.54
    ## W2_DAI_Total                  16.94   4.39     3.86   0.00   1.54
    ## W2_IOU_Total                 -12.17   4.99    -2.44   0.01   1.59
    ## W2_Paranoia_Total             13.18   4.29     3.07   0.00   1.78
    ## W2_Internal_Total              4.97   5.09     0.98   0.33   1.27
    ## W2_Chance_Total               -1.58   5.33    -0.30   0.77   1.93
    ## W2_PO_Total                    6.34   4.97     1.27   0.20   2.23
    ## -----------------------------------------------------------------

``` r
plot_coefs(se_polpsych_lab)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_polpsych_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-35-2.png)<!-- -->

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
    ##  7   0.227 W2_Newspaper_prefer1
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
    ##  7    0.199 W2_Trust_Body6      
    ##  8    0.183 W2_Newspaper_prefer1
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
    ##  7    0.208 W2_Newspaper_prefer1
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
    ##  7    0.237 W2_Trust_Body6      
    ##  8    0.158 SDO                 
    ##  9    0.137 W2_INFO_9           
    ## 10    0.131 W2_Newspaper_prefer9
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
                        W2_Trust_Body6 + #distrust of scientists
                        W2_Newspaper_prefer1 + #daily_mail
                        W2_Newspaper_prefer5 + #guardian
                        W2_Newspaper_prefer9 + #sun
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
    ## F(17,1381) = 25.26, p = 0.00
    ## R² = 0.24
    ## Adj. R² = 0.23 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -4.39   4.91    -0.89   0.37       
    ## W2_Gender_binary2              1.92   1.63     1.18   0.24   1.10
    ## W1_Education_binary1          -3.69   1.69    -2.18   0.03   1.12
    ## W1_Income_2019                -5.14   2.34    -2.19   0.03   1.16
    ## age_sc                         5.56   4.38     1.27   0.21   1.44
    ## fis_con                       -0.23   4.28    -0.05   0.96   1.45
    ## nat                           11.17   3.61     3.10   0.00   1.33
    ## W2_Trust_Body6                18.70   3.31     5.66   0.00   1.12
    ## W2_Newspaper_prefer1           9.71   1.80     5.40   0.00   1.12
    ## W2_Newspaper_prefer5          -2.68   2.13    -1.26   0.21   1.22
    ## W2_Newspaper_prefer9           9.36   2.50     3.75   0.00   1.11
    ## W2_INFO_5                      8.18   2.89     2.83   0.00   1.40
    ## W2_INFO_9                     13.32   3.05     4.37   0.00   1.23
    ## SDO                           10.34   5.26     1.96   0.05   1.46
    ## RWA                           21.54   5.45     3.95   0.00   1.40
    ## W2_DAI_Total                  17.81   4.34     4.11   0.00   1.51
    ## W2_IOU_Total                 -11.40   4.87    -2.34   0.02   1.52
    ## W2_Paranoia_Total             14.15   4.10     3.45   0.00   1.64
    ## -----------------------------------------------------------------

``` r
plot_coefs(se_polpsych_lab_2)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_polpsych_lab_2)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-40-2.png)<!-- -->

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
                  W2_Trust_Body6 + #distrust of scientists
                  W2_Newspaper_prefer1 + #daily_mail
                  W2_Newspaper_prefer5 + #guardian
                  W2_Newspaper_prefer9 + #sun
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
    ## F(20,1378) = 23.25, p = 0.00
    ## R² = 0.25
    ## Adj. R² = 0.24 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                  -11.50   5.05    -2.28   0.02       
    ## W2_Gender_binary2              0.95   1.63     0.58   0.56   1.12
    ## W1_Education_binary1          -3.39   1.68    -2.02   0.04   1.13
    ## W1_Income_2019                -4.46   2.34    -1.91   0.06   1.18
    ## age_sc                         2.47   4.44     0.56   0.58   1.51
    ## fis_con                        0.64   4.24     0.15   0.88   1.45
    ## nat                           10.99   3.58     3.07   0.00   1.33
    ## W2_Trust_Body6                18.62   3.29     5.65   0.00   1.13
    ## W2_Newspaper_prefer1           9.47   1.78     5.32   0.00   1.12
    ## W2_Newspaper_prefer5          -2.36   2.12    -1.11   0.27   1.23
    ## W2_Newspaper_prefer9           8.41   2.48     3.39   0.00   1.12
    ## W2_INFO_5                      6.92   2.89     2.40   0.02   1.42
    ## W2_INFO_9                     11.83   3.04     3.89   0.00   1.24
    ## SDO                           12.78   5.26     2.43   0.02   1.48
    ## RWA                           16.79   5.47     3.07   0.00   1.44
    ## W2_DAI_Total                  13.97   4.39     3.18   0.00   1.57
    ## W2_IOU_Total                 -11.56   4.86    -2.38   0.02   1.55
    ## W2_Paranoia_Total             12.60   4.08     3.09   0.00   1.65
    ## threat                         7.90   3.31     2.39   0.02   1.21
    ## crt                            8.75   2.38     3.68   0.00   1.24
    ## CRT_test                       3.05   1.67     1.83   0.07   1.09
    ## -----------------------------------------------------------------

``` r
plot_coefs(multi_lab)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(multi_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-41-2.png)<!-- -->

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
                  W2_Trust_Body6 + #distrust of scientists
                  W2_Newspaper_prefer1 + #daily_mail
                  W2_Newspaper_prefer5 + #guardian
                  W2_Newspaper_prefer9 + #sun
                  
                  #political-psychology variables
                  SDO +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  
                  #covid-anxety
                  threat +
                  
                  #CRT
                  crt +
                
                  #interactions
                  (W2_INFO_5*RWA) +
                  (W2_INFO_9*RWA),
                data = conspiracies)

summ(int_lab, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(21,1377) = 21.98, p = 0.00
    ## R² = 0.25
    ## Adj. R² = 0.24 
    ## 
    ## Standard errors: OLS
    ## -------------------------------------------------------------------
    ##                                Est.    S.E.   t val.      p     VIF
    ## -------------------------- -------- ------- -------- ------ -------
    ## (Intercept)                  -11.02    5.98    -1.84   0.07        
    ## W2_Gender_binary2              0.90    1.64     0.55   0.58    1.12
    ## W1_Education_binary1          -3.44    1.68    -2.05   0.04    1.13
    ## W1_Income_2019                -4.36    2.34    -1.86   0.06    1.18
    ## age_sc                         2.94    4.43     0.66   0.51    1.50
    ## fis_con                        0.53    4.25     0.12   0.90    1.45
    ## nat                           11.09    3.58     3.10   0.00    1.33
    ## W2_Trust_Body6                18.48    3.30     5.60   0.00    1.13
    ## W2_Newspaper_prefer1           9.39    1.78     5.26   0.00    1.12
    ## W2_Newspaper_prefer5          -2.50    2.12    -1.18   0.24    1.23
    ## W2_Newspaper_prefer9           8.51    2.49     3.42   0.00    1.12
    ## SDO                           12.15    5.27     2.31   0.02    1.48
    ## W2_DAI_Total                  14.21    4.40     3.23   0.00    1.58
    ## W2_IOU_Total                 -11.92    4.88    -2.44   0.01    1.55
    ## W2_Paranoia_Total             12.43    4.09     3.04   0.00    1.65
    ## threat                         8.29    3.30     2.51   0.01    1.20
    ## crt                            9.52    2.34     4.06   0.00    1.20
    ## W2_INFO_5                     14.41    8.31     1.73   0.08   11.76
    ## RWA                           18.74    8.48     2.21   0.03    3.45
    ## W2_INFO_9                      7.77    9.42     0.82   0.41   11.89
    ## W2_INFO_5:RWA                -14.97   15.06    -0.99   0.32   12.21
    ## RWA:W2_INFO_9                  7.75   17.24     0.45   0.65   13.52
    ## -------------------------------------------------------------------

``` r
plot_coefs(int_lab)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(int_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-42-2.png)<!-- -->

## DV Chinese lab conspiracy - full model incl. conspiracy ideation

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
                  W2_Trust_Body6 + #distrust of scientists
                  W2_Newspaper_prefer1 + #daily_mail
                  W2_Newspaper_prefer5 + #guardian
                  W2_Newspaper_prefer9 + #sun
                  W2_INFO_5 + #social media
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  
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
    ## F(23,1375) = 25.80, p = 0.00
    ## R² = 0.30
    ## Adj. R² = 0.29 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                  -14.19   5.33    -2.66   0.01       
    ## W2_Gender_binary2              1.00   1.58     0.63   0.53   1.12
    ## W1_Education_binary1          -3.48   1.63    -2.14   0.03   1.13
    ## W1_Income_2019                -3.00   2.27    -1.32   0.19   1.19
    ## age_sc                         3.73   4.30     0.87   0.39   1.51
    ## fis_con                        1.80   4.11     0.44   0.66   1.45
    ## nat                           11.52   3.49     3.30   0.00   1.35
    ## W2_Trust_Body6                10.00   3.31     3.02   0.00   1.22
    ## W2_Newspaper_prefer1           9.45   1.72     5.49   0.00   1.12
    ## W2_Newspaper_prefer5          -1.92   2.05    -0.94   0.35   1.24
    ## W2_Newspaper_prefer9           7.74   2.41     3.21   0.00   1.12
    ## W2_INFO_5                      4.36   2.81     1.55   0.12   1.44
    ## W2_INFO_9                      9.73   2.96     3.29   0.00   1.26
    ## SDO                           12.01   5.18     2.32   0.02   1.54
    ## RWA                           15.37   5.34     2.88   0.00   1.46
    ## W2_DAI_Total                  11.35   4.32     2.63   0.01   1.63
    ## W2_IOU_Total                 -10.67   4.78    -2.23   0.03   1.59
    ## W2_Paranoia_Total              7.87   3.99     1.97   0.05   1.69
    ## threat                         9.27   3.22     2.88   0.00   1.22
    ## crt                            6.38   2.32     2.76   0.01   1.25
    ## CRT_test                       2.21   1.62     1.37   0.17   1.09
    ## W1_Conspiracy_Total           25.50   3.99     6.40   0.00   1.11
    ## conspiracy2_sc               -11.97   2.71    -4.42   0.00   1.07
    ## conspiracy3_sc                22.52   4.04     5.58   0.00   1.39
    ## -----------------------------------------------------------------

``` r
plot_coefs(full_lab)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-43-2.png)<!-- -->

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

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

``` r
conspiracies <- conspiracies %>% 
  mutate(w2_conspiracy3_ihs = ihs(W2_Conspiracy_Theory3, best_theta))

conspiracies %>% 
  ggplot(aes(x = w2_conspiracy3_ihs)) +
  geom_histogram(colour = "darkgrey", fill = "lightblue")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-46-2.png)<!-- -->

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
daily_m_5g <- lm(W2_Conspiracy_Theory3 ~ 
                   W2_Newspaper_prefer1,
              data = conspiracies)

summ(daily_m_5g)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 11.96, p = 0.00
    ## R² = 0.01
    ## Adj. R² = 0.01 
    ## 
    ## Standard errors: OLS
    ## --------------------------------------------------------
    ##                              Est.   S.E.   t val.      p
    ## -------------------------- ------ ------ -------- ------
    ## (Intercept)                  9.82   0.70    14.09   0.00
    ## W2_Newspaper_prefer1         4.38   1.27     3.46   0.00
    ## --------------------------------------------------------

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

``` r
plot_coefs(se_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-51-2.png)<!-- -->

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
                   W2_Trust_Body6 + #distrust of scientists
                   W2_Newspaper_prefer1 + #daily_mail
                   W2_Newspaper_prefer5 + #guardian
                   W2_Newspaper_prefer9 + #sun
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
    ## F(12,1386) = 27.80, p = 0.00
    ## R² = 0.19
    ## Adj. R² = 0.19 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -1.24   2.70    -0.46   0.65       
    ## W2_Gender_binary2             -1.22   1.08    -1.13   0.26   1.05
    ## W1_Education_binary1          -0.80   1.14    -0.71   0.48   1.12
    ## W1_Income_2019                -4.42   1.54    -2.86   0.00   1.11
    ## age_sc                       -11.22   2.77    -4.05   0.00   1.27
    ## fis_con                        3.60   2.72     1.32   0.19   1.29
    ## nat                            7.61   2.37     3.21   0.00   1.26
    ## W2_Trust_Body6                23.81   2.15    11.09   0.00   1.04
    ## W2_Newspaper_prefer1           0.66   1.20     0.55   0.58   1.10
    ## W2_Newspaper_prefer5          -1.46   1.38    -1.05   0.29   1.14
    ## W2_Newspaper_prefer9           5.60   1.68     3.33   0.00   1.11
    ## W2_INFO_5                      9.58   1.92     4.98   0.00   1.36
    ## W2_INFO_9                      7.87   2.04     3.85   0.00   1.21
    ## -----------------------------------------------------------------

``` r
plot_coefs(se_pol_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_pol_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-52-2.png)<!-- -->

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
                        W2_Trust_Body6 + #distrust of scientists
                        W2_Newspaper_prefer1 + #daily_mail
                        W2_Newspaper_prefer5 + #guardian
                        W2_Newspaper_prefer9 + #sun
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
    ## F(20,1378) = 25.89, p = 0.00
    ## R² = 0.27
    ## Adj. R² = 0.26 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -9.79   3.84    -2.55   0.01       
    ## W2_Gender_binary2              0.12   1.06     0.11   0.91   1.11
    ## W1_Education_binary1          -0.47   1.09    -0.43   0.67   1.13
    ## W1_Income_2019                -3.01   1.53    -1.97   0.05   1.19
    ## age_sc                        -2.02   2.85    -0.71   0.48   1.47
    ## fis_con                        1.15   2.76     0.42   0.68   1.46
    ## nat                            3.56   2.34     1.53   0.13   1.35
    ## W2_Trust_Body6                17.09   2.15     7.94   0.00   1.15
    ## W2_Newspaper_prefer1           0.32   1.15     0.28   0.78   1.12
    ## W2_Newspaper_prefer5          -0.01   1.37    -0.01   0.99   1.22
    ## W2_Newspaper_prefer9           4.34   1.61     2.69   0.01   1.12
    ## W2_INFO_5                      7.40   1.86     3.98   0.00   1.40
    ## W2_INFO_9                      7.97   1.97     4.04   0.00   1.24
    ## SDO                           18.53   3.39     5.47   0.00   1.46
    ## RWA                           -8.39   3.51    -2.39   0.02   1.41
    ## W2_DAI_Total                  18.61   2.81     6.62   0.00   1.53
    ## W2_IOU_Total                 -13.25   3.20    -4.13   0.00   1.59
    ## W2_Paranoia_Total             12.09   2.75     4.39   0.00   1.78
    ## W2_Internal_Total              1.20   3.27     0.37   0.71   1.27
    ## W2_Chance_Total               -3.43   3.42    -1.00   0.32   1.92
    ## W2_PO_Total                    8.67   3.19     2.72   0.01   2.23
    ## -----------------------------------------------------------------

``` r
plot_coefs(se_polpsych_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_polpsych_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-53-2.png)<!-- -->

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
                  W2_Trust_Body6 + #distrust of scientists
                  W2_Newspaper_prefer1 + #daily_mail
                  W2_Newspaper_prefer5 + #guardian
                  W2_Newspaper_prefer9 + #sun
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
    ## F(21,1377) = 25.60, p = 0.00
    ## R² = 0.28
    ## Adj. R² = 0.27 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                  -11.82   3.32    -3.57   0.00       
    ## W2_Gender_binary2             -0.46   1.06    -0.44   0.66   1.13
    ## W1_Education_binary1          -0.16   1.09    -0.15   0.88   1.13
    ## W1_Income_2019                -2.16   1.52    -1.42   0.16   1.19
    ## age_sc                        -1.66   2.87    -0.58   0.56   1.51
    ## fis_con                        1.42   2.74     0.52   0.61   1.45
    ## nat                            3.67   2.31     1.59   0.11   1.33
    ## W2_Trust_Body6                16.47   2.14     7.71   0.00   1.14
    ## W2_Newspaper_prefer1           0.19   1.15     0.16   0.87   1.12
    ## W2_Newspaper_prefer5           0.40   1.37     0.29   0.77   1.23
    ## W2_Newspaper_prefer9           4.01   1.60     2.50   0.01   1.12
    ## W2_INFO_5                      6.69   1.86     3.59   0.00   1.42
    ## W2_INFO_9                      7.80   1.97     3.97   0.00   1.25
    ## SDO                           18.53   3.39     5.46   0.00   1.48
    ## RWA                           -9.87   3.54    -2.78   0.01   1.45
    ## W2_DAI_Total                  17.98   2.85     6.31   0.00   1.59
    ## W2_IOU_Total                 -12.60   3.19    -3.95   0.00   1.60
    ## W2_Paranoia_Total             11.33   2.74     4.14   0.00   1.78
    ## W2_PO_Total                    6.78   2.58     2.63   0.01   1.47
    ## threat                        -2.21   2.13    -1.03   0.30   1.21
    ## crt                            5.89   1.53     3.84   0.00   1.24
    ## CRT_test                      -0.01   1.08    -0.01   0.99   1.09
    ## -----------------------------------------------------------------

``` r
plot_coefs(multi_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(multi_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-54-2.png)<!-- -->

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
                  W2_Trust_Body6 + #distrust of scientists
                  W2_Newspaper_prefer1 + #daily_mail
                  W2_Newspaper_prefer5 + #guardian
                  W2_Newspaper_prefer9 + #sun
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

summ(int_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(22,1376) = 25.32, p = 0.00
    ## R² = 0.29
    ## Adj. R² = 0.28 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -8.38   3.42    -2.45   0.01       
    ## W2_Gender_binary2             -0.40   1.05    -0.38   0.70   1.13
    ## W1_Education_binary1          -0.19   1.08    -0.18   0.86   1.13
    ## W1_Income_2019                -2.10   1.51    -1.39   0.16   1.19
    ## age_sc                        -1.42   2.86    -0.50   0.62   1.51
    ## fis_con                        1.16   2.73     0.43   0.67   1.46
    ## nat                            3.42   2.30     1.49   0.14   1.33
    ## W2_Trust_Body6                16.53   2.13     7.78   0.00   1.14
    ## W2_Newspaper_prefer1           0.29   1.14     0.25   0.80   1.12
    ## W2_Newspaper_prefer5           0.26   1.36     0.19   0.85   1.24
    ## W2_Newspaper_prefer9           3.93   1.60     2.46   0.01   1.12
    ## W2_INFO_9                      7.26   1.96     3.70   0.00   1.25
    ## RWA                           -9.12   3.53    -2.58   0.01   1.45
    ## W2_DAI_Total                  17.32   2.84     6.10   0.00   1.60
    ## W2_IOU_Total                 -12.43   3.18    -3.91   0.00   1.60
    ## W2_Paranoia_Total             11.18   2.73     4.10   0.00   1.78
    ## W2_PO_Total                    6.21   2.57     2.41   0.02   1.47
    ## threat                        -1.58   2.13    -0.74   0.46   1.22
    ## crt                            5.68   1.53     3.71   0.00   1.24
    ## CRT_test                       0.05   1.07     0.05   0.96   1.09
    ## W2_INFO_5                     -4.81   3.57    -1.35   0.18   5.27
    ## SDO                            8.68   4.27     2.03   0.04   2.37
    ## W2_INFO_5:SDO                 33.03   8.76     3.77   0.00   6.23
    ## -----------------------------------------------------------------

``` r
plot_coefs(int_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-55-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(int_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-55-2.png)<!-- -->

``` r
AIC(multi_5g)
```

    ## [1] 12181.58

``` r
AIC(int_5g) # support for interaction term at this point
```

    ## [1] 12169.2

## DV 5G conspiracy - socio-economic variables + political/media + pol-psych + covid-threat and CRT + conspiracies

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
                W2_Trust_Body6 + #distrust of scientists
                W2_Newspaper_prefer1 + #daily_mail
                W2_Newspaper_prefer5 + #guardian
                W2_Newspaper_prefer9 + #sun
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
    ## F(24,1374) = 24.40, p = 0.00
    ## R² = 0.30
    ## Adj. R² = 0.29 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                  -12.09   3.54    -3.41   0.00       
    ## W2_Gender_binary2             -0.59   1.05    -0.57   0.57   1.13
    ## W1_Education_binary1           0.20   1.08     0.19   0.85   1.13
    ## W1_Income_2019                -1.79   1.50    -1.19   0.24   1.20
    ## age_sc                        -2.00   2.84    -0.70   0.48   1.51
    ## fis_con                        1.39   2.71     0.51   0.61   1.46
    ## nat                            2.44   2.31     1.05   0.29   1.36
    ## W2_Trust_Body6                14.44   2.16     6.69   0.00   1.20
    ## W2_Newspaper_prefer1          -0.75   1.15    -0.65   0.52   1.15
    ## W2_Newspaper_prefer5           0.69   1.35     0.51   0.61   1.24
    ## W2_Newspaper_prefer9           3.17   1.59     1.99   0.05   1.13
    ## W2_INFO_5                      5.93   1.85     3.21   0.00   1.43
    ## W2_INFO_9                      6.66   1.95     3.41   0.00   1.26
    ## SDO                           17.70   3.39     5.22   0.00   1.51
    ## RWA                          -11.91   3.53    -3.37   0.00   1.47
    ## W2_DAI_Total                  16.80   2.84     5.92   0.00   1.61
    ## W2_IOU_Total                 -11.80   3.19    -3.70   0.00   1.63
    ## W2_Paranoia_Total             10.29   2.72     3.79   0.00   1.79
    ## W2_PO_Total                    5.96   2.58     2.31   0.02   1.50
    ## threat                        -3.16   2.13    -1.49   0.14   1.23
    ## crt                            4.97   1.53     3.26   0.00   1.25
    ## CRT_test                      -0.37   1.07    -0.35   0.73   1.10
    ## W1_Conspiracy_Total            2.78   2.70     1.03   0.30   1.17
    ## conspiracy1_sc                 9.82   1.76     5.59   0.00   1.40
    ## conspiracy2_sc                 0.95   1.80     0.53   0.60   1.09
    ## -----------------------------------------------------------------

``` r
plot_coefs(full_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-57-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-57-2.png)<!-- -->

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
                W2_Trust_Body6 + #distrust of scientists
                W2_Newspaper_prefer1 + #daily_mail
                W2_Newspaper_prefer5 + #guardian
                W2_Newspaper_prefer9 + #sun
                #W2_INFO_5 + #social media
                W2_INFO_9 + #family and friends
                  
                #political-psychology variables
                #SDO +
                RWA +
                W2_DAI_Total +
                W2_IOU_Total +
                W2_Paranoia_Total +
                W2_PO_Total +
                
                #covid-anxety
                threat +
                
                #crt
                crt +
                CRT_test +
                    
                #conspiracies
                W1_Conspiracy_Total +
                conspiracy1_sc +
                conspiracy2_sc +
                
                # interactions
                (W2_INFO_5*SDO),
              data = conspiracies)

summ(full_int_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(25,1373) = 24.18, p = 0.00
    ## R² = 0.31
    ## Adj. R² = 0.29 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -8.85   3.63    -2.44   0.02       
    ## W2_Gender_binary2             -0.53   1.04    -0.51   0.61   1.13
    ## W1_Education_binary1           0.17   1.07     0.15   0.88   1.13
    ## W1_Income_2019                -1.73   1.50    -1.16   0.25   1.20
    ## age_sc                        -1.76   2.83    -0.62   0.53   1.51
    ## fis_con                        1.16   2.70     0.43   0.67   1.46
    ## nat                            2.22   2.30     0.97   0.33   1.36
    ## W2_Trust_Body6                14.49   2.15     6.74   0.00   1.20
    ## W2_Newspaper_prefer1          -0.63   1.14    -0.55   0.58   1.15
    ## W2_Newspaper_prefer5           0.56   1.35     0.41   0.68   1.24
    ## W2_Newspaper_prefer9           3.11   1.59     1.96   0.05   1.13
    ## W2_INFO_9                      6.17   1.95     3.17   0.00   1.27
    ## RWA                          -11.20   3.52    -3.18   0.00   1.48
    ## W2_DAI_Total                  16.21   2.83     5.73   0.00   1.62
    ## W2_IOU_Total                 -11.68   3.17    -3.68   0.00   1.63
    ## W2_Paranoia_Total             10.17   2.70     3.76   0.00   1.79
    ## W2_PO_Total                    5.38   2.57     2.09   0.04   1.51
    ## threat                        -2.54   2.12    -1.20   0.23   1.24
    ## crt                            4.77   1.52     3.14   0.00   1.25
    ## CRT_test                      -0.32   1.06    -0.30   0.76   1.10
    ## W1_Conspiracy_Total            3.07   2.68     1.14   0.25   1.17
    ## conspiracy1_sc                 9.61   1.75     5.49   0.00   1.40
    ## conspiracy2_sc                 0.87   1.79     0.48   0.63   1.09
    ## W2_INFO_5                     -5.19   3.53    -1.47   0.14   5.29
    ## SDO                            8.24   4.24     1.95   0.05   2.39
    ## W2_INFO_5:SDO                 31.94   8.67     3.69   0.00   6.24
    ## -----------------------------------------------------------------

``` r
plot_coefs(full_int_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_int_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-58-2.png)<!-- -->

``` r
# comparing AIC with and without interaction
AIC(full_5g)
```

    ## [1] 12152.01

``` r
AIC(full_int_5g) # support for interaction effect
```

    ## [1] 12140.24

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
daily_m_5g_ihs <- lm(w2_conspiracy3_ihs ~ 
                   W2_Newspaper_prefer1,
              data = conspiracies)

summ(daily_m_5g_ihs)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 20.66, p = 0.00
    ## R² = 0.01
    ## Adj. R² = 0.01 
    ## 
    ## Standard errors: OLS
    ## --------------------------------------------------------
    ##                              Est.   S.E.   t val.      p
    ## -------------------------- ------ ------ -------- ------
    ## (Intercept)                  0.41   0.01    29.29   0.00
    ## W2_Newspaper_prefer1         0.12   0.03     4.55   0.00
    ## --------------------------------------------------------

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

``` r
plot_coefs(se_5g_ihs)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-64-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_5g_ihs)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-64-2.png)<!-- -->

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
                   W2_Trust_Body6 + #distrust of scientists
                   W2_Newspaper_prefer1 + #daily_mail
                   W2_Newspaper_prefer5 + #guardian
                   W2_Newspaper_prefer9 + #sun
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
    ## F(12,1386) = 31.21, p = 0.00
    ## R² = 0.21
    ## Adj. R² = 0.21 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                   0.08   0.05     1.51   0.13       
    ## W2_Gender_binary2             0.04   0.02     1.99   0.05   1.05
    ## W1_Education_binary1         -0.01   0.02    -0.24   0.81   1.12
    ## W1_Income_2019               -0.11   0.03    -3.43   0.00   1.11
    ## age_sc                       -0.07   0.06    -1.34   0.18   1.27
    ## fis_con                       0.06   0.05     1.11   0.27   1.29
    ## nat                           0.18   0.05     3.81   0.00   1.26
    ## W2_Trust_Body6                0.52   0.04    12.20   0.00   1.04
    ## W2_Newspaper_prefer1          0.04   0.02     1.47   0.14   1.10
    ## W2_Newspaper_prefer5         -0.09   0.03    -3.34   0.00   1.14
    ## W2_Newspaper_prefer9          0.10   0.03     2.87   0.00   1.11
    ## W2_INFO_5                     0.23   0.04     6.01   0.00   1.36
    ## W2_INFO_9                     0.13   0.04     3.25   0.00   1.21
    ## ----------------------------------------------------------------

``` r
plot_coefs(se_pol_5g_ihs)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_pol_5g_ihs)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-65-2.png)<!-- -->

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
                        W2_Trust_Body6 + #distrust of scientists
                        W2_Newspaper_prefer1 + #daily_mail
                        W2_Newspaper_prefer5 + #guardian
                        W2_Newspaper_prefer9 + #sun
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
    ## F(20,1378) = 28.17, p = 0.00
    ## R² = 0.29
    ## Adj. R² = 0.28 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  -0.09   0.08    -1.19   0.23       
    ## W2_Gender_binary2             0.06   0.02     2.74   0.01   1.11
    ## W1_Education_binary1          0.00   0.02     0.16   0.87   1.13
    ## W1_Income_2019               -0.07   0.03    -2.36   0.02   1.19
    ## age_sc                        0.09   0.06     1.58   0.11   1.47
    ## fis_con                      -0.02   0.06    -0.44   0.66   1.46
    ## nat                           0.09   0.05     1.84   0.07   1.35
    ## W2_Trust_Body6                0.38   0.04     8.89   0.00   1.15
    ## W2_Newspaper_prefer1          0.02   0.02     0.72   0.47   1.12
    ## W2_Newspaper_prefer5         -0.04   0.03    -1.61   0.11   1.22
    ## W2_Newspaper_prefer9          0.07   0.03     2.30   0.02   1.12
    ## W2_INFO_5                     0.18   0.04     4.92   0.00   1.40
    ## W2_INFO_9                     0.14   0.04     3.63   0.00   1.24
    ## SDO                           0.36   0.07     5.38   0.00   1.46
    ## RWA                           0.08   0.07     1.08   0.28   1.41
    ## W2_DAI_Total                  0.39   0.06     7.01   0.00   1.53
    ## W2_IOU_Total                 -0.23   0.06    -3.61   0.00   1.59
    ## W2_Paranoia_Total             0.18   0.06     3.22   0.00   1.78
    ## W2_Internal_Total            -0.09   0.07    -1.35   0.18   1.27
    ## W2_Chance_Total              -0.10   0.07    -1.53   0.13   1.92
    ## W2_PO_Total                   0.18   0.06     2.77   0.01   2.23
    ## ----------------------------------------------------------------

``` r
plot_coefs(se_polpsych_5g_ihs)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_polpsych_5g_ihs)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-66-2.png)<!-- -->

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
                  W2_Trust_Body6 + #distrust of scientists
                  W2_Newspaper_prefer1 + #daily_mail
                  W2_Newspaper_prefer5 + #guardian
                  W2_Newspaper_prefer9 + #sun
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
    ## F(21,1377) = 29.22, p = 0.00
    ## R² = 0.31
    ## Adj. R² = 0.30 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  -0.25   0.07    -3.77   0.00       
    ## W2_Gender_binary2             0.04   0.02     1.94   0.05   1.13
    ## W1_Education_binary1          0.01   0.02     0.65   0.52   1.13
    ## W1_Income_2019               -0.06   0.03    -1.87   0.06   1.19
    ## age_sc                        0.07   0.06     1.27   0.21   1.51
    ## fis_con                      -0.02   0.05    -0.39   0.70   1.45
    ## nat                           0.08   0.05     1.68   0.09   1.33
    ## W2_Trust_Body6                0.38   0.04     8.92   0.00   1.14
    ## W2_Newspaper_prefer1          0.01   0.02     0.59   0.56   1.12
    ## W2_Newspaper_prefer5         -0.03   0.03    -1.10   0.27   1.23
    ## W2_Newspaper_prefer9          0.06   0.03     1.87   0.06   1.12
    ## W2_INFO_5                     0.16   0.04     4.39   0.00   1.42
    ## W2_INFO_9                     0.13   0.04     3.30   0.00   1.25
    ## SDO                           0.38   0.07     5.63   0.00   1.48
    ## RWA                           0.02   0.07     0.35   0.73   1.45
    ## W2_DAI_Total                  0.36   0.06     6.43   0.00   1.59
    ## W2_IOU_Total                 -0.21   0.06    -3.36   0.00   1.60
    ## W2_Paranoia_Total             0.16   0.05     2.88   0.00   1.78
    ## W2_PO_Total                   0.15   0.05     2.83   0.00   1.47
    ## threat                       -0.05   0.04    -1.17   0.24   1.21
    ## crt                           0.17   0.03     5.61   0.00   1.24
    ## CRT_test                      0.04   0.02     1.84   0.07   1.09
    ## ----------------------------------------------------------------

``` r
plot_coefs(multi_5g_ihs)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(multi_5g_ihs)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-67-2.png)<!-- -->

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
                  W2_Trust_Body6 + #distrust of scientists
                  W2_Newspaper_prefer1 + #daily_mail
                  W2_Newspaper_prefer5 + #guardian
                  W2_Newspaper_prefer9 + #sun
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
    ## F(22,1376) = 28.17, p = 0.00
    ## R² = 0.31
    ## Adj. R² = 0.30 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  -0.21   0.07    -3.06   0.00       
    ## W2_Gender_binary2             0.04   0.02     1.98   0.05   1.13
    ## W1_Education_binary1          0.01   0.02     0.63   0.53   1.13
    ## W1_Income_2019               -0.06   0.03    -1.86   0.06   1.19
    ## age_sc                        0.08   0.06     1.32   0.19   1.51
    ## fis_con                      -0.02   0.05    -0.44   0.66   1.46
    ## nat                           0.07   0.05     1.62   0.11   1.33
    ## W2_Trust_Body6                0.38   0.04     8.95   0.00   1.14
    ## W2_Newspaper_prefer1          0.01   0.02     0.64   0.52   1.12
    ## W2_Newspaper_prefer5         -0.03   0.03    -1.16   0.25   1.24
    ## W2_Newspaper_prefer9          0.06   0.03     1.84   0.07   1.12
    ## W2_INFO_9                     0.12   0.04     3.14   0.00   1.25
    ## RWA                           0.03   0.07     0.47   0.64   1.45
    ## W2_DAI_Total                  0.36   0.06     6.29   0.00   1.60
    ## W2_IOU_Total                 -0.21   0.06    -3.33   0.00   1.60
    ## W2_Paranoia_Total             0.16   0.05     2.86   0.00   1.78
    ## W2_PO_Total                   0.14   0.05     2.71   0.01   1.47
    ## threat                       -0.04   0.04    -1.00   0.32   1.22
    ## crt                           0.17   0.03     5.54   0.00   1.24
    ## CRT_test                      0.04   0.02     1.87   0.06   1.09
    ## W2_INFO_5                     0.03   0.07     0.43   0.66   5.27
    ## SDO                           0.27   0.09     3.13   0.00   2.37
    ## W2_INFO_5:SDO                 0.38   0.17     2.16   0.03   6.23
    ## ----------------------------------------------------------------

``` r
plot_coefs(int_5g_ihs)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-68-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(int_5g_ihs)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-68-2.png)<!-- -->

``` r
AIC(multi_5g_ihs)
```

    ## [1] 1216.787

``` r
AIC(int_5g_ihs) # no strong support for interaction term at this point
```

    ## [1] 1214.04

## DV 5G IHS conspiracy - socio-economic variables + political/media + pol-psych + covid-threat and CRT + conspiracies

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
                W2_Trust_Body6 + #distrust of scientists
                W2_Newspaper_prefer1 + #daily_mail
                W2_Newspaper_prefer5 + #guardian
                W2_Newspaper_prefer9 + #sun
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
    ## F(24,1374) = 28.46, p = 0.00
    ## R² = 0.33
    ## Adj. R² = 0.32 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  -0.23   0.07    -3.25   0.00       
    ## W2_Gender_binary2             0.04   0.02     1.82   0.07   1.13
    ## W1_Education_binary1          0.02   0.02     1.02   0.31   1.13
    ## W1_Income_2019               -0.05   0.03    -1.54   0.12   1.20
    ## age_sc                        0.07   0.06     1.18   0.24   1.51
    ## fis_con                      -0.02   0.05    -0.40   0.69   1.46
    ## nat                           0.05   0.05     1.20   0.23   1.36
    ## W2_Trust_Body6                0.33   0.04     7.72   0.00   1.20
    ## W2_Newspaper_prefer1         -0.01   0.02    -0.36   0.72   1.15
    ## W2_Newspaper_prefer5         -0.02   0.03    -0.88   0.38   1.24
    ## W2_Newspaper_prefer9          0.04   0.03     1.29   0.20   1.13
    ## W2_INFO_5                     0.14   0.04     3.96   0.00   1.43
    ## W2_INFO_9                     0.10   0.04     2.63   0.01   1.26
    ## SDO                           0.36   0.07     5.32   0.00   1.51
    ## RWA                          -0.02   0.07    -0.32   0.75   1.47
    ## W2_DAI_Total                  0.34   0.06     6.01   0.00   1.61
    ## W2_IOU_Total                 -0.19   0.06    -2.95   0.00   1.63
    ## W2_Paranoia_Total             0.13   0.05     2.43   0.02   1.79
    ## W2_PO_Total                   0.13   0.05     2.50   0.01   1.50
    ## threat                       -0.07   0.04    -1.58   0.11   1.23
    ## crt                           0.15   0.03     4.95   0.00   1.25
    ## CRT_test                      0.03   0.02     1.47   0.14   1.10
    ## W1_Conspiracy_Total           0.04   0.05     0.84   0.40   1.17
    ## conspiracy1_sc                0.23   0.03     6.58   0.00   1.40
    ## conspiracy2_sc               -0.02   0.04    -0.50   0.61   1.09
    ## ----------------------------------------------------------------

``` r
plot_coefs(full_5g_ihs)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-70-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_5g_ihs)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-70-2.png)<!-- -->

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
                W2_Trust_Body6 + #distrust of scientists
                W2_Newspaper_prefer1 + #daily_mail
                W2_Newspaper_prefer5 + #guardian
                W2_Newspaper_prefer9 + #sun
                #W2_INFO_5 + #social media
                W2_INFO_9 + #family and friends
                  
                #political-psychology variables
                #SDO +
                RWA +
                W2_DAI_Total +
                W2_IOU_Total +
                W2_Paranoia_Total +
                W2_PO_Total +
                
                #covid-anxety
                threat +
                
                #crt
                crt +
                CRT_test +
                    
                #conspiracies
                W1_Conspiracy_Total +
                conspiracy1_sc +
                conspiracy2_sc +
                
                # interactions
                (W2_INFO_5*SDO),
              data = conspiracies)

summ(full_int_5g_ihs, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(25,1373) = 27.55, p = 0.00
    ## R² = 0.33
    ## Adj. R² = 0.32 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  -0.19   0.07    -2.67   0.01       
    ## W2_Gender_binary2             0.04   0.02     1.86   0.06   1.13
    ## W1_Education_binary1          0.02   0.02     1.01   0.31   1.13
    ## W1_Income_2019               -0.05   0.03    -1.52   0.13   1.20
    ## age_sc                        0.07   0.06     1.23   0.22   1.51
    ## fis_con                      -0.02   0.05    -0.44   0.66   1.46
    ## nat                           0.05   0.05     1.15   0.25   1.36
    ## W2_Trust_Body6                0.33   0.04     7.74   0.00   1.20
    ## W2_Newspaper_prefer1         -0.01   0.02    -0.30   0.76   1.15
    ## W2_Newspaper_prefer5         -0.03   0.03    -0.94   0.35   1.24
    ## W2_Newspaper_prefer9          0.04   0.03     1.27   0.21   1.13
    ## W2_INFO_9                     0.10   0.04     2.49   0.01   1.27
    ## RWA                          -0.01   0.07    -0.21   0.83   1.48
    ## W2_DAI_Total                  0.33   0.06     5.89   0.00   1.62
    ## W2_IOU_Total                 -0.18   0.06    -2.93   0.00   1.63
    ## W2_Paranoia_Total             0.13   0.05     2.41   0.02   1.79
    ## W2_PO_Total                   0.12   0.05     2.37   0.02   1.51
    ## threat                       -0.06   0.04    -1.41   0.16   1.24
    ## crt                           0.15   0.03     4.88   0.00   1.25
    ## CRT_test                      0.03   0.02     1.50   0.13   1.10
    ## W1_Conspiracy_Total           0.05   0.05     0.90   0.37   1.17
    ## conspiracy1_sc                0.23   0.03     6.52   0.00   1.40
    ## conspiracy2_sc               -0.02   0.04    -0.53   0.60   1.09
    ## W2_INFO_5                     0.02   0.07     0.32   0.75   5.29
    ## SDO                           0.25   0.08     3.00   0.00   2.39
    ## W2_INFO_5:SDO                 0.35   0.17     2.05   0.04   6.24
    ## ----------------------------------------------------------------

``` r
plot_coefs(full_int_5g_ihs)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-71-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_int_5g_ihs)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-71-2.png)<!-- -->

``` r
# comparing AIC with and without interaction
AIC(full_5g_ihs)
```

    ## [1] 1173.727

``` r
AIC(full_int_5g_ihs) # no strong support for interaction effect
```

    ## [1] 1171.47

## DV 5G conspiracy poisson - socio-economic variables + political/media + pol-psych + covid-threat and CRT + conspiracies

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
                W2_Trust_Body6 + #distrust of scientists
                W2_Newspaper_prefer1 + #daily_mail
                W2_Newspaper_prefer5 + #guardian
                W2_Newspaper_prefer9 + #sun
                W2_INFO_5 + #social media
                W2_INFO_9 + #family and friends
                  
                #political-psychology variables
                SDO +
                RWA +
                W2_DAI_Total +
                W2_IOU_Total +
                W2_Paranoia_Total +
                W2_PO_Total +
                
                #covid-anxety
                threat +
                
                #crt
                crt +
                CRT_test +
                  
                #conspiracies
                W1_Conspiracy_Total +
                conspiracy1_sc +
                conspiracy2_sc,
              family = "poisson", data = conspiracies)

summary(full_5g_poiss)
```

    ## 
    ## Call:
    ## glm(formula = W2_Conspiracy_Theory3 ~ W2_Gender_binary + W1_Education_binary + 
    ##     W1_Income_2019 + age_sc + fis_con + nat + W2_Trust_Body6 + 
    ##     W2_Newspaper_prefer1 + W2_Newspaper_prefer5 + W2_Newspaper_prefer9 + 
    ##     W2_INFO_5 + W2_INFO_9 + SDO + RWA + W2_DAI_Total + W2_IOU_Total + 
    ##     W2_Paranoia_Total + W2_PO_Total + threat + crt + CRT_test + 
    ##     W1_Conspiracy_Total + conspiracy1_sc + conspiracy2_sc, family = "poisson", 
    ##     data = conspiracies)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -13.6472   -3.2303   -2.0538   -0.3159   26.5192  
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          -0.001174   0.062726  -0.019  0.98507    
    ## W2_Gender_binary2     0.011335   0.017103   0.663  0.50747    
    ## W1_Education_binary1  0.019999   0.017546   1.140  0.25437    
    ## W1_Income_2019       -0.119825   0.026253  -4.564 5.01e-06 ***
    ## age_sc               -0.474668   0.045501 -10.432  < 2e-16 ***
    ## fis_con              -0.105481   0.040983  -2.574  0.01006 *  
    ## nat                   0.076049   0.039096   1.945  0.05175 .  
    ## W2_Trust_Body6        0.878701   0.031056  28.294  < 2e-16 ***
    ## W2_Newspaper_prefer1 -0.006516   0.017570  -0.371  0.71076    
    ## W2_Newspaper_prefer5 -0.039937   0.022576  -1.769  0.07689 .  
    ## W2_Newspaper_prefer9  0.170930   0.021288   8.029 9.80e-16 ***
    ## W2_INFO_5             0.296121   0.030479   9.716  < 2e-16 ***
    ## W2_INFO_9             0.460369   0.032704  14.077  < 2e-16 ***
    ## SDO                   1.799435   0.058050  30.998  < 2e-16 ***
    ## RWA                  -0.489913   0.062247  -7.870 3.53e-15 ***
    ## W2_DAI_Total          1.440976   0.048572  29.667  < 2e-16 ***
    ## W2_IOU_Total         -1.211944   0.050446 -24.025  < 2e-16 ***
    ## W2_Paranoia_Total     0.773044   0.045579  16.961  < 2e-16 ***
    ## W2_PO_Total           0.400397   0.043538   9.197  < 2e-16 ***
    ## threat               -0.259534   0.036206  -7.168 7.59e-13 ***
    ## crt                   0.627731   0.031149  20.153  < 2e-16 ***
    ## CRT_test              0.014986   0.017262   0.868  0.38532    
    ## W1_Conspiracy_Total   0.389722   0.043220   9.017  < 2e-16 ***
    ## conspiracy1_sc        0.972708   0.030165  32.247  < 2e-16 ***
    ## conspiracy2_sc       -0.085401   0.030516  -2.799  0.00513 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 41421  on 1398  degrees of freedom
    ## Residual deviance: 24904  on 1374  degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## AIC: 27961
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
plot_coefs(full_5g_poiss)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-73-1.png)<!-- -->

``` r
full_int_5g_poiss <- glm(W2_Conspiracy_Theory3 ~
                #socio-economic variables
                W2_Gender_binary +
                W1_Education_binary +
                W1_Income_2019 +
                age_sc +
                  
                #political and media variables
                fis_con +
                nat +
                W2_Trust_Body6 + #distrust of scientists
                W2_Newspaper_prefer1 + #daily_mail
                W2_Newspaper_prefer5 + #guardian
                W2_Newspaper_prefer9 + #sun
                #W2_INFO_5 + #social media
                W2_INFO_9 + #family and friends
                  
                #political-psychology variables
                #SDO +
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
                    
                #conspiracies
                W1_Conspiracy_Total +
                conspiracy1_sc +
                conspiracy2_sc +
                
                # interactions
                (W2_INFO_5*SDO),
              family = "poisson", data = conspiracies)

summary(full_int_5g_poiss)
```

    ## 
    ## Call:
    ## glm(formula = W2_Conspiracy_Theory3 ~ W2_Gender_binary + W1_Education_binary + 
    ##     W1_Income_2019 + age_sc + fis_con + nat + W2_Trust_Body6 + 
    ##     W2_Newspaper_prefer1 + W2_Newspaper_prefer5 + W2_Newspaper_prefer9 + 
    ##     W2_INFO_9 + RWA + W2_DAI_Total + W2_IOU_Total + W2_Paranoia_Total + 
    ##     W2_PO_Total + threat + crt + CRT_test + W1_Conspiracy_Total + 
    ##     conspiracy1_sc + conspiracy2_sc + (W2_INFO_5 * SDO), family = "poisson", 
    ##     data = conspiracies)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -13.6525   -3.2311   -2.0496   -0.3177   26.5026  
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.005360   0.068196   0.079  0.93736    
    ## W2_Gender_binary2     0.011166   0.017116   0.652  0.51416    
    ## W1_Education_binary1  0.019863   0.017555   1.131  0.25786    
    ## W1_Income_2019       -0.119902   0.026255  -4.567 4.95e-06 ***
    ## age_sc               -0.475235   0.045560 -10.431  < 2e-16 ***
    ## fis_con              -0.105301   0.040988  -2.569  0.01020 *  
    ## nat                   0.075355   0.039203   1.922  0.05459 .  
    ## W2_Trust_Body6        0.878642   0.031059  28.290  < 2e-16 ***
    ## W2_Newspaper_prefer1 -0.006479   0.017570  -0.369  0.71234    
    ## W2_Newspaper_prefer5 -0.040066   0.022581  -1.774  0.07600 .  
    ## W2_Newspaper_prefer9  0.170661   0.021316   8.006 1.18e-15 ***
    ## W2_INFO_9             0.459420   0.032935  13.949  < 2e-16 ***
    ## RWA                  -0.488539   0.062506  -7.816 5.46e-15 ***
    ## W2_DAI_Total          1.441093   0.048573  29.669  < 2e-16 ***
    ## W2_IOU_Total         -1.212713   0.050549 -23.991  < 2e-16 ***
    ## W2_Paranoia_Total     0.773381   0.045597  16.961  < 2e-16 ***
    ## W2_PO_Total           0.399813   0.043601   9.170  < 2e-16 ***
    ## threat               -0.259109   0.036250  -7.148 8.81e-13 ***
    ## crt                   0.627564   0.031154  20.144  < 2e-16 ***
    ## CRT_test              0.015037   0.017264   0.871  0.38373    
    ## W1_Conspiracy_Total   0.389893   0.043227   9.020  < 2e-16 ***
    ## conspiracy1_sc        0.972676   0.030169  32.241  < 2e-16 ***
    ## conspiracy2_sc       -0.084508   0.030735  -2.750  0.00597 ** 
    ## W2_INFO_5             0.280500   0.070911   3.956 7.63e-05 ***
    ## SDO                   1.784174   0.085329  20.909  < 2e-16 ***
    ## W2_INFO_5:SDO         0.036901   0.151243   0.244  0.80724    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 41421  on 1398  degrees of freedom
    ## Residual deviance: 24904  on 1373  degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## AIC: 27963
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
plot_coefs(full_int_5g_poiss)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-74-1.png)<!-- -->

``` r
# comparing AIC with and without interaction
AIC(full_5g_poiss)
```

    ## [1] 27961.07

``` r
AIC(full_int_5g_poiss) # no support for interaction effect
```

    ## [1] 27963.01

## Brief foray into random forest for 5G belief

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

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-76-1.png)<!-- -->

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
daily_m_meat <- lm(W2_Conspiracy_Theory2 ~ W2_Newspaper_prefer1,
              data = conspiracies)

summ(daily_m_meat)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: W2_Conspiracy_Theory2
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 1.33, p = 0.25
    ## R² = 0.00
    ## Adj. R² = 0.00 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------------
    ##                               Est.   S.E.   t val.      p
    ## -------------------------- ------- ------ -------- ------
    ## (Intercept)                  63.38   0.92    69.18   0.00
    ## W2_Newspaper_prefer1          1.92   1.66     1.15   0.25
    ## ---------------------------------------------------------

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

``` r
plot_coefs(se_meat)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-81-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_meat)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-81-2.png)<!-- -->

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
                   W2_Trust_Body6 + #distrust of scientists
                   W2_Newspaper_prefer1 + #daily_mail
                   W2_Newspaper_prefer5 + #guardian
                   W2_Newspaper_prefer9 + #sun
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
    ## F(14,1384) = 4.49, p = 0.00
    ## R² = 0.04
    ## Adj. R² = 0.03 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   53.26   3.91    13.62   0.00       
    ## W2_Gender_binary2             -0.02   1.55    -0.02   0.99   1.06
    ## W1_Education_binary1          -0.82   1.63    -0.50   0.62   1.12
    ## W1_Income_2019                 5.83   2.23     2.62   0.01   1.12
    ## age_sc                         5.91   3.98     1.49   0.14   1.27
    ## right                          1.43   5.20     0.27   0.78   1.98
    ## soc_con                        1.49   3.37     0.44   0.66   1.50
    ## fis_con                       -0.17   4.97    -0.03   0.97   2.09
    ## nat                           14.56   3.43     4.24   0.00   1.29
    ## W2_Trust_Body6               -13.52   3.13    -4.32   0.00   1.08
    ## W2_Newspaper_prefer1           0.54   1.73     0.31   0.75   1.11
    ## W2_Newspaper_prefer5           1.09   2.00     0.55   0.59   1.16
    ## W2_Newspaper_prefer9           1.64   2.41     0.68   0.49   1.11
    ## W2_INFO_5                     -0.31   2.76    -0.11   0.91   1.37
    ## W2_INFO_9                      0.18   2.93     0.06   0.95   1.21
    ## -----------------------------------------------------------------

``` r
plot_coefs(se_pol_meat)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-82-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_pol_meat)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-82-2.png)<!-- -->

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
                        W2_Trust_Body6 + #distrust of scientists
                        W2_Newspaper_prefer1 + #daily_mail
                        W2_Newspaper_prefer5 + #guardian
                        W2_Newspaper_prefer9 + #sun
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
    ## F(20,1378) = 5.71, p = 0.00
    ## R² = 0.08
    ## Adj. R² = 0.06 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  34.26   5.68     6.03   0.00       
    ## W2_Gender_binary2            -0.78   1.56    -0.50   0.62   1.11
    ## W1_Education_binary1         -0.78   1.61    -0.48   0.63   1.13
    ## W1_Income_2019                5.00   2.26     2.21   0.03   1.19
    ## age_sc                        4.97   4.21     1.18   0.24   1.47
    ## fis_con                       1.37   4.09     0.33   0.74   1.46
    ## nat                          13.24   3.46     3.83   0.00   1.35
    ## W2_Trust_Body6               -9.73   3.19    -3.05   0.00   1.15
    ## W2_Newspaper_prefer1          0.66   1.71     0.38   0.70   1.12
    ## W2_Newspaper_prefer5          0.56   2.03     0.28   0.78   1.22
    ## W2_Newspaper_prefer9          0.87   2.38     0.36   0.72   1.12
    ## W2_INFO_5                    -1.85   2.75    -0.67   0.50   1.40
    ## W2_INFO_9                    -1.93   2.92    -0.66   0.51   1.24
    ## SDO                          -2.41   5.02    -0.48   0.63   1.46
    ## RWA                          -3.57   5.20    -0.69   0.49   1.41
    ## W2_DAI_Total                  5.05   4.16     1.21   0.22   1.53
    ## W2_IOU_Total                 16.60   4.74     3.50   0.00   1.59
    ## W2_Paranoia_Total            -5.83   4.07    -1.43   0.15   1.78
    ## W2_Internal_Total            18.90   4.83     3.91   0.00   1.27
    ## W2_Chance_Total              15.08   5.06     2.98   0.00   1.92
    ## W2_PO_Total                  -7.97   4.72    -1.69   0.09   2.23
    ## ----------------------------------------------------------------

``` r
plot_coefs(se_polpsych_meat)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-83-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_polpsych_meat)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-83-2.png)<!-- -->

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
                  W2_Trust_Body6 + #distrust of scientists
                  W2_Newspaper_prefer1 + #daily_mail
                  W2_Newspaper_prefer5 + #guardian
                  W2_Newspaper_prefer9 + #sun
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
    ## F(23,1375) = 5.86, p = 0.00
    ## R² = 0.09
    ## Adj. R² = 0.07 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  31.63   5.83     5.43   0.00       
    ## W2_Gender_binary2            -0.60   1.57    -0.39   0.70   1.13
    ## W1_Education_binary1         -1.12   1.61    -0.70   0.49   1.13
    ## W1_Income_2019                4.28   2.26     1.89   0.06   1.21
    ## age_sc                        2.05   4.28     0.48   0.63   1.54
    ## fis_con                       1.46   4.07     0.36   0.72   1.46
    ## nat                          12.90   3.44     3.75   0.00   1.35
    ## W2_Trust_Body6               -8.67   3.19    -2.72   0.01   1.16
    ## W2_Newspaper_prefer1          0.69   1.70     0.40   0.69   1.12
    ## W2_Newspaper_prefer5         -0.12   2.02    -0.06   0.95   1.23
    ## W2_Newspaper_prefer9          0.63   2.38     0.26   0.79   1.13
    ## W2_INFO_5                    -1.91   2.76    -0.69   0.49   1.42
    ## W2_INFO_9                    -3.24   2.92    -1.11   0.27   1.26
    ## W2_Internal_Total            20.62   4.82     4.27   0.00   1.28
    ## W2_Chance_Total              15.27   5.03     3.03   0.00   1.93
    ## SDO                          -0.17   5.03    -0.03   0.97   1.49
    ## RWA                          -4.53   5.24    -0.87   0.39   1.45
    ## W2_DAI_Total                  2.20   4.23     0.52   0.60   1.60
    ## W2_IOU_Total                 14.01   4.75     2.95   0.00   1.62
    ## W2_Paranoia_Total            -6.53   4.06    -1.61   0.11   1.79
    ## W2_PO_Total                  -7.78   4.70    -1.66   0.10   2.23
    ## threat                       13.49   3.17     4.26   0.00   1.22
    ## crt                          -2.45   2.27    -1.08   0.28   1.24
    ## CRT_test                     -1.22   1.59    -0.77   0.44   1.09
    ## ----------------------------------------------------------------

``` r
plot_coefs(multi_meat)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-84-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(multi_meat)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-84-2.png)<!-- -->

## DV Chinese meat market belief - full model incl. conspiracy ideation

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
                  W2_Trust_Body6 + #distrust of scientists
                  W2_Newspaper_prefer1 + #daily_mail
                  W2_Newspaper_prefer5 + #guardian
                  W2_Newspaper_prefer9 + #sun
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
    ## F(26,1372) = 6.04, p = 0.00
    ## R² = 0.10
    ## Adj. R² = 0.09 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   29.25   5.98     4.89   0.00       
    ## W2_Gender_binary2             -0.47   1.56    -0.30   0.76   1.13
    ## W1_Education_binary1          -1.51   1.60    -0.95   0.34   1.13
    ## W1_Income_2019                 3.80   2.25     1.68   0.09   1.22
    ## age_sc                         2.36   4.25     0.55   0.58   1.54
    ## fis_con                        1.57   4.05     0.39   0.70   1.47
    ## nat                           14.05   3.43     4.10   0.00   1.36
    ## W2_Trust_Body6                -7.16   3.28    -2.18   0.03   1.25
    ## W2_Newspaper_prefer1           1.81   1.71     1.06   0.29   1.15
    ## W2_Newspaper_prefer5          -0.35   2.01    -0.18   0.86   1.24
    ## W2_Newspaper_prefer9           1.52   2.37     0.64   0.52   1.14
    ## W2_INFO_5                     -1.34   2.76    -0.49   0.63   1.44
    ## W2_INFO_9                     -2.06   2.93    -0.70   0.48   1.28
    ## SDO                            1.26   5.09     0.25   0.80   1.55
    ## RWA                           -2.57   5.27    -0.49   0.63   1.48
    ## W2_DAI_Total                   3.49   4.28     0.82   0.41   1.66
    ## W2_IOU_Total                  12.61   4.78     2.64   0.01   1.66
    ## W2_Paranoia_Total             -5.42   4.07    -1.33   0.18   1.82
    ## W2_Internal_Total             21.12   4.81     4.39   0.00   1.28
    ## W2_Chance_Total               15.09   5.00     3.02   0.00   1.93
    ## W2_PO_Total                   -7.55   4.72    -1.60   0.11   2.28
    ## threat                        14.46   3.16     4.58   0.00   1.23
    ## crt                           -1.62   2.28    -0.71   0.48   1.26
    ## CRT_test                      -0.93   1.59    -0.58   0.56   1.10
    ## W1_Conspiracy_Total            2.38   4.02     0.59   0.55   1.18
    ## conspiracy1_sc               -11.91   2.63    -4.54   0.00   1.41
    ## conspiracy3_sc                 2.46   4.01     0.61   0.54   1.43
    ## -----------------------------------------------------------------

``` r
plot_coefs(full_meat)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-85-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_meat)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-85-2.png)<!-- -->

# Summary of final models

``` r
# Chinese lab belief - full set of variables
summ(full_lab)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(23,1375) = 25.80, p = 0.00
    ## R² = 0.30
    ## Adj. R² = 0.29 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------
    ##                                Est.   S.E.   t val.      p
    ## -------------------------- -------- ------ -------- ------
    ## (Intercept)                  -14.19   5.33    -2.66   0.01
    ## W2_Gender_binary2              1.00   1.58     0.63   0.53
    ## W1_Education_binary1          -3.48   1.63    -2.14   0.03
    ## W1_Income_2019                -3.00   2.27    -1.32   0.19
    ## age_sc                         3.73   4.30     0.87   0.39
    ## fis_con                        1.80   4.11     0.44   0.66
    ## nat                           11.52   3.49     3.30   0.00
    ## W2_Trust_Body6                10.00   3.31     3.02   0.00
    ## W2_Newspaper_prefer1           9.45   1.72     5.49   0.00
    ## W2_Newspaper_prefer5          -1.92   2.05    -0.94   0.35
    ## W2_Newspaper_prefer9           7.74   2.41     3.21   0.00
    ## W2_INFO_5                      4.36   2.81     1.55   0.12
    ## W2_INFO_9                      9.73   2.96     3.29   0.00
    ## SDO                           12.01   5.18     2.32   0.02
    ## RWA                           15.37   5.34     2.88   0.00
    ## W2_DAI_Total                  11.35   4.32     2.63   0.01
    ## W2_IOU_Total                 -10.67   4.78    -2.23   0.03
    ## W2_Paranoia_Total              7.87   3.99     1.97   0.05
    ## threat                         9.27   3.22     2.88   0.00
    ## crt                            6.38   2.32     2.76   0.01
    ## CRT_test                       2.21   1.62     1.37   0.17
    ## W1_Conspiracy_Total           25.50   3.99     6.40   0.00
    ## conspiracy2_sc               -11.97   2.71    -4.42   0.00
    ## conspiracy3_sc                22.52   4.04     5.58   0.00
    ## ----------------------------------------------------------

``` r
plot_coefs(full_lab) 
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-86-1.png)<!-- -->

``` r
# 5G belief - full set of variables, raw data is DV
summ(full_5g)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory3
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(24,1374) = 24.40, p = 0.00
    ## R² = 0.30
    ## Adj. R² = 0.29 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------
    ##                                Est.   S.E.   t val.      p
    ## -------------------------- -------- ------ -------- ------
    ## (Intercept)                  -12.09   3.54    -3.41   0.00
    ## W2_Gender_binary2             -0.59   1.05    -0.57   0.57
    ## W1_Education_binary1           0.20   1.08     0.19   0.85
    ## W1_Income_2019                -1.79   1.50    -1.19   0.24
    ## age_sc                        -2.00   2.84    -0.70   0.48
    ## fis_con                        1.39   2.71     0.51   0.61
    ## nat                            2.44   2.31     1.05   0.29
    ## W2_Trust_Body6                14.44   2.16     6.69   0.00
    ## W2_Newspaper_prefer1          -0.75   1.15    -0.65   0.52
    ## W2_Newspaper_prefer5           0.69   1.35     0.51   0.61
    ## W2_Newspaper_prefer9           3.17   1.59     1.99   0.05
    ## W2_INFO_5                      5.93   1.85     3.21   0.00
    ## W2_INFO_9                      6.66   1.95     3.41   0.00
    ## SDO                           17.70   3.39     5.22   0.00
    ## RWA                          -11.91   3.53    -3.37   0.00
    ## W2_DAI_Total                  16.80   2.84     5.92   0.00
    ## W2_IOU_Total                 -11.80   3.19    -3.70   0.00
    ## W2_Paranoia_Total             10.29   2.72     3.79   0.00
    ## W2_PO_Total                    5.96   2.58     2.31   0.02
    ## threat                        -3.16   2.13    -1.49   0.14
    ## crt                            4.97   1.53     3.26   0.00
    ## CRT_test                      -0.37   1.07    -0.35   0.73
    ## W1_Conspiracy_Total            2.78   2.70     1.03   0.30
    ## conspiracy1_sc                 9.82   1.76     5.59   0.00
    ## conspiracy2_sc                 0.95   1.80     0.53   0.60
    ## ----------------------------------------------------------

``` r
plot_coefs(full_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-87-1.png)<!-- -->

``` r
# 5G belief - full set of variables and interaction term
# DV is IHS transformed
# NOTE: interaction is significant in the non-transformed DV model as well
summ(full_int_5g_ihs)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(25,1373) = 27.55, p = 0.00
    ## R² = 0.33
    ## Adj. R² = 0.32 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------------
    ##                               Est.   S.E.   t val.      p
    ## -------------------------- ------- ------ -------- ------
    ## (Intercept)                  -0.19   0.07    -2.67   0.01
    ## W2_Gender_binary2             0.04   0.02     1.86   0.06
    ## W1_Education_binary1          0.02   0.02     1.01   0.31
    ## W1_Income_2019               -0.05   0.03    -1.52   0.13
    ## age_sc                        0.07   0.06     1.23   0.22
    ## fis_con                      -0.02   0.05    -0.44   0.66
    ## nat                           0.05   0.05     1.15   0.25
    ## W2_Trust_Body6                0.33   0.04     7.74   0.00
    ## W2_Newspaper_prefer1         -0.01   0.02    -0.30   0.76
    ## W2_Newspaper_prefer5         -0.03   0.03    -0.94   0.35
    ## W2_Newspaper_prefer9          0.04   0.03     1.27   0.21
    ## W2_INFO_9                     0.10   0.04     2.49   0.01
    ## RWA                          -0.01   0.07    -0.21   0.83
    ## W2_DAI_Total                  0.33   0.06     5.89   0.00
    ## W2_IOU_Total                 -0.18   0.06    -2.93   0.00
    ## W2_Paranoia_Total             0.13   0.05     2.41   0.02
    ## W2_PO_Total                   0.12   0.05     2.37   0.02
    ## threat                       -0.06   0.04    -1.41   0.16
    ## crt                           0.15   0.03     4.88   0.00
    ## CRT_test                      0.03   0.02     1.50   0.13
    ## W1_Conspiracy_Total           0.05   0.05     0.90   0.37
    ## conspiracy1_sc                0.23   0.03     6.52   0.00
    ## conspiracy2_sc               -0.02   0.04    -0.53   0.60
    ## W2_INFO_5                     0.02   0.07     0.32   0.75
    ## SDO                           0.25   0.08     3.00   0.00
    ## W2_INFO_5:SDO                 0.35   0.17     2.05   0.04
    ## ---------------------------------------------------------

``` r
plot_coefs(full_int_5g_ihs)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-88-1.png)<!-- -->

``` r
# Chinese meat market model - full set of variables
summ(full_meat)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory2
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(26,1372) = 6.04, p = 0.00
    ## R² = 0.10
    ## Adj. R² = 0.09 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------
    ##                                Est.   S.E.   t val.      p
    ## -------------------------- -------- ------ -------- ------
    ## (Intercept)                   29.25   5.98     4.89   0.00
    ## W2_Gender_binary2             -0.47   1.56    -0.30   0.76
    ## W1_Education_binary1          -1.51   1.60    -0.95   0.34
    ## W1_Income_2019                 3.80   2.25     1.68   0.09
    ## age_sc                         2.36   4.25     0.55   0.58
    ## fis_con                        1.57   4.05     0.39   0.70
    ## nat                           14.05   3.43     4.10   0.00
    ## W2_Trust_Body6                -7.16   3.28    -2.18   0.03
    ## W2_Newspaper_prefer1           1.81   1.71     1.06   0.29
    ## W2_Newspaper_prefer5          -0.35   2.01    -0.18   0.86
    ## W2_Newspaper_prefer9           1.52   2.37     0.64   0.52
    ## W2_INFO_5                     -1.34   2.76    -0.49   0.63
    ## W2_INFO_9                     -2.06   2.93    -0.70   0.48
    ## SDO                            1.26   5.09     0.25   0.80
    ## RWA                           -2.57   5.27    -0.49   0.63
    ## W2_DAI_Total                   3.49   4.28     0.82   0.41
    ## W2_IOU_Total                  12.61   4.78     2.64   0.01
    ## W2_Paranoia_Total             -5.42   4.07    -1.33   0.18
    ## W2_Internal_Total             21.12   4.81     4.39   0.00
    ## W2_Chance_Total               15.09   5.00     3.02   0.00
    ## W2_PO_Total                   -7.55   4.72    -1.60   0.11
    ## threat                        14.46   3.16     4.58   0.00
    ## crt                           -1.62   2.28    -0.71   0.48
    ## CRT_test                      -0.93   1.59    -0.58   0.56
    ## W1_Conspiracy_Total            2.38   4.02     0.59   0.55
    ## conspiracy1_sc               -11.91   2.63    -4.54   0.00
    ## conspiracy3_sc                 2.46   4.01     0.61   0.54
    ## ----------------------------------------------------------

``` r
plot_coefs(full_meat)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-89-1.png)<!-- -->

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
  conspiracies %>% dplyr::select(pid,W2_SocialDistance11,
                                 W2_SocialDistance12,
                                 W2_C19_Vax_Self,
                                 W2_C19_Vax_Child),
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
conspiracies2 <- conspiracies2 %>% 
  mutate(
    social_distance11 = fct_collapse(as.factor(W2_SocialDistance11),
                                     "Agree" = c("5","4"),
                                     "Neither" = "3",
                                     "Disagree" = c("2","1")),
    social_distance12 = fct_collapse(as.factor(W2_SocialDistance12),
                                     "Agree" = c("5","4"),
                                     "Neither" = "3",
                                     "Disagree" = c("2","1")),
    W2_SocialDistance11 = fct_relevel(as.factor(W2_SocialDistance11),
                                      c("5","4","3","2","1")),
    W2_SocialDistance12 = fct_relevel(as.factor(W2_SocialDistance12),
                                      c("5","4","3","2","1"))
    )
```

``` r
count(conspiracies2,W2_SocialDistance11) %>%  mutate(`%` = n / sum(n))
```

    ##   W2_SocialDistance11   n          %
    ## 1                   5 714 0.51036455
    ## 2                   4 435 0.31093638
    ## 3                   3 184 0.13152252
    ## 4                   2  45 0.03216583
    ## 5                   1  21 0.01501072

``` r
dist_full <- multinom(W2_SocialDistance11 ~ conspiracy1_sc +
                      conspiracy2_sc + conspiracy3_sc + W2_Trust_Body6 +
                      W2_Gender_binary + W1_Education_binary +
                      W1_Income_2019 + age_sc + W2_INFO_5 + W2_INFO_9 +
                      SDO + RWA + W2_DAI_Total + crt +
                      W1_Conspiracy_Total,
                    data = conspiracies2)
```

    ## # weights:  85 (64 variable)
    ## initial  value 2251.603639 
    ## iter  10 value 1470.307297
    ## iter  20 value 1429.395599
    ## iter  30 value 1422.834813
    ## iter  40 value 1422.455865
    ## final  value 1422.452948 
    ## converged

| variable               | response          | odds\_ratio |  p\_value |        |
| :--------------------- | :---------------- | ----------: | --------: | :----- |
| (Intercept)            | Agree             |   1.2693594 | 0.5644521 |        |
| (Intercept)            | Neither           |   2.4225705 | 0.1229356 |        |
| (Intercept)            | Disagree          |   0.1478413 | 0.0605122 | .      |
| (Intercept)            | Strongly disagree |   0.0220737 | 0.0066874 | \*\*   |
| conspiracy1\_sc        | Agree             |   0.8679837 | 0.5285327 |        |
| conspiracy1\_sc        | Neither           |   1.1576796 | 0.6516428 |        |
| conspiracy1\_sc        | Disagree          |   0.2729181 | 0.0345715 | \*     |
| conspiracy1\_sc        | Strongly disagree |   0.6408338 | 0.5729202 |        |
| conspiracy2\_sc        | Agree             |   0.6718544 | 0.0846893 | .      |
| conspiracy2\_sc        | Neither           |   0.2494167 | 0.0000144 | \*\*\* |
| conspiracy2\_sc        | Disagree          |   0.7194809 | 0.5617657 |        |
| conspiracy2\_sc        | Strongly disagree |   0.1773878 | 0.0221452 | \*     |
| conspiracy3\_sc        | Agree             |   2.1286942 | 0.0415465 | \*     |
| conspiracy3\_sc        | Neither           |   7.3364588 | 0.0000057 | \*\*\* |
| conspiracy3\_sc        | Disagree          |   3.8941918 | 0.0697229 | .      |
| conspiracy3\_sc        | Strongly disagree |   1.5168959 | 0.6977096 |        |
| W2\_Trust\_Body6       | Agree             |   4.3226785 | 0.0000003 | \*\*\* |
| W2\_Trust\_Body6       | Neither           |   5.8237632 | 0.0000050 | \*\*\* |
| W2\_Trust\_Body6       | Disagree          |  17.1375915 | 0.0000219 | \*\*\* |
| W2\_Trust\_Body6       | Strongly disagree |  12.6064216 | 0.0046743 | \*\*   |
| W2\_Gender\_binary2    | Agree             |   0.6495004 | 0.0014021 | \*\*   |
| W2\_Gender\_binary2    | Neither           |   0.5354844 | 0.0011261 | \*\*   |
| W2\_Gender\_binary2    | Disagree          |   1.1086786 | 0.7601600 |        |
| W2\_Gender\_binary2    | Strongly disagree |   0.9454714 | 0.9108299 |        |
| W1\_Education\_binary1 | Agree             |   0.9491967 | 0.7049038 |        |
| W1\_Education\_binary1 | Neither           |   0.8119841 | 0.2782698 |        |
| W1\_Education\_binary1 | Disagree          |   0.9995677 | 0.9990046 |        |
| W1\_Education\_binary1 | Strongly disagree |   0.3001858 | 0.0174995 | \*     |
| W1\_Income\_2019       | Agree             |   0.9550646 | 0.8075193 |        |
| W1\_Income\_2019       | Neither           |   1.0235662 | 0.9317188 |        |
| W1\_Income\_2019       | Disagree          |   0.8018637 | 0.6512936 |        |
| W1\_Income\_2019       | Strongly disagree |   0.7156006 | 0.6353303 |        |
| age\_sc                | Agree             |   0.2796030 | 0.0002098 | \*\*\* |
| age\_sc                | Neither           |   0.0930298 | 0.0000016 | \*\*\* |
| age\_sc                | Disagree          |   0.0468823 | 0.0005290 | \*\*\* |
| age\_sc                | Strongly disagree |   0.1046295 | 0.0765760 | .      |
| W2\_INFO\_5            | Agree             |   1.0655939 | 0.7894557 |        |
| W2\_INFO\_5            | Neither           |   0.7849742 | 0.4871877 |        |
| W2\_INFO\_5            | Disagree          |   2.9799177 | 0.0554007 | .      |
| W2\_INFO\_5            | Strongly disagree |   0.5732116 | 0.5475848 |        |
| W2\_INFO\_9            | Agree             |   0.7082466 | 0.1721285 |        |
| W2\_INFO\_9            | Neither           |   0.3334057 | 0.0030336 | \*\*   |
| W2\_INFO\_9            | Disagree          |   0.6301423 | 0.4678443 |        |
| W2\_INFO\_9            | Strongly disagree |   0.5566807 | 0.5381443 |        |
| SDO                    | Agree             |   4.5375455 | 0.0002631 | \*\*\* |
| SDO                    | Neither           |   9.3622252 | 0.0002339 | \*\*\* |
| SDO                    | Disagree          |  14.3037879 | 0.0122124 | \*     |
| SDO                    | Strongly disagree |  59.1529912 | 0.0036626 | \*\*   |
| RWA                    | Agree             |   0.2126227 | 0.0003930 | \*\*\* |
| RWA                    | Neither           |   0.1290994 | 0.0015792 | \*\*   |
| RWA                    | Disagree          |   0.0573293 | 0.0120371 | \*     |
| RWA                    | Strongly disagree |   0.0549487 | 0.0539663 | .      |
| W2\_DAI\_Total         | Agree             |   2.5239151 | 0.0041973 | \*\*   |
| W2\_DAI\_Total         | Neither           |   4.4952971 | 0.0012005 | \*\*   |
| W2\_DAI\_Total         | Disagree          |   1.2267316 | 0.7980082 |        |
| W2\_DAI\_Total         | Strongly disagree |   2.1531704 | 0.4742109 |        |
| crt                    | Agree             |   1.6554205 | 0.0100173 | \*     |
| crt                    | Neither           |   1.0977376 | 0.7316194 |        |
| crt                    | Disagree          |   1.3419953 | 0.5621575 |        |
| crt                    | Strongly disagree |   0.6226965 | 0.4852975 |        |
| W1\_Conspiracy\_Total  | Agree             |   0.4788002 | 0.0333928 | \*     |
| W1\_Conspiracy\_Total  | Neither           |   0.2306282 | 0.0026926 | \*\*   |
| W1\_Conspiracy\_Total  | Disagree          |   0.8010804 | 0.7998339 |        |
| W1\_Conspiracy\_Total  | Strongly disagree |  49.7806833 | 0.0027353 | \*\*   |

Multinomial Regression Results for Social Distancing: ‘wanted to do it’

``` r
count(conspiracies2,W2_SocialDistance12) %>%  mutate(`%` = n / sum(n))
```

    ##   W2_SocialDistance12   n           %
    ## 1                   5 794 0.567548249
    ## 2                   4 419 0.299499643
    ## 3                   3 155 0.110793424
    ## 4                   2  22 0.015725518
    ## 5                   1   9 0.006433167

``` r
dist12_full <- multinom(W2_SocialDistance12 ~ conspiracy1_sc +
                      conspiracy2_sc + conspiracy3_sc + W2_Trust_Body6 +
                      W2_Gender_binary + W1_Education_binary +
                      W1_Income_2019 + age_sc + W2_INFO_5 + W2_INFO_9 +
                      SDO + RWA + W2_DAI_Total + crt +
                      W1_Conspiracy_Total,
                    data = conspiracies2)
```

    ## # weights:  85 (64 variable)
    ## initial  value 2251.603639 
    ## iter  10 value 1251.537432
    ## iter  20 value 1206.060319
    ## iter  30 value 1193.408062
    ## iter  40 value 1190.597745
    ## iter  50 value 1190.331084
    ## final  value 1190.322929 
    ## converged

| variable               | response          | odds\_ratio |  p\_value |        |
| :--------------------- | :---------------- | ----------: | --------: | :----- |
| (Intercept)            | Agree             |   1.0535408 | 0.8999046 |        |
| (Intercept)            | Neither           |   1.1452455 | 0.8343549 |        |
| (Intercept)            | Disagree          |   0.0510618 | 0.0606686 | .      |
| (Intercept)            | Strongly disagree |   0.0173530 | 0.0621908 | .      |
| conspiracy1\_sc        | Agree             |   1.1461746 | 0.5450124 |        |
| conspiracy1\_sc        | Neither           |   0.8931324 | 0.7576747 |        |
| conspiracy1\_sc        | Disagree          |   0.3257177 | 0.2229575 |        |
| conspiracy1\_sc        | Strongly disagree |   0.3275309 | 0.3868982 |        |
| conspiracy2\_sc        | Agree             |   0.6127705 | 0.0345201 | \*     |
| conspiracy2\_sc        | Neither           |   0.2406427 | 0.0000775 | \*\*\* |
| conspiracy2\_sc        | Disagree          |   0.1467752 | 0.0252267 | \*     |
| conspiracy2\_sc        | Strongly disagree |   0.0885881 | 0.0369663 | \*     |
| conspiracy3\_sc        | Agree             |   1.4945229 | 0.2818547 |        |
| conspiracy3\_sc        | Neither           |  12.1691848 | 0.0000001 | \*\*\* |
| conspiracy3\_sc        | Disagree          |  26.1258184 | 0.0005519 | \*\*\* |
| conspiracy3\_sc        | Strongly disagree |   0.8148151 | 0.9093331 |        |
| W2\_Trust\_Body6       | Agree             |   5.0252674 | 0.0000000 | \*\*\* |
| W2\_Trust\_Body6       | Neither           |   8.2642506 | 0.0000005 | \*\*\* |
| W2\_Trust\_Body6       | Disagree          |  95.5781554 | 0.0000055 | \*\*\* |
| W2\_Trust\_Body6       | Strongly disagree |  93.9410478 | 0.0020020 | \*\*   |
| W2\_Gender\_binary2    | Agree             |   0.7604778 | 0.0450042 | \*     |
| W2\_Gender\_binary2    | Neither           |   0.3616269 | 0.0000030 | \*\*\* |
| W2\_Gender\_binary2    | Disagree          |   0.5436870 | 0.2174042 |        |
| W2\_Gender\_binary2    | Strongly disagree |   0.8455128 | 0.8477824 |        |
| W1\_Education\_binary1 | Agree             |   0.9235067 | 0.5658749 |        |
| W1\_Education\_binary1 | Neither           |   0.8358494 | 0.3992884 |        |
| W1\_Education\_binary1 | Disagree          |   2.2367711 | 0.1564754 |        |
| W1\_Education\_binary1 | Strongly disagree |   0.3242037 | 0.1740768 |        |
| W1\_Income\_2019       | Agree             |   1.0179751 | 0.9258001 |        |
| W1\_Income\_2019       | Neither           |   0.9173195 | 0.7777670 |        |
| W1\_Income\_2019       | Disagree          |   0.6495047 | 0.5704562 |        |
| W1\_Income\_2019       | Strongly disagree |   0.0614235 | 0.0718734 | .      |
| age\_sc                | Agree             |   0.2101497 | 0.0000070 | \*\*\* |
| age\_sc                | Neither           |   0.0696466 | 0.0000014 | \*\*\* |
| age\_sc                | Disagree          |   0.0134929 | 0.0029298 | \*\*   |
| age\_sc                | Strongly disagree |   0.0007207 | 0.0050895 | \*\*   |
| W2\_INFO\_5            | Agree             |   1.1993543 | 0.4484501 |        |
| W2\_INFO\_5            | Neither           |   1.2314802 | 0.5891209 |        |
| W2\_INFO\_5            | Disagree          |   4.5712162 | 0.0850685 | .      |
| W2\_INFO\_5            | Strongly disagree |   0.5722401 | 0.7206243 |        |
| W2\_INFO\_9            | Agree             |   0.7235577 | 0.2068969 |        |
| W2\_INFO\_9            | Neither           |   0.1800483 | 0.0000541 | \*\*\* |
| W2\_INFO\_9            | Disagree          |   0.3698449 | 0.3108044 |        |
| W2\_INFO\_9            | Strongly disagree |   0.0906646 | 0.1672067 |        |
| SDO                    | Agree             |  11.6133483 | 0.0000000 | \*\*\* |
| SDO                    | Neither           |  40.2090057 | 0.0000001 | \*\*\* |
| SDO                    | Disagree          | 120.0479818 | 0.0041771 | \*\*   |
| SDO                    | Strongly disagree | 636.0214285 | 0.0052811 | \*\*   |
| RWA                    | Agree             |   0.1167295 | 0.0000017 | \*\*\* |
| RWA                    | Neither           |   0.0919171 | 0.0013028 | \*\*   |
| RWA                    | Disagree          |   0.1528343 | 0.2711577 |        |
| RWA                    | Strongly disagree |   0.0564741 | 0.2185709 |        |
| W2\_DAI\_Total         | Agree             |   2.3953469 | 0.0071578 | \*\*   |
| W2\_DAI\_Total         | Neither           |   4.1083100 | 0.0064262 | \*\*   |
| W2\_DAI\_Total         | Disagree          |   0.3758357 | 0.4145978 |        |
| W2\_DAI\_Total         | Strongly disagree |   0.4971818 | 0.6912845 |        |
| crt                    | Agree             |   1.5072407 | 0.0359585 | \*     |
| crt                    | Neither           |   2.1080697 | 0.0190389 | \*     |
| crt                    | Disagree          |   0.7350924 | 0.6817695 |        |
| crt                    | Strongly disagree |   0.5257159 | 0.5802479 |        |
| W1\_Conspiracy\_Total  | Agree             |   0.4813288 | 0.0369808 | \*     |
| W1\_Conspiracy\_Total  | Neither           |   0.1701580 | 0.0010857 | \*\*   |
| W1\_Conspiracy\_Total  | Disagree          |   0.3321477 | 0.3667594 |        |
| W1\_Conspiracy\_Total  | Strongly disagree | 317.0135136 | 0.0143124 | \*     |

Multinomial Regression Results for Social Distancing: ‘good thing to do’

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
vax_full <- multinom(W2_C19_Vax_Self ~ conspiracy1_sc +
                      conspiracy2_sc + conspiracy3_sc + W2_Trust_Body6 +
                      W2_Gender_binary + W1_Education_binary +
                      W1_Income_2019 + age_sc + W2_INFO_5 + W2_INFO_9 +
                      SDO + RWA + W2_DAI_Total + crt +
                      W1_Conspiracy_Total,
                    data = conspiracies2)
```

    ## # weights:  51 (32 variable)
    ## initial  value 1527.071081 
    ## iter  10 value 1019.039960
    ## iter  20 value 1006.403596
    ## iter  30 value 1006.097940
    ## final  value 1006.095974 
    ## converged

| variable               | response | odds\_ratio |  p\_value |        |
| :--------------------- | :------- | ----------: | --------: | :----- |
| (Intercept)            | No       |   0.1195335 | 0.0036458 | \*\*   |
| (Intercept)            | Maybe    |   0.5330255 | 0.1389440 |        |
| conspiracy1\_sc        | No       |   2.2132923 | 0.0410937 | \*     |
| conspiracy1\_sc        | Maybe    |   0.9542526 | 0.8399116 |        |
| conspiracy2\_sc        | No       |   0.2850847 | 0.0009875 | \*\*\* |
| conspiracy2\_sc        | Maybe    |   0.5224210 | 0.0050461 | \*\*   |
| conspiracy3\_sc        | No       |   8.1480569 | 0.0000015 | \*\*\* |
| conspiracy3\_sc        | Maybe    |   0.9239459 | 0.8336542 |        |
| W2\_Trust\_Body6       | No       |  14.1973166 | 0.0000000 | \*\*\* |
| W2\_Trust\_Body6       | Maybe    |   2.7791102 | 0.0003734 | \*\*\* |
| W2\_Gender\_binary2    | No       |   1.2462675 | 0.3207793 |        |
| W2\_Gender\_binary2    | Maybe    |   1.3590864 | 0.0278282 | \*     |
| W1\_Education\_binary1 | No       |   1.1425252 | 0.5562986 |        |
| W1\_Education\_binary1 | Maybe    |   1.0988924 | 0.5075933 |        |
| W1\_Income\_2019       | No       |   0.7286568 | 0.3327100 |        |
| W1\_Income\_2019       | Maybe    |   0.4393181 | 0.0000279 | \*\*\* |
| age\_sc                | No       |   0.0398408 | 0.0000001 | \*\*\* |
| age\_sc                | Maybe    |   0.2315590 | 0.0000372 | \*\*\* |
| W2\_INFO\_5            | No       |   1.2212044 | 0.6117151 |        |
| W2\_INFO\_5            | Maybe    |   1.1719772 | 0.5160622 |        |
| W2\_INFO\_9            | No       |   0.5968970 | 0.2273772 |        |
| W2\_INFO\_9            | Maybe    |   0.8319015 | 0.4819490 |        |
| SDO                    | No       |   2.1073303 | 0.3024157 |        |
| SDO                    | Maybe    |   1.5230526 | 0.3273076 |        |
| RWA                    | No       |   1.6582732 | 0.5007991 |        |
| RWA                    | Maybe    |   1.3905525 | 0.4631349 |        |
| W2\_DAI\_Total         | No       |   0.4061793 | 0.0973401 | .      |
| W2\_DAI\_Total         | Maybe    |   0.8764441 | 0.6944873 |        |
| crt                    | No       |   1.2859932 | 0.4803296 |        |
| crt                    | Maybe    |   0.7947599 | 0.2526006 |        |
| W1\_Conspiracy\_Total  | No       |   1.0471541 | 0.9373028 |        |
| W1\_Conspiracy\_Total  | Maybe    |   2.0604475 | 0.0463090 | \*     |

Multinomial Regression Results for Vaccine Acceptance
