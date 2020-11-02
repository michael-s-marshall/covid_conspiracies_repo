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
# combining 2019 election variables into one, for parties and didn't votes

conspiracies <- conspiracies %>% 
  mutate(
    W1_Voted_Party_Name = to_factor(
      conspiracies$W1_Voted_Party,
      nolabel_to_na = TRUE),
    W1_Voted_GenElection_Name = to_factor(
      conspiracies$W1_Voted_GenElection,
      nolabel_to_na = TRUE),
    W1_2019_GE_Full = factor(ifelse(
      W1_Voted_GenElection_Name == "Voted.",
      as.character(W1_Voted_Party_Name),
      as.character(W1_Voted_GenElection_Name)))
    )

# making preferred newspaper dummy variable (i.e. replacing NA with 0)
na_to_zero <- function(x){
  x[is.na(x)] <- 0
  x <- as.numeric(x)
  return(x)
} 

paper_vars <- conspiracies %>%
  dplyr::select(W2_Newspaper_prefer1:W2_Newspaper_prefer11) %>% 
  names()

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
factors <- c("W1_Ethnicity","W1_C19_Infected",
             "W1_CRT1","W1_CRT2","W1_CRT3","W1_CRT4",
             "W1_CRT5","W1_CRT_test","W1_BornUK","W1_EURef",
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

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-2.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-3.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-4.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-5.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-6.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-7.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-8.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-9.png)<!-- -->

    ## Warning: Removed 4 rows containing non-finite values (stat_density).

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-10.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-11.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-12.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-13.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-14.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-15.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-16.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-17.png)<!-- -->![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-22-18.png)<!-- -->

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

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
biplot(pca_fit, choices = 3:4,
       col = c("lightgrey","red"))
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->

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

ihs <- function(x){
  log(x + ((x^2 + 1)^0.5))
}

pc <- pc %>% 
  mutate(
    pc1_ihs = ihs(PC1)
  )

pc %>% 
  ggplot(aes(x = pc1_ihs)) +
  geom_density() +
  labs(x = "Principal Component 1 (IHS)")
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
pc %>% 
  ggplot(aes(x = PC2)) +
  geom_density() +
  labs(x = "Principal Component 2")
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-25-2.png)<!-- -->

``` r
pc %>% 
  ggplot(aes(x = PC3)) +
  geom_density() +
  labs(x = "Principal Component 3")
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-25-3.png)<!-- -->

``` r
pc %>% 
  ggplot(aes(x = PC4)) +
  geom_density()  +
  labs(x = "Principal Component 4")
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-25-4.png)<!-- -->

Principal components 1 to 3 explain most of the variation in the data.

``` r
# modelling principal components against general conspiracy ideation
conspiracy_mod <- lm(W1_Conspiracy_Total ~ pc1_ihs + PC2 + PC3 + PC4,
                     data = pc)

par(mfrow = c(2,2))
plot(conspiracy_mod)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

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

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-26-2.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-26-3.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-26-4.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-26-5.png)<!-- -->

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

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-31-2.png)<!-- -->

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

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_pol_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->

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
                        W2_Chance_Total,
                      data = conspiracies)

summ(se_polpsych_lab, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(21,1377) = 20.49, p = 0.00
    ## R² = 0.24
    ## Adj. R² = 0.23 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -6.73   5.93    -1.14   0.26       
    ## W2_Gender_binary2              2.14   1.65     1.29   0.20   1.12
    ## W1_Education_binary1          -3.69   1.70    -2.18   0.03   1.13
    ## W1_Income_2019                -5.22   2.39    -2.19   0.03   1.20
    ## age_sc                         5.10   4.43     1.15   0.25   1.47
    ## right                          2.69   5.52     0.49   0.63   2.08
    ## soc_con                        2.84   3.69     0.77   0.44   1.68
    ## fis_con                       -2.96   5.19    -0.57   0.57   2.12
    ## nat                           10.66   3.66     2.91   0.00   1.36
    ## W2_Trust_Body6                18.59   3.38     5.50   0.00   1.17
    ## W2_Newspaper_prefer1           9.62   1.80     5.33   0.00   1.13
    ## W2_Newspaper_prefer5          -2.55   2.13    -1.19   0.23   1.23
    ## W2_Newspaper_prefer9           9.26   2.51     3.69   0.00   1.12
    ## W2_INFO_5                      8.08   2.90     2.79   0.01   1.40
    ## W2_INFO_9                     13.04   3.07     4.25   0.00   1.24
    ## SDO                            9.66   5.39     1.79   0.07   1.53
    ## RWA                           20.18   5.71     3.53   0.00   1.54
    ## W2_DAI_Total                  17.25   4.39     3.93   0.00   1.54
    ## W2_IOU_Total                 -11.68   4.97    -2.35   0.02   1.59
    ## W2_Paranoia_Total             14.11   4.23     3.34   0.00   1.73
    ## W2_Internal_Total              3.50   4.96     0.71   0.48   1.20
    ## W2_Chance_Total                2.10   4.48     0.47   0.64   1.36
    ## -----------------------------------------------------------------

``` r
plot_coefs(se_polpsych_lab)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_polpsych_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-33-2.png)<!-- -->

Looking at mulitcollinearity below for the following variables:  
\* right  
\* soc\_con  
\* fis\_con  
\* paranoia

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

    ## # A tibble: 22 x 2
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
    ## # ... with 12 more rows

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

    ## # A tibble: 22 x 2
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
    ## # ... with 12 more rows

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

    ## # A tibble: 22 x 2
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
    ## # ... with 12 more rows

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

    ## # A tibble: 22 x 2
    ##    cor_vec4 `names(cor_mat)`    
    ##       <dbl> <chr>               
    ##  1    1     W2_Paranoia_Total   
    ##  2    0.463 W2_IOU_Total        
    ##  3    0.441 W2_DAI_Total        
    ##  4    0.418 W2_Chance_Total     
    ##  5    0.247 W2_INFO_5           
    ##  6    0.237 W2_Trust_Body6      
    ##  7    0.158 SDO                 
    ##  8    0.137 W2_INFO_9           
    ##  9    0.131 W2_Newspaper_prefer9
    ## 10    0.114 soc_con             
    ## # ... with 12 more rows

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

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_polpsych_lab_2)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-38-2.png)<!-- -->

## DV Chinese lab conspiracy - socio-economic variables + political/media + pol-psych + covid-threat

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
                  threat,
                data = conspiracies)

summ(multi_lab, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(18,1380) = 24.41, p = 0.00
    ## R² = 0.24
    ## Adj. R² = 0.23 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -6.57   4.96    -1.33   0.19       
    ## W2_Gender_binary2              1.85   1.63     1.14   0.26   1.10
    ## W1_Education_binary1          -3.85   1.69    -2.28   0.02   1.12
    ## W1_Income_2019                -5.40   2.34    -2.31   0.02   1.16
    ## age_sc                         3.37   4.44     0.76   0.45   1.49
    ## fis_con                        0.06   4.27     0.01   0.99   1.45
    ## nat                           11.02   3.60     3.06   0.00   1.33
    ## W2_Trust_Body6                19.25   3.30     5.83   0.00   1.13
    ## W2_Newspaper_prefer1           9.68   1.79     5.40   0.00   1.12
    ## W2_Newspaper_prefer5          -3.02   2.13    -1.42   0.16   1.23
    ## W2_Newspaper_prefer9           9.09   2.49     3.65   0.00   1.11
    ## W2_INFO_5                      7.96   2.88     2.76   0.01   1.40
    ## W2_INFO_9                     12.35   3.06     4.03   0.00   1.24
    ## SDO                           12.04   5.29     2.28   0.02   1.48
    ## RWA                           20.11   5.46     3.69   0.00   1.41
    ## W2_DAI_Total                  15.50   4.40     3.52   0.00   1.56
    ## W2_IOU_Total                 -12.95   4.89    -2.65   0.01   1.54
    ## W2_Paranoia_Total             13.45   4.10     3.28   0.00   1.64
    ## threat                         9.26   3.31     2.80   0.01   1.20
    ## -----------------------------------------------------------------

``` r
plot_coefs(multi_lab)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(multi_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-39-2.png)<!-- -->

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
                  
                  #interactions
                  W2_INFO_5:RWA +
                  W2_INFO_9:RWA,
                data = conspiracies)

summ(int_lab, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(17,1381) = 25.35, p = 0.00
    ## R² = 0.24
    ## Adj. R² = 0.23 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                    1.72   4.70     0.37   0.72       
    ## W2_Gender_binary2              2.29   1.62     1.41   0.16   1.08
    ## W1_Education_binary1          -3.91   1.69    -2.31   0.02   1.12
    ## W1_Income_2019                -5.64   2.34    -2.41   0.02   1.16
    ## age_sc                         3.63   4.37     0.83   0.41   1.44
    ## fis_con                        1.16   4.21     0.28   0.78   1.41
    ## nat                           11.58   3.59     3.22   0.00   1.32
    ## W2_Trust_Body6                19.33   3.31     5.84   0.00   1.13
    ## W2_Newspaper_prefer1          10.03   1.79     5.61   0.00   1.11
    ## W2_Newspaper_prefer5          -3.38   2.09    -1.62   0.11   1.18
    ## W2_Newspaper_prefer9           8.90   2.50     3.56   0.00   1.12
    ## SDO                           14.08   5.18     2.72   0.01   1.41
    ## W2_DAI_Total                  15.74   4.41     3.57   0.00   1.56
    ## W2_IOU_Total                 -12.57   4.89    -2.57   0.01   1.54
    ## W2_Paranoia_Total             13.43   4.11     3.27   0.00   1.64
    ## threat                         9.60   3.31     2.90   0.00   1.19
    ## W2_INFO_5:RWA                 13.11   5.16     2.54   0.01   1.42
    ## RWA:W2_INFO_9                 24.79   5.42     4.57   0.00   1.32
    ## -----------------------------------------------------------------

``` r
plot_coefs(int_lab)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(int_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-40-2.png)<!-- -->

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
                  
                  #covid-anxety
                  threat +
                  
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
    ## F(21,1377) = 27.55, p = 0.00
    ## R² = 0.30
    ## Adj. R² = 0.29 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                  -10.83   5.25    -2.06   0.04       
    ## W2_Gender_binary2              1.64   1.57     1.04   0.30   1.10
    ## W1_Education_binary1          -3.80   1.63    -2.33   0.02   1.13
    ## W1_Income_2019                -3.61   2.27    -1.59   0.11   1.17
    ## age_sc                         4.42   4.29     1.03   0.30   1.49
    ## fis_con                        1.43   4.12     0.35   0.73   1.45
    ## nat                           11.52   3.50     3.29   0.00   1.35
    ## W2_Trust_Body6                10.07   3.32     3.04   0.00   1.22
    ## W2_Newspaper_prefer1           9.60   1.73     5.56   0.00   1.12
    ## W2_Newspaper_prefer5          -2.38   2.05    -1.16   0.25   1.23
    ## W2_Newspaper_prefer9           8.18   2.41     3.39   0.00   1.12
    ## W2_INFO_5                      4.98   2.80     1.78   0.08   1.42
    ## W2_INFO_9                     10.00   2.97     3.36   0.00   1.26
    ## SDO                           11.41   5.19     2.20   0.03   1.53
    ## RWA                           17.72   5.31     3.34   0.00   1.44
    ## W2_DAI_Total                  12.28   4.33     2.84   0.00   1.62
    ## W2_IOU_Total                 -11.62   4.79    -2.43   0.02   1.59
    ## W2_Paranoia_Total              8.26   4.00     2.06   0.04   1.68
    ## threat                        10.29   3.21     3.20   0.00   1.21
    ## W1_Conspiracy_Total           26.57   3.98     6.67   0.00   1.10
    ## conspiracy2_sc               -12.29   2.71    -4.53   0.00   1.07
    ## conspiracy3_sc                23.72   4.03     5.88   0.00   1.38
    ## -----------------------------------------------------------------

``` r
plot_coefs(full_lab)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-41-2.png)<!-- -->

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
                  W2_Trust_Body6 + #distrust of scientists
                  W2_Newspaper_prefer1 + #daily_mail
                  W2_Newspaper_prefer5 + #guardian
                  W2_Newspaper_prefer9 + #sun
                  #W2_INFO_5 + #social media
                  #W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  #RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  
                  #covid-anxety
                  threat +
                  
                  #conspiracies
                  W1_Conspiracy_Total +
                  conspiracy2_sc +
                  conspiracy3_sc +
                  
                  #interaction
                  W2_INFO_5:RWA +
                  W2_INFO_9:RWA,
                data = conspiracies)

summ(full_int_lab, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(20,1378) = 28.55, p = 0.00
    ## R² = 0.29
    ## Adj. R² = 0.28 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                   -4.48   5.06    -0.89   0.38       
    ## W2_Gender_binary2              2.05   1.56     1.31   0.19   1.08
    ## W1_Education_binary1          -3.88   1.63    -2.38   0.02   1.12
    ## W1_Income_2019                -3.85   2.27    -1.70   0.09   1.17
    ## age_sc                         4.89   4.22     1.16   0.25   1.44
    ## fis_con                        2.63   4.07     0.65   0.52   1.41
    ## nat                           12.10   3.49     3.47   0.00   1.34
    ## W2_Trust_Body6                10.14   3.32     3.06   0.00   1.22
    ## W2_Newspaper_prefer1           9.93   1.72     5.76   0.00   1.11
    ## W2_Newspaper_prefer5          -2.79   2.02    -1.38   0.17   1.19
    ## W2_Newspaper_prefer9           8.05   2.42     3.33   0.00   1.12
    ## SDO                           13.64   5.06     2.70   0.01   1.45
    ## W2_DAI_Total                  12.63   4.33     2.91   0.00   1.62
    ## W2_IOU_Total                 -11.58   4.78    -2.42   0.02   1.58
    ## W2_Paranoia_Total              8.30   4.01     2.07   0.04   1.68
    ## threat                        10.56   3.22     3.28   0.00   1.21
    ## W1_Conspiracy_Total           27.52   3.97     6.94   0.00   1.09
    ## conspiracy2_sc               -12.31   2.72    -4.53   0.00   1.07
    ## conspiracy3_sc                23.05   4.00     5.76   0.00   1.35
    ## W2_INFO_5:RWA                  8.30   5.00     1.66   0.10   1.43
    ## RWA:W2_INFO_9                 20.47   5.25     3.90   0.00   1.33
    ## -----------------------------------------------------------------

``` r
plot_coefs(full_int_lab)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_int_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-42-2.png)<!-- -->

``` r
# comparing the AIC with and without the interaction term
AIC(full_lab) 
```

    ## [1] 13323.42

``` r
AIC(full_int_lab)
```

    ## [1] 13327.12

``` r
# full_lab performs best
```

# Modelling for belief in 5G origin conspiracy

## Plots of DV, incl. inverse hyperbolic sine transformation

``` r
conspiracies %>% 
  ggplot(aes(x = W2_Conspiracy_Theory3)) +
  geom_histogram(colour = "darkgrey", fill = "lightblue")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

``` r
conspiracies <- conspiracies %>% 
  mutate(w2_conspiracy3_ihs = ihs(W2_Conspiracy_Theory3))

conspiracies %>% 
  ggplot(aes(x = w2_conspiracy3_ihs)) +
  geom_histogram(colour = "darkgrey", fill = "lightblue")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-44-2.png)<!-- -->

## DV 5G conspiracy - singular IV models

``` r
sdo_5g <- lm(w2_conspiracy3_ihs ~ SDO,
              data = conspiracies)

summ(sdo_5g)
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
rwa_5g <- lm(w2_conspiracy3_ihs ~ RWA,
              data = conspiracies)

summ(rwa_5g)
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

``` r
daily_m_5g <- lm(w2_conspiracy3_ihs ~ 
                   W2_Newspaper_prefer1,
              data = conspiracies)

summ(daily_m_5g)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 20.19, p = 0.00
    ## R² = 0.01
    ## Adj. R² = 0.01 
    ## 
    ## Standard errors: OLS
    ## --------------------------------------------------------
    ##                              Est.   S.E.   t val.      p
    ## -------------------------- ------ ------ -------- ------
    ## (Intercept)                  1.36   0.05    24.69   0.00
    ## W2_Newspaper_prefer1         0.45   0.10     4.49   0.00
    ## --------------------------------------------------------

``` r
social_m_5g <- lm(w2_conspiracy3_ihs ~ W2_INFO_5,
              data = conspiracies)

summ(social_m_5g)
```

    ## MODEL INFO:
    ## Observations: 1406
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(1,1404) = 98.00, p = 0.00
    ## R² = 0.07
    ## Adj. R² = 0.06 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------
    ##                     Est.   S.E.   t val.      p
    ## ----------------- ------ ------ -------- ------
    ## (Intercept)         1.07   0.06    17.48   0.00
    ## W2_INFO_5           1.38   0.14     9.90   0.00
    ## -----------------------------------------------

## DV 5G conspiracy - socio-economic variables

``` r
se_5g <- lm(w2_conspiracy3_ihs ~ W2_Gender_binary +
               W1_Education_binary +
               W1_Income_2019 +
               age_sc,
              data = conspiracies)

summ(se_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1403 (3 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(4,1398) = 18.69, p = 0.00
    ## R² = 0.05
    ## Adj. R² = 0.05 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                   2.46   0.15    16.58   0.00       
    ## W2_Gender_binary2             0.11   0.09     1.20   0.23   1.03
    ## W1_Education_binary1         -0.18   0.10    -1.94   0.05   1.06
    ## W1_Income_2019               -0.62   0.13    -4.80   0.00   1.06
    ## age_sc                       -1.31   0.21    -6.10   0.00   1.03
    ## ----------------------------------------------------------------

``` r
plot_coefs(se_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-49-2.png)<!-- -->

## DV 5G conspiracy - socio-economic variables + political/media

As with modelling above, fis\_con is included instead of soc\_con and
right to avoid multicollinearity problem.

``` r
se_pol_5g <- lm(w2_conspiracy3_ihs ~ 
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
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(12,1386) = 33.90, p = 0.00
    ## R² = 0.23
    ## Adj. R² = 0.22 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                   0.20   0.21     0.96   0.34       
    ## W2_Gender_binary2             0.09   0.08     1.06   0.29   1.05
    ## W1_Education_binary1         -0.04   0.09    -0.43   0.67   1.12
    ## W1_Income_2019               -0.43   0.12    -3.61   0.00   1.11
    ## age_sc                       -0.56   0.21    -2.61   0.01   1.27
    ## fis_con                       0.24   0.21     1.16   0.25   1.29
    ## nat                           0.69   0.18     3.74   0.00   1.26
    ## W2_Trust_Body6                2.14   0.17    12.84   0.00   1.04
    ## W2_Newspaper_prefer1          0.14   0.09     1.45   0.15   1.10
    ## W2_Newspaper_prefer5         -0.30   0.11    -2.78   0.01   1.14
    ## W2_Newspaper_prefer9          0.37   0.13     2.85   0.00   1.11
    ## W2_INFO_5                     0.92   0.15     6.17   0.00   1.36
    ## W2_INFO_9                     0.52   0.16     3.31   0.00   1.21
    ## ----------------------------------------------------------------

``` r
plot_coefs(se_pol_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_pol_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-50-2.png)<!-- -->

## DV 5G conspiracy - socio-economic variables + political/media + pol-psych

``` r
se_polpsych_5g <- lm(w2_conspiracy3_ihs ~
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
                        W2_Chance_Total,
                     data = conspiracies)

summ(se_polpsych_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(19,1379) = 32.04, p = 0.00
    ## R² = 0.31
    ## Adj. R² = 0.30 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  -0.33   0.29    -1.12   0.26       
    ## W2_Gender_binary2             0.14   0.08     1.70   0.09   1.10
    ## W1_Education_binary1          0.01   0.08     0.06   0.95   1.12
    ## W1_Income_2019               -0.31   0.12    -2.62   0.01   1.19
    ## age_sc                        0.10   0.22     0.43   0.66   1.47
    ## fis_con                      -0.07   0.21    -0.32   0.75   1.46
    ## nat                           0.34   0.18     1.88   0.06   1.34
    ## W2_Trust_Body6                1.61   0.17     9.69   0.00   1.14
    ## W2_Newspaper_prefer1          0.08   0.09     0.86   0.39   1.12
    ## W2_Newspaper_prefer5         -0.12   0.11    -1.15   0.25   1.22
    ## W2_Newspaper_prefer9          0.28   0.12     2.25   0.02   1.12
    ## W2_INFO_5                     0.73   0.14     5.11   0.00   1.40
    ## W2_INFO_9                     0.57   0.15     3.74   0.00   1.24
    ## SDO                           1.51   0.26     5.78   0.00   1.46
    ## RWA                          -0.03   0.27    -0.10   0.92   1.41
    ## W2_DAI_Total                  1.67   0.22     7.70   0.00   1.53
    ## W2_IOU_Total                 -0.97   0.25    -3.95   0.00   1.59
    ## W2_Paranoia_Total             0.87   0.21     4.13   0.00   1.73
    ## W2_Internal_Total            -0.43   0.25    -1.75   0.08   1.20
    ## W2_Chance_Total               0.04   0.22     0.18   0.86   1.36
    ## ----------------------------------------------------------------

``` r
plot_coefs(se_polpsych_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(se_polpsych_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-51-2.png)<!-- -->

## DV 5G conspiracy - socio-economic variables + political/media + pol-psych + covid-threat

``` r
multi_5g <- lm(w2_conspiracy3_ihs ~ 
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
                  
                  #covid-anxety
                  threat,
                 data = conspiracies)

summ(multi_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(20,1378) = 30.45, p = 0.00
    ## R² = 0.31
    ## Adj. R² = 0.30 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  -0.29   0.30    -0.99   0.32       
    ## W2_Gender_binary2             0.14   0.08     1.71   0.09   1.10
    ## W1_Education_binary1          0.01   0.08     0.08   0.93   1.13
    ## W1_Income_2019               -0.30   0.12    -2.58   0.01   1.20
    ## age_sc                        0.12   0.22     0.55   0.58   1.52
    ## fis_con                      -0.07   0.21    -0.34   0.74   1.46
    ## nat                           0.34   0.18     1.89   0.06   1.34
    ## W2_Trust_Body6                1.60   0.17     9.62   0.00   1.15
    ## W2_Newspaper_prefer1          0.08   0.09     0.86   0.39   1.12
    ## W2_Newspaper_prefer5         -0.12   0.11    -1.11   0.27   1.23
    ## W2_Newspaper_prefer9          0.28   0.12     2.28   0.02   1.12
    ## W2_INFO_5                     0.74   0.14     5.13   0.00   1.40
    ## W2_INFO_9                     0.58   0.15     3.79   0.00   1.26
    ## SDO                           1.49   0.26     5.65   0.00   1.48
    ## RWA                          -0.01   0.27    -0.03   0.97   1.42
    ## W2_DAI_Total                  1.70   0.22     7.69   0.00   1.58
    ## W2_IOU_Total                 -0.96   0.25    -3.85   0.00   1.60
    ## W2_Paranoia_Total             0.87   0.21     4.16   0.00   1.74
    ## W2_Internal_Total            -0.44   0.25    -1.80   0.07   1.21
    ## W2_Chance_Total               0.04   0.22     0.17   0.87   1.36
    ## threat                       -0.11   0.17    -0.69   0.49   1.20
    ## ----------------------------------------------------------------

``` r
plot_coefs(multi_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(multi_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-52-2.png)<!-- -->

As with the Chinese lab belief, the locus of control variables
(W2\_Internal\_Total and W2\_Chance\_Total) appear unrelated to the DV,
and are omitted in subsequent models.

## DV 5G conspiracy - multivariate and interaction model

``` r
int_5g <- lm(w2_conspiracy3_ihs ~
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
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  
                  #covid-anxety
                  threat +
                  
                  #interactions
                  W2_INFO_5:SDO +
                  W2_INFO_9:SDO,
             data = conspiracies)

summ(int_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(17,1381) = 36.39, p = 0.00
    ## R² = 0.31
    ## Adj. R² = 0.30 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  -0.07   0.24    -0.28   0.78       
    ## W2_Gender_binary2             0.13   0.08     1.63   0.10   1.08
    ## W1_Education_binary1          0.01   0.08     0.12   0.91   1.12
    ## W1_Income_2019               -0.33   0.12    -2.87   0.00   1.16
    ## age_sc                        0.05   0.22     0.21   0.83   1.47
    ## fis_con                      -0.08   0.21    -0.37   0.71   1.38
    ## nat                           0.31   0.18     1.74   0.08   1.32
    ## W2_Trust_Body6                1.65   0.16    10.10   0.00   1.11
    ## W2_Newspaper_prefer1          0.09   0.09     1.00   0.32   1.12
    ## W2_Newspaper_prefer5         -0.15   0.10    -1.42   0.16   1.19
    ## W2_Newspaper_prefer9          0.27   0.12     2.16   0.03   1.11
    ## RWA                           0.14   0.27     0.52   0.60   1.36
    ## W2_DAI_Total                  1.65   0.22     7.53   0.00   1.56
    ## W2_IOU_Total                 -0.94   0.24    -3.90   0.00   1.53
    ## W2_Paranoia_Total             0.91   0.20     4.47   0.00   1.64
    ## threat                       -0.07   0.16    -0.41   0.68   1.16
    ## W2_INFO_5:SDO                 2.01   0.36     5.67   0.00   1.69
    ## SDO:W2_INFO_9                 1.59   0.37     4.32   0.00   1.56
    ## ----------------------------------------------------------------

``` r
plot_coefs(int_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(int_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-53-2.png)<!-- -->

``` r
AIC(multi_5g)
```

    ## [1] 5030.628

``` r
AIC(int_5g) # reasonable support for interaction terms at this point
```

    ## [1] 5018.716

## DV 5G conspiracy - socio-economic variables + political/media + pol-psych + covid-threat and CRT + conspiracies

``` r
full_5g <- lm(w2_conspiracy3_ihs ~
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
                  
                #conspiracies
                W1_Conspiracy_Total +
                conspiracy1_sc +
                conspiracy2_sc,
              data = conspiracies)

summ(full_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(21,1377) = 33.09, p = 0.00
    ## R² = 0.34
    ## Adj. R² = 0.33 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  -0.53   0.27    -2.02   0.04       
    ## W2_Gender_binary2             0.12   0.08     1.48   0.14   1.10
    ## W1_Education_binary1          0.05   0.08     0.59   0.56   1.13
    ## W1_Income_2019               -0.28   0.11    -2.42   0.02   1.17
    ## age_sc                        0.04   0.22     0.17   0.87   1.49
    ## fis_con                      -0.10   0.21    -0.46   0.65   1.45
    ## nat                           0.21   0.18     1.18   0.24   1.36
    ## W2_Trust_Body6                1.41   0.17     8.52   0.00   1.18
    ## W2_Newspaper_prefer1         -0.02   0.09    -0.19   0.85   1.15
    ## W2_Newspaper_prefer5         -0.08   0.10    -0.76   0.44   1.23
    ## W2_Newspaper_prefer9          0.18   0.12     1.46   0.15   1.13
    ## W2_INFO_5                     0.64   0.14     4.53   0.00   1.41
    ## W2_INFO_9                     0.43   0.15     2.89   0.00   1.26
    ## SDO                           1.43   0.26     5.49   0.00   1.51
    ## RWA                          -0.25   0.27    -0.94   0.35   1.44
    ## W2_DAI_Total                  1.55   0.22     7.17   0.00   1.58
    ## W2_IOU_Total                 -0.81   0.24    -3.37   0.00   1.58
    ## W2_Paranoia_Total             0.77   0.20     3.82   0.00   1.66
    ## threat                       -0.18   0.16    -1.10   0.27   1.22
    ## W1_Conspiracy_Total           0.22   0.20     1.10   0.27   1.14
    ## conspiracy1_sc                1.00   0.13     7.40   0.00   1.39
    ## conspiracy2_sc               -0.08   0.14    -0.59   0.56   1.09
    ## ----------------------------------------------------------------

``` r
plot_coefs(full_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-55-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-55-2.png)<!-- -->

``` r
full_int_5g <- lm(w2_conspiracy3_ihs ~
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
                #W2_INFO_9 + #family and friends
                  
                #political-psychology variables
                #SDO +
                RWA +
                W2_DAI_Total +
                W2_IOU_Total +
                W2_Paranoia_Total +
                
                #covid-anxety
                threat +
                  
                #conspiracies
                W1_Conspiracy_Total +
                conspiracy1_sc +
                conspiracy2_sc +
                
                # interactions
                W2_INFO_5:SDO +
                W2_INFO_9:SDO,
              data = conspiracies)

summ(full_int_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1399 (7 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(20,1378) = 35.34, p = 0.00
    ## R² = 0.34
    ## Adj. R² = 0.33 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  -0.11   0.26    -0.44   0.66       
    ## W2_Gender_binary2             0.11   0.08     1.36   0.18   1.08
    ## W1_Education_binary1          0.05   0.08     0.57   0.57   1.13
    ## W1_Income_2019               -0.27   0.11    -2.35   0.02   1.17
    ## age_sc                        0.03   0.21     0.15   0.88   1.47
    ## fis_con                      -0.05   0.20    -0.24   0.81   1.39
    ## nat                           0.21   0.18     1.22   0.22   1.35
    ## W2_Trust_Body6                1.43   0.16     8.74   0.00   1.16
    ## W2_Newspaper_prefer1         -0.01   0.09    -0.09   0.93   1.14
    ## W2_Newspaper_prefer5         -0.12   0.10    -1.15   0.25   1.20
    ## W2_Newspaper_prefer9          0.18   0.12     1.45   0.15   1.12
    ## RWA                          -0.09   0.26    -0.34   0.73   1.38
    ## W2_DAI_Total                  1.52   0.22     7.03   0.00   1.58
    ## W2_IOU_Total                 -0.84   0.24    -3.48   0.00   1.57
    ## W2_Paranoia_Total             0.77   0.20     3.84   0.00   1.65
    ## threat                       -0.17   0.16    -1.06   0.29   1.19
    ## W1_Conspiracy_Total           0.21   0.20     1.02   0.31   1.12
    ## conspiracy1_sc                0.98   0.13     7.30   0.00   1.39
    ## conspiracy2_sc               -0.09   0.14    -0.65   0.51   1.09
    ## W2_INFO_5:SDO                 1.83   0.35     5.24   0.00   1.70
    ## SDO:W2_INFO_9                 1.35   0.36     3.71   0.00   1.58
    ## ----------------------------------------------------------------

``` r
plot_coefs(full_int_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(full_int_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-56-2.png)<!-- -->

``` r
# comparing AIC with and without interaction
AIC(full_5g)
```

    ## [1] 4973.076

``` r
AIC(full_int_5g) # reasonable support for interaction effect
```

    ## [1] 4963.368

# Parsimonious models

## Chinese lab belief

``` r
pars_lab <- lm(W2_Conspiracy_Theory1 ~
                  
                  #socio-economic variables
                  W1_Education_binary +
                  
                  #political and media variables
                  nat +
                  W2_Trust_Body6 + #distrust of scientists
                  W2_Newspaper_prefer1 + #daily_mail
                  W2_Newspaper_prefer9 + #sun
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  
                  #covid-anxiety
                  threat +
                  
                  #conspiracies
                  W1_Conspiracy_Total +
                  conspiracy2_sc +
                  conspiracy3_sc,
                data = conspiracies)

summ(pars_lab, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1402 (4 missing obs. deleted)
    ## Dependent Variable: W2_Conspiracy_Theory1
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(15,1386) = 38.21, p = 0.00
    ## R² = 0.29
    ## Adj. R² = 0.28 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------
    ##                                Est.   S.E.   t val.      p    VIF
    ## -------------------------- -------- ------ -------- ------ ------
    ## (Intercept)                  -11.24   4.41    -2.55   0.01       
    ## W1_Education_binary1          -4.61   1.57    -2.93   0.00   1.05
    ## nat                           12.41   3.35     3.70   0.00   1.24
    ## W2_Trust_Body6                10.30   3.30     3.12   0.00   1.22
    ## W2_Newspaper_prefer1           9.49   1.71     5.54   0.00   1.10
    ## W2_Newspaper_prefer9           8.05   2.37     3.40   0.00   1.08
    ## W2_INFO_9                     11.19   2.80     4.00   0.00   1.11
    ## SDO                           11.07   4.89     2.26   0.02   1.36
    ## RWA                           20.89   5.03     4.16   0.00   1.30
    ## W2_DAI_Total                  12.78   4.28     2.99   0.00   1.59
    ## W2_IOU_Total                 -10.82   4.72    -2.29   0.02   1.55
    ## W2_Paranoia_Total              8.40   3.81     2.20   0.03   1.53
    ## threat                        10.46   3.16     3.32   0.00   1.17
    ## W1_Conspiracy_Total           27.31   3.97     6.88   0.00   1.10
    ## conspiracy2_sc               -12.68   2.70    -4.70   0.00   1.07
    ## conspiracy3_sc                24.68   3.99     6.18   0.00   1.35
    ## -----------------------------------------------------------------

``` r
plot_coefs(pars_lab)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(pars_lab)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-58-2.png)<!-- -->

## 5G origin belief

``` r
pars_5g <- lm(w2_conspiracy3_ihs ~
                #socio-economic variables
                W1_Income_2019 +
                  
                #political and media variables
                W2_Trust_Body6 + #distrust of scientists
                  
                #political-psychology variables
                W2_DAI_Total +
                W2_IOU_Total +
                W2_Paranoia_Total +
                
                #conspiracies
                conspiracy1_sc +
                
                # interactions
                W2_INFO_5:SDO +
                W2_INFO_9:SDO,
              data = conspiracies)

summ(pars_5g, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 1402 (4 missing obs. deleted)
    ## Dependent Variable: w2_conspiracy3_ihs
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(8,1393) = 87.91, p = 0.00
    ## R² = 0.34
    ## Adj. R² = 0.33 
    ## 
    ## Standard errors: OLS
    ## -------------------------------------------------------------
    ##                            Est.   S.E.   t val.      p    VIF
    ## ----------------------- ------- ------ -------- ------ ------
    ## (Intercept)               -0.04   0.14    -0.27   0.79       
    ## W1_Income_2019            -0.29   0.11    -2.66   0.01   1.07
    ## W2_Trust_Body6             1.48   0.16     9.30   0.00   1.11
    ## W2_DAI_Total               1.51   0.21     7.20   0.00   1.50
    ## W2_IOU_Total              -0.87   0.23    -3.75   0.00   1.49
    ## W2_Paranoia_Total          0.78   0.19     4.06   0.00   1.53
    ## conspiracy1_sc             1.05   0.12     8.48   0.00   1.19
    ## W2_INFO_5:SDO              1.88   0.33     5.64   0.00   1.56
    ## SDO:W2_INFO_9              1.34   0.35     3.78   0.00   1.51
    ## -------------------------------------------------------------

``` r
plot_coefs(pars_5g)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(pars_5g)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-59-2.png)<!-- -->

``` r
summary(full_int_5g)$adj.r.squared
```

    ## [1] 0.3294262

``` r
summary(pars_5g)$adj.r.squared
```

    ## [1] 0.3316821

``` r
AIC(full_int_5g)
```

    ## [1] 4963.368

``` r
AIC(pars_5g)
```

    ## [1] 4959.191

# Ranking of conspiracies

``` r
## [maxn] Function to find nth highest column position for each row (to use among a subset of data)
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]  
```

``` r
## Create differenced DV: item - highest or next highest probability X item probability/100
subs <- names(pca_df)[1:3]

conspiracies$c_first <- apply(conspiracies[subs],
                              1, 
                              function(x)x[maxn(1)(x)])  # Highest probability
```

``` r
conspiracies$c_second <- apply(conspiracies[subs],
                              1, 
                              function(x)x[maxn(2)(x)])  # 2nd highest probability
```

``` r
conspiracies$c_third <- apply(conspiracies[subs],
                              1, 
                              function(x)x[maxn(3)(x)])  # 3rd highest probability
```

``` r
## A start to creating a differenced DV using the name of the top conspiracy and their ratings...

c_first_name <- rep(NA,nrow(conspiracies))

for(i in 1:nrow(conspiracies)){
  
  if(conspiracies$c_first[i] == 
     conspiracies$W2_Conspiracy_Theory1[i]){
    c_first_name[i] <- subs[1]
    } else if(conspiracies$c_first[i] ==
              conspiracies$W2_Conspiracy_Theory2[i]){
      c_first_name[i] <- subs[2]
      } else {
        c_first_name[i] <- subs[3]
        }
  
}

c_second_name <- rep(NA,nrow(conspiracies))

for(i in 1:nrow(conspiracies)){
  
  if(conspiracies$c_second[i] == 
     conspiracies$W2_Conspiracy_Theory1[i]){
    c_second_name[i] <- subs[1]
    } else if(conspiracies$c_second[i] ==
              conspiracies$W2_Conspiracy_Theory2[i]){
      c_second_name[i] <- subs[2]
      } else {
        c_second_name[i] <- subs[3]
        }
  
}

c_third_name <- rep(NA,nrow(conspiracies))

for(i in 1:nrow(conspiracies)){
  
  if(conspiracies$c_third[i] == 
     conspiracies$W2_Conspiracy_Theory1[i]){
    c_third_name[i] <- subs[1]
    } else if(conspiracies$c_third[i] ==
              conspiracies$W2_Conspiracy_Theory2[i]){
      c_third_name[i] <- subs[2]
      } else {
        c_third_name[i] <- subs[3]
        }
  
}

conspiracies <- cbind(conspiracies, 
                      c_first_name, 
                      c_second_name, 
                      c_third_name)
```

``` r
# c1_diff, c1_diff_norm, c2_diff, c2_diff_norm, c3_diff, c3_diff_norm

conspiracies <- conspiracies %>% 
  mutate(
    c1_diff = c_first - c_second,
    c2_diff = c_second - c_third,
    c3_diff = c_first - c_third,
    c1_diff_norm = c1_diff * c_first / 100,
    c2_diff_norm = c2_diff * c_second / 100,
    c3_diff_norm = c3_diff * c_first / 100
  )
```

``` r
conspiracies %>% 
  filter(c1_diff != 0) %>%
  mutate(c_first_name = fct_recode(c_first_name,
                                   "Chinese lab" =
                                     "W2_Conspiracy_Theory1",
                                   "Chinese meat market" = 
                                     "W2_Conspiracy_Theory2",
                                   "5G" = 
                                     "W2_Conspiracy_Theory3")) %>% 
  ggplot(aes(x = c1_diff_norm)) +
  geom_density(aes(colour = c_first_name, fill = c_first_name),
               size = 1, alpha = 0.6) +
  facet_wrap(~c_first_name, ncol = 1) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "top") +
  labs(colour = "Conspiracy", fill = "Conspiracy")
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

``` r
lab_believers <- conspiracies %>% 
  filter(c1_diff != 0 & c_first_name == "W2_Conspiracy_Theory1")

pars_lab_diff <- lm(log(c1_diff_norm) ~
                  
                  #socio-economic variables
                  W1_Education_binary +
                  
                  #political and media variables
                  nat +
                  W2_Trust_Body6 + #distrust of scientists
                  W2_Newspaper_prefer1 + #daily_mail
                  W2_Newspaper_prefer9 + #sun
                  W2_INFO_9 + #family and friends
                  
                  #political-psychology variables
                  SDO +
                  RWA +
                  W2_DAI_Total +
                  W2_IOU_Total +
                  W2_Paranoia_Total +
                  
                  #covid-anxiety
                  threat +
                  
                  #conspiracies
                  W1_Conspiracy_Total,
                data = lab_believers)

summ(pars_lab_diff, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 313 (1 missing obs. deleted)
    ## Dependent Variable: log(c1_diff_norm)
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(13,299) = 1.18, p = 0.29
    ## R² = 0.05
    ## Adj. R² = 0.01 
    ## 
    ## Standard errors: OLS
    ## ----------------------------------------------------------------
    ##                               Est.   S.E.   t val.      p    VIF
    ## -------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                   1.56   0.54     2.90   0.00       
    ## W1_Education_binary1          0.09   0.18     0.52   0.60   1.07
    ## nat                           0.02   0.37     0.05   0.96   1.18
    ## W2_Trust_Body6               -0.17   0.37    -0.46   0.64   1.23
    ## W2_Newspaper_prefer1         -0.03   0.19    -0.18   0.85   1.06
    ## W2_Newspaper_prefer9         -0.31   0.23    -1.32   0.19   1.07
    ## W2_INFO_9                     0.35   0.33     1.07   0.29   1.14
    ## SDO                           0.21   0.56     0.38   0.71   1.22
    ## RWA                           0.80   0.58     1.37   0.17   1.16
    ## W2_DAI_Total                 -0.74   0.49    -1.51   0.13   1.61
    ## W2_IOU_Total                 -0.37   0.57    -0.65   0.52   1.83
    ## W2_Paranoia_Total             0.86   0.43     1.99   0.05   1.50
    ## threat                       -0.26   0.37    -0.70   0.48   1.22
    ## W1_Conspiracy_Total           0.89   0.46     1.92   0.06   1.13
    ## ----------------------------------------------------------------

``` r
plot_coefs(pars_lab_diff)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-68-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(pars_lab_diff)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-68-2.png)<!-- -->

``` r
pr_ggplot(pars_lab_diff)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at -0.005

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 1.005

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 9.2056e-031

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 1.01

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-69-1.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-69-2.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at -0.005

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 0.505

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 7.5793e-016

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 0.0625

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-69-3.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at -0.005

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 1.005

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 4.0728e-030

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 1.01

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-69-4.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : at -0.005

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : radius 2.5e-005

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : all data on boundary of neighborhood. make span bigger

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at -0.005

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 0.005

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 1

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 1.01

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : zero-width neighborhood. make span bigger

    ## Warning: Computation failed in `stat_smooth()`:
    ## NA/NaN/Inf in foreign function call (arg 5)

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-69-5.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at -0.005

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 0.67167

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 1.7424e-015

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 0.45114

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-69-6.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-69-7.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-69-8.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-69-9.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-69-10.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-69-11.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-69-12.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-69-13.png)<!-- -->

``` r
fiveg_believers <- conspiracies %>% 
  filter(c1_diff != 0 & c_first_name == "W2_Conspiracy_Theory3")

# only 54 observations in this dataset
nrow(fiveg_believers)
```

    ## [1] 54

``` r
pars_5g_diff <- lm(log(c1_diff_norm) ~
                #socio-economic variables
                W1_Income_2019 +
                  
                #political and media variables
                W2_Trust_Body6 + #distrust of scientists
                  
                #political-psychology variables
                W2_DAI_Total +
                W2_IOU_Total +
                W2_Paranoia_Total +
                
                #conspiracies
                W1_Conspiracy_Total +
                
                # interactions
                W2_INFO_5:SDO +
                W2_INFO_9:SDO,
              data = fiveg_believers)

summ(pars_5g_diff, vifs = TRUE)
```

    ## MODEL INFO:
    ## Observations: 53 (1 missing obs. deleted)
    ## Dependent Variable: log(c1_diff_norm)
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(8,44) = 0.44, p = 0.89
    ## R² = 0.07
    ## Adj. R² = -0.09 
    ## 
    ## Standard errors: OLS
    ## ---------------------------------------------------------------
    ##                              Est.   S.E.   t val.      p    VIF
    ## ------------------------- ------- ------ -------- ------ ------
    ## (Intercept)                  0.08   1.34     0.06   0.95       
    ## W1_Income_2019              -0.90   0.79    -1.14   0.26   1.06
    ## W2_Trust_Body6               0.01   0.97     0.01   0.99   1.15
    ## W2_DAI_Total                 0.72   1.83     0.39   0.70   2.34
    ## W2_IOU_Total                -0.11   1.76    -0.06   0.95   2.16
    ## W2_Paranoia_Total            0.08   1.60     0.05   0.96   1.50
    ## W1_Conspiracy_Total          1.40   1.37     1.02   0.31   1.34
    ## W2_INFO_5:SDO                1.26   2.05     0.62   0.54   1.69
    ## SDO:W2_INFO_9                0.67   2.21     0.30   0.76   1.69
    ## ---------------------------------------------------------------

``` r
plot_coefs(pars_5g_diff)
```

    ## Loading required namespace: broom.mixed

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-70-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(pars_5g_diff)
```

![](covid_conspiracies_markdown2_files/figure-gfm/unnamed-chunk-70-2.png)<!-- -->
