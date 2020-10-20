covid\_conspiracies\_markdown
================
Michael Marshall
20/10/2020

## Loading Packages and Data

``` r
pacman::p_load(tidyverse, stringr, ggridges, forcats, labelled)

load("COVID W1_W2_W3 Cleaned 2878.RData") # needs to be in your wd
```

## Summary and distribution of different COVID specific conspiracies

``` r
df %>% 
  select(W2_Conspiracy_Theory1:W2_Conspiracy_Theory5) %>% 
  map(summary)
```

    ## $W2_Conspiracy_Theory1
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    3.00   45.50   38.26   62.00  100.00    1472 
    ## 
    ## $W2_Conspiracy_Theory2
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00   50.00   70.00   63.96   88.00  100.00    1472 
    ## 
    ## $W2_Conspiracy_Theory3
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    0.00    1.00   11.15    7.00  100.00    1472 
    ## 
    ## $W2_Conspiracy_Theory4
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    0.00    8.00   24.88   49.00  100.00    1472 
    ## 
    ## $W2_Conspiracy_Theory5
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    0.00    2.00   14.73   18.00  100.00    1472

``` r
# plotting density of different covid conspiracies
df %>% 
  select(W2_Conspiracy_Theory1:W2_Conspiracy_Theory5) %>% 
  gather(conspiracy_code, belief, W2_Conspiracy_Theory1:W2_Conspiracy_Theory5) %>%
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

![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Selecting variables for dataset

Sorry, this is a lot of manual code, and if you can think of a quicker
way I’d be happy to use it. I tend to prefer reducing the number of
variables to just those I expect to reasonably use, which the code below
aims to achieve.

``` r
df_names <- names(df)

w2_vars <- c(
  "W2_Age_year",
  "W2_Age_categories",
  "W2_Gender",
  "W2_Other_gender",
  "W2_Gender_binary",
  "W2_Employment",
  "W2_Living_alone",
  "W2_INFO_1",
  "W2_INFO_2",
  "W2_INFO_3",
  "W2_INFO_4",
  "W2_INFO_5",
  "W2_INFO_6",
  "W2_INFO_7",
  "W2_INFO_8",
  "W2_INFO_9",
  "W2_TRUST_1",
  "W2_TRUST_2",
  "W2_TRUST_3",
  "W2_TRUST_4",
  "W2_TRUST_5",
  "W2_TRUST_6",
  "W2_TRUST_7",
  "W2_TRUST_8",
  "W2_TRUST_9",
  "W2_COVID19_anxiety",
  "W2_LOC1",
  "W2_LOC2",
  "W2_LOC3",
  "W2_LOC4",
  "W2_LOC5",
  "W2_LOC6",
  "W2_LOC7",
  "W2_LOC8",
  "W2_LOC9",
  "W2_DAI1",
  "W2_DAI2",
  "W2_DAI3",
  "W2_DAI4",
  "W2_DAI5",
  "W2_DAI6",
  "W2_DAI7",
  "W2_DAI8",
  "W2_DAI9",
  "W2_DAI10",
  "W2_DAI11",
  "W2_DAI12",
  "W2_DAI13",
  "W2_DAI14",
  "W2_DAI15",
  "W2_DAI16",
  "W2_DAI17",
  "W2_IOU1",
  "W2_IOU2",
  "W2_IOU3",
  "W2_IOU4",
  "W2_IOU5",
  "W2_IOU6",
  "W2_IOU7",
  "W2_IOU8",
  "W2_IOU9",
  "W2_IOU10",
  "W2_IOU11",
  "W2_IOU12",
  "W2_Nationalism1",
  "W2_Nationalism2",
  "W2_Trust_Body1",
  "W2_Trust_Body2",
  "W2_Trust_Body3",
  "W2_Trust_Body4",
  "W2_Trust_Body5",
  "W2_Trust_Body6",
  "W2_Trust_Body7",
  "W2_Newspaper_prefer1",
  "W2_Newspaper_prefer2",
  "W2_Newspaper_prefer3",
  "W2_Newspaper_prefer4",
  "W2_Newspaper_prefer5",
  "W2_Newspaper_prefer6",
  "W2_Newspaper_prefer7",
  "W2_Newspaper_prefer8",
  "W2_Newspaper_prefer9",
  "W2_Newspaper_prefer10",
  "W2_Newspaper_prefer11",
  "W2_Conspiracy_Theory1",
  "W2_Conspiracy_Theory2",
  "W2_Conspiracy_Theory3",
  "W2_Conspiracy_Theory4",
  "W2_Conspiracy_Theory5",
  "W2_Nationalism_Total",
  "W2_Paranoia_Total",
  "W2_Dep_Total",
  "W2_GAD_Total",
  "W2_Internal_Total",
  "W2_Chance_Total",
  "W2_PO_Total",
  "W2_DAI_Total",
  "W2_ProspectiveAnx_Total",
  "W2_InhibitoryAnx_Total",
  "W2_IOU_Total"
)

w1_vars <- c(
  "pid",
  "W1_Area_residence",
  "W1_Ethnicity",
  "W1_Education",
  "W1_Education_binary",
  "W1_Religion",
  "W1_Hosuing_tenure",
  "W1_Income_2019",
  "W1_C19_Infected",
  "W1_C19_Infected_Binary",
  "W1_C19_SomeoneClose_Infected",
  "W1_C19_SomeoneClose_Infected_Binary",
  "W1_ReligiousBelief1",
  "W1_ReligiousBelief2",
  "W1_ReligiousBelief3",
  "W1_ReligiousBelief4",
  "W1_ReligiousBelief5",
  "W1_ReligiousBelief6",
  "W1_ReligiousBelief7",
  "W1_ReligiousBelief8",
  "W1_ReligiousBelief2_R",
  "W1_ReligiousBelief4_R",
  "W1_ReligiousBelief6_R",
  "W1_ReligiousBelief8_R",
  "W1_CRT1",
  "W1_CRT2",
  "W1_CRT3",
  "W1_CRT4",
  "W1_CRT5",
  "W1_CRT_test",
  "W1_Voted_GenElection",
  "W1_Voted_Party",
  "W1_EURef",
  "W1_Political_Scale",
  "W1_Political_Abortion_SSM",
  "W1_Political_Fiscal",
  "W1_Authoritarianism1",
  "W1_Authoritarianism2",
  "W1_Authoritarianism3",
  "W1_Authoritarianism4",
  "W1_Authoritarianism5",
  "W1_Authoritarianism6",
  "W1_Authoritarianism1_R",
  "W1_Authoritarianism4_R",
  "W1_Authoritarianism5_R",
  "W1_MigrantAttitudes1",
  "W1_MigrantAttitudes2",
  "W1_MigrantAttitudes3",
  "W1_Conspiracy_1",
  "W1_Conspiracy_2",
  "W1_Conspiracy_3",
  "W1_Conspiracy_4",
  "W1_Conspiracy_5",
  "W1_ReligiousBelief_Total",
  "W1_Authoritarianism_Total",
  "W1_Social_Dominance_Mean",
  "W1_Social_Dominance_Total",
  "W1_Conspiracy_Total"
)

w3_vars <- c(
  "W3_ContactTracing",
  "W3_Smartphone",
  "W3_C19App1",
  "W3_C19App2",
  "W3_C19App3",
  "W3_C19App4"
)

vars <- c(w1_vars,w2_vars,w3_vars)

mean(vars %in% df_names) # checking all variable names spelled correctly
```

    ## [1] 1

``` r
# selecting variables
df_thin <- df %>% 
  select(one_of(vars))
```

## Cleaning dataset

The following code filters down to just those observations that have
completed the battery of questions relating to COVID specific
conspiracies. It also creates a tibble counting the missing
observations, which can be useful to have as an object.

``` r
# filtering for completed dependent variable
conspiracies <- df_thin %>% 
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
  select(W2_Conspiracy_Theory1:W2_Conspiracy_Theory5) %>% 
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
dummy variables, as they were previously coded as *1=Yes* and everthing
else as *NA*.

``` r
# combining 2019 election variables into one, for parties and didn't votes
pacman::p_load(forcats,labelled)

conspiracies <- conspiracies %>% 
  mutate(
    W1_Voted_Party_Name = to_factor(conspiracies$W1_Voted_Party,
                                    nolabel_to_na = TRUE),
    W1_Voted_GenElection_Name = to_factor(conspiracies$W1_Voted_GenElection,
                                          nolabel_to_na = TRUE),
    W1_2019_GE_Full = factor(ifelse(
      W1_Voted_GenElection_Name == "Voted.",
      as.character(W1_Voted_Party_Name),
      as.character(W1_Voted_GenElection_Name)
      )
      )
    )

W1_2019_GE_Full <- factor(
  ifelse(
    conspiracies$W1_Voted_GenElection_Name == "Voted.",
    to_factor(conspiracies$W1_Voted_Party,
              nolabel_to_na = TRUE),
    to_factor(conspiracies$W1_Voted_GenElection,
              nolabel_to_na = TRUE)
    )
)


# making preferred newspaper a dummy variable (i.e. replacing NA with 0)
na_to_zero <- function(x){
  x[is.na(x)] <- 0
  x <- as.numeric(x)
  return(x)
} 

paper_vars <- conspiracies %>%
  select(W2_Newspaper_prefer1:W2_Newspaper_prefer11) %>% 
  names()

conspiracies[paper_vars] <- conspiracies[paper_vars] %>% 
  map_df(na_to_zero)
```

## Distribution of variables

A for loop to look at distribution of potential independent variables
(numeric only).

``` r
plot_vars <- conspiracies %>% 
  select(
    W1_Political_Scale:W1_Political_Fiscal,
    W1_ReligiousBelief_Total:W1_Conspiracy_Total,
    W2_Nationalism_Total:W2_IOU_Total
    ) %>%
  names()

for(i in seq_along(plot_vars)){
  
  x1 <- conspiracies[plot_vars][i] %>% as_vector()
  
  print(
    ggplot(data = NULL, aes(x = x1)) +
      geom_vline(aes(xintercept = mean(x1, na.rm =TRUE)), 
                 colour = "black",
                 #alpha = 0.7,
                 linetype = "dashed") +
      geom_vline(aes(xintercept = median(x1, na.rm =TRUE)), 
                 colour = "red",
                 #alpha = 0.7,
                 linetype = "dashed") +
      geom_density(fill = "lightblue", alpha = 0.7) +
      labs(x = plot_vars[i],
           caption = "Black = Mean, Red = Median")  
  )
  
}
```

![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

    ## Warning: Removed 30 rows containing non-finite values (stat_density).

![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-5.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-6.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-7.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-8.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-9.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-10.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-11.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-12.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-13.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-14.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-15.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-16.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-17.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-18.png)<!-- -->![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-6-19.png)<!-- -->

## Vote in 2019 election and conspiracy belief

``` r
conspiracies %>% 
  ggplot(aes(x = fct_reorder(W1_2019_GE_Full, W2_Conspiracy_Theory1),
             y = W2_Conspiracy_Theory1)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "Vote in 2019 GE",
       y = "Belief in Chinese Lab Origin")
```

![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# filtering down for some of the main parties
parties <- c("UKIP",
             "Did not vote.",
             "Conservative",
             "Labour",
             "Green",
             "Scottish Nationalists",
             "Liberal Democrats")

conspiracies %>% 
  filter(W1_2019_GE_Full %in% parties) %>% 
  ggplot(aes(x = W2_Conspiracy_Theory1,
             y = fct_reorder(W1_2019_GE_Full, W2_Conspiracy_Theory1))) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantiles = 4,
                      scale = 0.9,
                      jittered_points = TRUE,
                      vline_size = 1,
                      point_size = 0.6,
                      point_alpha = 0.6,
                      position = "raincloud") +
  labs(x = "Vote in 2019 GE",
       y = "Belief in Chinese lab origin")
```

    ## Picking joint bandwidth of 12

![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
# interestingly, Conservatives have relatively high belief in Chinese lab conspiracy

conspiracies %>% 
  ggplot(aes(x = fct_reorder(W1_2019_GE_Full, W2_Conspiracy_Theory3),
             y = W2_Conspiracy_Theory3)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "Vote in 2019 GE",
       y = "Belief in 5G Origin")
```

![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
conspiracies %>% 
  filter(W1_2019_GE_Full %in% parties) %>% 
  ggplot(aes(x = W2_Conspiracy_Theory3,
             y = fct_reorder(W1_2019_GE_Full, W2_Conspiracy_Theory3))) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantiles = 4,
                      scale = 0.9,
                      jittered_points = TRUE,
                      vline_size = 1,
                      point_size = 0.6,
                      point_alpha = 0.6,
                      position = "raincloud") +
  labs(x = "Vote in 2019 GE",
       y = "Belief in 5G origin")
```

    ## Picking joint bandwidth of 4.57

![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->

``` r
conspiracies %>% 
  ggplot(aes(x = fct_reorder(W1_2019_GE_Full, W2_Conspiracy_Theory4),
             y = W2_Conspiracy_Theory4)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "Vote in 2019 GE",
       y = "Belief it is no worse than flu")
```

![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->

``` r
conspiracies %>% 
  filter(W1_2019_GE_Full %in% parties) %>% 
  ggplot(aes(x = W2_Conspiracy_Theory4,
             y = fct_reorder(W1_2019_GE_Full, W2_Conspiracy_Theory4))) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantiles = 4,
                      scale = 0.9,
                      jittered_points = TRUE,
                      vline_size = 1,
                      point_size = 0.6,
                      point_alpha = 0.6,
                      position = "raincloud") +
  labs(x = "Vote in 2019 GE",
       y = "Belief it is no worse than flu")
```

    ## Picking joint bandwidth of 11

![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-7-6.png)<!-- -->

``` r
conspiracies %>% 
  ggplot(aes(x = fct_reorder(W1_2019_GE_Full, W2_Conspiracy_Theory5),
             y = W2_Conspiracy_Theory5)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "Vote in 2019 GE",
       y = "Belief in Vitamin C treatment")
```

![](covid_conspiracies_markdown_files/figure-gfm/unnamed-chunk-7-7.png)<!-- -->