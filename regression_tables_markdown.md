Tables
================
Michael Marshall
12/12/2020

``` r
# loading models
full_lab <- readRDS("full_lab.RDS")
full_5g <- readRDS("full_5g.RDS")
full_5g_ihs <- readRDS("full_5g_ihs.RDS")
full_5g_poiss <- readRDS("full_5g_poiss.RDS")
full_meat <- readRDS("full_meat.RDS")
full_dist <- readRDS("full_dist.RDS")
full_vax <- readRDS("full_vax.RDS")
conspiracies2 <- readRDS("conspiracies2.RDS")
```

## Stargazer

``` r
# setting up variable orders
to_plot <- c("age_sc","W1_Education_binary1","W2_Gender_binary2",
             "W1_Income_2019","elite_news","W2_INFO_9","mid_level_news",
             "W2_INFO_5","red_top_tabloid","threat","W2_DAI_Total",
             "distrust_science","fis_con","W2_IOU_Total","W2_Chance_Total",
             "W2_Internal_Total","W2_PO_Total","nat","W2_Paranoia_Total",
             "RWA","SDO","crt","CRT_test","W1_Conspiracy_Total",
             "conspiracy1_sc","conspiracy2_sc","conspiracy3_sc")
names(to_plot) <- c("Age","Education","Gender","Income","Elite news",
              "Family and friends","Mid-level news","Social media",
              "Tabloid news","COVID-19 anxiety","Death anxiety",
              "Distrust scientists","Fiscal conservatism",
              "Intolerance of uncertainty","LOC: chance","LOC: internal",
              "LOC: powerful others","Nationalism","Paranoia","RWA",
              "SDO","CRT","CRT pre-exposure","Conspiracy ideation",
              "Wuhan lab belief","Meat market belief","5G belief")

to_plot <- to_plot[sort(names(to_plot))]
```

``` r
stargazer(full_lab, full_5g, full_meat, 
          title="Table A2: OLS Regression Results Origin Theory Belief",
          dep.var.labels=c("Wuhan lab","5G","Meat market"),
          order = to_plot,
          covariate.labels = names(to_plot),
          #omit.stat=c("LL","ser","f","bic"),
          keep.stat = c("n","rsq","adj.rsq","aic"),
          single.row=TRUE,
          align = TRUE,
          report = "vcs*",
          type = "html")
```

<table style="text-align:center">

<caption>

<strong>Table A2: OLS Regression Results Origin Theory Belief</strong>

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

Wuhan lab

</td>

<td>

5G

</td>

<td>

Meat market

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

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

0.215 (0.041)<sup>\*\*\*</sup>

</td>

<td>

</td>

<td>

0.023 (0.040)

</td>

</tr>

<tr>

<td style="text-align:left">

Age

</td>

<td>

0.015 (0.043)

</td>

<td>

\-0.024 (0.028)

</td>

<td>

0.024 (0.042)

</td>

</tr>

<tr>

<td style="text-align:left">

Conspiracy ideation

</td>

<td>

0.250 (0.040)<sup>\*\*\*</sup>

</td>

<td>

0.026 (0.027)

</td>

<td>

0.024 (0.040)

</td>

</tr>

<tr>

<td style="text-align:left">

COVID-19 anxiety

</td>

<td>

0.096 (0.032)<sup>\*\*\*</sup>

</td>

<td>

\-0.034 (0.021)

</td>

<td>

0.143 (0.032)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

CRT

</td>

<td>

\-0.069 (0.026)<sup>\*\*\*</sup>

</td>

<td>

\-0.078 (0.017)<sup>\*\*\*</sup>

</td>

<td>

0.015 (0.026)

</td>

</tr>

<tr>

<td style="text-align:left">

CRT pre-exposure

</td>

<td>

0.023 (0.016)

</td>

<td>

\-0.007 (0.011)

</td>

<td>

\-0.009 (0.016)

</td>

</tr>

<tr>

<td style="text-align:left">

Death anxiety

</td>

<td>

0.112 (0.044)<sup>\*\*</sup>

</td>

<td>

0.160 (0.028)<sup>\*\*\*</sup>

</td>

<td>

0.038 (0.043)

</td>

</tr>

<tr>

<td style="text-align:left">

Distrust scientists

</td>

<td>

0.104 (0.033)<sup>\*\*\*</sup>

</td>

<td>

0.137 (0.022)<sup>\*\*\*</sup>

</td>

<td>

\-0.071 (0.033)<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Education

</td>

<td>

\-0.038 (0.016)<sup>\*\*</sup>

</td>

<td>

0.001 (0.011)

</td>

<td>

\-0.018 (0.016)

</td>

</tr>

<tr>

<td style="text-align:left">

Elite news

</td>

<td>

\-0.013 (0.016)

</td>

<td>

0.010 (0.011)

</td>

<td>

0.013 (0.016)

</td>

</tr>

<tr>

<td style="text-align:left">

Family and friends

</td>

<td>

0.094 (0.030)<sup>\*\*\*</sup>

</td>

<td>

0.063 (0.020)<sup>\*\*\*</sup>

</td>

<td>

\-0.022 (0.029)

</td>

</tr>

<tr>

<td style="text-align:left">

Fiscal conservatism

</td>

<td>

0.022 (0.041)

</td>

<td>

0.019 (0.027)

</td>

<td>

0.017 (0.040)

</td>

</tr>

<tr>

<td style="text-align:left">

Gender

</td>

<td>

0.012 (0.016)

</td>

<td>

\-0.004 (0.010)

</td>

<td>

\-0.004 (0.016)

</td>

</tr>

<tr>

<td style="text-align:left">

Income

</td>

<td>

\-0.032 (0.023)

</td>

<td>

\-0.013 (0.015)

</td>

<td>

0.036 (0.023)

</td>

</tr>

<tr>

<td style="text-align:left">

Intolerance of uncertainty

</td>

<td>

\-0.103 (0.049)<sup>\*\*</sup>

</td>

<td>

\-0.109 (0.032)<sup>\*\*\*</sup>

</td>

<td>

0.125 (0.048)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

LOC: chance

</td>

<td>

0.006 (0.051)

</td>

<td>

\-0.033 (0.033)

</td>

<td>

0.151 (0.050)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

LOC: internal

</td>

<td>

0.070 (0.049)

</td>

<td>

\-0.00002 (0.032)

</td>

<td>

0.212 (0.048)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

LOC: powerful others

</td>

<td>

0.0004 (0.048)

</td>

<td>

0.080 (0.031)<sup>\*\*</sup>

</td>

<td>

\-0.075 (0.047)

</td>

</tr>

<tr>

<td style="text-align:left">

Meat market belief

</td>

<td>

\-0.124 (0.027)<sup>\*\*\*</sup>

</td>

<td>

0.010 (0.018)

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Mid-level news

</td>

<td>

0.101 (0.017)<sup>\*\*\*</sup>

</td>

<td>

\-0.006 (0.011)

</td>

<td>

0.019 (0.017)

</td>

</tr>

<tr>

<td style="text-align:left">

Nationalism

</td>

<td>

0.106 (0.035)<sup>\*\*\*</sup>

</td>

<td>

0.019 (0.023)

</td>

<td>

0.141 (0.034)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Paranoia

</td>

<td>

0.087 (0.042)<sup>\*\*</sup>

</td>

<td>

0.102 (0.027)<sup>\*\*\*</sup>

</td>

<td>

\-0.055 (0.041)

</td>

</tr>

<tr>

<td style="text-align:left">

RWA

</td>

<td>

0.149 (0.053)<sup>\*\*\*</sup>

</td>

<td>

\-0.125 (0.035)<sup>\*\*\*</sup>

</td>

<td>

\-0.022 (0.052)

</td>

</tr>

<tr>

<td style="text-align:left">

SDO

</td>

<td>

0.133 (0.051)<sup>\*\*\*</sup>

</td>

<td>

0.176 (0.033)<sup>\*\*\*</sup>

</td>

<td>

0.018 (0.051)

</td>

</tr>

<tr>

<td style="text-align:left">

Social media

</td>

<td>

0.040 (0.028)

</td>

<td>

0.054 (0.018)<sup>\*\*\*</sup>

</td>

<td>

\-0.014 (0.028)

</td>

</tr>

<tr>

<td style="text-align:left">

Tabloid news

</td>

<td>

0.042 (0.018)<sup>\*\*</sup>

</td>

<td>

0.048 (0.012)<sup>\*\*\*</sup>

</td>

<td>

0.007 (0.018)

</td>

</tr>

<tr>

<td style="text-align:left">

Wuhan lab belief

</td>

<td>

</td>

<td>

0.092 (0.017)<sup>\*\*\*</sup>

</td>

<td>

\-0.119 (0.026)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

\-0.111 (0.063)<sup>\*</sup>

</td>

<td>

\-0.048 (0.041)

</td>

<td>

0.266 (0.062)<sup>\*\*\*</sup>

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

0.303

</td>

<td>

0.313

</td>

<td>

0.103

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.290

</td>

<td>

0.299

</td>

<td>

0.086

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

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

``` r
stargazer(full_5g_ihs, full_5g_poiss, 
          title="Table A3: Supplementary Models 5G Belief",
          dep.var.labels=c("5G belief","5G belief"),
          multicolumn = TRUE,
          column.labels = c("IHS","Poisson"),
          model.names = FALSE,
          model.numbers = FALSE,
          order = to_plot[-1],
          covariate.labels = names(to_plot)[-1],
          #omit.stat=c("LL","ser","f","bic"),
          keep.stat = c("n"),
          single.row=TRUE,
          align = TRUE,
          report = "vcs*",
          type = "html")
```

<table style="text-align:center">

<caption>

<strong>Table A3: Supplementary Models 5G Belief</strong>

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

5G belief

</td>

<td>

5G belief

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

IHS

</td>

<td>

Poisson

</td>

</tr>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Age

</td>

<td>

0.068 (0.056)

</td>

<td>

\-0.409 (0.255)

</td>

</tr>

<tr>

<td style="text-align:left">

Conspiracy ideation

</td>

<td>

0.048 (0.053)

</td>

<td>

0.492 (0.240)<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

COVID-19 anxiety

</td>

<td>

\-0.081 (0.042)<sup>\*</sup>

</td>

<td>

\-0.280 (0.202)

</td>

</tr>

<tr>

<td style="text-align:left">

CRT

</td>

<td>

\-0.216 (0.034)<sup>\*\*\*</sup>

</td>

<td>

\-0.858 (0.184)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

CRT pre-exposure

</td>

<td>

0.025 (0.021)

</td>

<td>

0.010 (0.096)

</td>

</tr>

<tr>

<td style="text-align:left">

Death anxiety

</td>

<td>

0.324 (0.056)<sup>\*\*\*</sup>

</td>

<td>

1.408 (0.273)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Distrust scientists

</td>

<td>

0.302 (0.043)<sup>\*\*\*</sup>

</td>

<td>

0.766 (0.174)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Education

</td>

<td>

0.022 (0.021)

</td>

<td>

0.016 (0.096)

</td>

</tr>

<tr>

<td style="text-align:left">

Elite news

</td>

<td>

\-0.009 (0.021)

</td>

<td>

0.054 (0.099)

</td>

</tr>

<tr>

<td style="text-align:left">

Family and friends

</td>

<td>

0.099 (0.038)<sup>\*\*</sup>

</td>

<td>

0.483 (0.181)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Fiscal conservatism

</td>

<td>

0.0001 (0.053)

</td>

<td>

\-0.046 (0.227)

</td>

</tr>

<tr>

<td style="text-align:left">

Gender

</td>

<td>

0.041 (0.021)<sup>\*\*</sup>

</td>

<td>

0.016 (0.095)

</td>

</tr>

<tr>

<td style="text-align:left">

Income

</td>

<td>

\-0.025 (0.030)

</td>

<td>

0.001 (0.148)

</td>

</tr>

<tr>

<td style="text-align:left">

Intolerance of uncertainty

</td>

<td>

\-0.163 (0.063)<sup>\*\*\*</sup>

</td>

<td>

\-1.053 (0.281)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

LOC: chance

</td>

<td>

\-0.098 (0.066)

</td>

<td>

\-0.289 (0.369)

</td>

</tr>

<tr>

<td style="text-align:left">

LOC: internal

</td>

<td>

\-0.109 (0.064)<sup>\*</sup>

</td>

<td>

\-0.723 (0.282)<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

LOC: powerful others

</td>

<td>

0.158 (0.062)<sup>\*\*</sup>

</td>

<td>

0.608 (0.310)<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Meat market belief

</td>

<td>

\-0.006 (0.035)

</td>

<td>

\-0.095 (0.172)

</td>

</tr>

<tr>

<td style="text-align:left">

Mid-level news

</td>

<td>

\-0.009 (0.022)

</td>

<td>

\-0.022 (0.096)

</td>

</tr>

<tr>

<td style="text-align:left">

Nationalism

</td>

<td>

0.055 (0.045)

</td>

<td>

0.094 (0.218)

</td>

</tr>

<tr>

<td style="text-align:left">

Paranoia

</td>

<td>

0.131 (0.053)<sup>\*\*</sup>

</td>

<td>

0.790 (0.253)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

RWA

</td>

<td>

\-0.030 (0.069)

</td>

<td>

\-0.431 (0.343)

</td>

</tr>

<tr>

<td style="text-align:left">

SDO

</td>

<td>

0.353 (0.066)<sup>\*\*\*</sup>

</td>

<td>

1.662 (0.319)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Social media

</td>

<td>

0.130 (0.036)<sup>\*\*\*</sup>

</td>

<td>

0.268 (0.168)

</td>

</tr>

<tr>

<td style="text-align:left">

Tabloid news

</td>

<td>

0.079 (0.023)<sup>\*\*\*</sup>

</td>

<td>

0.362 (0.095)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Wuhan lab belief

</td>

<td>

0.218 (0.034)<sup>\*\*\*</sup>

</td>

<td>

0.905 (0.169)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

0.043 (0.081)

</td>

<td>

0.988 (0.368)<sup>\*\*\*</sup>

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

1,399

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

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

``` r
stargazer(full_dist, full_vax,
          title="Table A4: Public Health Measures Model Results",
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
          type = "html")
```

<table style="text-align:center">

<caption>

<strong>Table A4: Public Health Measures Model Results</strong>

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

\-0.110 (0.023)<sup>\*\*\*</sup>

</td>

<td>

1.902 (0.461)<sup>\*\*\*</sup>

</td>

<td>

\-0.066 (0.389)

</td>

</tr>

<tr>

<td style="text-align:left">

Age

</td>

<td>

0.118 (0.024)<sup>\*\*\*</sup>

</td>

<td>

\-2.528 (0.649)<sup>\*\*\*</sup>

</td>

<td>

\-1.102 (0.388)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Conspiracy ideation

</td>

<td>

0.030 (0.022)

</td>

<td>

0.112 (0.614)

</td>

<td>

0.733 (0.378)<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

COVID-19 anxiety

</td>

<td>

0.061 (0.018)<sup>\*\*\*</sup>

</td>

<td>

\-1.957 (0.473)<sup>\*\*\*</sup>

</td>

<td>

\-1.314 (0.286)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

CRT

</td>

<td>

0.013 (0.014)

</td>

<td>

\-0.429 (0.405)

</td>

<td>

0.186 (0.235)

</td>

</tr>

<tr>

<td style="text-align:left">

CRT pre-exposure

</td>

<td>

0.003 (0.009)

</td>

<td>

0.017 (0.229)

</td>

<td>

0.002 (0.145)

</td>

</tr>

<tr>

<td style="text-align:left">

Death anxiety

</td>

<td>

\-0.106 (0.024)<sup>\*\*\*</sup>

</td>

<td>

\-0.267 (0.658)

</td>

<td>

0.068 (0.391)

</td>

</tr>

<tr>

<td style="text-align:left">

Distrust scientists

</td>

<td>

\-0.114 (0.018)<sup>\*\*\*</sup>

</td>

<td>

2.306 (0.456)<sup>\*\*\*</sup>

</td>

<td>

0.900 (0.299)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Education

</td>

<td>

0.014 (0.009)

</td>

<td>

0.157 (0.235)

</td>

<td>

0.130 (0.146)

</td>

</tr>

<tr>

<td style="text-align:left">

Elite news

</td>

<td>

0.006 (0.009)

</td>

<td>

0.071 (0.235)

</td>

<td>

\-0.204 (0.152)

</td>

</tr>

<tr>

<td style="text-align:left">

Family and friends

</td>

<td>

0.041 (0.016)<sup>\*\*</sup>

</td>

<td>

\-0.267 (0.448)

</td>

<td>

\-0.022 (0.273)

</td>

</tr>

<tr>

<td style="text-align:left">

Fiscal conservatism

</td>

<td>

\-0.013 (0.022)

</td>

<td>

0.015 (0.589)

</td>

<td>

0.151 (0.373)

</td>

</tr>

<tr>

<td style="text-align:left">

Gender

</td>

<td>

0.038 (0.009)<sup>\*\*\*</sup>

</td>

<td>

0.212 (0.231)

</td>

<td>

0.286 (0.144)<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Income

</td>

<td>

\-0.0004 (0.013)

</td>

<td>

\-0.161 (0.348)

</td>

<td>

\-0.739 (0.208)<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Intolerance of uncertainty

</td>

<td>

0.081 (0.027)<sup>\*\*\*</sup>

</td>

<td>

\-0.733 (0.701)

</td>

<td>

0.107 (0.437)

</td>

</tr>

<tr>

<td style="text-align:left">

LOC: chance

</td>

<td>

0.041 (0.028)

</td>

<td>

\-0.477 (0.841)

</td>

<td>

0.531 (0.461)

</td>

</tr>

<tr>

<td style="text-align:left">

LOC: internal

</td>

<td>

0.281 (0.027)<sup>\*\*\*</sup>

</td>

<td>

\-0.814 (0.689)

</td>

<td>

\-0.239 (0.445)

</td>

</tr>

<tr>

<td style="text-align:left">

LOC: powerful others

</td>

<td>

0.004 (0.026)

</td>

<td>

0.574 (0.746)

</td>

<td>

\-0.234 (0.431)

</td>

</tr>

<tr>

<td style="text-align:left">

Meat market belief

</td>

<td>

0.047 (0.015)<sup>\*\*\*</sup>

</td>

<td>

\-0.924 (0.397)<sup>\*\*</sup>

</td>

<td>

\-0.478 (0.241)<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Mid-level news

</td>

<td>

0.003 (0.009)

</td>

<td>

0.171 (0.238)

</td>

<td>

0.067 (0.156)

</td>

</tr>

<tr>

<td style="text-align:left">

Nationalism

</td>

<td>

0.015 (0.019)

</td>

<td>

\-0.856 (0.502)<sup>\*</sup>

</td>

<td>

\-0.698 (0.315)<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

Paranoia

</td>

<td>

\-0.035 (0.023)

</td>

<td>

1.034 (0.606)<sup>\*</sup>

</td>

<td>

0.321 (0.368)

</td>

</tr>

<tr>

<td style="text-align:left">

RWA

</td>

<td>

0.129 (0.029)<sup>\*\*\*</sup>

</td>

<td>

0.915 (0.790)

</td>

<td>

0.548 (0.486)

</td>

</tr>

<tr>

<td style="text-align:left">

SDO

</td>

<td>

\-0.182 (0.028)<sup>\*\*\*</sup>

</td>

<td>

0.419 (0.777)

</td>

<td>

0.195 (0.463)

</td>

</tr>

<tr>

<td style="text-align:left">

Social media

</td>

<td>

\-0.015 (0.015)

</td>

<td>

0.358 (0.407)

</td>

<td>

0.221 (0.251)

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

\-0.221 (0.247)

</td>

<td>

\-0.107 (0.163)

</td>

</tr>

<tr>

<td style="text-align:left">

Wuhan lab belief

</td>

<td>

0.010 (0.015)

</td>

<td>

0.943 (0.407)<sup>\*\*</sup>

</td>

<td>

0.092 (0.242)

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

0.490 (0.035)<sup>\*\*\*</sup>

</td>

<td>

\-0.712 (0.895)

</td>

<td>

\-0.357 (0.565)

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

0.335

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

0.322

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

2,074.292

</td>

<td>

2,074.292

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

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>
