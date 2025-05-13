Phlugis dataset
================
Harrison, Woodrow, Goode, Montealgre-Z, Deeming and Sutton

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
#Packages to install
install.packages("ggplot2", repos = "https://cloud.r-project.org/")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/1r/pxmpztmx37vdfjcbmsrvd4wc0000gn/T//RtmpVDAFkW/downloaded_packages

``` r
install.packages("viridis", repos = "https://cloud.r-project.org/")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/1r/pxmpztmx37vdfjcbmsrvd4wc0000gn/T//RtmpVDAFkW/downloaded_packages

``` r
install.packages("lme4", repos = "https://cloud.r-project.org/")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/1r/pxmpztmx37vdfjcbmsrvd4wc0000gn/T//RtmpVDAFkW/downloaded_packages

``` r
install.packages("DHARMa", repos = "https://cloud.r-project.org/")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/1r/pxmpztmx37vdfjcbmsrvd4wc0000gn/T//RtmpVDAFkW/downloaded_packages

``` r
install.packages("dplyr", repos = "https://cloud.r-project.org/")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/1r/pxmpztmx37vdfjcbmsrvd4wc0000gn/T//RtmpVDAFkW/downloaded_packages

``` r
install.packages("ggpubr", repos = "https://cloud.r-project.org/")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/1r/pxmpztmx37vdfjcbmsrvd4wc0000gn/T//RtmpVDAFkW/downloaded_packages

``` r
install.packages("sjPlot", repos = "https://cloud.r-project.org/")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/1r/pxmpztmx37vdfjcbmsrvd4wc0000gn/T//RtmpVDAFkW/downloaded_packages

``` r
install.packages("ggeffects", repos = "https://cloud.r-project.org/")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/1r/pxmpztmx37vdfjcbmsrvd4wc0000gn/T//RtmpVDAFkW/downloaded_packages

``` r
install.packages("emmeans", repos = "https://cloud.r-project.org/")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/1r/pxmpztmx37vdfjcbmsrvd4wc0000gn/T//RtmpVDAFkW/downloaded_packages

``` r
# Load these packages
library(ggplot2)
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(lme4)
```

    ## Loading required package: Matrix

``` r
library(DHARMa)
```

    ## This is DHARMa 0.4.7. For overview type '?DHARMa'. For recent changes, type news(package = 'DHARMa')

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggpubr)
library(sjPlot)
```

    ## Learn more about sjPlot with 'browseVignettes("sjPlot")'.

``` r
library(ggeffects)
library(emmeans)
```

    ## Welcome to emmeans.
    ## Caution: You lose important information if you filter this package's results.
    ## See '? untidy'

``` r
#Load dataset 1


data<- read.csv("dataset_1.csv", stringsAsFactors = FALSE)
str(data)
```

    ## 'data.frame':    151 obs. of  8 variables:
    ##  $ Animal : chr  "A1" "A1" "A1" "A1" ...
    ##  $ Jump_no: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ JH     : int  50 50 50 50 50 50 50 50 50 50 ...
    ##  $ LV     : num  1.3 1.15 1.22 1.64 1.66 ...
    ##  $ LA     : num  1.69 1.31 1.5 2.68 2.75 ...
    ##  $ AV     : num  36.9 59.3 56.6 50.1 42.9 ...
    ##  $ AA     : num  1364 3514 3202 2512 1841 ...
    ##  $ RE.KE  : num  1.4 4.67 3.72 1.63 1.17 ...

``` r
#Converts the jump heights from continuous to categorical variable
data$JH<-as.factor(data$JH)
```

``` r
#Kinematics

#Calculate the average angular velocity per-animal averages
average_per_animal_jumpLV <- data %>%
  group_by(Animal, JH) %>%
  summarize(animal_jump_LV = mean(`LV`, na.rm = TRUE), .groups = "drop")

print(average_per_animal_jumpLV)
```

    ## # A tibble: 21 × 3
    ##    Animal JH    animal_jump_LV
    ##    <chr>  <fct>          <dbl>
    ##  1 A1     50              1.23
    ##  2 A1     75              1.60
    ##  3 A1     100             1.91
    ##  4 A4     50              1.30
    ##  5 A4     75              1.80
    ##  6 A4     100             1.56
    ##  7 A5     50              1.32
    ##  8 A5     75              1.25
    ##  9 A5     100             1.66
    ## 10 A6     50              1.52
    ## # ℹ 11 more rows

``` r
#Calculate the average angular velocity per jump height based on per-animal averages
average_per_jump_heightLV <- average_per_animal_jumpLV %>%
  group_by(JH) %>%
  summarize(overall_average_linear_velocity = mean(animal_jump_LV, na.rm = TRUE), .groups = "drop")

print(average_per_jump_heightLV)
```

    ## # A tibble: 3 × 2
    ##   JH    overall_average_linear_velocity
    ##   <fct>                           <dbl>
    ## 1 50                               1.40
    ## 2 75                               1.56
    ## 3 100                              1.79

``` r
#Angular velocity

#Calculate the average angular velocity per-animal averages
average_per_animal_jumpAV <- data %>%
  group_by(Animal, JH) %>%
  summarize(animal_jump_AV = mean(`AV`, na.rm = TRUE), .groups = "drop")

print(average_per_animal_jumpAV)
```

    ## # A tibble: 21 × 3
    ##    Animal JH    animal_jump_AV
    ##    <chr>  <fct>          <dbl>
    ##  1 A1     50              48.8
    ##  2 A1     75              35.8
    ##  3 A1     100             31.7
    ##  4 A4     50              45.1
    ##  5 A4     75              46.3
    ##  6 A4     100             28.0
    ##  7 A5     50              47.7
    ##  8 A5     75              29.9
    ##  9 A5     100             33.5
    ## 10 A6     50              28.5
    ## # ℹ 11 more rows

``` r
#Calculate the average angular velocity per jump height based on per-animal averages
average_per_jump_heightAV <- average_per_animal_jumpAV %>%
  group_by(JH) %>%
  summarize(overall_average_angular_velocity = mean(animal_jump_AV, na.rm = TRUE), .groups = "drop")

print(average_per_jump_heightAV)
```

    ## # A tibble: 3 × 2
    ##   JH    overall_average_angular_velocity
    ##   <fct>                            <dbl>
    ## 1 50                                47.4
    ## 2 75                                38.1
    ## 3 100                               31.8

``` r
#Energetics

#Calculate the mean of RE_KE per animal and jump height
average_per_animal_jumpRE.KE <- data %>%
  group_by(Animal, JH) %>%
  summarize(animal_jump_RE.KE = mean(RE.KE, na.rm = TRUE), .groups = "drop")
print(average_per_animal_jumpRE.KE)
```

    ## # A tibble: 21 × 3
    ##    Animal JH    animal_jump_RE.KE
    ##    <chr>  <fct>             <dbl>
    ##  1 A1     50                3.03 
    ##  2 A1     75                0.945
    ##  3 A1     100               0.504
    ##  4 A4     50                1.75 
    ##  5 A4     75                0.916
    ##  6 A4     100               0.449
    ##  7 A5     50                2.89 
    ##  8 A5     75                1.20 
    ##  9 A5     100               0.843
    ## 10 A6     50                0.987
    ## # ℹ 11 more rows

``` r
#Calculate the average of the animal averages for each jump height
average_per_jump_heightRE.KE <- average_per_animal_jumpRE.KE %>%
  group_by(JH) %>%
  summarize(overall_average_RE.KE = mean(animal_jump_RE.KE, na.rm = TRUE), .groups = "drop")
print(average_per_jump_heightRE.KE)
```

    ## # A tibble: 3 × 2
    ##   JH    overall_average_RE.KE
    ##   <fct>                 <dbl>
    ## 1 50                    2.16 
    ## 2 75                    1.05 
    ## 3 100                   0.568

``` r
#Linear mixed effect models

#Linear velocity
LVmodel<- lmerTest::lmer(LV ~ JH + (1|Animal), data = data)
# summarise the linear velocity model and run an anova
summary(LVmodel)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: LV ~ JH + (1 | Animal)
    ##    Data: data
    ## 
    ## REML criterion at convergence: 30.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4517 -0.6307 -0.1086  0.7062  3.2473 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Animal   (Intercept) 0.03393  0.1842  
    ##  Residual             0.06046  0.2459  
    ## Number of obs: 151, groups:  Animal, 7
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)   1.35568    0.07881   7.83708  17.202 1.67e-07 ***
    ## JH75          0.20167    0.04959 142.68012   4.067 7.84e-05 ***
    ## JH100         0.49298    0.05058 142.97891   9.747  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr) JH75  
    ## JH75  -0.315       
    ## JH100 -0.310  0.535

``` r
anova(LVmodel)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##    Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## JH 5.8567  2.9284     2 142.55  48.434 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
LVpairwise_comparisons <- emmeans(LVmodel, pairwise ~ JH, adjust = "none")
print(LVpairwise_comparisons)
```

    ## $emmeans
    ##  JH  emmean     SE   df lower.CL upper.CL
    ##  50    1.36 0.0788 7.86     1.17     1.54
    ##  75    1.56 0.0789 7.81     1.37     1.74
    ##  100   1.85 0.0794 8.02     1.67     2.03
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast     estimate     SE  df t.ratio p.value
    ##  JH50 - JH75    -0.202 0.0496 143  -4.064  0.0001
    ##  JH50 - JH100   -0.493 0.0506 143  -9.736  <.0001
    ##  JH75 - JH100   -0.291 0.0483 142  -6.032  <.0001
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
#Angular velocity
AVmodel<- lmerTest::lmer(LV ~ JH + (1|Animal), data = data)
# summarise the angular velocity model and run an anova
summary(AVmodel)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: LV ~ JH + (1 | Animal)
    ##    Data: data
    ## 
    ## REML criterion at convergence: 30.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4517 -0.6307 -0.1086  0.7062  3.2473 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Animal   (Intercept) 0.03393  0.1842  
    ##  Residual             0.06046  0.2459  
    ## Number of obs: 151, groups:  Animal, 7
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)   1.35568    0.07881   7.83708  17.202 1.67e-07 ***
    ## JH75          0.20167    0.04959 142.68012   4.067 7.84e-05 ***
    ## JH100         0.49298    0.05058 142.97891   9.747  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr) JH75  
    ## JH75  -0.315       
    ## JH100 -0.310  0.535

``` r
anova(AVmodel)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##    Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## JH 5.8567  2.9284     2 142.55  48.434 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
AVpairwise_comparisons <- emmeans(AVmodel, pairwise ~ JH, adjust = "none")
print(AVpairwise_comparisons)
```

    ## $emmeans
    ##  JH  emmean     SE   df lower.CL upper.CL
    ##  50    1.36 0.0788 7.86     1.17     1.54
    ##  75    1.56 0.0789 7.81     1.37     1.74
    ##  100   1.85 0.0794 8.02     1.67     2.03
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast     estimate     SE  df t.ratio p.value
    ##  JH50 - JH75    -0.202 0.0496 143  -4.064  0.0001
    ##  JH50 - JH100   -0.493 0.0506 143  -9.736  <.0001
    ##  JH75 - JH100   -0.291 0.0483 142  -6.032  <.0001
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
#Energetics
RE.KE_model<- lmerTest::lmer(RE.KE ~ JH + (1|Animal), data = data)

# summarise the RE.KE model and run an anova
summary(RE.KE_model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: RE.KE ~ JH + (1 | Animal)
    ##    Data: data
    ## 
    ## REML criterion at convergence: 353.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3784 -0.3868 -0.0288  0.2901  3.6547 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Animal   (Intercept) 0.05655  0.2378  
    ##  Residual             0.56592  0.7523  
    ## Number of obs: 151, groups:  Animal, 7
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)   2.3440     0.1440  13.4709  16.283    3e-10 ***
    ## JH75         -1.4063     0.1512 143.8568  -9.299   <2e-16 ***
    ## JH100        -1.8650     0.1541 144.4114 -12.105   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr) JH75  
    ## JH75  -0.534       
    ## JH100 -0.527  0.532

``` r
anova(RE.KE_model)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##    Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## JH 89.384  44.692     2 143.32  78.972 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
RE.KE_pairwise_comparisons<- emmeans(RE.KE_model, pairwise ~ JH, adjust = "none")
print(RE.KE_pairwise_comparisons)
```

    ## $emmeans
    ##  JH  emmean    SE   df lower.CL upper.CL
    ##  50   2.344 0.145 14.5    2.035    2.653
    ##  75   0.938 0.144 13.1    0.626    1.249
    ##  100  0.479 0.147 14.0    0.164    0.795
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast     estimate    SE  df t.ratio p.value
    ##  JH50 - JH75     1.406 0.152 144   9.275  <.0001
    ##  JH50 - JH100    1.865 0.155 145  12.069  <.0001
    ##  JH75 - JH100    0.459 0.148 142   3.105  0.0023
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
#Body posture before and during jump
#Load dataset 2

data2 <- read.csv("dataset_2.csv", stringsAsFactors = FALSE)
data2$ranked_distance <- rank(data2$JH)


# View the data to check all looks good
head(data2)
```

    ##   Animal Jump_no JH    DF.FTJ     FTJ.F     DF.F    H.FTJ      H.F     H.DF
    ## 1     A1       1 50  9.224609  8.735022 2.698210 22.96174 24.18774 21.67811
    ## 2     A1       2 50  9.147253  8.434082 1.709275 24.53613 17.15123 15.78949
    ## 3     A1       3 50  9.241668  8.666397 3.062713 24.95205 20.08369 17.31728
    ## 4     A1       4 50  9.092002  8.387508 2.156613 26.74418 25.64416 23.55663
    ## 5     A1       5 50  7.972877  7.044354 2.377177 26.24607 25.36531 23.07739
    ## 6     A1       6 50 10.679359 10.557682 2.897507 27.00520 19.11888 17.15808
    ##   H.FTJangle ranked_distance
    ## 1   124.3851              24
    ## 2   148.3466              24
    ## 3   158.0201              24
    ## 4   118.1520              24
    ## 5   114.4682              24
    ## 6   154.1374              24

``` r
#Converts the jump heights from continuous to categorical variable
data2$JH <- as.factor(data2$JH)

# run mixed effects model with jump height as fixed effect
#DF.FTJ
modelDF.FTJ<- lmerTest::lmer(DF.FTJ ~ JH + (1|Animal), data = data2)
summary(modelDF.FTJ)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: DF.FTJ ~ JH + (1 | Animal)
    ##    Data: data2
    ## 
    ## REML criterion at convergence: 464.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2923 -0.5257  0.2006  0.6471  2.3971 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Animal   (Intercept) 0.1272   0.3567  
    ##  Residual             1.2002   1.0955  
    ## Number of obs: 151, groups:  Animal, 7
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)   7.8624     0.2121  13.6457   37.06 4.31e-15 ***
    ## JH75          0.4670     0.2203 143.9563    2.12   0.0357 *  
    ## JH100         0.2580     0.2244 144.4932    1.15   0.2521    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr) JH75  
    ## JH75  -0.528       
    ## JH100 -0.520  0.532

``` r
anova(modelDF.FTJ)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##    Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
    ## JH 5.3953  2.6977     2 143.47  2.2477 0.1094

``` r
pairwise_comparisons_DF.FTJ <- emmeans(modelDF.FTJ, pairwise ~ JH, adjust = "none")
print(pairwise_comparisons_DF.FTJ)
```

    ## $emmeans
    ##  JH  emmean    SE   df lower.CL upper.CL
    ##  50    7.86 0.213 14.2     7.41     8.32
    ##  75    8.33 0.213 12.8     7.87     8.79
    ##  100   8.12 0.217 13.7     7.65     8.59
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast     estimate    SE  df t.ratio p.value
    ##  JH50 - JH75    -0.467 0.221 144  -2.115  0.0362
    ##  JH50 - JH100   -0.258 0.225 145  -1.147  0.2535
    ##  JH75 - JH100    0.209 0.215 142   0.971  0.3331
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
#FTJ.F
modelFTJ.F<- lmerTest::lmer(FTJ.F ~ JH + (1|Animal), data = data2)
summary(modelFTJ.F)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: FTJ.F ~ JH + (1 | Animal)
    ##    Data: data2
    ## 
    ## REML criterion at convergence: 549.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8896 -0.7970 -0.0422  0.6239  3.4431 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Animal   (Intercept) 0.5558   0.7455  
    ##  Residual             2.0695   1.4386  
    ## Number of obs: 151, groups:  Animal, 7
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)   4.3366     0.3549  11.3225  12.219 7.21e-08 ***
    ## JH75          2.8994     0.2898 143.9131  10.005  < 2e-16 ***
    ## JH100         2.6165     0.2955 144.2933   8.856 2.77e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr) JH75  
    ## JH75  -0.411       
    ## JH100 -0.405  0.534

``` r
anova(modelFTJ.F)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##    Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## JH 242.87  121.43     2 143.71  58.676 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise_comparisons_FTJ.F <- emmeans(modelFTJ.F, pairwise ~ JH, adjust = "none")
print(pairwise_comparisons_FTJ.F)
```

    ## $emmeans
    ##  JH  emmean    SE   df lower.CL upper.CL
    ##  50    4.34 0.355 9.72     3.54     5.13
    ##  75    7.24 0.355 9.47     6.44     8.03
    ##  100   6.95 0.359 9.88     6.15     7.76
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast     estimate    SE  df t.ratio p.value
    ##  JH50 - JH75    -2.899 0.290 143  -9.991  <.0001
    ##  JH50 - JH100   -2.616 0.296 144  -8.839  <.0001
    ##  JH75 - JH100    0.283 0.283 142   1.001  0.3183
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
#DF.F
modelDF.F<- lmerTest::lmer(DF.F ~ JH + (1|Animal), data = data2)
summary(modelDF.F)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: DF.F ~ JH + (1 | Animal)
    ##    Data: data2
    ## 
    ## REML criterion at convergence: 302.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7924 -0.6036 -0.0746  0.4519  3.6391 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Animal   (Intercept) 0.1234   0.3513  
    ##  Residual             0.3870   0.6221  
    ## Number of obs: 151, groups:  Animal, 7
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept) 2.908e+00  1.623e-01 1.078e+01  17.919 2.28e-09 ***
    ## JH75        2.644e-03  1.254e-01 1.438e+02   0.021    0.983    
    ## JH100       1.037e-01  1.278e-01 1.442e+02   0.811    0.419    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr) JH75  
    ## JH75  -0.388       
    ## JH100 -0.382  0.534

``` r
anova(modelDF.F)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##     Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
    ## JH 0.34671 0.17336     2 143.66   0.448 0.6398

``` r
pairwise_comparisons_DF.F <- emmeans(modelDF.F, pairwise ~ JH, adjust = "none")
print(pairwise_comparisons_DF.F)
```

    ## $emmeans
    ##  JH  emmean    SE   df lower.CL upper.CL
    ##  50    2.91 0.162 9.18     2.54     3.27
    ##  75    2.91 0.162 9.00     2.54     3.28
    ##  100   3.01 0.164 9.35     2.64     3.38
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast     estimate    SE  df t.ratio p.value
    ##  JH50 - JH75  -0.00264 0.126 143  -0.021  0.9832
    ##  JH50 - JH100 -0.10367 0.128 143  -0.810  0.4194
    ##  JH75 - JH100 -0.10102 0.122 142  -0.827  0.4097
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
# H.FTJ
modelH.FTJ<- lmerTest::lmer(H.FTJ ~ JH + (1|Animal), data = data2)
summary(modelH.FTJ)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: H.FTJ ~ JH + (1 | Animal)
    ##    Data: data2
    ## 
    ## REML criterion at convergence: 651.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.9233 -0.5337  0.0220  0.6451  2.2200 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Animal   (Intercept) 1.828    1.352   
    ##  Residual             4.056    2.014   
    ## Number of obs: 151, groups:  Animal, 7
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)  23.9854     0.5938   8.2536  40.392 8.86e-11 ***
    ## JH75          1.3409     0.4060 142.8015   3.302  0.00121 ** 
    ## JH100         1.9076     0.4141 143.1452   4.607 8.97e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr) JH75  
    ## JH75  -0.343       
    ## JH100 -0.338  0.535

``` r
anova(modelH.FTJ)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##    Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## JH 90.068  45.034     2 142.65  11.102 3.301e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise_comparisons_H.FTJ <- emmeans(modelH.FTJ, pairwise ~ JH, adjust = "none")
print(pairwise_comparisons_H.FTJ)
```

    ## $emmeans
    ##  JH  emmean    SE   df lower.CL upper.CL
    ##  50    24.0 0.594 8.30     22.6     25.3
    ##  75    25.3 0.594 8.21     24.0     26.7
    ##  100   25.9 0.599 8.47     24.5     27.3
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast     estimate    SE  df t.ratio p.value
    ##  JH50 - JH75    -1.341 0.406 143  -3.299  0.0012
    ##  JH50 - JH100   -1.908 0.415 143  -4.600  <.0001
    ##  JH75 - JH100   -0.567 0.396 142  -1.432  0.1542
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
#H.F
modelH.F<- lmerTest::lmer(H.F ~ JH + (1|Animal), data = data2)
summary(modelH.F)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: H.F ~ JH + (1 | Animal)
    ##    Data: data2
    ## 
    ## REML criterion at convergence: 750.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8794 -0.5234  0.0572  0.7543  1.6801 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Animal   (Intercept) 2.069    1.438   
    ##  Residual             8.075    2.842   
    ## Number of obs: 151, groups:  Animal, 7
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)  24.1209     0.6908  11.7355  34.917 3.18e-13 ***
    ## JH75          1.0103     0.5724 144.0290   1.765   0.0797 .  
    ## JH100         2.6858     0.5835 144.4086   4.603 9.06e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr) JH75  
    ## JH75  -0.417       
    ## JH100 -0.411  0.534

``` r
anova(modelH.F)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##    Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## JH 176.49  88.243     2 143.82  10.928 3.819e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise_comparisons_H.F <- emmeans(modelH.F, pairwise ~ JH, adjust = "none")
print(pairwise_comparisons_H.F)
```

    ## $emmeans
    ##  JH  emmean    SE    df lower.CL upper.CL
    ##  50    24.1 0.691  9.88     22.6     25.7
    ##  75    25.1 0.691  9.61     23.6     26.7
    ##  100   26.8 0.700 10.03     25.2     28.4
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast     estimate    SE  df t.ratio p.value
    ##  JH50 - JH75     -1.01 0.573 143  -1.763  0.0801
    ##  JH50 - JH100    -2.69 0.585 144  -4.594  <.0001
    ##  JH75 - JH100    -1.68 0.558 142  -3.002  0.0032
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
#H.DF
modelH.DF<- lmerTest::lmer(H.DF ~ JH + (1|Animal), data = data2)
summary(modelH.DF)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: H.DF ~ JH + (1 | Animal)
    ##    Data: data2
    ## 
    ## REML criterion at convergence: 732
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2083 -0.5044  0.0424  0.7522  1.7249 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Animal   (Intercept) 1.198    1.094   
    ##  Residual             7.207    2.684   
    ## Number of obs: 151, groups:  Animal, 7
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)  21.3998     0.5769  14.1761  37.097 1.60e-15 ***
    ## JH75          0.9913     0.5403 144.4544   1.835   0.0686 .  
    ## JH100         2.4614     0.5506 144.8710   4.470 1.57e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr) JH75  
    ## JH75  -0.474       
    ## JH100 -0.467  0.533

``` r
anova(modelH.DF)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##    Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## JH 147.04  73.519     2 144.17  10.202 7.181e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise_comparisons_H.DF <- emmeans(modelH.DF, pairwise ~ JH, adjust = "none")
print(pairwise_comparisons_H.DF)
```

    ## $emmeans
    ##  JH  emmean    SE   df lower.CL upper.CL
    ##  50    21.4 0.578 11.7     20.1     22.7
    ##  75    22.4 0.578 11.1     21.1     23.7
    ##  100   23.9 0.587 11.7     22.6     25.1
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast     estimate    SE  df t.ratio p.value
    ##  JH50 - JH75    -0.991 0.541 144  -1.831  0.0691
    ##  JH50 - JH100   -2.461 0.552 144  -4.460  <.0001
    ##  JH75 - JH100   -1.470 0.527 142  -2.788  0.0060
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
#H.FTJangle
modelH.FTJangle<- lmerTest::lmer(H.FTJangle ~ JH + (1|Animal), data = data2)
summary(modelH.FTJangle)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: H.FTJangle ~ JH + (1 | Animal)
    ##    Data: data2
    ## 
    ## REML criterion at convergence: 1253.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.6649 -0.5658 -0.1790  0.5012  7.3325 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Animal   (Intercept)  97.19    9.858  
    ##  Residual             237.67   15.417  
    ## Number of obs: 151, groups:  Animal, 7
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)  116.417      4.386   9.015  26.541 7.21e-10 ***
    ## JH75           3.446      3.108 143.182   1.109    0.269    
    ## JH100          3.285      3.169 143.524   1.036    0.302    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr) JH75  
    ## JH75  -0.356       
    ## JH100 -0.350  0.535

``` r
anova(modelH.FTJangle)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##    Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
    ## JH 357.73  178.87     2 143.02  0.7526  0.473

``` r
pairwise_comparisons_H.FTJangle <- emmeans(modelH.FTJangle, pairwise ~ JH, adjust = "none")
print(pairwise_comparisons_H.FTJangle)
```

    ## $emmeans
    ##  JH  emmean   SE   df lower.CL upper.CL
    ##  50     116 4.39 8.52      106      126
    ##  75     120 4.39 8.41      110      130
    ##  100    120 4.43 8.70      110      130
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast     estimate   SE  df t.ratio p.value
    ##  JH50 - JH75    -3.446 3.11 143  -1.108  0.2698
    ##  JH50 - JH100   -3.285 3.17 143  -1.035  0.3024
    ##  JH75 - JH100    0.162 3.03 142   0.053  0.9575
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
#Knee angle at take-off
#Load dataset 3
data3 <- read.csv("dataset_3.csv", stringsAsFactors = FALSE)
str(data3)
```

    ## 'data.frame':    151 obs. of  4 variables:
    ##  $ Animal : chr  "A1" "A1" "A1" "A1" ...
    ##  $ Jump_no: chr  "A1J150" "A1J250" "A1J350" "A1J450" ...
    ##  $ JH     : int  50 50 50 50 50 50 50 50 50 50 ...
    ##  $ Knee   : num  130 119 126 139 127 ...

``` r
#Converts the jump heights from continuous to categorical variable
data3$JH <- as.factor(data3$JH)


# Group by Animal and JH, gt average knee angle per animal
data3 <- data3 %>%
  group_by(Animal, JH) %>%
  summarize(
    Knee_angle = mean(Knee, na.rm = TRUE), 
  )
```

    ## `summarise()` has grouped output by 'Animal'. You can override using the
    ## `.groups` argument.

``` r
print(data3)
```

    ## # A tibble: 21 × 3
    ## # Groups:   Animal [7]
    ##    Animal JH    Knee_angle
    ##    <chr>  <fct>      <dbl>
    ##  1 A1     50          124.
    ##  2 A1     75          135.
    ##  3 A1     100         143.
    ##  4 A4     50          130.
    ##  5 A4     75          133.
    ##  6 A4     100         141.
    ##  7 A5     50          126.
    ##  8 A5     75          121.
    ##  9 A5     100         137.
    ## 10 A6     50          135.
    ## # ℹ 11 more rows

``` r
#Calculate the average angular velocity per jump height based on per-animal averages
# Calculate overall Knee angle grouped by jump height (JH)
Knee_angle_summary <- data3 %>%
  
  group_by(JH) %>%
  summarize(
    overall_Knee_angle = mean(Knee_angle, na.rm = TRUE), 
  )

print(Knee_angle_summary)
```

    ## # A tibble: 3 × 2
    ##   JH    overall_Knee_angle
    ##   <fct>              <dbl>
    ## 1 50                  130.
    ## 2 75                  135.
    ## 3 100                 144.

``` r
#Test for differences between knee angle and jump height
data3_model<- lmerTest::lmer(Knee_angle ~ JH + (1|Animal), data = data3)

# summarise the model and run an anova
summary(data3_model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: Knee_angle ~ JH + (1 | Animal)
    ##    Data: data3
    ## 
    ## REML criterion at convergence: 119.5
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.68850 -0.36424  0.09089  0.44069  1.40877 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Animal   (Intercept) 31.18    5.584   
    ##  Residual             17.45    4.177   
    ## Number of obs: 21, groups:  Animal, 7
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)  129.570      2.636   9.877  49.161 3.88e-13 ***
    ## JH75           5.316      2.233  12.000   2.381   0.0347 *  
    ## JH100         14.329      2.233  12.000   6.418 3.31e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr) JH75  
    ## JH75  -0.424       
    ## JH100 -0.424  0.500

``` r
anova(data3_model)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##    Sum Sq Mean Sq NumDF DenDF F value   Pr(>F)    
    ## JH 734.57  367.28     2    12  21.054 0.000119 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#post hoc tests
data3_pairwise_comparisons <- emmeans(data3_model, pairwise ~ JH, adjust = "none")
print(data3_pairwise_comparisons)
```

    ## $emmeans
    ##  JH  emmean   SE   df lower.CL upper.CL
    ##  50     130 2.64 9.88      124      135
    ##  75     135 2.64 9.88      129      141
    ##  100    144 2.64 9.88      138      150
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast     estimate   SE df t.ratio p.value
    ##  JH50 - JH75     -5.32 2.23 12  -2.381  0.0347
    ##  JH50 - JH100   -14.33 2.23 12  -6.418  <.0001
    ##  JH75 - JH100    -9.01 2.23 12  -4.037  0.0016
    ## 
    ## Degrees-of-freedom method: kenward-roger
