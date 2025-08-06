# did_had
Estimation of treatment effect in heterogeneous adoption designs.

[Short description](#Short-description) | [Setup](#Setup) | [Syntax](#Syntax) | [Description](#Description) | [Options](#Options) 

[Example](#Example) | [Auxiliary packages](#Auxiliary-packages) | [References](#References) | [Authors](#Authors)

## Short description

Estimates the effect of a treatment on an outcome in a heterogeneous adoption design with no untreated, but some quasi-untrearted groups (see de Chaisemartin et. al. (2025)).

## Setup

### Stata
```s
ssc install did_had, replace
```

### R
```
install.packages("DIDHAD")
```

## Syntax

### Stata
```stata
did_had Y G T D [if] [in] [, effects(#) placebo(#) level(#) kernel(string) bw_method(string) dynamic trends_lin yatchew _no_updates graph_opts(string) graph_off]

```

### R
```r
did_had <- function(
    df,
    outcome,
    group,
    time,
    treatment,
    effects = 1,
    placebo = 0,
    level = 0.05,
    kernel = "epa",
    bw_method = "mse-dpi",
    trends_lin = FALSE,
    dynamic = FALSE,
    yatchew = FALSE,
    graph_off = FALSE
)
```

## Description
**did_had** estimates the effect of a treatment on an outcome in a heterogeneous adoption design (HAD) with no untreated, but some quasi-untreated groups. HADs are designs where all groups are untreated in the first period, and then some groups receive a strictly positive treatment dose at a period $F$, which has to be the same for all treated groups (with variation in treatment timing, the **did_multiplegt_dyn** package may be used). Therefore, there is variation in treatment intensity, but no variation in treatment timing. 


HADs without untreated groups are designs where all groups receive a strictly positive treatment dose at period $F$. Then, one cannot use untreated units to recover the counterfactual outcome evolution that treated groups would have experienced from before to after $F$,without treatment. To circumvent this, **did_had** implements the estimator from de Chaisemartin et. al. (2025) which uses so-called "quasi-untreated groups" as the control group. Quasi-untreated groups are groups that receive a "small enough" treatment dose at $F$ to be regarded as "as good as untreated". Therefore, **did_had** can only be used if there are groups with a treatment dose "close to zero". Formally, the command checks the presence of quasi untreated groups via the test proposed in section 3.3 of de Chaisemartin et al. (2025).


The command makes use of the **lprobust** command by Calonico, Cattaneo and Farrell (2019) to determine an optimal bandwidth, i.e. a treatment dose below which groups can be considered as quasi-untreated. To estimate the treatment's effect, the command starts by computing the difference between the change in outcome of all groups and the intercept in a local linear regression of the outcome change on the treatment dose among quasi-untreated groups. Then, that difference is scaled by groups' average treatment dose at period two. Standard errors and confidence intervals are also computed leveraging **lprobust**. We recommend that users of **did_had** cite de Chaisemartin et. al. (2025), Calonico, Cattaneo and Farrell (2019), and Calonico, Cattaneo and Farrell (2018). 

**Y** is the outcome variable.

**G** is the group variable.

**T** is the time period variable.

**D** is the treatment variable.

## Options
+ **effects**: allows you to specify the number of effects **did_had()** tries to estimate. Effect $\ell$ is the treatment's effect at period $F-1+\ell$, namely $\ell$ periods after adoption. By default, the command estimates only 1 effect and in case you specified more effects than your data allows to estimate the number of effects is automatically adjusted to the maximum. 
+ **placebo**: allows you to specify the number of placebo estimates **did_had()** tries to compute. Those placebos are constructed symmetrically to the estimators of the actual effects, except that the outcome evolution from $F-1$ to $F-1+\ell$ in the actual estimator is replaced by the outcome evolution from $F-1$ to $F-1-\ell$ in the placebo.  
+ **level**: allows you to specify (1-the level) of the confidence intervals shown by the command. By default this level is set to 0.05, thus yielding 95% level confidence intervals.
+ **kernel**: allows you to specify the kernel function used by **lprobust()**. Possible choices are triangular ("tri"), epanechnikov ("epa"), uniform ("uni") and gaussian ("gau"). By default, the program uses a epanechnikov kernel.
+ **bw_method**:  allows you to specify the bandwidth selection procedure used by **lprobust**. Possible methods are "mse-dpi", "mse-rot", "imse-dpi", "imse-rot", "ce-dpi", "ce-rot". By default, the program uses **mse-dpi**. For more details please consult the **lprobust** helpfile.
+ **dynamic**: when this option is specified, effect $\ell$ is scaled by groups' average total treatment dose received from period $F$ to $F-1+\ell$. Without this option, effect $\ell$ is scaled by groups' average treatment dose at period $F-1+\ell$. The latter normalization is appropriate if one assumes that groups' outcome at $F-1+\ell$ is only affected by their current treatment (static model). On the other hand, the former normalization is appropriate if one assumes that groups' outcome at $F-1+\ell$ can be affected by their current and past treatments (dynamic model).
+ **trends_lin**: when this option is specified, the command allows for group-specific linear trends.  This is done by using groups' outcome evolution from period $F-2$ to $F-1$ as an estimator of each group-specific linear trend, and then subtracting this trend from groups' actual outcome evolutions.  Note: due to the fitting of the linear trend in periods $F-2$ to $F-1$, the number of feasible placebo estimates is reduced by 1 with this option.
+ **yatchew**: yatchew yields the result from a non-parametric test that the conditional expectation of the $F-1$ to $F-1+\ell$ outcome evolution given the treatment at $F-1+\ell$ is linear (Yatchew, 1997). This test is implemented using the heteroskedasticity-robust test statistic proposed in Appendix E of de Chaisemartin et al. (2025) and it is performed for all the dynamic effects and placebos computed by **did_had()**. For placebos, the null being tested is that groups' $F-1$ to $F-1-\ell$ outcome evolution is mean independent of their $F-1+\ell$ treatment, a non-parametric version of a standard pre-trends test where the $F-1$ to $F-1-\ell$ outcome evolution is regressed on groups' $F-1+\ell$ treatment. This option requires the YatchewTest package, which is currently available on CRAN.

>[!IMPORTANT]
>**Interpreting the results from yatchew**. Following Theorem 5 of de Chaisemartin et. al. (2025), in designs where there are untreated or quasi-untreated groups, under a parallel trends assumption the treatment coefficient from a regression of groups' $F-1$ to $F-1+\ell$ outcome evolution on their the treatment at $F-1+\ell$ is unbiased for the WAS if and only if the conditional expectation of the outcome evolution from $F-1$ to $F-1+\ell$ given the treatment at $F-1+\ell$ is linear. As a result, if the linearity hypothesis cannot be rejected, then one can unbiasedly estimate the WAS at period $F-1+\ell$ using the simple OLS regression described above, rather than resorting to the non-parametric estimator computed by **did_had**.

+ **graph_off**: by default, **did_had()** outputs an event-study graph with the effect and placebo estimates and their confidence intervals. When specifying **graph_off**, the graph is suppressed.
+ **_no_updates**: this option stops automatic self-updates of the program, which are performed (on average) every 100 runs.
+ **graphoptions**: one can use this option to modify the appearance of the graph produced by the command. Options requested have to follow the syntax of Stata **twoway_options**.

## Example    

Installing the package and loading the data for the example:
```s
ssc install did_had, replace
use "https://raw.githubusercontent.com/chaisemartinPackages/did_had/main/tutorial_data.dta", clear
```

Estimating the effects over five periods and placebos for four pre-treatment periods:      
```s
did_had y g t d, effects(5) placebo(4)
```

Doing the same estimation, but with a triangular kernel and suppressing the graph output:
```s
did_had y g t d, effects(5) placebo(4) kernel(tri) graph_off
```

Changing the level of the confidence interval:
```s
did_had y g t d, effects(5) placebo(4) level(0.1)
```

## References
+ de Chaisemartin, C., Ciccia, D., D'Haultfoeuille, X. and Knau, F. (2025). [Difference-in-Differences Estimators When No Unit Remains Untreated](https://arxiv.org/abs/2405.04465).
+ Calonico, S., M. D. Cattaneo, and M. H. Farrell. (2019). [nprobust: Nonparametric Kernel-Based Estimation and Robust Bias-Corrected Inference](https://nppackages.github.io/references/Calonico-Cattaneo-Farrell_2019_JSS.pdf).
+ Calonico, S., M. D. Cattaneo, and M. H. Farrell. (2018). [On the Effect of Bias Estimation on Coverage Accuracy in Nonparametric Inference](https://nppackages.github.io/references/Calonico-Cattaneo-Farrell_2018_JASA.pdf).
+ Yatchew, A. (1997). [An elementary estimator of the partial linear model](doi:10.1016/S0165-1765(97)00218-8).

## Auxiliary packages

The command requires that the [**lprobust**](https://github.com/nppackages/nprobust/tree/master) package be installed on the user's machine.

## Authors
+ Clément de Chaisemartin, Economics Department, Sciences Po, France.
+ Diego Ciccia, Kellogg School of Management, Northwestern University, USA.
+ Xavier D'Haultfoeuille, CREST-ENSAE, France.
+ Felix Knau, LMU Munich, Germany.
+ Doulo Sow, Princeton University, USA.

**<ins>Contact:</ins>**  
[chaisemartin.packages@gmail.com](mailto:chaisemartin.packages@gmail.com)

