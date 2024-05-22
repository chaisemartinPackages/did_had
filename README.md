# did_had
Estimation of treatment effect in heterogeneous adoption designs.

[Short description](#Short-description) | [Vignettes](#vignettes) | [Setup](#Setup) | [Syntax](#Syntax) | [Description](#Description)

[Options](#Options) | [Example](#Example) | [FAQ](#FAQ) | [Auxiliary packages](#Auxiliary-packages) | [References](#References) | [Authors](#Authors)

## Short description

Estimates the effect of a treatment on an outcome in a heterogeneous adoption design with no stayers but some quasi stayers (see de Chaisemartin and D'Haultfoeuille (2024)).
 

## Vignettes

## Setup

### Stata
```s
ssc install did_multiplegt_dyn, replace
```

### R

## Syntax

### Stata
**did_had Y G T D** [*if*] [*in*] [, **effects(#) placebo(#) level(#) kernel(string) graph_off**]

### R

## Description

**did_had** estimates the effect of a treatment on an outcome in a heterogeneous adoption design (HAD) with no stayers but some
        quasi stayers. HADs are designs where all groups are untreated in the first period, and then some groups receive a strictly
        positive treatment dose at a period F, which has to be the same for all treated groups (with variation in treatment timing,
        the did_multiplegt_dyn package may be used).  Therefore, there is variation in treatment intensity, but no variation in
        treatment timing.  HADs without stayers are designs where all groups receive a strictly positive treatment dose at period
        F: no group remains untreated. Then, one cannot use untreated units to recover the counterfactual outcome evolution that
        treated groups would have experienced from before to after F, without treatment. To circumvent this, **did_had** implements the
        estimator from de Chaisemartin and D'Haultfoeuille (2024) which uses so-called "quasi stayers" as the control group.  Quasi
        stayers are groups that receive a "small enough" treatment dose at F to be regarded as "as good as untreated".  Therefore,
        **did_had** can only be used if there are groups with a treatment dose "close to zero". Formally, the density of groups'
        period-two treatment dose needs to be strictly positive at zero, something that can be assessed by plotting a kernel
        density estimate of that density.  The command makes use of the **lprobust** command by Calonico, Cattaneo and Farrell (2019)
        to determine an optimal bandwidth, i.e. a treatment dose below which groups can be considered as quasi stayers.  To
        estimate the treatment's effect, the command starts by computing the difference between the change in outcome of all groups
        and the intercept in a local linear regression of the outcome change on the treatment dose among quasi-stayers. Then, that
        difference is scaled by groups' average treatment dose at period two. Standard errors and confidence intervals are also
        computed leveraging **lprobust**.  We recommend that users of **did_had** cite de Chaisemartin and D'Haultfoeuille (2024),
        Calonico, Cattaneo and Farrell (2019), and Calonico, Cattaneo and Farrell (2018).


**Y** is the outcome variable.

**G** is the group variable.

**T** is the time period variable.

**D** is the treatment variable.

## Options

**effects(#)** allows you to specify the number of effects **did_had** tries to estimate. Effect **ℓ** is the treatment's effect at period
        F-1+**ℓ**, namely **ℓ** periods after adoption. By default, the command estimates only 1 effect and in case you specified more
        effects than your data allows to estimate the number of effects is automatically adjusted to the maximum.

**placebo(#)** allows you to specify the number of placebo estimates **did_had** tries to compute. Those placebos are constructed
        symmetrically to the estimators of the actual effects, except that the outcome evolution from F-1 to F-1+**ℓ** in the actual
        estimator is replaced by the outcome evolution from F-1 to F-1-**ℓ** in the placebo.

**level(#)** allows you to specify (1-the level) of the confidence intervals shown by the command. By default this level is set to
        0.05, thus yielding 95% level confidence intervals.

**kernel(string)** allows you to specify the kernel function used by **lprobust**. Possible choices are 
        **<ins>triangular**, **<ins>epanechnikov**, **<ins>uniform** and **<ins>gaussian**.  By default, the program uses a uniform kernel.
        
**graph_off** by default, **did_had** outputs an event-study graph with the effect and placebo estimates and their confidence intervals. When specifying **graph_off**, the
        graph is suppressed.

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

## FAQ

## References

de Chaisemartin, C, D'Haultfoeuille, X (2024). [Two-way Fixed Effects and Difference-in-Difference Estimators in Heterogeneous Adoption Designs.
](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4284811).

Calonico, S., M. D. Cattaneo, and M. H. Farrell. 2019. [nprobust: Nonparametric Kernel-Based Estimation and Robust Bias-Corrected Inference
](https://nppackages.github.io/references/Calonico-Cattaneo-Farrell_2019_JSS.pdf). Journal of Statistical Software, 91(8): 1-33.

Calonico, S., M. D. Cattaneo, and M. H. Farrell. 2018. [On the Effect of Bias Estimation on Coverage Accuracy in NonparametricInference.
](https://nppackages.github.io/references/Calonico-Cattaneo-Farrell_2018_JASA.pdf). Journal of the American Statistical Association 113(522): 767-779.

## Auxiliary packages

The command requires that the [**lprobust**](https://github.com/nppackages/nprobust/tree/master) package be installed on the user's machine.

## Authors

Clément de Chaisemartin, Economics Department, Sciences Po, France.  
Xavier D'Haultfoeuille, CREST-ENSAE, France.  
Diego Ciccia, Economics Department, Sciences Po, France.  
Felix Knau, Economics Department, Sciences Po, France.   
Doulo Sow, CREST-ENSAE, France.  

**<ins>Contact:</ins>**  
[chaisemartin.packages@gmail.com](mailto:chaisemartin.packages@gmail.com)

