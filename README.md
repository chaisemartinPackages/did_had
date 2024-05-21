# did_had
Estimation of treatment effect in heterogeneous adoption designs.

[Short description](#Short-description) | [Vignettes](#vignettes) | [Setup](#Setup) | [Syntax](#Syntax) | [Description](#Description)

[Options](#Options) | [Example](#Example) | [FAQ](#FAQ) | [Auxiliary packages](#Auxiliary-packages) | [References](#References) | [Authors](#Authors)

## Short description

Estimates the effect of a treatment on an outcome in a heterogeneous adoption design with quasi stayers (see de Chaisemartin and D'Haultfoeuille (2024)).
 

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

**did_had** estimates the effect of a treatment on an outcome in a heterogeneous adoption design with quasi stayers. This design
        applies to scenarios where all groups start with a treatment level of 0 in the first period and then all groups increase
        their treatment level in the consecutive period. As all groups change their treatment between the two periods, it is
        impossible to make the "natural comparison" between groups that changed treatment and those that remained at 0 to estimate
        a treatment effect. To circumvent this, did_had implements the estimator from de Chaisemartin and D'Haultfoeuille (2024)
        which uses so called "quasi stayers" as controls. Those quasi stayers are groups that change their treatment only by a
        small amount and are therefore regarded "as good as untreated".  The command makes use of the lprobust command by Calonico,
        Cattaneo and Farrell (2018) to determine the bandwidth up to which groups are considered to be a quasi stayer. Then, an
        estimator for the treatment effect can be constructed as the difference between the change in outcome of all groups and the
        change of those quasi stayer groups, scaled by the average change in the treatment between the two periods.
        
The estimator also applies to scenarios with more than two periods. The general setting has to be the same (all groups
        start with a treatment of 0 and change their treatment at the same period), but in case the data tracks more than just two
        periods it is possible to estimate the effect of the treatment over the course of more than just one post-treatment period,
        or in case of more than one pre-treatment period, those can be used to placebo test Assumption 2 from de Chaisemartin and
        D'Haultfoeuille (2024) (and in case you have both multiple pre and post periods you can of course do both at the same
        time).

**Y** is the outcome variable.

**G** is the group variable.

**T** is the time period variable.

**D** is the treatment variable.

## Options

**effects(#)** allows you to specify the number of effects **did_had** tries to estimate. Each effect **ℓ** referes to the outcome
        evolution from the period right before each group gets treated to **ℓ** periods after this period (effect 1 is the
        instantaneous effect, effect 2 is the effect of reciving the treatment for 2 periods, and so on). By default, the command
        estimates only 1 effect and in case you specified more effects than your data allows to estimate the number of effects is
        automatically adjusted to the maximum.

**placebo(#)** allows you to specify the number of placebo estimates **did_had** tries to compute. Those placebos are symmetric to the
        way the effects are constructed, with the difference that they compare the outcome evolution from the period before all
        groups get treated to **ℓ** periods before this period when all groups were still untreated. Constructing this estimator using
        pre-treatment periods allows to placebo-test Assumption 2 from de Chaisemartin and D'Haultfoeuille (2024).

**level(#)** allows you to specify the level alpha for which the 1-alpha confidence intervals are valid. By default, this option is
        set to 0.05.
        
**kernel(string)** allows you to specify the kernel function used by lprobust to estimate the optimal bandwidth and mu. Possible
        choices are **<ins>triangular**, **<ins>epanechnikov**, **<ins>uniform** and **<ins>gaussian**.  By default, the program uses a uniform kernel.
        
**graph_off** by default, **did_had** outputs an event-study like graph with the effect estimates and the corresponding standard
        errors according to the numbers you specified in **effects(#)** and **placebo(#)**. When specifying **graph_off**, the production of
        this graph will be suppressed.

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

Calonico, S., M. D. Cattaneo, and M. H. Farrell (2018). [On the Effect of Bias Estimation on Coverage Accuracy in NonparametricInference.
](https://nppackages.github.io/references/Calonico-Cattaneo-Farrell_2018_JASA.pdf). Journal of Statistical Software, 91(8): 1-33. doi: [10.18637/jss.v091.i08.](https://www.jstatsoft.org/article/view/v091i08)

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

