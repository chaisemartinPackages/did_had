{smcl}
{* *! version 1  2024-05-21}{...}
{viewerjumpto "Syntax" "did_had##syntax"}{...}
{viewerjumpto "Description" "did_had##description"}{...}
{viewerjumpto "Options" "did_had##options"}{...}
{viewerjumpto "Examples" "did_had##examples"}{...}
{viewerjumpto "Saved results" "did_had##saved_results"}{...}

{title:Title}

{p 4 8}
{cmd:did_had} {hline 2} Estimates the effect of a treatment on an outcome in a heterogeneous adoption design
with no stayers but some quasi stayers (see de Chaisemartin and D'Haultfoeuille (2024)).
{p_end}

{marker syntax}{...}
{title:Syntax}

{p 4 8}
{cmd:did_had Y G T D} {ifin}
[{cmd:,}
{cmd:effects(#)}
{cmd:placebo(#)}
{cmd:level(#)}
{cmd:kernel(string)}
{cmd:yatchew}
{cmd:graph_off}]
{p_end}

{synoptset 28 tabbed}{...}

{marker description}{...}
{title:Description}

{p 4 8}
{cmd:did_had} estimates the effect of a treatment on an outcome in a heterogeneous adoption
design (HAD) with no stayers but some quasi stayers. HADs are designs where all groups are untreated in the first period, 
and then some groups receive a strictly positive treatment dose at a period F, 
which has to be the same for all treated groups (with variation in treatment timing, the did_multiplegt_dyn package may be used). 
Therefore, there is variation in treatment intensity, but no variation in treatment timing. 
HADs without stayers are designs where all groups receive a strictly positive
treatment dose at period F: no group remains untreated. Then, one cannot use untreated units
to recover the counterfactual outcome evolution that treated groups would have experienced from before to after F,
without treatment. To circumvent this, {cmd:did_had} implements the estimator
from de Chaisemartin and D'Haultfoeuille (2024) which uses so-called "quasi stayers" as the control group. 
Quasi stayers are groups that receive a "small enough" treatment dose at F to be regarded as "as good as untreated".
Therefore, {cmd:did_had} can only be used if there are groups with a treatment dose "close to zero".
Formally, the density of groups' period-two treatment dose needs to be strictly positive
at zero, something that can be assessed by plotting a kernel density estimate of that density. 
The command makes use of the {cmd:lprobust} command by Calonico, Cattaneo and Farrell (2019) to determine an optimal bandwidth, i.e.
a treatment dose below which groups can be considered as quasi stayers. 
To estimate the treatment's effect, the command starts by computing 
the difference between the change in outcome of all groups and the intercept 
in a local linear regression of the outcome change on the treatment dose 
among quasi-stayers. Then, that difference is
scaled by groups' average treatment dose at period two. Standard errors and confidence intervals are also computed leveraging {cmd:lprobust}. 
We recommend that users of {cmd:did_had} cite de Chaisemartin and D'Haultfoeuille (2024), Calonico, Cattaneo and Farrell (2019), and
Calonico, Cattaneo and Farrell (2018). 
{p_end}

{p 8 8}
{cmd:Y} is the outcome variable.
{p_end}

{p 8 8}
{cmd:G} is the group variable.
{p_end}

{p 8 8}
{cmd:T} is the time period variable.
{p_end}

{p 8 8}
{cmd:D} is the treatment variable.
{p_end}


{marker options}{...}
{title:Options}

{p 4 8}
{cmd:effects(}{it:#}{cmd:)} allows you to specify the number of effects {cmd:did_had} tries to estimate. Effect {cmd:ℓ} is the treatment's effect
at period F-1+{cmd:ℓ}, namely {cmd:ℓ} periods after adoption. By default, the command estimates only 1 effect and
in case you specified more effects than your data allows to estimate the number of effects is automatically adjusted to the maximum. 
{p_end}

{p 4 8}
{cmd:placebo(}{it:#}{cmd:)} allows you to specify the number of placebo estimates {cmd:did_had} tries to compute. Those placebos
are constructed symmetrically to the estimators of the actual effects, except that the outcome evolution from F-1 to F-1+{cmd:ℓ} in the 
actual estimator is replaced by the outcome evolution from F-1 to F-1-{cmd:ℓ} in the placebo.  
{p_end}

{p 4 8}
{cmd:level(}{it:#}{cmd:)} allows you to specify (1-the level) of the confidence intervals
shown by the command. By default this level is set to 0.05, 
thus yielding 95% level confidence intervals.
{p_end}

{p 4 8}
{cmd:kernel(}{it:string}{cmd:)} allows you to specify the kernel function used by {cmd:lprobust}. Possible choices are {opt tri:angular}, {opt epa:nechnikov}, {opt uni:form} and {opt gau:ssian}.
By default, the program uses a uniform kernel.
{p_end}

{p 4 8} 
{cmd:yatchew} yields the result from a non-parametric test 
that the conditional expectation of the F-1 to F-1+{cmd:ℓ} outcome evolution
given the treatment at F-1+{cmd:ℓ} is linear (Yatchew, 1997). 
This test is implemented using the heteroskedasticity-robust 
test statistic proposed in Section 3 of de Chaisemartin and D'Haultfoeuille (2024) 
and it is performed for all the dynamic effects and placebos 
computed by {cmd:did_had}. This option requires the {cmd:yatchew_test} 
package, which is currently available on SSC.
{p_end}

{p 4 8}
{cmd:graph_off:} by default, {cmd:did_had} outputs an event-study graph with the effect and placebo estimates and their confidence intervals. 
When specifying {cmd:graph_off}, the graph is suppressed.
{p_end}

{marker Example}{...}
{title:Example}{cmd:: Artificial data from the GitHub page} 

{p 4 4}
The data for this example can be downloaded by running:
{p_end}

{phang2}{stata ssc install did_had, replace}{p_end}
{phang2}use "https://raw.githubusercontent.com/chaisemartinPackages/did_had/main/tutorial_data.dta", clear{p_end}


{p 4 4}
Estimating the effects over five periods and placebos for four pre-treatment periods:
{p_end}

{phang2}{stata did_had y g t d, effects(5) placebo(4)}{p_end}


{p 4 4}
Doing the same estimation, but with a triangular kernel and surpressing the graph output:
{p_end}

{phang2}{stata did_had y g t d, effects(5) placebo(4) kernel(tri) graph_off}{p_end}


{p 4 4}
Changing the level of the confidence interval:
{p_end}

{phang2}{stata did_had y g t d, effects(5) placebo(4) level(0.1)}{p_end}

{title:References}

{p 4 8}
de Chaisemartin, C and D'Haultfoeuille, X (2024).
{browse "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4284811":Two-way Fixed Effects and Difference-in-Difference Estimators in Heterogeneous Adoption Designs}.
{p_end}

{p 4 8}
 Calonico, S., M. D. Cattaneo, and M. H. Farrell. 2019.
{browse "https://nppackages.github.io/references/Calonico-Cattaneo-Farrell_2019_JSS.pdf":nprobust: Nonparametric Kernel-Based Estimation and Robust Bias-Corrected Inference}. 
Journal of Statistical Software, 91(8): 1-33.
{p_end}

{p 4 8}
 Calonico, S., M. D. Cattaneo, and M. H. Farrell. 2018.
{browse "https://nppackages.github.io/references/Calonico-Cattaneo-Farrell_2018_JASA.pdf":On the Effect of Bias Estimation on Coverage Accuracy in Nonparametric Inference}. 
Journal of the American Statistical Association 113(522): 767-779.
{p_end}

{title:Auxiliary packages}

{p 4 4}
The command requires that the {cmd:lprobust} package be installed on the user's machine.
{p_end}

{title:Authors}

{p 4 4}
Clément de Chaisemartin, Economics Department, Sciences Po, France.
{p_end}
{p 4 4}
Diego Ciccia, Sciences Po, France.
{p_end}
{p 4 4}
Xavier D'Haultfoeuille, CREST-ENSAE, France.
{p_end}
{p 4 4}
Felix Knau, Sciences Po, France.
{p_end}
{p 4 4}
Doulo Sow, Sciences Po, France.
{p_end}

{title:Contact}

{p 4 4}
Mail:
{browse "mailto:chaisemartin.packages@gmail.com":chaisemartin.packages@gmail.com}
{p_end}

{p 4 4}
GitHub:
{browse "https://github.com/chaisemartinPackages/did_had"}.
{p_end}

{marker saved_results}{...}
{title:Saved results}

{p 4 8}
{cmd:{ul:Matrix}:}
{p_end}

{p 8 8}
{cmd:e(estimates)}: Matrix storing the results table.
{p_end}