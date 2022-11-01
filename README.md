
# **IntervalQuestionStat**

<img src="man/figures/logo.png" align="right" height="139" />

## Tools to Deal with Interval-Valued Responses in Questionnaires

[![CRAN status](https://www.r-pkg.org/badges/version/IntervalQuestionStat)](https://cran.r-project.org/package=IntervalQuestionStat)
[![Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/IntervalQuestionStat)](https://CRAN.R-project.org/package=IntervalQuestionStat)

### Description
**IntervalQuestionStat** is an open source package for doing the statistical
analysis of interval-valued responses collected through interval-valued scales
in lots of different widely used questionnaires measuring many intrinsically
imprecise human attributes in **R** environment. In particular, this
package implements the theoretical concepts, results, and ideas suggested by the
SMIRE+CoDiRE (Statistical Methods with Imprecise Random Elements and Comparison
of Distributions of Random Elements) Research Group
(https://bellman.ciencias.uniovi.es/smire+codire/) from the University of Oviedo
(Spain) taking into account some applied investigations and real-life studies.

In Social and Educational Sciences and many other disciplines,
interval-valued scales arise as a strong alternative to both traditional
Likert-type or visual analogue scales in some questionnaires measuring
people's behavior (attitudes, opinions, perceptions, feelings, etc.).
This type of data can not be directly measured because it concerns inherently
imprecise features. Likert-type and visual analogue scales force respondents
to choose single-point answers linked to some items (statements or
questions), so individual differences are almost systematically overlooked.
In order to overcome the limitations of these scales in capturing uncertainty
over respondents' answers, interval-valued scales allow them to select a
real-valued interval and not being constrained to a single point.
 
This package provides S4 classes, methods, and functions for doing the
statistical analysis of interval-valued responses in questionnaires measuring
imprecise traits from both descriptive an inferential points of view. In
particular, it aims to provide the the following functionality:

* Definition of interval-valued data objects and instances.
* Calculation of basic operations with interval-valued data.
* Calculation of some central tendency and variation measures.
* Visualization of interval-valued data.
* Association of interval-valued responses and Likert-type or visual analogue answers
* Statistical analysis of reliability of questionnaire's responses.
* Simulation of interval-valued responses in questionnaires.

Finally, it should also be remarked that the **IntervalQuestionStat** package
includes some real-life data sets.

### Authors
José García-García, with contributions from María Asunción Lubiano.

### Installation
To install the **IntervalQuestionStat** package in **R** environment please use:

``` r
# Install from CRAN
install.packages("IntervalQuestionStat")

# Or the development version from GitHub
# install.packages("devtools")
devtools::install_github("garciagarjose/IntervalQuestionStat")
```

### Citation
To cite the **IntervalQuestionStat** package in publications please use:

> "García-García, J. (2022). ***IntervalQuestionStat**: Tools to Deal with
Interval-Valued Responses in Questionnaires*. R package version 0.2.0.
https://CRAN.R-project.org/package=IntervalQuestionStat."

A BibTeX entry for LaTeX users is:

```
@Manual{IntervalQuestionStatPkgR,
    title = {IntervalQuestionStat: Tools to Deal with Interval-Valued Responses
             in Questionnaires},
    author = {Jos{\'e} Garc{\'i}a-Garc{\'i}a},
    year = {2022},
    note = {R package version 0.2.0},
  }
```

### See also
For further information please visit the SMIRE+CoDiRE (Statistical Methods with
Imprecise Random Elements and Comparison of Distributions of Random Elements)
Research Group's webpage at https://bellman.ciencias.uniovi.es/smire+codire/
