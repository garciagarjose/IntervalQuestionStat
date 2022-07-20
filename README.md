# **IntervalQuestionStat**

[![CRAN status](https://www.r-pkg.org/badges/version/IntervalQuestionStat)](https://cran.r-project.org/package=IntervalQuestionStat)
[![R build status](https://github.com/garciagarjose/IntervalQuestionStat/workflows/R-CMD-check/badge.svg)](https://github.com/garciagarjose/IntervalQuestionStat/actions)
[![Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/IntervalQuestionStat)](https://CRAN.R-project.org/package=IntervalQuestionStat)

### Tools to Deal with Interval-Valued Responses in Questionnaires

**IntervalQuestionStat** is an open source package for R.
It provides S4 classes and methods to deal with interval-valued responses in questionnaires.
It also includes some basic functions for doing the statistical analysis of this type of data.

In Social and Educational Sciences and many other disciplines, interval-valued 
scales arise as an alternative to traditional Likert-type or visual analogue scales
in some questionnaires measuring people's behaviour (attitudes, opinions, perceptions, feelings, etc.).
This type of data cannot be numerically measured because they concern intrinsically imprecise valued
attributes. Likert-type and visual analogue scales force to choose a single point response linked
to a statement or question, so individual differences are almost systematically overlooked.
To overcome the limitations of these traditional scales in capturing uncertainty of respondent answers,
interval-valued scales allow respondents to select a range or interval of real data and not being constrained to a single point.

The **IntervalQuestionStat** package aims to provide the following functionality:

* Calculation of basic operations with interval-valued data.
* Calculation of some central tendency and variation measures.
* Visualization of interval-valued data.
* Transformation of interval-valued responses into Likert-type or visual analogue responses.
* Statistical analysis of reliability of questionnaire's responses.
* Simulation of interval-valued responses in questionnaires.

* * *

Package record on **CRAN**:  https://CRAN.R-project.org/package=IntervalQuestionStat

* * *

**Author**: Jose Garcia Garcia, with contributions from Asun Lubiano.

To **cite** the **IntervalQuestionStat** package in publications please use:

> "García-García, J. (2022). *IntervalQuestionStat: Tools to Deal with
Interval-Valued Responses in Questionnaires*. R package version 0.1.0.
https://CRAN.R-project.org/package=IntervalQuestionStat."

A BibTeX entry for LaTeX users is:

```
@Manual{IntervalQuestionStatPkgR,
    title = {IntervalQuestionStat: Tools to Deal with Interval-Valued Responses in Questionnaires},
    author = {Jose {Garcia Garcia}},
    year = {2022},
    note = {R package version 0.1.0},
  }
```

## Installation

``` r
# Install from CRAN
install.packages("IntervalQuestionStat")

# Or the development version from GitHub
# install.packages("devtools")
devtools::install_github("garciagarjose/IntervalQuestionStat")
```

