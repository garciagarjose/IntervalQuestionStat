# **NEWS about the IntervalQuestionStat package for R**

## **IntervalQuestionStat 0.2.0**

### *Breaking changes*

* `IntervalData` class `prototype` has been removed since, as it is indicated
  on the help page of `setClass()` function, *these arguments are currently
  allowed, but either they are unlikely to be useful or there are modern
  alternatives that are preferred*. This change motivates the simplification
  of `distance()` function by removing one of its checks.

* `IntervalData` class `representation` has been changed to `slots` argument
  since, as it is indicated on the help page of `setClass()` function, *these
  arguments are deprecated from version 3.0.0 of R and should be avoided*.
  
* `IntervalData`, `IntervalList`, and `IntervalMatrix` classes `validity` has
  been changed to `setValidity` since, as it is indicated on the help page of
  `setClass()` function, *these arguments are currently allowed, but either they
  are unlikely to be useful or there are modern alternatives that are
  preferred*.
  
* `IntervalList` and `IntervalMatrix` classes have been redefined in order to
  reduce the package's computational weight. In the previous version, their
  unique slot `.Data` was created by default as an object of class `list` or
  `matrix`, respectively, containing several `IntervalData` instances. Then,
  some of `list` or `matrix` methods were inherited by `IntervalList` and
  `IntervalMatrix` objects. Now, these two classes have two slots, named `mid`
  and `spr`, containing `numeric` vectors or `matrix` objects with the
  mid-points and the spreads of the intervals. It should be remarked that with
  this new implementation no `list` or `matrix` methods are inherited by
  `IntervalList` and `IntervalMatrix` objects.

* `age` variable from `lackinfo` data set has been removed. Therefore,
  the examples and tests that use this data have been modified in order
  to select the correct variables.

* `ivd2likert()` and `ivd2vas()` functions have been renamed as
  `ivs2likert()` and `ivs2vas()`, respectively.

* `ivd` argument in `cronbach()` function has been renamed as `ivs`.

* `simulIVS()` returns a `data.frame` object, not a `matrix` as in the previous
  package's version.

* `a1` and `a2` arguments in `IntervalData()` function are now forced to be
  single real numbers stored as unique `numeric` objects.
  
* `theta` argument in `cov()`, `cronbach()`, `distance()`, `ivs2likert()`, and
  `var()` functions is now forced to be a single positive real number stored
  as a unique `numeric` object.

* `bounds` and  `mid` arguments in `plot()` function's methods are now forced
  to be single `logical` objects.
  
* `ivs` argument in `cronbach()` function is now forced
  to be a single `logical` object.

* `k` argument in `ivs2likert()` function is now forced to be a single
  positive integer number stored as a unique `numeric` object.

* `minimum` and `maximum` arguments in `ivs2likert()` and `simulIVS()` functions
  are now forced to be single real numbers stored as single `numeric` objects.

* `n` and `k` arguments in `simulIVS()` function are now forced to be single
  positive integer numbers stored as single `numeric` objects.

* `w1`, `w2`, and `w3` arguments in `simulIVS()` function are now forced to be
  single real numbers in [0, 1] stored as single `numeric` objects.

* `p` and `q` arguments in `simulIVS()` function are now forced to be single
  non-negative real numbers stored as single `numeric` objects.

* `object` argument in `as.IntervalData()` function is now forced to be a single
  real number stored as a unique `numeric` value.
  
* `c()` function's methods have been redefined in a simpler way.

### *New features*

* `DESCRIPTION` file was updated as follows:
  - `Description` field was include a more detailed explanation of package's
    functionality and it also incorporates the links to the web pages of the
    two Research Groups, whose publications and investigations inspire the
    development of this package.
  - ORCID numbers from authors were added on `Authors@R:` and `Author` fields.
  - `BugReports`, `URL`, `Imports` and `Suggests` fields were added.
  - `RoxygenNote` field was updated to 7.2.1.
  - `Date` field was removed as it was suggested from `pkgcheck()` function from
    the **pkgcheck** package.
  - The **methods**, **grDevices**, **graphic**, and **stats** packages were
    moved from `Depends` field to `Imports` one.
  - The **testthat** and **vdiffr** packages were added on `Suggests` field.
  
* Due to the new definition of `IntervalList` and `IntervalMatrix` classes, it
  was necessary to create some new methods in order to calculate these objects
  sizes. In particular, `length()` function method was developed for
  `IntervalList` objects and `dim()`, `ncol()`, and `nrow()` methods were
  defined for `IntervalMatrix` objects.
  
* In order to provide customized and controlled access to the classes slots, two
  accessor functions, named `mid()` and `spr()`, were created. They return the
  information stored on `mid` and `spr` slots, respectively. They are a
  user-friendly alternative to the `@` accessor, since it does not force others
  to know the implementation details. Some replacement methods for these slots
  were defined through `mid<-` and "spr<-" commands.
  
* A new virtual class, named `IntervalDataOrIntervalList`, was created as the
  union of `IntervalData` and `IntervalList` classes only for internal purposes
  such as defining method signatures when several objects of these two classes
  are supplied.

* A new virtual class, named `IntervalListOrIntervalMatrix`, was created as the
  union of `IntervalList` and `IntervalMatrix` classes only for internal purposes
  such as defining method signatures when several objects of these two classes
  are supplied.

* The `c()` function's methods for combining `IntervalData` and `IntervalList`
  instances was updated to the new signature `IntervalDataOrIntervalList`.

* The `rbind()` and `cbind()` functions were created in order to combine
  `IntervalList` and `IntervalMatrix` instances by rows and columns,
  respectively, through the new signature `IntervalListOrIntervalMatrix`.

* The `arithmetic` methods were extended to the `IntervalList` and
  `IntervalMatrix` classes by allowing to carry out the implemented operations
  element by element.

* The `simulIVS` function's code was revised in order to reduce its execution
  time.

* The `extract` methods were modified in order to allow to use negative integers
  for indicating the elements to leave out of the selection. Some methods were
  defined in order to allow to replace parts of `IntervalList` and
  `IntervalMatrix` objects too. In particular, `[<-` and `[[<-` commands were
  included.

* Tests for code lines were developed though the **testthat** and **vdiffr**
  (used specifically for `plot` methods) packages. These tests provide a 100%
  coverage as it was calculated by `package_coverage()` function from *covr*
  package.
  
* Some good practice comments given by `pkgcheck()` function from the
  **pkgcheck** package motivated the following changes in the previous code:
  - All code lines were adjusted in order to not exceed the common length
    of 80 characters.
  - Code expressions like `1:nrow(data)`, `1:length(x)`, ... were avoided.
    Function `seq_len()` was used instead.
  - Cyclomatic complexity of the objects in this package was reduced by creating
    some subfunctions in order to do some repeated checks. In particular,
    `is_single_positive_integer()`, `is_single_positive_real_number()`,
    `is_single_real_number()`, and `is_single_proportion` were created for this
    purpose.
  - Calls to `sapply()` functions were avoided. Function `vapply` was used
    instead. 

* Some function's documentations were corrected and improved, including more
  detailed examples.

* DOIs were added to references (whenever they were found).

* Both `README.md` and `NEWS.md` files were also incorporated to the package.

* A logo for the **IntervalQuestionStat** package was also designed and included
  in the `man/figures/` directory.

### *Bug fixes*

* Colors were not used in the correct way in `plot()` function's methods defined
  for `IntervalData` and `IntervalList` objects, specially when the intervals
  lower and upper bounds or their mid-points were remarked. Therefore, this bug
  was fixed by changing the expression
  `dotarguments <- match.call(expand.dots = FALSE)$...` to
  `dotarguments <- list(...)`.

* Some default main and axis labels were misspelled in `plot()` function's
  methods, so they were corrected.

* The `xaxt` and `yaxt` arguments from `plot()` function's method for
  signature `IntervalData,IntervalData` were removed so the axis numbers
  are shown.

## **IntervalQuestionStat 0.1.0**

Initial release of the **IntervalQuestionStat** package on the Comprehensive
**R** Archive Network (**CRAN**). 
