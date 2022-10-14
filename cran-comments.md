## Test environments

* local R installation: R 4.2.1
* mac OS (on github actions): R-release
* windows (on github actions): R-release
* ubuntu 18.04 (on github actions): R-devel, R-release, R-oldrel-1

cf. https://github.com/larmarange/ggstats/actions

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

Following requests from CRAN:

* Value section has been added in documentation
* Examples have been updated, \dontrun{} is not used anymore,
  \dontttest{} is used when executable > 5 seconds
* No relevant external reference to be added in DESCRIPTION
