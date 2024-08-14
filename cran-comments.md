## Test environments

* local R installation: R-4.4.1 (windows)
* mac OS (on github actions): R-release
* windows (on github actions): R-release
* ubuntu (on github actions): R-devel, R-release, R-oldrel-1

cf. https://github.com/larmarange/ggstats/actions/workflows/R-CMD-check.yaml

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 4 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
 
## recheck GitHub action

------- Check results summary ------
Check status summary:
                  OK
  Source packages  1
  Reverse depends  2

Check results summary:
ggstats ... OK
rdepends_GGally ... OK
rdepends_ggplot2.utils ... OK

------- Check for regressions ------
No changes between old and new version

cf. https://github.com/larmarange/ggstats/actions/workflows/recheck.yml

