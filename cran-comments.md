## Test environments

* local R installation: R-4.5.2 (windows)
* mac OS (on github actions): R-release
* windows (on github actions): R-release
* ubuntu (on github actions): R-devel, R-release, R-oldrel-1

cf. https://github.com/larmarange/ggstats/actions/workflows/R-CMD-check.yaml

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 6 reverse dependencies (5 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
 
## recheck GitHub action

------- Check results summary ------
Check status summary:
                  OK
  Source packages  1
  Reverse depends  6

Check results summary:
ggstats ... OK
rdepends_GGally ... OK
rdepends_bregr ... OK
rdepends_broom.helpers ... OK
rdepends_ggplot2.utils ... OK
rdepends_gtsummary ... OK
rdepends_guideR ... OK

------- Check for regressions ------
No changes between old and new version

cf. https://github.com/larmarange/ggstats/actions/workflows/recheck.yml

