## Test environments

* local R installation: R-devel (windows)
* mac OS (on github actions): R-release
* windows (on github actions): R-release
* ubuntu (on github actions): R-devel, R-release, R-oldrel-1

cf. https://github.com/larmarange/ggstats/actions

## Comments

X-CRAN-Comment: Archived on 2023-11-14 as issues were not corrected
      in time.
      
The problem was due to the presence of a Unicode character in the
labels of some graphs. This Unicode character has been removed
and replaced by Latin-1 characters.
