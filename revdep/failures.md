# bregr ()

* GitHub: <https://github.com/larmarange/ggstats>
* Email: <mailto:joseph@larmarange.net>

Run `revdepcheck::revdep_details(, "bregr")` for more info

## Error before installation

### Devel

```

  Des versions binaires sont disponibles mais les versions des sources
  sont plus récentes:
           binary source needs_compilation
DOSE        4.5.0  4.6.0             FALSE
enrichplot 1.31.2 1.32.0             FALSE
KEGGREST   1.51.1 1.52.2             FALSE
S4Vectors  0.50.1 0.50.2              TRUE

  Binaries will be installed
...
package 'waldo' successfully unpacked and MD5 sums checked
package 'withr' successfully unpacked and MD5 sums checked
package 'WRS2' successfully unpacked and MD5 sums checked
package 'xfun' successfully unpacked and MD5 sums checked
package 'xml2' successfully unpacked and MD5 sums checked
package 'xtable' successfully unpacked and MD5 sums checked
package 'XVector' successfully unpacked and MD5 sums checked
package 'yaml' successfully unpacked and MD5 sums checked
package 'yulab.utils' successfully unpacked and MD5 sums checked
package 'zoo' successfully unpacked and MD5 sums checked


installation des packages sources 'DOSE', 'enrichplot', 'GO.db', 'KEGGREST'

Error in (function (libdir, packages, quiet, repos)  : 
  all(packages %in% rownames(installed.packages(libdir[1]))) n'est pas TRUE
De plus : Messages d'avis :
1: Dans utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  l'installation du package 'DOSE' a eu un statut de sortie non nul
2: Dans utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  l'installation du package 'DOSE' a eu un statut de sortie non nul
3: Dans utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  l'installation du package 'GO.db' a eu un statut de sortie non nul
4: Dans utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  l'installation du package 'GO.db' a eu un statut de sortie non nul
5: Dans utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  l'installation du package 'enrichplot' a eu un statut de sortie non nul
6: Dans utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  l'installation du package 'enrichplot' a eu un statut de sortie non nul


```
### CRAN

```

  Des versions binaires sont disponibles mais les versions des sources
  sont plus récentes:
           binary source needs_compilation
DOSE        4.5.0  4.6.0             FALSE
enrichplot 1.31.2 1.32.0             FALSE
KEGGREST   1.51.1 1.52.2             FALSE
S4Vectors  0.50.1 0.50.2              TRUE

  Binaries will be installed
...
package 'waldo' successfully unpacked and MD5 sums checked
package 'withr' successfully unpacked and MD5 sums checked
package 'WRS2' successfully unpacked and MD5 sums checked
package 'xfun' successfully unpacked and MD5 sums checked
package 'xml2' successfully unpacked and MD5 sums checked
package 'xtable' successfully unpacked and MD5 sums checked
package 'XVector' successfully unpacked and MD5 sums checked
package 'yaml' successfully unpacked and MD5 sums checked
package 'yulab.utils' successfully unpacked and MD5 sums checked
package 'zoo' successfully unpacked and MD5 sums checked


installation des packages sources 'DOSE', 'enrichplot', 'GO.db', 'KEGGREST'

Error in (function (libdir, packages, quiet, repos)  : 
  all(packages %in% rownames(installed.packages(libdir[1]))) n'est pas TRUE
De plus : Messages d'avis :
1: Dans utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  l'installation du package 'DOSE' a eu un statut de sortie non nul
2: Dans utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  l'installation du package 'DOSE' a eu un statut de sortie non nul
3: Dans utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  l'installation du package 'GO.db' a eu un statut de sortie non nul
4: Dans utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  l'installation du package 'GO.db' a eu un statut de sortie non nul
5: Dans utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  l'installation du package 'enrichplot' a eu un statut de sortie non nul
6: Dans utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  l'installation du package 'enrichplot' a eu un statut de sortie non nul


```
# GGally (2.4.0)

* GitHub: <https://github.com/ggobi/ggally>
* Email: <mailto:schloerke@gmail.com>
* GitHub mirror: <https://github.com/cran/GGally>

Run `revdepcheck::revdep_details(, "GGally")` for more info

## Newly broken

*   checking whether package 'GGally' can be installed ... ERROR
     ```
     Installation failed.
     See 'C:/Users/josep/Documents/GitHub/ggstats/revdep/checks/GGally/new/GGally.Rcheck/00install.out' for details.
     ```

## Installation

### Devel

```
* installing *source* package 'GGally' ...
** this is package 'GGally' version '2.4.0'
** package 'GGally' successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Erreur : l'objet 'ggcoef_multinom' n'est pas exporté par 'namespace:ggstats'
Exécution arrêtée
ERROR: lazy loading failed for package 'GGally'
* removing 'C:/Users/josep/Documents/GitHub/ggstats/revdep/checks/GGally/new/GGally.Rcheck/GGally'


```
### CRAN

```
* installing *source* package 'GGally' ...
** this is package 'GGally' version '2.4.0'
** package 'GGally' successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (GGally)


```
