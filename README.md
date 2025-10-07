<!-- badges: start -->
[![Version](https://img.shields.io/github/v/release/rapporteket/rapRegTemplate?sort=semver)](https://github.com/rapporteket/rapRegTemplate/releases)
[![R build status](https://github.com/rapporteket/rapRegTemplate/workflows/R-CMD-check/badge.svg)](https://github.com/rapporteket/rapRegTemplate/actions)
[![Codecov test coverage](https://codecov.io/gh/Rapporteket/rapRegTemplate/branch/main/graph/badge.svg)](https://codecov.io/gh/Rapporteket/rapRegTemplate?branch=main)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![GitHub open issues](https://img.shields.io/github/issues/rapporteket/rapRegTemplate.svg)](https://github.com/rapporteket/rapRegTemplate/issues)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Doc](https://img.shields.io/badge/Doc--grey.svg)](https://rapporteket.github.io/rapRegTemplate/)
<!-- badges: end -->
  
# Rapporteket for Deformitet <img src="man/figures/logo.svg" align="right" height="150" />
Deformitet er et under-register i Nasjonalt kvalitetsregister for ryggkirurgi. Dette er R-pakken som lager Rapporteket til Deformitet. 

## Last ned
Pakken kan lastes ned direkte i R. Installer først devtools-pakken (hvis du ikke allerede har den installert).


```r
install.packages("devtool")
```

Deretter installerer du rapbase (en R-pakke med basisfunksjoner til Rapportek):

```r
devtools::install_github("Rapporteket/rapbase")
```

Deretter installerer du denne pakken: 

```r
devtools::install_github("Rapporteket/deformitet")
```

## Utviklingsbidrag
Dersom du ønsker å bidra til denne pakken kan du legge til kode i greina "utvikling".
