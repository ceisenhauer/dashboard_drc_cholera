Dashboard : Measles in DRC <img src='www/logo.svg' align='right' alt='' width='200' />
====================================================================================================

<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
 <!--badges: end -->



Context
----------------------------------------------------------------------------------------------------
Surveillance dashboard to follow routine cholera surveillance data in the Democratic Republic of the Congo.


Immediate TODOs
----------------------------------------------------------------------------------------------------


CLARIFICATIONS
----------------------------------------------------------------------------------------------------

OTHER MUSINGS
----------------------------------------------------------------------------------------------------

NOTES
----------------------------------------------------------------------------------------------------
tidy table was for sures the way to go. dig this (calculated on 30 weeks of data across 407 health zones) :

*dplyr option* : 4.381 seconds
```
df %>%
      rowwise() %>%
      mutate(totalcas = .sum(across(starts_with('c'))),
             totaldeces = .sum(across(starts_with('d')))) 
```

*tidytable option* : 0.026 seconds
```
df %>%
      mutate_rowwise.(totalcas = .sum(c011mois, c1259mois, c5ansp),
                      totaldeces = .sum(d011mois, d1259mois, d5ansp)) 
```
