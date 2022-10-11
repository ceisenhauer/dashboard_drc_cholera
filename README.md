Dashboard : Measles in DRC <img src='www/logo.svg' align='right' alt='' width='200' />
====================================================================================================

<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
 <!--badges: end -->



Context
----------------------------------------------------------------------------------------------------
Measles remains a substantial burden in DRC and seasonal outbreaks occur every year and potentially substantial epidemics every 2-4 years. The initial surveillance data of 2022 suggests that DRC may be entering into a new measles epidemic.

**The present repository provides a basic dashboard designed to facilitate visualization and managment of the current epidemic situation.** 

Immediate TODOs
----------------------------------------------------------------------------------------------------

- [x] clean IDS history
- [x] clean lab history ? (maybe doesn't exist)
- [x] clean GIS --> packed into `epimaps`
- [x] mod : prep ids
- [x] mod : prep lab
- [x] port maps to epiplaces
    - [x] port lightweight versions...and jsons ?
- [x] convert everything to echarts
- [x] swap to bar plots ---- js 4 dayz it should be so easy...
- [ ] map page
    - [ ] filter map by province(s)
    - [ ] prov or hz
        - this can probably be done using some sort of id combination thing...ie div id = 'level+indicator' where indicator radios pull the toggle id and paste to get the correct div to show
    - [ ] add ripost pins
    - [ ] timeline ?
- [ ] html drop down links
- [x] data download on all plots
- [x] table upgrades
    - [x] external drop down (multiple select) filter
    - [x] column to select by section
    - [x] other filters ?
    - [x] download as csv -- but skip sparkline somehow
    - [x] hover over column headers to get definitions / sources
- [ ] downloads
    - formatted prevention / riposte table
    - dashboard
    - sitrep
- [x] info page
- [ ] map add lines for regions somehow


for later times : 
- [x] add in send results date to lab data
- [x] use imputation in lab data
- [x] annonymize lab data
- [ ] pipe to send data automatically
- [x] sharepoint for data


CLARIFICATIONS
----------------------------------------------------------------------------------------------------

OTHER MUSINGS
----------------------------------------------------------------------------------------------------
- try with echarts? ---> *awesome*
    - but no filtering / cross talk... probably need to stay with plotly
- indicators to test : epidemic phase
    - r from epiestim
    - r from flavio's method
    - r on smooth ?
    - linear trend
    - exponential trend
- indicators to evaluate :
    - fit years since epidemic + epi size + interventions = outbreak risk
- make a general all cause version of this dashboard


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
