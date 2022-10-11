#' Clean MSF vaccine activities data
#'
#' @description Cleans up MSF activities data. **WARNING** this code assumes that the dataset is already clean-ish and comes as a csv; you will also probably need to check regularly if the zone names match the MSF standard. This function also has **opinionated* expectations of column names.
#'
#' @param data_file `chr` Name of file.
#' @param data_root `chr` Name of directory path for the file.
mod_prepare_activites_msf <- function(data_file = 'vax_msf.csv',
                                      data_root = here::here('data', 'activity')) {

  fn <- paste0(data_root, data_file)
  
  acts <- rio::import(fn,
                      select = c('oc', 'reg', 'zone', 'aires', 'vax_start', 'ages', 'vax_n')) %>%
            mutate.(vax_start = as.Date(vax_start,
                                        format = '%d/%m/%Y'),
                    #ages = stringr::str_replace(ages, ' et ', '\n'),
                    vax_n = stringr::str_replace(vax_n, '\\.', ''),
                    vax_n = as.numeric(vax_n))
  
  acts[,'reg'] <- acts[,'reg'] %>% linelist::clean_data()
  acts[,'zone'] <- acts[,'zone'] %>% linelist::clean_data()

  fixes <- list('kiambi' = 'kiyambi',
                'kimbi_lulenge' = 'lulenge_kimbi',
                #'baraka' = '',
                'salambila' = 'saramabila',
                'mutchatcha' = 'mutshatsha',
                'kanama' = 'kaniama')
  
  acts <- acts %>%
          mutate.(zone = dplyr::recode(zone, !!!fixes),
                  kprov_zone = paste0(reg, '#', zone))
}
