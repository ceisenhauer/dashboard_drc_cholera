#' Add Outbreaks to a DataFrame
#' 
#' @description Takes a df of case health zone level case data and adds three new columns: outbreak
#' (bool), outbreak_id (str), and finished (bool). Outbreaks are identified using surveillance data
#' since 2015. A csv of outbreak statistics can be optionally saved. *WARNING* Function is 
#' opinionated and requres specific column naming.
#'
#' @param df `df` Dataframe of case data.
#' @param history_path `chr` Filepath to find historical (before 2019) surveillance data. Default is
#'   data/static_data/historique_rougeole_2015_2018_clean.csv (relative to the directory root).
#' @param save_path `chr` Filepath to use when saving outbreak statistics. Default is 
#'   out/outbreaks.csv. NB, this argument is only used when `save` is `TRUE`.
#' @param save `bool` Whether to save the statistics of the identified outbreaks. Default is `TRUE`.
#'
#' @import tinker
#' @import tidytable
#' @importFrom here here
#' @importFrom rio import export
mod_add_outbreaks <- function(df, ..., 
                              history_path = here::here('data', 'static_data',
                                                      'historique_rougeole_2015_2018_clean.csv'),
                              save_path = here::here('out', 'outbreaks.csv'), save = TRUE) {

  # ID OUTBREAKS SINCE 2015 ------------------------------------------------------------------------ 
  writeLines('identifying outbreaks...')
  df_hist <- rio::import(history_path,
                         select = c('prov', 'zs', 'debutsem', 'totalcas', 'totaldeces'),
                         col.names = c('reg', 'zone', 'date', 'cases', 'deaths'),
                         colClasses = c(debutsem = 'Date')) %>%
               mutate(year = lubridate::isoyear(date),
                      reg_zone = paste0(reg, '#', zone)) %>%
               bind_rows.(df) %>%
               select.(-cases_over_5, -pop) %>%
               mutate.(year = lubridate::isoyear(date),
                       across.(c(cases, deaths), ~ tidyr::replace_na(., 0))) %>%
               arrange.(reg, zone, date) %>%
               #group_by(reg, zone) %>%
               identify_outbreaks()

  # OUTBREAK STATS ---------------------------------------------------------------------------------
  outbreaks <- df_hist %>%
    filter.(outbreak_id != '') %>%
    mutate.(date = as.Date(date)) %>%
    summarize.(zone = unique(zone),
               reg = unique(reg),
               reg_zone = unique(reg_zone),
               year_start = .min(year),
               year_end = .max(year),
               start = .min(date),
               end = .max(date),
               size = .sum(cases),
               deaths = .sum(deaths),
               duration = n(),
               finished = unique(finished),
               .by = outbreak_id) %>%
    mutate.(recent = end >= .max(df$date) - 90)

  if (save) {
    writeLines('saving outbreak statistics...')
    rio::export(outbreaks,
                save_path)
  }

  # FILTER OUT HISTORICAL DATA ---------------------------------------------------------------------
  df <- df_hist %>%
          select.(reg, zone, date, outbreak, outbreak_id, finished) %>%
          right_join.(df,
                      by = c('reg', 'zone', 'date'))

  return(df)
}
