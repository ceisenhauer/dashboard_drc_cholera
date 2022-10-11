#' Clean and Save IDS Surveillance Data
#'
#' @description Loads and cleans IDS data with the option to save the cleaned output to a file in 
#'   addition to loading it into the environment.
#'
#' @param data_file `str` File to be loaded and cleaned.
#' @param data_root `str` Directory where data is stored. Default is 'data'.
#' @param year `int` Year to associate with the data in `data_file`. Default is the current year.
#' @param add_history `bool` If `TRUE`, historic IDS data (from 2019) will be added included in the
#'   output. Default is `TRUE`.
#' @param save `bool` If `TRUE`, the output data will be saved to a file. Default is `TRUE`.
#' @param save_path `str` Path to be used when saving the output data (if `save` is `TRUE`). Default
#'   is 'current_clean.csv'.
#'
#' @return df
#'
#' @import tidytable
#' @importFrom dplyr recode
#' @importFrom tinker iso_to_date #
#' @importFrom here here
#' @importFrom Hmisc mdb.get
#' @importFrom linelist clean_data
#' @importFrom rio ipmort export
#' @importFrom tidyr complete
mod_prepare_IDS <- function(data_file = NULL, data_root = here::here('data', 'ids'),
                            year = as.numeric(format(Sys.Date(), '%Y')), max_week = NULL,
                            add_history = TRUE, save = TRUE, 
                            save_path = here::here('out', 'current_clean.csv')) {

  # LOAD -------------------------------------------------------------------------------------------
  fn <- here::here(data_root, data_file)
  ext <- tools::file_ext(data_file)

  # extension specific import, throw warning if type is unrecognized
  if (ext == 'MDB') {
    df <- Hmisc::mdb.get(fn) 
  } else if (ext == 'xlsx') {
    df <- rio::import(fn)
  } else {
    warning(pate0('Unrecognized file type for IDS data. Expected MDB or xlsx but found ', ext,
                  '.\nAttempting to import with rio...'))
    df <- rio::import(fn)
  }



  # WRANGLE ----------------------------------------------------------------------------------------

  # SELECT (AND LOWERCASE ASCII ONLY PLEASE) -----
  df <- df %>%
          filter.(MALADIE == 'ROUGEOLE') %>%
          linelist::clean_data() %>%
          mutate.(across.(contains(c('mois', 'ans')), as.numeric))
        

  # NB: starting in 2021 data splits 5-15 years old and 15+ into seperate categories but our
  # analysis will still keep them as a single 5+ year old group 
  if (year > 2020) {
    df <- df %>%
            select.(prov, zs, pop, numsem, c011mois, d011mois, c1259mois, d1259mois,
                    c515ans, d515ans, cp15ans, dp15ans)
  } else {
    df <- df %>%
            select.(prov, zs, pop, numsem, c011mois, d011mois, c1259mois, d1259mois,
                    c5ansp, d5ansp) 
  }

  # ADD INDICATORS -----
  # if new data, combine 5-15 and 15+ age brackets then calc total cases
  if (year > 2020) {
    df <- df %>%
            mutate_rowwise.(c5ansp = .sum(c515ans, cp15ans),
                            d5ansp = .sum(d515ans, cp15ans)) %>% 
            select.(!ends_with('ans'))
  }

  df <- df %>%
          mutate_rowwise.(totalcas = .sum(c011mois, c1259mois, c5ansp),
                          totaldeces = .sum(d011mois, d1259mois, d5ansp)) 
                                

  # CLEAN -----
  # remove observations with dates in the future
  # TODO: log file with any entries that were removed, this would be great but is low priority as
  # we haven't seen any futuredate issues 
  df <- df %>%
          filter.(numsem <= 53)

  if (year == lubridate::year(Sys.Date())) {
    df <- df %>%
            filter.(numsem <= lubridate::week(Sys.Date()))
  }


  # name spelling issues
  fixes <- list('bafwabgobgo' = 'bafwagbogbo',
                'mambassa' = 'mambasa',
                'ruzizi03' = 'ruzizi',
                'shabunda_centre' = 'shabunda',
                'gbado_lite' = 'gbadolite',
                'mobayi_mbongo15' = 'mobayi_mbongo',
                'mobayi_mbogo' = 'mobayi_mbongo',
                'mobatyi_mbongo' = 'mobayi_mbongo',
                'mobayi_ubangi' = 'mobayi_mbongo',
                'waosolo' = 'wasolo',
                'was0lo' = 'wasolo',
                'yalifafo' = 'yalifafu',
                'gadolite' = 'gbadolite',
                'gbadlite' = 'gbadolite',
                'gbadplite' = 'gbadolite',
                'gbadplite' = 'gbadolite',
                'wapida' = 'wapinda',
                'bena_tshadi' = 'bena_tshiadi',
                'bosbolo' = 'bosobolo',
                'djalo_djeka' = 'djalo_ndjeka',
                'omendjadi' = 'omondjadi',
                'kalonda' = 'kalonda_ouest',
                'benetshiadi' = 'bena tshiadi',
                'bobobzo' = 'bobozo',
                'bukonde' = 'bunkonde',
                'bunkone' = 'bunkonde',
                'lubobdaie' = 'lubondaie',
                'muetshi' = 'mwetshi',
                'shikula' = 'tshikula',
                'yanfgala' = 'yangala',
                'omendjadi' = 'omondjadi',
                'dibele' = 'bena_dibele',
                'kiambi' = 'kiyambi',
                'mbulula' = 'mbulala')

  df <- df %>%
          mutate.(zs = dplyr::recode(zs, !!!fixes),
                  prov_zs = paste0(prov, '#', zs))


  # MAKE ZONES AND DATES WITH NO DATA EXPLICIT -----
  # build and save df of most recent population data available
  pops <- df %>%
            summarize.(pop = last.(na.omit(pop)),
                       .by = c(prov_zs, prov, zs))

  pops <- readRDS(here::here('data', 'reference', 'population_data.RDS')) %>%
            filter.(prov_zs %notin% pops$prov_zs) %>%
            bind_rows.(pops) %>%
            mutate.(numsem = 1)

  saveRDS(pops, here::here('data', 'reference', 'population_data.RDS'))

  # expand dates and add date for start of week
  max_week <- ifelse(is.null(max_week), max(df$numsem, na.rm = TRUE), max_week)
  missing_zones <- pops %>%
                     filter.(prov_zs %notin% unique(df$prov_zs))

  df <- df %>%
          bind_rows.(missing_zones) %>%
          complete.(numsem = seq(1, max_week),
                    .by = c(prov, zs, prov_zs))

  # if year is already in df, keep it
  # this requires work on max week stuff
  #if ('annee' %in% names(df)) {
    #df <- df %>%
            #rename(annee = 'year')
  #} else {
    #df <- df %>%
            #rename(year = year)
  #}

  df <- df %>%
          mutate.(#year = year,
                  debutsem = tinker::iso_to_date(week = numsem,
                                                 year = year),
                  pop = last.(na.omit(pop)),
                  .by = c(prov, zs, prov_zs))


  # REMOVE DUPLICATES -----
  # when rows have duplicated id columns (province, zone, date) but conflicting case / death data,
  # choose the row with the highest number of total cases, then total deaths, then cases over 5, etc
  # TODO: save duplicate entries to a log file so that they can be reported to the national
  # surveillance system for correction -- consider splitting these into full duplicates and rows 
  # that duplicate the id columns (province, health zone, date) but have different case / death data
  df <- df %>%
          arrange.(totalcas, totaldeces, c5ansp, d5ansp, c1259mois, d1259mois, c011mois,
                   d011mois) %>%
          distinct.(prov, zs, debutsem, .keep_all = TRUE) %>%
          arrange.(prov, zs, debutsem)

  # VALIDATE -----
	# realistic dates
	# no missing zones / provinces
	# all zones present with correct number of observations
  # pops should be unique (ex. more than a handful of zones w/ same pop is sus)
  # no duplicate entries (wrt prov zone date)
  # no missing data for year, week, date
  


#assert("T is bad for TRUE, and so is F for FALSE", {
    #T = FALSE
    #F = TRUE
    #(T != TRUE)  # note the parentheses
    #(F != FALSE)
#})


  # ADD HISTORY ------------------------------------------------------------------------------------
  if (add_history) {
    df <- readRDS(here::here('data', 'reference', 'IDS_history.RDS')) %>%
            bind_rows(df) %>%
            arrange.(prov, zs, debutsem)
  }
          

  # SAVE -------------------------------------------------------------------------------------------
  if (save) {
    rio::export(df, save_path)
  }

  
  # RETURN -----------------------------------------------------------------------------------------
  return(df)
}
