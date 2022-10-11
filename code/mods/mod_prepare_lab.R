#' Clean and Save Lab Data
#'
#' @description Loads and cleans lab data with the option to save the cleaned output to a file in 
#'   addition to loading it into the environment. TODO: improve date cleaning for dates too far in
#'   the past.
#'
#' @param data_file `str` File to be loaded and cleaned.
#' @param data_root `str` Directory where data is stored. Default is 'data'.
#' @param year `int` Year to associate with the data in `data_file`. Default is the current year.
#' @param add_history `bool` If `TRUE`, historic IDS data (from 2021) will be added included in the
#'   output. Default is `FALSE`.
#' @param save `bool` If `TRUE`, the output data will be saved to a file. Default is `TRUE`.
#' @param save_path `str` Path to be used when saving the output data (if `save` is `TRUE`). Default
#'   is 'current_clean.csv'.
#'
#' @return df
#'
#' @import dplyr
#' @import tinker
#' @importFrom mice mice complete
mod_prepare_lab <- function(data_file = NULL, data_root = here::here('data', 'lab'),
                            type = c('inrb', 'labolushi'),
                            impute_dates = TRUE, add_history = TRUE,
                            save = TRUE, save_path = here::here('out', 'current_clean.csv')) {

  writeLines(paste0('processing ', data_file, '...'))

  # LOAD -------------------------------------------------------------------------------------------
  type <- match.arg(type)
  skip <- ifelse(type == 'inrb', 10, 7)

  fn <- here::here(data_root, data_file)
  df <- rio::import(fn,
                    skip = skip) %>%
          linelist::clean_data() %>%
          select(!contains(c('naissance', 'nom', 'nature', 'etat', 'pev', 'sem', 'reponse')))

  # add placeholder column for result send date if none exists
  if (!any(grepl('date.*result', names(df)))) {
    df <- df %>%
            mutate(date_envoi_resultats = NA)
  }

  names(df) <- c('n_labo', 'n_epid', 'zs', 'age_ans', 'age_mois', 'sexe', 
                 'date_derniere_vaccination', 'date_debut_eruptions', 'date_collecte_echant', 
                 'date_envoi_echant', 'date_reception_au_labo', 'resultats_igm_rougeole',
                 'resultats_igm_rubeole', 'date_envoi_resultats')

  # remove non-data trailing rows
  df <- df %>%
          filter(grepl('\\d{2}rou\\d{4}', n_labo))

  # WRANGLE ---------------------------------------------------------------------------------------- 
  # fix name issues and missig data
  fixes <- list('bambu_mines' = 'bambu',
                'bili2' = 'bili',
                'bipemba' = 'bimpemba',
                'bosomodanda' = 'bosomondanda',
                'djalo_ndjeka' = 'djalo_djeka',
                'gbado_lite' = 'gbadolite',
                'kabeya_kamwanga' = 'kabeya_kamuanga',
                'kalambayi_kabanga' = 'kalambayi_kaban',
                'kalonda_ouest' = 'kalonda',
                'kamonia' = 'kamonya',
                'kamuesha' = 'kamwesha',
                'kasongolunda' = 'kasongo_lunda',
                'kimbau' = 'kimbao',
                'lubunga2' = 'lubunga',
                'lubondaie' = 'lubondayi',
                'lukalenge' = 'lukelenge',
                'miti_murrhesa' = 'miti_murhesa',
                'makiso_kisangani' = 'makiso_kisangan',
                'mobayi' = 'mobayi_mbongo',
                'muetshi' = 'mwetshi',
                'omendjadi' = 'omondjadi',
                'salamabila' = 'saramabila',
                'sekebanza' = 'seke_banza',
                'wamba_luadi' = 'wamba_lwadi',
                'kimbi_lulenge' = 'lulenge_kimbi',
                'lolanga_mampoko' = 'mampoko',
                'mwene_ditu' = 'muene_ditu',
                'nyankunde' = 'nyakunde',
                'lubondayi' = 'lubondaie',
                'mambassa' = 'mambasa',
                'nsona_pangu' = 'nsona_mpangu',
                'shabunda_centre' = 'shabunda',
                'banjow_moke' = 'bandjow_moke',
                'kuimba' = 'kwimba',
                'ntand_embelo' = 'ntandembelo')

  df <- df %>%
          mutate(zs = recode(zs, !!!fixes),
                 across(everything(), ~ replace(., as.character(.) == 'missing', NA)),
                 across(starts_with('date'), excel_to_date),
                 across(c(age_ans, age_mois), as.numeric))
  

  # get provinces and add concenience prov_zs column
  prov_codes <- rio::import(here::here('data', 'reference', 'prov_codes.csv')) %>%
                  select(code, prov)

  df <- df %>%
          mutate(code = substr(n_epid, 5, 7)) %>%
          left_join(prov_codes,
                    by = 'code') %>%
          select(-code) %>%
          mutate(prov_zs = paste0(prov, '#', zs))

  # remove samples still being processed, use grep to capture multiple spellings of "en cours'
  df <- df %>%
          filter(!if_any(starts_with('resultats'), ~ grepl('ours', .)))

  # fix coding issues in results, ensure gender is coded as m/f
  df <- df %>%
          mutate(across(starts_with('resultats'),
                        ~ case_when(. == 'positif' ~ 'positif',
                                    . == '1' ~ 'positif',
                                    . == 'negatif' ~ 'negatif',
                                    . == '2' ~ 'negatif',
                                    TRUE ~ '')),
                  sexe = case_when(sexe == 'f' ~ 2,
                                   sexe == 'm' ~ 1,
                                   sexe == '1' ~ 1,
                                   sexe == '2' ~ 2))

  # remove duplicates
  df <- df %>%
          arrange.(desc(resultats_igm_rougeole)) %>% 
          distinct(n_labo,
                   .keep_all = TRUE)
          #janitor::remove_empty(which = 'rows')

  new_observations <- unique(df$n_labo)


        
  # ANONYMIZE --------------------------------------------------------------------------------------
  # we are creating anonymized hashes using a variety of info (including subject name). this is 
  # necessary to ensure do not introduce duplicate observations when we update the data. these 
  # hashes are also stored as a list to help filter later as needed (ie if we add historical data
  # for imputation but only want to return new values)
  #df <- df %>%
          #mutate(id = paste(n_labo, nom, age_ans, age_mois, date_reception_au_labo, sep = '_')) %>%
          #mutate_rowwise.(hash = digest::digest(id)) %>%
          #select(-nom, -id)

  #new_hashes <- df$hash



  # HANDLE DATES -----------------------------------------------------------------------------------
  # replace future dates (relative to arrival at lab) with NA, convert dates to day counts
  # here date_collecte_imput is added as an empty column for compatability with datasets that used
  # date imputation
  df <- df %>%
         mutate.(across.((starts_with('date') & !contains('labo')),
                       ~ as.numeric(date_reception_au_labo - .)),
                across.((starts_with('date') & !contains('labo')),
                       ~ ifelse(. < 0, NA, .)),
                date_collecte_echant_imput = NA)
  
  df <- df %>%
          mutate(year = NA_integer_,
                 numsem = NA_integer_,
                 debutsem = as.Date(NA))

  # HANDLE IMPUTATION AND HISTORY ------------------------------------------------------------------
  # the logic gates here are slightly complicated; here's a breakdown :
  # impute_dates + add_history ----> bind (the new part of) df to historical data and impute missing
  #                                  sample dates (if not already imputed in the history)
  # impute_dates + !add_history ---> do above but filter out any observations not in df (still load
  #                                  and use historical data to improve imputation accuracy)
  # !impute_dates + add_history ---> bind (the new part) of df to historical data but do not impute
  #                                  missing sample dates; note that previously imputed dates are 
  #                                  *not* removed
  # !impute_dates + !add_history --> do NOTHING and go get yourself a coffee (you deserve it)
  #
  # TODO: i feel like this section could be cleaner, but also if it aint broke amiright?
  #       let's come back later.

  if (impute_dates) {
    writeLines('imputing dates...')

    # LOAD HISTORY (IF IT EXISTS) TO IMPROVE IMPUTATION -----
    fn <- here::here('out', 'current_labo_clean.csv')
    if (file.exists(fn)) {
      # get historical data (imputed and not)
      history <- rio::import(fn) %>%
                   mutate(across(starts_with('date'), as.Date)) %>%   # TODO: IDate weirdness
                   mutate(across((starts_with('date') & !contains('labo')),
                                 ~ as.numeric(date_reception_au_labo - .)))

      # filter out obs in df already in the history
      df <- df %>% 
              filter(n_labo %notin% history$n_labo)

      # build imputation dataset, removing previously imputed obs to avoid corrupting the imputation
      tmp <- history %>%
               filter(!is.na(date_collecte_echant)) %>%
               bind_rows(df)

      history_imputed <- history %>%
                           filter(is.na(date_collecte_echant))

        } else {
      tmp <- df
    }

    # IMPUTE -----
    tmp_imp <- tmp %>%
                 mutate(year = lubridate::year(date_reception_au_labo),
                        month = lubridate::month(date_reception_au_labo),
                        prov = as.factor(prov)) %>%
                 select(date_collecte_echant, year, month, date_debut_eruptions, date_envoi_echant,
                        prov)

    imp <- mice::mice(tmp_imp,
                      m = 1,
                      maxit = 30,
                      seed = 1)

    tmp$date_collecte_echant_imput <- mice::complete(imp) %>% pull(date_collecte_echant)

    df <- bind_rows.(tmp, history_imputed)


    # REMOVE HISTORY IF add_history == FALSE -----
    if (!add_history) {
      df <- df %>%
              filter(n_labo %in% new_observations)

    } else if (add_history & !exists('history')) {
      warning(paste('add_history = TRUE, but there was no history to add...',
                    'are you sure out/current_labo_clean.csv exist?'))
    }


  # CHECK WHETHER TO ADD HISTORY WHEN impute_dates == FALSE -----
  } else if (add_history) {
    fn <- here::here('out', 'current_labo_clean.csv')
    if (file.exists(fn)) {
      # get historical data (imputed and not)
      history <- rio::import(fn) %>%
                   mutate(across(starts_with('date'), as.Date)) %>%   # TODO: IDate weirdness
                   mutate(across((starts_with('date') & !contains('labo')),
                                 ~ as.numeric(date_reception_au_labo - .)))

      # filter out obs in df already in the history
      df <- df %>% 
              filter(n_labo %notin% history$n_labo) %>%
              bind_rows(history)

        } else {
      warning(paste('add_history = TRUE, but there was no history to add...',
                    'are you sure out/current_labo_clean.csv exist?'))
    }
  }

  

  # FINALIZE DATES ---------------------------------------------------------------------------------
  # convert dates back to date format
  df <- df %>%
          mutate(across((starts_with('date') & !contains('labo')),
                         ~ date_reception_au_labo - .))

  # add year, week, debut semaine; NOTE: these are based on the imputed date column and will be NA
  # if `imput_dates` is `FALSE`
  df <- df %>%
          mutate(year = lubridate::isoyear(date_collecte_echant_imput),
                 numsem = lubridate::isoweek(date_collecte_echant_imput),
                 debutsem = iso_to_date(week = numsem,
                                        year = year))

  
  # SORT -------------------------------------------------------------------------------------------
  df <- df %>% 
          arrange(debutsem, prov, zs)



  # SAVE -------------------------------------------------------------------------------------------
  if (save) {
    rio::export(df, here::here('out', 'current_labo_clean.csv'))
  }

  return(df)
}

