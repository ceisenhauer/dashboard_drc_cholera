# --------------------------------------------------------------------------------------------------
# Prepare Historic IDS Data
#
# Author : Catherine
# Date : July 2022
#
# This script is used to compile historic IDS data into a single RDS file to minimize repeatedly
# loading and cleaning old data every time the dashboard is built. *NOTE:* this code should be 
# updated once per year as the prior year's data is finalized.
#
# Depends:
#   tidytable
# --------------------------------------------------------------------------------------------------

library(tidytable)
source(here::here('code', 'mods', 'mod_prepare_ids_cholera.R'))

df <- data.frame()
for (year in c(2015:2022)) {
  writeLines(paste0('Processing year ', year, '...'))

  fn <- paste0('IDS_RDC_', year, '.xlsx')

  max_week <- ifelse(lubridate::leap_year(year), 53, 52)

  df <- mod_prepare_cholera(data_file = fn,
                            data_root = here::here('data', 'static_data'),
                            year = year,
                            max_week = max_week,
                            #add_history = FALSE,
                            save = FALSE) %>%
          bind_rows.(df)
}

writeLines('Saving...')
df <- df %>% 
        arrange.(prov, zs, debutsem)


# better col names
df <- df %>%
  rename.(zone = zs,
          prov_zone = prov_zs,
          year = year,
          week = numsem,
          date = debutsem,
          cases_0_11 = c011mois,
          cases_12_59 = c1259mois,
          cases_5years = c5ansp,
          cases = totalcas,
          deaths_0_11 = d011mois,
          deaths_12_59 = d1259mois,
          deaths_5years = d5ansp,
          deaths = totaldeces) %>%

df <- df %>%
  select.(prov, zone, prov_zone, year, week, date, starts_with('cases'), starts_with('deaths'))


df %>% 
  mutate.(cases = tidyr::replace_na(cases, 0)) %>%
  summarize.(cases = sum(cases),
             .by = date) %>%
  ggplot(aes(y = cases,
             x = date)) +
  geom_col() +
  labs(title = 'Weekly Suspected Cholera Cases',
       x = '',
       y = 'Cases') +
  epiplots::theme_clean()

ggsave('history_curve.png')


saveRDS(df, here::here('data', 'reference', 'IDS_cholera_history.RDS'))

writeLines('Done !')


