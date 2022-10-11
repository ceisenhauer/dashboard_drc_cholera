# --------------------------------------------------------------------------------------------------
# Main : Prepare data and plots
#
# Author : Catherine Eisenhauer
# Date : August 2022
#
# Depends:
#   tidytable
# --------------------------------------------------------------------------------------------------

library(dplyr)
library(tidytable)
library(tinker)
library(echarty)
library(echarts4r)
library(reactable)

tinker::source_directory(here::here('code', 'mods'))
#source(here::here('code', 'mods', 'mod_prepare_IDS.R'))
#source(here::here('code', 'mods', 'mod_prepare_lab.R'))
source(here::here('code', 'utils.R'))

# UPDATE DATA (IF NEEDED) --------------------------------------------------------------------------
# this is probably going to be moved to a bash script
#chol <- mod_prepare_IDS('IDS_2022_38.xlsx')


##data_file <- 'IDS_2022_35.MDB'
##data_root <- here::here('data', 'ids')


#lab <- mod_prepare_lab('RESULTAT ROUGEOLE_RUBEOLE_LABOLUSHI_2022.xlsx',
                       ##save = FALSE,
                       #type = 'labolushi')

#lab <- mod_prepare_lab('Rougeole24092022.xlsx',
                       ##save = FALSE,
                       #type = 'inrb')


# LOAD ---------------------------------------------------------------------------------------------
df <- rio::import(here::here('out', 'current_clean.csv'),
                  select = c('prov', 'zs', 'prov_zs', 'year', 'numsem', 'debutsem', 'pop',
                             'totalcas', 'totaldeces'),
                  col.names = c('reg', 'zone', 'reg_zone', 'year', 'week', 'date', 'pop', 'cases',
                                'deaths')) %>%
        filter.(zone != 'shabunda_centre') %>%
        mutate.(date = as.Date(date))

neighbordex <- readRDS(here::here('data', 'reference', 'neighbordex.RDS'))

drc_zone <- epiplaces::load_map('drc')
#drc_reg <- epiplaces::load_map('drc',
                               #level = 'reg')

# WRANGLE ------------------------------------------------------------------------------------------
df <- df %>% 
        mutate.(across.(c(cases, deaths),
                        ~ replace_na.(., 0)),
                cases_4w = zoo::rollsum(cases,
                                        k = 4,
                                        align = 'right',
                                        na.pad = TRUE),
                alert = cases_4w >= 1,
                alert = ifelse(!alert, NA, alert),  # fill in gaps of 8 weeks or less
                alert = zoo::na.locf0(alert,
                                      maxgap = 8),
                alert = replace_na.(alert, FALSE),
                .by = reg_zone)

# CURRENT INDICATORS -----
tbl <- df %>%
   select.(reg, zone, reg_zone, date, alert, cases, cases_4w, deaths) %>%
   filter.(date >=  max(date) - (7 * 6)) %>%      # calculate on the minimum amount of data needed
   mutate.(cases = ifelse(cases == 0, NA, cases), # reintroduce NAs to allow "indeterminant" trend
           deaths_4w = zoo::rollsum(deaths,
                                    k = 4,
                                    align = 'right',
                                    na.pad = TRUE),
           trend = epi_trend(cases),
           .by = reg_zone) %>%
   filter.(date == max(date)) %>%
   mutate.(trend = case_when(trend == 'increasing' ~ 'Hausse',
                             trend == 'decreasing' ~ 'Baisse',
                             trend == 'stable' ~ 'Stable',
                             TRUE ~ 'Inconnu'),
           cfr_4w = deaths_4w / cases_4w)


# add neighboring alerts
tbl$neighbors <- 0

for (i in 1:nrow(neighbordex)) {
  z <- drc_zone[i, ]$zone
  neighbors <- drc_zone[neighbordex[[i]], ]$zone

  idx <- tbl$zone %in% neighbors
  tbl[tbl$zone == z, 'neighbors'] <- .sum(tbl[idx, 'alert'])
}


# TIMESERIES TOUCH UPS -----
df <-  tbl %>%
         select(reg_zone, trend) %>%
         right_join(df,
                    by = 'reg_zone')


# add sparklines
tbl <- df %>%
  filter.(date >= max(date) - 7 * 8) %>%
  summarize.(cases_spk = list(cases),
             .by = reg_zone) %>%
  right_join(tbl)


tbl <- df %>%
  summarize.(cases_spk_long = list(cases),
             .by = reg_zone) %>%
  right_join(tbl)

# EXPORT -----
#tbl %>%
  #mutate.(reg = tinker::str_to_display(reg),
          #zone = tinker::str_to_display(zone)) %>%
  #select.(zone, reg, reg_zone, status_outbreak, cases_4w, trend, deaths_4w, cfr_4w) %>%
  #rio::export(here::here('out', 'current_table.csv'))

# BUILD DASHBOARD COMPONENTS -----------------------------------------------------------------------

# timeseries plots
grid_dimensions <- rio::import(here::here('data', 'static_data', 'ts_grids.csv'))

nrows <- grid_dimensions$rows
names(nrows) <- grid_dimensions$reg
ncols <- grid_dimensions$cols
names(ncols) <- grid_dimensions$reg

ts_plots <- list()
for (r in unique(df$reg)) {
  writeLines(r)
  ts_plots[[r]] <- df %>%
                     #filter(zone != 'shabunda') %>%
                     mod_plot_epicurves(region = r,
                                        nrow = nrows[[r]],
                                        ncol = ncols[[r]])
}

df_nat <- df %>% 
         filter(year == 2022) %>%
         select(date, reg, zone, cases) %>%
         distinct() %>%
         group_by(date, reg) %>%
         summarize(cases = sum(cases)) %>%
         group_by(reg) %>%
         mutate(cases_smooth = zoo::rollapply(cases,
                                              width = 6,
                                              FUN = mean,
                                              na.rm = TRUE,
                                              align = 'center',
                                              fill = NA,
                                              partial = TRUE),
                 trend = trend(cases_smooth,
                               n = 6,
                               thresh = 0.03),
                 trend = case_when(trend == 'decreasing' ~ 'Baisse',
                                   trend == 'increasing' ~ 'Hausse',
                                   trend == 'stable' ~ 'Stable',
                                   TRUE ~ 'Inconnu')) %>%
         ungroup() %>%
         mutate(zone = reg,
                reg = 'RDC',
                year = 2022)

df_nat <- df_nat %>%
            mutate(link = paste0('#', zone),
                   link = stringr::str_replace(link, '_', '-'))

ts_plots[['rdc']] <-  df_nat %>%
                       mod_plot_epicurves(region = 'RDC',
                                          links = df_nat$link,
                                          nrow = nrows[['RDC']],
                                          ncol = ncols[['RDC']])

ts_plots %>% saveRDS(here::here('out', 'ts_plots.RDS'))


# maps ---------------------------------------------------------------------------------------------

maps <- list()

maps$trend <- tbl %>%
  mutate(zone_display = tinker::str_to_display(zone),
         cases = cases_4w) %>%
  select(zone, cases, trend) %>%
  mod_plot_map(type = 'categorical',
               map = jsonlite::read_json(here::here('data', 'gis', 'drc_zone.json')),
               x = 'trend',
               categories = list('Hausse', 'Stable', 'Baisse', 'Inconnu'),
               colors = list('#AA8439',
                             '#7887AB',
                             '#2E4172',
                             '#f0eeeb'),
               map_name = 'zones',
               name_property = 'zone')

maps$alert <- tbl %>%
  mutate(zone_display = tinker::str_to_display(zone),
         cases = cases_4w,
         alert = ifelse(alert, 'Alerte', 'Pas d\'Alerte')) %>%
  select(zone, cases, alert) %>%
  mod_plot_map(type = 'categorical',
               map = jsonlite::read_json(here::here('data', 'gis', 'drc_zone.json')),
               x = 'alert',
               categories = list('Alerte', 'Pas d\'Alerte'),
               colors = list('#704D49', '#F0EEEB'),
               map_name = 'zones',
               name_property = 'zone')


maps$cases <- tbl %>%
  mutate(zone_display = tinker::str_to_display(zone),
         cases = cases_4w,
         cases_bin = case_when(cases >= 50 ~ '50+',
                               cases >= 25 ~ '25 - 49',
                               cases >= 15 ~ '15 - 24', 
                               cases >= 10 ~ '10 - 15',
                               cases >= 5 ~ '5 - 9',
                               cases > 0 ~ '1 - 4',
                               cases == 0 ~ 'Pas de Cas')) %>%
  select(zone, cases, cases_bin) %>%
  mod_plot_map(type = 'categorical',
               map = jsonlite::read_json(here::here('data', 'gis', 'drc_zone.json')),
               x = 'cases_bin',
               categories = list('50+',
                                 '25 - 49',
                                 '15 - 24',
                                 '10 - 15',
                                 '5 - 9',
                                 '1 - 4',
                                 'Pas de Cas'),
               colors = list('#292A1D',
                             '#4E4F39',
                             '#727356',
                             '#949674',
                             '#B6B894',
                             '#D6D8B4',
                             '#FFFFFE'),
               map_name = 'zones',
               name_property = 'zone')


maps$deaths <- tbl %>%
  mutate(zone_display = tinker::str_to_display(zone),
         deaths = deaths_4w,
         deaths_bin = case_when(deaths >= 50 ~ '50+',
                                deaths >= 25 ~ '25 - 49',
                                deaths >= 15 ~ '15 - 24', 
                                deaths >= 10 ~ '10 - 15',
                                deaths >= 5 ~ '5 - 9',
                                deaths > 0 ~ '1 - 4',
                                deaths == 0 ~ 'Pas de Décès')) %>%
  select(zone, deaths, deaths_bin) %>%
  mod_plot_map(type = 'categorical',
               map = jsonlite::read_json(here::here('data', 'gis', 'drc_zone.json')),
               x = 'deaths_bin',
               categories = list('50+',
                                 '25 - 49',
                                 '15 - 24',
                                 '10 - 15',
                                 '5 - 9',
                                 '1 - 4',
                                 'Pas de Décès'),
               colors = list('#111623',
                             '#35415F',
                             '#606F93',
                             '#7887AB',
                             '#A7B9D2',
                             '#DFE9F2',
                             '#FDFEFE'),
               map_name = 'zones',
               name_property = 'zone')


saveRDS(maps, here::here('out', 'maps.RDS'))

# bars ---------------------------------------------------------------------------------------------
tmp <- df %>%
         filter.(date >= (max(date) - 7 * 3)) %>%
         summarize.(cases = .sum(cases),
                    deaths = .sum(deaths),
                    .by = c(reg)) %>%
         mutate.(reg_display = tinker::str_to_display(reg))

trends <- df %>%
         summarize.(cases = .sum(cases),
                    .by = c(reg, date)) %>%
         mutate.(reg_display = tinker::str_to_display(reg),
                 trend = trend(cases,
                               n = 6,
                               thresh = 0.03),
                 .by = reg) %>%
         filter.(date == max(date)) %>%
         mutate.(trend = case_when(trend == 'decreasing' ~ 'Baisse',
                                   trend == 'increasing' ~ 'Hausse',
                                   trend == 'stable' ~ 'Stable',
                                   TRUE ~ 'Inconnu')) %>%
         select(reg_display, trend)

tmp <- tmp %>%
         left_join(trends)

bars <- tmp %>%
  mod_plot_bar()

saveRDS(bars, here::here('out', 'bars.RDS'))

# national curve -----------------------------------------------------------------------------------
e <- df %>%
  filter(year == 2022) %>%
  summarize.(cases = sum(cases),
             .by = date) %>%
  mutate.(trend = epi_trend(cases)) %>%
  mutate.(trend = case_when(trend == 'decreasing' ~ 'Baisse',
                            trend == 'increasing' ~ 'Hausse',
                            trend == 'stable' ~ 'Stable',
                            TRUE ~ 'Pas des Données')) %>%
  echarts4r::e_charts(date,
                      renderer = 'svg') %>%
  echarts4r::e_bar(cases,
                   color = '#AA8439',
                   barCategoryGap = 0,
                   name = 'Cas Suspects') %>%
    echarts4r::e_toolbox() %>%
    echarts4r::e_toolbox_feature(feature = c('dataZoom', 'saveAsImage'))

# table --------------------------------------------------------------------------------------------
tbl %>% rio::export(here::here('out', 'current_indicators.csv'))
tbl %>% saveRDS(here::here('out', 'current_indicators.RDS'))

tbl %>%
  #filter(zone != 'shabunda') %>%
	mutate(oc = ifelse(reg %in% c('ituri', 'tshopo'), 'OCG', '')) %>%
  mutate(trend = ifelse(trend == 'Inconnu', '', trend),
         alert = ifelse(alert == 'Alerte', 'Alerte', '')) %>%
  #filter(reg == 'tshopo' | reg == 'ituri') %>%
  mod_render_table()# %>%
  #htmlwidgets::saveWidget('table.html')

tbl %>%
  mutate(trend = ifelse(trend == 'Inconnu', '', trend),
         alert = ifelse(alert, 'Alerte', '')) %>%
  mod_render_table() %>%
  saveRDS(here::here('out', 'rctbl.RDS'))



