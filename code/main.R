# --------------------------------------------------------------------------------------------------
# Main : Prepare data and plots then render out
#
# Author : Catherine Eisenhauer
# Date : August 2022
#
# Depends:
#   dplyr, tidytable, tidyr, echarty, echarts4r, reactable, htmltools, rmarkdown
# --------------------------------------------------------------------------------------------------

library(dplyr)
#library(tidytable)
library(echarty)
library(echarts4r)
library(reactable)
library(htmltools)

sapply(list.files(here::here('code', 'mods'),
                  pattern = '*.R$',
                  full.names = TRUE,
                  ignore.case = TRUE),
       source,
       .GlobalEnv)

# LOAD ---------------------------------------------------------------------------------------------
df <- mod_prepare_cholera(data_root = here::here('data', 'raw'),
                          data_file = 'IDS_2023_52.xlsx',
                          year = 2023)
#df <- rio::import(here::here('data', 'clean', 'cholera_ids.RDS'))

min_date <- lubridate::today() - 180

endemic <- rio::import(here::here('data', 'reference', 'endemic_zones.csv'),
                       select = c('reg', 'zone')) %>%
             mutate(endemic = 'Endemique')

neighbordex <- readRDS(here::here('data', 'reference', 'neighbordex.RDS'))

drc_zone <- epiplaces::load_map('drc')


# WRANGLE ------------------------------------------------------------------------------------------
df <- df %>% 
        mutate(across(c(cases, deaths),
                        ~ tidyr::replace_na(., 0)),
                cases_4w = zoo::rollsum(cases,
                                        k = 4,
                                        align = 'right',
                                        na.pad = TRUE),
                alert = cases_4w >= 1,
                alert = ifelse(!alert, NA, alert),  # fill in gaps of 8 weeks or less
                alert = zoo::na.locf0(alert,
                                      maxgap = 8),
                alert = tidyr::replace_na(alert, FALSE),
                .by = reg_zone)

# CURRENT INDICATORS -----
# general
tbl <- df %>%
   select(reg, zone, reg_zone, date, alert, cases, cases_4w, deaths) %>%
   mutate(cases = ifelse(cases == 0, NA, cases), # reintroduce NAs to allow "indeterminant" trend
           deaths_4w = zoo::rollsum(deaths,
                                    k = 4,
                                    align = 'right',
                                    na.pad = TRUE),
           trend = epi_trend(cases),
           .by = reg_zone) %>%
   filter(date == max(date)) %>%
   mutate(cfr_4w = deaths_4w / cases_4w)

# add endemicity column
tbl <- tbl %>%
         left_join(endemic) %>%
         mutate(endemic = ifelse(is.na(endemic), '', endemic))


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
  filter(date >= max(date) - 7 * 8) %>%
  summarize(cases_spk = list(cases),
            .by = reg_zone) %>%
  right_join(tbl)


tbl <- df %>%
  filter(date >= min_date) %>%
  summarize(cases_spk_long = list(cases),
             .by = reg_zone) %>%
  right_join(tbl)


# BUILD DASHBOARD COMPONENTS -----------------------------------------------------------------------
colors <- list(Hausse = '#AA8439',
               Stable = '#7887AB',
               Baisse = '#2E4172',
               Inconnu = '#8f8f8f')

# timeseries plots ----------
grid_dimensions <- rio::import(here::here('data', 'reference', 'ts_grids.csv'))

nrows <- grid_dimensions$rows
names(nrows) <- grid_dimensions$reg
ncols <- grid_dimensions$cols
names(ncols) <- grid_dimensions$reg

regions_in_alert <- tbl %>%
                      summarize(alert = sum(alert),
                                 .by = 'reg') %>%
                      filter(alert > 0) %>%
                      pull(reg)

writeLines('building epiplots...')
ts_plots <- list()
for (r in regions_in_alert) {
  #writeLines(r)
  ts_plots[[r]] <- df %>%
                     filter(date >= min_date) %>%
                     mod_plot_epicurves(region = r,
                                        nrow = nrows[[r]],
                                        ncol = ncols[[r]])
}


ts_plots %>% saveRDS(here::here('out', 'ts_plots.RDS'))

# infocons ----------
infocons <- list()

df_infocon <- tbl %>%
  summarize(across(c(cases_4w, alert), ~ sum(., na.rm = TRUE)),
            .by = 'reg') %>%
  filter(alert > 0)

writeLines('building infocons...')
for (r in regions_in_alert) {
  #writeLines(r)

  tmp <- df_infocon %>%
    filter(reg == r)

  infocons[[r]]$cases <- infocon(value = tmp$cases_4w,
                                 title = 'Cas\nSuspects',
                                 subtitle = 'quatre semaines',
                                 img_path = here::here('www', 'cases.svg'))
  infocons[[r]]$alert <- infocon(value = tmp$alert,
                                 title = 'Alertes Suspect\u00e9es',
                                 subtitle = 'niveau de la zone',
                                 img_path = here::here('www', 'sus_alert.svg'))
}

infocons %>% saveRDS(here::here('out', 'infocons.RDS'))


# maps ----------
maps <- list()

writeLines('building maps...')
for (r in c('rdc', regions_in_alert)) {
  #writeLines(r)

  if (r == 'rdc') {
    map <- jsonlite::read_json(here::here('data', 'gis', 'drc_zone.json'))
    tmp <- tbl
  } else {
    map <- jsonlite::read_json(here::here('data', 'gis', paste0(r, '.json')))
    tmp <- tbl %>% filter(reg == r)
  }

  tmp <- tmp %>%
    mutate(zone_display = str_to_display(zone),
           alert = ifelse(alert, 'Alerte', 'Pas d\'Alerte'),
           cases_bin = case_when(cases_4w >= 50 ~ '50+',
                                 cases_4w >= 25 ~ '25 - 49',
                                 cases_4w >= 15 ~ '15 - 24', 
                                 cases_4w >= 10 ~ '10 - 14',
                                 cases_4w >= 5 ~ '5 - 9',
                                 cases_4w > 0 ~ '1 - 4',
                                 cases_4w == 0 ~ 'Pas de Cas'),
           deaths_bin = case_when(deaths_4w >= 25 ~ '25+',
                                  deaths_4w >= 20 ~ '20 - 24',
                                  deaths_4w >= 15 ~ '15 - 19',
                                  deaths_4w >= 10 ~ '10 - 14', 
                                  deaths_4w >= 5 ~ '5 - 9',
                                  deaths_4w > 0 ~ '1 - 4',
                                  deaths_4w == 0 ~ 'Pas de Décès'))

  # trend
  maps[[r]]$trend <- tmp %>%
    select(zone, cases_4w, trend) %>%
    mod_plot_map(type = 'categorical',
                 map = map,
                 x = 'trend',
                 categories = list('Hausse', 'Stable', 'Baisse', 'Inconnu'),
                 colors = list('#AA8439',
                               '#7887AB',
                               '#2E4172',
                               '#f0eeeb'),
                 map_name = r,
                 name_property = 'zone')

  # alert
  maps[[r]]$alert <- tmp %>%
    select(zone, cases_4w, alert) %>%
    mod_plot_map(type = 'categorical',
                 map = map,
                 x = 'alert',
                 categories = list('Alerte', 'Pas d\'Alerte'),
                 colors = list('#704D49', '#F0EEEB'),
                 map_name = r,
                 name_property = 'zone')


  # cases
  maps[[r]]$cases <- tmp %>%
    select(zone, cases_4w, cases_bin) %>%
    mod_plot_map(type = 'categorical',
                 map = map,
                 x = 'cases_bin',
                 categories = list('50+',
                                   '25 - 49',
                                   '15 - 24',
                                   '10 - 14',
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
                 map_name = r,
                 name_property = 'zone')

  # deaths
  maps[[r]]$deaths <- tmp %>%
    select(zone, deaths_4w, deaths_bin) %>%
    mod_plot_map(type = 'categorical',
                 map = map,
                 x = 'deaths_bin',
                 categories = list('25+',
                                   '20 - 24',
                                   '15 - 19',
                                   '10 - 14',
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
                 map_name = r,
                 name_property = 'zone')
}

saveRDS(maps, here::here('out', 'maps.RDS'))

# bars ----------
writeLines('building bar plots...')
tmp <- df %>%
         filter(date >= (max(date) - 7 * 3)) %>%
         summarize(cases = .sum(cases),
                    deaths = .sum(deaths),
                    .by = c(reg)) %>%
         mutate(reg_display = str_to_display(reg))

trends <- df %>%
         summarize(cases = .sum(cases),
                    .by = c(reg, date)) %>%
         mutate(reg_display = str_to_display(reg),
                 trend = trend(cases,
                               n = 6,
                               thresh = 0.03),
                 .by = reg) %>%
         filter(date == max(date)) %>%
         mutate(trend = case_when(trend == 'decreasing' ~ 'Baisse',
                                  trend == 'increasing' ~ 'Hausse',
                                  trend == 'stable' ~ 'Stable',
                                  TRUE ~ 'Inconnu')) %>%
         select(reg_display, trend)

tmp <- tmp %>%
         left_join(trends)

bars <- tmp %>%
  mod_plot_bar()

saveRDS(bars, here::here('out', 'bars.RDS'))

# national curve ----------
curves <- list()

writeLines('building national/regional epicurve...')
for (r in c('rdc', regions_in_alert)) {
  #writeLines(r)

  if (r == 'rdc') {
    tmp <- df %>%
             filter(date >= min_date)
 
  } else {
    tmp <- df %>%
            filter(reg == r,
                   date >= min_date)
  }

  tmp <- tmp %>%
    summarize(cases = sum(cases),
              deaths = sum(deaths),
              .by = date) %>%
    mutate(trend = epi_trend(cases))

  curves[[r]] <- tmp %>%
       e_charts(date,
                renderer = 'svg') %>%
       e_tooltip(trigger = 'axis') %>%
       e_bar(cases,
             color = colors[[unique(tmp$trend)]],
             x_index = 1,
             y_index = 1,
             barCategoryGap = 0,
             name = paste0('Cas Suspets (Tendance : ', unique(tmp$trend), ')')) %>%
       e_bar(deaths,
             color = '#a6a6a6',
             barCategoryGap = 0,
             name = 'Décès') %>%
       echarts4r::e_toolbox(orient = 'vertical') %>%
       echarts4r::e_toolbox_feature(feature = c('dataZoom', 'saveAsImage')) %>%
       e_grid(height = '35%') %>%
       e_grid(height = '35%',
              top = '50%') %>%
       e_y_axis(gridIndex = 1) %>%
       e_x_axis(gridIndex = 1) %>%
       e_legend(bottom = 0,
                left = 'center',
                orient = 'horizontal')
}

saveRDS(curves, here::here('out', 'curves.RDS'))


# table --------------------------------------------------------------------------------------------
writeLines('building summary table...')
tbl %>%
	mutate(oc = ifelse(reg %in% c('ituri', 'tshopo'), 'OCG', ''),
         trend = ifelse(trend == 'Inconnu', '', trend),
         alert = ifelse(alert, 'Alerte', ''),
         zone = str_to_display(zone)) %>%
  mod_render_table(link_list = regions_in_alert) %>%
  saveRDS(here::here('out', 'rctbl.RDS'))


# RENDER DASHBOARD ---------------------------------------------------------------------------------
writeLines('rendering dashboard...')
rmarkdown::render(here::here('code', 'dashboard.Rmd'),
                  output_file = here::here('out', 'site', 'index.html'))


# DONE ---------------------------------------------------------------------------------------------
writeLines('...Done!')
