mod_prepare_malnu <- function() {
  tmp <- rio::import(here::here('data', 'malnu_overview.csv')) %>%
           linelist::clean_data() %>%
           filter(grepl('zone_de_sante', code_zone)) %>%
           mutate(code_pronanut = substr(code_zone, 0, 2),
                  zone = substr(code_zone, 4, length(code_zone)),
                  zone = stringr::str_replace(zone, '_zone_de_sante', ''))
       
  prov_codes <- rio::import(here::here('data', 'reference', 'prov_codes.csv')) %>%
                  select(code_pronanut, prov)
  tmp <- tmp %>%
           left_join(prov_codes,
                     by = 'code_pronanut') %>%
           select(-code_pronanut, -code_zone) %>%
           rename(reg = prov)

  #df <- rio::import(here::here('out', 'current_clean.csv'),
                    #select = c('prov', 'zs'),
                    #col.names = c('reg', 'zone')) %>%
          #distinct()

  fixes <- list(# haut katanga
                ruashi = 'rwashi',
                # ituri
                gety = 'gethy',
                nyankunde = 'nyakunde',
                # kasai
                kamonia = 'kamonya',
                ndjoko_mpunda = 'ndjoko_punda',
                # kasai central
                bena_tshadi = 'bena_tshiadi',
                muetshi = 'mwetshi',
                # kasai oriental
                bipemba = 'bimpemba',
                kabeya_kamwanga = 'kabeya_kamuanga',
                # kinshasa
                kalamu_1 = 'kalamu_i',
                kalamu_2 = 'kalamu_ii',
                maluku_1 = 'maluku_i',
                maluku_2 = 'maluku_ii',
                masina_1 = 'masina_i',
                masina_2 = 'masina_ii',
                mont_ngafula_1 = 'mont_ngafula_i',
                mont_ngafula_2 = 'mont_ngafula_ii',
                # kongo central
                massa = 'masa',
                kuimba = 'kwimba',
                muanda = 'moanda',
                # kwango
                kisanji = 'kisandji',
                # lomami
                kalambayi_kabanga = 'kalambayi_kaban',
                mweneditu = 'muene_ditu',
                # maindombe
                banzow_moke = 'bandjow_moke',
                # mongala
                boso_manzi = 'bosomanzi',
                boso_mondanda = 'bosomondanda',
                # sankuru
                omendjadi = 'omondjadi',
                # sud kivu
                bagira = 'bagira_kasha',
                haut_plateau = 'hauts_plateaux',
                kimbi_lulenge = 'lulenge_kimbi',
                shabunda = 'shabunda_centre',
                # tanganyika
                kiambi = 'kiyambi',
                mbulula = 'mbulala',
                # tshopo
                makiso_kisangani = 'makiso_kisangan')

  tmp <- tmp %>%
          mutate.(zone = dplyr::recode(zone, !!!fixes))

  tmp <- tmp %>%
           mutate_rowwise.(admins = sum(c(unti, unta, uns), na.rm = TRUE),
                           underweight = underweight / 100)


  #tmp <- tmp %>%
    #pivot_longer.(c(-zone, -reg)) %>%

  #tmp <- tmp %>%
    #group_by(reg, zone) %>%
    #summarize(value = last(na.omit(value)))


  #plot_map(tmp,
           #x = 'value',
           #n_breaks = 6,
           #border_color = 'grey',
           #border_size = 0.5,
           #legend_title = '% of Children who are Underweight',
           #map = epimaps::load_map('drc')) +
  #geom_sf(data = epimaps::load_map('drc', level = 'reg'),
          #fill = 'transparent',
          #color = 'black',
          #size = 1)

  #ggsave('malnu_map.png')
}




