mod_prepare_gis_data <- function(data) {
  zs_sf <-
    sf::read_sf(here::here('data', 'gis', 'ZS', 'COD_htbnd_lvl1_ZS_a_msf181114.shp')) %>%
    linelist::clean_data(guess_dates = FALSE) %>%
    sf::st_make_valid()


  prov_sf <-
    sf::read_sf(paste('data', 'province', 'cod_admbnda_adm1_rgc_20170711.shp')) %>%
    linelist::clean_data(guess_dates = FALSE) %>%
    sf::st_make_valid()



  zs_sf %<>%
    mutate(
      province = case_when(
        province == "kasaa" ~ "kasai",
        province == "kasaa_oriental" ~ "kasai_oriental",
        province == "kasaa_central" ~ "kasai_central",
        province == "maa_ndombe" ~ "maindombe",
        province == "mai_ndombe" ~ "maindombe",
        TRUE ~ province
      ), 
      name = case_when(
        name == "ganga_dingila" ~ "ganga",
        name == "mufunga" ~ "mufunga_sampwe",
        name == "mongbalu" ~ "mongbwalu",
        name == "kamonia" ~ "kamonya",
        name == "ndjoko_mpunda" ~ "ndjoko_punda",
        name == "mongbalu" ~ "mongbwalu",
        name == "lubunga_ii" ~ "lubunga",
        name == "muetshi" ~ "mwetshi",
        name == "bipemba" ~ "bimpemba",
        name == "kalambayi_kabanga" ~ "kalambayi_kaban",
        name == "mwene_ditu" ~ "muene_ditu",
        name == "penjwa" ~ "pendjwa",
        name == "boso_manzi" ~ "bosomanzi",
        name == "boso_mondanda" ~ "bosomondanda",
        name == "bili_ii" ~ "bili",
        name == "omendjadi" ~ "omondjadi",
        name == "bagira" ~ "bagira_kasha",
        name == "mbulula" ~ "mbulala",
        name == "makiso_kisangani" ~ "makiso_kisangan",
        name == "bosanga" ~ "busanga",
        name == "banjow_moke" ~ "bandjow_moke",
        name == "kimbi_lulenge" ~ "lulenge_kimbi",
        name == "kuimba" ~ "kwimba",
        name == "kuimba" ~ "kwimba",
        name == "motumbe" ~ "lotumbe",
        name == "tshitenge" ~ "citenge",
        name == "ruzizi03" ~ "ruzizi",
        name == "kalonda" ~ "kalonda_ouest",
        name == "djalo_djeka" ~ "djalo_ndjeka",
        TRUE ~ name
      )
    )

# correct provinces in shapefile (16 ZS in lomami should be haut_lomami, 4 missing should be nord_kivu)
  zs_haut_lomami<-unique(data$zs[which(data$prov=="haut_lomami")])
  zs_sf$province[which(is.element(zs_sf$name,zs_haut_lomami)&zs_sf$province=="lomami")]<-"haut_lomami" 
  zs_sf$province[which(is.na(zs_sf$province))]<-"nord_kivu" 
  zs_sf$province[which(zs_sf$name=="manika")]<-"lualaba"

  unmatched <- data %>%
    distinct(prov, zs) %>%
    left_join(zs_sf %>%
                mutate(matched = 1) %>%
                select(matched, province, dist_adm, territoire, dist_sa, name),
              c(prov = "province", zs = "name")) %>%
    filter(is.na(matched))
  
  print(unmatched)
}

