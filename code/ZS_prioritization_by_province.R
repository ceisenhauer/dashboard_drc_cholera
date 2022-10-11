provs<-unique(summary_by_zs$prov)
provs_2<-tools::toTitleCase(str_replace_all(provs, "_", "-"))
provs_2[which(provs_2=="Maindombe")]<-"Mai-Ndombe"

d_prov<-NULL
for(p in 1:length(provs_2)){
file.name <- here::here('data', 'Province_prioritization', paste0('rank_top_5_priority.test_size_1_', provs_2[p], '_IDPFALSE_2020-12-16.csv'))
d<-read.csv(file.name,stringsAsFactors = F)
d$prov<-provs[p]
d<-clean_data(d)
d_prov<-rbind(d_prov,d)
}

names(d_prov)[2]<-"zs"

d_prov %<>%
  mutate(
    zs = case_when(
      zs == "ganga_dingila" ~ "ganga",
      zs == "mufunga" ~ "mufunga_sampwe",
      zs == "mongbalu" ~ "mongbwalu",
      zs == "kalonda_ouest" ~ "kalonda",
      zs == "kamonia" ~ "kamonya",
      zs == "ndjoko_mpunda" ~ "ndjoko_punda",
      #zs == "bena_tshiadi" ~ "bena_tshadi",
      zs == "mongbalu" ~ "mongbwalu",
      zs == "lubunga_ii" ~ "lubunga",
      zs == "muetshi" ~ "mwetshi",
      zs == "bipemba" ~ "bimpemba",
      zs == "kalambayi_kabanga" ~ "kalambayi_kaban",
      zs == "mwene_ditu" ~ "muene_ditu",
      zs == "penjwa" ~ "pendjwa",
      zs == "boso_manzi" ~ "bosomanzi",
      zs == "boso_mondanda" ~ "bosomondanda",
      zs == "bili_ii" ~ "bili2",
      zs == "bagira" ~ "bagira_kasha",
      zs == "mbulula" ~ "mbulala",
      zs == "makiso_kisangani" ~ "makiso_kisangan",
      zs == "bosanga" ~ "busanga",
      zs == "banjow_moke" ~ "bandjow_moke",
      zs == "kuimba" ~ "kwimba",
      zs == "kuimba" ~ "kwimba",
      zs == "motumbe" ~ "lotumbe",
      zs == "tshitenge" ~ "citenge",
      zs == "ruzizi03" ~ "ruzizi",
      zs=="moanda" ~ "muanda",
      zs=="gandajika" ~ "ngandajika",
      zs=="djalo_djeka" ~ "djalo_ndjeka",
      zs=="shabunda_centre" ~ "shabunda",
      zs=="bambu_mines" ~ "bambu", 
      zs=="bogose_nubea" ~ "bogosenubea", 
      zs=="bosomodanda" ~ "bosomondanda",
      zs=="gandajika" ~ "ngandajika",
      zs=="gety"~ "gethy",
      zs=="hauts_plateaux_d_uvira"~ "hauts_plateaux",
      #zs=="kalima" ~
      zs=="kamina_base_baka"~ "kamina",
      zs=="kamuesha" ~ "kamwesha",
      zs=="kashiobwe" ~ "kashobwe",
      zs=="kasongolunda" ~ "kasongo_lunda",
      #kayna
      zs=="kiambi" ~ "kiyambi",
      zs=="kimbau" ~ "kibao",
      zs=="kimbi_lulenge" ~ "lulenge_kimbi",
      #kyondo
      zs=="lolanga_mampoko" ~ "mampoko",
      zs=="lubondayi" ~ "lubondaie",
      zs=="lubunga_2" ~ "lubunga",
      zs=="miti_murrhesa"~ "miti_murhesa",
      zs=="mobayi" ~ "mobayi_mbongo",
      zs=="muanda"~"moanda",
      zs=="nsona_pangu"~ "nsona_mpangu",
      zs=="ntand_embelo"~ "ntandembelo",
      zs=="nyankunde" ~ "nyakunde",
      zs=="pendjua" ~ "pendjwa",
      zs=="ruashi" ~ "rwashi",
      #rwanguba
      zs=="salamabila" ~ "saramabila",
      zs=="sekebanza" ~ "seke_banza",
      zs=="wamba_luadi" ~ "wamba_lwadi",
      zs=="wanie_rukula" ~ "wanierukula",
      zs=="bili_2" ~ "bili",
      
      TRUE ~ zs
    )
  )


# test<-merge(d_prov,summary_by_zs,by=c("prov","zs"),all.x=T)
# table(test$zs[which(is.na(test$block))])
# test<-merge(summary_by_zs,d_prov,by=c("prov","zs"),all.x=T)
# table(test$zs[which(is.na(test$consensus_rank))])

d_prov$risk_ranking<-round(d_prov$consensus_rank,digits=0)
d_prov$risk_top_5<-NA
d_prov$risk_top_5[which(d_prov$consensus_rank<=5)]<-"Risque eleve"



# MoH prioritization ------------------------------------------------------
file.name <- here::here('data', 'Analyse_du_risque_MoH', 'Liste_ZS_risque_MoH.csv')
d_moh<-rio::import(file.name, encoding = 'Latin-1')
d_moh<-clean_data(d_moh)
names(d_moh)[1:2]<-c("prov","zs")

d_moh$prov[which(d_moh$prov=="mai_ndombe")]<-"maindombe"

test<-merge(d_moh,summary_by_zs,by=c("prov","zs"),all.x=T)
table(test$zs[which(is.na(test$block))])
test<-merge(summary_by_zs,d_moh,by=c("prov","zs"),all.x=T)
table(test$zs[which(is.na(test$total_points_risque))])


d_moh %<>%
  mutate(
    zs = case_when(
      zs == "bambu_mines" ~ "bambu",
      zs == "bandjau" ~ "bandjow_moke",
      zs == "bili_eqt" ~ "bili",
      zs == "bipemba" ~ "bimpemba",
      zs == "bogose_nubea" ~ "bogosenubea",
      zs == "bosomodanda" ~ "bosomondanda",
      zs == "hauts_plateaux_d_uvira" ~ "hauts_plateaux",
      zs == "kalambayi_kabanga" ~ "kalambayi_kaban",
      #zs == "kalima" ~ "",
      zs == "kamina_base_baka" ~ "baka",
      zs == "kamonia" ~ "kamonya",
      zs == "kamuesha" ~ "kamwesha",
      zs == "kasongolunda" ~ "kasongo_lunda",
      #zs == "kayna" ~ "",
      zs == "kiambi" ~ "kiyambi",
      zs == "kimbau" ~ "kimbao",
      zs == "kimbi_lulenge" ~ "lulenge_kimbi",
      zs == "kitoyi" ~ "katoyi",
      zs == "kuimba" ~ "kwimba",
      #zs=="kyondo" ~ "",
      zs=="lolanga_mampoko" ~ "mampoko",
      zs=="lubondayi" ~ "lubondaie",
      zs=="lubunga_2" ~ "lubunga",
      #zs=="lukashi_lualu" ~ "bambu", 
      zs=="makiso_kisangani" ~ "makiso_kisangan", 
      #zs=="mbomomange" ~ "",
      zs=="mbulula" ~ "mbulala",
      zs=="miti_murrhesa"~ "miti_murhesa",
      zs=="mobayi"~ "mobayi_mbongo",
      zs=="muanda" ~ "moanda",
      zs=="muetshi" ~ "mwetshi",
      zs=="mwene_ditu" ~ "muene_ditu",
      zs=="nsona_pangu" ~ "nsona_mpangu",
      zs=="ntand_embelo" ~ "ntandembelo",
      zs=="nyankunde" ~ "nyakunde",
      zs=="omendjadi" ~ "omondjadi",
      zs=="pendjua" ~ "pendjwa",
      #zs=="rwanguba"~"",
      zs=="salamabila"~ "saramabila",
      zs=="sekebanza"~ "seke_banza",
      zs=="shabunda_centre" ~ "shabunda",
      zs=="tshitenge" ~ "citenge",
      zs=="tshitshimbi" ~ "tshishimbi",
      zs=="wamba_luadi" ~ "wamba_lwadi",
      zs=="wanie_rukula" ~ "wanierukula",
      zs=="yumbi" ~ "yumbi",
      TRUE ~ zs
    )
  )




