
source("programs/requete_florileges.R")

df_florileges = df_florileges %>%
  filter(session_date >= "2000-01-01") %>%
  mutate(across(starts_with("taxon_count_"), as.integer)) %>%
  mutate(session_year = strftime(as.Date(session_date), "%Y"),
         session_year_factor_all = factor(session_year, levels = as.character(min(session_year, na.rm = T):max(session_year, na.rm = T))),
         taxon_presence = rowSums(select(., starts_with("taxon_count_Q")), na.rm=TRUE),
         taxon_presence_all = rowSums(select(., starts_with("taxon_count_")), na.rm=TRUE),
         taxon_combine = if_else(is.na(taxon_nom_scientifique), taxon, taxon_nom_scientifique)) %>%
  filter(taxon_combine != "null")

lst_florileges = sort(unique(df_florileges$taxon_nom_scientifique))

if (!exists("structure_name")) {
  structure_name = "Mairie de Montreuil"
}

df_florileges_struct = df_florileges %>%
  filter(structure_nom == structure_name) %>%
  mutate(session_year_factor = factor(session_year, levels = as.character(min(session_year, na.rm = T):max(session_year, na.rm = T))))

#######################################################
############## Répartition des taxons vus #############
#######################################################

ordre_taxonomique = read.csv2("data/doc_heloise/TAXREF_v18_2025/TAXREF_Plantae.csv", sep = ";")

df_obs_taxon = df_florileges_struct %>%
  group_by(taxon_combine) %>%
  summarise(nobs = n()) %>%
  left_join(ordre_taxonomique, by = c("taxon_combine" = "LB_NOM")) %>%
  filter(!is.na(REGNE), REGNE == "Plantae") %>%
  mutate(TRIBU_SAVE = TRIBU,
         TRIBU = if_else((SOUS_FAMILLE == "" & FAMILLE != ""),
                         "EFFACE", TRIBU),
         SOUS_FAMILLE = if_else(TRIBU == "EFFACE", TRIBU_SAVE, SOUS_FAMILLE),
         TRIBU = if_else(TRIBU == "EFFACE", "", TRIBU)) %>%
  arrange(ORDRE)

#######################################################
####################### Richesse ######################
#######################################################

df_richesse = df_florileges_struct %>%
  filter(taxon_presence_all != 0) %>%
  group_by(session_year, session_year_factor) %>%
  summarise(richesse = length(unique(taxon_combine)), .groups = 'drop')
df_richesse_5 = df_florileges_struct %>%
  filter(taxon_presence > 5) %>%
  group_by(session_year, session_year_factor) %>%
  summarise(richesse = length(unique(taxon_combine)), .groups = 'drop')

df_richesse_site = df_florileges_struct %>%
  filter(taxon_presence_all != 0) %>%
  group_by(session_year, session_year_factor, site_id, site) %>%
  summarise(richesse = length(unique(taxon_combine)), .groups = 'drop') %>%
  group_by(session_year) %>%
  mutate(rich_tot = sum(richesse),
         rich_moy = mean(richesse)) %>%
  select(session_year, session_year_factor, rich_moy) %>%
  unique()

#######################################################
################# Traits écologiques ##################
#######################################################

df_trait = read.csv2("data/SpeciesTrait2024.csv")

df_radar_all = df_florileges %>%
  select(taxon_nom_scientifique, taxon_combine, structure_id, structure_nom,
         session_id, site, site_id) %>%
  mutate(loca = str_locate(site, "\\(")[,1],
         site = if_else(!is.na(loca), str_sub(site, 1, loca-1), site)) %>%
  left_join(df_trait, by = c("taxon_nom_scientifique" = "Plant"))

df_radar_struct = df_radar_all %>%
  filter(structure_nom == structure_name)

min_nitro = min(df_radar_all$Nitrophilie, na.rm = T)
max_nitro = max(df_radar_all$Nitrophilie, na.rm = T)

min_entomo = min(df_radar_all$BioticVector, na.rm = T)
max_entomo = max(df_radar_all$BioticVector, na.rm = T)

min_vivace = min(df_radar_all$Vivace, na.rm = T)
max_vivace = max(df_radar_all$Vivace, na.rm = T)
