
source("programs/requete_florileges.R")

reg_dep = read.csv2("data/departements-france.csv", sep=",")
ordre_taxonomique = read.csv2("data/doc_heloise/TAXREF_v18_2025/TAXREF_Plantae.csv", sep = ";")

df_florileges = df_florileges %>%
  filter(session_date >= "2000-01-01") %>%
  mutate(across(starts_with("taxon_count_"), as.integer)) %>%
  mutate(session_year = strftime(as.Date(session_date), "%Y"),
         session_year_factor_all = factor(session_year, levels = as.character(min(session_year, na.rm = T):max(session_year, na.rm = T))),
         taxon_presence = rowSums(select(., starts_with("taxon_count_Q")), na.rm=TRUE),
         taxon_presence_all = rowSums(select(., starts_with("taxon_count_")), na.rm=TRUE),
         taxon_combine = if_else(is.na(taxon_nom_scientifique), taxon, taxon_nom_scientifique),
         cd_nom = as.integer(cd_nom),
         is.liste = if_else(is.na(taxon_nom_scientifique), "hors liste", "liste")) %>%
  filter(taxon_combine != "null") %>%
  left_join(ordre_taxonomique, by = c("cd_nom" = "CD_NOM")) %>%
  left_join(reg_dep, by = c("site_departement" = "code_departement"))

df_florileges_presence = df_florileges %>%
  filter(taxon_presence > 0)

lst_florileges = sort(unique(df_florileges$taxon_nom_scientifique))

if (!exists("structure_name")) {
  structure_name = "Mairie de Montreuil"
}

df_florileges_struct = df_florileges %>%
  filter(structure_nom == structure_name) %>%
  mutate(session_year_factor = factor(session_year, levels = as.character(min(session_year, na.rm = T):max(session_year, na.rm = T))))
df_florileges_presence_struct = df_florileges_presence %>%
  filter(structure_nom == structure_name) %>%
  mutate(session_year_factor = factor(session_year, levels = as.character(min(session_year, na.rm = T):max(session_year, na.rm = T))))

#######################################################
#################### Chiffres clés ####################
#######################################################

nb_site = length(unique(df_florileges_struct$site_id))
nb_saisisseur = length(unique(df_florileges_struct$user_id))
nb_sp_liste = length(na.omit(unique(df_florileges_presence_struct$taxon_nom_scientifique)))
# Nombre d'observateurs dans la structure -> compliqué
all_tax = unique(c(df_florileges_presence_struct$taxon_combine))

lst_florileges = sort(unique(df_florileges$taxon_nom_scientifique))
lst_structure = sort(unique(df_florileges_presence_struct$taxon))
lst_structure = lst_structure[which(!(lst_structure %in% lst_florileges))]

df_taxon_add = df_florileges_presence %>%
  filter(!(taxon %in% lst_florileges), !is.na(structure_nom)) %>%
  group_by(structure_nom) %>%
  summarise(n_tax = n_distinct(taxon))

# ---------- Abondance (donut + barplot) ----------

df_tax_ab = df_florileges_presence_struct %>%
  mutate(taxon_count = if_else(taxon_presence > 0, 1, 0)) %>%
  group_by(FAMILLE) %>%
  summarise(sum_pres = sum(taxon_count),
            .groups = 'drop') %>%
  mutate(prop_pres = sum_pres/sum(sum_pres)) %>%
  arrange(sum_pres) %>%
  filter(!is.na(FAMILLE), FAMILLE != "")

# Dataframe extrait pour retenir le top 5 des taxons uniquement
couleurs_top_5 = c("#ffb627", "#d16666", "#2e4052", "#98ce00", "#62A87C")
# On retient les 5 premiers noms 
lst_top_5 = (df_tax_ab %>% arrange(desc(sum_pres)) %>% pull(FAMILLE))[1:5]
# On supprime les NA s'il y a moins de 5 noms
lst_top_5 = lst_top_5[!is.na(lst_top_5)]
# On note les rangs dans le dataframe
names_top_5 = which(df_tax_ab$FAMILLE %in% lst_top_5)
# On extrait les lignes correspondantes en associant une couleur
df_top_5 = df_tax_ab[names_top_5,] %>%
  mutate(couleur = couleurs_top_5[1:n()])
# On ajoute la catégorie "Autres" qui regroupe les autres espèces
if (nrow(df_tax_ab) > nrow(df_top_5)) {
  df_top_5 = rbind(data.frame(FAMILLE = "Autres",
                              sum_pres = sum(df_tax_ab$sum_pres)-sum(df_top_5$sum_pres),
                              prop_pres = 1-sum(df_top_5$prop_pres),
                              couleur = "#c0cdf5"),
                   df_top_5)
}
# On ajoute les proportions cumulées pour le graphique en donut
df_top_5 =  df_top_5 %>%
  arrange(desc(row_number())) %>%
  mutate(ymax = cumsum(prop_pres),
         ymin = c(0, head(ymax, n = -1)),
         label_position = (ymax + ymin) / 2)

#######################################################
############## Répartition des taxons vus #############
#######################################################

df_obs_taxon = df_florileges_presence_struct %>%
  group_by(REGNE, FAMILLE, taxon_combine) %>%
  summarise(nobs = n(), .groups = 'drop') %>%
  filter(!is.na(REGNE), REGNE == "Plantae", FAMILLE != "") %>%
  # mutate(TRIBU_SAVE = TRIBU,
  #        TRIBU = if_else((SOUS_FAMILLE == "" & FAMILLE != ""),
  #                        "EFFACE", TRIBU),
  #        SOUS_FAMILLE = if_else(TRIBU == "EFFACE", TRIBU_SAVE, SOUS_FAMILLE),
  #        TRIBU = if_else(TRIBU == "EFFACE", "", TRIBU)) %>%
  arrange(FAMILLE)

#######################################################
####################### Richesse ######################
#######################################################

df_richesse = df_florileges_presence_struct %>%
  select(session_year, session_year_factor, taxon_combine, is.liste) %>%
  unique()
df_richesse_5 = df_florileges_presence_struct %>%
  filter(taxon_presence > 5) %>%
  group_by(session_year, session_year_factor) %>%
  summarise(richesse = length(unique(taxon_combine)), .groups = 'drop')

df_richesse_site = df_florileges_presence_struct %>%
  group_by(session_year, session_year_factor, site_id, site) %>%
  summarise(richesse = length(unique(taxon_combine)), .groups = 'drop') %>%
  group_by(session_year) %>%
  mutate(rich_moy = mean(richesse)) %>%
  select(session_year, session_year_factor, rich_moy) %>%
  unique()
df_richesse_site_reg = df_florileges_presence %>%
  filter(code_region == unique(df_florileges_struct$code_region)) %>%
  group_by(session_year, session_year_factor_all, site_id, site) %>%
  summarise(richesse = length(unique(taxon_combine)), .groups = 'drop') %>%
  group_by(session_year) %>%
  mutate(rich_moy = mean(richesse),
         rich_sd = sd(richesse)) %>%
  select(session_year, session_year_factor_all, rich_moy, rich_sd) %>%
  unique()

# ---------- Référentiel ----------
df_session_struct = df_florileges_presence_struct %>% select(session_year, session_id) %>%
  unique() %>% mutate(structure = "Structure")
df_session_non_struct = df_florileges_presence %>%
  filter(structure_nom != structure_name, code_region == unique(df_florileges_struct$code_region)) %>% 
  select(session_year, session_id) %>% unique() %>% mutate(structure = "Autres")
df_session = rbind(df_session_struct, df_session_non_struct) 

# ---------- Richesse cumulée ----------
# -> Liste Florilèges
df_rich_cumul_lst_flori = df_florileges_presence_struct %>%
  filter(!is.na(taxon_nom_scientifique)) %>%
  group_by(taxon_nom_scientifique) %>%
  summarise(min_year = min(session_year), .groups = 'drop') %>%
  group_by(min_year) %>%
  summarise(rich = n()) %>%
  mutate(rich_cumul = cumsum(rich))

# - Toutes les espèeces
df_rich_cumul_all = df_florileges_presence_struct %>%
  filter(!is.na(taxon_combine)) %>%
  group_by(taxon_combine) %>%
  summarise(min_year = min(session_year), .groups = 'drop') %>%
  group_by(min_year) %>%
  summarise(rich = n()) %>%
  mutate(rich_cumul = cumsum(rich))

#######################################################
#################### Participation ####################
#######################################################

df_site_tempo_line = df_florileges_struct %>%
  mutate(session_year = as.integer(session_year)) %>%
  arrange(session_date) %>%
  select(site_id, site, session_year) %>%
  unique() %>%
  group_by(site_id, site) %>%
  mutate(index = cur_group_id(),
         index_y = index+cumsum(c(TRUE, diff(session_year) != 1)),
         name = paste0(site_id, "_", str_sub(site, 1, 20)),
         groupe = paste0(site_id, index_y)) %>%
  arrange(session_year)

df_site_point = df_site_tempo_line %>%
  group_by(groupe) %>%
  filter(session_year == min(session_year) | session_year == max(session_year)) %>%
  ungroup()

#######################################################
################# Traits écologiques ##################
#######################################################

df_trait = read.csv2("data/SpeciesTrait2024.csv")

df_radar_all = df_florileges_presence %>%
  select(taxon_nom_scientifique, taxon_combine, structure_id, structure_nom,
         session_id, site, site_id, code_region, nom_region) %>%
  mutate(loca = str_locate(site, "\\(")[,1],
         site = if_else(!is.na(loca), str_sub(site, 1, loca-1), site)) %>%
  left_join(df_trait, by = c("taxon_nom_scientifique" = "Plant"))

df_radar_reg = df_radar_all %>%
  filter(code_region == unique(df_florileges_struct$code_region))

df_radar_struct = df_radar_all %>%
  filter(structure_nom == structure_name)

min_nitro = min(df_radar_all$Nitrophilie, na.rm = T)
max_nitro = max(df_radar_all$Nitrophilie, na.rm = T)

min_entomo = min(df_radar_all$BioticVector, na.rm = T)
max_entomo = max(df_radar_all$BioticVector, na.rm = T)

min_vivace = min(df_radar_all$Vivace, na.rm = T)
max_vivace = max(df_radar_all$Vivace, na.rm = T)

reg_trait = df_radar_reg %>% group_by(site_id) %>%
  summarise(Nitrophilie = mean(Nitrophilie, na.rm = T),
            BioticVector = mean(BioticVector, na.rm = T),
            Vivace = mean(Vivace, na.rm = T),
            nb_sp = n_distinct(taxon_nom_scientifique, na.rm = T),
            typicite_prairie = length(which(str_detect(HABITAT, "prairie")))/n())
reg_nitro = mean(reg_trait$Nitrophilie, na.rm = T)
reg_entomo = mean(reg_trait$BioticVector, na.rm = T)
reg_vivace = mean(reg_trait$Vivace, na.rm = T)
reg_sp_flori = mean(reg_trait$nb_sp)
reg_typicite_prairie = mean(reg_trait$typicite_prairie)


#######################################################
################## Tables de données ##################
#######################################################

df_tb = df_florileges_presence_struct %>%
  filter(session_year == max(df_florileges_presence_struct$session_year)) %>%
  select(taxon, taxon_combine, NOM_VERN, site_id, site) %>%
  unique() %>% arrange(site_id)
