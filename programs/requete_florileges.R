# HEADER --------------------------------------------
#
# Author:     Maël Pretet
# Copyright     Copyright 2024 - Maël Pretet
# Email:      mael.pretet1@mnhn.fr
#
# Date:     2025-03-24
#
# Script Name:    fonctions/requete_florilege.R
#
# Script Description:   Création du data frame opération papillons avec toutes
#   les espèces. Interrogation de la base de données mosaic et enregistrement
#   dans un fichier rds. Si le fichier existe déjà et qu'il date de moins d'une
#   semaine, celui-ci est directement lu.
#
#
# ------------------------------------

library(dplyr)
library(here)
library(httr)
library(readr)

if (Sys.getenv("CI") != "true") {
  readRenviron(".env")
}
source("programs/function_import_from_mosaic.R")
is_intranet <- function(url = "https://virtualianet.mnhn.fr/") {
  tryCatch({
    response <- GET(url)
    return(status_code(response) == 200)
  }, error = function(e) {
    return(FALSE)
  })
}

### Dataframe des données pour toutes les espèces
# -----------------------------------------------


if (!file.exists("data/rdata/df_florileges.rds") & !is_intranet()) {
  stop("Le fichier n'existe pas et ne peut pas être créé.")
}else if ((!file.exists("data/rdata/df_florileges.rds") |                                 # Si le fichier n'existe pas OU
          (strftime(Sys.Date(), "%A") == "lundi" &                                     #  [que la date du jour est un lundi ET
           Sys.Date()-as.Date(file.info("data/rdata/df_florileges.rds")$ctime) > 5)) &    #   que le fichier a plus de 5 jours]
          is_intranet()) {    
  # Lecture depuis la base mosaic
  df_florileges = import_from_mosaic(query = read_sql_query("sql/florilege_export_a_plat_standard.sql"),
                                  database_name = "espaces_verts")
  
  # On sauvegarde si on ne se trouve pas sur le serveur gitlab
  if (Sys.getenv("CI") != "true") {
    # Sauvegarde du df en format RDS
    saveRDS(object = df_florileges, file = "data/rdata/df_florileges.rds")
  }
  
}else{
  # Lecture du fichier RDS
  df_florileges = readRDS("data/rdata/df_florileges.rds")
}

