
graph_donut <- function(df_donut, ymin = "ymin", ymax = "ymax",
                        xmin = 5, xmax = 10, fill = "taxon", vec_col,
                        xlim_min = 0, xlim_max = 18,
                        xlab = 15, ylab = "label_position", txtlab = "taxon"){
  return(
    ggplot(df_donut,
           aes(ymin = !!sym(ymin), ymax = !!sym(ymax),
               xmin = xmin, xmax = xmax,
               fill = !!sym(fill)) ) +
      geom_rect(fill = vec_col) +
      coord_polar(theta = "y") +
      xlim(c(0, 18)) +
      geom_label(aes(x = xlab, y = !!sym(ylab),
                     label = !!sym(txtlab), vjust = 0),
                 color = vec_col,
                 fill = "#f8f8f8", size = 3) +
      theme_void() + 
      theme(legend.position = "none")
  )
}

# Histogramme de proportion des abondances d'espèces
#' gg_histo_plotly
#' 
#' @param df_hp A dataframe
#' @param x A character (column's dataframe)
#' @param y A character (column's dataframe)
#' @param fill A character (column's dataframe)
#' @param title A character 
#' @param ytxt A character 
#' @param limits A character vector
#' @param breaks A character vector
#' @param couleur A character vector 
#' 
#' @returns A ggplotly object.
#' 
#' @examples
#' gg_histo_plotly(df_hp = data.frame(nom = c("A", "B", "C"),
#'                                    value = c(0.23, 0.11, 0.219)),
#'                 x = "nom", y = "value", fill = "nom", title = "Proportions",
#'                 limits = c("B", "C", "A"), couleur = c("red", "grey", "grey"))
gg_histo_plotly <- function(df_hp, x = "nom_espece", y = "rel_ab",
                            fill = "nom_espece", 
                            title = "Proportion d'abondance de chaque espèce parmi toutes les observations", 
                            ytxt = "% d'abondance",
                            limits, couleur, percent = TRUE){
  
  if (percent) {
    gg <- df_hp %>%
      ggplot() +
      # On effectue un mapping selon les colonnes x et y, et on colore les
      # histogrammes selon la colonne fill
      geom_bar(mapping = aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill),
                             # On ajoute un argument 'text" pour ggplotly
                             # ("x" : "y" [au format % avec une précision au centième 12.9062 -> 12.91])
                             text = paste0(!!sym(x), " : ",
                                           scales::percent(!!sym(y), accuracy = 0.01))),
               stat = "identity") +
      # Mise de l'axe y au format % en précision 1 (21.2% -> 21%)
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  }else{
    gg <- df_hp %>%
      ggplot() +
      # On effectue un mapping selon les colonnes x et y, et on colore les
      # histogrammes selon la colonne fill
      geom_bar(mapping = aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill),
                             # On ajoute un argument 'text" pour ggplotly
                             # ("x" : "y" [pas de format])
                             text = paste0(!!sym(x), " : ", !!sym(y))),
               stat = "identity")
  }
  
  gg = gg +
    scale_x_discrete(limits=limits) +       # Fonction pour ordonner l'axe x
    ylab(ytxt) +
    ggtitle(title) +
    theme_cowplot() +
    theme(axis.text.y = element_text(size = 9),
          axis.title.y = element_blank(),   # Suppression du nom de l'axe y
          title = element_text(size = 9),   # Titre en taille 9
          legend.position = "none" ) +      # Suppression de la légende
    # Fonction pour colorer les histogrammes selon des valeurs choisies (dans le même ordre que l'axe x)
    scale_fill_manual(breaks = limits,
                      values = couleur) +
    # Inversion des axes x et y pour que les histogrammes soient horizontaux
    coord_flip()
  
  # On utilise ggplotly avec l'argument "tooltip" pour que le "text" s'affiche
  # quand on passe la souris sur les histogrammes
  return(ggplotly(gg, tooltip = "text"))
}

suivi_temporel_transects <- function(df, x = "year", y = "transect_id", group = "groupe",
                                     color = "#aa3839", size_p = 1.5, vec_limits_x,
                                     limits_y=c(), labels_y=c(), xlab = "Années",
                                     ylab = "Nom du transect", grid = FALSE){
  # Data frame pour avoir les points aux extrémités des lignes
  df_points <- df %>%
    group_by(!!sym(group)) %>%
    filter(!!sym(x) == min(!!sym(x)) | !!sym(x) == max(!!sym(x))) %>%
    ungroup()
  
  gg <- ggplot(df, aes(x = !!sym(x), y = !!sym(y))) +
    geom_line(aes(group = !!sym(group)), color = color) +
    geom_point(data = df_points, mapping = aes(x = !!sym(x), y = !!sym(y)),
               size = size_p, color = color) +
    scale_x_continuous(limits = c(min(as.integer(vec_limits_x), na.rm = T)-1,
                                  max(as.integer(vec_limits_x), na.rm = T)+1)) +
    theme_minimal() + xlab(xlab) + ylab(ylab)
  
  if (grid & ("site" %in% colnames(df))) {
    gg <- gg +
      facet_grid(site ~ ., scales = "free_y") +
      scale_x_continuous(limits = c(min(as.integer(vec_limits_x), na.rm = T)-1,
                                    max(as.integer(vec_limits_x), na.rm = T)+1),
                         sec.axis = dup_axis()) +
      theme(strip.text.y = element_text(angle = 0),
            axis.text.x.top = element_text())
  }else if(length(limits_y)>0 & length(labels_y)>0){
    gg <- gg + scale_y_discrete(limits = limits_y, labels = labels_y)
  }
  
  return(gg)
}
