

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
