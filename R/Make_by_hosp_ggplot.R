#' Make plots by hospital
#'
#'
#'

gg_bar_by_hos <- function(data, x_var, q, w, l){

  plot.data = data %>%
    dplyr::select({{x_var}})

  colnames(plot.data) = "z"

  plot =
    ggplot2::ggplot(plot.data, aes(     # opens ggplot
      x = z))+                            # varies by input

    geom_bar(                             # opens bar-plot
      fill = "#6CACE4",                   # choosing colors on columns
      color = "#BFCED6",                  # choosing colors in column outlines
      alpha = 0.8)+                       # setting alpha

    theme_light()+                        # choosing theme

    xlab(q)+                              # title of x-axis
    ylab("Antall")+                        # title of y-axis ## DETTE BØR BLI ANDELL ETTER HVERT!! ##

    geom_text(label = paste(l, "\n ", w),
              x = "Bergen",
              y = 140, color = "#003087",
              family = "mono",
              fontface = "plain",
              size = 4)+

    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


  return(plot)
}

# gg_bar_by_hos(regdata, CENTRESHORTNAME, "Fordeling på sykehus", "Sykehus", " ")
