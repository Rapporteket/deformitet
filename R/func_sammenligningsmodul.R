#' Check for small sample size
#'
#' @return a list of data frames
#'
#' @export

check_small_sample <- function (data, var1, var2) {

  # Remove NAs and add sample count
  data1 <- data %>%
    dplyr::filter(!is.na(.data[[var1]])) %>%
    dplyr::add_count(name = "count_var1")

  data2 <- data %>%
    dplyr::filter(!is.na(.data[[var2]])) %>%
    add_count(name = "count_var2")

  # If one (or both) of the chosen variables has less than 20 observations

  if (data1$count_var1[1] < 20) {
    data1[[var1]] = 0
  } else {
    data1 <- data1
  }

  if (data2$count_var2[1] < 20) {
    data2[[var2]] = 0
  } else {
    data2 <- data2
  }

  return (list(data1, data2))
}

# nolint start
## SJEKK AT DET FUNGERER:

## f <- check_small_sample(g, "SRS22_MAIN_SCORE", "SRS22_FULL_SCORE") # vi returnerer ei liste
## plot_data1 <- data.frame(f[1])
## plot_data2 <- data.frame(f[2])

## g[[2]]$SRS22_FULL_SCORE_patient12mths # sjekk at the fungerer
# nolint end

#' Make labels
#'
#' @return a dataframe with conditional labels
#'
#' @export

make_labels <- function (data1, data2, comp1, comp2) {
  labels = data.frame(var1 = 0,
                      var2 = 0,
                      ggtitle = "",
                      ggcaption = "")

  labels$var1 <- data1$count_var1[1] # var1 is the number of observations in variable1 that are not NAs
  labels$var2 <- data2$count_var2[1] # var2 is the number of observations in variable2 that are not NAs

  ## Conditional ggtitle
  if (labels$var1[1] < 20 && labels$var2[1] < 20) {
  labels$ggtitle <- "For få observasjoner i begge valgte variabler"
  } else {
    if (labels$var1[1] < 20) {
      labels$ggtitle <- paste0("For få observasjoner i ", comp1)
    } else {
      if (labels$var2[1] < 20) {
        labels$ggtitle <- paste0("For få observasjoner i ", comp2)
      } else {
        labels$ggtitle <- paste0("Sammenligning av ", comp1, " og ", comp2)
      }
    }
  }

  ## Conditional ggcaption
  if (labels$var1[1] - labels$var2[1] > 50) {
    labels$ggcaption <- "Obs! Det er store forskjeller mellom de to variablene i antall observasjoner"
  }

  return(labels) #returns the data set
}

# nolint start
## check that it works
## labels <- make_labels(plot_data1, plot_data2, "SRS22_MAIN_SCORE", "SRS22_FULL_SCORE")
# nolint end

#' Plot for comparison
#'
#' @return a plot
#'
#' @export

comparison_plot_continuous <- function(data1, data2, labels, comp1, comp2) {

  # Make a new data frame with count of observations for var1 and var2
  # Add conditional ggtitle and ggcaption
  # This function needs two dataframes to exist in advance -
      # plot_data1 and plot_data2

  sam_plot = ggplot2::ggplot()

  sam_plot = sam_plot +
  # Comp 1:
    ggplot2::geom_histogram(data = data1, binwidth = .3, aes(x = .data[[comp1]], color = "før"),
                            fill = "#6CACE4", alpha = .2) +
    # Comp2 2:
    ggplot2::geom_histogram(data = data2, binwidth = .3, aes(x = .data[[comp2]], color = "etter"),
                                fill = "#003087", alpha = .2) +


    ggplot2::theme(legend.position = "right")+
    ggplot2::guides(color = guide_legend(""))+

    ggplot2::scale_color_manual(values = c("før" = "#6CACE4", "etter" = "#003087"),
                                limits = c("før", "etter"),
                                labels = c(paste0("Før (n= ", labels$var1, ")"),
                                           paste0("Etter (n= ", labels$var2, ")")))+
    ggplot2::xlab("Fordeling") +
    ggplot2::ylab("Andel pasienter")+
    ggplot2::labs(
      title = labels$ggtitle,
      caption = labels$ggcaption)+

    ggplot2::theme_light()+
    ggplot2::theme(plot.title = element_text(size = 10,
                                             face = "bold"),
                   plot.caption = element_text(size = 12,
                                               face = "italic", color = "#87189D"))

  return(sam_plot)

}

# nolint start
## p <-  comparison_plot_continuous(plot_data1, plot_data2, labels, "PRE_MAIN_CURVE", "POST_MAIN_CURVE")
## p
# nolint end

#' Plot for comparison
#'
#' @return a plot
#'
#' @export

comparison_plot_discrete <- function(data1, data2, labels, comp1, comp2) {

  # Make a new data frame with count of observations for var1 and var2
  # Add conditional ggtitle and ggcaption
  # This function needs two dataframes to exist in advance -
  # plot_data1 and plot_data2

  sam_plot = ggplot2::ggplot()

  sam_plot = sam_plot +
    # Comp 1:
    ggplot2::geom_bar(data = data1, aes(x = .data[[comp1]], color = "før"),
                        fill = "#6CACE4", alpha = .3) +

    # Comp2 :
    ggplot2::geom_bar(data = data2, aes(x = .data[[comp2]], color = "etter"),
                      fill = "#003087", alpha = .3) +

    ggplot2::theme(legend.position = "right")+
    ggplot2::guides(color = guide_legend(""))+

    ggplot2::scale_color_manual(values = c("før" = "#6CACE4", "etter" = "#003087"),
                                limits = c("før", "etter"),
                                labels = c(paste0("Før (n= ", labels$var1, ")"),
                                           paste0("Etter (n= ", labels$var2, ")")))+
    ggplot2::xlab("Fordeling") +
    ggplot2::ylab("Andel pasienter")+
    ggplot2::labs(
      title = labels$ggtitle,
      caption = labels$ggcaption)+

    ggplot2::theme_light()+
    ggplot2::theme(plot.title = element_text(size = 10,
                                             face = "bold"),
                   plot.caption = element_text(size = 12,
                                               face = "italic", color = "#87189D"))

  return(sam_plot)

}

# nolint start
##
##p <-  comparison_plot_discrete(plot_data1, plot_data2, labels, "Kurve_pre", "Kurve_post")
##p
## p
# nolint end

#'@title Sammenligningstabell
#'
#'@export

tabell_sam <- function (data, comp1, comp2) {

  comp1 <- enquo(comp1)
  comp2 <- enquo(comp2)

  data <- data %>%
    dplyr::filter(!is.na(.data[[comp1]]))

  data <- data %>%
    dplyr::filter(!is.na(.data[[comp2]])) %>%
    dplyr::select(Sykehus, Kjønn, !!comp1, !!comp2) %>%
    dplyr::mutate(diff_pre_post = .data[[comp1]] - .data[[comp2]]) %>%
    dplyr::group_by(Sykehus, Kjønn) %>%
    dplyr::summarise(mean_diff = mean(diff_pre_post)) %>%
    dplyr::mutate(mean_diff = round(mean_diff,3))

}

# nolint start
## SJEKK AT DET FUNGERER:
##
##
## f <- tabell_sam(regdata, "SRS22_MAIN_SCORE", "SRS22_FULL_SCORE")
# nolint end

#'@title Sammenligningstabell - grupper av faktorer
#'
#'@export

tabell_sam_discrete <- function(data, comp1, comp2) {

  comp1 <- enquo(comp1)
  comp2 <- enquo(comp2)

  data <- data %>%
    dplyr::filter(!is.na(.data[[comp1]]))

  data <- data %>%
    dplyr::filter(!is.na(.data[[comp2]])) %>%
    dplyr::select(Sykehus, Kjønn, !!comp1, !!comp2)

  data_post <- data %>%
    dplyr::group_by(Sykehus, Kjønn) %>%
    dplyr::rename(var := !!comp2) %>%
    dplyr::count(var, name = "count_post")

  data_pre <- data %>%
    dplyr::group_by(Sykehus, Kjønn) %>%
    dplyr::rename(var := !!comp1) %>%
    dplyr::count(var, name = "count_pre")


  data <- dplyr::full_join(data_pre, data_post)

  return (data)
}

# nolint start
## SJEKK AT DET FUNGERER:
## f <- tabell_sam_discrete(regdata, "Helsetilstand", "Helsetilstand_3mnd")
# nolint end

