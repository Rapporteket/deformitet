#' @title Make gg_plot for fordelingsfirgurer og -tabeller
#'
#' @export
#### MAKE ANDELSPLOT ####

######### maybe add something about how if the "andel" is lower than 2%, the column will be value = 0, label = "lower than 0.02" #######
#### OR SOMETHING?? ####

################################################################################
################## MAKING PLOT--------------------------------------------------

makePlot_gg <- function(data, gg_data, data_var, choice_var) {

  #### TIDYING DATA -------


  table <- data.frame(data)

  table <- table %>%
    dplyr::rename(var = colnames(table[2]))


  # Split data into two tables - alle vs. valgt enhet

  if (choice_var == "hele landet"){
    ## browser() ==> sett inn for å sjekke koden
    table1 <- table %>%
      dplyr::filter(Sykehus != "Alle")

    table2 <- table %>%
      dplyr::filter(Sykehus == "Alle")

    table1 <- table1 %>%
      dplyr::mutate(Sykehus = paste(table1[,1], "n:", table1[,3]))

    table2 <- table2 %>%
      dplyr::mutate(Sykehus = paste(table2[,1], "n:", table2[,3]))

    label = c(paste(table1[,1],"n:", table1[,3]), paste(table2[,1], "n:", table2[,3]))
  }

  else{
    table <- table %>%
    dplyr::rename(var = colnames(table[2])) %>%
    dplyr::mutate(Sykehus = paste(table[,1], "n:", table[,3]))

    label = paste(table[,1],"n:", table[,3])
  }


  # Making plot

  fig_plot = ggplot()

    # Columns for hospital user belongs to

    # Point for "resten"

  if (choice_var == "hele landet"){ # => med sammenligning
    fig_plot = fig_plot+
      ggplot2::geom_col(data = table2, aes(x = var, y = Prosent,
                                           color = Sykehus), fill = "#6CACE4")+
      ggplot2::geom_point(data= table1, aes(x= var, y = Prosent,
                                            color = Sykehus), shape = 23,
                          fill = "#003087", size = 2.5)+

      ggplot2::scale_color_manual(values = # adding chosen colors
                                  c("#6CACE4", "#6CACE4", "#6CACE4", "#6CACE4", "#6CACE4", "#003087", "#003087"))
  }

  if (choice_var != "hele landet"){ # => hvert sykehus og hele landet uten sammenligning
    fig_plot = fig_plot+
      ggplot2::geom_col(data = table, aes(x = var, y = Prosent, fill = Sykehus), alpha = .9)+
      ggplot2::facet_wrap(~Sykehus)+

      ggplot2::scale_fill_manual(values = # adding chosen colors
                                    c("#6CACE4", "#ADDFB3", "#87189D", "black"))
  }


    # Change names of labels
  fig_plot = fig_plot +
    ggplot2::xlab(gg_data$xlab)+
    ggplot2::ylab("Andel")+
    ggplot2::labs(title = gg_data$title,
                  caption = paste0("**Valgte variabler:**", "\n", data_var[1,], ", ", data_var[2,], "\n",
                                   data_var[3,], "-", data_var[4,], "\n",
                                   data_var[5,], "-", data_var[6,]))+


    ggplot2::theme_bw()+ # light theme

    ggplot2::theme(plot.caption = element_text(color = "#87189D", # add caption
                                               face = "italic"))



  return(fig_plot)

}


### Testkode for å sjekke om funksjonen fungerer -------------------------------

# data_var_fff <- data.frame("Valg"= c("BMI_kategori", "kvinne", "10/01/23", "10/01/24", "10", "15"))
# d_var <- c("BMI_kategori", "kvinne", "10/01/23", "10/01/24", "10", "15")


# makePlot_gg(d, gg_data, data_var_fff, "egen enhet")
# d <- makeTable(f, 103240, "egen enhet")


