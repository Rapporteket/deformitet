#' @title Make gg_plot
#'
#' @export
#'
#'
#### MAKE ANDELS PLOT ####

######### maybe add something about how if the "andel" is lower than 2%, the column will be value = 0, label = "lower than 0.02" #######
#### OR SOMETHING?? ####


# Make a plot

makePlot_gg <- function(data, gg_data, data_var) {

  table <- data.frame(data)

  table <- table %>%
    dplyr::rename(var = colnames(table[2]))

  table1 <- table %>%
    dplyr::filter(Sykehus != "Resten")

  table2 <- table %>%
    filter(Sykehus == "Resten")


  table1 <- table1 %>%
    mutate(Sykehus = paste(table1[1,1], "n:", table1[1,4]))


  table2 <- table2 %>%
    mutate(Sykehus = paste(table2[1,1], "n:", table2[1,4]))


  label = c(paste(table1[1,1],"n:", table1[1,4]), paste(table2[1,1], "n:", table2[1,4]))


  plot = ggplot() +
    geom_col(data = table1, aes(x = var, y = andel, color = Sykehus), fill = "#6CACE4")+  #fill = "#6CACE4", color = "#6CACE4")+


    geom_point(data= table2, aes(x= var, y = andel, color = Sykehus), shape = 23, fill = "#003087", size = 2.5)+ #, color = "Darkblue",
    #fill = "Darkblue", shape = 23)+

    xlab(gg_data$xlab)+
    ylab("Andel")+
    labs(title = gg_data$title,
         caption = paste0("**Valgte variabler:**", "\n", data_var[1,], ", ", data_var[2,], "\n",
                          data_var[3,], "-", data_var[4,], "\n",
                          data_var[5,], "-", data_var[6,]))+


    theme_light()+

    theme(plot.caption = element_text(color = "#87189D", face = "italic"))+



    scale_color_manual(values = c("#6CACE4", "#6CACE4", "#003087"))


  return(plot)

}


# data_var_fff <- data.frame("Valg"= c("BMI_kategori", "kvinne", "10/01/23", "10/01/24", "10", "15"))
# d_var <- c("BMI_kategori", "kvinne", "10/01/23", "10/01/24", "10", "15")
#
#
# makePlot_gg(table, gg_data, data_var_fff)


