#### MAKE ANDELS PLOT ####

######### maybe add something about how if the "andel" is lower than 2%, the column will be value = 0, label = "lower than 0.02" #######
#### OR SOMETHING?? ####




# Make table for the column of choice
table <- make_table(df, "Bergen")

# Make a plot

MakePlot_gg <- function(data) {

  table <- data %>%
    rename(var = colnames(table[2]))

  table1 <- data %>%
    filter(Sykehus != "Resten")

  table2 <- data %>%
    filter(Sykehus == "Resten")


  label = c(paste(table1[1,1],"n:", table1[1,4]), paste(table2[1,1], "n:", table2[1,4]))


  plot = ggplot() +
    geom_col(data = table1, aes(x = var, y = andel, color = Sykehus), fill = "#6CACE4")+  #fill = "#6CACE4", color = "#6CACE4")+


    geom_point(data= table2, aes(x= var, y = andel, color = Sykehus), shape = 23, fill = "Darkblue")+ #, color = "Darkblue",
               #fill = "Darkblue", shape = 23)+

    xlab(gg_data$xlab)+
    ylab("Andel")+
    labs(title = gg_data$title,
         caption = paste0("Valgte variabler:", "\n", colnames(table1[2])),
         color = "Sykehus")+

    theme(plot.caption = element_text(color = "Darkblue", face = "italic"))+

    theme_light()+

    scale_color_manual(values = c("#6CACE4", "Darkblue", "#6CACE4"), labels = label)

return(plot)

}

MakePlot_gg(table)

