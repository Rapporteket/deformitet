#### MAKE ANDELS PLOT ####

######### maybe add something about how if the "andel" is lower than 2%, the column will be value = 0, label = "lower than 0.02" #######
#### OR SOMETHING?? ####



# Prep data on the column of choice
res= prep(regdata, Alder)

# Make table for the column of choice
table <- make_table(df, Alder, "St.Olav")


ggplot() +
  geom_col(data = table1, aes(x = Alder, y = andel), fill = "#6CACE4")+


  geom_point(data= table2, aes(x= Alder, y = andel), color = "Darkblue",
             fill = "Darkblue", shape = 23)+

  xlab(gg_data$xlab)+
  ylab("Andel")+
  labs(title = gg_data$title,
       caption = paste0("Valgte variabler:", "\n", "Alder"))+

  theme(plot.caption = element_text(color = "Darkblue", face = "italic"))+

  theme_light()




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
              x = "Alvorlig undervekt",
              y = 140, color = "#003087",
              family = "mono",
              fontface = "plain",
              size = 4)+

    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


  return(plot)
}
