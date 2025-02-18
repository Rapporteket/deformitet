#### FUNCTION FOR MAKING PLOT FOR KVALITETSINDIKATORER #########################
#### GGPLOT2 ###################################################################

# data should be data that has been undergoing PrepVar() and kval_count()
# gg_data is made under PrepVar()
# data_var should be reactive data that stores UI choices for PrepVar
# choice_kjønn will be a radio button indicating "fordelt på kjønn?" "ja" vs. "nei"

#' @export

kval_plot <- function(data, gg_data, data_var, choice_kjønn){


  kval_plot <- data %>%
    dplyr::filter(dplyr::case_when({{choice_kjønn}} != "begge" ~
                                     Kjønn == "kvinne" |
                                     Kjønn == "mann" |
                                     Kjønn == "begge",
                                   TRUE ~ Kjønn == "begge")) %>%

    ggplot2::ggplot(aes(x = Kjønn, y = andel_per_syk, fill = Kjønn))+

    ggplot2::geom_col(alpha = .7)+

    ggplot2::geom_hline(yintercept = gg_data$yintercept, linetype = "dashed", color = "#87189D")+

    # ggplot2::geom_rect(aes(ymin = 0, ymax = 5, xmin = x_start, xmax = x_end), alpha = .5)+

    ggplot2::scale_y_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100))+

    #ggplot2::geom_col(data = kval, aes(x = Sykehus, y = andel_per_syk), color = "#003087" )+

    #### TITLES ################################################################

    ggplot2::xlab(gg_data$xlab)+

    ggplot2::ylab("Andel pasienter (%)")+

    ggplot2::labs(title = gg_data$title,
                  caption = paste0("**Valgte variabler:**", "\n", data_var[1,],
                                   ", ", "\n", "Kjønn - ", data_var[2,], "\n",
                                   data_var[3,], "-", data_var[4,], "\n",
                                   data_var[5,], "-", data_var[6,]))+

    ggplot2::geom_label(aes(y = -.5, label = paste(antall_kval_syk, "av", per_syk)),
                        fill = "#BFCED6", color = "#003087", fontface = "italic",
                        position = position_dodge(.9), vjust = -.2, size = 3,
                        alpha = .8)+


    ##### THEME AND COLOURS ####################################################

  ggplot2::theme_light()+ # light theme

    ggplot2::theme(plot.caption = element_text(color = "#87189D", # add caption
                                               face = "italic"),
                   legend.position = case_when({{choice_kjønn}} == "begge" ~ "none",
                                               TRUE ~ "right"),
                   axis.text.x = element_text(color = case_when({{choice_kjønn}} == "nei" ~ "white",
                                                                TRUE ~ "black"), size = 14),
                   axis.text.y = element_text(size = 14))+

    ggplot2::scale_fill_manual(values = c("#6CACE4", "#6FA287", "#BFCED6"))+

    ##### FACETING #############################################################


  ggplot2::facet_wrap(~Sykehus)


  return(kval_plot)
}


# Test to see if it works:
## kval_plot(kval, gg_data, bla, "no")
## required dataframe
## bla <- data.frame(d_var)

