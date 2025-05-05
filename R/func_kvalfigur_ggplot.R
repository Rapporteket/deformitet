#### FUNCTION FOR MAKING PLOT FOR KVALITETSINDIKATORER #########################
#### GGPLOT2 ###################################################################

# data should be data that has been undergoing PrepVar() and kval_count()
# gg_data is made under PrepVar()
# data_var should be reactive data that stores UI choices for PrepVar
# choice_kjønn will be a radio button indicating "fordelt på kjønn?" "ja" vs. "nei"

#' @export

kval_plot <- function(data, gg_data, data_var, choice_kjønn){


  plotdata <- data %>%
    dplyr::filter(dplyr::case_when({{choice_kjønn}} != "begge" ~
                                     Kjønn == "kvinne" |
                                     Kjønn == "mann" |
                                     Kjønn == "begge",
                                   TRUE ~ Kjønn == "begge"))

  kval_plot <-
    ggplot2::ggplot(data = plotdata, aes(x = andel_per_syk, y = Sykehus, fill = Kjønn))+

    annotate('rect', xmin = gg_data$ymin, xmax = gg_data$ymax,
             ymin=-Inf, ymax=Inf, alpha=0.2, fill="palegreen4") +

    ggplot2::geom_col(alpha = .7)+

    #ggplot2::geom_hline(xintercept = gg_data$yintercept, linetype = "dashed", color = "#87189D")+

    # ggplot2::geom_rect(aes(ymin = 0, ymax = 5, xmin = x_start, xmax = x_end), alpha = .5)+

    ggplot2::scale_x_continuous(breaks = c(0, 5, 10,20,30,40,50,60,70,80,90,100))+

    #ggplot2::geom_col(data = kval, aes(x = Sykehus, y = andel_per_syk), color = "#003087" )+

    #### TITLES ################################################################

    ggplot2::xlab("Andel pasienter (%)")+

    ggplot2::ylab(gg_data$ylab)+

    ggplot2::labs(title = gg_data$title,
                  caption = paste0("**Valgte variabler:**", "\n", data_var[1,],
                                   ", ", "\n", "Kjønn - ", data_var[2,], "\n",
                                   data_var[3,], "-", data_var[4,], "\n",
                                   data_var[5,], "-", data_var[6,]))+

    ggplot2::geom_label(aes(x = 0, label = paste(antall_kval_syk, "av", per_syk)),
                        fill = "#BFCED6", color = "#003087", fontface = "italic",
                        position = position_dodge(.9), vjust = -.2, size = 3,
                        alpha = .8)+


    ##### THEME AND COLOURS ####################################################

    ggplot2::theme_bw(base_size = 16)+ # light theme

    ggplot2::theme(plot.caption = element_text(color = "#87189D", # add caption
                                               face = "italic"),
                   legend.position = case_when({{choice_kjønn}} == "begge" ~ "none",
                                               TRUE ~ "right"),
                   axis.text.x = element_text(color = case_when({{choice_kjønn}} == "nei" ~ "white",
                                                                TRUE ~ "black"), size = 14),
                   axis.text.y = element_text(size = 14))+

    ggplot2::scale_fill_manual(values = c("#6CACE4", "#6FA287", "#BFCED6"))

    ##### FACETING #############################################################


  #ggplot2::facet_wrap(~Kjønn)


  return(kval_plot)
}

