# Funksjoner til sammenligningsmodul
# Funksjonene lager to faner - density plot og boxplot


###----------------- BOXPLOT --------------------------------###################

#' Find variables over time
#'
#' @return values that can be used for selection
#'
#' @export

find_variables <- function(var) {

  variables <- dplyr::case_when({{var}} == "SRS22 totalskår" ~ c("SRS22_MAIN_SCORE",
                                                                 "SRS22_FULL_SCORE",
                                                                 "SRS22_FULL_SCORE_patient12mths",
                                                                 "SRS22_FULL_SCORE_patient60mths"),

                                {{var}} == "Funksjon" ~ c("SRS22_FUNCTION_SCORE",
                                                          "SRS22_FUNCTION_SCORE_patient3mths",
                                                          "SRS22_FUNCTION_SCORE_patient12mths",
                                                          "SRS22_FUNCTION_SCORE_patient60mths"),

                                {{var}} == "Selvbilde" ~ c("SRS22_SELFIMAGE_SCORE",
                                                           "SRS22_SELFIMAGE_SCORE_patient3mths",
                                                           "SRS22_SELFIMAGE_SCORE_patient12mths",
                                                           "SRS22_SELFIMAGE_SCORE_patient60mths"),

                                {{var}} == "Mental helse" ~ c("SRS22_MENTALHEALTH_SCORE",
                                                              "SRS22_MENTALHEALTH_SCORE_patient3mths",
                                                              "SRS22_MENTALHEALTH_SCORE_patient12mths",
                                                              "SRS22_MENTALHEALTH_SCORE_patient60mths"),

                                {{var}} == "Smerte" ~ c("SRS22_PAIN_SCORE",
                                                        "SRS22_PAIN_SCORE_patient3mths",
                                                        "SRS22_PAIN_SCORE_patient12mths",
                                                        "SRS22_PAIN_SCORE_patient60mths"),

                                {{var}} == "Helsetilstand" ~ c("HELSETILSTAND_SCALE",
                                                               "HEALTH_CONDITION_SCALE",
                                                               "HEALTH_CONDITION_SCALE_patient12mths",
                                                               "HEALTH_CONDITION_SCALE_patient60mths"),

                                {{var}} == "Tilfredshet" ~ c("SRS22_SATISFACTION_SCORE",
                                                             "SRS22_SATISFACTION_SCORE",
                                                             "SRS22_SATISFACTION_SCORE_patient12mths",
                                                             "SRS22_SATISFACTION_SCORE_patient60mths"))
}

#x <- find_variables("Funksjon")


#' Make table
#'
#' @return a dataframe that is in the long format
#' @export

make_comparability_table <- function(data, var) {

  variables <- find_variables({{var}})

  data_long <- data %>%
    select(Sykehus, all_of(variables)) %>%
    pivot_longer(cols = all_of(variables), names_to = "Punkt", values_to = "Score")

  return(data_long)
}

# nolint start
## r <- make_comparability_table(regdata, "Funksjon")
# nolint end

# Deretter gi nye navn til variablene slik at vi får "før operasjon", "3mnd", "2 år" og "5 år"

#' Making labels
#'
#' @return a dataframe with labels
#'
#' @export

new_labels <- function (data) {

  data <- data %>%
    mutate(Punkt = case_match(Punkt, c("SRS22_MAIN_SCORE",
                                       "SRS22_FUNCTION_SCORE",
                                       "SRS22_SELFIMAGE_SCORE",
                                       "SRS22_MENTALHEALTH_SCORE",
                                       "SRS22_PAIN_SCORE",
                                       "HELSETILSTAND_SCALE") ~ "Pre-operativt",

                              c("SRS22_FULL_SCORE",
                                "SRS22_FUNCTION_SCORE_patient3mths",
                                "SRS22_SELFIMAGE_SCORE_patient3mths",
                                "SRS22_MENTALHEALTH_SCORE_patient3mths",
                                "SRS22_PAIN_SCORE_patient3mths",
                                "HEALTH_CONDITION_SCALE",
                                "SRS22_SATISFACTION_SCORE") ~ "3 mnd",

                              c("SRS22_FULL_SCORE_patient12mths",
                                "SRS22_FUNCTION_SCORE_patient12mths",
                                "SRS22_SELFIMAGE_SCORE_patient12mths",
                                "SRS22_MENTALHEALTH_SCORE_patient12mths",
                                "SRS22_PAIN_SCORE_patient12mths",
                                "HEALTH_CONDITION_SCALE_patient12mths",
                                "SRS22_SATISFACTION_SCORE_patient12mths") ~ "12 mnd",

                              c("SRS22_FULL_SCORE_patient60mths",
                                "SRS22_FUNCTION_SCORE_patient60mths",
                                "SRS22_SELFIMAGE_SCORE_patient60mths",
                                "SRS22_MENTALHEALTH_SCORE_patient60mths",
                                "SRS22_PAIN_SCORE_patient60mths",
                                "HEALTH_CONDITION_SCALE_patient60mths",
                                "SRS22_SATISFACTION_SCORE_patient60mths") ~ "5 aar"))


  return(data)

}

# nolint start
## g <- new_labels(r)
# nolint end

# Ting som gjenstår å gjøre:
# Først filtrer ut NA-er
# Deretter tell antall observasjoner i hver gruppe (hvis færre enn 5, ikke vis)

#' Cleaning data frame
#'
#' @export

clean_comparability_table <- function(data, var) {

  data <- data %>%
    mutate(Punkt = forcats::as_factor(Punkt),
           Punkt = case_when({{var}} %in% c("SRS22 totalskår", "Funksjon",
                                            "Selvbilde", "Mental helse",
                                            "Smerte", "Helsetilstand") ~
                               forcats::fct_relevel(Punkt, "Pre-operativt",
                                                    "3 mnd", "12 mnd", "5 aar"),

                             {{var}} == "Tilfredshet" ~
                               forcats::fct_relevel(Punkt, "3 mnd",
                                                    "12 mnd", "5 aar")))


  data <- data %>%
    filter(!is.na(Score))

  data <- data %>%
    group_by(Punkt, Sykehus) %>%
    add_count(Punkt)

  data <- data %>%
    filter(n > 5)

  return(data)

}


# nolint start
##f <- clean_comparability_table(g, "Funksjon")
# nolint end



# Make data nice for plotting
#' GG data for boxplot
#'
#' @export

gg_data_comparability <- function (var) {

  gg_data <- data.frame(xlab = "", ylab = "")

  gg_data <- gg_data %>%
    dplyr::mutate(xlab = case_when({{var}} == "SRS22 totalskår" ~ "SRS22 totalskår (1: dårlig - 5: bra)",
                                   {{var}} == "Funksjon" ~ "SRS22 funksjon (1: dårlig - 5: bra)",
                                   {{var}} == "Selvbilde" ~ "SRS22 selvbilde (1: dårlig - 5: bra)",
                                   {{var}} == "Mental helse" ~ "SRS22 mental helse (1: dårlig - 5: bra)",
                                   {{var}} == "Smerte" ~ "SRS22 smerte (1: dårlig - 5: bra)",
                                   {{var}} == "Helsetilstand" ~ "Helsetilstand (0-100)",
                                   {{var}} == "Tilfredshet" ~ "SRS22 tilfredshet (1: dårlig - 5: bra)"),
                  ylab = "Skår")

}

# nolint start
## h <- gg_data_comparability("Funksjon")
# nolint end


# Make plot
#' Make boxplot
#'
#' @export

ggplot_comparability <- function (data, gg_data, input_data) {

  comp_plot = ggplot2::ggplot()

  comp_plot = comp_plot +
    ggplot2::geom_boxplot(data = data, aes(x = Punkt, y = Score), fill = "#6CACE4")+
    ggplot2::ylab(gg_data$ylab)+
    ggplot2::xlab("Utvikling over tid")+
    ggplot2::labs(title = gg_data$xlab,
                  caption = paste0("**Valgte variabler:**", "\n", input_data[1,], ", ", input_data[2,], "\n",
                                   input_data[3,], "-", input_data[4,], "\n",
                                   input_data[5,], "-", input_data[6,]))+
    ggplot2::theme_light(base_size = 16)+
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                   plot.title = element_text(size = 10,
                                             face = "bold"),
                   plot.caption = element_text(size = 12,
                                               face = "italic", color = "#87189D"))

  return(comp_plot)

}

# nolint start

#input_data <- c("Funksjon", "kvinne", "10/01/23", "10/01/24", "10", "15")
## j <- ggplot_comparability(f, h, input_data)
## j
# nolint end


###------------- DENSITY -------------##########################################

# Explanation of steps taken to create the plot (see module_sammenligning)
# 1. find_variables()
# 2. make_comparability_table()
# 3. new_labels()
# 4. clean_comparability_table()
# 5. find two variables based on UI-choices
# 6. make density plot


#' Find exact variables
#'
#' @return two variables chosen by UI
#'
#' @export

find_comp_variables <- function (data, choice_comp) {

  data <- data %>%
    dplyr::filter(Punkt == dplyr::case_when({{choice_comp}} == "Før operasjon - 3 mnd" ~ "Pre-operativt",
                                            {{choice_comp}} == "Før operasjon - 12 mnd" ~ "Pre-operativt",
                                            {{choice_comp}} == "Før operasjon - 5 år" ~ "Pre-operativt",
                                            {{choice_comp}} == "3 mnd - 12 mnd" ~ "3 mnd",
                                            {{choice_comp}} == "3 mnd - 5 år" ~ "3 mnd",
                                            {{choice_comp}} == "12 mnd - 5 år" ~ "12 mnd") |
                    Punkt == dplyr::case_when({{choice_comp}} == "Før operasjon - 3 mnd" ~ "3 mnd",
                                              {{choice_comp}} == "Før operasjon - 12 mnd" ~ "12 mnd",
                                              {{choice_comp}} == "Før operasjon - 5 år" ~ "5 aar",
                                              {{choice_comp}} == "3 mnd - 12 mnd" ~ "12 mnd",
                                              {{choice_comp}} == "3 mnd - 5 år" ~ "5 aar",
                                              {{choice_comp}} == "12 mnd - 5 år" ~ "5 aar")
  )
}

# nolint start
## r <- find_comp_variables(f, "Før operasjon - 12 mnd")
# nolint end


#' Plot for comparison
#'
#' @return a density plot
#'
#' @export

density_plot_comparability <- function(data, labels, input_data) {

  sam_plot <- ggplot2::ggplot(data = data, aes(x = Score, fill = Punkt))+
    geom_density(alpha = .3)+

    ggplot2::scale_fill_manual(values = c("#6CACE4","#003087"))+
    ggplot2::xlab(labels$xlab) +
    ggplot2::ylab("Tetthet")+
    ggplot2::labs(
      caption = paste0("**Valgte variabler:**", "\n", input_data[1,], ", ", input_data[2,], "\n",
                       input_data[3,], "-", input_data[4,], "\n",
                       input_data[5,], "-", input_data[6,]))+
    ggplot2::guides(fill = guide_legend(""))+
    ggplot2::theme_light(base_size = 16)+
    ggplot2::theme(plot.title = element_text(size = 10,
                                             face = "bold"),
                   plot.caption = element_text(size = 12,
                                               face = "italic", color = "#87189D"))

  return(sam_plot)

}

# nolint start

#input_data <- c("Funksjon", "kvinne", "10/01/23", "10/01/24", "10", "15")

##p <-  density_plot_comparability(r, h)
##p
# nolint end


