#'@title Preprocess for SPC-function
#'
#'@export

prepros_SPC <- function (data, var, time_grouping) {


  ## REMOVE ALL NS #############
  spc_data <- data %>%
    dplyr::filter(!is.na(.data[[var]]))

  # ## GROUP BY CHOICE ###########

  spc_data <- spc_data %>%
    dplyr::group_by(tidsperiode = cut(.data$SURGERY_DATE, time_grouping)) %>%
    dplyr::summarize(value = mean(.data[[var]]))  %>%
    dplyr::mutate(value = round(value, 2))

  spc_data$tidsperiode <- as.Date(spc_data$tidsperiode)



  return (spc_data)

}

# no lint start
## Check to test whether it works:
## h <- prepros_SPC(g, "PRE_MAIN_CURVE", "week")
# no lint end


#'@title Text function for spc-plot
#'
#'@export

spc_text <- function (var) {

  spc_text_df <- data.frame(y_axis_label = "")


  spc_text_df <- spc_text_df %>%
    dplyr::mutate(y_axis_label = dplyr::case_when({{var}} == "PRE_MAIN_CURVE" ~
                                                    "Grader, pre-operativ kurve",
                                                  {{var}} == "BED_DAYS_TOTALT" ~
                                                    "Liggetid, antall dager",
                                                  {{var}} == "PER_BLOOD_LOSS_VALUE" ~
                                                    "Blodtap (ml)"),
                  main_title = dplyr::case_when({{var}} == "PRE_MAIN_CURVE" ~
                                                  "SPC-graf for pre-operativ kurve",
                                                {{var}} == "BED_DAYS_TOTALT" ~
                                                  "SPC-graf for antall dager liggetid i sykehus",
                                                {{var}} == "PER_BLOOD_LOSS_VALUE" ~
                                                  "SPC-graf for antall ml blodtap under operasjon"))

  return(spc_text_df)
}


# nolint start
## Test to see if it works:
## ggg <- spc_text("PRE_MAIN_CURVE")
# nolint end

#'@title Spc plot
#'
#'@export

spc_function <- function (data, spc_value, time, direction, var) { #, value, time, direction) {

  spc_text <- spc_text(var)

  f <- data %>%
    NHSRplotthedots::ptd_spc(
      value_field = {{spc_value}},
      date_field = {{time}},
      improvement_direction = direction) %>%
    NHSRplotthedots::ptd_create_ggplot(
      y_axis_label = spc_text$y_axis_label,
      main_title = spc_text$main_title
    )
  return(f)
}

# nolint start
## p <- spc_function(h, value, time_period, "decrease", "PRE_MAIN_CURVE") #, value, time_period, "decrease")
## p
# nolint end
