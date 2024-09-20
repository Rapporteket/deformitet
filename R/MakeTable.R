#' Table function for calculating proportions by hospital
#' and by choice of two variable - intention is to use "GENDER" given the set up
#' of the shiny application + one choice of variable (e.g., BMI_CATEGORY)
#'
#'
#'@param data : data frame
#'@param my_var : the variable chosen from the small drop down menu -
#'gender: both, men, women
#'@param my_var2 : the variable chosen from the long drop down menu with all variables
#'that you can want to see proportion analyses for
#'
#'@examples takes a data frame and (i) preforms selections based on input; (ii)
#'calculates proportions and percentages; (iii) returns a data frame
#'
#'@returns a data frame



make_table <- function(data, my_var, my_var2){

  data$GENDER <- as.character(data$GENDER)

  data <- data %>%
    mutate(GENDER = recode(GENDER, "1" = "Mann", "2" = "Kvinne"))

  by_my_var <- data %>%
    select(CENTRESHORTNAME, {{my_var}}) %>%
    summarise(num_by_my_var = n(), .by = c(CENTRESHORTNAME, {{my_var}})) %>%
    mutate(total = sum(num_by_my_var), .by = CENTRESHORTNAME)

  by_my_var2 <- regdata %>%
    select(CENTRESHORTNAME, {{my_var}}, {{my_var2}}) %>%
    summarise(my_var2_name = n(), .by = c({{my_var2}}, CENTRESHORTNAME, {{my_var}}))


  by_my_vars <- left_join(by_my_var, by_my_var2) %>%
    drop_na({{my_var2}}) %>%
    pivot_wider(names_from = {{my_var}}, names_sep = "_",
                values_from = c(num_by_my_var, my_var2_name)) %>%
    replace(is.na(.), 0) %>%
    mutate(total_my_var2 = my_var2_name_mann + my_var2_name_kvinne,
           .by = CENTRESHORTNAME) %>%

    mutate(prop_by_hosp = total_my_var2/total, .by = CENTRESHORTNAME) %>%
    mutate(prec_by_hosp = prop_by_hosp*100, .by = CENTRESHORTNAME) %>%

    mutate(prop_my_var2_mann = my_var2_name_mann/num_by_my_var_mann,
           .by = CENTRESHORTNAME) %>%
    mutate(prop_my_var2_kvinne = my_var2_name_kvinne/num_by_my_var_kvinne,
           .by =CENTRESHORTNAME) %>%
    mutate(prec_my_var2_mann = prop_my_var2_mann*100,
           .by = CENTRESHORTNAME) %>%
    mutate(prec_my_var2_kvinne= prop_my_var2_kvinne*100,
           .by =CENTRESHORTNAME) %>%
    replace(is.na(.), 0)


  return(by_my_vars)
}

