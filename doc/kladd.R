# This is my "kladde-område"
# Overview of the work ahead
# 1: check that changes we've made so far work <- yes they work
# 2: add ggplot-function + check that it works <- geom_bar is going to be my friend... one discrete variable
# 3: change description file + check that it works
# 4: add other plot functions?


# example to run the ggplot plot function to test if it works (bar plot - histogram with one discrete variable)

navn = gg_makeHist2(regdata, BMI_CATEGORY)
navn + xlab("BMI")+
  ggtitle("Fordeling av")+
  theme(plot.title = element_text(size = 14, face ="bold", hjust = 0.5 , vjust = 1.5))

# Working with tables and making tables by proportions

test = function(data, x){
  data %>%
    dplyr::group_by("x") %>%
    mutate(total = n())
}

test(regdata, BMI_CATEGORY)

make_table = function(data, x){
  data %>%
    dplyr::select(CENTRESHORTNAME, GENDER, {{x}}) %>%
    tidyr::drop_na({{x}}) %>%
    dplyr::group_by(CENTRESHORTNAME, GENDER, {{x}}) %>%
    dplyr::summarise(total = n()) %>%
    ungroup() %>%
    mutate(rate = total/sum(total),
           perc = rate*100) %>%
    group_by(CENTRESHORTNAME, {{x}}, GENDER) %>%
    pivot_wider(names_from = GENDER, names_sep = ".", values_from = c(rate, perc, total)) %>%
    replace(is.na(.), 0) %>%
    rename("rate mann" = rate.1,
           "rate kvinne" = rate.2,
           "prosent mann" = perc.1,
           "prosent kvinne" = perc.2,
           "n mann" = total.1,
           "n kvinne" = total.2) %>%
    mutate("total rate" = `rate mann` + `rate kvinne`,
           "total prosent" = `prosent mann` + `prosent kvinne`,
           "total n" = `n mann` + `n kvinne`)
}

make_table_by_hosp = function(data, x){
    data %>%
    dplyr::select(CENTRESHORTNAME, GENDER, {{x}}) %>%
    tidyr::drop_na({{x}}) %>%
    dplyr::group_by(CENTRESHORTNAME, GENDER, {{x}}) %>%
    dplyr::summarise(total = n()) %>%
    tidyr::pivot_wider(names_from = CENTRESHORTNAME, values_from = total)

    # ungroup() %>%
    # mutate(rate = total/sum(total),
    #        perc = rate*100) %>%
    # group_by(CENTRESHORTNAME, {{x}}, GENDER) %>%
    # pivot_wider(names_from = GENDER, names_sep = ".", values_from = c(rate, perc, total)) %>%
    # replace(is.na(.), 0) %>%
    # rename("rate mann" = rate.1,
    #        "rate kvinne" = rate.2,
    #        "prosent mann" = perc.1,
    #        "prosent kvinne" = perc.2,
    #        "n mann" = total.1,
    #        "n kvinne" = total.2) %>%
    # mutate("total rate" = `rate mann` + `rate kvinne`,
    #        "total prosent" = `prosent mann` + `prosent kvinne`,
    #        "total n" = `n mann` + `n kvinne`)
}

make_table_by_hosp(regdata, BMI_CATEGORY)


get_n <- function(data, var_hosp, hosp){
  data %>%
    filter({{var_hosp}} == "hosp") %>%
    summarise(n = n())
  return(n)
}

get_n(regdata, CENTRESHORTNAME, Bergen)

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
    pivot_wider(names_from = {{my_var}}, names_sep = "_", values_from = c(num_by_my_var, my_var2_name)) %>%
    replace(is.na(.), 0) %>%
    mutate(total_my_var2 = my_var2_name_mann + my_var2_name_kvinne, .by = CENTRESHORTNAME) %>%

    mutate(prop_by_hosp = total_my_var2/total, .by = CENTRESHORTNAME) %>%
    mutate(prec_by_hosp = prop_by_hosp*100, .by = CENTRESHORTNAME) %>%

    mutate(prop_my_var2_mann = my_var2_name_mann/num_by_my_var_mann, .by = CENTRESHORTNAME) %>%
    mutate(prop_my_var2_kvinne = my_var2_name_kvinne/num_by_my_var_kvinne, .by =CENTRESHORTNAME) %>%
    mutate(prec_my_var2_mann = prop_my_var2_mann*100, .by = CENTRESHORTNAME) %>%
    mutate(prec_my_var2_kvinne= prop_my_var2_kvinne*100, .by =CENTRESHORTNAME) %>%
    replace(is.na(.), 0)


  return(by_my_vars)
}

make_table(regdata, GENDER, BMI_CATEGORY)


# f <- regdata %>%
#   select(CENTRESHORTNAME, GENDER) %>%
#   summarise(num_by_gender = n(), .by = c(CENTRESHORTNAME, GENDER)) %>%
#   mutate(total = sum(num_by_gender), .by = CENTRESHORTNAME)

# s <- regdata %>%
#   select(CENTRESHORTNAME, GENDER, BMI_CATEGORY) %>%
#   summarise(bmi = n(), .by = c(BMI_CATEGORY, CENTRESHORTNAME, GENDER))

d <- left_join(f, s) %>%
  drop_na(BMI_CATEGORY) %>%
  pivot_wider(names_from = GENDER, names_sep = "_", values_from = c(num_by_gender, bmi)) %>%
  replace(is.na(.), 0) %>%
  mutate(total_bmi = bmi_mann + bmi_kvinne, .by = CENTRESHORTNAME) %>%

  mutate(prop_by_hosp = total_bmi/total, .by = CENTRESHORTNAME) %>%
  mutate(prec_by_hosp = prop_by_hosp*100, .by = CENTRESHORTNAME) %>%

  mutate(prop_bmi_mann = bmi_mann/num_by_gender_mann, .by = CENTRESHORTNAME) %>%
  mutate(prop_bmi_kvinne = bmi_kvinne/num_by_gender_kvinne, .by =CENTRESHORTNAME) %>%
  mutate(prec_bmi_mann = prop_bmi_mann*100, .by = CENTRESHORTNAME) %>%
  mutate(prec_bmi_kvinne= prop_bmi_kvinne*100, .by =CENTRESHORTNAME) %>%
  replace(is.na(.), 0)


regdata <- regdata %>%
  mutate(GENDER = recode(GENDER, "1" = "mann", "2" = "kvinne"))

unique(regdata$GENDER)
