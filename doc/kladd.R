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

f <- regdata %>%
  select(CENTRESHORTNAME, GENDER) %>%
  summarise(gender = n(), .by = c(CENTRESHORTNAME, GENDER)) %>%
  mutate(total = sum(gender), .by = CENTRESHORTNAME)

s <- regdata %>%
  select(CENTRESHORTNAME, GENDER, BMI_CATEGORY) %>%
  summarise(bmi = n(), .by = c(BMI_CATEGORY, CENTRESHORTNAME, GENDER))

d <- left_join(f, s) %>%
  drop_na(BMI_CATEGORY) %>%
  pivot_wider(names_from = GENDER, names_sep = "_", values_from = c(gender, bmi)) %>%
  replace(is.na(.), 0) %>%
  mutate(prop_bmi1 = bmi_1/total, .by = CENTRESHORTNAME) %>%
  mutate(prop_bmi2 = bmi_2/total, .by =CENTRESHORTNAME) %>%
  mutate(prec_bmi1 = bmi_1/total*100, .by = CENTRESHORTNAME) %>%
  mutate(prec_bmi2 = bmi_2/total*100, .by =CENTRESHORTNAME)


