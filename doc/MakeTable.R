#' Table function for calculating proportions by hospital
#' and by choice of two variable - intention is to use "GENDER" given the set up
#' of the shiny application + one choice of variable (e.g., BMI_CATEGORY)
#'
#'@param data : data frame
#'@param my_var : the variable chosen from the small drop down menu -
#'gender: both, men, women
#'@param my_var2 : the variable chosen from the long drop down menu with all variables
#'that you can want to see proportion analyses for
#'@param my_var3 : the variable you want to select table by (i.e., mann, kvinne, begge)
#'
#'Takes a data frame and (i) preforms selections based on input; (ii)
#'calculates proportions and percentages; (iii) builds a new functions selecting
#'certain columns and; (iv) returns a data frame
#'
#'@returns a data frame
#'@export


# Getting the variables I need

res= prep(regdata, Kurve_pre)

# Unpacking the list

df <- data.frame(res[1])

gg_data <- data.frame(res[2])


#### MAKE TABLE FUNCTION ####


# Working with tables and making tables by proportions

make_table <- function(data, my_var, my_var2, my_var3){

  # change levels in GENDER


  data$GENDER <- as.character(data$GENDER)

  data <- data %>%
    mutate(GENDER = recode(GENDER, "1" = "mann", "2" = "kvinne"))


  # make a df with summary statistics by hospital and my_var


  by_my_var <- data %>%

    select(CENTRESHORTNAME, {{my_var}}) %>%

    summarise(num_by_my_var = n(), .by = c(CENTRESHORTNAME, {{my_var}})) %>%

    mutate(total = sum(num_by_my_var), .by = CENTRESHORTNAME)


  # make a df with summary statistics by hospital, my_var and my_var2


  by_my_var2 <- regdata %>%

    select(CENTRESHORTNAME, {{my_var}}, {{my_var2}}) %>%

    summarise(my_var2_name = n(), .by = c({{my_var2}}, CENTRESHORTNAME, {{my_var}}))


  # merge the dfs and calculate proportions

  by_my_vars <- left_join(by_my_var, by_my_var2) %>%

    drop_na({{my_var2}}) %>% # drop nas

    pivot_wider(names_from = {{my_var}}, names_sep = "_",
                values_from = c(num_by_my_var, my_var2_name)) %>%

    replace(is.na(.), 0) %>%

    # make by-hospital proportions for my_var2

    mutate(total_my_var2 = my_var2_name_mann + my_var2_name_kvinne,
           .by = CENTRESHORTNAME) %>%

    mutate(prop_by_hosp = total_my_var2/total, .by = CENTRESHORTNAME) %>%

    mutate(prec_by_hosp = prop_by_hosp*100, .by = CENTRESHORTNAME) %>%

    # make by-hospital and by my_var proportions for my_var2

    mutate(prop_my_var2_mann = my_var2_name_mann/num_by_my_var_mann,
           .by = CENTRESHORTNAME) %>%

    mutate(prop_my_var2_kvinne = my_var2_name_kvinne/num_by_my_var_kvinne,
           .by =CENTRESHORTNAME) %>%

    mutate(prec_my_var2_mann = prop_my_var2_mann*100, .by = CENTRESHORTNAME) %>%

    mutate(prec_my_var2_kvinne= prop_my_var2_kvinne*100, .by =CENTRESHORTNAME) %>%

    replace(is.na(.), 0)    # again replace nas with 0

  # nesten function which tells returns only a set of columns specified by the
  # function above

  my_select <- function(data, var3){
    ifelse(var3 == "mann", cols <- grep("mann", names(data), value = TRUE), # if man - select only columns containing "mann"
           ifelse(var3 == "kvinne", cols <- grep("kvinne", names(data), value = TRUE), # if woman - select only columns containing "kvinne"
                  cols <- grep("_", names(data), value = TRUE))) # if neither - select all

    d_ <- data[,cols]

    return(d_)
  }

  my_table <- my_select(by_my_vars, my_var3) # function takes the data frame made in main function


  return(my_table) # returns the final df
}


### TEST ###
# run this to test that the function works
make_table(regdata, GENDER, BMI_CATEGORY, "begge")
