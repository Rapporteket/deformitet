# This is my "kladde-område"
# Overview of the work ahead
# 1: check that changes we've made so far work <- yes they work
# 2: add ggplot-function + check that it works <- geom_bar is going to be my friend... one discrete variable
# 3: change description file + check that it works
# 4: add other plot functions?



### TIDENES KODE: mutate(Komplikasjonstype = ifelse(Komplikasjonstype != "0", "0", "1")) ###





# example to run the ggplot plot function to test if it works (bar plot - histogram with one discrete variable)

navn = gg_makeHist2(regdata, BMI_CATEGORY)
navn + xlab("BMI")+
  ggtitle("Fordeling av")+
  theme(plot.title = element_text(size = 14, face ="bold", hjust = 0.5 , vjust = 1.5))

#### PREPVARS ####

# FOR GENDER:
# 1. Turn into character
regdata$GENDER <- as.character(regdata$GENDER)

# 2. Rename rows:
regdata <- regdata %>%
  mutate(GENDER = recode(GENDER, "1" = "mann", "2" = "kvinne"))


#

##### MAKE TABLE #####

make_table <- function(data, my_var, my_var2, my_var3, my_var4) {
  # my_var = gender
  # my_var2 = hvilken variabel hurhelst
  # my_var3 = hvilket kjønn du vil se for
  # my_var4 = tilhørigheten din

  data <- data %>%

    select(CENTRESHORTNAME, {{my_var}}, {{my_var2}}) %>%

    mutate(Sykehus = ifelse(CENTRESHORTNAME == {{my_var4}}, paste(CENTRESHORTNAME), "resten"))

  # make a df with summary statistics by hospital and my_var


  by_my_var <- data %>%

    select(Sykehus, {{my_var}}) %>%

    summarise(n = n(), .by = c(Sykehus, {{my_var}})) %>%

    mutate(total = sum(n), .by = Sykehus)


  # make a df with summary statistics by hospital, my_var and my_var2


  by_my_var2 <- data %>%

    select(Sykehus, {{my_var}}, {{my_var2}}) %>%

    summarise(my_var2_name = n(), .by = c({{my_var2}}, Sykehus, {{my_var}}))


  # merge the dfs and calculate proportions

  by_my_vars <- left_join(by_my_var, by_my_var2) %>%

    drop_na({{my_var2}}) %>% # drop nas

    pivot_wider(names_from = {{my_var}}, names_sep = "_",
                values_from = c(n, my_var2_name)) %>%

    replace(is.na(.), 0) %>%

    # make by-hospital proportions for my_var2

    mutate(total_my_var2 = my_var2_name_mann + my_var2_name_kvinne,
           .by = Sykehus) %>%

    mutate(prop_by_hosp = total_my_var2/total, .by = Sykehus) %>%

    mutate(prec_by_hosp = prop_by_hosp*100, .by = Sykehus) %>%

    # make by-hospital and by my_var proportions for my_var2

    mutate(prop_my_var2_mann = my_var2_name_mann/n_mann,
           .by = Sykehus) %>%

    mutate(prop_my_var2_kvinne = my_var2_name_kvinne/n_kvinne,
           .by =Sykehus) %>%

    mutate(prec_my_var2_mann = prop_my_var2_mann*100, .by = Sykehus) %>%

    mutate(prec_my_var2_kvinne= prop_my_var2_kvinne*100, .by =Sykehus) %>%

    replace(is.na(.), 0)    # again replace nas with 0

  mann <- by_my_vars %>%
    select("Sykehus", {{my_var2}}, ends_with("mann"))

  kvinne <- by_my_vars %>%
    select("Sykehus", {{my_var2}}, ends_with("kvinne"))

  begge <- by_my_vars %>%
    select(!ends_with("mann")) # må legge til at verken kvinne eller mann skal returneres!!


  ifelse(my_var3 == "mann", return(mann), ifelse(my_var3 == "kvinne", return(kvinne), return(begge)))

}


### TEST ###
# run this to test that the function works
make_table(regdata, GENDER, BMI_CATEGORY, "fds", "Bergen")


d %>%
  select(all_of(my_table))

my_table <- c("Sykehus", "BMI_CATEGORY", grep("mann", names(d), value = TRUE))

d %>%
  select(my_table)

by_my_vars <- by_my_vars %>%
  select(my_table)
