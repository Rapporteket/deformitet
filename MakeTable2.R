# New table function based on PrepVar

# First - load in the data and prep it
res= prep(regdata, Alder)

# Unpacking the list that is returned by prep()

df <- data.frame(res[1])

gg_data <- data.frame(res[2])


make_table <- function(data, var, reshID){

  df <- df %>%
    mutate(Sykehus  = case_when(as_name(reshID) == "Bergen" ~ recode(Sykehus, "Riksen" = "Resten", "St.Olav" = "Resten"),
                                as_name(reshID) == "Riksen" ~ recode(Sykehus, "Bergen" = "Resten", "St.Olav" = "Resten"),
                                as_name(reshID) == "St.Olav" ~ recode(Sykehus, "Bergen" = "Resten", "St.Olav" = "Resten"),
                                TRUE ~ Sykehus))

  a <- df %>%
    group_by(Sykehus, {{var}}, .drop = FALSE) %>%
    tally(name = "antall_pr_var")

  b <- df %>%
    group_by(Sykehus) %>%
    count(name = "antall_pr_sykh")

  c <- df %>%
    group_by(Sykehus, Kjønn, {{var}}, .drop = FALSE) %>%
    count(name = "antall_pr_var_kjønn_sykh")

  d <- df %>%
    group_by(Sykehus, Kjønn, .drop = FALSE) %>%
    count(name = "antall_pr_kjønn_sykh")

  e <- left_join(a, b)

  f <- left_join(e, c)

  g <- left_join(f, d)

  my_table <- g %>%
    mutate(andel = antall_pr_var/antall_pr_sykh,
           prosent = andel*100,
           andel_pr_kjønn = antall_pr_var_kjønn_sykh/antall_pr_kjønn_sykh,
           prosent_pr_kjønn = andel_pr_kjønn*100)


  return(my_table)
}

# Test for å se om det funker
table <- make_table(df, Alder, "St.Olav")





