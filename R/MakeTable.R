# New table function based on PrepVar

# First - load in the data and prep it
res= prep(regdata, Blodtap_100, "kvinne", "2023-01-02", "2024-09-02", 10, 20)

# Unpacking the list that is returned by prep()

df <- data.frame(res[1])

gg_data <- data.frame(res[2])


make_table <- function(data, reshID){


# Make "me vs. the rest" in the table based on the reshID of the user
  data <- data %>%
    mutate(Sykehus  = case_when(as_name(reshID) == "Bergen" ~ recode(Sykehus, "Riksen" = "Resten", "St.Olav" = "Resten"),
                                as_name(reshID) == "Riksen" ~ recode(Sykehus, "Bergen" = "Resten", "St.Olav" = "Resten"),
                                as_name(reshID) == "St.Olav" ~ recode(Sykehus, "Bergen" = "Resten", "Riksen" = "Resten"),
                                TRUE ~ Sykehus))

  a <- data %>%
    group_by(Sykehus, data[2], .drop = FALSE) %>%
    tally(name = "antall_pr_var")

  b <- data %>%
    group_by(Sykehus) %>%
    count(name = "antall_pr_sykh")

  c <- left_join(a, b)

  my_table <- c %>%
    mutate(andel = antall_pr_var/antall_pr_sykh,
           prosent = andel*100)

  return(my_table)
}





