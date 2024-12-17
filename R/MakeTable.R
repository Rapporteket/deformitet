#' @title makeTable
#'
#' @export


# New table function based on prepVar


makeTable <- function(data, var_reshID){

# Make "me vs. the rest" in the table based on the reshID of the user
  data <- data %>%
    dplyr::mutate(Sykehus = dplyr::case_when({{var_reshID}} == "Bergen" ~ dplyr::recode(Sykehus, "Riksen" = "Resten", "St.Olav" = "Resten"),
                                            {{var_reshID}} == "Riksen" ~ dplyr::recode(Sykehus, "Bergen" = "Resten", "St.Olav" = "Resten"),
                                            {{var_reshID}} == "St.Olav" ~ dplyr::recode(Sykehus, "Bergen" = "Resten", "Riksen" = "Resten"),
                                            TRUE ~ Sykehus))

  a <- data %>%
    dplyr::group_by(Sykehus, data[2], .drop = FALSE) %>%
    dplyr::tally(name = "antall_pr_var")

  b <- data %>%
    dplyr::group_by(Sykehus) %>%
    dplyr::count(name = "antall_pr_sykh")

  c <- dplyr::left_join(a, b)

  my_table <- c %>%
    dplyr::mutate(andel = antall_pr_var/antall_pr_sykh,
                  prosent = andel*100)

  return(my_table)

}





