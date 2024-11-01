#' @title makeTable
#'
#' @export


# New table function based on prepVar


makeTable <- function(data, var_reshID){

# Make user's hospital vs. the rest in the table based on reshID of user
  data <- data %>%
    dplyr::mutate(Sykehus = dplyr::case_when({{var_reshID}} == "Bergen" ~ dplyr::recode(Sykehus, "Riksen" = "Resten", "St.Olav" = "Resten"),
                                            {{var_reshID}} == "Riksen" ~ dplyr::recode(Sykehus, "Bergen" = "Resten", "St.Olav" = "Resten"),
                                            {{var_reshID}} == "St.Olav" ~ dplyr::recode(Sykehus, "Bergen" = "Resten", "Riksen" = "Resten"),
                                            TRUE ~ Sykehus))

  # a = dummy holder for data 1
  a <- data %>%
    dplyr::group_by(Sykehus, data[2], .drop = FALSE) %>% # group by hospital and second column
    dplyr::tally(name = "antall_pr_var") # count observations in each group with tally

  # b = dummy holder for data 2
  b <- data %>%
    dplyr::group_by(Sykehus) %>%
    dplyr::count(name = "antall_pr_sykh") # count number


  # c = dummy holder for data 3
  c <- dplyr::left_join(a, b) # merge a and b and make c

  # my_table = final table that is returned
  my_table <- c %>%
    dplyr::mutate(andel = antall_pr_var/antall_pr_sykh, # calculate andel
                  prosent = andel*100) # calculate percentage

  return(my_table)

}





