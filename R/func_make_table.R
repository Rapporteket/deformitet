#' @title makeTable
#'
#' @export


# New table function based on prepVar


makeTable <- function(data, var_reshID){

# Make user's hospital vs. the rest in the table based on reshID of user

  ###### I DONT WANT THE ME VS REST VIEW ########

  data <- data %>%
    dplyr::mutate(Sykehus = dplyr::case_when({{var_reshID}} == "Haukeland" ~
                                               dplyr::recode(Sykehus,
                                                             "Rikshospitalet" = "Resten",
                                                             "St.Olav" = "Resten"),
                                             {{var_reshID}} == "Rikshospitalet" ~
                                               dplyr::recode(Sykehus,
                                                             "Haukeland" = "Resten",
                                                             "St.Olav" = "Resten"),
                                             {{var_reshID}} == "St.Olav" ~
                                               dplyr::recode(Sykehus,
                                                             "Haukeland" = "Resten",
                                                             "Rikshospitalet" = "Resten"),
                                                 TRUE ~ Sykehus))


  # a = dummy holder for data 1
  a <- data %>%
    dplyr::group_by(Sykehus, data[3], .drop = FALSE) %>% # group by hospital and second column
    dplyr::tally(name = "antall_pr_var") # count observations in each group with tally

  # b = dummy holder for data 2
  b <- data %>%
    dplyr::group_by(Sykehus) %>%
    dplyr::count(name = "antall_pr_sykh") # count number


  # c = dummy holder for data 3
  c <- dplyr::left_join(a, b) # merge a and b and make c

  # my_table = final table that is returned
  my_table <- c %>%
    dplyr::mutate(andel = round(antall_pr_var/antall_pr_sykh, 4), # calculate andel
                  prosent = round(andel*100, 2)) # calculate percentage

  return(my_table)

}





