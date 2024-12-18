#' @title makeTable
#'
#' @export


# New table function based on prepVar


makeTable <- function(data, var_reshID, choice_var){

# Make user's hospital vs. the rest in the table based on reshID of user

  if(choice_var != "hver enhet"){
    data_hosp <- data %>%
      select(-c(CURRENT_SURGERY)) %>%
      dplyr::filter(CENTREID == {{var_reshID}})

    data_hosp <- data_hosp %>%
      dplyr::select(-c(CENTREID, Kjønn)) %>%
      dplyr::add_tally(name = "n") %>%
      dplyr::group_by(data_hosp[3]) %>%
      dplyr::add_count(name = "by_var") %>%
      dplyr::mutate(Prosent = round(by_var/n*100, 2)) %>%
      dplyr::rename("n pr variabel" = by_var) %>%
      dplyr::distinct()
  }

  if(choice_var == "hver enhet"){
    data_hosp_all <- data

    data_hosp_all <- data_hosp_all %>%
      dplyr::select(-c(CENTREID, Kjønn, CURRENT_SURGERY)) %>%
      dplyr::group_by(Sykehus) %>%
      dplyr::add_tally(name = "n") %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Sykehus, data_hosp_all[3]) %>%
      dplyr::add_count(name = "by_var") %>%
      dplyr::mutate(Prosent = round(by_var/n*100, 2)) %>%
      dplyr::rename("n pr variabel" = by_var) %>%
      dplyr::distinct()
  }

  else{
    data_all <- data

    data_all <- data_all %>%
    dplyr::select(-c(CENTREID, Kjønn, CURRENT_SURGERY)) %>%
    dplyr::mutate(Sykehus = recode(Sykehus,
                                   "Haukeland" = "Alle",
                                   "Rikshospitalet" = "Alle",
                                   "St.Olav" = "Alle")) %>%
    dplyr::add_tally(name = "n") %>%
    dplyr::group_by(data_all[3]) %>%
    dplyr::add_count(name = "by_var") %>%
    dplyr::mutate(Prosent = round(by_var/n*100, 2)) %>%
    dplyr::rename("n pr variabel" = by_var) %>%
    dplyr::distinct()


  data_full <- dplyr::full_join(data_hosp, data_all)}


  if(choice_var == "hver enhet"){
    return(data_hosp_all)
  }
  if(choice_var == "egen enhet"){
    return(data_hosp)
  }
  if(choice_var == "hele landet, uten sammenligning"){
    return(data_all)
  }
  else{return(data_full)} # => hele landet med sammenligning
  }
#
#   # a = dummy holder for data 1
#   a <- data %>%
#     dplyr::group_by(Sykehus, data[3], .drop = FALSE) %>% # group by hospital and second column
#     dplyr::tally(name = "antall_pr_var") # count observations in each group with tally
#
#   # b = dummy holder for data 2
#   b <- data %>%
#     dplyr::group_by(Sykehus) %>%
#     dplyr::count(name = "antall_pr_sykh") # count number
#
#
#   # c = dummy holder for data 3
#   c <- dplyr::left_join(a, b) # merge a and b and make c
#
#   # my_table = final table that is returned
#   my_table <- c %>%
#     dplyr::mutate(andel = round(antall_pr_var/antall_pr_sykh, 4), # calculate andel
#                   prosent = round(andel*100, 2)) # calculate percentage




# Test to see if it works:
## g <- makeTable(f, 103240, "hele landet, uten sammenligning")

