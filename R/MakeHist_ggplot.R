
#' ggplot2 function for use in Rapporteket
#' the function creates bar plots
#'
#' @param data dataframe from which output is to be made
#' @param x_var defining which variable in the data frame to be used to plot (on the x-axis)
#' @param makeTable Logical that if TRUE function will return a data frame
#' containing the bin borders and count within each bin
#'
#' @return a graphical object or data frame
#' @export
#'
#' @examples
#' gg_makeHist(data = regdata, x_var = BMI_CATEGORY) # provide without quotationmarks


gg_makeHist <- function(data, x_var){

  plot.data = data %>%
    dplyr::select({{x_var}})

  colnames(plot.data) = "z"

    plot =
      ggplot2::ggplot(plot.data, aes(     # opens ggplot
      x = z))+                            # varies by input

    geom_bar(                             # opens bar-plot
      fill = "#6CACE4",                   # choosing colors on columns
      color = "#BFCED6",                  # choosing colors in column outlines
      alpha = 0.8)+                       # setting alpha

    theme_light()+                        # choosing theme

    ylab("Antall")                        # title of y-axis

    return(plot)
}
