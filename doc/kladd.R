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

