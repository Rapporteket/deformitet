################################################################################
# Entrypoint of the shiny app
#
# Author: Ingrid Bondevik
# ##############################################################################


# Loading libraries ------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(rlang)
library(tidyr)
library(bslib)
library(DT)
library(shinyjs)


# Config -----------------------------------------------------------------------

# Colors and themes ------------------------------------------------------------

# Data -------------------------------------------------------------------------
#### Read in data:
regdata <- deformitet::les_og_flate_ut()

#### Clean and tidy data:
regdata <- deformitet::pre_pros(regdata)

# App states -------------------------------------------------------------------

# Utilities --------------------------------------------------------------------

# Modules ----------------------------------------------------------------------

source("modules/module_datadump.R")
