library(ggplot2)
library(haven)

StartData_wide <- read.csv("StartData_wide.csv")

# Pour réaliser des digrammes en barres :
diagramme_en_barres <- function(variable) {
  ggplot(data = StartData_wide, aes_string(variable)) + geom_bar()
}
# Changer le nom de la base et le nom de la variable :
diagramme_en_barres("sclddr1")
diagramme_en_barres("sclddr2")


# Pour réaliser des histogrammes :
histogramme <- function(bdd, variable) {
  ggplot(data = bdd, aes_string(variable)) + geom_histogram()
}

histogramme(StartData_wide, "totinc_bu_s1")

