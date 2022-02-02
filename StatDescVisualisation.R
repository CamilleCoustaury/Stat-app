library(ggplot2)

# Pour réaliser des digrammes en barres :
diagramme_en_barres <- function(bdd, variable) {
  ggplot(data = bdd, aes_string(variable)) + geom_bar()
}
# Changer le nom de la base et le nom de la variable :
diagramme_en_barres(data_wave1, "hegenh")


# Pour réaliser des histogrammes :
histogramme <- function(bdd, variable) {
  ggplot(data = bdd, aes_string(variable)) + geom_histogram()
}

histogramme(data_wave1, "sinc_bu_i")
histogramme(data_wave1, "ppen_bu_i")
histogramme(data_wave1, "spen_r_i")
