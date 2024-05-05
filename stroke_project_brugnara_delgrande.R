library(ggplot2)
library(tidyverse)

dati <- read.csv("healthcare-dataset-stroke-data.csv")

############################################################################################
#                                Data exploration                                                   #
############################################################################################

print("Prime righe del dataset:")
print(head(dati))

print("Riepilogo del dataset:")
print(summary(dati))

print("Nome delle colonne disponibili:")
print(colnames(dati))

# Missing values
missing_values <- sum(is.na(dati))
print(paste("Numero totale di missing values nel dataset:", missing_values))


print("Statistiche descrittive per le variabili numeriche:")
print(summary(select(dati, where(is.numeric))))

print("Frequenze per le variabili categoriche:")
print(table(dati$gender)) # Cambia 'gender' con il nome della colonna appropriata

############################################################################################
#                                 Graphs                                                   #
############################################################################################

pdf("Istogrammi_per_colonna.pdf", width = 11, height = 8.5)

for (colonna in names(dati)) {
  # Controllare se la colonna è numerica
  if (is.numeric(dati[[colonna]])) {

    p <- ggplot(dati, aes(x = !!sym(colonna))) +
      geom_histogram(bins = 30, fill = "blue", color = "black") +
      labs(title = paste("Istogramma di", colonna), x = colonna, y = "Frequenza") +
      theme_minimal()
    print(p)
  }
}
dev.off()

pdf("Boxplots_per_colonna.pdf", width = 11, height = 8.5)

# Generare un box plot per ogni colonna numerica
for (colonna in names(dati)) {

  if (is.numeric(dati[[colonna]])) {

    p <- ggplot(dati, aes(x = factor(0), y = !!sym(colonna))) +
      geom_boxplot() +
      labs(title = paste("Box plot di", colonna), y = colonna, x = "") +
      theme_minimal()
    print(p)
  }
}
dev.off()


