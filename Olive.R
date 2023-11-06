library(dplyr)

#PULIZIA DATI-------------------------------------------------------------------------------------------------------
data <- olive

data <- data %>%
  mutate(Area = case_when(
    Area == 1 ~ "Puglia Nord",
    Area == 2 ~ "Calabria",
    Area == 3 ~ "Puglia Sud",
    Area == 4 ~ "Sicilia",
    Area == 5 ~ "Sardegna Entroterra",
    Area == 6 ~ "Sardegna Costa",
    Area == 7 ~ "Liguria Est",
    Area == 8 ~ "Liguria Ovest",
    Area == 9 ~ "Umbria",
    TRUE ~ as.character(Area)  # This retains the original values for any other Area values not covered above
  ))


data <- data %>%
  mutate(Region = case_when(
    Region == 1 ~ "Sud",
    Region == 2 ~ "Sardegna",
    Region == 3 ~ "Nord",
    TRUE ~ as.character(Region)  # This retains the original values for any other Area values not covered above
  ))

head(data)


lista_Acidi <- c('Palmitic', 'Palmitoleic', 'Stearic', 'Oleic', 'Linoleic', 'Linolenic', 'Arachidic', 'Eicosenoic')
data_df <- data[lista_Acidi] / 100.0
data[lista_Acidi] <- data_df
head(data, n = 5)


totalPcts <- data %>%
  select(-Region,-Area) %>%
  rowSums()

summary(totalPcts)
length(totalPcts)

data_sat <- data %>%
  rowwise() %>%
  mutate(percent_saturated = sum(Palmitic, Stearic, Arachidic),
         percent_monounsaturated = sum(Palmitoleic, Oleic, Eicosenoic),
         percent_polyunsaturated = sum(Linoleic, Linolenic))
head(data_sat)

library(dplyr)
library(tidyr)
library(knitr)
# ANALISI DESCRITTIVA -------------------------------------------------------------------------------------------------------
# Calcola la media dei valori raggruppati per Area e trasforma il dataframe
means_region <- data %>% 
  group_by(Region) %>% 
  gather(key = "fatty_acid", value = "percentage", -Region, -Area) %>% 
  group_by(Region, fatty_acid) %>% 
  summarise(Mean = mean(percentage, na.rm = TRUE), .groups = "keep") %>% 
  spread(key = fatty_acid, value = Mean)

# Stampa la tabella dei valori medi
means_region %>%
  knitr::kable(digits = 2, align = "c")



library(tidyr)
mean_area <- data_sat %>%
  group_by(Area) %>%
  mutate(Media_Polinsaturi = mean(percent_polyunsaturated), 
         Media_Saturi = mean(percent_saturated),
         Media_Monoinsaturi = mean(percent_monounsaturated)) %>%
  select(Region, Area, Media_Polinsaturi, Media_Saturi, Media_Monoinsaturi)%>%
  distinct()

mean_area %>%
  knitr::kable(digits = 2, align = "c")

# Carica i pacchetti necessari
library(tidyr)
library(ggplot2)

# Supponendo che mean_area sia già stato creato e configurato come mostrato nel tuo messaggio precedente

# Trasforma i dati in formato lungo
mean_area_long <- mean_area %>%
  pivot_longer(
    cols = starts_with("Media_"), 
    names_to = "TipoDiAcidoGrasso", 
    values_to = "Valore"
  )


ggplot(mean_area_long, aes(x = Area, y = Valore, fill = TipoDiAcidoGrasso)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  facet_wrap(~TipoDiAcidoGrasso, scales = "free_y") +
  labs(x = "Area", y = "Media Valore") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# CLUSTERING -------------------------------------------------------------------------------------------------------
library(factoextra)
library(tidyverse)
library(dplyr)


# Supponendo che 'data_cluster' abbia una colonna 'Area'
# Assicurati di includere questa colonna quando crei 'data_cluster'
data_cluster <- data %>%
  select(Area, Palmitic, Palmitoleic, Stearic, Oleic, Linoleic, Linolenic, Arachidic, Eicosenoic) %>%
  na.omit()

# Now you can use mutate from dplyr
data_cluster <- data_cluster %>%
  mutate(Area = case_when(
    Area == "Puglia Nord" ~ "PN",
    Area == "Calabria" ~ "C",
    Area == "Puglia Sud" ~ "PS",
    Area == "Sicilia" ~ "SIC",
    Area == "Sardegna Entroterra" ~ "SAE",
    Area == "Sardegna Costa" ~ "SAC",
    Area == "Umbria" ~ "U",
    Area == "Liguria Est" ~ "LE",
    Area == "Liguria Ovest" ~ "LO",
    TRUE ~ Area 
  ))

# Normalizzazione dei dati senza la colonna 'Area'
data_scaled <- data_cluster %>%
  select(-Area) %>%
  scale()

# Esecuzione del k-means clustering con 3 cluster
set.seed(123) # Imposta un seed per riproducibilità
kmeans_result <- kmeans(data_scaled, centers = 4, nstart = 100)

# Aggiungi i risultati del cluster al set di dati
data_cluster$Cluster <- factor(kmeans_result$cluster)

# Prepara il set di dati per la visualizzazione
data_for_plot <- data_cluster %>%
  mutate(Cluster = as.factor(Cluster))

# [Il tuo codice precedente per creare 'data_for_plot'...]

# Utilizzare fviz_cluster per visualizzare i cluster con colori specifici per 'Area'
fviz_cluster(list(data = data_scaled, cluster = kmeans_result$cluster)) +
  geom_point(aes(color = data_for_plot$Area)) + # Aggiungi i colori basati sulla colonna 'Area'
  scale_color_manual(values = c("PN" = "red", "C" = "red", "PS" = "red", # ecc...
                                "SIC" = "red", "SAE" = "green", "SAC" = "green",
                                "U" = "blue", "LE" = "blue", "LO" = "blue"))
# CLUSTERING GERARCHICO -------------------------------------------------------------------------------------------------------
# Supponendo che `data_scaled` sia il tuo dataframe scalato.
pam_result <- pam(data_scaled, k = 4)

# Aggiungi i risultati del clustering al tuo dataframe originale
data_with_clusters <- data
data_with_clusters$Cluster <- pam_result$clustering

# Esegui PCA se necessario per la visualizzazione
pca <- prcomp(data_scaled)
pca_data <- as.data.frame(pca$x[, 1:2])
pca_data$Cluster <- pam_result$clustering
pca_data$Area <- data$Area  # sostituisci con il tuo dataframe originale

# Now you can use mutate from dplyr
pca_data <- pca_data %>%
  mutate(Area = case_when(
    Area == "Puglia Nord" ~ "PN",
    Area == "Calabria" ~ "C",
    Area == "Puglia Sud" ~ "PS",
    Area == "Sicilia" ~ "SIC",
    Area == "Sardegna Entroterra" ~ "SAE",
    Area == "Sardegna Costa" ~ "SAC",
    Area == "Umbria" ~ "U",
    Area == "Liguria Est" ~ "LE",
    Area == "Liguria Ovest" ~ "LO",
    TRUE ~ Area 
  ))

# Grafico con ggplot2
ggplot(pca_data, aes(x = PC1, y = PC2, color = factor(Cluster))) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = Area), vjust = 1.5, color = "black") +
  scale_color_brewer(palette = "Set1") +
  labs(color = "Cluster") +
  theme_minimal()


#------------------------------------------------------------------------------------------------------------------------------------
library(cluster)
library(factoextra)

# Supponiamo che 'data_scaled' sia il tuo dataframe di dati scalati.
sil_width <- rep(0, 10)  # Pre-allocare lo spazio per i risultati del coefficiente silhouette per k da 1 a 10

for(k in 2:10) {  # Inizia da 2 perché il coefficiente silhouette non può essere calcolato per un solo cluster
  pam_fit <- pam(data_scaled, k)
  sil_width[k] <- silhouette(pam_fit$clustering, dist(data_scaled))[, 3] |> mean()
}

plot(2:10, sil_width[-1], type = "b", pch = 19, frame = FALSE, 
     xlab = "Numero di cluster 'k'", ylab = "Coefficiente silhouette medio")

