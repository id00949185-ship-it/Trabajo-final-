# Trabajo-final-
Usos de las redes sociales en España: comparación entre popularidad y distribución urbana
---
title: "Trabajo-final-"
author: "Konstancja Kazmierska"
date: "`r Sys.Date()`"
output: 
  html_document:
    toc: true
    toc_depth: 3
    theme: united
---

# 1. Introducción y Preparación del Entorno

```{r inicializacion, message=FALSE, warning=FALSE}
# ---- Limpieza del entorno ----
rm(list = ls())

# ---- Carga de Paquetes Necesarios ----
library(tidyverse) 
library(psych)     
library(timeDate)  
library(haven)     
install.packages("usethis")
library(usethis)
```

# 2. Importación y Depuración de Datos (Data Wrangling)
En esta sección importamos los tres archivos del proyecto e implementamos correcciones tipográficas cruciales para normalizar los formatos numéricos españoles.

```{r importacion_y_limpieza, message=FALSE, warning=FALSE}
# ---- Importación de Archivos ----
archivo1_pop    <- read.csv2("pop_nacional.csv")
archivo2_demo   <- read.csv2("usuarios_demo.csv")
archivo3_ciudad <- read.csv2("ciudades.csv")

# ---- Corrección del nombre de columna en Archivo 1 ----
archivo1_pop <- archivo1_pop %>% 
  rename(Porcentaje = Porcentaje.de.encuestados....)

# ---- Paso 3.1: Depuración de Ciudades (Eliminar puntos de miles) ----
ciudades_limpias <- archivo3_ciudad %>%
  mutate(across(c(Facebook, Instagram, LinkedIn, Twitter), 
                ~ as.numeric(gsub("\\.", "", .))))

# ---- Paso 3.2: Depuración de Demografía (Cambiar comas por puntos decimales) ----
datos_demo_limpios <- archivo2_demo %>%
  mutate(across(c(Hombres, Mujeres), 
                ~ as.numeric(gsub(",", ".", .))))
```

# 3. Presentación Visual de los Datos

## 3.1 Popularidad Nacional de Plataformas (Archivo 1)
Analizamos visualmente la penetración nacional de cada plataforma. Usamos barras horizontales ordenadas para mejorar la legibilidad.

```{r grafico_popularidad}
ggplot(archivo1_pop, aes(x = reorder(Parámetro, Porcentaje), y = Porcentaje)) +
  geom_bar(stat = "identity", fill = "#003366") +
  coord_flip() +
  labs(title = "Popularidad de Plataformas en España (General)", 
       x = "Red Social", y = "Porcentaje de Uso (%)") +
  theme_minimal()
```

## 3.2 Cuota de Mercado (Top 5 España)
A continuación, representamos de forma proporcional las cuotas estimadas de las redes líderes a escala nacional.

```{r grafico_tarta}
# Datos de uso para el Top 5 de plataformas
plataformas <- c("YouTube", "WhatsApp", "Facebook", "Instagram", "Otras")
valores     <- c(89, 86, 79, 65, 53)

# Calcular etiquetas dinámicas con porcentajes
porcentajes <- round(valores / sum(valores) * 100, 1)
etiquetas   <- paste0(plataformas, " (", porcentajes, "%)")

# Gráfico de sectores clásico
colores <- c("#003366", "#1e40af", "#3b82f6", "#60a5fa", "#94a3b8")
pie(valores, labels = etiquetas, col = colores, 
    main = "Cuota de Mercado Digital (Top 5 España)")
```

## 3.3 Distribución Demográfica por Edad y Sexo (Archivo 2)
Implementamos una representación de barras paralelas para comparar la adopción entre hombres y mujeres por cohorte generacional.

```{r grafico_demografico}
# 1. Pivotar a formato largo para el modelado en ggplot
datos_demo_largo <- datos_demo_limpios %>%
  pivot_longer(cols = c(Hombres, Mujeres), names_to = "Sexo", values_to = "Porcentaje")

# 2. Renderizado del gráfico agrupado
ggplot(datos_demo_largo, aes(x = Parámetro, y = Porcentaje, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Uso de Redes por Edad y Sexo en España", 
       x = "Grupos de Edad", y = "Porcentaje (%)") +
  theme_minimal()
```

# 4. Exploración Numérica y Descriptiva (Archivo 3)
Analizamos los parámetros estadísticos de centralización y dispersión del uso de la red visual **Instagram** en las principales capitales de España.

```{r exploracion_descriptiva}
# Métricas de tendencia central y localización (Ignorando NAs de forma segura)
media_ig     <- mean(ciudades_limpias$Instagram, na.rm = TRUE)
mediana_ig   <- median(ciudades_limpias$Instagram, na.rm = TRUE)
cuartiles_ig <- quantile(ciudades_limpias$Instagram, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Mostrar resultados tabulados
cat("Estadísticos para Instagram en las Grandes Ciudades:\n",
    "- Media Aritmética:", round(media_ig, 2), "usuarios\n",
    "- Mediana (Q2):", mediana_ig, "usuarios\n",
    "- Cuartil 1 (25%):", cuartiles_ig[1], "usuarios\n",
    "- Cuartil 3 (75%):", cuartiles_ig[3], "usuarios\n")
```

# 5. Técnicas Avanzadas de Datos

## 5.1 Clustering K-means de Ciudades
Para clasificar los perfiles urbanos sin sesgos de tamaño poblacional, imputamos los valores nulos con la mediana e implementamos el algoritmo de agrupamiento K-means fijando $K = 3$.

```{r clustering_urbano}
# 1. Imputación robusta de NAs con la mediana de cada red social
ciudades_sin_na <- ciudades_limpias %>%
  mutate(across(c(Facebook, Instagram, LinkedIn, Twitter), 
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# 2. Estandarización de las columnas cuantitativas
datos_escalados <- scale(ciudades_sin_na[, c("Facebook", "Instagram", "LinkedIn", "Twitter")])

# 3. Aplicación de K-means (Semilla fija para consistencia)
set.seed(3)
cluster_proyecto <- kmeans(datos_escalados, centers = 3)

# 4. Asignación de clústeres a la tabla original
ciudades_sin_na$Cluster <- as.factor(cluster_proyecto$cluster)

# Corregido: Seleccionamos la primera columna (cualquiera que sea su nombre) para mostrar la ciudad y su Cluster asignado
nombre_col_ciudad <- names(ciudades_sin_na)[1]
ciudades_sin_na %>% select(all_of(nombre_col_ciudad), Cluster)
```

## 5.2 Tabla de Contingencia Proporcional
Finalmente, analizamos la relación estructural y la dependencia relativa de las variables demográficas combinadas.

```{r tabla_contingencia}
# Matriz de frecuencias relativas cruzadas
tabla_cruces <- table(datos_demo_largo$Sexo, datos_demo_largo$Parámetro)
tabla_contingencia_porcentual <- round(prop.table(tabla_cruces, margin = 2) * 100, 2)

# Imprimir matriz resultante
print(tabla_contingencia_porcentual)

```
