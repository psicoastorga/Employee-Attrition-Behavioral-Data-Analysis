df <- read.csv("IBM-HR-Employee-Attrition-ES.csv")

#Resumen de variables numéricas

library(dplyr)

resumen_numericas <- df %>%
  select(where(is.numeric)) %>%
  summarise(
    across(
      everything(),
      list(
        Media = mean,
        Mediana = median,
        SD = sd,
        Min = min,
        Max = max
      ),
      na.rm = TRUE
    )
  )

resumen_numericas

#Resumen más cómodo (una variable por fila)

numericas <- df %>% select(where(is.numeric))

tabla_numericas <- data.frame(
  Variable = names(numericas),
  Media = sapply(numericas, mean, na.rm = TRUE),
  Mediana = sapply(numericas, median, na.rm = TRUE),
  SD = sapply(numericas, sd, na.rm = TRUE),
  Minimo = sapply(numericas, min, na.rm = TRUE),
  Maximo = sapply(numericas, max, na.rm = TRUE)
)

tabla_numericas <- round(tabla_numericas, 2)

tabla_numericas

#Frecuencias de variables categóricas

categoricas <- df %>% select(where(~is.character(.) | is.factor(.)))

frecuencias <- lapply(categoricas, table)

frecuencias$Attrition

#Informe univariable completo

install.packages("psych")
library(psych)

describe(df %>% select(where(is.numeric)))