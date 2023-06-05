library(tidyverse)
library(here)


datos.aprov.2019 <- read_csv2(here("aprovechamientos-2019.csv"))

datos.l <- datos.aprov.2019 %>%
           select(1,11,12,13,14,15,16,17,18,19,21,23) %>%
           rename("T_ext"= "Tipo de Obra" ,
                  "Vol"= "Volumen",
                  "Dest"= "Destino",
                  "M_ini" = "Mes Inicio",
                  "M_fin" = "Mes Final",
                  "H_anio" = "Horas x Año",
                  "Depto" = "Depto.-Padrón",
                  "Cod_cuenca" = "Codigo Cuencas de nivel 5",
                  "Cuenca_ext" = "Cuencas de nivel 3")
           
datos.l$T_ext <- factor(datos.l$T_ext)
datos.l$Uso <- factor(datos.l$Uso)
datos.l$Dest <- factor(datos.l$Dest)
datos.l$Depto <- factor(datos.l$Depto)
datos.l$Cod_cuenca <- factor(datos.l$Cod_cuenca)


summary(datos.l)

