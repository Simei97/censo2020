library(tidyverse)
#Cargamos este paquete ya que usaremos números muy grandes
library(scales)

#Carga de datos
datos <- read_csv("ITER_14CSV20.csv")

#Resumen de poblacion total conforme al sexo
poblacion_sexo <- datos %>% 
        summarise(
                Total_Fem = sum(as.numeric(POBFEM), na.rm = TRUE),
                Total_Masc = sum(as.numeric(POBMAS), na.rm = TRUE)
        ) %>% 
        pivot_longer(everything(), names_to = "Sexo", values_to = "Cantidad")

#Grafico del resumen de poblacion total conforme al sexo
ggplot(poblacion_sexo, aes(x = Sexo, y = Cantidad, fill = Sexo))+
        geom_col()+
        labs(
                title = "Población total por sexo", 
                caption = "Fuente: Instituto de Información Estadística y Geográfica del Estado de Jalisco", 
                y = "Cantidad", 
                x = "Sexo")+
        scale_y_continuous(labels = label_comma())+
        theme_minimal()

#Resumen de 18-24 años por sexo
poblacion_18_24 <- datos %>% 
        summarise(
                F_18_24 = sum(as.numeric(P_18A24_F), na.rm = TRUE),
                M_18_24 = sum(as.numeric(P_18A24_M), na.rm = TRUE)
        ) %>% 
        pivot_longer(everything(), names_to = "Grupo", values_to = "Cantidad")

#Grafico del resumen
ggplot(poblacion_18_24, aes(x = Grupo, y = Cantidad, fill = Grupo))+
        geom_col()+
        labs(
                title = "Población de 18 a 24 años por sexo",
                caption = "Fuente: Instituto de Información Estadística y Geográfica del Estado de Jalisco",
                y = "Cantidad", 
                x = "Grupo"
            )+
        scale_y_continuous(labels = label_comma())+
        theme_minimal()

#Resumen de 60 años o mas por sexo
poblacion_60_mas <- datos %>% 
        summarise(
                F_60_MAS = sum(as.numeric(P_60YMAS_F), na.rm = TRUE),
                M_60_MAS = sum(as.numeric(P_60YMAS_M), na.rm = TRUE)
        ) %>% 
        pivot_longer(everything(), names_to = "Grupo", values_to = "Cantidad")

#Grafico
ggplot(poblacion_60_mas, aes(x = Grupo, y = Cantidad, fill = Grupo)) +
        geom_col() +
        labs(
                title = "Población 60+ años por sexo",
                caption = "Fuente: Instituto de Información Estadística y Geográfica del Estado de Jalisco",
                y = "Cantidad", 
                x = "Grupo") +
        scale_y_continuous(labels = label_comma())+
        theme_minimal()

#Hogares donde el jefe de familia es hombre o mujer
hogares_jefe <- datos %>% 
        summarise(
                Hogares_Jefe_F = sum(as.numeric(HOGJEF_F), na.rm = TRUE),
                Hogares_Jefe_M = sum(as.numeric(HOGJEF_M), na.rm = TRUE)
        ) %>% 
        pivot_longer(everything(), names_to = "Sexo", values_to = "Cantidad")

#Grafico
ggplot(hogares_jefe, aes(x = Sexo, y = Cantidad, fill = Sexo)) +
        geom_col() +
        labs(title = "Sexo del jefe del hogar",
             caption = "Fuente: Instituto de Información Estadística y Geográfica del Estado de Jalisco", 
             y = "Cantidad", 
             x = "Sexo") +
        scale_y_continuous(labels = label_comma())+
        theme_minimal()

#Viviendas con/sin electricidad
electricidad <- datos %>% 
        summarise(
                Con_Elec = sum(as.numeric(VPH_C_ELEC), na.rm = TRUE),
                Sin_Elec = sum(as.numeric(VPH_S_ELEC), na.rm = TRUE)
        ) %>% 
        pivot_longer(everything(), names_to = "Estado", values_to = "Cantidad")

#Grafico
ggplot(electricidad, aes(x = Estado, y = Cantidad, fill = Estado)) +
        geom_col() +
        labs(title = "Disponibilidad de electricidad",
             caption = "Fuente: Instituto de Información Estadística y Geográfica del Estado de Jalisco", 
             y = "Cantidad", 
             x = "Estado") +
        scale_y_continuous(labels = label_comma())+
        theme_minimal()


#Transporte que utilizan mas (auto, moto, bici)
transporte <- datos %>% 
        summarise(
                Automovil = sum(as.numeric(VPH_AUTOM), na.rm = TRUE),
                Moto = sum(as.numeric(VPH_MOTO), na.rm = TRUE),
                Bicicleta = sum(as.numeric(VPH_BICI), na.rm = TRUE)
        ) %>% 
        pivot_longer(everything(), names_to = "Medio", values_to = "Cantidad")

#Grafico
ggplot(transporte, aes(x = Medio, y = Cantidad, fill = Medio)) +
        geom_col() +
        labs(
                title = "Medios de transporte disponibles",
                caption = "Fuente: Instituto de Información Estadística y Geográfica del Estado de Jalisco", 
                y = "Cantidad", 
                x = "Medio de transporte") +
        scale_y_continuous(labels = label_comma())+
        theme_minimal()

#Qué tecnología tienen disponible en cada hogar
tecnologia <- datos %>% 
        summarise(
                TV = sum(as.numeric(VPH_TV), na.rm = TRUE),
                PC = sum(as.numeric(VPH_PC), na.rm = TRUE),
                Telefono = sum(as.numeric(VPH_TELEF), na.rm = TRUE),
                Celular = sum(as.numeric(VPH_CEL), na.rm = TRUE),
                Internet = sum(as.numeric(VPH_INTER), na.rm = TRUE)
        ) %>%
        pivot_longer(everything(), names_to = "Tecnologia", values_to = "Cantidad")

#Grafico
ggplot(tecnologia, aes(x = Tecnologia, y = Cantidad, fill = Tecnologia)) +
        geom_col() +
        labs(
                title = "Acceso a tecnologías en las viviendas",
                caption = "Fuente: Instituto de Información Estadística y Geográfica del Estado de Jalisco", 
                y = "Cantidad", 
                x = "Tecnología") +
        scale_y_continuous(labels = label_comma())+
        theme_minimal()
