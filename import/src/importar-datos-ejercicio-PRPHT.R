# Autor:     LM
# Mantenimiento: LM
# Date: 12-Mar-24
# =========================================

# Importar datos de ejercicio para la posición de "data specialist" para el PRPHT
# Esta tarea creara una version de los datos en csv para further processing, dejando la 
# version original de los datos intacta y readonly.

# --- libs --- 
if(!require(pacman))install.packages("pacman")
p_load(dplyr, 
       here,
       readxl,
       stringr
       )

args <- list(input = here("import/input/Datos para ejercicio práctico-Data Specialist.xlsx"), 
             output = here("import/output/datos-entrevista-PRPHT.csv"))


# --- Importar datos --- 
datos_PRPHT_orig <- read_excel(args$input)
datos_PRPHT <- datos_PRPHT_orig

# --- Limpieza de datos --- 

# Revisión
head(datos_PRPHT) 
summary(datos_PRPHT) 
str(datos_PRPHT)

## Verificar duplicados
duplicated(datos_PRPHT)

## Verificar valores unicos en cada variable
lapply(datos_PRPHT, unique)


# Comentarios:
## Ninguna variable tiene duplicados 
## Muchas de las variables son categorias definidas, las cambiare a factor 
## Objectid no necesariamente lleva un orden, pero esto no es estrictamente necesario 
## Muni_rs presenta errores, convertir a NA

# Limpieza
datos_PRPHT <- datos_PRPHT %>%
  mutate_all(~str_squish(.)) %>% # Elimina espacios adicionales o "extra"
  mutate_all(~str_to_sentence(.)) %>% # Capitaliza solo la primera letra
  mutate_all(na_if, "NA") %>% # Convierte "NA" caracteres en valores NA
  mutate_at(vars(-fecha_referido), as.factor)  # Convierte variables a tipo factor excepto fecha_referido


head(datos_PRPHT)



# --- Analisis --- 

## 1.	¿Cuántos tipos de referidos se realizaron? 
datos_PRPHT %>% 
  group_by(tipo_referido) %>% 
  summarise(n = n()) %>% n_distinct()

# En total fueron 2 tipos de referidos

## 2.	¿Cuántos referidos de condición de salud se documentaron? 
datos_PRPHT %>%
  filter(tipo_referido == "Condición de salud") %>%
  summarise(count = n())


# En total fue 1 referido por condición de salud

## 3.Menciones cuales servicios sociales se identificaron en el Municipio de Culebra.
datos_PRPHT %>% 
  filter(municipio_rs == "Culebra") %>% 
  count(servicios_sociales)

# En culebra se identificaron 2 servicios sociales, alimentos y empleo o ingreso. Sin embargo,
# Uno serivicio social se desconoce. 


##  4.Evalué las variables llamadas: municipio_rs, condición de salud y tipo de
## referido e identifiqué incongruencias o errores. De encontrar incongruencias o 
## errores explique cuales son, y cómo mejorar la recopilación de datos para que no 
## vuelvan a ocurrir esos errores. 

# Revisando categorias unicas de las variables de interes
datos_PRPHT %>% distinct(municipio_rs) %>%  
  arrange(desc(municipio_rs))  # Errores convertirlos a valores NA 

datos_PRPHT %>% distinct(condicion_salud) %>% 
  arrange(desc(condicion_salud))  # Errores a corregir y otros a valores NA

datos_PRPHT %>% distinct(tipo_referido) # No errores

# Debo realizar correciones en las variables de municipio_rs y condición de salud.
# Algunas de estas categorias no corresponden a condiciones de salud y hay municipios que 
# no corresponden a Puerto Rico o apenas existen. 

datos_PRPHT <- datos_PRPHT %>%  
  mutate(municipio_rs = case_when(municipio_rs %in% c("La plata", "Nueva york", "San luis") ~  NA,
                     TRUE ~ municipio_rs),
         condicion_salud = case_when(str_detect(condicion_salud, "^Nutri") ~ "Nutrición",
                     condicion_salud %in% c("Servicios de ama de llaves", "Deuda en el servicio de energia electrica") ~  NA, 
                     TRUE ~ condicion_salud))
        


## 5.	Cree categorías para la variable condicion_salud
???
     

6.	Menciones dos recomendaciones que usted daría como analista de datos para mejorar la base de datos. 
library(dplyr)






 
 
 
 
