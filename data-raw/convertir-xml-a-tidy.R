library(censAr)
library(xml2)
library(tidyverse)

d <- read_xml("data-raw/descripcion.xml")

cadena_a_titulo <- function(x) {
  x %>%
    iconv(., to = "UTF-8") %>%
    str_trim() %>%
    ifelse(. == "", NA, .) %>%
    str_to_title() %>%
    str_replace_all(., "/A", "/a") %>%
    str_replace_all(., " De ", " de ") %>%
    str_replace_all(., " De\\)", " de)") %>%
    str_replace_all(., " Del ", " del ") %>%
    str_replace_all(., " O ", " o ") %>%
    str_replace_all(., " Y ", " y ") %>%
    str_replace_all(., " Por ", " por ") %>%
    str_replace_all(., " En ", " en ") %>%
    str_replace_all(., " U ", " u ") %>%
    str_replace_all(., " El ", " el ") %>%
    str_replace_all(., " La ", " la ") %>%
    str_replace_all(., " Al ", " al ") %>%
    str_replace_all(., "\\(Grupo", " (Grupo") %>%
    str_replace_all(., "_recode", " recodificado/a")
}

# explorar ----

# persona <-
#   xml_attrs(xml_child(xml_child(xml_child(
#     xml_child(xml_child(xml_child(
#       xml_child(xml_child(xml_child(xml_child(
#         d, 2
#       ), 2), 3), 3), 3
#     ), 2), 3), 2
#   ), 14), 13))
#
# hogar <-
#   xml_attrs(xml_child(xml_child(xml_child(
#     xml_child(xml_child(xml_child(
#       xml_child(xml_child(xml_child(d, 2), 2), 3), 3
#     ), 3), 2), 3
#   ), 2), 14))
#
# vivienda <-
#   xml_attrs(xml_child(xml_child(xml_child(
#     xml_child(xml_child(xml_child(
#       xml_child(xml_child(d, 2), 2), 3
#     ), 3), 3), 2
#   ), 3), 2))
#
# zonaloc <-
#   xml_attrs(xml_child(xml_child(xml_child(
#     xml_child(xml_child(xml_child(xml_child(
#       d, 2
#     ), 2), 3), 3), 3
#   ), 2), 3))
#
# distrito <-
#   xml_attrs(xml_child(xml_child(xml_child(
#     xml_child(xml_child(d, 2), 2), 3
#   ), 3), 3))
#
# comuna <-
#   xml_attrs(xml_child(xml_child(xml_child(xml_child(
#     d, 2
#   ), 2), 3), 3))
#
# provincia <- xml_attrs(xml_child(xml_child(xml_child(d, 2), 2), 3))
#
# region <- xml_attrs(xml_child(xml_child(d, 2), 2))
#
# censo <- xml_attrs(xml_child(xml_child(d, 2), 2))

# persona ----

persona <- xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(d, 1), 1), 4), 4), 2), 2), 12), 22)

# xml_attrs(xml_child(persona, 1))
# xml_attrs(xml_child(persona, 34))
persona2 <- xml_attrs(xml_children(persona))

persona2 <- bind_rows(persona2) %>%
  select(variable = name, descripcion = label, tipo = type,
         tamanio = size, decimales = decimals, rango = range)

persona2 <- persona2 %>%
  mutate(
    rango = str_replace_all(rango, " TO ", " - "),
    descripcion = cadena_a_titulo(descripcion)
  ) %>%
  mutate_if(is.character, function(x) { str_trim(x) }) %>%
  mutate(
    variable = str_to_lower(variable)
  )

# xml_attrs(xml_child(xml_child(xml_child(persona, 1), 1), 1))
# xml_attrs(xml_child(xml_child(xml_child(persona, 1), 1), 2))
persona_codificacion <- map_df(
  seq_along(persona2$variable),
  function(x) {
    d <- bind_rows(xml_attrs(xml_children(xml_children(xml_child(persona, x)))))
    d$variable <- persona2$variable[[x]]

    d <- d %>%
      select(variable, valor = name, descripcion = value)

    d <- d %>%
      mutate(
        descripcion = case_when(
          descripcion == "MISSING" ~ "Valor Perdido",
          descripcion == "NOTAPPLICABLE" ~ "No Aplica",
          TRUE ~ descripcion
        ),
        descripcion = str_trim(cadena_a_titulo(descripcion))
      )

    d
  }
)

persona_codificacion <- persona_codificacion %>%
  distinct(variable, valor, .keep_all = T)

persona_codificacion <- persona_codificacion %>%
  mutate(tabla = "persona") %>%
  select(tabla, everything())

persona <- persona2 %>%
  mutate(rango = ifelse(rango == "", NA_character_, rango)) %>%
  mutate(tabla = "persona") %>%
  select(tabla, everything())

rm(persona2)

# hogar ----

hogar <-xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(d, 1), 1), 4), 4), 2), 2), 12)

# xml_attrs(xml_child(hogar, 1))
# xml_attrs(xml_child(hogar, 34))
hogar2 <- xml_attrs(xml_children(hogar))

hogar2 <- bind_rows(hogar2) %>%
  select(variable = name, descripcion = label, tipo = type,
         tamanio = size, decimales = decimals, rango = range)

hogar2 <- hogar2 %>%
  mutate(
    rango = str_replace_all(rango, " TO ", " - "),
    descripcion = cadena_a_titulo(descripcion)
  ) %>%
  mutate_if(is.character, function(x) { str_trim(x) }) %>%
  mutate(
    variable = str_to_lower(variable)
  )

# xml_attrs(xml_child(xml_child(xml_child(hogar, 1), 1), 1))
# xml_attrs(xml_child(xml_child(xml_child(hogar, 1), 1), 2))
hogar_codificacion <- map_df(
  seq_along(hogar2$variable),
  function(x) {
    print(x)

    d <- bind_rows(xml_attrs(xml_children(xml_children(xml_child(hogar, x)))))
    d$variable <- hogar2$variable[[x]]

    if (!any(colnames(d) %in% "name")) { d$name <- NA_character_ }
    if (!any(colnames(d) %in% "value")) { d$value <- NA_character_ }

    d <- d %>%
      select(variable, valor = name, descripcion = value)

    d <- d %>%
      mutate(
        descripcion = case_when(
          descripcion == "MISSING" ~ "Valor Perdido",
          descripcion == "NOTAPPLICABLE" ~ "No Aplica",
          TRUE ~ descripcion
        ),
        descripcion = str_trim(cadena_a_titulo(descripcion)),
        variable = str_to_lower(variable)
      )

    d
  }
)

hogar_codificacion <- hogar_codificacion %>%
  distinct(variable, valor, .keep_all = T)

hogar_codificacion <- hogar_codificacion %>%
  mutate(tabla = "hogar") %>%
  select(tabla, everything())

hogar <- hogar2 %>%
  drop_na(descripcion) %>%
  mutate(rango = ifelse(rango == "", NA_character_, rango)) %>%
  mutate(tabla = "hogar") %>%
  select(tabla, everything())

rm(hogar2)

# vivienda ----

vivienda <- xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(d, 1), 1), 4), 4), 2), 2)

# xml_attrs(xml_child(vivienda, 1))
# xml_attrs(xml_child(vivienda, 34))
vivienda2 <- xml_attrs(xml_children(vivienda))

vivienda2 <- bind_rows(vivienda2) %>%
  select(variable = name, descripcion = label, tipo = type,
         tamanio = size, decimales = decimals, rango = range)

vivienda2 <- vivienda2 %>%
  mutate(
    rango = str_replace_all(rango, " TO ", " - "),
    descripcion = cadena_a_titulo(descripcion)
  ) %>%
  mutate_if(is.character, function(x) { str_trim(x) }) %>%
  mutate(
    variable = str_to_lower(variable)
  )

# xml_attrs(xml_child(xml_child(xml_child(vivienda, 1), 1), 1))
# xml_attrs(xml_child(xml_child(xml_child(vivienda, 1), 1), 2))
vivienda_codificacion <- map_df(
  seq_along(vivienda2$variable),
  function(x) {
    print(x)

    d <- bind_rows(xml_attrs(xml_children(xml_children(xml_child(vivienda, x)))))
    d$variable <- vivienda2$variable[[x]]

    if (!any(colnames(d) %in% "name")) { d$name <- NA_character_ }
    if (!any(colnames(d) %in% "value")) { d$value <- NA_character_ }

    d <- d %>%
      select(variable, valor = name, descripcion = value)

    d <- d %>%
      mutate(
        descripcion = case_when(
          descripcion == "MISSING" ~ "Valor Perdido",
          descripcion == "NOTAPPLICABLE" ~ "No Aplica",
          TRUE ~ descripcion
        ),
        descripcion = str_trim(cadena_a_titulo(descripcion)),
        variable = str_to_lower(variable)
      )

    d
  }
)

vivienda_codificacion <- vivienda_codificacion %>%
  distinct(variable, valor, .keep_all = T)

vivienda_codificacion <- vivienda_codificacion %>%
  mutate(tabla = "vivienda") %>%
  select(tabla, everything())

vivienda <- vivienda2 %>%
  drop_na(descripcion) %>%
  mutate(rango = ifelse(rango == "", NA_character_, rango)) %>%
  mutate(tabla = "vivienda") %>%
  select(tabla, everything())

rm(vivienda2)

# zonas ----

#zonas <- tibble(
#  tabla = "zonas",
#  variable = "geocodigo",
#  descripcion = "Sub-División Comunal de la forma RRPCCDDLLLL. RR = Región; RRP = Provincia; RRPCC = Comuna; RRPCCDD = Distrito Censal; RRPCCDDLLLL = Zona Censal.",
#  tipo = "string",
#  tamanio = "11",
#  decimales = "0",
#  rango = NA_character_
#)


# radop ----

radio <-
  xml_child(xml_child(xml_child(xml_child(xml_child(d, 1), 1), 4), 4), 2)

# xml_attrs(xml_child(comunas, 1))
# xml_attrs(xml_child(comunas, 34))
radio2 <- xml_attrs(xml_children(radio))

radio2 <- bind_rows(radio2) %>%
  select(variable = name, descripcion = label, tipo = type,
         tamanio = size, decimales = decimals, rango = range)

radio2 <- radio2 %>%
  mutate(
    rango = str_replace_all(rango, " TO ", " - "),
    descripcion = cadena_a_titulo(descripcion)
  ) %>%
  mutate_if(is.character, function(x) { str_trim(x) }) %>%
  mutate(
    variable = str_to_lower(variable)
  )

radio <- radio2 %>%
  drop_na(descripcion) %>%
  mutate(rango = ifelse(rango == "", NA_character_, rango)) %>%
  mutate(tabla = "radio") %>%
  select(tabla, everything())

rm(radio2)

# frac ----

frac <-  xml_child(xml_child(xml_child(xml_child(d, 1), 1), 4), 4)



# xml_attrs(xml_child(comunas, 1))
# xml_attrs(xml_child(comunas, 34))
frac2 <- xml_attrs(xml_children(frac))

frac2 <- bind_rows(frac2) %>%
  select(variable = name, descripcion = label, tipo = type,
         tamanio = size, decimales = decimals, rango = range)

frac2 <- frac2 %>%
  mutate(
    rango = str_replace_all(rango, " TO ", " - "),
    descripcion = cadena_a_titulo(descripcion)
  ) %>%
  mutate_if(is.character, function(x) { str_trim(x) }) %>%
  mutate(
    variable = str_to_lower(variable)
  )

frac <- frac2 %>%
  drop_na(descripcion) %>%
  mutate(rango = ifelse(rango == "", NA_character_, rango)) %>%
  mutate(tabla = "frac") %>%
  select(tabla, everything())

rm(frac2)

# frac ----

frac <-  xml_child(xml_child(xml_child(xml_child(d, 1), 1), 4), 4)



# xml_attrs(xml_child(comunas, 1))
# xml_attrs(xml_child(comunas, 34))
frac2 <- xml_attrs(xml_children(frac))

frac2 <- bind_rows(frac2) %>%
  select(variable = name, descripcion = label, tipo = type,
         tamanio = size, decimales = decimals, rango = range)

frac2 <- frac2 %>%
  mutate(
    rango = str_replace_all(rango, " TO ", " - "),
    descripcion = cadena_a_titulo(descripcion)
  ) %>%
  mutate_if(is.character, function(x) { str_trim(x) }) %>%
  mutate(
    variable = str_to_lower(variable)
  )

frac <- frac2 %>%
  drop_na(descripcion) %>%
  mutate(rango = ifelse(rango == "", NA_character_, rango)) %>%
  mutate(tabla = "frac") %>%
  select(tabla, everything())

rm(frac2)

# dpto ----

dpto <-  xml_child(xml_child(xml_child(d, 1), 1), 4)




# xml_attrs(xml_child(comunas, 1))
# xml_attrs(xml_child(comunas, 34))
dpto2 <- xml_attrs(xml_children(dpto))

dpto2 <- bind_rows(dpto2) %>%
  select(variable = name, descripcion = label, tipo = type,
         tamanio = size, decimales = decimals, rango = range)

dpto2 <- dpto2 %>%
  mutate(
    rango = str_replace_all(rango, " TO ", " - "),
    descripcion = cadena_a_titulo(descripcion)
  ) %>%
  mutate_if(is.character, function(x) { str_trim(x) }) %>%
  mutate(
    variable = str_to_lower(variable)
  )

dpto <- dpto2 %>%
  drop_na(descripcion) %>%
  mutate(rango = ifelse(rango == "", NA_character_, rango)) %>%
  mutate(tabla = "dpto") %>%
  select(tabla, everything())

rm(dpto2)



# prov ----

prov <- xml_child(xml_child(d, 1), 1)




# xml_attrs(xml_child(comunas, 1))
# xml_attrs(xml_child(comunas, 34))
prov2 <- xml_attrs(xml_children(prov))

prov2 <- bind_rows(prov2) %>%
  select(variable = name, descripcion = label, tipo = type,
         tamanio = size, decimales = decimals, rango = range)

prov2 <- prov2 %>%
  mutate(
    rango = str_replace_all(rango, " TO ", " - "),
    descripcion = cadena_a_titulo(descripcion)
  ) %>%
  mutate_if(is.character, function(x) { str_trim(x) }) %>%
  mutate(
    variable = str_to_lower(variable)
  )

prov <- prov2 %>%
  drop_na(descripcion) %>%
  mutate(rango = ifelse(rango == "", NA_character_, rango)) %>%
  mutate(tabla = "dpto") %>%
  select(tabla, everything())

rm(prov2)


# unir ----

censo_variables <- persona %>%
  bind_rows(hogar) %>%
  bind_rows(vivienda) %>%
  bind_rows(frac) %>%
  bind_rows(radio) %>%
  bind_rows(dpto) %>%
  bind_rows(prov)

censo_variables <- censo_variables %>%
  mutate(
    tipo = str_to_lower(tipo),
    tamanio = as.integer(tamanio),
    decimales = as.integer(decimales)
  )

#censo_variables <- censo_variables %>%
#  mutate(
#    tabla = ifelse(tabla == "vivienda", "vivienda", tabla),
#  )

persona <- censo_tabla("persona")
vivienda <- censo_tabla("vivienda")
hogar <- censo_tabla("hogar")
#zonas <-  censo_tabla("zonas")
#comunas <- censo_tabla("comunas")
radio <- censo_tabla("radio")
frac <- censo_tabla("frac")
dpto <- censo_tabla("dpto")
prov <- censo_tabla("prov")

censo_variables <- censo_variables %>%
  mutate(
    pretipo = paste0("class(", tabla, "$", variable, ")")
  )

tipo <- NULL
for(i in seq_len(nrow(censo_variables))) {
  tipo[i] <- eval(parse(text = censo_variables$pretipo[i]))
}

censo_variables$tipo <- tipo

censo_variables <- censo_variables %>%
  select(-c(tamanio, decimales, pretipo))

censo_variables <- censo_variables %>%
  mutate(
    tabla = as_factor(tabla),
    tipo = as_factor(tipo)
  )

censo_codificacion_variables <- persona_codificacion %>%
  bind_rows(hogar_codificacion) %>%
  bind_rows(vivienda_codificacion)

censo_codificacion_variables <- censo_codificacion_variables %>%
  drop_na() %>%
  mutate(valor = as.integer(valor))

data.table::fwrite(censo_variables, "data-raw/variables.tsv", sep = "\t")
data.table::fwrite(censo_codificacion_variables, "data-raw/variables_codificacion.tsv", sep = "\t")
