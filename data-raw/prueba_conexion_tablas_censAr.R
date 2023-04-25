

library(dplyr)
library(censAr)

### UNION DE TODAS LAS TABLAS
 tbl(censo_conectar(), "prov") %>%
  inner_join(tbl(censo_conectar(), "DPTO"), by = "PROV_REF_ID" ) %>%
  inner_join(tbl(censo_conectar(), "FRAC"), by = "DPTO_REF_ID" )%>%
  inner_join(tbl(censo_conectar(), "RADIO"), by = "FRAC_REF_ID" )%>%
  inner_join(tbl(censo_conectar(), "VIVIENDA"), by = "RADIO_REF_ID" ) %>%
  inner_join(tbl(censo_conectar(), "HOGAR"), by = "VIVIENDA_REF_ID" )%>%
  inner_join(tbl(censo_conectar(), "PERSONA"), by = "HOGAR_REF_ID" )
