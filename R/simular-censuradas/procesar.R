library(tidyverse)
library(lubridate)
library(broom)


# Leer cómputos
alianzas_tbl <- read_csv("../datos/coaliciones-2017.csv")

computos_mex_tbl <- read_delim("../datos/Gob_2023_MEX.txt", delim = "|", 
  escape_double = FALSE, trim_ws = TRUE) |> 
  mutate(num_casilla = row_number())

mex_votos_tbl <- computos_mex_tbl |> 
  pivot_longer(cols = PAN:CAND_IND1, names_to = "PARTIDO", values_to = "votos") |> 
  left_join(alianzas_tbl) |> 
  select(-PARTIDO) |> 
  group_by(num_casilla, CANDIDATO) |> 
  summarise(votos = sum(votos)) |> 
  pivot_wider(names_from = CANDIDATO, values_from = votos) |> 
  ungroup()

computos_mex_tbl <- computos_mex_tbl |> 
  left_join(mex_votos_tbl) |> 
  mutate(cand_1 = CAND_1, cand_2 = CAND_4, cand_3 = CAND_2)


# coah
computos_coah_tbl <- read_delim("../datos/Gob_2023_COAH.txt", 
  delim = "|", escape_double = FALSE, trim_ws = TRUE) |> 
  mutate(num_casilla = row_number())
coah_votos_tbl <- computos_coah_tbl |> 
  pivot_longer(cols = PAN:CAND_IND1, names_to = "PARTIDO", values_to = "votos") |> 
  left_join(alianzas_tbl) |> 
  select(-PARTIDO) |> 
  group_by(num_casilla, CANDIDATO) |> 
  summarise(votos = sum(votos)) |> 
  pivot_wider(names_from = CANDIDATO, values_from = votos) |> 
  ungroup()

computos_coah_tbl <- computos_coah_tbl |> 
  left_join(coah_votos_tbl) |> 
  mutate(cand_1 = CAND_1, cand_2 = CAND_0, cand_3 = CAND_4)




lista_computos <-
  list(computos_coah_tbl, computos_mex_tbl)

# quitar casillas sin ID_DTTO_2021
#lista_computos <- map(lista_computos, ~ filter(.x, !is.na(ID_DTTO_LOCAL_Nov2021)))
#lista_computos <- map(lista_computos, ~ filter(.x, !is.na(VOTOS_NULOS) ))

map_df(lista_computos, function(computos){
   computos |> group_by(ID_ESTADO) |>
     summarise(cand_1 = sum(cand_1), cand_2 = sum(cand_2),
                         cand_3 = sum(cand_3), total = sum(TOTAL_VOTOS)) |>
     pivot_longer(cand_1:cand_3, names_to = "cand", values_to = "votos") |>
     mutate(prop = votos / first(total)) |> select(-votos) |>
     pivot_wider(names_from = cand, values_from = prop)
 })
# 
lista_computos <- map(lista_computos, function(computos){
   computos |>
     mutate(cand_1_std = (log(1+cand_3) - mean(log(1+cand_3)))/sd(log(1+cand_3)),
            cand_2_std = (log(1+cand_2) - mean(log(1+cand_2)))/sd(log(1+cand_2)),
            cand_3_std = (log(1+cand_1) - mean(log(1+cand_1)))/sd(log(1+cand_1)),
            total_std = (log(1+TOTAL_VOTOS) - mean(log(1+TOTAL_VOTOS)))/sd(log(1+TOTAL_VOTOS)))
 })



###### Leer conteo 2018

estados <- c("COAH", "MEX")

# codigos
estados_tbl <- read_csv("../datos/datos-sim/df_mxstate.csv")
# marco nal
marco <- read_csv("../datos/datos-sim/LISTADO_CASILLAS_2018.csv") |>
  mutate(CLAVE_CASILLA = paste0(str_sub(ID, 2, 3), str_sub(ID, 6, -1))) |>
  rename(LISTA_NOMINAL_CASILLA = LISTA_NOMINAL) |>
  mutate(estrato = ID_ESTRATO_F) |>
  mutate(tipo_seccion = factor(TIPO_SECCION))
# datos en shapes de INEGI
# inegi_shps unión de coords casillas y polígonos secciones
# inegi_shps <- read_csv("../datos/inegi_seccion.csv")
# inegi_dbfs <- list.files("../datos/shapefiles/inegi_vivienda_2010",
#                          full.names = TRUE, pattern = ".dbf", recursive = TRUE) |>
#   map_df(~foreign::read.dbf(., as.is = TRUE)) |>
#   select(ENTIDAD, SECCION, OCUPVIVPAR:VPH_SNBIEN)
# # union INEGI shps (puntos en polígonos) a marco
# marco_inegi <- marco |>
#   left_join(inegi_shps, by = c("SECCION", "iD_ESTADO" = "ENTIDAD"))
# # faltantes unimos por variable sección
# marco_inegi <- marco_inegi |>
#   filter(is.na(OCUPVIVPAR)) |>
#   select(ID:tipo_seccion) |>
#   left_join(inegi_dbfs, by = c("SECCION", "iD_ESTADO" = "ENTIDAD")) |>
#   bind_rows(filter(marco_inegi, !is.na(OCUPVIVPAR)))
# unios INEGI a marco
#marco_inegi <- marco |>
#  left_join(select(inegi, -MUNICIPIO), by = c("SECCION", "iD_ESTADO" = "ENTIDAD"))
# write_csv(marco_inegi, file = "../datos/marco_ext.csv")
marco_ext <- read_csv(file = "../datos/datos-sim/marco_ext.csv")
# muestra seleccionada
muestra_selec <- read_csv("../datos/datos-sim/4-ConteoRapido18MUESTRA-ELECCION-PRESIDENCIAL.csv") |>
  mutate(CLAVE_CASILLA = paste0(str_sub(ID, 2, 3), str_sub(ID, 6, -1)))
nrow(muestra_selec)

### Conteos
encabezado <- read_lines("../datos/datos-sim/presidencia.csv", skip = 6, n_max = 1) |>
  str_replace("\\|\\|", "") |>
  str_split_fixed("\\|", n = 42)

conteo <- read_delim("../datos/datos-sim/presidencia.csv", delim = "|",
                     col_names = encabezado,
                     skip = 7,
                     quote = "'", na = c("", "NA", "-", " "), locale = locale(encoding = "ISO-8859-1")) |>
  mutate(cand_1 = MORENA + PT + `ENCUENTRO SOCIAL` + PT_MORENA +
           MORENA_PES + PT_PES + PT_MORENA_PES,
         cand_2 = PAN + PRD + `MOVIMIENTO CIUDADANO` + PAN_PRD + PAN_MC +
           PRD_MC + PAN_PRD_MC,
         cand_3 = PRI + PVEM + `NUEVA ALIANZA` + PRI_PVEM + PRI_NA +
           PVEM_NA + PRI_PVEM_NA) |>
  left_join(estados_tbl |>
              rename(ID_ESTADO = region) |>
              mutate(ID_ESTADO = as.numeric(ID_ESTADO)),
            by = "ID_ESTADO") |>
  filter(TIPO_CASILLA != "M") |>
  mutate(tipo_casilla = factor(TIPO_CASILLA, levels= c("B", "C", "E", "S"))) |>
  mutate(lista_nominal_log = log(1 + as.numeric(LISTA_NOMINAL_CASILLA))) |>
  ungroup() |>
  mutate(media_ln_log = mean(lista_nominal_log, na.rm = TRUE)) |>
  mutate(ln_log_c = lista_nominal_log - media_ln_log) |>
  mutate(huso = case_when(state_abbr %in% c("BC", "SON") ~ 2,
                          state_abbr %in% c("CHIH", "BCS", "NAY", "SIN") ~ 1,
                          TRUE ~ 0)) |>
  left_join(marco_ext |>
              select(CLAVE_CASILLA, estrato, tipo_seccion, TIPO_CASILLA, TVIVHAB:VPH_SNBIEN),
            by = c("CLAVE_CASILLA")) |>
  filter(TOTAL_VOTOS_CALCULADOS != 0) |> # alrededor de 200 casillas no entregadas
  filter(!is.na(tipo_seccion)) # casillas

conteo <- conteo |>
  mutate(across(VPH_PISOTI:VPH_S_ELEC, ~ .x / (1 + TVIVPARHAB), .names = "p_{.col}"))

# agregar componente principal
comps_1 <- prcomp(conteo |> select(p_VPH_PISOTI:p_VPH_TV),
                  center = TRUE, scale = TRUE)
#summary(comps_1)

conteo <- conteo |>
  nest(data = everything()) |>
  mutate(pca = map(data, ~ prcomp(.x |> select(p_VPH_PISOTI:p_VPH_TV),
                                  center = TRUE, scale = TRUE)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y)))
conteo <- select(conteo, pca_aug) |>
  unnest(cols = pca_aug)

conteo |> group_by(ID_ESTADO, state_abbr) |>
  filter(state_abbr %in% estados) |>
  summarise(cand_1 = sum(cand_1) / sum(TOTAL_VOTOS_CALCULADOS),
            cand_2 = sum(cand_2) / sum(TOTAL_VOTOS_CALCULADOS),
            cand_3 = sum(cand_3) / sum(TOTAL_VOTOS_CALCULADOS))



#write_csv(conteo, file = "../datos/conteo_inegi.csv")
# recuperar conteos de muestra completa
datos_muestra <- muestra_selec |>
  select(-LISTA_NOMINAL) |>
  left_join(conteo |>
              select(CLAVE_CASILLA, LISTA_NOMINAL_CASILLA, cand_1:cand_3,
                     TOTAL_VOTOS_CALCULADOS, lista_nominal_log, ln_log_c,
                     huso, tipo_casilla,
                     estrato, tipo_seccion, TVIVHAB:.fittedPC5) |>
              rename(LISTA_NOMINAL = LISTA_NOMINAL_CASILLA),
            by = c("CLAVE_CASILLA"))

# casillas de la muestra con faltantes:
datos_muestra |> group_by(is.na(TOTAL_VOTOS_CALCULADOS)) |>
  count() ## 33 casillas
datos_muestra <- datos_muestra |>
  filter(!is.na(TOTAL_VOTOS_CALCULADOS)) |>
  left_join(estados_tbl |> mutate(iD_ESTADO = region))
## asignación muestra_selec
estratos_nal <- conteo |>
  group_by(state_abbr, estrato) |> count() |>
  ungroup() |> select(-state_abbr)


# muestra obtenida por hora de llegada, calcular huso
# Clave casilla: ID_ESTADO-SECCION-TIPO_CASILLA-ID_CASILLA-EXT_CONTIGUA
remesas <- read_delim("../datos/datos-sim/remesas_nal/remesas_nal/REMESAS0100020000.txt",
                      delim = "|", skip = 1) |>
  mutate(timestamp = ymd_hms(paste(ANIO, MES, DIA, HORA, MINUTOS, SEGUNDOS, sep = "-"),
                             tz = "America/Mexico_City")) |>
  mutate(ID_ESTADO = str_pad(iD_ESTADO, 2, pad = "0"),
         SECCION = str_pad(SECCION,4, pad = "0"),
         TIPO_CASILLA = ifelse(TIPO_CASILLA == "MEC", "M", TIPO_CASILLA),
         ID_CASILLA = str_pad(ID_CASILLA, 2, pad = "0"),
         EXT_CONTIGUA = str_pad(EXT_CONTIGUA, 2, pad = "0")) |>
  mutate(CLAVE_CASILLA = paste0(ID_ESTADO, SECCION, TIPO_CASILLA, ID_CASILLA, EXT_CONTIGUA))

muestra_tot <-
  left_join(
    datos_muestra |> select(CLAVE_CASILLA, LISTA_NOMINAL, TIPO_SECCION,
                            ID_ESTRATO_F, ID_AREA_RESPONSABILIDAD, state_abbr,
                            TOTAL_VOTOS_CALCULADOS, tipo_casilla, lista_nominal_log,
                            ln_log_c,
                            tipo_seccion, LISTA_NOMINAL, huso, cand_1:cand_3, TVIVHAB:.fittedPC5),
    remesas |> select(-TIPO_SECCION, - TIPO_CASILLA, -LISTA_NOMINAL),
    by = c("CLAVE_CASILLA")) |>
  mutate(llegada = ifelse(is.na(TOTAL), 0, 1)) # llegó antes de las 12:00 ?


# Construir datos de llegadas
llegadas_tbl <- muestra_tot |>
  ungroup() |>
  select(timestamp, huso, llegada, state_abbr, tipo_casilla,
         tipo_seccion,TVIVPARHAB, VPH_INTER, VPH_PISOTI, VPH_LAVAD, VPH_REFRI,
         VPH_CEL, .fittedPC1:.fittedPC5,
         cand_1, cand_2, cand_3,
         lista_nominal_log, ln_log_c,
         TOTAL_VOTOS_CALCULADOS,
         LISTA_NOMINAL, ID_ESTRATO_F.x, ID_AREA_RESPONSABILIDAD.x,
         TOTAL, ID_ESTADO) |>
  mutate(timestamp = if_else(is.na(timestamp),
                             ymd_hms("2018-07-01 23:59:59", tz = "America/Mexico_City"), timestamp)) |>
  mutate(timestamp = with_tz(timestamp, "America/Mexico_City")) |>
  mutate(tiempo = difftime(timestamp,
                           ymd_hms("2018-07-01 18:30:00", tz ="America/Mexico_City"),
                           units = "hours")) |>
  mutate(cae = paste0(ID_ESTRATO_F.x, ID_AREA_RESPONSABILIDAD.x)) |>
  group_by(cae) |>
  mutate(n_reporte = rank(timestamp)) |>
  ungroup() |>
  group_by(state_abbr) |>
  mutate(status = llegada)

# Tabla para ajustar modelos
llegadas_tbl <- llegadas_tbl |>
  mutate(tiempo_huso = ifelse(tiempo - huso > 0, tiempo - huso, 0.001))
llegadas_tbl <- llegadas_tbl |> group_by(state_abbr) |>
  mutate(cand_1_std = (log(1+cand_3) - mean(log(1+cand_3)))/sd(log(1+cand_3)),
         cand_2_std = (log(1+cand_2) - mean(log(1+cand_2)))/sd(log(1+cand_2)),
         cand_3_std = (log(1+cand_1) - mean(log(1+cand_1)))/sd(log(1+cand_1)),
         total_std = (log(1+TOTAL_VOTOS_CALCULADOS) - mean(log(1+TOTAL_VOTOS_CALCULADOS)))/
           sd(log(1+TOTAL_VOTOS_CALCULADOS))) |>
  ungroup()

llegadas_filtrado_tbl <- llegadas_tbl |> filter(state_abbr %in% estados)

