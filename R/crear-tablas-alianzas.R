#crear csv coaliciones
library(tidyverse)

## 2017
# edomex
mex <- read_delim("../datos/Gob_2023_MEX.txt", delim = "|", escape_double = FALSE, trim_ws = TRUE)
alianza_pri_mex <- str_subset(colnames(mex), "PRI|PVEM|NVA_ALIANZA|_ES$|(^ES$)")
alianza_pri_mex

mex_tidy <- mex |>
  mutate(PRI_TOTAL = rowSums(across(alianza_pri_mex)),
         otros = CAND_IND1 + NUM_VOTOS_CAN_NREG + NUM_VOTOS_NULOS,
         total = NUM_VOTOS_VALIDOS) |>
  select(-alianza_pri_mex)
mex_tidy |>
  summarise(pri = sum(PRI_TOTAL),
            morena = sum(MORENA),
            pan = sum(PAN))
# coah
coah <- read_delim("../datos/Gob_2023_COAH.txt", delim = "|", escape_double = FALSE, trim_ws = TRUE)
alianza_pri <- str_subset(colnames(coah), "PRI|PVEM|NVA_ALIANZA|SI|PJ|PRC|PCP") |>
  str_subset("CASILLA|TEPJF", negate = TRUE)
alianza_pri
alianza_pan <- str_subset(colnames(coah), "PAN|UDC|PPC|_ES$|(^ES$)")
alianza_pan

coah_tidy <- coah |>
  mutate(cand_pri = rowSums(across(alianza_pri)),
         cand_pan = rowSums(across(alianza_pan)),
         otros = CAND_IND1 + CAND_IND2 + NUM_VOTOS_CAN_NREG + NUM_VOTOS_NULOS,
         total = NUM_VOTOS_VALIDOS)
#glimpse(coah_tidy)

partidos_mex <- tibble(ID_ESTADO = 15, CANDIDATO = "CAND_0", PARTIDO = "PAN") |>
  bind_rows(tibble(ID_ESTADO = 15, CANDIDATO = "CAND_1", PARTIDO = alianza_pri_mex)) |>
  bind_rows(tibble(ID_ESTADO = 15, CANDIDATO = "CAND_2", PARTIDO = "PRD")) |>
  bind_rows(tibble(ID_ESTADO = 15, CANDIDATO = "CAND_3", PARTIDO = "PT")) |>
  bind_rows(tibble(ID_ESTADO = 15, CANDIDATO = "CAND_4", PARTIDO = "MORENA")) |>
  bind_rows(tibble(ID_ESTADO = 15, CANDIDATO = "CAND_5", PARTIDO = "CAND_IND1")) |>
  bind_rows(tibble(ID_ESTADO = 15, CANDIDATO = "CAND_6", PARTIDO = "NUM_VOTOS_CAN_NREG")) |>
  bind_rows(tibble(ID_ESTADO = 15, CANDIDATO = "CAND_7", PARTIDO = "NUM_VOTOS_NULOS"))
partidos_coah <- tibble(ID_ESTADO = 5, CANDIDATO = "CAND_0", PARTIDO = alianza_pan) |>
  bind_rows(tibble(ID_ESTADO = 5, CANDIDATO = "CAND_1", PARTIDO = alianza_pri)) |>
  bind_rows(tibble(ID_ESTADO = 5, CANDIDATO = "CAND_2", PARTIDO = "PRD")) |>
  bind_rows(tibble(ID_ESTADO = 5, CANDIDATO = "CAND_3", PARTIDO = "PT")) |>
  bind_rows(tibble(ID_ESTADO = 5, CANDIDATO = "CAND_4", PARTIDO = "MORENA")) |>
  bind_rows(tibble(ID_ESTADO = 5, CANDIDATO = "CAND_5", PARTIDO = "CAND_IND1")) |>
  bind_rows(tibble(ID_ESTADO = 5, CANDIDATO = "CAND_6", PARTIDO = "CAND_IND2")) |>
  bind_rows(tibble(ID_ESTADO = 5, CANDIDATO = "CAND_7", PARTIDO = "NUM_VOTOS_CAN_NREG")) |>
  bind_rows(tibble(ID_ESTADO = 5, CANDIDATO = "CAND_8", PARTIDO = "NUM_VOTOS_NULOS"))
readr::write_csv(rbind(partidos_mex, partidos_coah), "../datos/coaliciones-2017.csv")


# ## 2011
# # edomex
# mex <- read_delim("../datos/Gob_2011_MEX_2023-02-17.txt", delim = "|", escape_double = FALSE, trim_ws = TRUE)
# 
# # coah
# coah <- read_delim("../datos/Gob_2011_COAH_2023-02-17.txt", delim = "|", escape_double = FALSE, trim_ws = TRUE)
# alianza_pri <- str_subset(colnames(coah), "PRI|CC1|PVEM|PNA|PSDC|PPC")
# alianza_pri
# alianza_pt <- str_subset(colnames(coah), "CC2|PT|CONV")
# alianza_pt
# 
# partidos_mex <- tibble(ID_ESTADO = 15, CANDIDATO = "CAND_0", PARTIDO = "PAN") |>
#   bind_rows(tibble(ID_ESTADO = 15, CANDIDATO = "CAND_1", PARTIDO = "UPT")) |>
#   bind_rows(tibble(ID_ESTADO = 15, CANDIDATO = "CAND_2", PARTIDO = "UPM")) |>
#   bind_rows(tibble(ID_ESTADO = 15, CANDIDATO = "CAND_3", PARTIDO = "NUM_VOTOS_NREG")) |>
#   bind_rows(tibble(ID_ESTADO = 15, CANDIDATO = "CAND_4", PARTIDO = "NUM_VOTOS_NULOS")) 
# 
# partidos_coah <- tibble(ID_ESTADO = 5, CANDIDATO = "CAND_0", PARTIDO = "COALICION_COAHUILA_LIBRE_Y_SEGURO") |>
#   bind_rows(tibble(ID_ESTADO = 5, CANDIDATO = "CAND_1", PARTIDO = alianza_pri)) |>
#   bind_rows(tibble(ID_ESTADO = 5, CANDIDATO = "CAND_2", PARTIDO = "PRD")) |>
#   bind_rows(tibble(ID_ESTADO = 5, CANDIDATO = "CAND_3", PARTIDO = alianza_pt)) |>
#   bind_rows(tibble(ID_ESTADO = 5, CANDIDATO = "CAND_4", PARTIDO = "NUM_VOTOS_NULOS"))
# 
# readr::write_csv(rbind(partidos_mex, partidos_coah), "../datos/coaliciones-2011.csv")
