

simular_cuantiles <- function(id, datos, datos_nuevos = NULL, reg, horas_censura = 5,
                              solo_tiempos = FALSE, permutar = FALSE, prop_azar = 0.3){
  if(is.null(datos_nuevos)){
    datos_nuevos <- datos
  }
  # simular del modelo original

  mat_cuantiles <- predict(reg, newdata = datos,
                           type = "quantile", p = seq(0.001, 0.999, by = 0.001))
  rownames(mat_cuantiles) <- NULL
  sims_sin_censura <- apply(mat_cuantiles, 1, function(cuantiles){
    sample(cuantiles, 1)
  })
  sims_tbl <- as_tibble(datos) |>
    mutate(sim_tiempo_sc = sims_sin_censura)
  ## bootstrap paramétrico: ajustar a simulacion
  obs_boot_1 <- sims_tbl |>
    mutate(tiempo_huso = sim_tiempo_sc, status = 1)
  reg_boot <- survreg(formula = formula,
                      data = as.data.frame(obs_boot_1),
                      dist = 'loglogistic',
                      control = survreg.control(maxiter = 5000),
                      model = TRUE, x = TRUE, y = TRUE)
  ## simular modelo bootstrap y permutar
  if(permutar){
    cands_tbl <- datos_nuevos |> select(cand_1_std:cand_3_std)
    nombres_perm <- sample(names(cands_tbl), 3)
    names(cands_tbl) <- nombres_perm
    datos_nuevos <- datos_nuevos |>  select(!(cand_1_std:cand_3_std))
    datos_nuevos <- bind_cols(datos_nuevos, cands_tbl)
  }
  mat_cuantiles <- predict(reg_boot, newdata = datos_nuevos,
                           type = "quantile", p = seq(0.005, 0.995, by = 0.001))
  rownames(mat_cuantiles) <- NULL
  sims_sin_censura <- apply(mat_cuantiles, 1, function(cuantiles){
    sample(cuantiles, 1)
  })
  # prep simulacions
  sims_tbl <- as_tibble(datos_nuevos) |>
    mutate(sim_tiempo_sc = sims_sin_censura)  |>
    mutate(max_time = horas_censura) |> # + ifelse(huso == 0, 1, 0)) |>
    mutate(status_sim = ifelse(sim_tiempo_sc > max_time, 0, 1)) |>
    ungroup() |>
    mutate(tiempo_obs_sim = ifelse(status_sim == 0, max_time, sim_tiempo_sc)) |>
    select(tiempo_obs_sim, status_sim, state_abbr) |>
    rename(tiempo = tiempo_obs_sim, status = status_sim)
  sims_tbl <- sims_tbl |> mutate(id = id) |>
    mutate(id_casilla = row_number())
  if(prop_azar > 0.0){
    sims_tbl <- sims_tbl |> mutate(perm = rbinom(nrow(sims_tbl), 1, prop_azar))
    sims_1_tbl <- filter(sims_tbl, perm == 1)
    sims_2_tbl <- filter(sims_tbl, perm == 0)
    sims_1_tbl <- mutate(sims_1_tbl, tiempo = sample(tiempo, length(tiempo)))
    sims_tbl <- bind_rows(sims_1_tbl, sims_2_tbl) |>
      arrange(id_casilla) |>
      select(state_abbr, status, tiempo, id)
  }
  ## producir salidas
  if(solo_tiempos){
    salida <- sims_tbl
  } else {
    gg <- ggsurvplot(survfit(Surv(tiempo, status) ~ state_abbr, sims_tbl), data = sims_tbl)
    gg <- gg$data.survplot |> mutate(id = id)
    salida <- gg
  }
  salida
}

prep_muestra_orden <- function(computos, llegadas_filtrado_tbl, frac = 0.15, prop_azar = 0.3, estado_abbr){
  muestra <- select_sample_prop(computos, stratum = ID_DTTO_LOCAL_Ago2022, frac = frac)
  #nrow(muestra)
  muestra_pred <- muestra |> mutate(state_abbr = estado_abbr) |>
    mutate(
      tipo_seccion = ifelse(TIPO_SECC_Ago2022 == "U", 1, 2),
      tipo_seccion = ifelse(TIPO_SECC_Ago2022 == "R", 3, tipo_seccion),
      tipo_seccion = ifelse(is.na(TIPO_SECC_Ago2022), 1, tipo_seccion),
      tipo_casilla = str_sub(CASILLA, 1, 1))
  tiempos <- simular_cuantiles(1, llegadas_filtrado_tbl, muestra_pred, reg = reg_2,
                               solo_tiempos = TRUE, horas_censura = 100, permutar = TRUE, prop_azar = prop_azar) |>
    pull(tiempo)
  muestra_pred$tiempo <- rank(tiempos, ties.method = "random")
  muestra_pred <- arrange(muestra_pred, tiempo)
  muestra_pred
}
