library(survminer)
library(survival)
library(tidyverse)


set.seed(1385)
#train_ind <- sample(1:nrow(X), floor(0.85 * nrow(X)))
#test_ind <- setdiff(1:nrow(X), train_ind)
y_lower_bound <- llegadas_filtrado_tbl$tiempo_huso
y_upper_bound <- ifelse(llegadas_filtrado_tbl$status == 0, Inf, y_lower_bound)

formula <- as.formula("Surv(tiempo_huso, status) ~ 1 +
      state_abbr + 
      state_abbr:I(tipo_seccion == 1) +
      state_abbr:I(tipo_seccion == 2) +
      state_abbr:cand_1_std +
      state_abbr:cand_2_std +
      state_abbr:cand_3_std +
      state_abbr:total_std  ")
reg_2 <- survreg(formula,
                 llegadas_filtrado_tbl,
                 dist='loglogistic', x = TRUE,
                 control = survreg.control(maxiter = 50000))

preds_tbl <- tibble(pred = predict(reg_2, llegadas_filtrado_tbl), y_lower_bound = y_lower_bound)
ggplot(preds_tbl, aes(x = pred, y = y_lower_bound)) +
  geom_point() + coord_equal()
concordance(reg_2)
