# library(ProjectTemplate)
# load.project()



# 
# # Add number of routes each indsamler is registered for
# d$indsamler_sub
# d$hs_sub
# missing_rutes_sub <- merge(d$indsamler_sub,
#       d$routes_sub,
#       by = "indsamlerid",
#       all.x = TRUE)[is.na(ruteid.y), .(indsamlerid, ruteid.x)]
# 
# d$ruter[indsamlerid  %in% missing_rutes_sub$indsamlerid]
# d$ruter[ruteid  %in% missing_rutes_sub$ruteid.x]
# d$indsamler_sub[indsamlerid.x != indsamlerid.y]

# PREDICT NO-SHOWS --------------------------------------------------------

forms <- list()

forms$full <- no_show ~ hs_group + (age) + gender
forms$full.rcs <- no_show ~ hs_group + rcs(age) + gender
forms$hs.age <- no_show ~ hs_group + age
forms$hs <- no_show ~ hs_group

# model <- list()
# model$full <- lrm(forms$a, data = d$indsamler_sub)
# model$full.rcs <- lrm(forms$b, data = d$indsamler_sub)
# model$hs_gender <- lrm(forms$c, data = d$indsamler_sub)
# model$gender <- lrm(forms$d, data = d$indsamler_sub)
# model$hs  <- lrm(forms$e, data = d$indsamler_sub)

d.inds[, "model_out" := list(map(forms, lrm, maxit = 1000, data = data_training[[1]]))]
d.inds$data_test[[1]][, predictions := list(map(d.inds$model_out[[1]], predictRisk, newdata =
                                                        d.inds$data_test[[1]]))]
tmp <- d.inds$data_test[[1]][, predictions[[1]]]
tmp_names <- paste0("predict_", names(forms))
setnames(tmp, tmp_names)

d.inds$data_test[[1]] <- cbind(d.inds$data_test[[1]], tmp)
d.inds$data_test[[1]][, predictions := NULL]


d.inds[, scores := list(Score(
  list(
    "full" = model_out[[1]]$full,
    "full.rcs" = model_out[[1]]$full.rcs,
    "hs.age" = model_out[[1]]$hs.age,
    "hs" = model_out[[1]]$hs
    
    
    
  ),
  formula = no_show ~ 1,
  data = data_test[[1]],
  plots = c("roc", "calibration")
))]

d.inds$model_out[[1]]$hs.age
d.inds$scores[[1]]

res <- exp(coef(d.inds$model_out[[1]]$hs.age))
d.inds$model_out[[1]]$hs.age


dd <- datadist(d.inds$data_training[[1]])
options(datadist="dd")

exp(coef(d.inds$model_out[[1]]$hs.age) + 1.96 * 0.086489)
res.no.shows <- round(1/summary(d.inds$model_out[[1]]$hs.age)[, c("Effect", "Lower 0.95", "Upper 0.95")], digits = 1)


# plotROC(d.inds$scores[[1]])
# plotCalibration(d.inds$scores[[1]])


# # # Predict number of routes completed --------------------------------------
# # # Predict failure for HS group --------------------------------------------
# #
forms_hs <- list()
forms_hs$a <-
  out_15ruter ~ rcs(lag_mean) + rcs(indtastings_mean) + antal_koordinatorer
forms_hs$b <- out_15ruter ~ rcs(lag_mean) + rcs(indtastings_mean)
forms_hs$b.4 <- out_15ruter ~ rcs(lag_mean, 4) + rcs(indtastings_mean)
forms_hs$mean.rcs <- out_15ruter ~ rcs(lag_mean)
forms_hs$mean.rcs.3 <- out_15ruter ~ rcs(lag_mean, 4)
forms_hs$d <-
  out_15ruter ~ rcs(lag_mean) + (indtastings_mean)
forms_hs$e <-
  out_15ruter ~ rcs(lag_mean) + (oprettet_before_dday)
forms_hs$e.4 <-
  out_15ruter ~ rcs(lag_mean, 4) + (oprettet_before_dday)
forms_hs$lag_5 <-
  out_15ruter ~ (lag_5_in_days) + (oprettet_before_dday)


forms_hs <- list()
forms_hs$a <-
  antal_ruter_med_hjerteid ~ rcs(lag_mean) + rcs(indtastings_mean) + antal_koordinatorer
forms_hs$b <- antal_ruter_med_hjerteid ~ rcs(lag_mean) + rcs(indtastings_mean)
forms_hs$b.4 <- antal_ruter_med_hjerteid ~ rcs(lag_mean, 4) + rcs(indtastings_mean)
forms_hs$mean.rcs <- antal_ruter_med_hjerteid ~ rcs(lag_mean)
forms_hs$mean.rcs.3 <- antal_ruter_med_hjerteid ~ rcs(lag_mean, 4)
forms_hs$d <-
  antal_ruter_med_hjerteid ~ rcs(lag_mean) + (indtastings_mean)
forms_hs$e <-
  antal_ruter_med_hjerteid ~ rcs(lag_mean) + (oprettet_before_dday)
forms_hs$lag_mean <-
  antal_ruter_med_hjerteid ~ rcs(lag_mean)
forms_hs$lag_5 <-
  antal_ruter_med_hjerteid ~ rcs(lag_5_in_days) + (oprettet_before_dday)




# FIT MODELS --------------------------------------------------------------
d.validation[, "model_out" := list(map(forms_hs, Glm, maxit = 1000, data = data_training[[1]]))]
d.validation$model_out[[1]]$lag_5
d.validation$model_out[[1]]$lag_mean

# Predict risks on test data
# d.validation$data_test[[1]][, predictions := list(map(d.validation$model_out[[1]], predictRisk, newdata =
#                                                         d.validation$data_test[[1]]))]
# 
# tmp <- d.validation$data_test[[1]][, predictions[[1]]]
# tmp_names <- paste0("predict_", names(forms_hs))
# setnames(tmp, tmp_names)
# 
# d.validation$data_test[[1]] <-
#   cbind(d.validation$data_test[[1]], tmp)
# d.validation$data_test[[1]][, predictions := NULL]
# 
# 
# dd <- datadist(d.validation$data_training[[1]])
# options(datadist="dd")
# 
# 
# d.validation[, scores := list(Score(
#   list(
#     "a" = model_out[[1]]$a,
#     "b" = model_out[[1]]$b,
#     "b.4" = model_out[[1]]$b.4,
#     "mean.rcs" = model_out[[1]]$mean.rcs,
#     "mean.rcs.3" = model_out[[1]]$mean.rcs.3,
#     "d" = model_out[[1]]$d,
#     "e" = model_out[[1]]$e,
#     "lag_mean" = model_out[[1]]$lag_mean
#   ),
#   formula = out_15ruter ~ 1,
#   data = data_test[[1]],
#   plots = c("roc", "calibration")
# ))]
# 
# d.validation$scores
# 
# plotROC(d.validation$scores[[1]])
# plotCalibration(d.validation$scores[[1]])
