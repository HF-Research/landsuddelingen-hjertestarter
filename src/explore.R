library(ProjectTemplate)
load.project()




# Add number of routes each indsamler is registered for
d$indsamler_sub
d$hs_sub
missing_rutes_sub <- merge(d$indsamler_sub,
      d$routes_sub,
      by = "indsamlerid",
      all.x = TRUE)[is.na(ruteid.y), .(indsamlerid, ruteid.x)]

d$ruter[indsamlerid  %in% missing_rutes_sub$indsamlerid]
d$ruter[ruteid  %in% missing_rutes_sub$ruteid.x]
d$indsamler_sub[indsamlerid.x != indsamlerid.y]

# PREDICT NO-SHOWS --------------------------------------------------------

forms <- list()

forms$a <- no_show ~ hs_group + (age) + gender
forms$b <- no_show ~ hs_group + rcs(age) + gender
forms$c <- no_show ~ hs_group + gender
forms$d <- no_show ~ gender
forms$e <- no_show ~ hs_group
d$indsamler_sub
lrm(forms$a, data = d$indsamler_sub)
lrm(forms$b, data = d$indsamler_sub)
lrm(forms$c, data = d$indsamler_sub)
lrm(forms$d, data = d$indsamler_sub)
lrm(forms$e, data = d$indsamler_sub)
exp(coef(lrm(forms$e, data = d$indsamler_sub)))


# Predict number of routes completed --------------------------------------






# FORMAULS ----------------------------------------------------------------
forms <- list()
forms$a <-
  out_15ruter ~ rcs(lag_mean) + rcs(indtastings_mean) + antal_koordinatorer
forms$b <- out_15ruter ~ rcs(lag_mean) + rcs(indtastings_mean)
forms$b.4 <- out_15ruter ~ rcs(lag_mean, 4) + rcs(indtastings_mean)
forms$mean.rcs <- out_15ruter ~ rcs(lag_mean)
forms$mean.rcs.3 <- out_15ruter ~ rcs(lag_mean, 4)
forms$d <-
  out_15ruter ~ rcs(lag_mean) + (indtastings_mean)
forms$e <- 
  out_15ruter ~ rcs(lag_mean) + (oprettet_before_dday)
forms$e.4 <- 
  out_15ruter ~ rcs(lag_mean, 4) + (oprettet_before_dday)




# FIT MODELS --------------------------------------------------------------
dat.validation[, "model_out" := list(map(forms, lrm, maxit = 1000, data = data_training[[1]]))]

# Predict risks on test data
dat.validation$data_test[[1]][, predictions := list(map(dat.validation$model_out[[1]], predictRisk, newdata =
                                                                       dat.validation$data_test[[1]]))]

tmp <- dat.validation$data_test[[1]][, predictions[[1]]]
tmp_names <- paste0("predict_", names(forms))
setnames(tmp, tmp_names)

dat.validation$data_test[[1]] <- cbind(dat.validation$data_test[[1]], tmp)
dat.validation$data_test[[1]][, predictions := NULL]


dat.validation[, scores := list(Score(
  list(
    "a" = model_out[[1]]$a,
    "b" = model_out[[1]]$b,
    "b.4" = model_out[[1]]$b.4,
    "mean.rcs" = model_out[[1]]$mean.rcs,
    "mean.rcs.3" = model_out[[1]]$mean.rcs.3,
    "d" = model_out[[1]]$d,
    "e" = model_out[[1]]$e,
    "e.4" = model_out[[1]]$e.4
  ),
  formula = out_15ruter ~ 1,
  data = data_test[[1]],
  plots = c("roc", "calibration")
))]

dat.validation$scores

plotROC(dat.validation$scores[[1]])
plotCalibration(dat.validation$scores[[1]])

