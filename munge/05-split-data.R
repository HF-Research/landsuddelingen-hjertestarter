tmp <- d$hjertestarter[!is.na(lag_mean)]

n <- nrow(tmp)
set.seed(1)
training_ids <- sample(1:n, size = 0.632 * n, replace = 0)
test_ids <- (1:n)[!(1:n) %in% training_ids]


d.validation <- tmp[training_ids, list(data_training = list(.SD))]
tmp1 <- tmp[test_ids, list(data_test = list(.SD))]
tmp2 <- tmp[, list(data_all = list(.SD))]
d.validation <- cbind(d.validation, tmp1, tmp2)

d.validation[, n_training := nrow(d.validation$data_training[[1]])]
d.validation[, n_test := nrow(d.validation$data_test[[1]])]
