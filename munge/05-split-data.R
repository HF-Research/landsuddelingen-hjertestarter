tmp <- d$hjertestarter[!is.na(lag_mean) & lag_HS_inds > 0 & oprettet_date < gl$cut.day]

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


tmp <- d$indsamler_sub

n <- nrow(tmp)
set.seed(1)
training_ids <- sample(1:n, size = 0.632 * n, replace = 0)
test_ids <- (1:n)[!(1:n) %in% training_ids]


d.inds <- tmp[training_ids, list(data_training = list(.SD))]
tmp1 <- tmp[test_ids, list(data_test = list(.SD))]
tmp2 <- tmp[, list(data_all = list(.SD))]
d.inds <- cbind(d.inds, tmp1, tmp2)

d.inds[, n_training := nrow(d.inds$data_training[[1]])]
d.inds[, n_test := nrow(d.inds$data_test[[1]])]
