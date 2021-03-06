# JOIN GROUP LEVEL DATA TO INDIVIDUALS --------------------------------
var_to_join <- c("oprettet", "antal_ruter_med_hjerteid", "out_15ruter", "hjertestarterid")
d$indsamlerny <- merge(d$indsamlerny, d$hjertestarter[, ..var_to_join], by = "hjertestarterid", all.x = TRUE)



# Count order of indsamler signups. This allows us to easily examine signup
# order
d$indsamlerny[, reg_order := 1] %>%
  .[, reg_order := cumsum(reg_order), by = hjertestarterid]

d$indsamler_sub[, reg_order := 1] %>%
  .[, reg_order := cumsum(reg_order), by = hjertestarterid]
# Time between successive enrollments, and avg time. 
setorder(d$indsamlerny, hjertestarterid, indtastningstidspunkt)
d$indsamlerny[, lag_time := indtastningstidspunkt - shift(indtastningstidspunkt), by = hjertestarterid] %>%
  .[, lag_time_min := as.numeric(lag_time / 60)] %>% 
  .[, lag_mean := mean(as.numeric(lag_time), na.rm = TRUE) / 3600, by = hjertestarterid] %>%
  .[, lag_median := median(as.numeric(lag_time), na.rm = TRUE) / 3600, by = hjertestarterid]


# SUBSET INDSAMLER TO THOSE W. >10 MIN LAG TIME ---------------------------


d$indsamler_sub[, lag_time := indtastningstidspunkt - shift(indtastningstidspunkt), by = hjertestarterid] %>%
  .[, lag_time_min := as.numeric(lag_time / 60)]
# d$indsamler_sub <- d$indsamler_sub[lag_time_min > 10]
d$indsamler_sub %>% 
  .[, lag_mean := mean(as.numeric(lag_time), na.rm = TRUE) / 3600, by = hjertestarterid] %>%
  .[, lag_median := median(as.numeric(lag_time), na.rm = TRUE) / 3600, by = hjertestarterid]

# Lag time for the first 5 signups
d$indsamlerny[reg_order <= 5, lag_time_first5 := indtastningstidspunkt - shift(indtastningstidspunkt), by = hjertestarterid]
d$indsamlerny[, lag_mean_first5 := mean(as.numeric(lag_time_first5), na.rm = TRUE)/3600, by = hjertestarterid]

# Lag time between oprettet of HS group and first indsammler signup (in hours)
d$indsamlerny[, lag_HS_inds := as.numeric(indtastningstidspunkt - oprettet) / 60]


# Variable describing the number of days before d.day that each indsamler signed
# up. The higher the number, the earlier they signed up

d$indsamlerny[, indtastings_mean := mean(indtastnings_days_before), by = "hjertestarterid"]



# TIME TO FIRST 5 COLLECTORS ----------------------------------------------
hs_cut <- d$hjertestarter[oprettet_date < gl$cut.day]
inds_cut <- d$indsamlerny[indtastningstidspunkt < gl$cut.day, .(hjertestarterid, gender, indtastningstidspunkt, age)]
setorder(inds_cut, hjertestarterid, indtastningstidspunkt)
inds_cut[, seq := seq.int(.N), by = hjertestarterid]
inds_cut[, lag_5 := indtastningstidspunkt - shift(indtastningstidspunkt, n = 4L), by = hjertestarterid]
inds_cut[, lag_5_in_days := as.numeric(lag_5) / 86400]
# Merge only those with seq number 5
inx <- inds_cut[, seq == 5]






# JOIN CALCULATED DATA TO GROUP-LEVEL DATASET ----------------------------------------
var_to_join <- c("lag_mean","lag_median", "lag_HS_inds","indtastings_mean", "hjertestarterid")
tmp <- unique(d$indsamlerny[, ..var_to_join], by = "hjertestarterid")
d$hjertestarter <- merge(d$hjertestarter, tmp, by = "hjertestarterid", all.x = TRUE)

tmp <- inds_cut[inx, .(hjertestarterid, lag_5_in_days)]
d$hjertestarter <- merge(d$hjertestarter, tmp, by = "hjertestarterid", all.x = TRUE)



# Make sure the groups we remove had 0 indsamler and 0 money
stopifnot(all(d$hjertestarter[grep("Nej", aktiv), belob_indsamlet ] == 0))
stopifnot(all(d$hjertestarter[grep("Nej", aktiv), is.na(lag_mean) ] == 1))
# d$hjertestarter[!grep("Nej", aktiv)]
# d$hjertestarter[grep("Ja", aktiv)][is.na(lag_mean)]




# CALCULATIONS ON SUBSET --------------------------------------------------


