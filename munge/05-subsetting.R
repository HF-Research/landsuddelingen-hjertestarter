## SUBSET ------------------------------------------------------------------

# Subset to only those entries occuring 3 weeks before d.day
d$indsamler_sub <- d$indsamlerny[indtastningstidspunkt < gl$cut.day]
d$hs_sub <- d$hjertestarter[oprettet_date < gl$cut.day & at_risk == 1]


# DATA FOR EXPORT ---------------------------------------------------------
keep_vars <- c("hjertestarterid","indsamlerid","ruteid","indtastningstidspunkt")
export_ls <- list()
export_ls$indsamlers <- d$indsamlerny[, ..keep_vars]

keep_vars <- c("hjertestarterid","antal_ruter_med_hjerteid")
export_ls$groups <- d$hjertestarter[, ..keep_vars]

saveRDS(export_ls, file = "data/landsindsamling_anonymous.rds")
