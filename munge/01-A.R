
# LOAD DATA ---------------------------------------------------------------
dat.path <- list.files("data/", full.names = TRUE, pattern = "*.csv")
dat <- map(dat.path, fread, encoding = "UTF-8")
dat_names <-
  c(
    "hjertestarter_old",
    "hjertestarter",
    "indsamler_log",
    "indsamlerny",
    "koordinator",
    "omraader",
    "ruter"
  )
names(dat) <- dat_names


# Clean the column names - i.e. all lowercase, and underscore seperated
d <- map(dat, clean_names)


d$hjertestarter_old[, hvorfor := NULL]
d$hjertestarter_old[, placering := NULL]
d$hjertestarter_old[, billede := NULL]
d$hjertestarter_old[, guid := NULL]
d$hjertestarter[, c("v1", "v2", "intern_kommentar", "placering", "kommentar") := NULL]
d$ruter <-
  d$ruter[, .(
    ruteid,
    omraade,
    omraadeid,
    postnummer,
    byen,
    rutenr,
    husstande,
    markerpos,
    optaget,
    optagetden,
    indsamlerid,
    maerkelig,
    oprettet,
    rating_dkk,
    kommunekode,
    kommunenavn,
    bankbeloeb,
    afleveretbeloeb,
    ikkemoedtop,
    hjerteid1,
    hjerteid2,
    antalpersonerpaaruten
  )]
d$indsamlerny <-
  d$indsamlerny[, .(
    indsamlerid,
    hjertestarterid,
    ruteid,
    indtastningstidspunkt,
    belindsamlet,
    beloebsendt,
    beloebsendtdato,
    fornavn,
    efternavn,
    postnummer,
    byen,
    mobil_telefon,
    email,
    foedselsdato
    )]


# Get dates/times in correct format
d$indsamlerny[, indtastningstidspunkt := as_datetime(indtastningstidspunkt)]
d$indsamlerny[, indtastings_date := lubridate::date(indtastningstidspunkt)]
d$indsamlerny[, indtastnings_days_before := as.numeric(indtastings_date - gl$d.day)]

d$hjertestarter[, oprettet := lubridate::dmy_hms(oprettet)]
d$hjertestarter[, oprettet_date := lubridate::date(oprettet)]
d$hjertestarter[, oprettet_before_dday := as.numeric(gl$d.day - oprettet_date)]
d$indsamler_log[, date_time := as_datetime(dato)]
d$indsamler_log[, date := lubridate::date(date_time)]

d$ruter[, optagetden := lubridate::ymd_hms(optagetden)]


# Trim extra spaces before or after character strings
d$indsamlerny[,  fornavn := tolower(fornavn)]
d$indsamlerny[,  efternavn := trimws(efternavn)]
d$indsamlerny[,  fornavn := trimws(fornavn, whitespace = "[\\h\\v]")]
d$hjertestarter[,  gruppenavn := trimws(gruppenavn)]

# Remove obviously trash data. Those with dates just after d.day should be
# included, they were probably registered on paper, and only later entered into
# the system.
d$indsamlerny <- d$indsamlerny[indtastings_date < gl$d.day + 60]
d$indsamler_log <- d$indsamler_log[date < gl$d.day + 60]


# Remove test entries in hjertestarter
d$hjertestarter <- d$hjertestarter[!grep("^test", gruppenavn)]


# Convert numbers to numeric
d$hjertestarter[, belob_indsamlet := as.numeric(sub(",", ".", belob_indsamlet))]

# Part of HS group or not
d$indsamlerny[, hs_group := 0]
d$indsamlerny[hjertestarterid != 0, hs_group := 1]

# Order dataset
setorder(d$indsamlerny, hjertestarterid, indtastningstidspunkt)


# OUTCOME VARIABLES -------------------------------------------------------
d$hjertestarter[, out_15ruter := 0L]
d$hjertestarter[antal_ruter_med_hjerteid >= 15, out_15ruter := 1L]

d$hjertestarter[, out_10_14 := 0L]
d$hjertestarter[between(antal_ruter_med_hjerteid, 10, 14), out_10_14 := 1L]



d$indsamlerny[, no_show := 0]
d$indsamlerny[beloebsendt == 0, no_show := 1]
d$indsamlerny[, age := year(gl$d.day) - foedselsdato]


# SELECT REAL HJERTESTARTER GROUPS ----------------------------------------

d$hjertestarter[, at_risk := 0]
d$hjertestarter[grep("Ja", aktiv), at_risk := 1]




# PROVIDE LINKING IDS -----------------------------------------------------
# It makes it easier to see whats happening if hs, indsamler, and route IDs are put on each dataset applicable
d$indsamler_log <- merge(d$indsamler_log, d$indsamlerny[, .(indsamlerid, hjertestarterid)], by = "indsamlerid", all.x = TRUE)





# INVESTIGATE HOW ROUTES ARE COUNTED  -------------------------------------

# How to calculate who showed up or not
x <- merge(d$ruter[hjerteid1 != "" | hjerteid2 !="" & optaget == 1], d$indsamlerny[hjertestarterid >1], by = "indsamlerid")
x[no_show==0]

x <- unique(d$indsamler_log[typen == "taget"], by = "ruteid")[, .N, by = hjertestarterid]
z1 <- unique(d$indsamlerny, by = "ruteid")[, .N, by = hjertestarterid]
setnames(z1, "N", "routes_inds")
setnames(x, "N", "routes_log")
x1 <- merge(x, z1)

x2 <- merge(x1, d$hjertestarter[, .(hjertestarterid, antal_ruter, antal_ruter_med_hjerteid)], by = "hjertestarterid")
x2[, diff := antal_ruter - routes_log]
x2[, diff2 := antal_ruter - routes_inds]
x2[, diff3 := routes_log - routes_inds]

x <- merge(d$indsamler_log, d$indsamlerny[, .(indsamlerid, indtastningstidspunkt)])
x <- x[, diff := date_time - indtastningstidspunkt][typen == "taget"]
unique(x[order(diff)], by = "indsamlerid")[order(diff)]
x[indsamlerid == 25638]


z <- merge(d$ruter[, .(ruteid, indsamlerid, optagetden)], d$indsamler_log[typen == "taget"], by = c("ruteid", "indsamlerid"))
z[, diff:= optagetden - date_time]
get_dupes(z, ruteid)
get_dupes(d$indsamler_log[typen == "taget"], indsamlerid, ruteid)

# Indsamler with multiple routes in multiple cities_
get_dupes(d$ruter[indsamlerid !=0, .N, by = .(byen, indsamlerid)], indsamlerid)
