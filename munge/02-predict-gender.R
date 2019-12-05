# # Predict gender of name.
# # DO NOT RE-RUN SCRIPT WITH EVERY LOAD. This uses an API which allows only 1000 names/day to be checked
# 
# api_size <- 990
# first_name <- list()
# tmp <- d$indsamlerny[hjertestarterid!=0, .(indsamlerid, fornavn)]
# 
# tmp[, fornavn := sub(" - ", "-", fornavn)]
# tmp[, fornavn := sub("  ", " ", fornavn)]
# tmp[, fornavn := sub(" .*", "", fornavn)]
# tmp[, fornavn := sub("hjerte", "", fornavn)]
# 
# # We need a matching column in the indsamlerny dataset
# d$indsamlerny[, name_gender := fornavn]
# d$indsamlerny[, name_gender := sub(" - ", "-", name_gender)]
# d$indsamlerny[, name_gender := sub("  ", " ", name_gender)]
# d$indsamlerny[, name_gender := sub(" .*", "", name_gender)]
# d$indsamlerny[, name_gender := sub("hjerte", "", name_gender)]
# 
# # # Format text and remove duplicate names - this in only done on the tmp dataset
# tmp <- tmp[!grep(" og ", fornavn), ]
# tmp <- tmp[!grep("&", fornavn),]
# tmp <- tmp[!grep("@|/", fornavn),]
# tmp <- unique(tmp, by = "fornavn")
#  
# # Prepare obj for sending to API
# obs <- nrow(tmp)
# n.groups <- ceiling(obs/api_size)
# group.id <- rep(1:n.groups, each=api_size)
# tmp[ , group.id := group.id[1:obs]]
# 
#  
# tmp.list <- split(tmp, f = group.id)
# day1 <- findGivenNames(tmp.list$`1`$fornavn, country = "DK", textPrepare = FALSE)
# out.names <- day1
# # day2 <- findGivenNames(tmp.list$`2`$fornavn, country = "DK", textPrepare = FALSE)
# # out.names <- rbind(day1, day2)
# 
# out <- merge(tmp, out.names, by.x = "fornavn", by.y = "name")
# 
# z <- merge(d$indsamlerny, out, by.x = "name_gender", by.y = "fornavn", all.x = TRUE) 
# no_confi <- rbind(
#   out.names[count <5],
#   out.names[(probability <.85) & count <25],
#   out.names[(probability <.95) & count <10]
# ) %>% unique(by = "name")
# 
# x <- merge(dat$indsamlerny, out.names[, .(name, gender)], by.x = "name_gender", by.y= "name", all.x = TRUE)
# x[is.na(gender) & hjertestarterid !=0]
# 
# x[grep(" og |&|@|/", fornavn), gender := NA]
# x[!is.na(gender)]
