# Create State of the Union
sotu <- readLines("data-raw/state_of_the_union_1790_2009.txt")
sotu <- stringr::str_split(sotu, pattern = "\t")
sotu <- data.frame(V1 = unlist(lapply(sotu, function(x) x[1])),
                   V2 = unlist(lapply(sotu, function(x) x[2])),
                   V3 = unlist(lapply(sotu, function(x) x[3])))
sotu$V1 <- as.numeric(unlist(lapply(stringr::str_split(sotu$V1, "-"), function(x) x[2])))
sotu <- sotu[, c(2,1,3)]
names(sotu) <- c("year", "paragraph", "text")
sotu$paragraph <- as.integer(sotu$paragraph)

sotu <- sotu[sotu$year >= 1946 & sotu$year <= 2000,]
#sotu$text <- stringi::stri_enc_toascii(sotu$text)
#idx <- which(!stringi::stri_enc_isascii(sotu$text))
#sotu$text[idx]
usethis::use_data(sotu, overwrite = TRUE)

