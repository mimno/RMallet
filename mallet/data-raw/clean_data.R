# Create State of the Union
sotu <- read.table("data-raw/state_of_the_union_1790_2009.txt", sep = "\t", stringsAsFactors = FALSE)
sotu <- as_data_frame(sotu)
sotu$V1 <- as.numeric(unlist(lapply(stringr::str_split(sotu$V1, "-"), function(x) x[2])))
sotu <- sotu[, c(2,1,3)] 
names(sotu) <- c("year", "paragraph", "text")
sotu$paragraph <- as.integer(sotu$paragraph)

devtools::use_data(sotu, overwrite = TRUE)
