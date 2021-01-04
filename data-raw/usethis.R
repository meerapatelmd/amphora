library(readr)
antineoplastics <- readr::read_csv("/Users/meerapatel/GitHub/packages/amphora/data-raw/antineoplastics.csv")
usethis::use_data(
	antineoplastics,
overwrite = TRUE
)
