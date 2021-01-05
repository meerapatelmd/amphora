library(readr)
library(broca)
antineoplastics_annotation <- readr::read_csv("/Users/meerapatel/GitHub/packages/amphora/data-raw/antineoplastics.csv")
MOMOP_TABLES <- broca::read_full_excel("/Users/meerapatel/GitHub/packages/amphora/data-raw/MOMOP_TABLES.xlsx")
UPTODATE_TABLES <- broca::read_full_excel("/Users/meerapatel/GitHub/packages/amphora/data-raw/UPTODATE_TABLES.xlsx")
usethis::use_data(
	antineoplastics_annotation,
	MOMOP_TABLES,
	UPTODATE_TABLES,
overwrite = TRUE
)
