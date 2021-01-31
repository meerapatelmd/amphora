library(tidyverse)
library(chariot)

conn <- connectAthena()
output <- read_csv("first_standard_library.csv")

for (i in 4387:nrow(output)) {
        secretary::typewrite_progress(i,
                                      total = nrow(output))
        subClass <- output$subClass[i]

        if (!(is.na(output$concept_id[i]))) {
        concept_obj <- chariot::get_concept(concept_id = output$concept_id[i])
       sl_add_omop_concept(conn = conn,
                                    standard_library_schema = "standard_library",
                                    concept_obj = concept_obj,
                                    subclass = subClass,
                                    render_only = FALSE)

        } else {
                sl_add_new_concept(conn = conn,
                                   concept_name = output$concept_name[i],
                                   subclass = subClass,
                                   standard_library_schema = "standard_library")
        }

        Sys.sleep(2)
}

dcAthena(conn = conn)
