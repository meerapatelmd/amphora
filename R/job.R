library(tidyverse)
library(chariot)

conn <- connectAthena()
output <- read_csv("first_standard_library.csv")

for (i in 1:nrow(output)) {
        secretary::typewrite_progress(i,
                                      total = nrow(output))
        subClass <- output$subClass[i]
        concept_obj <- chariot::get_concept(concept_id = output$concept_id[i])
        add_omop_concept_to_library(conn = conn,
                                    standard_library_schema = "standard_library",
                                    concept_obj = concept_obj,
                                    subclass = subClass,
                                    render_only = FALSE)
}

dcAthena(conn = conn)
