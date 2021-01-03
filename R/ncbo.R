




download_ncit_owl <-
        function(url = "http://data.bioontology.org/ontologies/NCIT/submissions/100/download?apikey=8b5b7825-538d-40e0-9e9e-5ab9274a9aeb") {

                download.file(url = url,
                              destfile = "inst/ncit.owl")
        }



download_loinc_owl <-
        function(url = "http://data.bioontology.org/ontologies/LOINC/submissions/19/download?apikey=8b5b7825-538d-40e0-9e9e-5ab9274a9aeb") {

                download.file(url = url,
                              destfile = "inst/loinc.owl")
        }


download_icd10cm_owl <-
        function(url = "http://data.bioontology.org/ontologies/ICD10CM/submissions/18/download?apikey=8b5b7825-538d-40e0-9e9e-5ab9274a9aeb") {

                download.file(url = url,
                              destfile = "inst/icd10cm.owl")
        }
