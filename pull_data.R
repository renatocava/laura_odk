library(tidyverse)
# # Enable the rOpenSci universe
# options(repos = c(
#   ropensci = "https://ropensci.r-universe.dev",
#   CRAN = "https://cloud.r-project.org"
# ))
# install.packages("ruODK")

# ODK Central's OData URL contains base URL, project ID, and form ID
# ODK Central credentials can live in .Renviron
# See vignette("setup") for setup and authentication options.
ruODK::ru_setup(
  svc = Sys.getenv("ODKC_SVC"),
  un = Sys.getenv("ODKC_UN"),
  pw = Sys.getenv("ODKC_PW"),
  tz = "America/Lima",
  verbose = TRUE
)

# ruODK contributors: see contributing guidelines for .Renviron variables

# Review settings
ruODK::ru_settings()

ruODK::project_list()
ruODK::submission_list()

fq_svc <- ruODK::odata_service_get()

fq_data <- ruODK::odata_submission_get(
  table = fq_svc$name[1],
  # local_dir = loc,
  wkt = TRUE
)

fq_data %>% 
  select(-c(meta_instance_id:odata_context)) %>%
  View()
# DT::datatable(fq_data)
