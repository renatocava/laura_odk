library(httr)
library(jsonlite)
library(tidyverse)

res <- GET(
  url = paste0(Sys.getenv("ODK_BASE_URL"), "v1/projects/", Sys.getenv("ODK_PROJECT_ID"), "/datasets/", Sys.getenv("ODK_ENTITY_NAME"), ".svc/Entities"),
  authenticate(Sys.getenv("ODK_USERNAME"), Sys.getenv("ODK_PASSWORD"), type = "basic"),
  accept("application/json")
)
data_json <- content(res, as = "text", encoding = "UTF-8")
data_list <- fromJSON(data_json)
df_part <- as_tibble(data_list$value)

# Obtener los submissions de preregistro (formularios llenados) ----
res_pre_reg <- GET(
  url = paste0(Sys.getenv("ODK_BASE_URL"), "v1/projects/", Sys.getenv("ODK_PROJECT_ID"), "/forms/", "Laura2-piloto-encuesta-preregistro", "/submissions.csv"),
  authenticate(Sys.getenv("ODK_USERNAME"), Sys.getenv("ODK_PASSWORD"), type = "basic"),
  accept("text/csv")
)
# Leer el CSV en un dataframe
df_pre_r <- read_csv(content(res_pre_reg, as = "raw"))

# Obtener los submissions de cosentimiento (formularios llenados) ----
res_ci <- GET(
  url = paste0(Sys.getenv("ODK_BASE_URL"), "v1/projects/", Sys.getenv("ODK_PROJECT_ID"), "/forms/", "Laura2-piloto-encuesta-ic", "/submissions.csv"),
  authenticate(Sys.getenv("ODK_USERNAME"), Sys.getenv("ODK_PASSWORD"), type = "basic"),
  accept("text/csv")
)
# Leer el CSV en un dataframe
df_ci <- read_csv(content(res_ci, as = "raw"))

# Obtener los submissions de encuenta nacional parte 1 (formularios llenados) -----
res_p1 <- GET(
  url = paste0(Sys.getenv("ODK_BASE_URL"), "v1/projects/", Sys.getenv("ODK_PROJECT_ID"), "/forms/", "Laura2-piloto-encuesta-p1", "/submissions.csv"),
  authenticate(Sys.getenv("ODK_USERNAME"), Sys.getenv("ODK_PASSWORD"), type = "basic"),
  accept("text/csv")
)
# Leer el CSV en un dataframe
df_p1 <- read_csv(content(res_p1, as = "raw"))

# Obtener los submissions de encuesta nacional parte 2 (formularios llenados) ----
res_p2 <- GET(
  url = paste0(Sys.getenv("ODK_BASE_URL"), "v1/projects/", Sys.getenv("ODK_PROJECT_ID"), "/forms/", "Laura2-piloto-encuesta-p2", "/submissions.csv"),
  authenticate(Sys.getenv("ODK_USERNAME"), Sys.getenv("ODK_PASSWORD"), type = "basic"),
  accept("text/csv")
)
# Leer el CSV en un dataframe
df_p2 <- read_csv(content(res_p2, as = "raw"))

# Obtener los submissions de encuesta nacional parte 3 (formularios llenados) ----
res_p3 <- GET(
  url = paste0(Sys.getenv("ODK_BASE_URL"), "v1/projects/", Sys.getenv("ODK_PROJECT_ID"), "/forms/", "Laura2-piloto-encuesta-p3", "/submissions.csv"),
  authenticate(Sys.getenv("ODK_USERNAME"), Sys.getenv("ODK_PASSWORD"), type = "basic"),
  accept("text/csv")
)
# Leer el CSV en un dataframe
df_p3 <- read_csv(content(res_p3, as = "raw"))

# Obtener los submissions de encuesta nacional (formularios llenos) ----
res_en <- GET(
  url = paste0(Sys.getenv("ODK_BASE_URL"), "v1/projects/", Sys.getenv("ODK_PROJECT_ID"), "/forms/", "Laura2-piloto-encuesta", "/submissions.csv"),
  authenticate(Sys.getenv("ODK_USERNAME"), Sys.getenv("ODK_PASSWORD"), type = "basic"),
  accept("text/csv")
)
# Leer el CSV en un dataframe
df_en <- read_csv(content(res_en, as = "raw"))

# Obtener los submissions de seguimiento fase 2 (formularios llenados) ----
res_seg <- GET(
  url = paste0(Sys.getenv("ODK_BASE_URL"), "v1/projects/", Sys.getenv("ODK_PROJECT_ID"), "/forms/", "Laura2-fase2-encuesta-seguimiento-Lima-TC", "/submissions.csv"),
  authenticate(Sys.getenv("ODK_USERNAME"), Sys.getenv("ODK_PASSWORD"), type = "basic"),
  accept("text/csv")
)
# Leer el CSV en un dataframe
df_seg <- read_csv(content(res_seg, as = "raw"))
# Exportar en xlsx
writexl::write_xlsx(x = df_seg %>% select(SubmissionDate, `preamble-entity_short_id`, `preamble-visit_nr`, `preamble-visit_date`), path = paste("output/df_seguimiento_", lubridate::today(), ".xlsx", sep = ""))

# Unir encuesta nacional ----
df_p1 %>% 
  arrange(desc(SubmissionDate)) %>% 
  distinct(`preamble-entity_name`, .keep_all = T) %>% 
  mutate(
    SubmissionDate = lubridate::ymd_hms(SubmissionDate, tz = "UTC"),
    SubmissionDate = lubridate::with_tz(SubmissionDate, tzone = "America/Lima")
  ) %>% 
  select(
    `preamble-part_id_2`,
    # `preamble-entity_name`,
    `EN P1`=SubmissionDate
    # `preamble-complete_p1`,
    
  ) %>% 
  # View()
  full_join(
    df_p2 %>% 
      mutate(
        SubmissionDate = lubridate::ymd_hms(SubmissionDate, tz = "UTC"),
        SubmissionDate = lubridate::with_tz(SubmissionDate, tzone = "America/Lima")
      ) %>% 
      arrange(desc(SubmissionDate)) %>% 
      distinct(`preamble-entity_name`, .keep_all = T) %>% 
      select(
        `preamble-part_id_3`,
        # `preamble-entity_name`,
        # `preamble-complete_p2`,
        `EN P2`=SubmissionDate
      ),
    by = c("preamble-part_id_2"="preamble-part_id_3")
  ) %>% 
  # count(`preamble-part_id_2`) %>% 
  # View()
  full_join(
    df_p3 %>% 
      arrange(desc(SubmissionDate)) %>% 
      distinct(`preamble-entity_name`, .keep_all = T) %>% 
      mutate(
        SubmissionDate = lubridate::ymd_hms(SubmissionDate, tz = "UTC"),
        SubmissionDate = lubridate::with_tz(SubmissionDate, tzone = "America/Lima")
      ) %>% 
      select(
        `preamble-part_id_4`,
        # `preamble-entity_name`,
        # `preamble-complete_p3`,
        `EN P3`=SubmissionDate
      ),
    by = c("preamble-part_id_2"="preamble-part_id_4")
  ) %>% 
  full_join(
    df_en %>% 
      select(`preamble-entity_details-long_id`, EN = SubmissionDate),
    by = c("preamble-part_id_2"="preamble-entity_details-long_id")
  ) %>% 
  arrange(`EN P1`, `EN P2`, EN) %>% 
  View()
  # writexl::write_xlsx(path = paste("output/df_en_", lubridate::today(), ".xlsx", sep = ""))



## Integrar todo ----
df_part %>% 
  select(
    phone:complete_p3
  ) %>%
  # add_count(phone) %>%
  # filter(n>1) %>%
  View()

df_pre_r %>% 
  filter(`participantes-exists_id`=="no") %>% 
  add_count(`participantes-participante_id`) %>% 
  filter(n>1) %>% 
  # select(`participantes-participante_id`, `participantes-correo`) %>%
  # distinct(`participantes-participante_id`, .keep_all = T) %>%
  View()

df_ci %>% 
  select(
    `preamble-part_id`,
    `preamble-entity_name`,
    `consent-Q0_accept_consent`
  ) %>% 
  # View()
  # add_count(`preamble-part_id`) %>% 
  # filter(n==1) %>% 
  arrange(`preamble-part_id`, `preamble-entity_name`) %>%
  distinct(`preamble-part_id`, .keep_all = T) %>%
  View()

df_p1 %>% 
  # count(`preamble-part_id_2`) %>% 
  # View()
  # add_count(`preamble-entity_name`) %>% 
  # filter(n>1) %>%
  arrange(desc(SubmissionDate)) %>% 
  distinct(`preamble-entity_name`, .keep_all = T) %>% 
  # View()
  select(
    `preamble-part_id_2`,
    `preamble-entity_name`,
    `preamble-complete_p1`,
  ) %>% 
  View()

df_p2 %>%
  # count(`preamble-part_id_3`) %>%
  # View()
  # count(`preamble-entity_name`) %>%
  # View()
  # filter(n>1) %>%
  arrange(desc(SubmissionDate)) %>% 
  distinct(`preamble-entity_name`, .keep_all = T) %>% 
  # View()
  select(
    `preamble-part_id_2`,
    `preamble-entity_name`,
    `preamble-complete_p1`,
  ) %>% 
  View()

df_p3 %>% 
  # add_count(`preamble-part_id_4`) %>%
  # filter(n>1) %>%
  # View()
  # count(`preamble-entity_name`) %>%
  # View()
  # filter(n>1) %>%
  arrange(desc(SubmissionDate)) %>% 
  distinct(`preamble-entity_name`, .keep_all = T) %>% 
  # View()
  select(
    `preamble-part_id_2`,
    `preamble-entity_name`,
    `preamble-complete_p1`,
  ) %>% 
  View()

## ----
  
df_part %>% 
  mutate(
    createdAt = lubridate::ymd_hms(`__system`$createdAt, tz = "UTC"),
    createdAt = lubridate::with_tz(createdAt, tzone = "America/Lima")
  ) %>% 
  # View()
  select(
    createdAt,
    phone:complete_p3
  ) %>%
  # View()
  # distinct(phone, .keep_all = T) %>%
  # add_count(phone) %>% 
  # filter(n>1) %>% 
  # View()
  left_join(
    df_pre_r %>% 
      mutate(
        SubmissionDate = lubridate::ymd_hms(SubmissionDate, tz = "UTC"),
        SubmissionDate = lubridate::with_tz(SubmissionDate, tzone = "America/Lima")
      ) %>% 
      # count(`participantes-participante_id_counts`, `participantes-exists_id`)
      filter(`participantes-exists_id`=="no") %>% 
      arrange(desc(SubmissionDate)) %>% 
      select(Preregistro=SubmissionDate, `participantes-participante_id`) %>%
      distinct(`participantes-participante_id`, .keep_all = T),
    by = c("long_id"="participantes-participante_id")
  ) %>%
  # View()
  # filter(
  #   consent == "" # 25 participantes que enviaron preregistro sin registro de consentieminto en lista de entidades
  # ) %>% 
  # View()
  left_join(
    df_ci %>% 
      mutate(
        SubmissionDate = lubridate::ymd_hms(SubmissionDate, tz = "UTC"),
        SubmissionDate = lubridate::with_tz(SubmissionDate, tzone = "America/Lima")
      ) %>% 
      arrange(`preamble-part_id`, `preamble-entity_name`) %>%
      distinct(`preamble-part_id`, .keep_all = T) %>% 
      select(
        `preamble-part_id`,
        # `preamble-entity_name`,
        `consent-Q0_accept_consent`,
        Consentimiento=SubmissionDate
      ),
    by = c("long_id" = "preamble-part_id")
  ) %>% 
  # View()
  # select(
  #   phone,
  #   long_id,
  #   consent:complete_p3,
  #   `consent-Q0_accept_consent`
  # ) %>% 
  full_join(
    df_p1 %>% 
      arrange(desc(SubmissionDate)) %>% 
      distinct(`preamble-entity_name`, .keep_all = T) %>% 
      mutate(
        SubmissionDate = lubridate::ymd_hms(SubmissionDate, tz = "UTC"),
        SubmissionDate = lubridate::with_tz(SubmissionDate, tzone = "America/Lima")
      ) %>% 
      select(
        `preamble-part_id_2`,
        # `preamble-entity_name`,
        `EN P1`=SubmissionDate
        # `preamble-complete_p1`,
        
      ),
    by = c("long_id"="preamble-part_id_2")
  ) %>% 
  # View()
  full_join(
    df_p2 %>% 
      mutate(
        SubmissionDate = lubridate::ymd_hms(SubmissionDate, tz = "UTC"),
        SubmissionDate = lubridate::with_tz(SubmissionDate, tzone = "America/Lima")
      ) %>% 
      select(
        `preamble-part_id_3`,
        # `preamble-entity_name`,
        # `preamble-complete_p2`,
        `EN P2`=SubmissionDate
      ),
    by = c("long_id"="preamble-part_id_3")
  ) %>% 
  full_join(
    df_p3 %>% 
      arrange(desc(SubmissionDate)) %>% 
      distinct(`preamble-entity_name`, .keep_all = T) %>% 
      mutate(
        SubmissionDate = lubridate::ymd_hms(SubmissionDate, tz = "UTC"),
        SubmissionDate = lubridate::with_tz(SubmissionDate, tzone = "America/Lima")
      ) %>% 
      select(
        `preamble-part_id_4`,
        # `preamble-entity_name`,
        # `preamble-complete_p3`,
        `EN P3`=SubmissionDate
      ),
    by = c("long_id"="preamble-part_id_4")
  ) %>% 
  arrange(Preregistro) %>% 
  # View()
  mutate(
    across(c(consent:complete_p3), ~case_when(.==""~NA_character_, T~.))
  ) %>% 
  mutate(
    across(everything(), ~as.character(.))
  ) %>% 
  View()
  # left_join(
  #   googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1keq67qOvyZU_Ow9qwuGjLpktX9DOqBJ1cjH-61qz7GA/edit?gid=548388021#gid=548388021", sheet = "2025-06-24") %>%
  #     select(long_id, `Observaciones TCs`, Observaciones, Acciones),
  #   by = "long_id"
  # ) %>%
  # # View()
  # googlesheets4::write_sheet(ss = "https://docs.google.com/spreadsheets/d/1keq67qOvyZU_Ow9qwuGjLpktX9DOqBJ1cjH-61qz7GA/edit?gid=0#gid=0", sheet = paste(lubridate::today()))
