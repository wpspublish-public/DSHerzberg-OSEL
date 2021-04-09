source("~/Desktop/R/GENERAL/CODE/stratified-function.R")

set.seed(12345)
temp_typical <- stratified(
  output_df,
  group = c("coder", "age_years", "clinical"),
  size = .1,
  select = list(clinical = 1)
)

set.seed(12345)
temp_clinical <- stratified(
  output_df,
  group = c("coder", "age_years", "clinical"),
  size = .22,
  select = list(clinical = 2)
)

temp1 <- bind_rows(
  temp_typical,
  temp_clinical
) %>% 
  arrange(coder, age_years, clinical)

table_output_df_coder <- as_tibble(
  table(output_df$coder), 
  .name_repair = "minimal") %>% 
  set_names("coder", "n")

table_output_df_age_years <- as_tibble(
  table(output_df$age_years), 
  .name_repair = "minimal") %>% 
  set_names("age_year", "n")

table_output_df_clinical <- as_tibble(
  table(output_df$clinical), 
  .name_repair = "minimal") %>% 
  set_names("clinical", "n")

table_temp1_coder <- as_tibble(
  table(temp1$coder), 
  .name_repair = "minimal") %>% 
  set_names("coder", "n")

table_temp1_age_years <- as_tibble(
  table(temp1$age_years), 
  .name_repair = "minimal") %>% 
  set_names("age_year", "n")

table_temp1_clinical <- as_tibble(
  table(temp1$clinical), 
  .name_repair = "minimal") %>% 
  set_names("clinical", "n")

