strat_buckets <- output_df %>% 
  select(-ID) %>% 
  group_by(coder, age_years, clinical) %>% 
  count() %>%
  ungroup %>% 
  mutate(bucket = row_number())

set.seed(12345)
input_interrater <- output_df %>% 
  left_join(strat_buckets, by = c("coder", "age_years", "clinical")) %>% 
  arrange(bucket) %>% 
  group_by(bucket) %>% 
  slice_sample(prop = .15, replace = FALSE) %>% 
  mutate(interrater = 1) %>% 
  ungroup() %>% 
  select(ID, interrater)
  

df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
df %>% replace_na(list(x = 0, y = "unknown"))

