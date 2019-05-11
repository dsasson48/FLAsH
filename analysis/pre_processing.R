project <- "nyu-datathon"

# Example query - select copies of files with content containing "TODO"
sql_t2 <- "SELECT * FROM [nyu-datathon.Team_9.t2_HF]"
# Execute the query and store the result
t2_hf <- query_exec(sql_t2, project = project, useLegacySql = FALSE, max_pages = 400)

sql_dm <- "SELECT * FROM [nyu-datathon.Team_9.NEW_Demographics_EWC]"
# Execute the query and store the result
demo_hf <- query_exec(sql_dm, project = project, useLegacySql = FALSE)

sql_med_cv <- "SELECT * FROM [nyu-datathon.Team_9.NEW_meds_cv_wmy]"
# Execute the query and store the result
meds_cv.raw <- query_exec(sql_med_cv, project = project, useLegacySql = FALSE, max_pages = 60)

sql_med_mv <- "SELECT * FROM [nyu-datathon.Team_9.NEW_meds_mv_wmy]"
# Execute the query and store the result
meds_mv.raw <- query_exec(sql_med_mv, project = project, useLegacySql = FALSE, max_pages = 20)

sql_lab <- "SELECT * FROM [nyu-datathon.Team_9.NEW_GetLabs_EWC]"
# Execute the query and store the result
labs.raw <- query_exec(sql_lab, project = project, useLegacySql = FALSE, max_pages = 50)

sql_oasis <- "SELECT * FROM [nyu-datathon.Team_9.NEW_Oasis_EWC]"
# Execute the query and store the result
oasis.raw <- query_exec(sql_oasis, project = project, useLegacySql = FALSE, max_pages = 2)

save.image(file = "hf_raw.RData")

demo_1ststay = demo_hf %>% #filter(hospital_expire_flag == 0) %>% 
  filter(icu_los >= 0.5) %>% 
  group_by(hadm_id) %>% arrange(icu_intime) %>% 
  dplyr::slice(1)
oasis.tidy = oasis.raw %>% filter(age > 0) %>% mutate(age = ifelse(age>90, 90, age)) %>% 
  semi_join(demo_1ststay)
## exclude patients with first creatinine value of hadm_id > 3
exclude_px = labs.raw %>%
  group_by(HADM_ID) %>% 
  filter(ITEMID==50912 | ITEMID==51081) %>% 
  arrange(CHARTTIME) %>% 
  dplyr::slice(1) %>% 
  filter(VALUENUM > 3)
labs.tidy = labs.raw %>% 
  semi_join(demo_1ststay, by = c("SUBJECT_ID" = "subject_id", "HADM_ID" = "hadm_id")) %>% 
  filter(!HADM_ID %in% exclude_px$HADM_ID)
meds_cv.tidy = meds_cv.raw %>% 
  semi_join(demo_1ststay, by = c("SUBJECT_ID" = "subject_id", "HADM_ID" = "hadm_id", "ICUSTAY_ID" = "icustay_id"))
meds_mv.tidy = meds_mv.raw %>% 
  semi_join(demo_1ststay, by = c("SUBJECT_ID" = "subject_id", "HADM_ID" = "hadm_id", "ICUSTAY_ID" = "icustay_id"))
t2.tidy = t2_hf %>% 
  semi_join(demo_1ststay, by = "icustay_id") %>% 
  inner_join(demo_1ststay %>% select(icustay_id, icu_intime)) 
a = t2.tidy %>% filter(icustay_id==258343) %>% arrange(endtime)

save.image(file = "hf_tidy.RData")
save(demo_1ststay, labs.tidy, meds_cv.tidy, meds_mv.tidy, oasis.tidy, file = "hf_just_tidy.RData")

## age distribution
oasis.raw %>% 
  distinct(subject_id, age) %>% 
  mutate(age = ifelse(age>90, 90, age)) %>% 
  filter(age > 0) %>% 
  ggplot(aes(x = age))+geom_histogram(bins = 74)
age.summary = oasis.raw %>% 
  distinct(subject_id, age) %>% 
  mutate(age = ifelse(age>90, 90, age)) %>% 
  filter(age > 0) %>% 
  group_by(age) %>% 
  summarise(count = n()) %>% 
  arrange(desc(age))

## icu_los distribution
demo_1ststay %>% 
  semi_join(oasis.tidy, by = 'subject_id') %>% 
  filter(icu_los <= 25) %>% 
  ggplot(aes(x = icu_los))+geom_histogram()

## lab
labs.tidy %>% filter(ITEMID==50912 | ITEMID==51081) %>% ggplot(aes(x=VALUENUM))+geom_histogram()

## 
