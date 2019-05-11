labs.icustay = demo_1ststay %>% 
  inner_join(labs.tidy %>% 
               filter(ITEMID==50912 | ITEMID==51081 | ITEMID==51011 | 
                        ITEMID==50862 | ITEMID==50963 | ITEMID==51006), by = c("hadm_id" = "HADM_ID")) %>%
  ungroup() %>% 
  select(icustay_id, ITEMID, VALUENUM, CHARTTIME, icu_intime) %>%
  mutate(lab_time = as.numeric(difftime(CHARTTIME, icu_intime, units = "hours"))) %>% 
  filter(lab_time > 0) %>% 
  group_by(icustay_id, ITEMID) %>% 
  dplyr::slice(1) %>% 
  spread(key = ITEMID, value = VALUENUM) %>% 
  mutate(creatinine = sum(`50912`, `51081`, na.rm = T),
         alb = sum(`51011`, `50862`, na.rm = T)) %>% 
  rename(bnp = `50963`, bun = `51006`) %>% 
  select(-`50912`, -`51081`, -`51011`, -`50862`) %>% 
  ungroup() %>% 
  mutate(creatinine = if_else(is.na(creatinine), mean(creatinine, na.rm = T), creatinine),
         alb = if_else(is.na(alb), mean(alb, na.rm = T), alb),
         bnp = if_else(is.na(bnp), mean(bnp, na.rm = T), bnp),
         bun = if_else(is.na(bun), mean(bun, na.rm = T), bun))




t2.model = t2.roll.min_per_day %>% mutate(flag.diur_grp = if_else(cum_rate_all >= -3000, "Low", "High")) %>% 
  select(icustay_id, flag.diur_grp) %>% 
  inner_join(labs.icustay) %>% 
  inner_join(demo_1ststay %>% select(icustay_id, gender, icu_first_careunit, icu_los)) %>% 
  inner_join(oasis.tidy %>% select(icustay_id, age, OASIS)) %>% 
  ungroup() %>% 
  select(icu_los, flag.diur_grp, creatinine, alb, bnp, bun, gender, icu_first_careunit, age, OASIS)
  