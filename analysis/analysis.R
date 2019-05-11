## fluid balance of rolling window per day
t2.roll = t2.tidy %>%
  arrange(icustay_id, starttime) %>% 
  mutate(stay_time = difftime(endtime, icu_intime, units = "hours"),
         rate_all = rate_blood_product + rate_colloid + rate_crystalloid + rate_dialysis_input -
           rate_dialysis_output - rate_drain + rate_irrigant + rate_oral + rate_general_intake -
           rate_general_output + rate_nutrition - rate_uo) %>% 
  filter(stay_time <= 120) %>% 
  mutate(time_bin = case_when(
    stay_time <= 24 ~ "24",
    stay_time <= 48 ~ "48",
    stay_time <= 72 ~ "72",
    T ~ ">72"
  )) %>% 
  group_by(icustay_id, time_bin) %>% mutate(cum_rate_all = cumsum(rate_all))
  
t2.roll.min = t2.roll %>% group_by(icustay_id, time_bin) %>% 
  dplyr::slice(which.min(cum_rate_all))
t2.roll.eod = t2.roll %>% group_by(icustay_id, time_bin) %>% 
  dplyr::slice(n())
t2.roll.multdays = t2.roll.min %>% 
  group_by(icustay_id) %>% 
  filter(n()>1) %>% .$icustay_id
a = t2.roll.eod %>% filter(icustay_id == 200143)

icustay_diur_cv = meds_cv.tidy %>% 
  inner_join(demo_1ststay %>% select(icustay_id, icu_intime), by = c("ICUSTAY_ID" = "icustay_id")) %>% 
  filter(ITEMID %in% c(221794, 228340, 3439, 6120, 30123, 4888, 7780, 4219,
                       7809, 46690, 45275, 41518, 46554, 46197, 43047,
                       3192, 46771)) %>% 
  mutate(flag.1st_day = if_else(as.numeric(difftime(CHARTTIME, icu_intime, units = "hours")) <= 24, 1, 0)) %>% 
  filter(flag.1st_day == 1) %>% 
  select(ICUSTAY_ID)
icustay_diur_mv = meds_mv.tidy %>% 
  inner_join(demo_1ststay %>% select(icustay_id, icu_intime), by = c("ICUSTAY_ID" = "icustay_id")) %>% 
  filter(ITEMID %in% c(221794, 228340, 3439, 6120, 30123, 4888, 7780, 4219,
                       7809, 46690, 45275, 41518, 46554, 46197, 43047,
                       3192, 46771)) %>% 
  mutate(flag.1st_day = if_else(as.numeric(difftime(STARTTIME, icu_intime, units = "hours")) <= 24, 1, 0)) %>% 
  filter(flag.1st_day == 1) %>% 
  select(ICUSTAY_ID)
icustay_diur_bind = bind_rows(icustay_diur_cv, icustay_diur_mv)
t2.roll.eod.diur = t2.roll.eod %>% 
  semi_join(icustay_diur_bind, by = c("icustay_id" = "ICUSTAY_ID"))
t2.roll.multdays = t2.roll.eod.diur %>% 
  group_by(icustay_id) %>% 
  filter(n()>1) %>% .$icustay_id

t2.roll.min_per_day = t2.roll.eod.diur %>% 
  group_by(icustay_id) %>% 
  dplyr::slice(which.min(cum_rate_all))
median(t2.roll.min_per_day$cum_rate_all)
mean(t2.roll.min_per_day$cum_rate_all)

a = t2.roll.eod.diur %>% filter(icustay_id == 200011)
t2.roll.eod.diur %>% ggplot(aes(x = cum_rate_all))+geom_histogram(bins = 100)
t2.roll.min_per_day %>% ggplot(aes(x = cum_rate_all))+geom_histogram(bins = 100)

t2.los.cp = t2.roll.min_per_day %>% 
  inner_join(demo_1ststay %>% select(icustay_id, icu_los, ADMITTIME, DISCHTIME)) %>% 
  mutate(flag.diur_grp = if_else(cum_rate_all >= -3000, "Low", "High"),
         adm_los = difftime(DISCHTIME, ADMITTIME, units = "days")) %>% 
  select(icustay_id, cum_rate_all, icu_los, adm_los, flag.diur_grp)

t2.los.cp %>% ggplot(aes(x = icu_los, fill = flag.diur_grp))+geom_histogram(bins = 50)
t2.los.cp %>% rename(Diuretics_Group = flag.diur_grp, ICU_LOS = icu_los) %>% 
  ggplot(aes(x = ICU_LOS, fill = Diuretics_Group))+geom_histogram(bins = 50) +
  facet_grid(Diuretics_Group~.)
ggsave("diuretics_vs_los.png", device = "png", plot = last_plot())

t2.los.cp %>% ggplot(aes(x = adm_los, fill = flag.diur_grp))+geom_histogram(bins = 50)
t2.los.cp %>% ggplot(aes(x = adm_los, fill = flag.diur_grp))+geom_histogram(bins = 50) +
  facet_grid(flag.diur_grp~.)

t.test(t2.los.cp %>% filter(flag.diur_grp=='High') %>% .$icu_los, 
       t2.los.cp %>% filter(flag.diur_grp=='Low') %>% .$icu_los)
t.test(t2.los.cp %>% filter(flag.diur_grp=='High') %>% .$adm_los, 
       t2.los.cp %>% filter(flag.diur_grp=='Low') %>% .$adm_los)

## compare creatinine 
creat.icustay = demo_1ststay %>% 
  inner_join(labs.tidy %>% filter(ITEMID==50912 | ITEMID==51081), by = c("hadm_id" = "HADM_ID"))
t2.creat.cp = t2.roll.min_per_day %>% 
  inner_join(creat.icustay %>% select(icustay_id, CHARTTIME, VALUENUM)) %>% 
  mutate(flag.diur_grp = if_else(cum_rate_all >= -3000, "Low", "High")) %>% 
  mutate(creat.time = as.numeric(difftime(CHARTTIME, icu_intime, units = "days"))) %>% 
  filter(creat.time > 0) %>% 
  group_by(icustay_id) %>% 
  dplyr::slice(which.max(VALUENUM))
t2.creat.cp %>% rename(Diuretics_Group = flag.diur_grp) %>% 
  ggplot(aes(x = VALUENUM, fill = Diuretics_Group))+geom_histogram(bins = 100) +
  facet_grid(Diuretics_Group~.) +
  labs(x = "Creatinine Value")
ggsave("diuretics_vs_creat.png", device = "png", plot = last_plot())

t.test(t2.creat.cp %>% filter(flag.diur_grp=='High') %>% .$VALUENUM, 
       t2.creat.cp %>% filter(flag.diur_grp=='Low') %>% .$VALUENUM)

