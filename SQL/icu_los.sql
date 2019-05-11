"SELECT
  a.subject_id,
  a.hadm_id,
  c.icustay_id,
  DOB,
  ADMITTIME,
  DISCHTIME,
  ethnicity,
  hospital_expire_flag,
  gender,
  deathtime,
  c.los AS icu_los,
  c.first_careunit AS icu_first_careunit,
  intime AS icu_intime,
  outtime AS icu_outime,
  c.last_careunit AS icu_last_careunit
FROM
  `physionet-data.mimiciii_clinical.admissions` A
INNER JOIN
  `physionet-data.mimiciii_clinical.patients` b
ON
  a.subject_id=b.subject_id
INNER JOIN
  `physionet-data.mimiciii_clinical.icustays` C
ON
  a.hadm_id = c.hadm_id
WHERE
  A.HADM_ID IN (
  SELECT
    DISTINCT HADM_ID
  FROM
    `physionet-data.mimiciii_clinical.diagnoses_icd`
  WHERE
    ICD9_CODE IN ('4280',
      '4281',
      '4289',
      '39891',
      '40201',
      '40211',
      '40291',
      '40401',
      '40403',
      '40411',
      '40413',
      '40491',
      '40493',
      '42820',
      '42821',
      '42822',
      '42823',
      '42830',
      '42831',
      '42832',
      '42833',
      '42840',
      '42841',
      '42842',
      '42843'))"