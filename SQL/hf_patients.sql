"SELECT * FROM `physionet-data.mimiciii_clinical.diagnoses_icd` WHERE HADM_ID IN (

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