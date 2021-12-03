library(dplyr)
library(tidyr)
treatment_order <- c("MK9999" = "Xanomeline", "Placebo" = "Placebo")

tb <- tibble(
  USUBJID = c("01", "02", "03", "04",  "05" , "06"),
  stratum = c("a", "b", "a", "b", "a","b"),
  treatment = factor(c(
    "Xanomeline", "Placebo", "Placebo","Xanomeline", "Xanomeline","Xanomeline"
  )))

db <- tibble(
  USUBJID = c("01", "01", "02", "03", "03", "03", "04", "04", "05"),
  stratum = c("a", "a", "b", "a", "a", "a", "b", "b", "a"),
  treatment = factor(c(
    "Xanomeline", "Xanomeline", "Placebo", "Placebo", "Placebo",
    "Placebo", "Xanomeline", "Xanomeline", "Xanomeline"
  ),
  levels = treatment_order, labels = treatment_order
  ),
  AEDECOD = c(
    "DIARRHOEA", "FATIGUE", "FATIGUE", "RASH", "COUGH",
    "COUGH", "DIARRHOEA", "DIARRHOEA", "DIARRHOEA"
  ),
  AEBODSYS = c(
    "GASTROINTESTINAL DISORDERS", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS", "SKIN AND SUBCUTANEOUS TISSUE DISORDERS", "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",
    "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS", "GASTROINTESTINAL DISORDERS", "GASTROINTESTINAL DISORDERS", "GASTROINTESTINAL DISORDERS"
  ),
  AESER = c("N", "N", "N", "Y", "N", "N", "N", "Y", "N"),
  AEREL = c("N", "Y", "N", "N", "N", "Y", "N", "N", "Y")
)

test_that("compare outputs for all specific AEs", {
  tlf_ae_specific(population_from = tb,observation_from = db,
                  population_where = NULL,observation_where = NULL,
                  treatment_var = "treatment",
                  treatment_order =treatment_order,
                  ae_var = "AEDECOD",
                  ae_grp = NULL,
                  display_ci = FALSE,
                  display_total = TRUE,
                  display_pval = FALSE,
                  stratum_var = NULL,
                  title_text = "Participants With Adverse Events by System Organ Class and Preferred Term",
                  subtitle_text = c("(Incidence > 0% in More or More Treatment Group)","(APaT Population)"),
                  end_notes = "Every subject is counted a single time for each applicable row and column.
Database Cutoff Date: 01SEP2021.",
                  output_report = file.path(tempdir(), 'ae0specific.rtf'),
                  output_dataframe = file.path(tempdir(), 'ae0specific.RData'))

  load(file.path(tempdir(), 'ae0specific.RData'))
  expect_equal(as.numeric(x$exp), c(4,3,1,NA,0,3,1,0))
  expect_equal(as.numeric(x$exp_CI), round(c(NA,3,1,NA,0,3,1,0)/4*100,1))
  expect_equal(as.numeric(x$pbo), c(2,2,0,NA,1,0,1,1))
  expect_equal(as.numeric(x$pbo_CI), round(c(NA,2,0,NA,1,0,1,1)/2*100,1))
  expect_equal(as.numeric(x$total), c(6,5,1,NA,1,3,2,1))
  expect_equal(as.numeric(x$total_CI), round(c(NA,5,1,NA,1,3,2,1)/6*100,1))
  })

test_that("compare outputs for drug-related AEs", {
  tlf_ae_specific(population_from = tb,observation_from = db,
                  population_where = NULL,observation_where = "AEREL=='Y'",
                  treatment_var = "treatment",
                  treatment_order =treatment_order,
                  ae_var = "AEDECOD",
                  ae_grp = NULL,
                  display_ci = FALSE,
                  display_total = TRUE,
                  display_pval = FALSE,
                  stratum_var = NULL,
                  title_text = "Participants With Adverse Events by System Organ Class and Preferred Term",
                  subtitle_text = c("(Incidence > 0% in More or More Treatment Group)","(APaT Population)"),
                  end_notes = "Every subject is counted a single time for each applicable row and column.
Database Cutoff Date: 01SEP2021.",
                  output_report = file.path(tempdir(), 'ae0dr.rtf'),
                  output_dataframe = file.path(tempdir(), 'ae0dr.RData'))
  
  load(file.path(tempdir(), 'ae0dr.RData'))
  expect_equal(as.numeric(x$exp), c(4,2,2,NA,0,1,1))
  expect_equal(as.numeric(x$exp_CI), round(c(NA,2,2,NA,0,1,1)/4*100,1))
  expect_equal(as.numeric(x$pbo), c(2,1,1,NA,1,0,0))
  expect_equal(as.numeric(x$pbo_CI), round(c(NA,1,1,NA,1,0,0)/2*100,1))
  expect_equal(as.numeric(x$total), c(4,2,2,NA,0,1,1)+c(2,1,1,NA,1,0,0))
  expect_equal(as.numeric(x$total_CI), round((c(NA,2,2,NA,0,1,1)+c(NA,1,1,NA,1,0,0))/6*100,1))
})

test_that("compare outputs for serious AEs", {
  tlf_ae_specific(population_from = tb,observation_from = db,
                  population_where = NULL,observation_where = "AESER=='Y'",
                  treatment_var = "treatment",
                  treatment_order =treatment_order,
                  ae_var = "AEDECOD",
                  ae_grp = NULL,
                  display_ci = FALSE,
                  display_total = TRUE,
                  display_pval = FALSE,
                  stratum_var = NULL,
                  title_text = "Participants With Adverse Events by System Organ Class and Preferred Term",
                  subtitle_text = c("(Incidence > 0% in More or More Treatment Group)","(APaT Population)"),
                  end_notes = "Every subject is counted a single time for each applicable row and column.
Database Cutoff Date: 01SEP2021.",
                  output_report = file.path(tempdir(), 'ae0ser.rtf'),
                  output_dataframe = file.path(tempdir(), 'ae0ser.RData'))
  
  load(file.path(tempdir(), 'ae0ser.RData'))
  expect_equal(as.numeric(x$exp), c(4,1,3,NA,1,0))
  expect_equal(as.numeric(x$exp_CI), round(c(NA,1,3,NA,1,0)/4*100,1))
  expect_equal(as.numeric(x$pbo), c(2,1,1,NA,0,1))
  expect_equal(as.numeric(x$pbo_CI), round(c(NA,1,1,NA,0,1)/2*100,1))
  expect_equal(as.numeric(x$total), c(4,1,3,NA,1,0)+c(2,1,1,NA,0,1))
  expect_equal(as.numeric(x$total_CI), round((c(NA,1,3,NA,1,0)+c(NA,1,1,NA,0,1))/6*100,1))
})



test_that("compare outputs for AE by SOC and PT", {
  tlf_ae_specific(population_from = tb,observation_from = db,
                  population_where = NULL,observation_where = NULL,
                  treatment_var = "treatment",
                  treatment_order =treatment_order,
                  ae_var = "AEDECOD",
                  ae_grp = "AEBODSYS",
                  display_ci = FALSE,
                  display_total = TRUE,
                  display_pval = FALSE,
                  stratum_var = NULL,
                  title_text = "Participants With Adverse Events by System Organ Class and Preferred Term",
                  subtitle_text = c("(Incidence > 0% in More or More Treatment Group)","(APaT Population)"),
                  end_notes = "Every subject is counted a single time for each applicable row and column.
Database Cutoff Date: 01SEP2021.",
                  output_report = file.path(tempdir(), 'ae0bysoc.rtf'),
                  output_dataframe = file.path(tempdir(), 'ae0bysoc.RData'))
  
  load(file.path(tempdir(), 'ae0bysoc.RData'))
  expect_equal(as.numeric(x$exp), c(4,3,1,NA,3,3,1,1,0,0,0,0))
  expect_equal(as.numeric(x$exp_CI), round(c(NA,3,1,NA,3,3,1,1,0,0,0,0)/4*100,1))
  expect_equal(as.numeric(x$pbo), c(2,2,0,NA,0,0,1,1,1,1,1,1))
  expect_equal(as.numeric(x$pbo_CI), round(c(NA,2,0,NA,0,0,1,1,1,1,1,1)/2*100,1))
  expect_equal(as.numeric(x$total), c(4,3,1,NA,3,3,1,1,0,0,0,0)+c(2,2,0,NA,0,0,1,1,1,1,1,1))
  expect_equal(as.numeric(x$total_CI), round((c(NA,3,1,NA,3,3,1,1,0,0,0,0)+c(NA,2,0,NA,0,0,1,1,1,1,1,1))/6*100,1))
})


test_that("compare unstratified difference estimates, CI and p-value for specific AE term 'DIARRHOEA'", {
  tlf_ae_specific(population_from = tb,observation_from = db,
                  population_where = NULL,observation_where = "AEDECOD=='DIARRHOEA'",
                  treatment_var = "treatment",
                  treatment_order =treatment_order,
                  ae_var = "AEDECOD",
                  ae_grp = NULL,
                  display_ci = TRUE,
                  display_total = FALSE,
                  display_pval = TRUE,
                  stratum_var = NULL,
                  title_text = "Participants With Adverse Events by System Organ Class and Preferred Term",
                  subtitle_text = c("(Incidence > 0% in More or More Treatment Group)","(APaT Population)"),
                  end_notes = "Every subject is counted a single time for each applicable row and column.
Database Cutoff Date: 01SEP2021.",
                  output_report = file.path(tempdir(), 'diarrhoea0un0ratecompare.rtf'),
                  output_dataframe = file.path(tempdir(), 'diarrhoea0un0ratecompare.RData'))
  
  load(file.path(tempdir(), 'diarrhoea0un0ratecompare.RData'))
  res <- rate_compare_sum(n0=2,n1=4,x0=0,x1=3,weight="ss")
  expect_equal(paste(format(round(res$est * 100, 1), nsmall = 1),
                     " (", 
                     format(round(res$lower * 100, 1), nsmall = 1), 
                     ",", 
                     format(round(res$upper * 100, 1), nsmall = 1), 
                     ")", 
                     sep = ""), x[5,]$est)
  expect_equal(format(round(res$p, 3), nsmall = 3), x[5,]$pval)
})





test_that("compare stratified difference estimates, CI and p-value for specific AE term 'DIARRHOEA'", {
  tlf_ae_specific(population_from = tb,observation_from = db,
                  population_where = NULL,observation_where = "AEDECOD=='DIARRHOEA'",
                  treatment_var = "treatment",
                  treatment_order =treatment_order,
                  ae_var = "AEDECOD",
                  ae_grp = NULL,
                  display_ci = TRUE,
                  display_total = FALSE,
                  display_pval = TRUE,
                  stratum_var = "stratum",
                  title_text = "Participants With Adverse Events by System Organ Class and Preferred Term",
                  subtitle_text = c("(Incidence > 0% in More or More Treatment Group)","(APaT Population)"),
                  end_notes = "Every subject is counted a single time for each applicable row and column.
Database Cutoff Date: 01SEP2021.",
                  output_report = file.path(tempdir(), 'diarrhoea0ratecompare.rtf'),
                  output_dataframe = file.path(tempdir(), 'diarrhoea0ratecompare.RData'))
  
  load(file.path(tempdir(), 'diarrhoea0ratecompare.RData'))
  res <- rate_compare_sum(n0=c(1,1),n1=c(2,2),x0=c(0,0),x1=c(2,1),strata=c("a","b"),weight="ss")
  expect_equal(paste(format(round(res$est * 100, 1), nsmall = 1),
                     " (", 
                     format(round(res$lower * 100, 1), nsmall = 1), 
                     ",", 
                     format(round(res$upper * 100, 1), nsmall = 1), 
                     ")", 
                     sep = ""), x[5,]$est)
  expect_equal(format(round(res$p, 3), nsmall = 3), x[5,]$pval)
})




