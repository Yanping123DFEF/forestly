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
  AEREL = c("N", "Y", "N", "N", "N", "Y", "N", "N", "Y"),
  SAFFL = c("Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"),
  TRTEMFL = c("Y", "Y", "N", "Y", "Y", "Y", "N", "Y", "Y")
)

test_that("expect warning if user wants to dispaly ci and total", {
  expect_error(tlf_ae_summary(population_from = tb,
                              observation_from = db,
                              population_where = NULL,
                              observation_where = NULL,
                              treatment_var = "treatment",
                              treatment_order =treatment_order,
                              ae_var = "AEDECOD",
                              ae_interested = define_ae_select_list(ae_criterion = c('AESER == "Y"', 'AEREL = "Y"'),
                                                                    ae_label = c("with serious adverse events",
                                                                                 "with drug-related adverse events")),
                              stratum_var = NULL,
                              display_ci = TRUE,
                              display_total = TRUE,
                              title_text = "Analysis of Adverse Event Summary", 
                              subtitle_text = NULL,
                              end_notes ="Every subject is counted a single time for each applicable row and column.",
                              output_report = file.path(tempdir(), 'ae0summary.rtf'),
                              output_dataframe = file.path(tempdir(), 'ae0summary.RData')))
})

test_that("dispaly ci in ae summary", {
 tlf_ae_summary(population_from = tb,
                              observation_from = db,
                              population_where = NULL,
                              observation_where = NULL,
                              treatment_var = "treatment",
                              treatment_order = treatment_order,
                              ae_var = "AEDECOD",
                              ae_interested = define_ae_select_list(ae_criterion = c('AESER == "Y"', 'AEREL = "Y"'),
                                                                    ae_label = c("with serious adverse events",
                                                                                 "with drug-related adverse events")),
                              stratum_var = NULL,
                              display_ci = TRUE,
                              display_total = FALSE,
                              title_text = "Analysis of Adverse Event Summary", 
                              subtitle_text = NULL,
                              end_notes ="Every subject is counted a single time for each applicable row and column.",
                              output_report = file.path(tempdir(), 'ae0summary.rtf'),
                              output_dataframe = file.path(tempdir(), 'ae0summary.RData'))
  
  load(file.path(tempdir(), 'ae0summary.RData'))
  
  expect_equal(as.numeric(x$n_1), c(4,3,1,1,3))
  expect_equal(as.numeric(x$pct_1), c(NA,75, 25, 25, 75))
  expect_equal(as.numeric(x$n_2), c(2,1, 1, 1, 2))
  expect_equal(as.numeric(x$pct_2), c(NA,50, 50, 50, 100))
  expect_equal(as.character(x$est), c(NA, "25.0(-0.5, 0.8)", "-25.0(-0.8, 0.5)","-25.0(-0.8, 0.5)", "-25.0(-0.7, 0.5)"))
  expect_equal(as.numeric(x$pvalue), c(NA,0.288, 0.712, 0.712, 0.760))
  
  encode <- paste(readLines(file.path(tempdir(), "ae0summary.rtf")), collapse = "\n")
  expect_snapshot_output(encode)
  
})

test_that("dispaly total in ae summary", {
  tlf_ae_summary(population_from = tb,
                 observation_from = db,
                 population_where = NULL,
                 observation_where = NULL,
                 treatment_var = "treatment",
                 treatment_order = treatment_order,
                 ae_var = "AEDECOD",
                 ae_interested = define_ae_select_list(ae_criterion = c('AESER == "Y"', 'AEREL = "Y"'),
                                                       ae_label = c("with serious adverse events",
                                                                    "with drug-related adverse events")),
                 stratum_var = NULL,
                 display_ci = FALSE,
                 display_total = TRUE,
                 title_text = "Analysis of Adverse Event Summary", 
                 subtitle_text = NULL,
                 end_notes ="Every subject is counted a single time for each applicable row and column.",
                 output_report = file.path(tempdir(), 'ae0summary.rtf'),
                 output_dataframe = file.path(tempdir(), 'ae0summary.RData'))
  
  load(file.path(tempdir(), 'ae0summary.RData'))
  
  expect_equal(as.numeric(x$n_1), c(4, 3, 1,1,3))
  expect_equal(as.numeric(x$pct_1), c(NA,75, 25, 25, 75))
  expect_equal(as.numeric(x$n_2), c(2,1, 1, 1,2))
  expect_equal(as.numeric(x$pct_2), c(NA,50, 50, 50, 100))
  expect_equal(as.numeric(x$tot_n), c(6, 4, 2, 2,5))
  expect_equal(as.numeric(x$tot_pct), c(NA,66.7, 33.3, 33.3, 83.3))
  
  encode <- paste(readLines(file.path(tempdir(), "ae0summary.rtf")), collapse = "\n")
  expect_snapshot_output(encode)
  
})

test_that("dispaly no ci and total in ae summary", {
  tlf_ae_summary(population_from = tb,
                 observation_from = db,
                 population_where = NULL,
                 observation_where = NULL,
                 treatment_var = "treatment",
                 treatment_order = treatment_order,
                 ae_var = "AEDECOD",
                 ae_interested = define_ae_select_list(ae_criterion = c('AESER == "Y"', 'AEREL = "Y"'),
                                                       ae_label = c("with serious adverse events",
                                                                    "with drug-related adverse events")),
                 stratum_var = NULL,
                 display_ci = FALSE,
                 display_total = FALSE,
                 title_text = "Analysis of Adverse Event Summary", 
                 subtitle_text = NULL,
                 end_notes ="Every subject is counted a single time for each applicable row and column.",
                 output_report = file.path(tempdir(), 'ae0summary.rtf'),
                 output_dataframe = file.path(tempdir(), 'ae0summary.RData'))
  
  load(file.path(tempdir(), 'ae0summary.RData'))
  
  expect_equal(as.numeric(x$n_1), c(4,3, 1, 1,3))
  expect_equal(as.numeric(x$pct_1), c(NA,75, 25, 25, 75))
  expect_equal(as.numeric(x$n_2), c(2,1,1,1,2))
  expect_equal(as.numeric(x$pct_2), c(NA,50, 50, 50, 100))
  
  encode <- paste(readLines(file.path(tempdir(), "ae0summary.rtf")), collapse = "\n")
  expect_snapshot_output(encode)
  
})

test_that("stratum_var not null", {
  tlf_ae_summary(population_from = tb,
                 observation_from = db,
                 population_where = NULL,
                 observation_where = NULL,
                 treatment_var = "treatment",
                 treatment_order = treatment_order,
                 ae_var = "AEDECOD",
                 ae_interested = define_ae_select_list(ae_criterion = c('AESER == "Y"', 'AEREL = "Y"'),
                                                       ae_label = c("with serious adverse events",
                                                                    "with drug-related adverse events")),
                 stratum_var = "stratum",
                 display_ci = TRUE,
                 display_total = FALSE,
                 title_text = "Analysis of Adverse Event Summary", 
                 subtitle_text = NULL,
                 end_notes ="Every subject is counted a single time for each applicable row and column.",
                 output_report =  'ae0summary.rtf',
                 output_dataframe = file.path(tempdir(), 'ae0summary.RData'))
  
  load(file.path(tempdir(), 'ae0summary.RData'))
  
  expect_equal(as.numeric(x$n_1), c(4,3, 1, 1,3))
  expect_equal(as.numeric(x$pct_1), c(NA,75, 25, 25, 75))
  expect_equal(as.numeric(x$n_2), c(2,1,1,1,2))
  expect_equal(as.numeric(x$pct_2), c(NA,50, 50, 50, 100))
  expect_equal(as.character(x$est), c(NA, "25.0(-0.5, 0.8)", "-25.0(-0.8, 0.5)", "-25.0(-0.8, 0.5)", "-25.0(-0.8, 0.6)"))
  expect_equal(as.numeric(x$pvalue), c(NA,0.240, 0.760, 0.691, 0.760))
  
  encode <- paste(readLines(file.path(tempdir(), "ae0summary.rtf")), collapse = "\n")
  expect_snapshot_output(encode)
  
})

test_that("ae_interested equal to 'null'", {
  tlf_ae_summary(population_from = tb,
                 observation_from = db,
                 population_where = NULL,
                 observation_where = NULL,
                 treatment_var = "treatment",
                 treatment_order = treatment_order,
                 ae_var = "AEDECOD",
                 ae_interested = "NULL",
                 stratum_var = NULL,
                 display_ci = FALSE,
                 display_total = FALSE,
                 title_text = "Analysis of Adverse Event Summary", 
                 subtitle_text = NULL,
                 end_notes ="Every subject is counted a single time for each applicable row and column.",
                 output_report = file.path(tempdir(), 'ae0summary.rtf'),
                 output_dataframe = file.path(tempdir(), 'ae0summary.RData'))
  
  load(file.path(tempdir(), 'ae0summary.RData'))
  
  expect_equal(as.numeric(x$n_1), c(4,3, 1))
  expect_equal(as.numeric(x$pct_1), c(NA,75, 25))
  expect_equal(as.numeric(x$n_2), c(2,1,1))
  expect_equal(as.numeric(x$pct_2), c(NA,50, 50))
  
  encode <- paste(readLines(file.path(tempdir(), "ae0summary.rtf")), collapse = "\n")
  expect_snapshot_output(encode)
  
})

test_that("no end note", {
  tlf_ae_summary(population_from = tb,
                 observation_from = db,
                 population_where = NULL,
                 observation_where = NULL,
                 treatment_var = "treatment",
                 treatment_order = treatment_order,
                 ae_var = "AEDECOD",
                 ae_interested = NULL,
                 stratum_var = NULL,
                 display_ci = FALSE,
                 display_total = FALSE,
                 title_text = "Analysis of Adverse Event Summary", 
                 subtitle_text = NULL,
                 end_notes =NULL,
                 output_report = file.path(tempdir(), 'ae0summary.rtf'),
                 output_dataframe = file.path(tempdir(), 'ae0summary.RData'))
  
  load(file.path(tempdir(), 'ae0summary.RData'))
  
  expect_equal(as.numeric(x$n_1), c(4,3, 1))
  expect_equal(as.numeric(x$pct_1), c(NA,75, 25))
  expect_equal(as.numeric(x$n_2), c(2,1,1))
  expect_equal(as.numeric(x$pct_2), c(NA,50, 50))
  
  encode <- paste(readLines(file.path(tempdir(), "ae0summary.rtf")), collapse = "\n")
  expect_snapshot_output(encode)
  
})

