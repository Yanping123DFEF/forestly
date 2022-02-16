test_that("throw error message is the path is missing", {
  expect_error(tlf_download())
})

test_that("download all files under the given path", {
   temp_dir <- tempdir()
   tlf_ae_specific(population_from = adsl %>% rename(TRTA=TRT01A),
   observation_from = adae,
   population_where = "ITTFL == 'Y'",
   observation_where = "TRTEMFL == 'Y'",
   treatment_var = "TRTA",
   treatment_order =c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
   ae_var = "AEDECOD",
   ae_grp = "AEBODSYS",
   display_ci = FALSE,
   display_total = FALSE,
   display_pval = FALSE,
   stratum_var = NULL,
   title_text = "Participants With Adverse Events by System Organ Class and Preferred Term",
   subtitle_text = c("(Incidence > 0% in More or More Treatment Group)","(APaT Population)"),
   end_notes = "Every subject is counted a single time for each applicable row and column.
   Database Cutoff Date: 01SEP2021.",
   output_report = file.path(temp_dir, 'ae0specific.rtf'),
   output_dataframe = NULL)

   expect_snapshot_output(tlf_download(
     path = temp_dir,
     output_name = "AE Specific",
     button_label = "Download AE Specific table"
   ))

   # unlink("temp_dir", recursive = TRUE)
   list.files(temp_dir)
   expect_true( "ae0specific.rtf" %in% list.files(temp_dir))

})