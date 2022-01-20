db <- tidy_ae_table(population_from  = adsl %>% rename(TRTA = TRT01A),
                    observation_from = adae,
                    population_where = NULL,
                    observation_where = NULL,
                    treatment_var = "TRTA",
                    treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                    stratum_var = NULL,
                    ae_var = "AEDECOD",
                    ae_interested = define_ae_select_list(ae_criterion = c('AESER == "Y"', 'AEREL != "NONE"'),
                                                          ae_label = c("with serious adverse events", "with drug-related adverse events")),
                    listing_interested = define_ae_listing(listing_var = c("USUBJID", "SEX", "AGE", "AEDECOD"),
                                                           listing_label = c("ID", "Gender", "Age", "Adverse Event")),
                    ae_listing_label = "Adverse Event")


test_that("expect errors if fig_prop_color is not a string vector of length 2", {
  expect_error(forestly(db,
           fig_prop_color = c("gold", "purple", "blue"),
           fig_prop_label = NULL,
           fig_diff_color = "black",
           fig_diff_label = NULL,
           small_sample = c(4, 4)))
})

test_that("expect errors if fig_prop_colwidth/fig_diff_colwidth is not a positive number", {
  expect_error(forestly(db,
                        fig_prop_color = c("gold", "purple"),
                        fig_prop_label = NULL,
                        fig_diff_color = "black",
                        fig_diff_label = NULL,
                        fig_prop_colwidth = -100,
                        small_sample = c(4, 4)))
  expect_error(forestly(db,
                        fig_prop_color = c("gold", "purple"),
                        fig_prop_label = NULL,
                        fig_diff_color = "black",
                        fig_diff_label = NULL,
                        fig_diff_colwidth = -100,
                        small_sample = c(4, 4)))
})

snap_save_html <- function(code) {
  path <- tempfile(fileext = ".html")
  htmltools::save_html(code, path, background = "white", libdir = "lib", lang = "en")
  path
}

test_that("snapshot testing for interactive forest plot using forestly() (no stratum)", {
   ## Generate interavtive forest plot
  
   html <- forestly(db,
            fig_prop_color = c("gold", "purple"),
            fig_prop_label = NULL,
            fig_diff_color = "black",
            fig_diff_label = NULL,
            small_sample = c(4, 4))
   
   
   expect_snapshot_file(snap_save_html(html), 'forestly0no0stratum.html')
   
})

adsl$STRATUMN <- sample(seq(1,3), size = length(adsl$USUBJID), prob = c(0.3, 0.3, 0.4), replace = TRUE)
adae <- adae %>% left_join(data.frame(USUBJID = adsl$USUBJID, STRATUMN = adsl$STRATUMN))
## Tidy data
db <- tidy_ae_table(population_from  = adsl %>% rename(TRTA = TRT01A),
                    observation_from = adae,
                   population_where = NULL,
                   observation_where = NULL,
                   treatment_var = "TRTA",
                   treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                     stratum_var = "STRATUMN", 
                     ae_var = "AEDECOD",
                     ae_interested = define_ae_select_list(ae_criterion = c('AESER == "Y"',
                                                                            'AEREL != "NONE"'),
                                                           ae_label = c("with serious adverse events",
                                                                        "with drug-related adverse events")),
                     listing_interested = define_ae_listing(listing_var = c("USUBJID", "SEX", "AGE", "AEDECOD"),
                                                            listing_label = c("ID", "Gender", "Age", "Adverse Event")),
                     ae_listing_label = "Adverse Event")

test_that("snapshot testing for interactive forest plot using forestly() (with stratum)", {
  ## Generate interavtive forest plot
  html <- forestly(db,
                   fig_prop_color = c("gold", "purple"),
                   fig_prop_label = NULL,
                   fig_diff_color = "black",
                   fig_diff_label = NULL,
                   small_sample = c(4, 4))
  
  expect_snapshot_file(snap_save_html(html), 'forestly0stratum.html')
  
})