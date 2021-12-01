test_that("define_ae_list() gives errors if the number of variable names does't match that of variable labels", {
  
   expect_error(define_ae_listing(listing_var = c("USUBJID", "ADURN", "ADURU"),
                     listing_label = c("ID", "Duration", "Duration", "Age")))
})

test_that("define_ae_list() gives default list if `listing_var` and `listing_lavel` are null", {
  
  expect_equal(define_ae_listing()$listing_var,  c("USUBJID", "SEX", "RACE", "AGE"))
  
  expect_equal(define_ae_listing()$listing_label, c("Participant ID", "Gender", "Race", "Age"))
})
