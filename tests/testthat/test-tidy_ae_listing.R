library(tibble)
db <- tibble(USUBJID = c("01", "01", "02", "03", "03", "03", "04", "04", "05"),
           SITEID = c("site1", "site1", "site2", "site3", "site3", "site3", "site4", "site4", "site5"),
           SEX = c("F", "F", "M", "F", "F", "F", "M", "M", "F"),
           AGE = c(65, 65, 30, 22, 22, 22, 70, 70, 53),
           RACE = c("Asian", "Asian", "Latino or Hispanic",
                    "WHITE", "WHITE", "WHITE",
                    "BLACK OR AFRICAN AMERICAN",
                    "BLACK OR AFRICAN AMERICAN",
                    "AMERICAN INDIAN OR ALASKA NATIVE"),
           ADURN = c(11, 3, 4, 5, 7, 20, 90, 34, 58),
           ADURU = rep("minutes", 9))


test_that("compare the colname with standard AE tables", {
  ae_listing1 <- tidy_ae_listing(db, listing_var = c("USUBJID", "SITEID", "SEX", "RACE", "AGE"),
                                listing_label = c("ID", "Site Number", "Sex", "Race", "Age"))
  expect_equal(names(ae_listing1), c("ID", "Site Number", "Sex", "Race", "Age"))
  
  ae_listing2 <- tidy_ae_listing(db, listing_var = c("USUBJID", "SEX", "ADURN", "ADURU"),
                                listing_label = c("ID", "Gender", "Duration", "Duration"))
  expect_equal(names(ae_listing2), c("ID", "Gender", "Duration"))
})

test_that("compare tidy_ae_listing(db) with db[, listing_var] ignoring the colnames, the order of columns and AE duration", {
  ae_listing3 <- tidy_ae_listing(db, listing_var = c("USUBJID", "AGE", "RACE", "ADURN", "ADURU"),
                                 listing_label = c("ID",  "Age", "Race", "Duration", "Duration"))

  db_sub3 <-db[,c("USUBJID","AGE", "RACE", "ADURN", "ADURU")]

  expect_equal(ae_listing3$ID, db_sub3$USUBJID)
  expect_equal(ae_listing3$Age, as.character(db_sub3$AGE))
  expect_equal(ae_listing3$Race, stringr::str_to_title(db_sub3$RACE))
  expect_equal(ae_listing3$Duration, paste(db_sub3$ADURN, stringr::str_to_title(db_sub3$ADURU)))
  
})

test_that("will pop-up error when column not in input dataset", {
  expect_error(tidy_ae_listing(db, listing_var = c("USUBJID", "AGE", "ETHNICITY"),
                               listing_label = c("ID",  "Age", "Race")))
  
})
