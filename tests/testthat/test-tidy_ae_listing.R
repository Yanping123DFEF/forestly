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

ae_listing <- tidy_ae_listing(db, listing_var = c("USUBJID", "SITEID", "SEX", "RACE", "AGE"),
                              listing_label = c("ID", "Site Number", "Sex", "Race", "Age"))

test_that("compare the colname with standard AE tables", {

})

test_that("compare tidy_ae_listing(db) with db[, listing_var] ignoring the colnames, the order of columns and AE duration", {

})
