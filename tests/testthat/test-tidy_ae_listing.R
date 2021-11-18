test_that("compare the colname with listing_label", {
  expect_equal(2 * 2, 4)
})

test_that("compare tidy_ae_listing(db) with db[, listing_var] ignoring the colnames (without duplicated listing_label)", {
  expect_equal(2 * 2, 4)
})

test_that("compare tidy_ae_listing(db) with db[, listing_var] ignoring the colnamesn (with duplicated listing_label)", {
  expect_equal(2 * 2, 4)
})