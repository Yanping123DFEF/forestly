tb = data.frame(ae = c("headache", "pain", "fever", "running nose", 
                      "fever", "headache", "running nose"),
               ae_label = c("ALL", "ALL", "ALL", "ALL", "AESER", "AEREL", "AEREL"),
               n_1 = c(2, 3, 1, 5, 1, 1, 3),
               n_2 = c(4, 5, 3, 7, 3, 3, 5),
               N_1 = rep(50, 7),
               N_2 = rep(50, 7))
tb = tb %>% dplyr::mutate(pct_1 = n_1/N_1,
                         pct_2 = n_2/N_2,
                         fig_diff = pct_1 - pct_2,
                         lower = fig_diff - 0.05,
                         upper = fig_diff + 0.05)

details_list <- forestly_design_details(tb,
                            fig_prop_range = c(0, 0.8),
                            fig_prop_color = c("blue", "green"),
                            fig_prop_colwidth = 300,
                            fig_diff_range = c(-0.5, 0.9),
                            fig_diff_label = "treatment <- Favor -> control",
                            fig_diff_color = "blue",
                            fig_diff_colwidth = 300)

test_that("test if the defaults works", {
  expect_equal(length(details_list), 4)
  expect_equal(names(details_list), c("design_prop_cell", "design_diff_cell", "design_prop_footer", "design_diff_footer"))
})

test_that("compare one of the list objective is the same as returned from sparkline_point_js()", {
  expect_equal(details_list$design_prop_cell, sparkline_point_js(tbl = tb,
                                                                 type = "cell",
                                                                 x = c("pct_1", "pct_2"),
                                                                 y = c(1, 1),
                                                                 xlim = c(0, 0.8),
                                                                 color = c("blue", "green"),
                                                                 width = 300, 
                                                                 height = 30,
                                                                 text = c("x[0]", "x[1]")))
  
  expect_equal(details_list$design_diff_cell, sparkline_point_js(tbl = tb,
                                                                 type = "cell",
                                                                 x = "fig_diff",
                                                                 x_lower = "lower",
                                                                 x_upper = "upper",
                                                                 xlim = c(-0.5, 0.9),
                                                                 width = 300,
                                                                 text = "'range:' + x + '(' + x_lower + ',' + x_upper + ')'",
                                                                 color = "blue"))
})