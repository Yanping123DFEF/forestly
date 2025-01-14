#' Generate interactive forest plot with a select list as filter
#'
#' @param db a list of two components:
#'           (1) db$table: ae data frame rbinded by interested ae labels
#'           (2) db$listing: a standard adverse event data frame with select columns
#' @param fig_prop_color a character vector of length two to specify the colors to plot the two proportions.
#'                       The first entry refers to the color of the AE proportion in the treatment arm.
#'                       The second entry refers to the color of the AE proportion in the control arm.
#'                       The default value is c("gold", "purple").
#' @param fig_prop_label a character vector of length two to specify the x-axis label of the two propositions plots.
#'                       The first entry refers to the x-lab legend of the treatment arm.
#'                       The second entry refers to the x-lab legend of the control arm.
#'                       The default value is  c("treatment", "control")
#' @param fig_prop_colwidth a numerical value to specify the column width of the two proportion plot
#'                          the default value is 300
#' @param fig_diff_color a string to specify the color to plot the error bar.
#'                       The default value is "black".
#' @param fig_diff_label a string to specify the x-axis legend of the error bar.
#'                       The default value is "treatment <- Favor -> control".
#' @param fig_diff_colwidth a numerical value to specify the column width of the two proportion plot
#'                          the default value is 300
#' @param small_sample a integral vector of length 2. The first element is for treatment group and 
#'                     the second element is for control group. The default value is c(4, 4).
#' @param forest_table_width the width of the forest plot reactbale
#' @param forest_table_bottom_margin the bottom margin of the forest plot reactable
#' 
#' @return a reactable with a select list
#' @export
#'
#' @examples
#' \dontrun{
#' library(forestly)
#' ## No stratum ----------------------------------------------
#' ## Tidy data
#' db <- tidy_ae_table(population_from  = adsl %>% rename(TRTA = TRT01A),
#'                     observation_from = adae,
#'                     population_where = NULL,
#'                     observation_where = NULL,
#'                     treatment_var = "TRTA",
#'                     treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
#'                     stratum_var = NULL,
#'                     ae_var = "AEDECOD",
#'                     ae_interested = define_ae_select_list(ae_criterion = c('AESER == "Y"',
#'                                                                            'AEREL != "NONE"'),
#'                                                           ae_label = c("with serious adverse events",
#'                                                                        "with drug-related adverse events")),
#'                     listing_interested = define_ae_listing(listing_var = c("USUBJID", "SEX", "AGE", "AEDECOD"),
#'                                                            listing_label = c("ID", "Gender", "Age", "Adverse Event")),
#'                     ae_listing_label = "Adverse Event")
#' ## Generate interavtive forest plot
#' forestly(db)
#' forestly(db,
#'          fig_prop_color = c("gold", "purple"),
#'          fig_prop_label = NULL,
#'          fig_diff_color = "black",
#'          fig_diff_label = NULL,
#'          small_sample = c(4, 4))
#' 
#' ## With stratum ----------------------------------------------
#' ## Impute stratum 
#' adsl$STRATUMN <- sample(seq(1,3), size = length(adsl$USUBJID), prob = c(0.3, 0.3, 0.4), replace = TRUE)
#' adae <- adae %>% left_join(data.frame(USUBJID = adsl$USUBJID, STRATUMN = adsl$STRATUMN))
#' ## Tidy data
#' db <- tidy_ae_table(population_from  = adsl %>% rename(TRTA = TRT01A),
#'                     observation_from = adae,
#'                     population_where = NULL,
#'                     observation_where = NULL,
#'                     treatment_var = "TRTA",
#'                     treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
#'                     stratum_var = "STRATUMN", 
#'                     ae_var = "AEDECOD",
#'                     ae_interested = define_ae_select_list(ae_criterion = c('AESER == "Y"',
#'                                                                            'AEREL != "NONE"'),
#'                                                           ae_label = c("with serious adverse events",
#'                                                                        "with drug-related adverse events")),
#'                     listing_interested = define_ae_listing(listing_var = c("USUBJID", "SEX", "AGE", "AEDECOD"),
#'                                                            listing_label = c("ID", "Gender", "Age", "Adverse Event")),
#'                     ae_listing_label = "Adverse Event")
#' ## Generate interavtive forest plot
#' forestly(db)
#' forestly(db,
#'          fig_prop_color = c("gold", "purple"),
#'          fig_prop_label = NULL,
#'          fig_diff_color = "black",
#'          fig_diff_label = NULL,
#'          small_sample = c(4, 4))
#'}



forestly <- function(db, 
                     fig_prop_color = c("#00857C", "#66203A"), 
                     fig_prop_label = NULL,
                     fig_prop_colwidth = 300,
                     fig_diff_color = "black", 
                     fig_diff_label = NULL,
                     fig_diff_colwidth = 300,
                     small_sample = NULL,
                     forest_table_width = 1200,
                     forest_table_bottom_margin = 50){
  
  # Check the correctness of input
  if(length(fig_prop_color) != 2){
    stop("Argument fig_prop_color should be a string vector of length 2!")
  }
  
  if(!is.numeric(fig_prop_colwidth) || fig_prop_colwidth < 0){
    stop("Argument fig_prop_colwidth should be positive number!")
  }
  
  if(!is.numeric(fig_diff_colwidth) || fig_diff_colwidth < 0){
    stop("Argument fig_prop_colwidth should be positive number!")
  }
  
  # Set the default arguments
  if(is.null(fig_prop_label)){
    fig_prop_label = names(db$treatment_order)
  }
  
  if(is.null(fig_diff_label)){
    fig_diff_label = paste0(names(db$treatment_order)[1],
                            "<- Favor ->",
                            names(db$treatment_order)[2])
  }
  
  if(is.null(small_sample)){
    small_sample = c(4, 4)
  }
  
  # Set the order of ae, ae_label
  db$table <- db$table %>% dplyr::mutate(ae = as.factor(ae),
                                         ae_label = as.factor(ae_label)) 
  
  # Listing of subjects details
  t_detail <- db$listing
  
  # Count number of events for each AE regardless the stratum
  tb_no_stratum <- db$table %>% 
    dplyr::select(-pct_1, -pct_2) %>% 
    dplyr::group_by(ae, ae_label) %>% 
    dplyr::summarise(n_1 = sum(n_1), n_2 = sum(n_2), N_1 = sum(N_1), N_2 = sum(N_2)) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(pct_1 = n_1 / N_1 * 100, pct_2 = n_2 / N_2 * 100)
  
  # Calculate test using M&N method 
  tb_rate_compare <- as.data.frame(do.call(rbind, 
                                           db$table %>% 
                                             dplyr::group_by(ae, ae_label) %>% 
                                             dplyr::group_map(~ rate_compare_sum(n0 = .x$N_1, n1 = .x$N_2, 
                                                                                 x0 = .x$n_1, x1 = .x$n_2, 
                                                                                 strata = .x$stratum))))
  tb <- cbind(tb_no_stratum, tb_rate_compare) %>% 
    dplyr::mutate_at(dplyr::vars(pct_1, pct_2, est, lower, upper, p), ~round(., 4)) %>%   # round into 4 digits
    dplyr::mutate(est = est * 100, lower = lower * 100, upper = upper * 100)              # change 0.1 into 10%
  # tb <- cbind(db$table,
  #             with(db$table, prop_test_mn(x0 = n_2, n0 = N_2, x1 = n_1, n1 = N_1))) 
  
  # Calculate the range of the forest plot
  fig_prop_range = round(range(c(tb$pct_1, tb$pct_2)) + c(-2, 2)) #c(-0.51, 0.51))
  fig_diff_range = round(range(c(tb$lower, tb$upper)) + c(-2, 2)) #c(-0.51, 0.51))
  
  # Calculate the sample size without stratum
  sample_size_no_stratum <- stats::aggregate(N ~ treatment, FUN = sum, data = db$sample_size)
  
  # Sort the data frame to display in the reactable
  t_display <- tb %>% dplyr::mutate(fig_prop = NA, 
                                    fig_diff = round(est, 2), 
                                    ae = tools::toTitleCase(tolower(ae))) %>% 
                      dplyr::select(ae, ae_label, 
                                    fig_prop, fig_diff, 
                                    lower, upper, 
                                    n_1, pct_1, n_2, pct_2)
  
  # Make the error bar = 0 if the sample size is smaller than 4
  t_display$fig_diff[(t_display$n_1 <= small_sample[1] & t_display$n_2 <= small_sample[2])] = 0
  t_display$lower[(t_display$n_1 <= small_sample[1] & t_display$n_2 <= small_sample[2])] = 0
  t_display$upper[(t_display$n_1 <= small_sample[1] & t_display$n_2 <= small_sample[2])] = 0
  
  # Create two variable for slider bar
  t_display$n_max <- pmax(t_display$n_1, t_display$n_2)
  t_display$n_min <- pmin(t_display$n_1, t_display$n_2)
  t_display$pct_max <- pmax(t_display$pct_1, t_display$pct_2)
  t_display$pct_min <- pmin(t_display$pct_1, t_display$pct_2)
  
  # Make the data frame eligible for check box design
  t_display1 <- crosstalk::SharedData$new(t_display)
  
  # Store the design details in a object
  design_details = forestly_design_details(t_display, 
                                           fig_prop_range, 
                                           fig_prop_label = fig_prop_label,
                                           fig_prop_color = fig_prop_color,
                                           fig_prop_colwidth = fig_prop_colwidth,
                                           fig_diff_range, 
                                           fig_diff_label = fig_diff_label,
                                           fig_diff_color = fig_diff_color,
                                           fig_diff_colwidth = fig_diff_colwidth)
  
  # Make a reactable with a select list
  p <- crosstalk::bscols(
    # Width of the select list and reactable
    widths = c(4, 4, 4, 12),
    # Make a select list
    crosstalk::filter_select(id = "filter_AEcategory", 
                             label = "AE filters", 
                             sharedData = t_display1, 
                             group = ~ae_label, 
                             multiple = FALSE),
    # Make a slider bar of the incidence percentage
    crosstalk::filter_slider(id = "filter_incidence", 
                             label = "Incidence (%) in One or More Treatment Groups", 
                             sharedData = t_display1, 
                             column = ~pct_max,                    # whose values will be used for this slider
                             step = 1,                             # specifies interval between each select-able value on the slider
                             #width = 250,                         # width of the slider control 
                             min = max(0, min(t_display$pct_min)), # the leftmost value of the slider
                             max = max(t_display$pct_max) + 2      # the rightmost value of the slider
                             ),
    # Make a slider bar of the AE count
    crosstalk::filter_slider(id = "filter_count", 
                             label = "Count in One or More Treatment Groups", 
                             sharedData = t_display1, 
                             column = ~n_max, 
                             step = 1, 
                             #width = 250,
                             min = max(0, min(t_display$n_min)),
                             max = max(t_display$n_max) + 2),
    # Make a reactable 
    mk_reactable(  #mk_reactable saved in the R/ folder: define default behavior of reactable.
      
      # Data frame to plot
      t_display1,
      
      # Define the width of the reactable, i.e., the forest plot table
      width = forest_table_width,
      
      # Default sort variable
      defaultSorted = c("ae_label", "fig_diff"),
      defaultSortOrder = c("desc", "asc"),
      
      # Define listing of subjects
      details = function(index){
        ae_label_idx = -1
        for (i in seq_along(t_detail)) {
          if(t_display[index, ]$ae_label == t_detail[[i]]$ae_label){
            ae_label_idx <- i
            break
          }
        }
        subset(t_detail[[ae_label_idx]]$ae_listing,
               toupper(t_detail[[ae_label_idx]]$ae_listing[[db$ae_listing_label]]) %in% toupper(t_display[index, ]$ae)) %>% mk_reactable
      },
      # details = function(index){
      #   t_row <- t_display[index, ]
      #   subset(t_detail,
      #          toupper(AE) %in% toupper(t_row$ae)) %>% mk_reactable
      # },
      
      # Customize cell feature
      columnGroups = list(
        reactable::colGroup(name = paste0(fig_prop_label[1], "(N=", as.numeric(sample_size_no_stratum[1, "N"]), ")"),  columns = c("n_1", "pct_1")),
        reactable::colGroup(name = paste0(fig_prop_label[2], "(N=", as.numeric(sample_size_no_stratum[2, "N"]), ")"), columns = c("n_2", "pct_2"))
      ),
      
      # List of column definitions
      columns = list(
        ae = reactable::colDef(header = "Adverse Events", minWidth = 150, align = "right"),
        
        #stratum = reactable::colDef(header = "Stratum", show = TRUE),
        
        ae_label = reactable::colDef(header = "Label", show = FALSE),
        
        fig_prop = reactable::colDef(header = "AE Proportion (%)",
                                     width = fig_prop_colwidth, 
                                     align = "center",
                                     cell = reactable::JS(design_details$design_prop_cell),
                                     footer = reactable::JS(design_details$design_prop_footer),
                                     html = TRUE,
                                     style = "font-size: 0px; padding: 0px; margin: 0px;",
                                     footerStyle = "font-size: 0px; padding: 0px; margin: 0px;"),
        
        fig_diff = reactable::colDef(header = "Risk Diff (%) + 95% CI",
                                     defaultSortOrder = "desc",
                                     width = fig_prop_colwidth, 
                                     align = "center",
                                     cell = reactable::JS(design_details$design_diff_cell),
                                     footer = reactable::JS(design_details$design_diff_footer),
                                     html = TRUE,
                                     style = "font-size: 0px; padding: 0px; margin: 0px;"),
        
        n_1 = reactable::colDef(header = "n", defaultSortOrder = "desc", width = 60, align = "center"),
        n_2 = reactable::colDef(header = "n", defaultSortOrder = "desc", width = 60, align = "center"),
        
        pct_1 = reactable::colDef(header = "(%)", defaultSortOrder = "desc", width = 80,
                                  align = "center", format = reactable::colFormat(digits = 2) ),
        pct_2 = reactable:: colDef(header = "(%)", defaultSortOrder = "desc", width = 80,
                                   align = "center", format = reactable::colFormat(digits = 2) ),
        
        lower = reactable::colDef(header = "lower CI", show = FALSE),
        upper = reactable::colDef(header = "upper CI", show = FALSE),
        
        # variables helps the slider bar
        pct_max = reactable::colDef(header = "max proportion", show = FALSE),
        pct_min = reactable::colDef(header = "min proportion", show = FALSE),
        n_max = reactable::colDef(header = "max count", show = FALSE),
        n_min = reactable::colDef(header = "min count", show = FALSE)
      )
    ),
    crosstool::crosstool(t_display1,
                         # Transceiver widgets are more like normal crosstalk widgets.
                         class = "transceiver",
                         # Set the initial value
                         init = which(t_display$ae_label == "All"),
                         # Channel set to "filter" to use the crosstalk filter handle
                         channel = "filter",
                         # Reset optional vector of crosstalk group keys;
                         # Use with init when data == relay (one crosstalk group) to 
                         # Reset the initial filter/select handle.
                         reset = rownames(t_display), height = forest_table_bottom_margin)
  )
  
  
  plotly_js <- "https://unpkg.com/react-plotly.js@1.0.2/dist/create-plotly-component.js"
  htmltools::browsable(htmltools::tagList(
    reactR::html_dependency_react(),
    htmltools::tags$script(src = "https://cdn.plot.ly/plotly-latest.min.js"),
    htmltools::tags$script(src = plotly_js),
    p
  ))
  
}

