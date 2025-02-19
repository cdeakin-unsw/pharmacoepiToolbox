#####
## EPE function
## written by Claire Deakin 2023-06-23

epe2 <- function(data,
                id,
                pbs_code,
                atc_code,
                date_dispensing,
                use_75th_percentile=TRUE,
                selected_pbs_codes=unique(pbs_code),
                max_time_period=180) {
  #'
  #' @title Copy of function to calculate the estimated period of exposure to dispensed medicines based on dispensing dates
  #' @author Claire Deakin
  #' @description
  #' Copy of 'epe' function which uses dispensing dates to calculate the estimated period of exposure (EPE) as the 75th percentile of the distribution of time periods between dispensings of medicines at the level of PBS codes.
  #' Gaps in dispensing are excluded from this period based on a default of 180 days (although alternative values can be specified).
  #' Original code has been modified to comment out the date variable check as this was causing an error message for the `mydata` toy dataset
  #'
  #'
  #' @param data data.frame containing input data; check your input data is a data.frame and not a tibble, coerce to data.frame if necessary. To use, specify data=your_input_data
  #' @param id name of character vector in `data` specifying a unique identifier for individual people/patients (can handle integer variables). To use, specify id="your_id_variable_name"
  #' @param pbs_code name of character vector in `data` specifying the PBS code for the medicine. To use, specify pbs_code="your_pbs_code_variable_name"
  #' @param atc_code name of character vector in `data` specifying the ATC code for the medicine. To use, specify atc_code="your_atc_code_variable_name"
  #' @param use_75th_percentile logical vector to specify whether the 75th percentile should be used as the EPE value. Default is `TRUE` i.e. the 75th percentile is used. If set to `FALSE` then the median is used instead
  #' @param date_dispensing name of date vector in `data` specifying the dates on which each medicine were dispensed. To use, specify date_dispensing="your_date_of_dispensing_variable_name"
  #' @param selected_pbs_codes optional character vector specifying the PBS codes of interest if EPE is to be calculated for those codes only. If you don't use or specify anything for this argument, the code will still run and use all PBS codes. To use, specify selected_pbs_codes=c("pbs_code1", "pbs_code2", "pbs_code3" etc)
  #' @param max_time_period integer value specifying the maximum permitted number of days between dispensing to be included in the distribution of time periods used to calculate the EPE. Time periods longer than this value will be excluded. Default value is 180 days i.e. if you don' use or specify anything for this argument, the code will still run and use 180 days as this value
  #'
  #' @returns Object of class list, the first element of which is a data.frame containing the EPE values for each PBS code. The second element is a panel of plots for each PBS code visualising the distributions of time between dispensings (after gaps in treatment were identified and excluded).
  #'
  #' @examples
  #' epe(data=your_input_data, id="your_id_variable_name", pbs_code="your_pbs_code_variable_name", date_dispensing="your_dispensing_date_variable_name")
  #'
  #' epe(data=your_input_data, id="your_id_variable_name", pbs_code="your_pbs_code_variable_name", date_dispensing="your_dispensing_date_variable_name",
  #' selected_pbs_codes = c("code1", "code2", "code3")
  #' )
  #'
  #' epe(data=your_input_data, id="your_id_variable_name", pbs_code="your_pbs_code_variable_name", date_dispensing="your_dispensing_date_variable_name",
  #' max_time_period = 120
  #' )
  #'
  #' Example using 'mydata' sample dataset provided with the package
  #' epe(data=mydata, id="PPN", atc_code="ATC_code", pbs_code="PBS", date_dispensing="dispensing_date")
  #'
  #' @references Anton Pottegård and Jesper Hallas, "Assigning exposure duration to single prescriptions by use of the waiting time distribution" (2013) Pharmacoepidemiology and Drug Safety 22(8):803-809. DOI:10.1002/PDS.3459


  # Dependencies: 'tidyverse', 'lubridate'

  suppressWarnings({

    library(tidyverse)
    library(lubridate)

    data$id <- data[, which(names(data)==id)]
    data$pbs_code <- data[, which(names(data)==pbs_code)]
    data$atc_code <- data[, which(names(data)==atc_code)]
    data$date_dispensing <- data[, which(names(data)==date_dispensing)]

    ## checks of input data

    # check the date_dispensing variable is a 'date'

    #if(!is.Date(data$date_dispensing)) {
    #  stop("date_dispensing is not a vector of class 'date'", call. = FALSE)
    #}


    ## if use_75th_percentile is set to true, use the 75th percentile; otherwise use the median

    if(use_75th_percentile==TRUE) {
      percentile <- "75%"
    } else {
      percentile <- "50%"
    }


    ## within each individual and PBS code, order by date then calculate time periods between dispensings

    data <- data %>%
      arrange(id, pbs_code, date_dispensing) %>%
      group_by(id, pbs_code) %>%
      mutate(
        lead_date_dispensing = lead(date_dispensing),
        days_until_next_dispensing = as.numeric(lead_date_dispensing - date_dispensing)
      ) %>%
      ungroup()


    ## exclude missing values and values greater than max_time_period (default is 180)

    data <- data %>%
      filter(!is.na(days_until_next_dispensing) & (days_until_next_dispensing <= max_time_period))


    ## generate table of EPE values for each PBS code

    epe_table <- data %>%
      group_by(pbs_code) %>%
      summarise(
        ATC_code = head(atc_code, n = 1),
        epe_value = quantile(days_until_next_dispensing, na.rm=TRUE)[[percentile]]
      ) %>%
      ungroup()


    ## merge the epe_table onto data to bring in the calculated epe_value (this will help with plotting)

    data <-
      left_join(
        data,
        epe_table,
        by = join_by(pbs_code==pbs_code)
      )


    ## if a character vector was supplied to the argument selected_pbs_codes, filter to those codes (both epe_table and data)

    if(!missing(selected_pbs_codes)) {
      epe_table <- epe_table %>%
        filter(pbs_code %in% selected_pbs_codes)
      data <- data %>%
        filter(pbs_code %in% selected_pbs_codes)
    }



    distribution_plots <-
      ggplot(data, aes(x=days_until_next_dispensing)) +
      geom_histogram(colour="black", fill="grey90") +
      facet_wrap(. ~ pbs_code, scales="free", ncol=4) +
      geom_vline(aes(xintercept=epe_value), linetype="dashed") +
      xlab("Period between dispensing (days)") +
      ylab("Count") +
      theme(legend.position="none") +
      theme_bw()


    ## list object containing epe_table and distribution_plots

    epe_result <- list(epe_table, distribution_plots)
    names(epe_result) <- c("epe_table", "distribution_plot")


    ## print the epe_result list

    return(epe_result)

  })

}

