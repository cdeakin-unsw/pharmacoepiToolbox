#####
## Exposure period function
## written by Claire Deakin 2023-07-05

exposure_period <- function(data,
                            epe_table,
                            id,
                            atc_code,
                            pbs_code,
                            date_dispensing,
                            gap_as_epe_multiplier = TRUE,
                            gap_days = NULL,
                            epe_gap_multiplier=3) {

  #' @title Function to calculate the time period an individual has been exposed to a dispensed medicine
  #' @author Claire Deakin
  #' @description
  #' Uses dispensing dates and the output from the `epe` function to calculate the exposure period in days to a given medicine, based on ATC codes and the EPE value calculated for each PBS code.
  #' The exposure period is the number of days from the first date of dispensing until 1x EPE after the last date of dispensing. Gaps in dispensing are excluded from this period based on a default of 3x EPE days, however, other values can be specified e.g. a number of days or a different integer multiplier of the EPE
  #'
  #' @param data data.frame containing input data; check your input data is a data.frame and not a tibble, coerce to data.frame if necessary. To use, specify data=your_input_data
  #' @param epe_table data.frame object containing PBS codes and EPE values e.g. if output from `epe` function stored as epe_res, use epe_table = epe_res$epe_table to extract the epe_table from the list object
  #' @param id name of character vector in `data` specifying a unique identifier for individual people/patients (can handle integer variables). To use, specify id="your_id_variable_name"
  #' @param atc_code name of character vector in `data` specifying the ATC code for the medicine individuals have been exposed to. Note, if there are multiple ATC codes for a given medicine, you may wish to use the medicine name instead so long as it is a unique identifier for the medicine. To use, specify atc_code="your_atc_code_variable_name"
  #' @param pbs_code name of character vector in `data` specifying the PBS code for the medicine. To use, specify pbs_code="your_pbs_code_variable_name"
  #' @param date_dispensing name of date vector in `data` specifying the dates on which each medicine were dispensed. To use, specify date_dispensing="your_date_of_dispensing_variable_name"
  #' @param gap_as_epe_multiplier argument to specify whether a gap in treatment will be identified based on being an integer value multiplied by the EPE, with that integer value specified in the epe_gap_multiplier argument. Defaults to TRUE
  #' @param gap_days optional integer value to use if gap_as_epe_multiplier = FALSE, to specify the number of days to use as a gap a gap isn't defined as an integer value multipled by the EPE
  #' @param epe_gap_multiplier optional argument to specify the integer to multiply the EPE by to define a gap. Defaults to 3 if no value is given by the user
  #'
  #' @returns Object of class data.frame containing the id, atc_code, exposure_start, exposure_end, number_dispensings and exposure_days
  #'
  #' @examples
  #' exposure_period(data = your_input_data,
  #' epe_table = epe_res$epe_table,
  #' id = "your_id_variable_name",
  #' atc_code = "your_atc_code_variable_name",
  #' pbs_code = "your_pbs_code_variable_name"
  #' date_dispensing = "your_dispensing_date_variable_name"
  #' )
  #'
  #' exposure_period(data = your_input_data,
  #' epe_table = epe_res$epe_table,
  #' id = "your_id_variable_name",
  #' atc_code = "your_atc_code_variable_name",
  #' pbs_code = "your_pbs_code_variable_name",
  #' date_dispensing = "your_dispensing_date_variable_name"
  #' epe_gap_multiplier = 5
  #' )
  #'
  #' exposure_period(data = your_input_data,
  #' epe_table = epe_res$epe_table,
  #' id = "your_id_variable_name",
  #' atc_code = "your_atc_code_variable_name",
  #' pbs_code = "your_pbs_code_variable_name",
  #' date_dispensing = "your_dispensing_date_variable_name"
  #' gap_as_epe_multiplier = FALSE,
  #' gap_days = 120
  #' )
  #'
  #' @examples using 'mydata'
  #' epe_table <- epe(data=mydata, id="PPN", pbs_code="PBS", date_dispensing="dispensing_date")
  #' exposure_period(data=mydata, epe_table=epe_res$epe_table, id="PPN", atc_code="ATC_code", pbs_code="PBS", date_dispensing="dispensing_date")
  #'
  #' @references Anton Pottegård and Jesper Hallas, "Assigning exposure duration to single prescriptions by use of the waiting time distribution" (2013) Pharmacoepidemiology and Drug Safety 22(8):803-809. DOI:10.1002/PDS.3459


  # Dependencies: 'tidyverse', 'lubridate'

  suppressWarnings({

    library(tidyverse)
    library(lubridate)

    temp_dat <- data
    temp_epe_table <- epe_table
    temp_dat$id <- data[, which(names(data)==id)]
    temp_dat$atc_code <- data[, which(names(data)==atc_code)]
    temp_dat$pbs_code <- data[, which(names(data)==pbs_code)]
    temp_dat$date_dispensing <- data[, which(names(data)==date_dispensing)]


    ## checks of input data

    if(!is.Date(temp_dat$date_dispensing)) {
      stop("date_dispensing is not a vector of class 'date'", call. = FALSE)
    }

    if(gap_as_epe_multiplier == FALSE & missing(gap_days)) {
      stop("must supply a value for 'gap_days' if gap_as_epe_multiplier == FALSE", call. = FALSE)
    }

    ## check temp_dat is ordered by id then date_dispensing
    temp_dat <- temp_dat %>%
      arrange(id, date_dispensing)


    ## merge the epe_table with temp on pbs_code to bring the epe_value variable into temp_dat
    temp_dat <- left_join(
      x=temp_dat,
      y=temp_epe_table,
      by=join_by(pbs_code == pbs_code)
    )


    ## calculate gap_size based on epe_gap_multiplier * epe_value (epe_gap_multiplier has default value of 3)
    temp_dat <- temp_dat %>% mutate(gap_size = epe_gap_multiplier * epe_value)


    ## if gap_as_epe_multiplier == FALSE and a value was specified for gap_days, repace gap_size with that gap_days value
    if(gap_as_epe_multiplier == FALSE & !missing(gap_days)) {
      temp_dat <- temp_dat %>%
        mutate(gap_size = rep(gap_days, nrow(temp_dat)))
    }


    ## group temp_dat by id and then atc_code, add index variable
    ## then add variable for lead_date_dispensing to represent the next date of dispensing following each record
    ## calculate the difference in days between lead_date_dispensing and date_dispensing
    ## make a flag variable to indicate whether this difference is > gap_size
    ## calculate the cumulative sum of gap_flag to use for filtering (the first record >0 is the first gap to be excluded i.e. keep all gap_flag==0)
    ## add epe_value to the dispensing dates (so we can extract the last dispensing date plus EPE based on filtering cumsum==0 and max of index)
    temp_dat <- temp_dat %>%
      group_by(id, atc_code) %>%
      mutate(index = c(1:length(id)),
             lead_date_dispensing  = lead(date_dispensing ),
             date_diff = as.numeric(lead_date_dispensing  - date_dispensing ),
             gap_flag = if_else((date_diff > gap_size),
                                1,
                                0
             ),
             cumsum_gap_flag = cumsum(gap_flag),
             date_dispensing_plus_epe = (date_dispensing + epe_value)
      ) %>%
      ungroup()


    ## make a temp_dat_summary_1 dataframe for individuals with >2 dispensing
    ## group by id then atc_code again
    ## then filter on cumsum_gap_flag==0 and max(index) > 2
    ## summarise (for each id and atc_code) the exposure_start as the first date_dispensing, the exposure_end as the last date_dispensing_plus_epe and the number_dispensings
    ## note that we add 1 to the index to calculate the number of dispensings because filtering on cumsum_gap_flag==0 excludes the last included date of dispensing (and this entry has cumsum_gap_flag==NA)
    ## calculate the exposure_days as exposure_end minus exposure_start
    temp_dat_summary_1 <- temp_dat %>%
      group_by(id, atc_code) %>%
      filter(cumsum_gap_flag == 0 & max(index) > 2) %>%       # arguably the cumsum_gap_flag == 0 is redundant when filtering on max(index) > 2 but keep it in for now
      summarise(
        exposure_start = head(date_dispensing, n=1),
        exposure_end = tail(date_dispensing_plus_epe, n=1),
        number_dispensings = (tail(index, n=1) + 1),
        exposure_days = as.numeric(exposure_end - exposure_start)
      ) %>%
      ungroup()


    ## make a temp_dat_summary_2 dataframe for individuals with 1 dispensing (including those with 2 dispensings but a gap > gap_size)
    ## group by id then atc_code again
    ## again summarise (for each id and atc_code) the exposure_start as the first date_dispensing, the exposure_end as the FIRST date_dispensing_plus_epe and the number_dispensings
    ## note that the number_dispensings and exposure_end use head here rather than tail because the true number of dispensings without a gap is 1
    ## calculate the exposure_days as exposure_end minus exposure_start
    temp_dat_summary_2 <- temp_dat %>%
      group_by(id, atc_code) %>%
      filter(max(index) == 1 | (max(index) == 2 & head(cumsum_gap_flag, n=1) == 1)) %>%
      summarise(
        exposure_start = head(date_dispensing, n=1),
        exposure_end = head(date_dispensing_plus_epe, n=1),
        number_dispensings = head(index, n=1),
        exposure_days = as.numeric(exposure_end - exposure_start)
      ) %>%
      ungroup()


    ## make a temp_dat_summary_3 dataframe for individuals with 2 dispensing (NOT including those with 2 dispensings but a gap > gap_size i.e. no flagged gap so gap_flag==0)
    ## group by id then atc_code again
    ## again summarise (for each id and atc_code) the exposure_start as the first date_dispensing, the exposure_end as the last date_dispensing_plus_epe and the number_dispensings
    ## note that we don't need to add 1 to the index to calculate the number of dispensings because we did not filter on cumsum_gap_flag==0
    temp_dat_summary_3 <- temp_dat %>%
      group_by(id, atc_code) %>%
      filter((max(index) == 2 & head(cumsum_gap_flag, n=1) == 0)) %>%
      summarise(
        exposure_start = head(date_dispensing, n=1),
        exposure_end = tail(date_dispensing_plus_epe, n=1),
        number_dispensings = (tail(index, n=1)),
        exposure_days = as.numeric(exposure_end - exposure_start)
      )


    ## combine temp_dat_summary_1, temp_dat_summary_2 and temp_dat_summary_3 as temp_dat_summary
    ## order by id then atc_code
    temp_dat_summary <- rbind.data.frame(temp_dat_summary_1, temp_dat_summary_2, temp_dat_summary_3)
    temp_dat_summary <- temp_dat_summary %>% arrange(id, atc_code)


    ## return temp_dat_summary

    return(temp_dat_summary)

  })

}

