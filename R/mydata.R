#' Made up toy dataset to practise running 'epe' and 'exposure_period' functions
#'
#' Simulated data to generate known values for the EPE and exposure period
#'
#' @docType data
#'
#' @usage data(mydata)
#'
#' @format `mydata`
#' A data frame with 108 rows and 5 columns:
#' \describe{
#'   \item{PPN}{Individual identifier}
#'   \item{ATC_code}{Anatomical Therapeutic Code for medicine (fictitious code)}
#'   \item{PBS}{Pharmaceutical Benefit Scheme code for medicine (fictitious code)}
#'   \item{dispensing_date}{Date on which medicine was dispensed}
#'   ...
#'
#'  @examples
#'  # load the data
#'  data(mydata)
#'
#'  epe(data=mydata, id="PPN", pbs_code="PBS", date_dispensing="dispensing_date")
#'  # expected EPE is 57 for AB0012 and 60.2 for AC0128 when default of 75th percentile is used
#'
#'  epe_res <- epe(data=mydata, id="PPN", pbs_code="PBS", date_dispensing="dispensing_date")
#'
#'  exposure_period(data=mydata, epe_table=epe_res$epe_table, id="PPN", atc_code="ATC_code", pbs_code="PBS", date_dispensing="dispensing_date")
#'  # A tibble: 8 × 6
#'  #id atc_code exposure_start exposure_end number_dispensings exposure_days
#'  #<int> <chr>    <date>         <date>                    <dbl>         <dbl>
#'  #1     1 x001     2020-01-01     2022-01-18                   18          748
#'  #2     2 x001     2022-06-05     2023-07-02                   10          392
#'  #3     2 x002     2020-09-16     2021-11-23                   10          433.
#'  #4     3 x001     2019-03-14     2020-11-05                   15          602
#'  #5     4 x002     2020-05-29     2022-03-29                   16          669.
#'  #6     5 x002     2018-04-20     2019-06-20                   10          426.
#'  #7     6 x002     2019-10-18     2021-11-03                   16          747.
#'  #8     7 x002     2017-05-31     2018-12-27                   13          575.
#'
"mydata"
