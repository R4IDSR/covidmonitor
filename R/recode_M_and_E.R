#' Recode country monitoring and evaluation excel tool data to fit
#' new ODK based tool (key performance indicators)
#'
#' @param inputfile a data frame produced by the merge_kpi function
#'
#' @importFrom rio import
#' @importFrom matchmaker match_vec
#' @importFrom dplyr rename
#'
#' @author Alice Carr, Alex Spina

recode_kpi <- function(inputfile) {

  ## read in tamayi's recode dictionary
  recode_dict <- rio::import(
    system.file("extdata", "mne_dictionary.xlsx",
                package = "covidmonitor"),
    which = 3
  )


  ## recode the names of input file back to the full names
  names(inputfile) <- matchmaker::match_vec(
    names(inputfile),
    ## vardict is read in via the merge_kpi function
    dictionary = var_dict,
    from = "var_name",
    to = "english_label"
  )


  ## recode the names to ODK names
  names(inputfile) <- matchmaker::match_vec(
    names(inputfile),
    dictionary = recode_dict[which(!is.na(recode_dict$Description) &
                                            !is.na(recode_dict$FullFieldName)), ],
    from = "Description",
    to = "FullFieldName"
  )

## TODO MEASLES IS WRONG FIX TO THE DPT VARIABLE
  ## catch the ones missed
  inputfile <- rename(inputfile,
                      "cpm_C1i0_group_cpm/cpm_C1i14_n_staffimsInCountry" = "Number of national staff in IMST",
                      "poe_C4i0_group_poe/poe_C4i2_n_poeDesignated" = "Number of points of entry with screening, isolation facilities and referral system for COVID-19",
                      "cmg_C7i0_group_cmg/cmg_C7i11_i_bedOccupancyRate" = "Performance Indicator 17: Bed occupancy rate for confirmed cases  at present",
                      "cehs_C11i0_group_essentials/cehs_C11i19_n_numSurvivingInfant3rdMeaslesVacc" = "Number of surviving infants receiving third dose of DPT-containing vaccine during the last month",
                      "cehs_C11i0_group_essentials/cehs_C11i19_d_numSurvivingInfant_3rdMeaslesVacc2019" = "Number of surviving infants receiving third dose of DPT-containing vaccine during the same month in 2019",
                      "cehs_C11i0_group_essentials/cehs_C11i21_n_numARTDispensed" = "Number of people living with HIV in target area who received ART in the current week (hasta Agosto 2020)"

                      )


}
