#' Merge country monitoring and evaluation excel tool data (key performance indicators)
#'
#' @param inputdirectory path to folder containing excel tool datasets
#'
#' @param outputdirectory path to folder where merged file is to be saved (must exist already)
#'
#' @param outputname character string to name merged file
#'
#' @param isotomerge 3 letter ISO-code for country of interest, in quotation marks,
#' combine multiple countries with c(...), default is "AFRO" to read in all available.
#'
#' @param wide logical (TRUE/FALSE) of whether the output data frame should be
#' in wide format (default), else will produce long format data
#'
#' @param clean logical (TRUE/FALSE) of whether to clean the output data frame
#'
#' @param variabledict path to dictionary file containing variable definitions.
#' Default is a predefined within the package. For details of how to use your own
#' file see [README](https://github.com/R4IDSR/covidmonitor)
#'
#' @param cleaningdict path to dictionary file containing cleaning rule definitions
#' for country names.
#' Default is a predefined within the package. For details of how to use your own
#' file see [README](https://github.com/R4IDSR/covidmonitor)
#'
#' @importFrom rio import export
#' @importFrom tidyr fill pivot_wider
#' @importFrom matchmaker match_vec
#' @importFrom dplyr bind_rows case_when select_if mutate arrange
#'
#' @author Alice Carr, Alex Spina
#' @export

merge_kpi <- function(inputdirectory,
                      isotomerge = "AFRO",
                      outputdirectory = tempdir(),
                      outputname = "kpi_merged_",
                      wide = TRUE,
                      clean = TRUE,
                      variabledict = system.file("extdata", "mne_dictionary.xlsx", package = "covidmonitor"),
                      cleaningdict = system.file("extdata", "cleaning_dictionary.xlsx", package = "covidmonitor")
                      ) {

  # Read in file list. Creat output directory.
  files <- list.files(path = inputdirectory,
                            full.names = TRUE,
                            recursive = TRUE)

  # drop files we dont want
  # those that have not been assigned a name yet and the combined.csv
  files <- files[-grep("---.|Combined.|CombinedKPIDataFile|!", files)]


  if (length(files) == 0) {
    stop("No files found, check the path used for inputdirectory")
  }

  ## subset files to read-in if isotomerge is not "AFRO"
  if (!"AFRO" %in% isotomerge) {

    ## combine all the countries in to one string seperated with OR
    lookers <- paste(paste0("/", isotomerge, ".KPI"), collapse = "|")

    ## subset file list based on search strings
    files <- files[grepl(lookers, files)]

  }

  ## chuck an error if ISO Code wrong
  if (length(files) == 0) {
    stop("No files found, check the country codes are correct (3 letters)")
  }


  # read in dictionary for renaming variables
  var_dict <- rio::import(
    variabledict,
    which = "var_defs"
  )

  # read in dictionary for checking comments empty or not
  comment_dict <- rio::import(
    variabledict,
    which = "comment_refs"
  )



  # create an empty list to fill in with datasets
  output <- list()

  # for each file listed
  for (f in 1:length(files)) {


    # read in excel sheet of interest
      # try "Data fields" tab and if error than try "Saisie des donees"
      og_sheet <- suppressMessages(
        tryCatch(expr = {rio::import(files[f], which = "Data fields",
                                               col_names = FALSE)},
               error = function(e){
                 rio::import(files[f], which = "Saisie des données",
                             col_names = FALSE)})
      )


      ## only keep first 8 columns
      og_sheet <- og_sheet[ , 1:8]

      ## temporarily rename columns to be workable
      colnames(og_sheet) <- paste0("X", 1:8)

      ## define language
      lang <- dplyr::case_when(
        grepl("Pays", og_sheet$X1[1])   ~   "french",    ## weird character somewhere means cant just use ==
        og_sheet$X1[1] == "Country:"    ~   "english"
      )


      ## dictionary recode variable names (once so dont have to repeat lower down)
      og_sheet$X1 <- matchmaker::match_vec(og_sheet$X1,
                                           dictionary = var_dict,
                                           from = paste0(lang, "_label"),
                                           to = "var_name")

      ## manually recode variable names with spelling errors
      og_sheet <- dplyr::mutate(og_sheet,
                                X1 = dplyr::case_when(
                                  X1 == "Total approved national COVID-19 response budget ($)"           ~ "budget_covid",
                                  X1 == "Amount of approved budget national budget utilized to date ($)" ~ "budget_total",
                                  X1 == "Total WCO response funding ($)"                                 ~ "intervention_funding",
                                  X1 == "Amount of WCO response funding utilized to date ($)"            ~ "intervention_funding_total",
                                  X1 == "Anticipated total IMST staff needed"                            ~ "esgi_status",
                                  X1 == "Number of ntational staff in IMST"                              ~ "esgi_national_staff",
                                  X1 == "Indicateur d’exécution n° 5 : Pourcentage du personnel de l’ESGI du bureau de pays déployé dans les ESGI décentralisées ou les soutenant" ~ "indicator_5",
                                  X1 == "Number of points of entry with  screening, isolation facilities and referral system for COVID-19"  ~ "entry_points_covid",
                                  X1 == "Nuimber of points of entry with  screening, isolation facilities and referral system for COVID-19" ~ "entry_points_covid",
                                  X1 == "Nombre de cas contacts signalés au cours des sept derniers jours"  ~ "new_cases_reported_total",
                                  X1 == "Nombre de cas contacts signalés ayant fait l’objet d’une investigation dans les 24 heures au cours des sept derniers jours" ~ "reported_cases_investigated_24h",
                                  X1 == "Number of contacts identified during the last 7 days"            ~ "cum_num_contacts",
                                  X1 == "Total number of contacts under follow-up during the last 7 day"  ~ "contacts_followed_7days",
                                  X1 == "Total number of contacts under follow-up during the last 7 days" ~ "contacts_followed_7days",
                                  X1 == "Total number of contacts under follow-up"                        ~ "contacts_followed_total",
                                  X1 == "Number of laboratory results made available within 48 hours for samples collected within the last two days" ~ "lab_results_communicated",
                                  grepl("Cumulative number of tests per 10 000 population performed at the end of the previous week",
                                        og_sheet$X1)                                                      ~ "cum_num_tests_prev_week",
                                  grepl("Cumulative number of tests per 10 000 population performed at the end of the current week",
                                        og_sheet$X1)                                                      ~ "cum_num_tests_curr_week",
                                  X1 == "Indicateur d’exécution n° 15 : Taux d’occupation actuel des lits par les cas suspects (en pourcentage)" ~ "indicator_15",
                                  X1 == "Performance Indicator 17: Bed occupancy rate for confirmed cases at present" ~ "indicator_17",
                                  X1 == "Indicateur d’exécution n° 19 : Taux de létalité"                 ~ "indicator_19",
                                  X1 == "Indicateur d’exécution n° 23 : Taux de létalité des cas confirmés signalés au cours des sept derniers jours" ~ "indicator_23",
                                  X1 == "Estimate of percentageof individuals reached by RCCE activity (e.g. CHW  trainings, health worker trainings, hotline usage )" ~ "rcce_outreach_percent",
                                  X1 == "Estimate of percentageof individuals reached by RCCE activity (e.g. CHW  trainings, health worker trainings, hotline usage)"  ~ "rcce_outreach_percent",
                                  grepl("Number of consultations in primary health facilities and prenatal clinics during the last month ",
                                        og_sheet$X1)                                                      ~ "primare_care_consultations_month",
                                  grepl("Number of consultations in primary health facilities and prenatal clinics during the same month in 2019",
                                        og_sheet$X1)                                                      ~ "primare_care_consultations_month_2019",
                                  grepl("Number of surviving infants receivig third dose of DPT-containing vaccine during the last month",
                                        og_sheet$X1)                                                      ~ "infant_survival_d_t_p_month",
                                  grepl("Number of surviving infants receivig third dose of DPT-containing vaccine during the same month",
                                        og_sheet$X1)                                                      ~ "infant_survival_d_t_p_month_2019",
                                  X1 == "Performance Indicator 28: % of change in surviving infants receiving third dose of DPT-containing vaccine" ~ "indicator_28",
                                  grepl("Number of OPD attendance during the last month",
                                        og_sheet$X1)                                                      ~ "outpatients_month",
                                  grepl("Nombre de patients en soins ambulatoires au cours du mois dernier",
                                        og_sheet$X1)                                                      ~ "outpatients_month",
                                  grepl("Number of OPD attendance during the same month in",
                                        og_sheet$X1)                                                      ~ "outpatients_month_2019",
                                  grepl("Number of people living with HIV in target area who received ART in the current week",
                                        og_sheet$X1)                                                      ~ "hiv_treated_month",
                                  grepl("Number of people living with HIV in target area who received ART during the same week in 2019",
                                        og_sheet$X1)                                                      ~ "hiv_treated_month_2019",
                                  TRUE ~ X1
                                )
                         )





      ## pulling separate bits apart

      # identifiers: country, date of report, week
      # find anchors for bit of interest
      iden_start  <- grep("country", og_sheet$X1)[1]

      iden_stop   <- grep("epi_week", og_sheet$X1)

      # subset rows of interest based on anchors
      identifiers <- og_sheet[iden_start:iden_stop, ]

      # yes_nos: the line on data collection / summarisation
      yes_no_start <- grep("submitted_contaminaion_route", og_sheet$X1)

      # flip to long
      yes_nos <- data.frame(
        # pull od columns (with var)
        t(og_sheet[yes_no_start, c(TRUE, FALSE)]),
        # pull even columns (with response)
        t(og_sheet[yes_no_start, c(FALSE, TRUE)]),
        row.names = NULL)
      # fix names
      names(yes_nos) <- c("X1", "X2")
      # drop missings
      yes_nos <- yes_nos[!is.na(yes_nos$X1), ]
      # drop note on reporting frequency
      yes_nos <- yes_nos[1:3, ]


      ## recode yes/nos with language (cant do further up because yes/nos in long format previously)
      yes_nos$X1 <- matchmaker::match_vec(yes_nos$X1,
                                          dictionary = var_dict,
                                          from = paste0(lang, "_label"),
                                          to = "var_name")


      # cut_offs: the cut off values for evaluation of indicators
      cut_off_start <- grep(
        "Évaluation des indicateurs d’exécution|Performance Indicator Assessment",
                            og_sheet$X1)
      cut_off_stop <- nrow(og_sheet)
      cut_offs <- og_sheet[cut_off_start:cut_off_stop, ]


      # indicators: the main data we are interested in
      indi_start <- grep("Coordination et gestion des incidents|Coordination & Incident Management",
                         og_sheet$X1)
      indi_stop  <- grep("indicator_31", og_sheet$X1)
      table_sheet1 <- og_sheet[indi_start:indi_stop, ]

      # fill-in missing rows from above (indicator names for merged cells)
      table_sheet1 <- tidyr::fill(table_sheet1, X1)


      ## reshaping indicator data

      # pull the extra giblets together if leaving in long format
      if (!wide) {
        # indicator grouping variables for later
        table_sheet1$grps <- table_sheet1$X1
        table_sheet1$grps[!grepl("Réponse|Response", table_sheet1$X6)] <- NA
        table_sheet1$grps[grep("_", table_sheet1$grps)] <- NA
        # fill-in groups for use later
        table_sheet1 <- tidyr::fill(table_sheet1, grps)
      }

      # drop rows with "Reponse"
      table_sheet1 <- table_sheet1[!grepl("Réponse|Response", table_sheet1$X6), ]

      # drop empty columns
      table_sheet1 <- dplyr::select_if(table_sheet1,
                                       ~!all(is.na(.)))

      ## Dealing with the observation columns (free text comments)

      # set those that are same same as dictionary to NA
      table_sheet1$X8[table_sheet1$X8 %in% comment_dict[, paste0(lang, "_comment")]] <- NA


      ## fix which variables we want to keep and the order

      ## define the list of variables from the dictionary that we are interested in
      indis_of_interest <- var_dict$var_name
      indis_of_interest <- indis_of_interest[!indis_of_interest %in% c("country", "date", "epi_week")]
      indis_of_interest <- indis_of_interest[!grepl("observation_", indis_of_interest)]

      ## find which ones are missing
      missings <- indis_of_interest[!indis_of_interest %in% table_sheet1$X1]
      # ignore the yes/nos for this, but keep in indis_of_interest so can use for selecting after
      missings <- missings[!missings %in% c("submitted_contaminaion_route",
                                            "updated_submitted_tracing",
                                            "presentation_covid")]

      ## add in any missing variables
      if (length(missings) > 0) {

        # different number of columns for long/wide (long has groups added)
        colnums <- ifelse(wide, 3, 4)

        # create a matrix missing variables and appropriate empties
        temp_adder <- matrix(c(missings,
                               rep.int(NA, length(missings) * colnums)),
                             ncol = colnums + 1)

        # make column names fit
        colnames(temp_adder) <- names(table_sheet1)

        # combine with indicator dataframe
        table_sheet1 <- rbind(table_sheet1, temp_adder)

      }

      ## get which vars were dropped or added
      droppers <- table_sheet1$X1[!which(table_sheet1$X1 %in% indis_of_interest)]
      doubles  <- table_sheet1$X1[duplicated(table_sheet1$X1)]

      ## chuck a warning if any of those happened
      if (length(missings) > 0 | length(droppers) > 0 | length(doubles) > 0) {
        warning(
            paste0(identifiers$X2[1], " W", identifiers$X2[3],                            "| ",
            length(missings), " missings added: ",        paste0(missings, sep = ", "),   "| ",
            length(droppers), " non-template dropped: ",  paste0(droppers, sep = ", "),   "| ",
            length(doubles),  " duplicates dropped: ",    paste0(doubles, sep = ", ")),
          call. = FALSE
        )

      }


      ## only keep indicators of interest
      table_sheet1 <- table_sheet1[which(table_sheet1$X1 %in% indis_of_interest), ]

      ## drop duplicates from X1 (issue from niger - merged X1 row that splits in X2)
      table_sheet1 <- table_sheet1[!duplicated(table_sheet1$X1), ]

      ## arrange indicators (rows) according to template
      table_sheet1 <- dplyr::arrange(table_sheet1,
                                     match(X1, indis_of_interest))


############# pull together the wide version of data set
      if (wide) {

        # define which rows to keep based on dictionary
        obs_rows <- which(!is.na(comment_dict[ , paste0(lang, "_comment")]))

        # pull together observations in an appropriately named dataframe
        observations <- data.frame(
          # add x1 column with var names
          "X1" = paste0("observations_", 1:length(obs_rows)),
          # only keep the rows defined in the dictionary
          "X2" = table_sheet1$X8[obs_rows]
        )

        # only keep indicators and obs
        table_sheet1 <- table_sheet1[ , c(1, 2)]
        names(table_sheet1) <- c("X1", "X2")

        # bind rows of different bits want
        upload <- rbind(
          identifiers[ , 1:2],
          table_sheet1,
          yes_nos,
          observations
        )

        # flip to wide format
        upload <- tidyr::pivot_wider(upload,
                                     names_from = X1,
                                     values_from = X2,
                                     names_repair = "minimal")

        # change to date
        upload$date <- as.Date(
          as.numeric(upload$date),
          origin = "1899-12-30")


        # choose all the indicator columns
        indicator_cols <- paste0("indicator_",
                                 sort(
                                   c(
                                   7, 9:11, 15, 17, 18, 24,
                                   6, 19:21,
                                   12:13,
                                   14, 16,
                                   27:30)
                                   ))

        # for each indicator column create appropriate evaluation column
        for (i in indicator_cols) {

          # make in to a numeric variable
          upload[ , i] <- suppressWarnings(as.numeric(upload[ , i]))

          # define name of an evaluation column
          eval_col <- paste0("eval_", i)

          # create an empty eval column
          upload[ , eval_col] <- NA_character_

          # choose indicators to work on
          if (i %in% paste0("indicator_", c(7, 9:11, 15, 17, 18, 24))) {

            # define groups
            upload[!is.na(upload[[i]]) &
                            upload[[i]] >= 90,
                   eval_col] <- "Good"
            upload[!is.na(upload[[i]]) &
                     upload[[i]] >= 80 & upload[[i]] <= 89,
                   eval_col] <- "Acceptable"
            upload[!is.na(upload[[i]]) &
                     upload[[i]] < 80,
                   eval_col] <- "Poor"
          }

          if (i %in% paste0("indicator_", c(6, 19:21))) {
            # define groups
            upload[!is.na(upload[[i]]) &
                     upload[[i]] < 5,
                   eval_col] <- "Good"
            upload[!is.na(upload[[i]]) &
                     upload[[i]] >= 5 & upload[[i]] <= 10,
                   eval_col] <- "Acceptable"
            upload[!is.na(upload[[i]]) &
                     upload[[i]] > 10,
                   eval_col] <- "Poor"

          }

          if (i %in% paste0("indicator_", c(12:13))) {
            # define groups
            upload[!is.na(upload[[i]]) &
                     upload[[i]] > 60,
                   eval_col] <- "Good"
            upload[!is.na(upload[[i]]) &
                     upload[[i]] >= 40 & upload[[i]] <= 60,
                   eval_col] <- "Acceptable"
            upload[!is.na(upload[[i]]) &
                     upload[[i]] < 40,
                   eval_col] <- "Poor"
          }

          if (i %in% paste0("indicator_", c(14, 16))) {
            # define groups
            upload[!is.na(upload[[i]]) &
                     upload[[i]] > 40,
                   eval_col] <- "Good"
            upload[!is.na(upload[[i]]) &
                     upload[[i]] >= 20 & upload[[i]] <= 40,
                   eval_col] <- "Acceptable"
            upload[!is.na(upload[[i]]) &
                     upload[[i]] < 20,
                   eval_col] <- "Poor"
          }

          if (i %in% paste0("indicator_", c(27:30))) {
            # define groups
            upload[!is.na(upload[[i]]) &
                     upload[[i]] >= 0,
                   eval_col] <- "Good"
            upload[!is.na(upload[[i]]) &
                     upload[[i]] >= -5 & upload[[i]] < 0,
                   eval_col] <- "Acceptable"
            upload[!is.na(upload[[i]]) &
                     upload[[i]] < -5,
                   eval_col] <- "Poor"
          }
        }



        # save data in list
        output[[f]] <- upload

      } else {
########### if keeping in long format

        # add in extra cols for yes_nos
        yes_nos <- cbind(yes_nos, NA, NA , "Submitted documents")

        # rename yes_nos to bind
        names(yes_nos) <- names(table_sheet1)

        # combine with indicator table
        table_sheet1 <- rbind(table_sheet1, yes_nos)

        # add in identifier vars
        table_sheet1$country <- identifiers[1, 2]
        table_sheet1$date <- as.Date(
          as.numeric(identifiers[2, 2]),
          origin = "1899-12-30")
        table_sheet1$epi_week <- as.numeric(identifiers[3, 2])

        # recode group vars based on dictionary
        table_sheet1$grps <- matchmaker::match_vec(table_sheet1$X1,
                                                    dictionary = var_dict,
                                                    from = "var_name",
                                                    to = "grp_type")


        # define which indicators are character responses and date responses
        char_vars <- var_dict$var_name[
          var_dict$var_type %in% c("yn_vars", "ynp_vars")]
        dat_vars  <- var_dict$var_name[
          var_dict$var_type %in% c("date_vars")]


        # copy indicator responses in to new columns
        table_sheet1$num_vars <- table_sheet1$X6
        table_sheet1$str_vars <- table_sheet1$X6
        table_sheet1$dat_vars <- table_sheet1$X6

        # set appropriate ones to empty
        table_sheet1$num_vars[table_sheet1$X1 %in% c(char_vars, dat_vars)] <- NA
        table_sheet1[!table_sheet1$X1 %in% char_vars,
                     "str_vars"] <- NA
        table_sheet1[!table_sheet1$X1 %in% dat_vars,
                     "dat_vars"] <- NA

        # remove combined variable
        table_sheet1 <- table_sheet1[, -2]

        # make numeric indicators
        table_sheet1$num_vars <- suppressWarnings(as.numeric(table_sheet1$num_vars))

        # fix names
        names(table_sheet1) <- c("indicator",
                                 "reporting_frequency",
                                 "observations",
                                 "grps",
                                 "country",
                                 "date",
                                 "epi_week",
                                 "num_vars",
                                 "str_vars",
                                 "dat_vars")

        ## define evaluation variable
        table_sheet1$evaluation <- NA

        # choose which rows to work on
        indicator_rows <- paste0("indicator_", c(7, 9:11, 15, 17, 18, 24))

        # assign values
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                        table_sheet1$num_vars >= 90] <- "Good"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars >= 80 &
                                  table_sheet1$num_vars <= 89] <- "Acceptable"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars < 80] <- "Poor"

        # choose which rows to work on
        indicator_rows <- paste0("indicator_", c(6, 19:21))

        # assign values
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars < 5] <- "Good"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars >= 5 &
                                  table_sheet1$num_vars <= 10] <- "Acceptable"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars > 10] <- "Poor"

        # choose which rows to work on
        indicator_rows <- paste0("indicator_", c(12:13))

        # assign values
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars > 60] <- "Good"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars >= 40 &
                                  table_sheet1$num_vars <= 60] <- "Acceptable"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars < 40] <- "Poor"

        # choose which rows to work on
        indicator_rows <- paste0("indicator_", c(14, 16))

        # assign values
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars > 40] <- "Good"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars >= 20 &
                                  table_sheet1$num_vars <= 40] <- "Acceptable"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars < 20] <- "Poor"
        # choose which rows to work on
        indicator_rows <- paste0("indicator_", c(27:30))

        # assign values
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars >= 0] <- "Good"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars >= -5 &
                                  table_sheet1$num_vars < -0] <- "Acceptable"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars < -5] <- "Poor"

        # save data in list
        output[[f]] <- table_sheet1

      }


  }


  ## pull together a single dataset
  output <- dplyr::bind_rows(output)

  if (clean) {

    ## read in cleaning dictionary (for countries)
    clean_dict <- rio::import(
      cleaningdict,
      which = "country"
    )

    ## run the cleaning function
    output <- clean_kpi(inputfile = output,
                        var_dict = var_dict,
                        clean_dict = clean_dict,
                        wide = wide)
  }

  ## output file
  # define path to output to
  filename <- paste0(outputdirectory,"/", outputname, Sys.Date(), ".xlsx")
  # write file
  rio::export(output, file = filename)

  # return dataframe
  output
}
