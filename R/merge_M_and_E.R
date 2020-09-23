#' Merge weekly report monitoring and evaluation data.
#' Currently based on Guinea template; to be further in case of non-templated
#' documents
#'
#' @param inputdirectory path to folder containing datasets
#'
#' @param outputdirectory path to folder where merged file is to be saved
#'
#' @param outputname character string to name merged file
#'
#' @param template logical (TRUE/FALSE) of whether the input data frame fits
#' the standard template
#'
#' @param wide logical (TRUE/FALSE) of whether the output data frame should be
#' in wide format (default), else will produce long format data
#'
#' @importFrom rio import export
#' @importFrom tidyr fill pivot_wider
#' @importFrom matchmaker match_vec
#' @importFrom dplyr bind_rows
#'
#' @author Alice Carr, Alex Spina
#' @export



merge_mne <- function(inputdirectory,
                     outputdirectory = tempdir(),
                     outputname = "filename",
                     template = TRUE,
                     wide = TRUE) {

  # Read in file list. Creat output directory.
  files <- base::list.files(path = inputdirectory, full.names = TRUE)

  # create folder for output
  base::dir.create(outputdirectory, showWarnings = FALSE)


  # read in dictionary for renaming variables
  var_dict <- rio::import(
    system.file("extdata", "mne_dictionary.xlsx",
                package = "covidmonitor"),
    which = 1
  )

  # read in dictionary for checking comments empty or not
  comment_dict <- rio::import(
    system.file("extdata", "mne_dictionary.xlsx",
                package = "covidmonitor"),
    which = 2
  )


  # create an empty list to fill in with datasets
  output <- list()

  # for each file listed
  for (f in 1:base::length(files)) {

    # read in excel sheet of interest
    og_sheet <- rio::import(files[f], which = 2,
                                col_names = paste0("X", 1:8))


    ## for those fitting the standard template
    if (template) {

      ## pulling separate bits apart

      # identifiers: country, date of report, week
      # find anchors for bit of interest
      iden_start  <- grep("Pays", og_sheet$X1)
      iden_stop   <- grep("Semaine épidémiologique", og_sheet$X1)
      # subset rows of interest based on anchors
      identifiers <- og_sheet[iden_start:iden_stop, ]

      # yes_nos: the line on data collection / summarisation
      yes_no_start <- grep("Liste des cas soumise par filière de contamination",
                           og_sheet$X1)
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


      # cut_offs: the cut off values for evaluation of indicators
      cut_off_start <- grep("Évaluation des indicateurs d’exécution",
                            og_sheet$X1)
      cut_off_stop <- nrow(og_sheet)
      cut_offs <- og_sheet[cut_off_start:cut_off_stop, ]


      # indicators: the main data we are interested in
      indi_start <- grep("Coordination et gestion des incidents", og_sheet$X1)
      indi_stop  <- grep("Indicateur d’exécution n° 31", og_sheet$X1)
      table_sheet1 <- og_sheet[indi_start:indi_stop, ]

      # fill-in missing rows from above (indicator names for merged cells)
      table_sheet1 <- tidyr::fill(table_sheet1, X1)


      ## reshaping indicator data


      # pull the extra giblets together if leaving in long format
      if (!wide) {
        # indicator grouping variables for later
        table_sheet1$grps <- table_sheet1$X1
        table_sheet1$grps[!grepl("Réponse", table_sheet1$X6)] <- NA
        table_sheet1$grps[grep("Indicateur|Date|Estimation",
                               table_sheet1$grps)] <- NA
        # fill-in groups for use later
        table_sheet1 <- tidyr::fill(table_sheet1, grps)
      }

      # drop rows with "Reponse"
      table_sheet1 <- table_sheet1[!grepl("Réponse", table_sheet1$X6), ]

      # drop empty columns
      table_sheet1 <- table_sheet1[ , -c(2:5)]

      ## Dealing with the observation columns (free text comments)

      # set those that are same same as dictionary to NA
      table_sheet1$X8[table_sheet1$X8 == comment_dict$french_comment] <- NA


      ## pull together the wide version of data set
      if (wide) {

        # define which rows to keep based on dictionary
        obs_rows <- which(!is.na(comment_dict$french_comment))

        # pull together observations in an appropriately named dataframe
        observations <- data.frame(
          # add x1 column with var names
          "X1" = paste0("observations_", 1:length(obs_rows)),
          # only keep the rows defined in the dictionary
          "X2" = table_sheet1$X8[obs_rows]
        )

        # only keep indicators and obs
        table_sheet1 <- subset(table_sheet1, select = c(1, 2))
        names(table_sheet1) <- c("X1", "X2")

        # bind rows of different bits want
        upload <- rbind(
          identifiers[ , 1:2],
          table_sheet1,
          yes_nos,
          observations

        )

        # recode variables based on dictionary
        upload$X1 <- matchmaker::match_vec(upload$X1,
                                           dictionary = var_dict,
                                           from = "french_label",
                                           to = "var_name")
        # flip to wide format
        upload <- tidyr::pivot_wider(upload,
                                     names_from = X1,
                                     values_from = X2)

        # change to date
        upload$date <- as.Date(
          as.numeric(upload$date),
          origin = "1899-12-30")


        # consider making week using tsibble


        #can make colour coded variables for the indicators as specified
        #ie
        #upload["Indicator_6_evaluation",f] <- ifelse(table_sheet1$X2[6]<5 ,"well",
        # ifelse(table_sheet1$X2[6]>10,"poor",
        #        ifelse(table_sheet1$X2[6]>=5 & table_sheet1$X2[6]<=10, "acceptable",
        #               NA)))

        # save data in list
        output[[f]] <- upload

      } else {
        ## if keeping in long format

        # add in extra cols for yes_nos
        yes_nos <- cbind(yes_nos, NA, NA , NA)

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

        # see which indicators are character responses
        char_vars <- grep("[a-z]", table_sheet1$X6)

        # copy indicator reponses in to new columns
        table_sheet1$num_vars <- table_sheet1$X6
        table_sheet1$str_vars <- table_sheet1$X6

        # set appropriate ones to empty
        table_sheet1$num_vars[char_vars] <- NA
        table_sheet1[-char_vars, "str_vars"] <- NA

        # make numeric indicators
        table_sheet1$num_vars <- as.numeric(table_sheet1$num_vars)

        # remove combined variable
        table_sheet1 <- table_sheet1[, -2]


        # recode variables based on dictionary
        table_sheet1$X1 <- matchmaker::match_vec(table_sheet1$X1,
                                           dictionary = var_dict,
                                           from = "french_label",
                                           to = "var_name")

        # fix names
        names(table_sheet1) <- c("indicator",
                                 "reporting_frequency",
                                 "observations",
                                 "grps",
                                 "country",
                                 "date",
                                 "epi_week",
                                 "num_vars",
                                 "str_vars")

        ## define evaluation variable
        table_sheet1$evaluation <- NA

        # choose which rows to work on
        indicator_rows <- paste0("indicator_", c(7, 9:11, 15, 17, 18, 24))

        # assign values
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                        table_sheet1$num_vars >= 90] <- "Bon"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars >= 80 &
                                  table_sheet1$num_vars <= 89] <- "Acceptable"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars < 80] <- "Mediocre"

        # choose which rows to work on
        indicator_rows <- paste0("indicator_", c(6, 19:21))

        # assign values
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars < 5] <- "Bon"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars >= 5 &
                                  table_sheet1$num_vars <= 10] <- "Acceptable"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars > 10] <- "Mediocre"

        # choose which rows to work on
        indicator_rows <- paste0("indicator_", c(12:13))

        # assign values
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars > 60] <- "Bon"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars >= 40 &
                                  table_sheet1$num_vars <= 60] <- "Acceptable"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars < 40] <- "Mediocre"

        # choose which rows to work on
        indicator_rows <- paste0("indicator_", c(14, 16))

        # assign values
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars > 40] <- "Bon"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars >= 20 &
                                  table_sheet1$num_vars <= 40] <- "Acceptable"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars < 20] <- "Mediocre"
        # choose which rows to work on
        indicator_rows <- paste0("indicator_", c(14, 16))

        # assign values
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars >= 0] <- "Bon"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars >= -5 &
                                  table_sheet1$num_vars <= -1] <- "Acceptable"
        table_sheet1$evaluation[table_sheet1$indicator %in% indicator_rows &
                                  table_sheet1$num_vars < -5] <- "Mediocre"


        # save data in list
        output[[f]] <- table_sheet1

      }
    } # end template conform section



    ## for non standard template files

    # else{
    #
    # }





  }


  ## pull together a single dataset
  output <- dplyr::bind_rows(output)

  ## output file
  # define path to output to
  filename <- base::paste(outputdirectory,"/",outputname,".xlsx", sep = "")
  # write file
  rio::export(output, file = filename)

  # return dataframe
  output
}
