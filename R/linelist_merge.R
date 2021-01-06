#' Merge linelists from AFRO
#'
#' @param inputdirectory path to folder containing datasets
#'
#' @param outputdirectory path to folder where merged file is to be saved (must exist already)
#'
#' @param outputname character string to name merged file
#'
#' @param isotomerge 3 letter ISO-code for country of interest, in quotation marks,
#' combine multiple countries with c(...), default is "AFRO" to read in all available.
#' See details for countries currently supported.
#'
#' @param quietly TRUE/FALSE of whether to show message of which country being
#' processed as the function runs through files. If false will show message for
#' each country (default = TRUE).
#'
#'
#' @importFrom rio import export
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate filter select across all_of contains rename_with
#' @importFrom tools file_path_sans_ext
#'
#' @details
#' Countries currently supported include:
#' "BFA", "CIV", "COD", "COG", "DZA", "GIN", "KEN", "LBR", "MOZ",
#' "MUS", "NAM", "NER", "RWA", "SEN", "SLE", "STP", "SWZ", "SYC",
#' "TCD", "UGA", "ZWE"
#'
#' @author Alice Carr, Alex Spina
#'
#' @export

## TODO delete this
#the inputs that i have been using
# inputdirectory <- "inst/extdata/frank_linelists_uptaded_2020-01-04//"
# outputdirectory <- "inst/"
# outputname <- "Merged_linelist_"
# isotomerge <- "AFRO"

merge_linelist <- function(inputdirectory,
                           outputdirectory = tempdir(),
                           outputname = "Merged_linelist_",
                           isotomerge = "AFRO",
                           quietly = TRUE) {

  # define countries in AFRO
  if (isotomerge == "AFRO") {
    isolist <- c("BFA", "CIV", "COD", "COG", "DZA", "GIN", "KEN", "LBR", "MOZ",
                 "MUS", "NAM", "NER", "RWA", "SEN", "SLE", "STP", "SWZ", "SYC",
                 "TCD", "UGA", "ZWE")
  } else {
    # if not reading all of afro then specify which
    isolist <- isotomerge
  }

  # only list the files of interest
  files <- list.files(path = inputdirectory,
                      full.names = TRUE)

  ## chuck an error if ISO Code wrong
  if (length(files) == 0) {
    stop("No files found, check the country codes are correct (3 letters)")
  }

  # add in a catch for files not processed properly (default is that these are
  # labelled AFRO) - drop with a warning

  # define which files dont start with a 3 letter iso code
  # i.e. the fourth letter is a not a dot
  droppers <- substr(basename(files), 4, 4) != "." |
    substr(basename(files), 0, 3) == "---"

  # if there are any to drop then throw a warning saying which
  if (sum(droppers) > 0) {
    warning(paste("Dropping incorrectly named files: \n",
                   paste0(basename(files)[droppers], collapse = "\n")))
  }

  # only keep the files not identified for dropping
  files <- files[!droppers]

  # only keep countries of interest
  files <- files[substr(basename(files), 0, 3) %in% isolist]

  ## chuck an error if ISO Code wrong
  if (length(files) == 0) {
    stop("No files found, check the country codes are correct (3 letters)")
  }

  # read in dictionary for renaming variables country specific sheet
  var_dict <- rio::import(
    system.file("extdata", "linelist_dictionary.xlsx",
    package = "covidmonitor"),
    which = 1
  )

  # clean variable names for dictionary
  var_dict <- janitor::clean_names(var_dict)
  # cleaning old variable names from unwanted regex patterns
  var_dict <- dplyr::mutate(var_dict, dplyr::across(c("old_var_r"), gsub, pattern = "\\s+$", replacement = "_"))
  var_dict <- dplyr::mutate(var_dict, dplyr::across(c("old_var_r"), gsub, pattern = "\\s+", replacement = "_"))
  var_dict <- dplyr::mutate(var_dict, dplyr::across(c("old_var_r"), gsub, pattern = "'", replacement = ""))
  var_dict <- dplyr::mutate(var_dict, dplyr::across(c("old_var_r"), gsub, pattern = "[[:punct:]]",replacement ="_"))
  var_dict <- dplyr::mutate(var_dict, dplyr::across(c("old_var_r"), gsub, pattern = "\\_+", replacement = "_"))
  var_dict <- dplyr::mutate(var_dict, dplyr::across(c("old_var_r"), gsub, pattern = "\\_$", replacement = ""))
  var_dict <- dplyr::mutate(var_dict, dplyr::across(c("old_var_r"), gsub, pattern = "^\\_", replacement = ""))
  var_dict <- dplyr::mutate(var_dict, dplyr::across(c("old_var_r"), gsub, pattern = "([a-z])([A-Z])",replacement = "\\1_\\2"))
  var_dict <- dplyr::mutate(var_dict, dplyr::across(c("old_var_r"), gsub, pattern = "([A-Z])([A-Z])([a-z])",replacement = "\\1_\\2\\3"))
  var_dict <- dplyr::mutate(var_dict, dplyr::across(c("old_var_r", "new_var"), tolower))
  # cleaning old variable names replacing all spaces and punctuation with . (as this is dont on the variable names upon loading files)
  var_dict <- dplyr::mutate(var_dict, dplyr::across(c("old_var_r", "new_var"), iconv, from = "UTF-8", to = "ASCII//TRANSLIT"))


  # read in template and sheet checking dictionary
  template_check <- rio::import(
    system.file("extdata", "linelist_dictionary.xlsx",
    package = "covidmonitor"),
    which = 3
  )
  template_check <- janitor::clean_names(template_check)

  # read in variables of interest
  variables_to_keep <- rio::import(
    system.file("extdata", "linelist_dictionary.xlsx",
    package = "covidmonitor"),
    which = 4
  )

  variables_to_keep <- janitor::clean_names(variables_to_keep)

  # create an empty list to fill in with datasets
  output <- list()


  # for each file listed in input directory:
  for (f in 1:length(files)) {
    # ISO codes of country file
    iso <- substr(tools::file_path_sans_ext(basename(files[f])), 0, 3)

    ## say which file is currently being processed
    if (!quietly) {
      message(paste(iso, "processing..."))
    }

    # for this iso code set parameters dictating file load in and clean process using the template_check dictionary
    sheetname <- template_check$sheetname[template_check$country == iso]
    skip <- template_check$skip[template_check$country == iso]
    template <- template_check$template[template_check$country == iso]

    # load in file
    # TODO files that are xlsb (CIV) cannot be open (easily on mac)
    # for ease resave the file in an xlsx format,
    #  this can be edited to use the RODBC package which i think works on windows
    if (grepl(files[f], pattern = "\\.xlsb$", ignore.case = TRUE)) {
      warning(paste("Following file is in xlsb format, please resave file in xlsx format before proceeding: \n",
                    basename(files[f])))
    } else {
      # guess max to ensure no cells are missing when reading in if many empty columns preceeded
      og_sheet <- rio::import(files[f], which = sheetname, skip = skip, guess_max = 10000000)
    }

    # clean variable names to removed unwated regex patterns, replace all spaces with a ., inorder to match var_dict dictionary
    og_sheet <- janitor::clean_names(og_sheet)
    names(og_sheet) <- iconv(names(og_sheet), from = "UTF-8", to = "ASCII//TRANSLIT")
    names(og_sheet) <- sub(names(og_sheet), pattern = "(^[x0-9]{1})",replacement = "")

    og_sheet <- dplyr::mutate(og_sheet, dplyr::across(c(-1), iconv, from = "UTF-8", to = "ASCII//TRANSLIT"))
    og_sheet <- dplyr::mutate(og_sheet, dplyr::across(c(-1), gsub, pattern = "['?]", replacement = "", ignore.case = T, perl = T))
    og_sheet <- dplyr::mutate(og_sheet, dplyr::across(c(-1), gsub, pattern = "\r\n", replacement = "", fixed = T))
    og_sheet <- dplyr::mutate(og_sheet, dplyr::across(c(-1), gsub, pattern = "\\s+", replacement = " ", ignore.case = T))
    og_sheet <- dplyr::mutate(og_sheet, dplyr::across(c(-1), gsub, pattern = "\\s+$", replacement = "", ignore.case = T))
    # must keep this pattern all one line or doesnt work
    og_sheet <- dplyr::mutate(og_sheet, dplyr::across(c(-1), gsub, pattern = "(?i)^NA$|(?i)^N/A$|(?i)^N/A,|(?i)^N\\A$|(?i)^Unknown$|(?i)^dont know$|(?i)^Unkown$|(?i)^N.A$|(?i)^NE SAIT PAS$|(?i)^inconnu$|^ $|(?i)^Nao aplicavel$|(?i)^Sem informacao$|(?i)^Unk$|(?i)^NP$", replacement = NA, perl = T))

#     Alex: i think this is a duplicate of what is happening above....
#     names(og_sheet) <- gsub(x = names(og_sheet), pattern ="\r\n",replacement = "", fixed = T)
#     names(og_sheet) <- gsub(x = names(og_sheet), pattern ="\\s+", replacement =" ")
#     names(og_sheet) <- gsub(x = names(og_sheet), pattern ="\\s+$", replacement ="")
#     names(og_sheet) <- gsub(x = names(og_sheet), pattern ="\\s+|[[:punct:]]", replacement ="_")
#     names(og_sheet) <- tolower(names(og_sheet))
#     names(og_sheet) <- stringi::stri_trans_general(names(og_sheet) , "Latin-ASCII")

    # filter variable cleaning dictionary specific to country file loaded
    var_dict_country <- dplyr::filter(var_dict, country == iso)
    var_dict_country <- var_dict_country[var_dict_country$old_var_r %in% names(og_sheet), ]

    # list variables to drop that are non-template (or not defined)
    drop_nontemplate_vars <- c(
      # those that are defined in our dictionary but non-template
      var_dict_country$old_var_r[
        var_dict_country$notes == "non_template" &
          !is.na(var_dict_country$notes)],
      # those that are not defined in our dictionary even
      names(og_sheet)[!names(og_sheet) %in% var_dict_country$old_var_r]
    )

    # drop variables from dataframe which are non-tmeplate
    if (length(drop_nontemplate_vars) != 0) {
      og_sheet <- og_sheet[, !(names(og_sheet) %in% drop_nontemplate_vars)]

      warning(paste(basename(files[f]), "\n Non-template variables dropped:",
                    paste(drop_nontemplate_vars, collapse = ", ")))
    }

    # if file requires no cleaning as matched WHO standard template do this loop
    if (template == TRUE) {
      output_sheet <- og_sheet

      # If file needs recoding of variables/merging of variables
    } else if (template == FALSE) {
      # recode variables
      recode_vars <- var_dict_country$old_var_r[grepl("Recode",
                                                      var_dict_country$notes) &
                                                  !is.na(var_dict_country$notes)]

      if (length(recode_vars) != 0) {
        # these variables are usually patinfo_occus (proffession),
        # comcond_exist (pre exsiting medical) and
        # patsympt_other (other symptoms which have often been split into separate variables)
        variable_to <- sub("Recode to ", "",
                           as.list(var_dict_country$notes[grepl("Recode",
                                                                var_dict_country$notes) &
                                                            !is.na(var_dict_country$notes)]))
        what_to_match <- unique(variable_to)

        warning(paste(
          basename(files[f]),
          "\n Recoding variables:", paste(recode_vars, collapse = ", "),
          "\n \n Recoding to:", paste(what_to_match, collapse = ", ")))

        recode_sheet <- og_sheet[, which(names(og_sheet) %in% recode_vars)]
        # remove the columns that required recoding from original sheet
        output_sheet <- dplyr::select(og_sheet,-c(dplyr::contains(recode_vars)))

        # remove following instances from variable names that need recoding for
        # the next step to leave what will become the recoded value
        names(recode_sheet) <- gsub(x = names(recode_sheet),
                                    pattern = "COVID.|Co19.|comcond.|patsympt.|patinfo.|COVID_19_",
                                    replacement = "",
                                    ignore.case = T)

        # replace all instances of no with missing
        recode_sheet <- data.frame(lapply(recode_sheet, function(x) {
          gsub("NAO|NON|NO|none|nil|^[nN]$|Know|0", NA, x, ignore.case = T, perl = T)
        }), stringsAsFactors = F)
        # replace all instances of yes with 1
        recode_sheet <- data.frame(lapply(recode_sheet, function(x) {
          gsub("SIM|OUI|YES|1|(?i)^si$|^[yY]$", 1, x, ignore.case = T, perl = T)
        }), stringsAsFactors = F)

        # where all instances were 1 (yes) recode to the name of the variable for merge
        recode_sheet <- data.frame(sapply(names(recode_sheet), function(x)
          ifelse(recode_sheet[, x] == 1, x, recode_sheet[, x])), stringsAsFactors = F)
        colnames(recode_sheet) <- gsub("^X", "",  colnames(recode_sheet))

        # renames variables with key of what to code to
        recode_vars <- gsub(x = recode_vars,
                            pattern = "COVID.|Co19.|comcond.|patsympt.|patinfo.|COVID_19_",
                            replacement = "", ignore.case = T)

        # above line present as this step was done to the variables in recode sheet
        recode_sheet <- dplyr::rename_with(recode_sheet,
                                           .fn = ~paste(recode_vars, variable_to, sep = "."),
                                           .cols = dplyr::all_of(recode_vars))

        # concatenate all common variables using split method,
        # nb. method now not dependant on data.table
        recode_sheet[names(base::split.default(recode_sheet, sub(".*\\.", "", names(recode_sheet))))] <-
          suppressWarnings(lapply(base::split.default(recode_sheet, sub(".*\\.", "", names(recode_sheet))), function(x) do.call(paste, c(x, sep = ","))))

        # remove the _ which was a place holder for a space in variable name
        recode_sheet <- data.frame(
          lapply(recode_sheet, function(x) {
            gsub("_", " ", x, fixed = T)}),
          stringsAsFactors = F)
        # replace the NA values which have been coded as text NA after reduce
        recode_sheet <- data.frame(
          lapply(recode_sheet, function(x) {
            gsub("NA", "", x, perl = T)}),
          stringsAsFactors = F)
        # replace everything that has a more than one comma or singular commas with no characters
        recode_sheet <- data.frame(
          lapply(recode_sheet, function(x) {
            gsub("^,*|(?<=,),|,*$", "", x, perl = T)}),
          stringsAsFactors = F)
        # replace blanks with NA
        recode_sheet[recode_sheet == ""] <- NA
        # select newly recoded columns
        recode_sheet$id <- rownames(recode_sheet) # create an id variable for when merging back to og sheet occurs
        colnames(recode_sheet) <- gsub("^X", "",  colnames(recode_sheet))
        recode_sheet <- recode_sheet[, which(names(recode_sheet) %in% c(what_to_match, "id"))]

        # match old variable names with dictionary for new variable names
        names(output_sheet) <- with(var_dict_country, new_var[match(names(output_sheet), old_var_r)])
        # cbind with newly recoded variables
        output_sheet$id <- rownames(output_sheet) # create an id variable for when merging back to og sheet occurs
        output_sheet <- merge(output_sheet, recode_sheet, by = "id")
        output_sheet <- dplyr::select(output_sheet, -c(id))


      } else {
        # if there are no variables that needed recoding
        # just match old variable names with dictionary for new variable names
        output_sheet <- og_sheet
        names(output_sheet) <- with(var_dict_country, new_var[match(names(output_sheet), old_var_r)])
      }
    }

    ########
    # fix dates (see ?clean_dates details for method)
    output_sheet <- dplyr::mutate(output_sheet,
                               dplyr::across(
                                 dplyr::contains("Date", ignore.case = TRUE),
                                 covidmonitor::clean_dates))


    ########
    # remove rows where all columns except ID is missing or if case id is missing
    output_sheet$na_rows <- rowSums(!is.na(output_sheet))
    output_sheet <- dplyr::filter(output_sheet, !is.na(patinfo_id) | na_rows > 1)
    output_sheet <- dplyr::select(output_sheet,-c(na_rows))
    # make variable names lower case for output sheet
    names(output_sheet) <- tolower(names(output_sheet))

    # checking that variables of interest exists
    # if not create missing column in cleaning we can use surrogate variables to infill

    vars <- colnames(output_sheet)
    varstokeep <- variables_to_keep$variable
    output_sheet$id <- rownames(output_sheet)

    # find variables not present
    cols <- setdiff(varstokeep, vars)

    if (length(grep("patsympt", vars)) == 0) {
      cols <- c(cols, "patsympt_other")
    }

    # Looping to create them in output_sheet
    if (length(cols != 0)) {

      warning(paste(
        basename(files[f]),
        "\n Creating variables not present:", paste(cols, collapse =", ")))

      output_sheet[ , cols] <- NA

    }


    # make column for ISO code
    output_sheet$country_iso <- iso
    # change all date columns (newly created ones are logical)
    output_sheet <- dplyr::mutate(output_sheet, dplyr::across(contains("date"), as.Date, origin = "1899-12-30"))
    # some ages had issues on and wouldnt merge
    output_sheet$patinfo_ageonset <- as.numeric(iconv(output_sheet$patinfo_ageonset, "utf-8", "ascii", sub = ""))

    # keep variables of interest
    output_sheet <- dplyr::select(output_sheet,
                                  dplyr::all_of(variables_to_keep$variable),
                                  contains("patsympt"))

    # add cleaned output sheet to a list
    output[[f]] <- output_sheet

  }

  # merge all cleaned sheets into one
  output_fin <- output[!sapply(output, is.null)]
  big_data <- Reduce(function(...) merge(..., all = T), output_fin)
  big_data <- big_data[order(big_data$country_iso), ]

  # define path to output to
  filename <- paste0(outputdirectory,"/", outputname, Sys.Date(), ".xlsx")

  # write file
  rio::export(big_data, file = filename)

}
