#' Merge linelists from AFRO
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
#' @importFrom rio import export
#' @importFrom tidyr fill pivot_wider
#' @importFrom matchmaker match_vec
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate filter select
#' @importFrom janitor clean_names
#' @importFrom stringi stri_trans_general
#' @author Alice Carr, Alex Spina
#' @export

library(dplyr) #still have this here because across not supported in standard dplyr ?

# the inputs thati have been using
# inputdirectory <- "inst/extdata/frank_linelists/"
# outputdirectory <- "inst/"
# outputname <- "Merged_linelist_"
# isotomerge <- "AFRO"

merge_linelist <- function(inputdirectory,
                           outputdirectory = tempdir(),
                           outputname = "Merged_linelist_", isotomerge = "AFRO") {
  if (isotomerge == "AFRO") {
    isolist <- c("BFA", "CIV", "COD", "COG", "DZA", "GIN", "KEN", "LBR", "MOZ", "MUS", "NAM", "NER", "RWA", "SEN", "SLE", "STP", "SWZ", "SYC", "TCD", "UGA", "ZWE")
  } else {
    isolist <- isotomerge
  }

  files <- base::list.files(path = inputdirectory, full.names = TRUE, pattern = paste(isolist, collapse = "|"))

  # create folder for output
  base::dir.create(outputdirectory, showWarnings = FALSE)

  # read in dictionary for renaming variables contry specific sheet
  var_dict <- rio::import(
    # system.file(
    here::here("inst", "extdata", "linelist_dictionary.xlsx"),
    # package = "covidmonitor"),
    which = 1
  )
  var_dict <- janitor::clean_names(var_dict)
  var_dict <- dplyr::mutate(var_dict, across(c("old_var_r"), gsub, pattern = "\\s+$", replacement = "_")) # cleaning old variable names from unwanted regex patterns
  var_dict <- dplyr::mutate(var_dict, across(c("old_var_r"), gsub, pattern = "\\s+", replacement = "_"))
  var_dict <- dplyr::mutate(var_dict, across(c("old_var_r"), gsub, pattern = "'", replacement = ""))
  var_dict <- dplyr::mutate(var_dict, across(c("old_var_r"), gsub, pattern = "[[:punct:]]",replacement ="_"))
  var_dict <- dplyr::mutate(var_dict, across(c("old_var_r"), gsub, pattern = "\\_+", replacement = "_"))
  var_dict <- dplyr::mutate(var_dict, across(c("old_var_r"), gsub, pattern = "\\_$", replacement = ""))
  var_dict <- dplyr::mutate(var_dict, across(c("old_var_r"), gsub, pattern = "^\\_", replacement = ""))
  var_dict <- dplyr::mutate(var_dict, across(c("old_var_r"), gsub, pattern = "([a-z])([A-Z])",replacement = "\\1_\\2"))
  var_dict <- dplyr::mutate(var_dict, across(c("old_var_r"), gsub, pattern = "([A-Z])([A-Z])([a-z])",replacement = "\\1_\\2\\3"))
  var_dict <- dplyr::mutate(var_dict, across(c("old_var_r", "new_var"), tolower))
  var_dict <- dplyr::mutate(var_dict, across(c("old_var_r", "new_var"), stringi::stri_trans_general, "Latin-ASCII")) # cleaning old variable names replacing all spaces and punctuation with . (as this is dont on the variable names upon loading files)


  # read in template and sheet checking dictionary
  template_check <- rio::import(
    # system.file(
    here::here("inst", "extdata", "linelist_dictionary.xlsx"),
    # package = "covidmonitor")
    which = 3
  )
  template_check <- janitor::clean_names(template_check)


  variables_to_keep <- rio::import(
    # system.file(
    here::here("inst", "extdata", "linelist_dictionary.xlsx"),
    # package = "covidmonitor"),
    which = 4
  )
  variables_to_keep <- janitor::clean_names(variables_to_keep)

  # create an empty list to fill in with datasets
  output <- list()


  # for each file listed in input directory:
  for (f in 1:base::length(files)) {
    # ISO codes of country file
    iso <- substr(tools::file_path_sans_ext(basename(files[f])), 0, 3)
    warning(paste(iso, "processing"))
    # print(iso) #here for debugging

    # for this iso code set parameters dictating file load in and clean process using the template_check dictionary
    sheetname <- template_check$sheetname[template_check$country == iso]
    skip <- template_check$skip[template_check$country == iso]
    template <- template_check$template[template_check$country == iso]

    # load in file
    # files that are xlsb (CIV) cannot be open (easily on mac) for ease resave the file in an xlsx format, this can be edited to use the RODBC package which i think works on windows
    if (grepl(files[f], pattern = "\\.xlsb$", ignore.case = TRUE)) {
      warning(paste(iso, "file is in xlsb format, please resave file in xlsx format before proceeding"))
    } else {
      og_sheet <- rio::import(files[f], which = sheetname, skip = skip)
    }

    # clean variable names to removed unwated regex patterns, replace all spaces with a ., inorder to match var_dict dictionary
    og_sheet <- janitor::clean_names(og_sheet)
    names(og_sheet) <- stringi::stri_trans_general(names(og_sheet), "Latin-ASCII")
    names(og_sheet) <- sub(names(og_sheet), pattern = "(^[x0-9]{1})",replacement = "")

    og_sheet <- dplyr::mutate(og_sheet, across(c(-1), stringi::stri_trans_general, "Latin-ASCII"))
    og_sheet <- dplyr::mutate(og_sheet, across(c(-1), gsub, pattern = "['?]", replacement = "", ignore.case = T, perl = T))
    og_sheet <- dplyr::mutate(og_sheet, across(c(-1), gsub, pattern = "\r\n", replacement = "", fixed = T))
    og_sheet <- dplyr::mutate(og_sheet, across(c(-1), gsub, pattern = "\\s+", replacement = " ", ignore.case = T))
    og_sheet <- dplyr::mutate(og_sheet, across(c(-1), gsub, pattern = "\\s+$", replacement = "", ignore.case = T))
    og_sheet <- dplyr::mutate(og_sheet, across(c(-1), gsub, pattern = "(?i)^NA$|(?i)^N/A$|(?i)^N/A,|(?i)^N\\A$|(?i)^Unknown$|(?i)^dont know$|(?i)^Unkown$|(?i)^N.A$|(?i)^NE SAIT PAS$|(?i)^inconnu$|^ $|(?i)^Nao aplicavel$|(?i)^Sem informacao$", replacement = NA, perl = T))
    # must keep this pattern all one line or doesnt work

    names(og_sheet) <- gsub(x= names(og_sheet),pattern ="\r\n",replacement = "", fixed = T)
    names(og_sheet) <-gsub(x= names(og_sheet),pattern ="\\s+", replacement =" ")
    names(og_sheet) <-gsub(x= names(og_sheet),pattern ="\\s+$", replacement ="")
    names(og_sheet) <-gsub(x= names(og_sheet),pattern ="\\s+|[[:punct:]]", replacement ="_")
    names(og_sheet) <-tolower(names(og_sheet))
    names(og_sheet)<- stringi::stri_trans_general(names(og_sheet) , "Latin-ASCII")

    #remove all accents from values in dataframe
    #replace all values in file that should be coded as missing
    #using gsub and regex pattern matching here as some patterns require to be fixed and others not
    og_sheet<-og_sheet %>% mutate_at(vars(-1),stringi::stri_trans_general,"Latin-ASCII") %>%
      mutate_at(vars(-1),gsub,pattern="['?]",replacement = "",ignore.case = T, perl = T) %>%
      mutate_at(vars(-1),gsub,pattern="\r\n",replacement = "",fixed=T) %>%
      mutate_at(vars(-1),gsub,pattern="\\s+",replacement = " ",ignore.case = T) %>%
      mutate_at(vars(-1),gsub,pattern ="\\s+$", replacement ="", ignore.case = T) %>%
      mutate_at(vars(-1),gsub, pattern="(?i)^NA$|(?i)^N/A$|(?i)^N/A,|(?i)^N\\A$|(?i)^Unknown$|(?i)^dont know$|(?i)^Unkown$|(?i)^N.A$|(?i)^NE SAIT PAS$|(?i)^inconnu$|^ $|(?i)^Nao aplicavel$|(?i)^Sem informacao$", replacement=NA, perl = T)
#must keep this pattern all one line or doesnt work


    # filter variable cleaning dictionary specific to country file loaded
    var_dict_country <- dplyr::filter(var_dict, country == iso)
    var_dict_country <- var_dict_country[var_dict_country$old_var_r %in% names(og_sheet), ]

    # list variables to drop that are non-template
    drop_nontemplate_vars <- var_dict_country$old_var_r[var_dict_country$notes == "non_template" & !is.na(var_dict_country$notes)]


    # drop variables from dataframe which are non-tmeplate
    if (length(drop_nontemplate_vars) != 0) {
      og_sheet <- og_sheet[, !(names(og_sheet) %in% drop_nontemplate_vars)]
      warning(paste(iso, "\n Non-template variables dropped:", paste(drop_nontemplate_vars, collapse = ","), "\n Epi-week:", Sys.Date()))
    }

    # if file requires no cleaning as matched WHO standard template do this loop
    if (template == TRUE) {
      output_sheet <- og_sheet

      # If file needs recoding of variables/merging of variables
    } else if (template == FALSE) {
      # recode variables
      recode_vars <- var_dict_country$old_var_r[grepl("Recode", var_dict_country$notes) & !is.na(var_dict_country$notes)]

      if (length(recode_vars) != 0) {
        # these variables are usually patinfo_occus (proffession), comcond_exist (pre exsiting medical) and patsympt_other (other symptoms which have often been split into separate variables)
        variable_to <- sub("Recode to ", "", as.list(var_dict_country$notes[grepl("Recode", var_dict_country$notes) & !is.na(var_dict_country$notes)]))
        what_to_match <- unique(variable_to)
        warning(paste("Recoding variables:", paste(recode_vars, collapse = ","), "\n \n Recoding to:", paste(what_to_match, collapse = ",")))

        recode_sheet <- og_sheet[, which(names(og_sheet) %in% recode_vars)]

        # remove following instances from variable names that need recoding for the next step to leave what will become the recoded value
        names(recode_sheet) <- gsub(x = names(recode_sheet), pattern = "COVID.|Co19.|comcond.|patsympt.|patinfo.", replacement = "", ignore.case = T)

        # replace all instances of no with missing
        recode_sheet <- data.frame(lapply(recode_sheet, function(x) {
          gsub("NAO|NON|NO|none|nil|^[nN]$|Know", NA, x, ignore.case = T, perl = T)
        }), stringsAsFactors = F)
        # replace all instances of yes with 1
        recode_sheet <- data.frame(lapply(recode_sheet, function(x) {
          gsub("SIM|OUI|YES|1|(?i)^si$|^[yY]$", 1, x, ignore.case = T, perl = T)
        }), stringsAsFactors = F)

        # where all instances were 1 (yes) recode to the name of the variable for merge
        recode_sheet <- data.frame(sapply(names(recode_sheet), function(x) ifelse(recode_sheet[, x] == 1, x, recode_sheet[, x])), stringsAsFactors = F)
        # renames variables with key of what to code to
        names(recode_sheet) <- paste(names(recode_sheet), variable_to, sep = ".")

        # concatenate all common variables using split method, method now not dependant on data.table
        recode_sheet[what_to_match] <-
          suppressWarnings(lapply(base::split.default(recode_sheet, what_to_match), function(x) do.call(paste, c(x, sep = ","))))

        # remove the _ which was a place holder for a space in variable name
        recode_sheet <- data.frame(lapply(recode_sheet, function(x) {
          gsub("_", " ", x, fixed = T)
        }), stringsAsFactors = F)
        # replace the NA values which have been coded as text NA after reduce
        recode_sheet <- data.frame(lapply(recode_sheet, function(x) {
          gsub("NA", "", x, perl = T)
        }), stringsAsFactors = F)
        # replace everything that has a more than one comma or singula commas with no characters
        recode_sheet <- data.frame(lapply(recode_sheet, function(x) {
          gsub("^,*|(?<=,),|,*$", "", x, perl = T)
        }), stringsAsFactors = F)
        # replace blanks with NA
        recode_sheet[recode_sheet == ""] <- NA
        # select newly recoded columns
        recode_sheet$id <- rownames(recode_sheet) # create an id variable for when merging back to og sheet occurs
        recode_sheet <- recode_sheet[, which(names(recode_sheet) %in% c(what_to_match, "id"))]

        # remove the columns that required recoding from original sheet
        output_sheet <- og_sheet[, -which(names(og_sheet) %in% recode_vars)]

        # match old variable names with dictionary for new variable names
        names(output_sheet) <- with(var_dict_country, new_var[match(names(output_sheet), old_var_r)])
        # cbind with newly recoded variables
        output_sheet$id <- rownames(output_sheet) # create an id variable for when merging back to og sheet occurs
        output_sheet <- merge(output_sheet, recode_sheet, by = "id")
        output_sheet <- select( output_sheet,-c(id))

        # if there are no variables that needed recoding just match old variable names with dictionary for new variable names
      } else {
        output_sheet <- og_sheet
        names(output_sheet) <- with(var_dict_country, new_var[match(names(output_sheet), old_var_r)])
      }
    }

    # handle dates that are numeric. All character dates -> NA.
    # Convert to numeric and change date format using origin of excel
    # Convert to character date for final merge
    output_sheet$id <- rownames(output_sheet)

    datesnumeric <- output_sheet %>%
      select(contains("Date", ignore.case = T), id) %>%
      mutate_all(.funs = gsub, pattern = ".", replacement = "/", fixed = T, perl = T) %>%
      mutate_at(vars(contains("Date", ignore.case = T)), as.numeric) %>%
      mutate_at(vars(contains("Date", ignore.case = T)), as.Date, origin = "1899-12-30") %>%
      mutate_at(vars(contains("Date", ignore.case = T)), as.character)

    # handle dates that are characters. Select first 10 characters in cell This will select all full dates and not any times appended.
    # Convert character formats using parse date time.
    # Convert to numeric and change date format using origin of excel
    # Convert to character date for final merge
    datescharacter <- output_sheet %>%
      select(contains("Date", ignore.case = T), id) %>%
      mutate_at(vars(contains("Date", ignore.case = T)), lubridate::parse_date_time, orders = c("ymd", "Ymd", "dmy", "dmY", "%Y-%m-%d", "%y-%m-%d", "%d-%m-%y", "dBY", "ymd HMS", "Ymd HMS")) %>%
      mutate_at(vars(contains("Date", ignore.case = T)), as.Date, origin = "1899-12-30") %>%
      mutate_at(vars(contains("Date", ignore.case = T)), as.character)


    # Natural join of data frames, fills in missing values in one df with values from other if not misssing

    dates <- rquery::natural_join(datesnumeric, datescharacter,
      by = "id",
      jointype = "FULL"
    )

    # final date handling: change all columns to date class type (double)
    dates_final <- dates %>% mutate_at(vars(contains("Date", ignore.case = T)), lubridate::parse_date_time, orders = c("ymd"))
    # merge in these new handled dates to orginal sheet, removing old coded dates
    date_vars <- dates_final %>%
      select(contains("Date", ignore.case = T)) %>%
      names(.)
    output_sheet <- output_sheet[, -which(names(output_sheet) %in% date_vars)]
    # output_sheet$id<-rownames(output_sheet)
    output_sheet <- merge(output_sheet, dates_final, by = "id") %>% select(-c(id))


    ########
    # remove rows where all columns except ID is missing or if case id is missing
    output_sheet$na_rows <- rowSums(!is.na(output_sheet))
    output_sheet <- dplyr::filter(output_sheet, !is.na(patinfo_id) | na_rows > 1)
    output_sheet <- select(output_sheet,-c(na_rows))
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
      for (v in cols) {
        output_sheet[[v]] <- NA
        warning(paste("Creating variables not present:", paste(cols, collapse =",")))
      }
    } else {
      warning("All variables of interest present")
    }


    # make column for ISO code
    output_sheet$country_iso <- iso
    #change all date columns
    output_sheet <- mutate(output_sheet, across(contains("date"), as.Date, origin = "1899-12-30"))
    # some ages had issues on and wouldnt merge
    output_sheet$patinfo_ageonset <- as.numeric(base::iconv(output_sheet$patinfo_ageonset, "utf-8", "ascii", sub = ""))

    # keep variables of interest
    output_sheet <- dplyr::select(
      output_sheet,patinfo_id, report_date, patinfo_ageonset, patinfo_ageonsetunit, patinfo_ageonsetunitdays,
      patinfo_sex, patinfo_resadmin1,
      patinfo_resadmin2, report_classif,
      pat_symptomatic, pat_asymptomatic,
      comcond_preexist1, comcond_preexist,
      patinfo_occus, expo_travel, expo_travel_country,
      expo_contact_case, lab_result,
      lab_datetaken, lab_resdate,
      patcourse_status,
      patcourse_datedeath, patcourse_datedischarge, country_iso, contains("patsympt"), patcurrent_status
    )

    # add cleaned output sheet to a list
    output[[f]] <- output_sheet

    #keep variables of interest
    output_sheet<- output_sheet %>% dplyr::select(patinfo_id,report_date, patinfo_ageonset, patinfo_ageonsetunit,patinfo_ageonsetunitdays,
                                                  patinfo_sex, patinfo_resadmin1,
                                                  patinfo_resadmin2, report_classif,
                                                  pat_symptomatic, pat_asymptomatic,
                                                  comcond_preexist1, comcond_preexist,
                                                  patinfo_occus, expo_travel, expo_travel_country,
                                                  expo_contact_case, lab_result,
                                                  lab_datetaken, lab_resdate,
                                                  patcourse_status,
                                                  patcourse_datedeath, patcourse_datedischarge, country_iso, contains("patsympt"), patcurrent_status)



    #add cleaned output sheet to a list
    output[[f]] <-output_sheet
  }

  # merge all cleaned sheets into one
  output_fin <- output[!sapply(output, is.null)]
  big_data <- Reduce(function(...) merge(..., all = T), output_fin)
  big_data <- big_data[order(big_data$country_iso), ]

  rio::export(big_data,
    # system.file(
    file=here::here(outputdirectory, paste0(outputname, Sys.Date(), ".xlsx"))
    # package = "covidmonitor"),
  )

}
