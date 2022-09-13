library(tidyverse)
library(lubridate)
library(hms)



parseGoodVals <- function(x) {
  if (any(grepl("\\d+, ", x))) {
    # out <- stringr::str_extract(x, "\\d+")
    out <- stringr::str_extract(x, "\\d+\\.?\\d*")  # this gets floats as well
  } else if (length(x) == 0) {  # if there is nothing there at all, 
    out <- NA
  }
  else {
    out <- "garbage"
  }
  return(out)
}


makeDoubleDigit <- function(q) {
  ifelse(nchar(q) == 1, paste0("0", q), q)
}


convertToTime <- function(x, d) {
  for (nm in names(x)) {
    nmchk <- d[d$Variable...Field.Name == tolower(nm), "Field.Label"]
    if (length(nmchk) == 0) {
      next
    }
    # only find entries where "Hour" is the first word.
    if (grepl("^Hour", nmchk, ignore.case=T)) {
      # make sure values aren't already entered as just numbers
      if (!all(is.na(as.numeric(ifelse(x[,nm] == "NULL", NA, x[,nm]))))) {
        next
      }
      # if hour is in the name of the variable
      # this won't give it as a double-digit if it's a single digit.
      # apparently needs to be "military time", so like, 13:21 or 02:18
      hour_num <- makeDoubleDigit(hour(as.POSIXct(ifelse(x[,nm] == "NULL", NA, x[,nm]))))
      min_num <- makeDoubleDigit(minute(as.POSIXct(ifelse(x[,nm] == "NULL", NA, x[,nm]))))
      hour_mins <- paste(hour_num, min_num, sep=":")
      # see if value should be integer. if so, only use the hour_num
      chkval <- d[d$Variable...Field.Name == tolower(nm), 'Text.Validation.Type.OR.Show.Slider.Number']
      if (chkval == "integer") {
        x[,nm] <- hour_num
      } else {
        x[,nm] <- hour_mins
      }
      # now turn the NAs back to actual NAs.
      x[,nm] <- ifelse(grepl("NA", x[,nm]), NA, x[,nm])
    }
  }
  return(x)
}


prepareData <- function(dat, dict, event_name) {
  # make all names lowercase
  names(dat) <- tolower(names(dat))
  # rm these columns
  rmcols <- names(dat)[which(!names(dat) %in% dict$Variable...Field.Name)]
  dat <- dat[,-which(names(dat) %in% rmcols)]
  # turn "NULL" to actual na
  dat[,1:ncol(dat)] <- lapply(dat[,1:ncol(dat)], function(x) ifelse(x=="NULL", NA, x))
  # FIRST extract HH:MM from datetime if the variable designates that instead of the date
  dat <- convertToTime(x=dat, d=dict)
  # fix potential date columns
  dat[,1:ncol(dat)] <- lapply(dat[,1:ncol(dat)], function(x) {
    if (any(grepl("^\\d+-\\d+-\\d+", x))) {
      # add in a quick fix in case someone added a real date to the beginning of a huge chunk of text
      if (max(nchar(x), na.rm=T) < 50) {
        as.Date(x)
      } else {
        x
      }
      
    } else {
      x
    }
  })
  # create object of vars and acceptable values
  goodvals <- dict[,c('Variable...Field.Name', 'Choices..Calculations..OR.Slider.Labels')]
  names(goodvals) <- c("var", "vals")
  valsparse <- strsplit(goodvals$vals, "\\|")
  # now have to remove the text from the numbers..
  valsparse2 <- lapply(valsparse, parseGoodVals)
  
  # this SHOULD be searchable for each var now to check whether the values
  # that are in 'dat' match with the values in this list
  names(valsparse2) <- goodvals$var
  
  # just remove any values from dat that are not in the acceptable value list.
  # don't try and change any values in dat.
  
  for (vname in names(dat)) {
    acceptable_vals <- valsparse2[[vname]]
    if (all(is.na(acceptable_vals)) | any(acceptable_vals == "garbage")) {
      # check if field type is 'yesno'
      if (dict[dict$Variable...Field.Name == vname, "Field.Type"] == "yesno") {
        # then replace anything that's not a 1, 0 with NA
        rmidx <- which(!dat[,vname] %in% c(0, 1))
        dat[rmidx, vname] <- NA
        
       # also add in a check if the value type is integer. if so, floor all the values.
      } else if (dict[dict$Variable...Field.Name == vname, "Text.Validation.Type.OR.Show.Slider.Number"] == "integer") {
        dat[,vname] <- floor(as.numeric(dat[,vname]))
        
      } else {
        next
      }
      # } else if (!all(unique(dat[,vname]) %in% acceptable_vals)) {
    } else if (!all(na.omit(unique(dat[,vname])) %in% acceptable_vals)) {
      # have to ALLOW for NAs.
      rmidx <- which(!dat[,vname] %in% c(acceptable_vals, NA))
      dat[rmidx, vname] <- NA
    }
  }
  
  
  dat$redcap_event_name <- event_name
  
  return(dat)
}


ReChooseRCEvents <- function(tab, dictt, eventz) {
  evuse <- list()
  # Find which form each variable belongs to.....
  for (nm in names(tab)) {
    ev <- dictt[dictt$Variable...Field.Name == nm, "Form.Name"]
    if (length(ev) == 0) {
      next
    }
    evuse[[ev]] <- unique(c(evuse[[ev]], nm))
  }
  
  dfs <- list()
  # .....And then find what event name corresponds to the variables in each form, 
  for (form in names(evuse)) {
    evnm <- unique(eventz[which(eventz$form == form), "unique_event_name"])
    if (length(evnm) == 0) {
      next
    }
    cols_use <- evuse[[form]]
    if (!"id" %in% cols_use) {
      # drop=F in case there's only a single column. need to keep as a df.
      df_subset <- tab[,c("id", cols_use), drop=F]  # always need that id column!
    } else {
      df_subset <- tab[,c(cols_use),drop=F]  # unless it's already in the cols to use.
    }
    
    df_subset$redcap_event_name <- evnm
    dfs[[form]] <- df_subset
  }
  
  return(dfs)
}


processFiles <- function(x, d, e) {
  fl <- read.csv(x)
  names(fl) <- tolower(names(fl))
  # the 'base variable name' 
  # have to do this first to remove numbers like "372i1" with an i in them
  base_nms1 <- unique(gsub('i\\d+', '', names(fl)[grep('\\d+', names(fl))]))
  base_nms <- unique(gsub('\\d+', '', base_nms1))
  
  # possible form names for any of the base names found
  # I need to make sure the "^" is in front of every possible base_nms
  form_names <- unique(d[grepl(paste(paste0("^", base_nms), collapse="|"), d$Variable...Field.Name), "Form.Name"])
  event_names <- unique(e[grepl(paste(form_names, collapse="|"), e$form), "unique_event_name"])
  
  # see if this is a 3 and 12 month followup-er
  if (grepl("3month", x) & length(event_names) > 1) {
    event_name <- event_names[grepl("3month", event_names)]
  } else if (grepl("12month", x) & length(event_names) > 1) {
    event_name <- event_names[grepl("12month", event_names)]
  } else if (!grepl("12month|3month", x) & length(event_names) == 1) {
    event_name <- event_names[1]
  } else if (!grepl("12month|3month", x) & length(event_names) > 1 & "enrollment_arm_1" %in% event_names) {
    print(x)
    message("Choosing enrollment_arm_1 for event name")
    event_name <- event_names[which(event_names == "enrollment_arm_1")]
  } else {
    print(x)
    print(event_names)
    message("Choosing the first event name")
    event_name <- event_names[1]
  }
  
  fl2 <- prepareData(dat=fl, dict=dict, event_name=event_name)
  
  # remove mrn if need be
  if ('mrn' %in% names(fl2) & unique(fl2$redcap_event_name) != "enrollment_arm_1") {
    fl2 <- fl2[,-which(names(fl2)=="mrn"),]
  }
  
  # also need to search the dict using the current form name and see if there's a new "X_date" column under that form name
  if ('date' %in% names(fl2)) {
    varz <- d[grepl(paste(form_names, collapse="|"), d$Form.Name), "Variable...Field.Name"]
    date_var <- varz[grep('date', varz)]
    # remove the plain 'date' var if it shows up
    # ...but only if it's not the only one.
    if (length(unique(date_var)) > 1 & !all(unique(date_var) == 'date')) {
      date_var <- date_var[date_var != 'date']
    }
    if (length(date_var) == 0) {
      print(form_names)
      stop("Couldn't find the date column for this form")
    } else if (length(date_var) > 1) {
      print(x)
      print(date_var)
      message("found multiple date vars")
      # pick the date column that matches the most-represented base name
      # this counts how many grep matches there are for each base name
      match_tally_counts <- sapply(base_nms, function(x) length(grep(x, names(fl))))
      # wow this is a very verbose way to get the name of the element in the vector with the max value.
      # if you just use the max function, the names get stripped.
      most_represented_name <- names(match_tally_counts[which(match_tally_counts==max(match_tally_counts))])
      # then find the corresponding date var to this base name
      date_var <- grep(most_represented_name, date_var, value=T)
      
      print('choosing this date var:')
      print(date_var)
    }
    
    names(fl2)[which(names(fl2)=="date")] <- date_var
  }
  
  return(fl2)
}


checkRawDat <- function(x, varchk) {
  daters <- lapply(x, read.csv)
  nmchk <- lapply(daters, function(y) grep(varchk, names(y)))
  return(nmchk)
}
