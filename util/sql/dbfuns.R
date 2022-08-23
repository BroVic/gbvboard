# This function is temporarily created for dealing with projects
# other than NFWP, pending harmonization of approach to data import
loadData <-
  function(dbpath,
           state = character(0),
           type = c("services", "capacity")) {
    stopifnot(length(state) == 1, is.character(state))
    type <- match.arg(type)
    
    try({
      con <- RSQLite::dbConnect(RSQLite::SQLite(), dbpath)
      on.exit(RSQLite::dbDisconnect(con))
      rgx <- paste(tolower(state), type, 'clean', sep = '.')
      tbl <- grep(rgx, RSQLite::dbListTables(con), value = TRUE)
      RSQLite::dbReadTable(con, tbl)
    })
  }




# Runs an SQL query on the database, generically.
query_db <- function(db, qry)
{
  if (!file.exists(db))
    stop("'db' does not exist")
  if (!is.character(qry) || length(qry) > 1L)
    stop("'qry' must be a character vector of length 1")
  require(RSQLite)
  tryCatch({
    dbcon <- dbConnect(SQLite(), db)
    r <- dbSendStatement(dbcon, qry)
    res <- dbFetch(r)
  },
  error = function(e)
    stop(e, call. = FALSE),
  finally = {
    if (exists('r', envir = environment()))
      dbClearResult(r)
    dbDisconnect(dbcon)
  })
  invisible(res)
}




# Appends data to already created database tables
append_to_db <- function(df, tbl, db) {
  require(RSQLite, quietly = TRUE)
  
  tryCatch({
    message("Populating table ", sQuote(tbl), "... ", appendLF = FALSE)
    con <- dbConnect(SQLite(), db)
    
    if (.checkExistingData(con, df, tbl)) {
      stop(sprintf(
        "The data in '%s' already exists in table %s",
        deparse(substitute(df)),
        tbl
      ), call. = FALSE)
    }
    
    tryCatch({
      old <- .fetchDbTable(con, tbl)
      
      if (nrow(old))
        dbAppendTable(con, tbl, df)
      else
        dbWriteTable(con, tbl, df, append = TRUE)
      
      message("Done")
    },
    error = function(e) {
      message("Failed")
      message(conditionMessage(e), call. = FALSE)
    })
  },
  error = function(w) {
    message("Skipped. ", sQuote(tbl), " already exists")
  }, finally = dbDisconnect(con))
}



# Internal function that checks whether the data already exist
# in the database table, returning TRUE when this is so, otherwise FALSE
# If non-matching tables are checked, it signals an error.
.checkExistingData <- function(conn, data, table) {
  .assertDbConnect(conn, table)
  
  dd <- .fetchDbTable(conn, table)
  
  if (!nrow(dd))
    return(FALSE)
  
  dfnames <- names(data)
  
  if ("id" %in% names(dd) && isFALSE("id" %in% dfnames))
    dd$id <- NULL
  
  if (!all(dfnames %in% names(dd)))
    stop(
      sprintf("The column names in the table and the input data differ")
    )
  
  # We bind a few rows of the data we want to input to the
  # previously existing one. If there are any duplications, we
  # can safely assume that the data are pre-existent.
  if (anyDuplicated(data)) {
    data <- unique(data)  # deal with duplicate records in original data
    warning("Duplicate rows were found and removed") # TODO: Quantify.
  }
  
  dfx <- rbind(dd, head(data))
  as.logical(anyDuplicated(dfx, fromLast = TRUE))
}



.fetchDbTable <- function(con, tbl) {
  .assertDbConnect(con, tbl)
  dbGetQuery(con, sprintf("SELECT * FROM %s;", tbl))
}


.assertDbConnect <- function(c, t) {
  stopifnot(RSQLite::dbIsValid(c), is.character(t))
}



get_labels_via_rgx <- function(df, rgx) {
  stopifnot(is.data.frame(df), is.character(rgx))
  res <- get_var_labels(df, grep(rgx, names(df)))
  # res <- res[!grepl("other", res, ignore.case = TRUE)]
  res
}



# prepare_ref_table <- function(variable, options, data = alldata) {
#   stopifnot(is.character(options), is.data.frame(data))
#   
#   if (!variable %in% names(data))
#     stop(sQuote(variable), " is not a column name of 'data'")
#   
#   pwd <- data[[variable]] |>
#     unique() |>
#     na.omit() |>
#     as.character() |>
#     factor(levels = options,
#            ordered = TRUE)
#   
#   data.frame(id = sort(as.integer(pwd)), response = levels(pwd))
# }



link_db_tables <- function(x, y, by.x, by.y = NULL, ref.col = NULL, db = NULL) {
  
  if (is.character(y)) {
    
    if (length(y) > 1L)
      stop("'y' must be a single string")
    
    if (is.null(db))
      stop(sQuote(y),
           " is a string (i.e. a table is being read) but 'db' is NULL")
    
    y <- read_from_db(db, y)
  }
  
  if (is.null(by.y)) 
    by.y <- by.x
  
  if (is.null(ref.col)) 
    ref.col <- by.x
  
  ndf <- merge(x, y, by.x = by.x, by.y = by.y, all = TRUE)
  ndf[[by.x]] <- NULL
  names(ndf)[match("id", names(ndf))] <- ref.col
  ndf
}



create_multiresponse_group <-
  function(rgx,
           label.tbl,
           bridge.tbl = NULL,
           data,
           db) {
    lbls <- get_labels_via_rgx(data, rgx)
    
    if (is.null(lbls))
      stop("There no labels for this group")
    
    lbl.df <- data.frame(name = lbls)
    print(append_to_db(lbl.df, label.tbl, db))
    
    if (is.null(bridge.tbl))
      return(invisible())
    
    nlbl.df <- read_from_db(db, label.tbl)
    fac.lbl.df <- select(data, facility_id, matches(rgx))
    names(fac.lbl.df) <- c("facility_id", lbls)
    
    df <- fac.lbl.df %>%
      pivot_longer(2:last_col()) %>%
      filter(value != 0L) %>%
      left_join(nlbl.df, by = "name") %>%
      select(-c(name, value)) %>%
      rename(opt_id = id)
    
    append_to_db(df, bridge.tbl, db)
  }



create_singleresponse_tbl <- function(col, tblname, data, db) {
  stopifnot({
    is.character(col)
    is.character(tblname)
    is.data.frame(data)
  })
  
  val <- if (length(col) > 1L)
    col
  else
    unique(data[[col]])
  
  vals <- val |>
    na.omit() |>
    as.character()
  
  df <- data.frame(name = vals)
  
  append_to_db(df, tblname, db)
}





update_linked_tables <-
  function(fctnames,
           tblnames,
           refcolnames,
           data,
           db,
           scrublist = NULL,
           insertions = NULL,
           ...) {
    argd <- 
      data.frame(var = fctnames, tbl = tblnames, refcol = refcolnames)
    
    for (i in seq(nrow(argd))) {
      x <- argd$var[i]
      indx <- match(x, names(data))
      
      if (length(indx) > 1L)
        stop(sQuote(x, " matches more than one variable in 'data'"))
      
      y <- argd$tbl[i]
      indy <- match(y, names(insertions))
      insert <- NULL
      
      if (!is.na(indy)) {
        nmi <- names(insertions)[indy]  # TODO: Refactor
        insert <- insertions[[indy]]
        names(insert) <- nmi
      }
      
      data[[indx]] <-
        .createRefCol(x, y, data, db, scrub = scrublist, insert = insert, ...)
      names(data)[indx] <- argd$refcol[i]
    }
    data
  }



# Converts a column with categorical variables into one that has
# integer values, referencing a corresponding table from the database
.createRefCol <-
  function(colname,
           tblname,
           data,
           db,
           scrubs = NULL,
           insert = NULL,
           manual = FALSE
  ) {
    tab <- jGBV::read_from_db(db, tblname) |>
      dplyr::arrange(id)
    
    # Update the table in the DB when there is a category
    # introduced by the incoming data
    if (!is.null(insert)) {
      dest.tbl <- names(insert)
      
      if (!identical(tblname, dest.tbl))
        stop(
          "Value(s) ",
          sQuote(paste(insert, sep = ", ")),
          " to be into the ",
          sQuote(dest.tbl),
          " table, not ",
          sQuote(tblname)
        )
      
      tryCatch({
        message("Updating the table ", sQuote(tblname), "... ",
                appendLF = FALSE)
        append_to_db(data.frame(name = insert), tblname, db)
        message("Done")
      }, 
      error = function(e) {
        message("Failed")
        warning(e, call. = FALSE)
      })
    }
    
    col <- data[[colname]]
    isFactor <- inherits(col, "factor")
    
    dfcats <-
      if (isFactor)
        levels(col)
    else
      unique(col)
    
    dbcats <- tab$name
    
    if (!all(na.exclude(dfcats) %in% dbcats)) {
      
      if (manual) {
        
        while (TRUE) {
          y <- -1L
          x <- menu(dfcats, title = "Value to be replaced: ")
          
          if (x) {
            prompt <- sprintf("Value to replace %s with: ", sQuote(colname))
            y <- menu(dbcats, title = prompt)
          }
          
          if (!x || !y) {
            message("Exited menu-based editing")
            break
          }
          
          x.val <- dfcats[x]
          col[col %in% x.val] <- dbcats[y]
          dfcats <- dfcats[-x]
        }
      }
      else if (!is.null(scrublist)) {
        
        scrub <- scrubs[[tblname]]
        
        for (i in seq(nrow(scrub))) {
          pat <- scrub[i, 1]
          rep <- scrub[i, 2]
          
          if (pat == "" && rep == "")
            next
          
          message("Replacing ", sQuote(pat), " with ", sQuote(rep))
          col <- gsub(pat, rep, col)
        }
      }
    }
    
    # The end product is an integer vector, 
    # derived from a factor
    if (!isFactor)
      col <- factor(col, dbcats)
    
    as.integer(col)
  }






make_boolean <- function(x) {
  stopifnot(is.character(x))
  ifelse(x == "Yes", 1L, 0L)
}





filter_alldata <- function(data, service.col, selected, bools = NULL) {
  require(rlang, quietly = TRUE)
  
  if (!is.data.frame(data))
    stop("'data' must be a data frame")
  
  srvcol <- enexpr(service.col)
  
  ret <- data %>%
    filter(!!srvcol == 1) %>%
    select(facility_id, all_of(selected))
  
  if (is.null(bools))
    return(ret)
  
  mutate(ret, across(all_of(bools), make_boolean))
}




drop_db_table <- function(tblname) {
  stmt <- sprintf("DROP TABLE IF EXISTS %s;", tblname)
  
  tryCatch({
    message("Dropping table ", sQuote(tblname), "... ", appendLF = FALSE)
    dbcon <- dbConnect(SQLite(), dbpath)
    on.exit(dbDisconnect(dbcon))
    r <- dbSendQuery(dbcon, stmt)
    dbClearResult(r)
    message("Done")
  },
  error = function(e) {
    message("Failed")
    warning(conditionMessage(e))
  })
}


# Makes changes to selected variables in the data frame
# depending on the project being worked on
transform_mismatched <- function(data, projectname) {
  stopifnot({
    is.data.frame(data)
    is.character(projectname)
  })
  
  tryCatch({
    
    within(data, {
  
      if (projectname == "NFWP") {
        policefee_case = as.double(policefee_case)
        policefee_safety = as.double(policefee_safety)
        psychfee_therapy = as.double(psychfee_therapy)
        psychfee_safety = as.double(psychfee_safety)
        psychfee_other = as.double(psychfee_other)
        policefee_other = as.double(policefee_other)
      }
      else if (projectname == "NEDC") {
        healthfee_other = as.double(healthfee_other)
        uuid = as.character(uuid)
        notes = as.character(notes)
        submitted_by = as.character(submitted_by)
        today = jGBV::make_date(today)
        gps_lat = as.double(gps_lat)
        gps_long = as.double(gps_long)
        gps_alt = as.double(gps_alt)
        gps_prec = as.double(gps_prec)
        started_ops = jGBV::make_date(started_ops)
        started_gbv = jGBV::make_date(started_gbv)
        legalfee_med = as.double(legalfee_med)
        legalfee_counsel = as.double(legalfee_counsel)
        policefee_case = as.double(policefee_case)
        policefee_safety = as.double(policefee_safety)
        tags = as.character(tags)
      }
      else
        stop("No project called ", sQuote(projectname))
    })
  }, 
  error = function(e) {
    warning(conditionMessage(e), call. = FALSE)
    data
  })
}






# Activates focus project by applying the project options,
# returning them for use in this project
get_project_options <- function(dir, reset = FALSE) {
  
  if (!dir.exists(dir))
    stop("No directory ", sQuote(dir), " found")
  
  if (!is.logical(reset))
    stop("'reset' must be a logical value")
  
  curr.opts <- options()
  source(file.path(dir, ".Rprofile"), chdir = TRUE)
  
  jOpts <- lapply(
    c(
      "jgbv.project.name",
      "jgbv.project.year",
      "jgbv.multiresponse.regex",
      "jgbv.project.states"
    ),
    getOption
  ) |>
    setNames(c("name", "year", "regex", "states"))
  
  if (!reset)
    options(curr.opts)
  
  renv::load(quiet = TRUE)
  
  jOpts
}




combine_project_data <- function(name, states, database) {
  require(dplyr, warn.conflicts = FALSE)
  
  states |>
    lapply(function(s) {
      
      df <- loadData(database, s) |>
        lapply(function(c) {
          
          if (all(is.na(c)))
            rep(NA_character_, length(c))
          else
            c
          
        }) |>
        bind_cols()
      
      transform_mismatched(df, name)
      
    }) |>
    bind_rows() |>
    as_tibble()
}




inspect_data <- function(base, new) {
  
  lapply(names(base), function(name) {
    bcol <- base[[name]]
    
    if (!is.character(bcol))
      return()
    
    bval <- unique(bcol)
    nval <- unique(new[[name]])
    
    lbrk <- "\n"
    cat(sprintf("Variable: %s%s", name, lbrk))
    cat("* Base:", paste(bval, collapse = lbrk), sep = lbrk, fill = TRUE)
    cat("* New:", paste(nval, collapse = lbrk), sep = lbrk)
    
    readline("Press any ENTER... ")
  }) 
}


scrublist <- function(context = NULL, tables = character(), insert = NULL) {
  slist <- list(
    facility = list(
      cbind(
        c(
          "adults_and_children",
          "only_adults__18_and_over",
          "only_children__under_18"
        ),
        c(
          "Adults and children",
          "Only adults",
          "Only children"
        )
      ),
      cbind(
        c(
          "governmental__please_specify_m|Governmental (Please specify ministry or service)",
          "international_ngo",
          "other__describe",
          "faith_based_organization",
          "national_ngo",
          "community_based_organization"
        ),
        c(
          "Governmental ",
          "International NGO",
          "Other",
          "Faith-based organization",
          "National NGO",
          "Community-based organization"
        )
      ),
      cbind(
        c("yes__open_24_7", "no__only_open_during_certain_h"),
        c("Yes, open 24/7", "No, only open during certain hours")
      ),
      cbind(
        "^No.*$", 
        "No, respondent is unable to show them"
      ),
      cbind(
        c(
          "only_physical_data_are_stored",
          "both_electronic_and_physical_s",
          "only_electronic_storage_of_dat"
        ),
        c(
          "Only physical data are stored",
          "Both electronic and physical storage of data",
          "Only electronic storage of data"
        )
      ),
      cbind("Don_t_know", "Don't Know"),
      cbind("", ""),
      cbind("^Yes.*$", "Yes, always"),
      cbind("", ""),
      cbind(
        c(
          "every_six_months_or_less",
          "it_has_never_been_updated",
          "every_year",
          "more_than_a_year"
        ),
        c(
          "Every six months or less",
          "It has never been updated",
          "Every year",
          "More than a year"
        )
      ),
      cbind("Don_t_know", "Don't Know"),
      cbind("", ""),
      cbind("", ""),
      cbind("", "")
    ),
    
    legal = list(
      cbind("Pay.+", "Paid"),
      cbind(
        c("the_case_is_transferred_to_ano",
          "other",
          "the_survivor_is_asked_to_pay",
          "the_case_is_closed"),
        c("The case is transferred to another organization",
          "Other",
          "The survivor is asked to pay",
          "The case is closed")
      )
    )
  )
  
  context <- if (is.null(context))
    "facility"
  else
    tolower(context)
    
  obj <- structure(slist[[context]], names = tables)
  
  # Add up the values used to update the DB table
  # so that they are used in the comparisons
  # if (!is.null(insert)) {
  #   nm <- names(insert)
  #   obj[[nm]] <- c(obj[[nm]], insert)
  # }
  obj
}




# Gives a data frame project IDs which are to be used in the 
# database table
apply_project_id <- function(df, db, proj.name, proj.id.name = "proj_id") {
  if (!is.data.frame(df))
    stop("df' must be a data frame")
  
  if (!file.exists(db))
    stop(sQuote(db), "does not exist")
  
  if (!is.character(proj.name) || !is.character(proj.id.name))
    stop("'proj.name' or 'proj.id.name' must of type 'character'")
  
  proj <- read_from_db(db, "Projects")
  proj.id.no <- proj$id[proj$name == proj.name]
  df[[proj.id.name]] <- proj.id.no
  df
}
