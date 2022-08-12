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
  
  if (!identical(dfnames, names(dd)))
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



prepare_ref_table <- function(variable, options, data = alldata) {
  stopifnot(is.character(options), is.data.frame(data))
  
  if (!variable %in% names(data))
    stop(sQuote(variable), " is not a column name of 'data'")
  
  pwd <- data[[variable]] |>
    unique() |>
    na.omit() |>
    as.character() |>
    factor(levels = options,
           ordered = TRUE)
  
  data.frame(id = sort(as.integer(pwd)), response = levels(pwd))
}



link_db_tables <- function(x, y, by.x, by.y = NULL, ref.col = NULL) {
  if (is.character(y))
    y <- read_from_db(dbpath, y) # Note that this is an impure function. See def.
  
  if (is.null(by.y)) 
    by.y <- by.x
  
  if (is.null(ref.col)) 
    ref.col <- by.x
  
  ndf <- 
    merge(x, y, by.x = by.x, by.y = by.y, all = TRUE)
  
  ndf[[by.x]] <- NULL
  names(ndf)[match("id", names(ndf))] <- ref.col
  
  ndf
}



create_multiresponse_group <-
  function(rgx,
           label.tbl,
           bridge.tbl = NULL,
           data) {
    lbls <- get_labels_via_rgx(data, rgx)
    
    if (is.null(lbls))
      stop("There no labels for this group")
    
    lbl.df <- data.frame(name = lbls)
    print(try(append_to_db(lbl.df, label.tbl, dbpath)))
    
    if (is.null(bridge.tbl))
      return(invisible())
    
    nlbl.df <- read_from_db(dbpath, label.tbl)
    fac.lbl.df <- select(data, facility_id, matches(rgx))
    names(fac.lbl.df) <- c("facility_id", lbls)
    
    df <- fac.lbl.df %>%
      pivot_longer(2:last_col()) %>%
      filter(value != 0L) %>%
      left_join(nlbl.df, by = "name") %>%
      select(-c(name, value)) %>%
      rename(opt_id = id)
    
    try(append_to_db(df, bridge.tbl, dbpath))
  }



create_singleresponse_tbl <- function(col, tblname, data) {
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
  
  append_to_db(df, tblname, dbpath)
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
      "jgbv.new.varnames",
      "jgbv.multiresponse.regex",
      "jgbv.project.states"
    ),
    getOption
  ) |>
    setNames(c("name", "year", "vars", "regex", "states"))
  
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
    
    cat("Variable:\n")
    cat("* Base:", paste(bval, collapse = "\n"), fill = TRUE)
    cat("* New:", paste(nval, collapse = "\n"))
    
    readline("Press any ENTER... ")
  }) 
}