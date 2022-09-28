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
  require(RSQLite, quietly = TRUE)
  
  if (!file.exists(db))
    stop("'db' does not exist")
  
  if (!is.character(qry) || length(qry) > 1L)
    stop("'qry' must be a character vector of length 1")
  
  tryCatch({
    dbcon <- dbConnect(SQLite(), db)
    r <- dbSendStatement(dbcon, qry)
    res <- dbFetch(r)
  },
  error = function(e) {
    stop(e, call. = FALSE)
  },
  finally = {
    if (exists('r', envir = environment()))
      dbClearResult(r)
    
    dbDisconnect(dbcon)
  })
  
  invisible(res)
}




# Fetches the elements with the same name
extract_elements <- function(x, name) {
  if (length(x) == 0L)
    stop("Cannot index into a zero-length vector")
  
  names.x <- names(x)
  
  if (is.null(names.x))
    stop("'x' is not named")
  
  if (length(name) > 1L) {
    name <- name[1]
    warning("Only the first element of 'name' was used")
  }
  
  if (!name %in% names.x) {
    if (interactive())
      warning(sQuote(name), " is not a name in ", sQuote(x))
    return()
  }
  
  i <- grep(name, names.x)
  
  if (max(i) > length(x) || sign(min(i)) == -1)
    stop("Subscript is indexing beyond the bounds of 'x'")
  
  x[i]
}




make_pivot_tabledf <-
  function(data,
           var.indices,
           serv.type = NULL,
           id = 'Facility') {
    
  stopifnot(is.data.frame(data))
  nvn <- jGBV::new.varnames
  
  if (!all(var.indices %in% names(nvn)))
    stop("'var.indices' must be names of 'jGBV::new.varnames'")
  
  if (!is.null(serv.type)) {
    if (!is.character(serv.type) || length(serv.type) != 1L)
      stop("'serv.type' supplied is not a string")
    
    expected.start <- "srvtyp"
    
    if (!startsWith(serv.type, expected.start))
      stop(paste(
        "'serv.type' is not a variable starting with",
        sQuote(expected.start)
      ))
  }
  
  varnames <- .add_id_name(var.indices, id)
  bools <- extract_elements(var.indices, 'bool')
  .filter_alldata(data, serv.type, varnames, bools)
}





# we set this to reference other tables
set_id_col <- function(data, db, id) {
  stopifnot(is.data.frame(data), file.exists(db))
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  
  id.str <- .mk_id(id, "_")
  qry <- sprintf("SELECT MAX(%s) FROM %s;", id.str, id)
  result <- query_db(db, qry)
  nrw <- result[1, 1]
  
  if (is.na(nrw))
    nrw <- 0L
  
  data %>% 
    mutate(facility_id = nrw + seq(nrow(.)))
}




# Starts off the field names with the 'facility_id'
.add_id_name <- function(x, id) {
  if (!is.character(x))
    stop("The indices used are character strings")
  
  if (is.null(id))
    y <- id
  else {
    y <- .mk_id(id, "_")
    names(y) <- .mk_id(id, ".")
  }
  
  vars <- jGBV::new.varnames[x]
  c(y, vars)
}



# Creates a string for an id_column
.mk_id <- function(id, sep) {
  stopifnot({
    is.character(id)
    is.character(sep)
  })
  
  paste(tolower(id), 'id', sep = sep)
}




.filter_alldata <- function(data, service.col, selected, bools = NULL) {
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  if (!is.data.frame(data))
    stop("'data' must be a data frame")
  
  if (!is.null(service.col)) {
    filtered.rows <- data[[service.col]] == 1
    data <- data[filtered.rows,]
  }
  
  # If we don't unname `selected` here, it's names
  # are passed on to the selected columns, thereby
  # distorting the original names of the structure.
  usel <- unname(selected)
  data <- select(data, all_of(usel))
  
  if (is.null(bools))
    return(data)
  
  # The boolean-designate columns are fished out using
  # the names of the monitoring variable jGBV::new.varnames,
  # which at this pointed is in a shorter version, the object
  # `selected`.
  ubools <- unname(selected[bools])
  mutate(data, across(all_of(ubools), make_boolean))
}





# Appends data to already created database tables
append_to_dbtable <- function(df, tbl, db) {
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
    stop(sprintf("The column names in the table and the input data differ"))
  
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
  jGBV::get_var_labels(df, grep(rgx, names(df)))
}



# Prepares the indices used to extract specific variables
# from the data. They are named with keys that represent 
# what kind of field they represent:
# - fkey: Foreign Keys
# - bool: Binary (binomial) variables coded as 1/0
# - field: Regular fields
set_variable_indices <- function(fkey = NULL, bool = NULL, field = NULL) {
  args <- list(fkey = fkey, bool = bool, field = field)
  custom.attr <- 'tag'
  
  keys <- purrr::imap(args, function(.x, .y) {
    if (is.null(.x))
      return()
    
    attr(.x, custom.attr) <- .y
    .x
  })
  
  spreadName <- function(x, y) {
    .f <- function(i) {
      if (!is.null(names(i)))
        return(i)
      
      name <- attr(i, which = custom.attr)
      setNames(i, rep(name, length(i)))
    }
    
    c(.f(x), .f(y))
  }
  
  Reduce(spreadName, keys, accumulate = FALSE)
}



# Creates a matrix that contains, in columnwise fashion, the elements
# for linking together tables with categories with the main table. The
# columns are:
# - name of the table
# - indices for retrieving the variable that populates the table
# - optionally addition column(s) and most commonly the name of new ID columns
table_info_matrix <- function(..., proj.opts, type = c('factor', 'regex')) {
  if (length(unique(sapply(list(...), length))) != 1L)
    stop("All elements of `...` must have the same length")
  
  type <- match.arg(type)
  mt <- cbind( ...)
  
  .fun <- function(option) {
    ind <-
      purrr::map_lgl(as.data.frame(mt), ~ all(.x %in% names(option)))
    
    if (sum(ind) > 1L)
      stop("Malformed object created; there should only be one 'index' column")
    
    col <- mt[, ind]
    option[col]
  }
  
  opt <- with(proj.opts, switch(type, factor = vars, regex = var.regex))
  mt <- cbind(mt, .fun(opt))
  colnames(mt) <- sub("^$", type, colnames(mt))
  rownames(mt) <- NULL
  mt
}




# Takes updates the main table by creating the linkages with reference tables.
# The internal implementation for NFWP differs from all the other projects.
unite_main_with_ref_data <- function(data, tableinfo, proj.opts, db, ...) {
  stopifnot({
    is.data.frame(data)
    is.matrix(tableinfo)
    file.exists(db)
  })
  
  y.tables <- tableinfo[, 'tablename']
  by.x <- tableinfo[, 'factor']
  ref.col <- tableinfo[, 'refcolumn']
  
  if (proj.opts$proj.name == "NFWP") {
    dots <- list(...)
    
    drop <- NULL
    
    if (all('drop' %in% names(dots)))
      drop <- dots$drop
    
    for (i in seq_along(y.tables)) {
      data <-
        link_db_tables(data,
                       y.tables[i],
                       by.x[i],
                       ref.col[i],
                       db,
                       drop = drop)
    }
    
    return(data)
  }
  
  update_linked_tables(data, y.tables, by.x, ref.col, db)
}



link_db_tables <-
  function(x,
           y,
           by.x,
           ref.col = NULL,
           db = NULL,
           drop = NULL) {
  
  if (is.character(y)) {
    
    if (length(y) > 1L)
      stop("'y' must be a single string")
    
    if (is.null(db))
      stop(sQuote(y),
           " is a string (i.e. a table is being read) but 'db' is NULL")
    
    y <- jGBV::read_from_db(db, y)
  }
  
  if (is.null(ref.col)) 
    ref.col <- by.x
  
  if (!is.null(drop))
    y[, drop] <- NULL
  
  ndf <- merge(x, y, by.x = by.x, by.y = 'name', all = TRUE, all.y = FALSE)
  ndf[[by.x]] <- NULL
  
  
  names(ndf)[match("id", names(ndf))] <- ref.col
  ndf
}





# Collectively works on updating bridge tables, which exist for
# the benefit of the multiple response data
update_bridge_tables <- function(data, tableinfo, db) {
  stopifnot({
    is.data.frame(data)
    is.matrix(tableinfo)
    file.exists(db)
  })
  
  len <- nrow(tableinfo)
  tlist <- sapply(c('regex', 'tablename', 'bridge'),
                  function(n) unname(tableinfo[, n]),
                  simplify = FALSE)
  cols <- tlist$regex
  tables <- tlist$tablename
  bridges <- tlist$bridge
  
  for (i in seq(len))
    create_multiresponse_group(data, db, cols[i], tables[i], bridges[i])
}




create_multiresponse_group <-
  function(data,
           db,
           rgx,
           label.tbl,
           bridge.tbl = NULL) {
    require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
    require(tidyr, quietly = TRUE)
    
    lbls <- get_labels_via_rgx(data, rgx)
    
    if (is.null(lbls))
      stop("There no labels for this group")
    
    lbl.df <- data.frame(name = lbls)
    print(append_to_dbtable(lbl.df, label.tbl, db))
    
    if (is.null(bridge.tbl))
      return(invisible())
    
    nlbl.df <- jGBV::read_from_db(db, label.tbl)
    serv.df <- select(data, facility_id, matches(rgx))
    names(serv.df) <- c("facility_id", lbls)
    
    df <- serv.df %>%
      pivot_longer(2:last_col()) %>%
      filter(value != 0L) %>%
      left_join(nlbl.df, by = "name") %>%
      select(-c(name, value)) %>%
      rename(opt_id = id)
    
    append_to_dbtable(df, bridge.tbl, db)
  }




update_many_singleresponse_tbls <- function(data, tableinfo, db) {
  stopifnot({
    is.data.frame(data)
    is.matrix(tableinfo)
    file.exists(db)
  })
  
  v <- tableinfo[, 'factor']
  t <- tableinfo[, 'tablename']
  
  for (i in seq(nrow(tableinfo)))
    update_singleresponse_tbl(data, v[i], t[i], db)
}




update_singleresponse_tbl <- function(data, col, tblname, db) {
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
  append_to_dbtable(df, tblname, db)
}





update_linked_tables <-
  function(data,
           tblnames,
           fctnames,
           refcolnames,
           db,
           scrublist = NULL,
           insertions = NULL,
           ...) {
    argd <- 
      data.frame(var = fctnames, tbl = tblnames, refcol = refcolnames)
    
    if (!is.null(scrublist)) {
      
      if (!is.vector(scrublist, "list"))
        stop("'scrublist' must be a list")
      
      if (!identical(length(scrublist), length(fctnames)))
        stop(" scrublist' and 'fctnames' must have the same length")
    }
    
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
        .createRefCol(x, y, data, db, scrubs = scrublist[[y]], insert = insert, ...)
      
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
        append_to_dbtable(data.frame(name = insert), tblname, db)
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
      else if (!is.null(scrubs)) {
        
        for (i in seq(nrow(scrubs))) {
          pat <- scrubs[i, 1]
          rep <- scrubs[i, 2]
          
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




drop_db_table <- function(tblname, db) {
  require(RSQLite, quietly = TRUE)
  stmt <- sprintf("DROP TABLE IF EXISTS %s;", tblname)
  
  tryCatch({
    message("Dropping table ", sQuote(tblname), "... ", appendLF = FALSE)
    dbcon <- dbConnect(SQLite(), db)
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
    stop(e)
  })
}






# Activates focus project by applying the project options,
# returning them for use in this project
fetch_proj_options <- function(dir, reset = FALSE) {
  
  if (!dir.exists(dir))
    stop("No directory ", sQuote(dir), " found")
  
  if (!is.logical(reset))
    stop("'reset' must be a logical value")
  
  curr.opts <- options()
  source(file.path(dir, ".Rprofile"), chdir = TRUE)
  
  jOpts <- lapply(
    c("jgbv.project.name",
      "jgbv.project.year",
      "jgbv.multiresponse.regex",
      "jgbv.project.states"), 
    getOption
    ) |>
    setNames(c("proj.name", "proj.year", "var.regex", "proj.states"))
  
  if (!reset)
    options(curr.opts)
  
  renv::load(quiet = TRUE)
  
  jOpts
}




combine_project_data <- function(proj.dir, proj.opts) {
  require(purrr, quietly = TRUE, warn.conflicts = FALSE)
  require(labelled, quietly = TRUE)
  
  if (!dir.exists(proj.dir))
    stop(sQuote(proj.dir), " does not contain a JHPIEGO GBV project")
  
  db.fname <- paste0(tolower(basename(proj.dir)), ".db")
  database <- file.path(proj.dir, "data", db.fname)
  
  if (!file.exists(database))
    stop("The database ", sQuote(database), " does not exist")
  
  states <- proj.opts$proj.states
  
  comb <- states |>
    map_dfr(function(s) {
      
      df <- loadData(database, s) |>
        map_dfc(function(c) {
          
          if (all(is.na(c)))
            rep(NA_character_, length(c))
          else
            c
          
        })
      
      transform_mismatched(df, proj.opts$proj.name)
    }) |>
    dplyr::as_tibble()

  one.s <- jGBV::load_data(database, states[1])
  var_label(comb) <- var_label(one.s, unlist = TRUE)
  comb
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
      AgeGrp = cbind(
        c(
          "adults_and_children",
          "only_adults__18_and_over",
          "only_children__under_18"
        ),
        c("Adults and children",
          "Only adults",
          "Only children")
      ),
      OrgTypes = cbind(
        c(
          "governmental__please_specify_m|Governmental \\(Please specify ministry or service\\)",
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
      OpenAccess = cbind(
        c("yes__open_24_7", "no__only_open_during_certain_h"),
        c("Yes, open 24/7", "No, only open during certain hours")
      ),
      DocsAreShown = cbind("^No.*$",
                           "No, respondent is unable to show them"),
      DataStorage = cbind(
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
      PrivateQuesOpts = cbind("Don_t_know", "Don't Know"),
      Electronicstore = cbind("", ""),
      ContactAuthority = cbind("^Yes.*$", "Yes, always"),
      SignedCOC = cbind("", ""),
      UpdateRefdir = cbind(
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
      ChooseTreatment = cbind("Don_t_know", "Don't Know"),
      States = cbind("", ""),
      LGAs = cbind("", ""),
      Devices = cbind("", "")
    ),
    
    legal = list(
      CostOpts = cbind("Pay.+", "Paid"),
      ActionNoresrc = cbind(
        c(
          "the_case_is_transferred_to_ano",
          "other",
          "the_survivor_is_asked_to_pay",
          "the_case_is_closed"
        ),
        c(
          "The case is transferred to another organization",
          "Other",
          "The survivor is asked to pay",
          "The case is closed"
        )
      )
    )
  )
  
  context <- if (is.null(context))
    "facility"
  else
    tolower(context)
    
  slist[[context]]
}




# Gives a data frame project IDs which are to be used in the 
# database table
apply_project_id <- function(df, db, proj.name, proj.id.name = "project_id") {
  if (!is.data.frame(df))
    stop("df' must be a data frame")
  
  if (!file.exists(db))
    stop(sQuote(db), "does not exist")
  
  if (!is.character(proj.name) || !is.character(proj.id.name))
    stop("'proj.name' or 'proj.id.name' must of type 'character'")
  
  proj <- jGBV::read_from_db(db, "Projects")
  proj.id.no <- proj$id[proj$name == proj.name]
  df[[proj.id.name]] <- proj.id.no
  df
}


## Uses a given view to create a separate database file that can be used to
## export the app to a remote server.
get_view <- function(db, view)
{
  require(RSQLite, quietly = TRUE)
  
  if (!file.exists(db))
    stop("'db' does not exist")
  
  tryCatch({
    con <- dbConnect(SQLite(), db)
    tbl <- dbReadTable(con, view)
  }, finally = dbDisconnect(con))
  
  tryCatch(
    error = function(e)
      stop(e),
    {
      db2 <- here::here(sprintf("app/%s.db", tolower(view)))
      con2 <- dbConnect(SQLite(), db2)
      dbWriteTable(con2, view, tbl)
    },
    finally = dbDisconnect(con2)
  )
  invisible(db2)
}
