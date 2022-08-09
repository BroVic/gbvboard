# Source file: initdb.R
#
# License: MIT
#
# Copyright (c) Victor Ordu 2022

# R script for the initial population of the created DB tables with
# data from the project. We are using cleaned/transformed data 

# Dependencies ----
library(tidyr)
library(labelled)
library(jGBV, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE)
suppressPackageStartupMessages(library(here))

params <- commandArgs(trailingOnly = TRUE)

dir <- if (interactive()) {
  rs.proj <- file.choose()
  if (!endsWith(rs.proj, ".Rproj"))
    stop("Selected file is not an RStudio project file")
  dirname(rs.proj)
} else {
  params[1]
}

if (is.na(dir) || identical(dir, ""))
  stop("No project directory chosen")
if (!dir.exists(dir))
  stop("Directory", sQuote(dir), "does not exist")
dir <- normalizePath(dir, winslash = "/")
source(file.path(dir, ".Rprofile"))


# Options ----
projOpts <-
  list(
    name = getOption("jgbv.project.name"),
    year = getOption("jgbv.project.year"),
    vars = getOption("jgbv.new.varnames"),
    regex = getOption("jgbv.multiresponse.regex"),
    states = getOption("jgbv.project.states")
  )


# Functions ----
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
      tbl <- tolower(paste(state, type, sep = "_"))
      RSQLite::dbReadTable(con, tbl)
    })
  }


# Appends data to already created database tables
append_to_db <- function(df, tbl, db) {
  require(RSQLite, quietly = TRUE)
  
  tryCatch({
    cat("Populating table", sQuote(tbl), "... ")
    con <- dbConnect(SQLite(), db)
    
    if (.checkExistingData(con, df, tbl)) {
      warning(sprintf("The data in '%s' already exists in table %s",
                   deparse(substitute(df)),
                   tbl),
              call. = FALSE)
    }
    old <- .fetchDbTable(con, tbl)
    if (nrow(old))
      dbAppendTable(con, tbl, df)
    else
      dbWriteTable(con, tbl, df, append = TRUE)
    cat("Done\n")
  }, 
  error = function(e) {
    cat("Failed\n")
    warning(conditionMessage(e), call. = FALSE)
  },
  warning = function(w) {
    cat("Skipped.", sQuote(tbl), "already exists\n")
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
    df <- unique(data)  # deal with duplicate records in original data
    warning("Duplicate rows were found and removed") # TODO: Quantify.
  }
  dfx <- rbind(dd, head(df))
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
           data = alldata) {
    lbls <- get_labels_via_rgx(data, rgx)
    if (is.null(lbls))
      stop("There no labels for this group")
    lbl.df <- data.frame(name = lbls)
    print(try(append_to_db(lbl.df, label.tbl, dbpath)))
    
    if (is.null(bridge.tbl))
      return(invisible())
    
    nlbl.df <- read_from_db(dbpath, label.tbl)
    fac.lbl.df <- select(alldata, facility_id, matches(rgx))
    names(fac.lbl.df) <- c("facility_id", lbls)
    
    df <- fac.lbl.df %>%
      pivot_longer(2:last_col()) %>%
      filter(value != 0L) %>%
      left_join(nlbl.df, by = "name") %>%
      select(-c(name, value)) %>%
      rename(opt_id = id)
    
    try(append_to_db(df, bridge.tbl, dbpath))
  }



create_singleresponse_tbl <- function(col, tblname, data = alldata) {
  stopifnot({
    is.character(col)
    is.character(tblname)
    is.data.frame(data)
  })
  val <- if (length(col) > 1L) col else unique(data[[col]])
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



filter_alldata <- function(service.col, selected, bools = NULL) {
  require(rlang, quietly = TRUE)
  srvcol <- enexpr(service.col)
  ret <- alldata %>%
    filter(!!srvcol == 1) %>%
    select(facility_id, all_of(selected))
  if (is.null(bools))
    return(ret)
  mutate(ret, across(all_of(bools), make_boolean))
}




drop_db_table <- function(tblname) {
  stmt <- sprintf("DROP TABLE IF EXISTS %s;", tblname)
  dbcon <- dbConnect(SQLite(), dbpath)
  on.exit(dbDisconnect(dbcon))
  
  tryCatch({
    cat("Dropping table", sQuote(tblname), "... ")
    r <- dbSendQuery(dbcon, stmt)
    dbClearResult(r)
    cat("Done\n")
  },
  error = function(e) {
    cat("Failed\n")
    warning(conditionMessage(e))
  })
}



# Project Data ----
dbpath <- if (interactive()) {
  file.choose()
} else {
  message("Using the NFWP database by default")
  params[2]
}
    

states <- projOpts$states

alldata <- states %>% 
  lapply(\(state) {
    # Old code used purrr::map_df. This workaround is used
    # to circumvent a type disparity in the variable being
    # transformed below.
    rems <- removed_variables(state)
    vars <- projOpts$vars
    
    if (!is.null(rems))
      vars <- vars[-rems]
    
    browser()
    d <- load_data(dbpath, state, vars = vars)
    d <- transform(
      d, 
      descr_oth_sheltamen = as.character(descr_oth_sheltamen)
    )
  }) |>
  bind_rows() |>
  as_tibble()

var_label(alldata) <- var_label(load_data(dbpath, states[1]), unlist = TRUE)
var.rgx <- as.list(projOpts$regex)

## Some of the variable are converted to factors by `loadData`; we don't 
## want this for the purposes of creating the database.
factorvars <-
  projOpts$vars[c("start",
                  "end",
                  "today",
                  "opstart",
                  "gbvstart",
                  "age",
                  "update.refdir")]

alldata <- alldata %>%
  mutate(facility_id = seq_len(nrow(.))) %>%  # we set this to reference other tables
  mutate(across(contains(factorvars), as.character))

# Table operations ----

## We start by populating the Project and States tables
proj <- data.frame(name = projOpts$name, year = projOpts$year)
try( append_to_db(proj, tbl = "Projects", dbpath) )

local({
  tables <- c("States", "LGAs", "Devices")
  variables <- projOpts$vars[c("state", "lga", "device.id")]
  invisible(Map(create_singleresponse_tbl, variables, tables))
})

# The Interviewers
local({
  ivars <- projOpts$vars
  interviewer <- alldata[, c(ivars['interviewer'], ivars['interviewer.contact'])]
  interviewer <- interviewer[!duplicated(interviewer[[1L]]), ]
  names(interviewer) <- c("name", "contact")
  append_to_db(interviewer, "Interviewer", dbpath)
})

# Facility-specific data
local({
  var.index <- 
    c(
      "start",
      "end",
      "today",
      "has.office",
      "has.phone",
      "continue",
      "consent",
      "orgname",
      "opstart",
      "gbvstart",
      "state",               # FK
      "lga",                 # FK
      "ward",
      "address",
      "phone",
      "email",
      "interviewer",         # FK
      "device.id",           # FK
      "staffname",
      "title",
      "info",
      "gps.long",
      "gps.lat",
      "gps.alt",
      "gps.prec",
      "age",                 # FK
      "org.type",            # FK
      "govt.spec",
      "oth.org.type",
      "open.247",
      "open.time",
      "close.time",
      "oth.gbv.dscrb",
      "oth.fund.dscrb",
      "fulltime.staff",
      "partime.staff",
      "female.staff",
      "uses.docs"   ,        # bool
      "showed.docs",         # FK
      "doc.photo",
      "oth.docs.dscrb",
      "process.nodoc",
      "child.docs",          # bool
      "standard.forms",      # bool
      "how.data",            # FK
      "data.is.stored",      # bool
      "computer.secured",    # FK 
      "contact.authority",   # FK
      "why.contact",     
      "contact.case",
      "contact.authtype",
      "priv",                # bool
      "priv.room",           # bool
      "private.ques",        # FK
      "details.miss.equip",
      "serve.disabled",      # bool
      "disabled.special",    # bool
      "oth.disabl.dscrb",
      "coc.signed",          # FK
      "coc.copies",          # bool
      "coc.confidentiality", # bool
      "coc.equity",          # bool
      "has.focalperson",     # bool
      "focalperson.contact",
      "has.gbv.trained",     # bool
      "num.gbv.trained",
      "who.gbv.trained",
      "which.gbv.trained",
      "has.refdir",          # bool
      "refdir.pic",
      "refto.health",        # FK
      "refto.psych",         # FK
      "refto.police",        # FK
      "refto.legal",         # FK
      "refto.shelt",         # FK
      "refto.econ",          # FK
      "refto.other",         # FK
      "oth.refto.dscrb",
      "update.refdir",       # FK
      "gbvcase.contact",
      "choose.referral",     # bool
      "why.nochoose.ref",
      "choose.treatment",    # FK
      "coordination",        # bool
      "which.coord",
      "comment.coord",
      "service.othersdetail"
    )
  varnames <- unname(projOpts$vars[var.index])
  facilcol <- c("facility_id", varnames)
  
  # Convert Y/N variables to binary values
  bools <- c(38, 43:44, 46, 52:53, 56:57, 60:63, 65, 69, 81, 84) + 1
  bools <- facilcol[bools]
  
  facdb <- alldata %>% 
    select(all_of(facilcol)) %>% 
    mutate(across(all_of(bools), make_boolean))
  
  tables <-
    c(
      "AgeGrp",
      "OrgTypes",
      "OpenAccess",
      "DocsAreShown",
      "DataStorage",
      "PrivateQuesOpts",
      "Electronicstore",
      "ContactAuthority",
      "SignedCOC",
      "UpdateRefdir",
      "ChooseTreatment",
      "ShowDocs"
    )
  var.index <- c(
    "age",
    "org.type", 
    "open.247",
    "showed.docs",
    "how.data",
    "private.ques",
    "computer.secured",
    "contact.authority",
    "coc.signed",
    "update.refdir",
    "choose.treatment",
    "showed.docs"
  )
  
  fields <- projOpts$vars[var.index]
  invisible( Map(create_singleresponse_tbl, fields, tables) )
  create_singleresponse_tbl(c("Always", "Sometimes", "Never"), "ReferralToOptions")
  
  ## NB: `link_db_tables` can work with a table name OR a session data frame!
  last.element <- length(tables)
  tables <- tables[-last.element]
  var.index <- var.index[-last.element]
  
  var.index <- c(var.index, "state", "lga", "device.id")
  y.tables <- c(tables, "States", "LGAs", "Devices")
  by.x <- unname(projOpts$vars[var.index])
  ref.col <-
    c(
      "agegrp_id",
      "orgtype_id",
      "open247_id",
      "docsareshown_id",
      "datastorage_id",
      "privateques_id",
      "datastorage_id",
      "elecstore_id", 
      "signedcoc_id",
      "updates_id",
      "choosetreat_id",
      "state_id",
      "lga_id",
      "device_id"
    )
  
  for (i in seq_along(y.tables))
    facdb <- link_db_tables(facdb, y.tables[i], by.x[i], "name", ref.col[i])
  
  # Merge reference tables with main one via their respective PK IDs
  interviewer.db <- read_from_db(dbpath, "Interviewer")
  interviewer.db$contact <- NULL  # column not needed for referencing
  facdb <-
    link_db_tables(facdb, interviewer.db, "interviewer_name", "name", "interviewer_id")
  
  # For those variables for "referrals to" i.e. always/sometimes/never
  for(i in grep("^refto_", names(alldata), value = TRUE))
    facdb <- 
      link_db_tables(facdb, "ReferralToOptions", i, "name", paste0(i, "_id"))
  
  append_to_db(facdb, "Facility", dbpath)
})


local({
  rgx.index <-
    c(
      "gbv.types",
      "equip.missing",
      "docs.used",
      "funding",
      "daysofweek",
      "service.type"
    )
  tables <-
    c("GBVtypes",
      "Missing",
      "Doctypes",
      "Funding",
      "Days",
      "ServiceType")
  
  bridges <-
    c(
      "GBVtypesFacility",
      "MissingFacility",
      "DoctypesFacility",
      "FundingFacility",
      "DaysFacility",
      "ServicetypeFacility"
    )
  
  for (i in seq_along(rgx.index))
    create_multiresponse_group(var.rgx[[rgx.index[i]]], tables[i], bridges[i])
})


# Health services
local({
  
  var.index <-  
    c(
      "hf.type",                   # FK
      "hf.type.others",
      "oth.srvhealth.dscrb",
      "total.health",
      "has.pep",                   # bool
      "has.no.pep",
      "has.contracep",             # bool
      "has.no.contracep",
      "health.paid",               # FK
      "access.srv",                # bool
      "healthfee.clin",
      "healthfee.inj",
      "healthfee.pep",
      "healthfee.contra",
      "healthfee.hiv",
      "healthfee.sti",
      "healthfee.foren",
      "healthfee.psych",
      "healthfee.case",
      "healthfee.basic",
      "healthfee.other",
      "forms.yes",                 # bool
      "comment.elem",
      "comment.suppl",
      "oth.hlthtrain.dscrb",
      "qual.staff"
    )
  varnames <- projOpts$vars[var.index]
  health.cols <- unname(c("facility_id", varnames))
  booleans <- health.cols[c(6, 8, 11, 23)]
  
  h.data <- filter_alldata(srvtype_health, health.cols, booleans)
  
  rgx.index <-
    c("health.services",
      "equip.missing",
      "supplies",
      "form.types",
      "trained.health")
  tables <-
    c("HealthServices",
      "Elements",
      "Medicines",
      "HealthForm",
      "TrainedHealth")
  bridges <-
    c(
      "HealthservicesFacility",
      "ElementsFacility",
      "MedicinesFacility",
      "HealthFormFacility",
      "TrainedHealthFacility"
    )
  
  for (i in seq_along(tables))
    create_multiresponse_group(var.rgx[[rgx.index[i]]], tables[i], bridges[i])
  
  variables <- projOpts$vars[c("hf.type", "health.paid")]
  tables <- c("HfType", "CostOpts")
  ref.col <- c("hftype_id", "healthfees_id")
  for (i in 1:2) {
    create_singleresponse_tbl(variables[i], tables[i], h.data)
    h.data <- link_db_tables(h.data, tables[i], variables[i], "name", ref.col[i])
  }
  
  append_to_db(h.data, "Health", dbpath)
  
})


# Legal Aid Services
local({
  var.index <-
    c(
      "oth.srvleg.dscrb",
      "total.legal",
      "legal.paid",                # FK
      "legal.access", 
      "legalfee.consult",
      "legalfee.rep",
      "legalfee.court",
      "legalfee.med",
      "legalfee.secur",
      "legalfee.counsel",
      "support.for.court",         # bool
      "no.resources1",             # FK
      "no.resources2"
    )
  varnames <- projOpts$vars[var.index]
  legcol <- unname(c("facility_id", varnames))
  booleans <- legcol[c(5, 12)]
  
  l.data <- filter_alldata(srvtype_legal, legcol, booleans)
  
  create_multiresponse_group(var.rgx[['legal.services']],
                             "LegalServices", 
                             "LegalservicesFacility")
  varnames <- unname(projOpts$vars['no.resources1'])
  create_singleresponse_tbl(varnames, "ActionNoresrc", l.data)
  
  tables <- c("CostOpts", "ActionNoresrc")
  var.index <- c("legal.paid", "no.resources1")
  ref.col <- c("legalfees_id", "noresource1_id")
  varnames <- unname(projOpts$vars[var.index])
  
  for (i in 1:2)
    l.data <- link_db_tables(l.data, tables[i], varnames[i], "name", ref.col[i])
  
  append_to_db(l.data, "Legal", dbpath)
})



# Psychosocial support
local({
  var.index <- 
    c(
      "oth.srvpsy.dscrb",
      "total.psychosocial",
      "psych.paid",              # FK
      "psych.access",
      "psychfee.counsel",
      "psychfee.case",
      "psychfee.therapy",
      "psychfee.safety",
      "psychfee.other",
      "oth.psychtrain.dscrb",
      "qualstaff.id"
    )
  varnames <- projOpts$vars[var.index]
  psy.col <- unname(c("facility_id", varnames))
  psy.data <- filter_alldata(srvtype_psych, psy.col)

  rgx.index <- c("psychosoc.services", "trained.psychosoc")
  tables <- c("PsychoServices", "PsychoTrain")
  bridges <- c("PsychoservicesFacility", "PsychotrainFacility")
  
  for (i in 1:2)
    create_multiresponse_group(var.rgx[[rgx.index[i]]], tables[i], bridges[i])
  
  psy.data <-
    link_db_tables(psy.data, "CostOpts", "psych_fees", "name", "psychfees_id")
  
  append_to_db(psy.data, "Psychosocial", dbpath)
  
})


# Security/Police
local({
  var.index <-
    c(
      "oth.srvpol.dscrb",
      "gbv.police",                 # bool
      "who.gbvpolice",
      "refer.otherpolice",          # bool
      "trainedpolice.id",
      "total.police.rape",
      "total.police.ipv",
      "total.police.csa",
      "total.police.fgm",
      "total.police.oth",
      "oth.polnum.dscrb",
      "police.fees",                # FK
      "police.access",
      "policefee.case",
      "policefee.safety",
      "policefee.other",
      "oth.policeresrc.dscrb",
      "police.followup",            # bool
      "police.confidential"
    )
  varnames <- projOpts$vars[var.index]
  pol.col <- unname(c("facility_id", varnames))
  booleans <- pol.col[c(3, 5, 19)]
  pol.data <- filter_alldata(srvtype_police, pol.col, booleans)
  
  rgx.index <-
    c("police.services", "trained.police", "resources.police")
  tables <- c("PoliceServices", "TrainedPolice", "PoliceResources")
  bridges <-
    c("PoliceservicesFacility",
      "TrainedpoliceFacility",
      "PoliceresourcesFacility")
  
  for (i in 1:3)
    create_multiresponse_group(var.rgx[[rgx.index[i]]], tables[i], bridges[i])
  
  pol.data <-
    link_db_tables(pol.data, "CostOpts", "police_fees", "name", "policefees_id")
  
  append_to_db(pol.data, "Police", dbpath)
  
})




# Temporary shelter
local({
  var.index <- 
    c(
      "health.srvshelt.dscrb",
      "oth.srvshelt.dscrb",
      "shelter.famfriendly",        # bool
      "shelter.kidfriendly",        # bool
      "oth.sheltpriv.dscrb",
      "oth.sheltamen.dscrb",
      "electricwater",              # FK
      "total.shelter.f",
      "total.shelter.m",
      "shelter.support",            # bool
      "shelter.new.support"         # bool
    )
  varnames <- unname(projOpts$vars[var.index])
  shel.col <- c("facility_id", varnames)
  booleans <- shel.col[c(4:5, 11:12)]
  shel.data <- filter_alldata(srvtype_shelt, shel.col, booleans)
  
  rgx.index <- 
    c("shelter.services", "privacy.shelter", "amenities.shelter")
  tables <- c("ShelterServices", "ShelterPrivacy", "ShelterAmenities")
  bridges <-
    c("ShelterservicesFacility",
      "ShelterprivacyFacility",
      "ShelteramenitiesFacility")
  
  for (i in 1:3)
    create_multiresponse_group(var.rgx[[rgx.index[i]]], tables[i], bridges[i])
  
  varname <- unname(projOpts$vars['electricwater'])
  elec.tbl <- "ElectricWater"
  create_singleresponse_tbl(varname, elec.tbl, shel.data)
  
  shel.data <-
    link_db_tables(shel.data, elec.tbl, varname, "name", "elecwater_id")
  
  append_to_db(shel.data, "Shelter", dbpath)
})



# Economic empowerment/livelihoods
local({
  var.index <- 
    c(
      "oth.srvecon.dscrb",
      "total.economic",
      "econ.areas",               # bool
      "econ.reject"               # bool
    )
  varnames <- projOpts$vars[var.index]
  econ.col <- unname(c("facility_id", varnames))
  booleans <- econ.col[4:5]
  econ.data <- filter_alldata(srvtype_econ, econ.col, booleans)
  
  create_multiresponse_group(var.rgx[['economic.services']],
                             "EconServices",
                             "EconservicesFacility")
  
  append_to_db(econ.data, "Economic", dbpath)
})

# Remove the original tables
local({
  ans <- 1L
  if (interactive())
    ans <- menu(c("Yes", "No"), title = "Remove the original tables?")
  if (ans == 2L)
    return()
  for (state in states) {
    for (category in c("services", "capacity")) {
      for (table in c("cleaned", "labelled")) {
        tblname <- sprintf("%s_%s_%s", tolower(state), category, table)
        drop_db_table(tblname)
      }
    }
  }
})
