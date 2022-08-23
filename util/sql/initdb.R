# Source file: initdb.R
#
# License: MIT
#
# Copyright (c) Victor Ordu 2022

# R script for the initial population of the created DB tables with
# data from the project. We are using cleaned/transformed data 

# Dependencies ----
here::i_am(script.path <- "util/sql/initdb.R")
suppressPackageStartupMessages(library(here))

helperfile <- "dbfuns.R"

if (getwd() == here())
  helperfile <- file.path(dirname(script.path), helperfile)

source(helperfile)

library(tidyr)
library(purrr)
library(labelled)
library(jGBV, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE)


# Project Data ----
## Fetch inputs
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

opts <- get_project_options(dir)
opts$vars <- jGBV::new.varnames

dbpath <- file.path(dir, "data", paste0(tolower(basename(dir)), ".db"))
if (!file.exists(dbpath))
  stop("The database ", sQuote(dbpath), " does not exist")

## Fetch the data
states <- opts$states
alldata <- combine_project_data(opts$name, opts$states, dbpath)

## Clean up bad State entry for NEDC project
if (opts$name == "NEDC")
  alldata <- 
    transform(alldata, stateorigin = sub("NG002", "Adamawa", stateorigin))

## Apply variable labels
var_label(alldata) <- 
  var_label(load_data(dbpath, states[1], vars = opts$vars), unlist = TRUE)

var.rgx <- as.list(opts$regex)

## Some of the variable are converted to factors by `loadData`; we don't 
## want this for the purposes of creating the database.
factorvars <-
  opts$vars[c("start",
              "end",
              "today",
              "opstart",
              "gbvstart",
              "age",
              "update.refdir")]


# Get the primary key i.e. the ID column from the 'Facility' table in the 
# database and apply it to the main data frame, so that it can be used as
# a reference in subsequent operations. 

# Change to app database
dbpath <- here::here("app/data.db")

if (!file.exists(dbpath))
  stop(sQuote(dbpath), " does not exist")

nrw <- query_db(dbpath, "SELECT MAX(facility_id) FROM Facility;")[1, 1]

if (is.na(nrw))
  nrw <- 0L
  
alldata <- alldata %>%
  mutate(facility_id = nrw + seq(nrow(.))) %>%  # we set this to reference other tables
  mutate(across(contains(factorvars), as.character))



# Table operations ----

## We start by populating the Project and States tables
proj <- data.frame(name = opts$name, year = opts$year)
try( append_to_db(proj, tbl = "Projects", dbpath) )

local({
  tables <- c("States", "LGAs", "Devices")
  variables <- opts$vars[c("state", "lga", "device.id")]
  
  invisible(
    walk2(variables,
          tables,
          create_singleresponse_tbl,
          data = alldata,
          db = dbpath)
  )
})

# The Interviewers
local({
  ivars <- opts$vars[c('interviewer', 'interviewer.contact')]
  ra <- alldata[, ivars]
  ra.name <- ra[[ivars[1]]]
  ra <- ra[!duplicated(ra.name), ]
  names(ra) <- c("name", "contact")
  ra <- apply_project_id(ra, dbpath, opts$name)
  
  append_to_db(ra, "Interviewer", dbpath)
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
  
  varnames <- unname(opts$vars[var.index])
  facilcol <- c("facility_id", varnames)
  
  # Convert Y/N variables to binary values
  # We have added one column to account for the 'facility_id' that was
  # added after the position of the booleans that were manually identified
  # from the variable list.
  bools <- c(38, 43:44, 46, 52:53, 56:57, 60:63, 65, 69, 81, 84) + 1L
  bools <- facilcol[bools]
  
  facility.df <- alldata %>% 
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
  factor.indx <- c(
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
  
  cat.vars <- opts$vars[factor.indx]
  
  if (opts$name == "NFWP") {
    invisible(
      walk2(cat.vars,
            tables,
            create_singleresponse_tbl,
            data = alldata,
            db = dbpath)
    )
    
    create_singleresponse_tbl(
      c("Always", "Sometimes", "Never"), 
      "ReferralToOptions",
      alldata,
      dbpath
    )
  }
  
  ## NB: `link_db_tables` can work with a table name OR a session data frame!
  last.element <- length(tables)
  tables <- tables[-last.element]
  factor.indx <- factor.indx[-last.element]
  
  factor.indx <- c(factor.indx, "state", "lga", "device.id")
  y.tables <- c(tables, "States", "LGAs", "Devices")
  by.x <- unname(opts$vars[factor.indx])
  ref.col <-
    c(
      "agegrp_id",
      "orgtype_id",
      "open247_id",
      "docsareshown_id",
      "datastorage_id",
      "privateques_id",
      "elecstore_id",
      "contactauth_id",
      "signedcoc_id",
      "updates_id",
      "choosetreat_id",
      "state_id",
      "lga_id",
      "device_id"
    )
  
  if (opts$name == "NFWP") {
    for (i in seq_along(y.tables)) {
      facility.df <-
        link_db_tables(facility.df, y.tables[i], by.x[i], "name", ref.col[i], dbpath)
    }
  }
  else {
    facility.df <-
      update_linked_tables(by.x, y.tables, ref.col, facility.df, dbpath, scrublist())
  }
  
  # Add a column for 'Projects"
  facility.df <- apply_project_id(facility.df, dbpath, opts$name)
  
  # Merge reference tables with main one via their respective PK IDs
  all.interviewers <- read_from_db(dbpath, "Interviewer")
  proj <- read_from_db(dbpath, "Projects")
  proj.id <- proj$id[proj$name == opts$name]
  proj.interviewers <- subset(all.interviewers, proj_id == proj.id)
  proj.interviewers[c('proj_id', "contact")] <- NULL 
  
  facility.df <-
    link_db_tables(
      facility.df, 
      proj.interviewers, 
      "interviewer_name", 
      "name", 
      "interviewer_id"
    )
  
  # For those variables for "referrals to" i.e. always/sometimes/never
  for(i in grep("^refto_", names(alldata), value = TRUE)) {
    refname <- paste0(i, "_id")
    facility.df <-
      link_db_tables(facility.df, "ReferralToOptions", i, "name", refname, dbpath)
  }
  
  append_to_db(facility.df, "Facility", dbpath)
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
  
  for (i in seq_along(rgx.index)) {
    col <- var.rgx[[rgx.index[i]]]
    create_multiresponse_group(col, tables[i], bridges[i], alldata, dbpath)
  }
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
  varnames <- opts$vars[var.index]
  health.cols <- unname(c("facility_id", varnames))
  booleans <- health.cols[c(6, 8, 11, 23)]
  
  h.data <- filter_alldata(alldata, srvtype_health, health.cols, booleans)
  
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
  
  for (i in seq_along(tables)) {
    col <- var.rgx[[rgx.index[i]]]
    try(
      create_multiresponse_group(col, tables[i], bridges[i], alldata, dbpath)
    )
  }
  
  variables <- opts$vars[c("hf.type", "health.paid")]
  tables <- c("HfType", "CostOpts")
  ref.col <- c("hftype_id", "healthfees_id")
  
  if (opts$name == "NFWP") {
    for (i in seq_along(tables)) {
      create_singleresponse_tbl(variables[i], tables[i], h.data, dbpath)
      h.data <- 
        link_db_tables(h.data, tables[i], variables[i], "name", ref.col[i], dbpath)
    }
  }
  else {
    h.data <- update_linked_tables(variables, tables, ref.col, h.data, dbpath)
  }
  append_to_db(h.data, "Health", dbpath)
})


# Legal Aid Services
local({
  context <- "Legal"
  
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
      "legalfee.other",
      "support.for.court",         # bool
      "no.resources1",             # FK
      "no.resources2"
    )
  varnames <- opts$vars[var.index]
  legcol <- unname(c("facility_id", varnames))
  booleans <- legcol[c(5, 12)]
  
  l.data <- filter_alldata(alldata, srvtype_legal, legcol, booleans)
  
  create_multiresponse_group(var.rgx[['legal.services']],
                             "LegalServices", 
                             "LegalservicesFacility",
                             alldata,
                             dbpath)
  
  
  
  var.index <- c("legal.paid", "no.resources1")
  varnames <- unname(opts$vars[var.index])
  tables <- c("CostOpts", "ActionNoresrc")
  ref.col <- c("legalfees_id", "noresource1_id")
  
  if (opts$name == "NFWP") {
    t <- tables[-1]
    v <- varnames[-1]
    
    for (i in seq_along(t))
      create_singleresponse_tbl(v[i], t[i], l.data, dbpath)
    
    for (i in seq_along(tables))
      l.data <- 
        link_db_tables(l.data, tables[i], varnames[i], "name", ref.col[i], dbpath)
  }
  else {
    new.value <- if (opts$name == "NEDC")
      c(ActionNoresrc = "The case is closed")
    
    scrubs <- scrublist(context, tables, new.value)
    
    l.data <- update_linked_tables(
      fctnames = varnames,
      tblnames = tables,
      refcolnames = ref.col,
      data = l.data,
      db = dbpath,
      scrublist = scrubs,
      insertions = new.value
    )
  }
  append_to_db(l.data, context, dbpath)
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
  varnames <- opts$vars[var.index]
  psy.col <- unname(c("facility_id", varnames))
  psy.data <- filter_alldata(alldata, srvtype_psych, psy.col)

  rgx.index <- c("psychosoc.services", "trained.psychosoc")
  tables <- c("PsychoServices", "PsychoTrain")
  bridges <- c("PsychoservicesFacility", "PsychotrainFacility")
  
  for (i in 1:2) {
    col <- var.rgx[[rgx.index[i]]]
    create_multiresponse_group(col, tables[i], bridges[i], alldata, dbpath)
  }
  
  fees <- "psych_fees"
  tbl.costs <- "CostOpts"
  ref.fees <- "psychfees_id"
  
  psy.data <- if (opts$name == "NFWP")
    link_db_tables(psy.data, tbl.costs, fees, "name", ref.fees, dbpath)
  else
    update_linked_tables(fees, tbl.costs, ref.fees, psy.data, dbpath)
  
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
  varnames <- opts$vars[var.index]
  pol.col <- unname(c("facility_id", varnames))
  booleans <- pol.col[c(3, 5, 19)]
  pol.data <- filter_alldata(alldata, srvtype_police, pol.col, booleans)
  
  rgx.index <-
    c("police.services", "trained.police", "resources.police")
  tables <- c("PoliceServices", "TrainedPolice", "PoliceResources")
  bridges <-
    c("PoliceservicesFacility",
      "TrainedpoliceFacility",
      "PoliceresourcesFacility")
  
  for (i in 1:3) {
    col <- var.rgx[[rgx.index[i]]]
    create_multiresponse_group(col, tables[i], bridges[i], alldata, dbpath)
  }
  pol.data <-
    link_db_tables(pol.data, "CostOpts", "police_fees", "name", "policefees_id", dbpath)
  
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
  varnames <- unname(opts$vars[var.index])
  shel.col <- c("facility_id", varnames)
  booleans <- shel.col[c(4:5, 11:12)]
  shel.data <- filter_alldata(alldata, srvtype_shelt, shel.col, booleans)
  
  rgx.index <- 
    c("shelter.services", "privacy.shelter", "amenities.shelter")
  tables <- c("ShelterServices", "ShelterPrivacy", "ShelterAmenities")
  bridges <-
    c("ShelterservicesFacility",
      "ShelterprivacyFacility",
      "ShelteramenitiesFacility")
  
  for (i in 1:3) {
    col <- var.rgx[[rgx.index[i]]]
    create_multiresponse_group(col, tables[i], bridges[i], alldata, dbpath)
  }
  varname <- unname(opts$vars['electricwater'])
  elec.tbl <- "ElectricWater"
  create_singleresponse_tbl(varname, elec.tbl, shel.data, dbpath)
  
  shel.data <-
    link_db_tables(shel.data, elec.tbl, varname, "name", "elecwater_id", dbpath)
  
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
  varnames <- opts$vars[var.index]
  econ.col <- unname(c("facility_id", varnames))
  booleans <- econ.col[4:5]
  econ.data <- filter_alldata(alldata, srvtype_econ, econ.col, booleans)
  
  create_multiresponse_group(var.rgx[['economic.services']],
                             "EconServices",
                             "EconservicesFacility",
                             alldata,
                             dbpath)
  
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
