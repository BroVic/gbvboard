# Source file: initdb.R
#
# License: MIT
#
# Copyright (c) Victor Ordu 2022

# R script for the initial population of the created DB tables with
# data from the project. We are using cleaned/transformed data 

# Dependencies ----
library(tidyr)
library(purrr)
library(labelled)
library(jGBV, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE)
suppressPackageStartupMessages(library(here))

source(here("util/sql/dbfuns.R"))

# Project Data ----
## Fetch inputs
dir <- if (interactive()) {
  gbvprojects <- c("NFWP", "NEDC")
  p <- menu(gbvprojects, TRUE, "Pick a project")
  root <- dirname(here())
  file.path(root, gbvprojects[p])
} else {
  params <- commandArgs(trailingOnly = TRUE)
  params[1]
}

dir <- normalizePath(dir, winslash = "/")
opts <- get_project_options(dir)
var.rgx <- as.list(opts$regex)
opts$vars <- jGBV::new.varnames

## Fetch the data
dbpath <- file.path(dir, "data", paste0(tolower(basename(dir)), ".db"))
alldata <- combine_project_data(opts, dbpath)

## Clean up bad State entry for NEDC project
if (opts$name == "NEDC") {
  alldata <- 
    transform(alldata, stateorigin = sub("NG002", "Adamawa", stateorigin))
}

# Change focus to app database.
# We will use the existing database tables 
# to set a main ID column which represents the
# individual facilities.
dbpath <- here("app/data.db")
alldata <- set_facility_id_col(alldata, dbpath)

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
  interviewer <- alldata[, ivars]
  interviewer.name <- interviewer[[ivars[1]]]
  interviewer <- interviewer[!duplicated(interviewer.name), ]
  names(interviewer) <- c("name", "contact")
  interviewer <- apply_project_id(interviewer, dbpath, opts$name)
  append_to_db(interviewer, "Interviewer", dbpath)
})

# Facility-specific data ----
local({
  var.index <- 
    c(
      field = "start",
      field = "end",
      field = "today",
      field = "has.office",
      field = "has.phone",
      field = "continue",
      field = "consent",
      field = "orgname",
      field = "opstart",
      field = "gbvstart",
      fkey = "state",
      fkey = "lga",
      field = "ward",
      field = "address",
      field = "phone",
      field = "email",
      fkey = "interviewer",
      fkey = "device.id",
      field = "staffname",
      field = "title",
      field = "info",
      field = "gps.long",
      field = "gps.lat",
      field = "gps.alt",
      field = "gps.prec",
      fkey = "age",
      fkey = "org.type",
      field = "govt.spec",
      field = "oth.org.type",
      field = "open.247",
      field = "open.time",
      field = "close.time",
      field = "oth.gbv.dscrb",
      field = "oth.fund.dscrb",
      field = "fulltime.staff",
      field = "partime.staff",
      field = "female.staff",
      bool = "uses.docs",
      fkey = "showed.docs",
      field = "doc.photo",
      field = "oth.docs.dscrb",
      field = "process.nodoc",
      bool = "child.docs",
      bool = "standard.forms",
      fkey = "how.data",
      bool = "data.is.stored",
      fkey = "computer.secured",
      fkey = "contact.authority",
      field = "why.contact",     
      field = "contact.case",
      field = "contact.authtype",
      bool = "priv",
      bool = "priv.room",
      fkey = "private.ques",
      field = "details.miss.equip",
      bool = "serve.disabled",
      bool = "disabled.special",
      field = "oth.disabl.dscrb",
      fkey = "coc.signed",
      bool = "coc.copies",          
      bool = "coc.confidentiality", 
      bool = "coc.equity",          
      bool = "has.focalperson",     
      field = "focalperson.contact",
      bool = "has.gbv.trained",     
      field = "num.gbv.trained",
      field = "who.gbv.trained",
      field = "which.gbv.trained",
      bool = "has.refdir",          
      field = "refdir.pic",
      fkey = "refto.health",        
      fkey = "refto.psych",         
      fkey = "refto.police",        
      fkey = "refto.legal",         
      fkey = "refto.shelt",         
      fkey = "refto.econ",          
      fkey = "refto.other",         
      field = "oth.refto.dscrb",
      fkey = "update.refdir",       
      field = "gbvcase.contact",
      bool = "choose.referral",     
      field = "why.nochoose.ref",
      fkey = "choose.treatment",    
      bool = "coordination",        
      field = "which.coord",
      field = "comment.coord",
      field = "service.othersdetail"
    )
  
  fac.df <- make_pivot_tabledf(alldata, var.index)
  
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
  
  if (opts$name == "NFWP") {
    cat.vars <- opts$vars[factor.indx]
    
    walk2(cat.vars,
          tables,
          create_singleresponse_tbl,
          data = alldata,
          db = dbpath)
    
    create_singleresponse_tbl(c("Always", "Sometimes", "Never"),
                              "ReferralToOptions",
                              alldata,
                              dbpath)
  }
  
  ## NB: `link_db_tables` can work with a table name OR a session data frame!
  last.element <- length(tables)
  tables <- tables[-last.element]
  y.tables <- c(tables, "States", "LGAs", "Devices")
  
  factor.indx <- factor.indx[-last.element] |>
    c("state", "lga", "device.id")
  
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
      fac.df <- fac.df |>
        link_db_tables(y.tables[i], by.x[i], "name", ref.col[i], dbpath)
    }
    
  }
  else {
    fac.df <- fac.df |>
      update_linked_tables(y.tables, by.x, ref.col, dbpath, scrublist())
  }
  
  # Add a column for 'Projects"
  fac.df <- apply_project_id(fac.df, dbpath, opts$name)
  
  # Merge reference tables with main one via their respective PK IDs
  all.interviewers <- read_from_db(dbpath, "Interviewer")
  proj <- read_from_db(dbpath, "Projects")
  proj.id <- proj$id[proj$name == opts$name]
  proj.interviewers <- subset(all.interviewers, proj_id == proj.id)
  proj.interviewers[c('proj_id', "contact")] <- NULL 
  
  fac.df <-
    link_db_tables(
      fac.df, 
      proj.interviewers, 
      "interviewer_name", 
      "name", 
      "interviewer_id"
    )
  
  # For those variables for "referrals to" i.e. always/sometimes/never
  for(i in grep("^refto_", names(alldata), value = TRUE)) {
    refname <- paste0(i, "_id")
    fac.df <-
      link_db_tables(fac.df, "ReferralToOptions", i, "name", refname, dbpath)
  }
  
  append_to_db(fac.df, "Facility", dbpath)
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
      fkey = "hf.type",                   
      field = "hf.type.others",
      field = "oth.srvhealth.dscrb",
      field = "total.health",
      bool = "has.pep",                   
      field = "has.no.pep",
      bool = "has.contracep",             
      field = "has.no.contracep",
      fkey = "health.paid",               
      bool = "access.srv",                
      field = "healthfee.clin",
      field = "healthfee.inj",
      field = "healthfee.pep",
      field = "healthfee.contra",
      field = "healthfee.hiv",
      field = "healthfee.sti",
      field = "healthfee.foren",
      field = "healthfee.psych",
      field = "healthfee.case",
      field = "healthfee.basic",
      field = "healthfee.other",
      bool = "forms.yes",                 
      field = "comment.elem",
      field = "comment.suppl",
      field = "oth.hlthtrain.dscrb",
      field = "qual.staff"
    )
  
  h.data <- make_pivot_tabledf(alldata, var.index, "srvtype_health")
  
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
      h.data <- h.data |>
        link_db_tables(tables[i], variables[i], "name", ref.col[i], dbpath)
    }
  }
  else {
    h.data <- h.data |>
      update_linked_tables(tables, variables, ref.col, dbpath)
  }
  
  append_to_db(h.data, "Health", dbpath)
})


# Legal Aid Services
local({
  context <- "Legal"
  
  var.index <-
    c(
      field = "oth.srvleg.dscrb",
      field = "total.legal",
      fkey = "legal.paid",                
      field = "legal.access", 
      field = "legalfee.consult",
      field = "legalfee.rep",
      field = "legalfee.court",
      field = "legalfee.med",
      field = "legalfee.secur",
      field = "legalfee.counsel",
      field = "legalfee.other",
      bool = "support.for.court",         
      fkey = "no.resources1",             
      field = "no.resources2"
    )
  
  l.data <- make_pivot_tabledf(alldata, var.index, serv.type = "srvtype_legal")
  
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
      l.data <- l.data |>
        link_db_tables(tables[i], varnames[i], "name", ref.col[i], dbpath)
  }
  else {
    new.value <- if (opts$name == "NEDC")
      c(ActionNoresrc = "The case is closed")
    
    scrubs <- scrublist(context, tables, new.value)
    
    l.data <- l.data |>
      update_linked_tables(tables, varnames, ref.col, dbpath, scrubs, new.value)
  }
  append_to_db(l.data, context, dbpath)
})



# Psychosocial support
local({
  var.index <- 
    c(
      field = "oth.srvpsy.dscrb",
      field = "total.psychosocial",
      fkey = "psych.paid",              
      field = "psych.access",
      field = "psychfee.counsel",
      field = "psychfee.case",
      field = "psychfee.therapy",
      field = "psychfee.safety",
      field = "psychfee.other",
      field = "oth.psychtrain.dscrb",
      field = "qualstaff.id"
    )
  
  psy.data <- make_pivot_tabledf(alldata, var.index, 'srvtype_psych')
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
    update_linked_tables(psy.data, tbl.costs, fees, ref.fees, dbpath)
  
  append_to_db(psy.data, "Psychosocial", dbpath)
})


# Security/Police
local({
  var.index <-
    c(
      field = "oth.srvpol.dscrb",
      bool = "gbv.police",                 
      field = "who.gbvpolice",
      bool = "refer.otherpolice",          
      field = "trainedpolice.id",
      field = "total.police.rape",
      field = "total.police.ipv",
      field = "total.police.csa",
      field = "total.police.fgm",
      field = "total.police.oth",
      field = "oth.polnum.dscrb",
      fkey = "police.fees",                
      field = "police.access",
      field = "policefee.case",
      field = "policefee.safety",
      field = "policefee.other",
      field = "oth.policeresrc.dscrb",
      bool = "police.followup",            
      field = "police.confidential"
    )
  
  psy.data <- make_pivot_tabledf(alldata, var.index, 'srvtype_police')
  rgx.index <- c("police.services", "trained.police", "resources.police")
  tables <- c("PoliceServices", "TrainedPolice", "PoliceResources")
  
  bridges <-
    c("PoliceservicesFacility",
      "TrainedpoliceFacility",
      "PoliceresourcesFacility")
  
  for (i in 1:3) {
    col <- var.rgx[[rgx.index[i]]]
    create_multiresponse_group(col, tables[i], bridges[i], alldata, dbpath)
  }
  
  pol.data <- link_db_tables(pol.data,
                             "CostOpts",
                             "police_fees",
                             "name",
                             "policefees_id",
                             dbpath)
  
  append_to_db(pol.data, "Police", dbpath)
  
})




# Temporary shelter
local({
  var.index <- 
    c(
      field = "health.srvshelt.dscrb",
      field = "oth.srvshelt.dscrb",
      bool = "shelter.famfriendly",        
      bool = "shelter.kidfriendly",        
      field = "oth.sheltpriv.dscrb",
      field = "oth.sheltamen.dscrb",
      fkey = "electricwater",              
      field = "total.shelter.f",
      field = "total.shelter.m",
      bool = "shelter.support",            
      bool = "shelter.new.support"         
    )
 
  shel.data <- make_pivot_tabledf(alldata, var.index, 'srvtype_shelt')
  rgx.index <- c("shelter.services", "privacy.shelter", "amenities.shelter")
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
      field = "oth.srvecon.dscrb",
      field = "total.economic",
      bool = "econ.areas",               
      bool = "econ.reject"               
    )
  
  econ.data <- make_pivot_tabledf(alldata, var.index, 'srvtype_econ')
  
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
