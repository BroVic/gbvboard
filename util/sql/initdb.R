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
opts <- fetch_proj_options(dir)
var.rgx <- opts$var.regex
opts$vars <- var.mtch <- jGBV::new.varnames

## Fetch the data
alldata <- combine_project_data(dir, opts)

## Clean up bad State entry for NEDC project
## TODO: This has to go!
if (opts$proj.name == "NEDC") {
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
local({
  tblname <- "Projects"
  proj <- data.frame(name = opts$proj.name, year = opts$proj.year)
  append_to_db(proj, tbl = tblname, dbpath)
})

local({
  tables <- c("States", "LGAs", "Devices")
  variables <- var.mtch[c("state", "lga", "device.id")]
  
  walk2(variables,
        tables,
        ~ update_singleresponse_tbl(alldata, .x, .y, dbpath))
})

# The Interviewers
local({
  ivars <- var.mtch[c('interviewer', 'interviewer.contact')]
  interviewer <- alldata[, ivars]
  interviewer.name <- interviewer[[ivars[1]]]
  interviewer <- interviewer[!duplicated(interviewer.name), ]
  names(interviewer) <- c("name", "contact")
  interviewer <- apply_project_id(interviewer, dbpath, opts$proj.name)
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
      "ChooseTreatment"
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
    "choose.treatment"
  )
  
  tblinfo <- table_info_matrix(opts, factor.indx, tables)
  
  if (opts$proj.name == "NFWP") {
    update_many_singleresponse_tbls(alldata, tblinfo, dbpath)
    
    # Here we used actual values rather than a selected variable
    # from the data to obtain the responses.
    responses <- c("Always", "Sometimes", "Never")
    update_singleresponse_tbl(alldata, responses, "ReferralToOptions", dbpath)
  }
  
  # Here, the data used for creating reference tables was extended
  add.tbls <- c("States", "LGAs", "Devices")
  add.indx <- c("state", "lga", "device.id")
  added.info <- table_info_matrix(opts, add.indx, add.tbls)
  tblinfo <- rbind(tblinfo, added.info)
  
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
  
  tblinfo <- cbind(tblinfo, refcolumn = ref.col)
  fac.df <- unite_main_with_ref_data(fac.df, tblinfo, opts, dbpath, scrublist())
  
  # Add a column for 'Projects"
  fac.df <- apply_project_id(fac.df, dbpath, opts$proj.name)
  
  # Merge reference tables with main one via their respective PK IDs
  all.interviewers <- read_from_db(dbpath, "Interviewer")
  proj <- read_from_db(dbpath, "Projects")
  proj.id <- proj$id[proj$name == opts$proj.name]
  proj.interviewers <- subset(all.interviewers, proj_id == proj.id)
  proj.interviewers[c('proj_id', "contact")] <- NULL 
  
  fac.df <-
    link_db_tables(
      fac.df, 
      proj.interviewers, 
      "interviewer_name",
      "interviewer_id"
    )
  
  # For those variables for "referrals to" i.e. always/sometimes/never
  for(i in grep("^refto_", names(alldata), value = TRUE)) {
    refname <- paste0(i, "_id")
    fac.df <-
      link_db_tables(fac.df, "ReferralToOptions", i, refname, dbpath)
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
  
  tblinfo <-
    table_info_matrix(opts,
                      index = rgx.index,
                      tablename = tables,
                      bridge = bridges)
  
  update_bridge_tables(alldata, tblinfo, opts, dbpath)
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
  
  ## Bridge tables
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
  
  tblinfo <-
    table_info_matrix(opts,
                      index = rgx.index,
                      tablename = tables,
                      bridge = bridges)
  
  update_bridge_tables(alldata, tblinfo, opts, dbpath)
  
  # Link tables with external references
  tblinfo <- 
    table_info_matrix(opts,
                      index = c("hf.type", "health.paid"),
                      tablename = c("HfType", "CostOpts"),
                      refcolumn = c("hftype_id", "healthfees_id"))
  
  unite_main_with_ref_data(h.data, tblinfo, opts, dbpath)
  
  # Put it all together
  append_to_db(h.data, "Health", dbpath)
})


# Legal Aid Services
local({
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
  
  # Bridge table for legal aid data
  tblinfo <- 
    table_info_matrix(opts,
                      index = 'legal.services',
                      tablename = "LegalServices",
                      bridge = "LegalservicesFacility")
  
  update_bridge_tables(alldata, tblinfo, opts, dbpath)
  
  # Linkage with reference table for legal aid 
  tblinfo2 <- 
    table_info_matrix(opts,
                      index = c("legal.paid", "no.resources1"),
                      tablename = c("CostOpts", "ActionNoresrc"),
                      refcolumn = c("legalfees_id", "noresource1_id"))
 
  update_many_singleresponse_tbls(l.data, tblinfo2, dbpath)
  
  context <- "Legal"
  scrubs <- scrublist(context, tables, new.value)
  l.data <- unite_main_with_ref_data(l.data, tblinfo2, opts, db, scrubs)
  
  # Update the database
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
  
  # Bridge tables
  tblinfo <-
    table_info_matrix(
      opts,
      index = c("psychosoc.services", "trained.psychosoc"),
      tablename = c("PsychoServices", "PsychoTrain"),
      bridge = c("PsychoservicesFacility", "PsychotrainFacility")
    )
  
  update_bridge_tables(alldata, tblinfo, opts, dbpath)
  
  # Linking reference tables
  tblinfo <-
    table_info_matrix(opts,
                      index = "psych.paid",
                      tablename = "CostOpts",
                      refcolumn = "psychfees_id")
  
  psy.data <- unite_main_with_ref_data(psy.data, tblinfo, opts, dbpath)
  
  # Finalize for psychosocial services
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
  
  pol.data <- make_pivot_tabledf(alldata, var.index, 'srvtype_police')
  
  # Make bridge tables
  tblinfo <- 
    table_info_matrix(
      index = c("police.services", "trained.police", "resources.police"),
      tablename = c("PoliceServices", "TrainedPolice", "PoliceResources"),
      bridge = c("PoliceservicesFacility",
                 "TrainedpoliceFacility",
                 "PoliceresourcesFacility")
    )
  
  update_bridge_tables(alldata, tblinfo, opts, dbpath)
  
  # Link reference table and save the data
  pol.data <-
    link_db_tables(pol.data,
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
  
  # Bridge tables
  tblinfo <- 
    table_info_matrix(
      index = c("shelter.services", "privacy.shelter", "amenities.shelter"),
      tablename = c("ShelterServices", "ShelterPrivacy", "ShelterAmenities"),
      bridge = c("ShelterservicesFacility",
                 "ShelterprivacyFacility",
                 "ShelteramenitiesFacility")
    )
  
  update_bridge_tables(alldata, tblinfo, opts, dbpath)
  
  # Link reference tables
  varname <- unname(var.mtch['electricwater'])
  elec.tbl <- "ElectricWater"
  update_singleresponse_tbl(shel.data, varname, elec.tbl, dbpath)
  
  shel.data <-
    link_db_tables(shel.data, elec.tbl, varname, "elecwater_id", dbpath)
  
  # Add to the database
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
  
  create_multiresponse_group(alldata,
                             dbpath,
                             var.rgx[['economic.services']],
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
