# Source file: initdb.R
#
# License: MIT
#
# Copyright (c) Victor Ordu 2022

# R script for the initial population of the created DB tables with
# data from the project. We are using cleaned/transformed data 

# Dependencies ----
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

## Each project has certain `options` set to values that
## determine how its dataset is handled, and here they
## are collected as a list.
opts <- fetch_proj_options(dir)
opts$vars <- jGBV::new.varnames

## Fetch the data
alldata <- combine_project_data(dir, opts)

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
  variables <- opts$vars[c("state", "lga", "device.id")]
  
  purrr::walk2(variables,
               tables,
               ~ update_singleresponse_tbl(alldata, .x, .y, dbpath))
})

# The Interviewers
local({
  ivars <- opts$vars[c('interviewer', 'interviewer.contact')]
  interviewer <- alldata[, ivars]
  interviewer.name <- interviewer[[ivars[1]]]
  interviewer <- interviewer[!duplicated(interviewer.name), ]
  names(interviewer) <- c("name", "contact")
  interviewer <- apply_project_id(interviewer, dbpath, opts$proj.name)
  append_to_db(interviewer, "Interviewer", dbpath)
})

# Facility-specific data ----
local({
  fkey <- c(
    "state",
    "lga",
    "interviewer",
    "device.id",
    "age",
    "showed.docs",
    "org.type",
    "how.data",
    "private.ques",
    "computer.secured",
    "contact.authority",
    "coc.signed",
    "refto.health",
    "refto.psych",
    "refto.police",
    "refto.legal",
    "refto.shelt",
    "refto.econ",
    "refto.other",
    "update.refdir",
    "choose.treatment"
  )
  
  bool <- c(
    "uses.docs",
    "child.docs",
    "standard.forms",
    "data.is.stored",
    "priv",
    "priv.room",
    "serve.disabled",
    "disabled.special",
    "coc.copies",
    "coc.confidentiality",
    "coc.equity",
    "has.focalperson",
    "has.gbv.trained",
    "has.refdir",
    "choose.referral",
    "coordination"
  )
  
  field <- c(
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
    "ward",
    "address",
    "phone",
    "email",
    "title",
    "info",
    "gps.long",
    "gps.lat",
    "gps.alt",
    "gps.prec",
    "oth.org.type",
    "open.247",
    "open.time",
    "close.time",
    "oth.gbv.dscrb",
    "staffname",
    "oth.fund.dscrb",
    "govt.spec",
    "fulltime.staff",
    "partime.staff",
    "female.staff",
    "doc.photo",
    "oth.docs.dscrb",
    "process.nodoc",
    "why.contact",
    "contact.case",
    "contact.authtype",
    "details.miss.equip",
    "oth.disabl.dscrb",
    "focalperson.contact",
    "num.gbv.trained",
    "who.gbv.trained",
    "which.gbv.trained",
    "refdir.pic",
    "oth.refto.dscrb",
    "gbvcase.contact",
    "why.nochoose.ref",
    "which.coord",
    "comment.coord",
    "service.othersdetail"
  )
  
  var.index <- set_variable_indices(fkey = fkey, bool = bool, field = field)
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
  
  tblinfo <-
    table_info_matrix(index = factor.indx,
                      tablename = tables,
                      proj.opts = opts)
  
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
  added.info <-
    table_info_matrix(index = add.indx,
                      tablename = add.tbls,
                      proj.opts = opts)
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
  
  # Populate the bridge tables for multiple response data
  
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
    table_info_matrix(index = rgx.index,
                      tablename = tables,
                      bridge = bridges,
                      proj.opts = opts,
                      type = 'regex')
  
  update_bridge_tables(alldata, tblinfo, dbpath)
  
  # Merge reference tables with main one via their respective PK IDs
  tbls <-
    lapply(c("Interviewer", "Projects"), function(x)
      jGBV::read_from_db(dbpath, x))
  all.interviewers <- tbls[[1]]
  proj <- tbls[[2]]
  rm(tbls)
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


# Health services
local({
  
  fkey <- c("hf.type", "health.paid")
  bool <- c("has.pep", "has.contracep", "access.srv", "forms.yes")
  
  field <- c(
    "hf.type.others",
    "oth.srvhealth.dscrb",
    "total.health",
    "has.no.pep",
    "has.no.contracep",
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
    "comment.elem",
    "comment.suppl",
    "oth.hlthtrain.dscrb",
    "qual.staff"
  )
  
  var.index <- set_variable_indices(fkey = fkey, bool = bool, field = field)
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
    table_info_matrix(index = rgx.index,
                      tablename = tables,
                      bridge = bridges,
                      proj.opts = opts,
                      type = 'regex')
  
  update_bridge_tables(alldata, tblinfo, dbpath)
  
  # Link tables with external references
  tblinfo <- 
    table_info_matrix(index = c("hf.type", "health.paid"),
                      tablename = c("HfType", "CostOpts"),
                      refcolumn = c("hftype_id", "healthfees_id"),
                      proj.opts = opts)
  
  unite_main_with_ref_data(h.data, tblinfo, opts, dbpath)
  
  # Put it all together
  append_to_db(h.data, "Health", dbpath)
})


# Legal Aid Services
local({
  bool <- "support.for.court" 
  fkey <- c("legal.paid", "no.resources1")
  field <- c(
    "oth.srvleg.dscrb",
    "total.legal",
    "legal.access",
    "legalfee.consult",
    "legalfee.rep",
    "legalfee.court",
    "legalfee.med",
    "legalfee.secur",
    "legalfee.counsel",
    "legalfee.other",
    "no.resources2"
  )
  
  var.index <- set_variable_indices(fkey = fkey, bool = bool, field = field)
  l.data <- make_pivot_tabledf(alldata, var.index, serv.type = "srvtype_legal")
  
  # Bridge table for legal aid data
  tblinfo <- 
    table_info_matrix(index = 'legal.services',
                      tablename = "LegalServices",
                      bridge = "LegalservicesFacility",
                      proj.opts = opts,
                      type = 'regex')
  
  update_bridge_tables(alldata, tblinfo, dbpath)
  
  # Linkage with reference table for legal aid 
  tblinfo2 <- 
    table_info_matrix(index = c("legal.paid", "no.resources1"),
                      tablename = c("CostOpts", "ActionNoresrc"),
                      refcolumn = c("legalfees_id", "noresource1_id"),
                      proj.opts = opts)
 
  update_many_singleresponse_tbls(l.data, tblinfo2, dbpath)
  
  context <- "Legal"
  scrubs <- scrublist(context, tables, new.value)
  l.data <- unite_main_with_ref_data(l.data, tblinfo2, opts, db, scrubs)
  
  # Update the database
  append_to_db(l.data, context, dbpath)
})



# Psychosocial support
local({
  fkey <- "psych.paid"
  field <- c(
    "oth.srvpsy.dscrb",
    "total.psychosocial",
    "psych.access",
    "psychfee.counsel",
    "psychfee.case",
    "psychfee.therapy",
    "psychfee.safety",
    "psychfee.other",
    "oth.psychtrain.dscrb",
    "qualstaff.id"
  )
  
  var.index <- set_variable_indices(fkey = fkey, field = field)
  psy.data <- make_pivot_tabledf(alldata, var.index, 'srvtype_psych')
  
  # Bridge tables
  tblinfo <-
    table_info_matrix(
      index = c("psychosoc.services", "trained.psychosoc"),
      tablename = c("PsychoServices", "PsychoTrain"),
      bridge = c("PsychoservicesFacility", "PsychotrainFacility"),
      proj.opts = opts,
      type = 'regex'
    )
  
  update_bridge_tables(alldata, tblinfo, dbpath)
  
  # Linking reference tables
  tblinfo <-
    table_info_matrix(index = "psych.paid",
                      tablename = "CostOpts",
                      refcolumn = "psychfees_id",
                      proj.opts = opts)
  
  psy.data <- unite_main_with_ref_data(psy.data, tblinfo, opts, dbpath)
  
  # Finalize for psychosocial services
  append_to_db(psy.data, "Psychosocial", dbpath)
})


# Security/Police
local({
  fkey <-  "police.fees"
  bool <- c("gbv.police",
            "refer.otherpolice",
            "police.followup")
  field <- c(
    "who.gbvpolice",
    "oth.srvpol.dscrb",
    "trainedpolice.id",
    "total.police.rape",
    "total.police.ipv",
    "total.police.csa",
    "total.police.fgm",
    "total.police.oth",
    "oth.polnum.dscrb",
    "police.access",
    "policefee.case",
    "policefee.safety",
    "policefee.other",
    "oth.policeresrc.dscrb",
    "police.confidential"
  )
  
  var.index <- set_variable_indices(fkey, bool, field)
  pol.data <- make_pivot_tabledf(alldata, var.index, 'srvtype_police')
  
  # Make bridge tables
  tblinfo <- 
    table_info_matrix(
      index = c("police.services", "trained.police", "resources.police"),
      tablename = c("PoliceServices", "TrainedPolice", "PoliceResources"),
      bridge = c("PoliceservicesFacility",
                 "TrainedpoliceFacility",
                 "PoliceresourcesFacility"),
      proj.opts = opts,
      type = 'regex'
    )
  
  update_bridge_tables(alldata, tblinfo, dbpath)
  
  # Link reference table and save the data
  tblinfo <-
    table_info_matrix(
      index = "police.fees",
      tablename = "CostOpts",
      refcolumn = "policefees_id",
      proj.opts = opts
    )
  pol.data <- unite_main_with_ref_data(pol.data, tblinfo, opts, dbpath)
  
  append_to_db(pol.data, "Police", dbpath)
})




# Temporary shelter
local({
  fkey <-  "electricwater"
  field <- c(
    "health.srvshelt.dscrb",
    "oth.srvshelt.dscrb",
    "oth.sheltpriv.dscrb",
    "oth.sheltamen.dscrb",
    "total.shelter.f",
    "total.shelter.m"
  )
  bool <- c(
    "shelter.famfriendly",
    "shelter.kidfriendly",
    "shelter.support",
    "shelter.new.support"
  )
  var.index <- set_variable_indices(fkey, bool, field)
  shel.data <- make_pivot_tabledf(alldata, var.index, 'srvtype_shelt')
  
  # Bridge tables
  tblinfo <- 
    table_info_matrix(
      index = c("shelter.services", "privacy.shelter", "amenities.shelter"),
      tablename = c("ShelterServices", "ShelterPrivacy", "ShelterAmenities"),
      bridge = c("ShelterservicesFacility",
                 "ShelterprivacyFacility",
                 "ShelteramenitiesFacility"),
      proj.opts = opts, 
      type = 'regex'
    )
  
  update_bridge_tables(alldata, tblinfo, dbpath)
  
  # Link reference tables
  tblinfo <-
    table_info_matrix(index = "electricwater",
                      tablename = "ElectricWater",
                      refcolumn = "elecwater_id",
                      proj.opts = opts)
  
  update_many_singleresponse_tbls(shel.data, tblinfo, dbpath)
  shel.data <- unite_main_with_ref_data(shel.data, tblinfo, opts, dbpath)
  
  # Add to the database
  append_to_db(shel.data, "Shelter", dbpath)
})



# Economic empowerment/livelihoods
local({
  field <- c("oth.srvecon.dscrb",
             "total.economic")
  bool <- c("econ.areas",
            "econ.reject")
  
  var.index <- set_variable_indices(bool = bool, field = field)
  econ.data <- make_pivot_tabledf(alldata, var.index, 'srvtype_econ')
  
  tblinfo <-
    table_info_matrix(index = 'economic.services',
                      tablename = "EconServices",
                      bridge = "EconservicesFacility",
                      proj.opts = opts,
                      type = 'regex')
  
  update_bridge_tables(alldata, tblinfo, dbpath)
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
