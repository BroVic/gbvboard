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
source(here('util/sql/dbflds.R'))

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
alldata <- set_id_col(alldata, dbpath, "facility")
data.selectors <- get_selecion_args()

# Table operations ----
local({
  tblname <- "Projects"
  proj <- data.frame(name = opts$proj.name, year = opts$proj.year)
  append_to_dbtable(proj, tbl = tblname, dbpath)
})


local({
  tblinfo <-
    table_info_matrix(
      index = c("state", "lga", "device.id"),
      tablename = c("States", "LGAs", "Devices"),
      proj.opts = opts
    )
  update_many_singleresponse_tbls(alldata, tblinfo, dbpath)
})

# The Interviewers
local({
  context <- "Interviewer"
  var.index <- get_var_index(data.selectors, context)
  
  cols <- opts$vars[c('interviewer', 'interviewer.contact')]
  ind <- !duplicated(alldata[, cols[1]])
  interv.data <- alldata[ind, ]
  interv.data <- make_pivot_tabledf(interv.data, var.index, id = NULL)
  
  tblinfo <- 
    table_info_matrix(
      index = 'proj.name',
      tablename = "Projects",
      refcolumn = 'project_id',
      proj.opts = opts
    )
  
  interv.data <-
    unite_main_with_ref_data(interv.data,
                             tblinfo,
                             opts,
                             dbpath,
                             drop = "year")
  
  names(interv.data) <- c("name", "contact", "project_id")
  append_to_dbtable(interv.data, context, dbpath)
})

# Facility-specific data ----
local({
  context <- "Facility"
  var.index <- get_var_index(data.selectors, context)
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
  interviewer.data <- jGBV::read_from_db(dbpath, "Interviewer")
  
  fac.df <-
    link_db_tables(
      fac.df, 
      interviewer.data,
      "interviewer_name",
      "interviewer_id",
      drop = c('project_id', 'contact')
    )
  
  # For those variables for "referrals to" i.e. always/sometimes/never
  for(i in grep("^refto_", names(alldata), value = TRUE)) {
    refname <- paste0(i, "_id")
    fac.df <-
      link_db_tables(fac.df, "ReferralToOptions", i, refname, dbpath)
  }
  
  append_to_dbtable(fac.df, context, dbpath)
})


# Health services
local({
  context <- "Health"
  var.index <- get_var_index(data.selectors, context)
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
  
  h.data <- unite_main_with_ref_data(h.data, tblinfo, opts, dbpath)
  
  # Put it all together
  append_to_dbtable(h.data, context, dbpath)
})


# Legal Aid Services
local({
  context <- "Legal"
  var.index <- get_var_index(data.selectors, context)
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
  scrubs <- scrublist(context, tables, new.value)
  l.data <- unite_main_with_ref_data(l.data, tblinfo2, opts, dbpath, scrubs)
  
  # Update the database
  append_to_dbtable(l.data, context, dbpath)
})



# Psychosocial support
local({
  context <- "Psychosocial"
  var.index <- get_var_index(data.selectors, context)
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
  append_to_dbtable(psy.data, context, dbpath)
})


# Security/Police
local({
  context <- "Police"
  var.index <- get_var_index(data.selectors, context)
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
  
  append_to_dbtable(pol.data, context, dbpath)
})




# Temporary shelter
local({
  context <- "Shelter"
  var.index <- get_var_index(data.selectors, context)
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
  append_to_dbtable(shel.data, context, dbpath)
})

# Economic empowerment/livelihoods
local({
  context <- "Economic"
  var.index <- get_var_index(data.selectors, context)
  econ.data <- make_pivot_tabledf(alldata, var.index, 'srvtype_econ')
  
  tblinfo <-
    table_info_matrix(index = 'economic.services',
                      tablename = "EconServices",
                      bridge = "EconservicesFacility",
                      proj.opts = opts,
                      type = 'regex')
  
  update_bridge_tables(alldata, tblinfo, dbpath)
  append_to_dbtable(econ.data, context, dbpath)
})

# Remove the original tables
local({
  ans <- 1L
  
  if (interactive())
    ans <- menu(c("Yes", "No"), title = "Remove the original tables?")
  
  if (ans == 2L)
    return()
  
  walk(opts$proj.states, function(state) {
    walk(c("services", "capacity"), function(category) {
      walk(c("cleaned", "labelled"), function(table) {
        
        tblname <- sprintf("%s_%s_%s", tolower(state), category, table)
        drop_db_table(tblname, dbpath)
        
      })
    })
  })
})
