-- Source file: create.sql
--
-- License: MIT
--
-- Copyright (c) Victor Ordu 2022


------          ----
-- PRELIMINARIES
------          ----
-- Table projects
-- Description: Jhpiego GBV projects
CREATE TABLE IF NOT EXISTS Projects (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT UNIQUE NOT NULL,
  year TEXT
);

-- Table: states
-- Description: States where mapping was conducted
CREATE TABLE IF NOT EXISTS States (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT UNIQUE NOT NULL
);

-- Table: lgas
-- Description: Study Local Government Areas
CREATE TABLE IF NOT EXISTS LGAs (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT UNIQUE NOT NULL
);

CREATE TABLE IF NOT EXISTS PrivateQuesOpts (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

-- Table: Facility
-- Description: General Facility Information
CREATE TABLE IF NOT EXISTS Facility (
  facility_id INTEGER PRIMARY KEY,
  int_start TEXT,
  int_end TEXT,
  today TEXT,
  has_office INTEGER,
  has_phone INTEGER,
  continue_mapping INTEGER,
  respondent_consent INTEGER,
  org_name TEXT NOT NULL,
  started_ops TEXT,
  started_gbv TEXT,
  state_id INTEGER NOT NULL,
  lga_id INTEGER NOT NULL,
  ward TEXT,
  address TEXT,
  org_phone TEXT,
  org_email TEXT,
  interviewer_id INTEGER,
  device_id INTEGER,
  respondent_names TEXT,
  respondent_title TEXT,
  respondent_info TEXT,
  gps_long REAL,
  gps_lat REAL,
  gps_alt REAL,
  gps_prec REAL,
  agegrp_id INTEGER,
  orgtype_id INTEGER,
  govt_spec TEXT,
  other_org_type TEXT,
  open247_id INTEGER,
  open_time TEXT,
  close_time TEXT,
  describe_other_gbv TEXT,
  describe_other_funding TEXT,
  num_fulltime INTEGER,
  num_parttime INTEGER,
  num_female INTEGER,
  doc_areused INTEGER,
  docsareshown_id INTEGER,
  doc_photo TEXT,
  describe_other_docused TEXT,
  nodoc_process TEXT,
  docs_child INTEGER,
  standard_forms INTEGER,
  datastorage_id INTEGER,
  secure_physical INTEGER,
  elecstore_id INTEGER,
  contactauth_id INTEGER,
  contact_whysome TEXT,
  contact_casetyp TEXT,
  contact_authtyp TEXT,
  privacy INTEGER,
  private_room INTEGER,
  privateques_id INTEGER,
  describe_other_facmiss TEXT,
  disabled_srv INTEGER,
  disabled_spec INTEGER,
  describe_other_disabled INTEGER,
  signedcoc_id INTEGER,
  coc_copy INTEGER,
  coc_conf INTEGER,
  coc_equity INTEGER,
  gbv_focal INTEGER,
  fp_contact TEXT,
  gbvtrain INTEGER,
  gbvtrain_num INTEGER,
  gbvtrain_who TEXT,
  gbvtrain_which TEXT,
  refdir INTEGER,
  refdir_pic TEXT,
  refto_health_id INTEGER,
  refto_psych_id INTEGER,
  refto_police_id INTEGER,
  refto_legal_id INTEGER,
  refto_shelt_id INTEGER,
  refto_econ_id INTEGER,
  refto_other_id INTEGER,
  describe_other_refto TEXT,
  updates_id INTEGER,
  gbvcase_contact TEXT,
  choice_ref INTEGER,
  choice_ref_whynot TEXT,
  choosetreat_id INTEGER,
  coord INTEGER,
  coord_which TEXT,
  coord_comments TEXT,
  describe_other_srvtype TEXT,
  FOREIGN key (state_id) REFERENCES States(id),
  FOREIGN KEY (lga_id) REFERENCES LGAs(id),
  FOREIGN KEY (interviewer_id) REFERENCES Interviewer(id),
  FOREIGN KEY (device_id) REFERENCES Devices(id),
  FOREIGN KEY (orgtype_id) REFERENCES OrgTypes (id),
  FOREIGN KEY (open247_id) REFERENCES OpenAccess(id),
  FOREIGN KEY (agegrp_id) REFERENCES AgeGrp(id),
  FOREIGN KEY (privateques_id) REFERENCES PrivateQuesOpts(id),
  FOREIGN KEY (docsareshown_id) REFERENCES DocsAreShown(id),
  FOREIGN KEY (datastorage_id) REFERENCES Datastorage(id),
  FOREIGN KEY (elecstore_id) REFERENCES ElectronicStore(id),
  FOREIGN KEY (contactauth_id) REFERENCES ContactAuthority(id),
  FOREIGN KEY (signedcoc_id) REFERENCES SignedCOC(id),
  FOREIGN KEY (updates_id) REFERENCES UpdateRefdir(id),
  FOREIGN KEY (choosetreat_id) REFERENCES ChooseTreatment(id)
);

CREATE TABLE IF NOT EXISTS Devices (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT UNIQUE NOT NULL
);

CREATE TABLE IF NOT EXISTS Interviewer (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  contact TEXT
);

CREATE TABLE IF NOT EXISTS GBVtypes (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS GBVtypesFacility (
  facility_id INTEGER,
  opt_id INTEGER NOT NULL,
  FOREIGN KEY (facility_id) REFERENCES Facility(facility_id),
  FOREIGN key (opt_id) REFERENCES GBVtypes(id)
);

CREATE TABLE IF NOT EXISTS OrgTypes (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS OpenAccess (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS Funding (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS FundingFacility (
  facility_id INTEGER,
  opt_id INTEGER NOT NULL,
  FOREIGN KEY (facility_id) REFERENCES Facility(facility_id),
  FOREIGN key (opt_id) REFERENCES Funding(id)
);

CREATE TABLE IF NOT EXISTS Days (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS DaysFacility (
  facility_id INTEGER,
  opt_id INTEGER NOT NULL,
  FOREIGN KEY (facility_id) REFERENCES Facility(facility_id),
  FOREIGN key (opt_id) REFERENCES Days(id)
);

CREATE TABLE IF NOT EXISTS AgeGrp (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS Datastorage (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS DocsAreShown (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS DocTypes (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS DoctypesFacility (
  facility_id INTEGER,
  opt_id INTEGER NOT NULL,
  FOREIGN KEY (facility_id) REFERENCES Facility(facility_id),
  FOREIGN key (opt_id) REFERENCES DocTypes(id)
);

CREATE TABLE IF NOT EXISTS ShowDocs (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS Electronicstore (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS ContactAuthority (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS SignedCOC (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS Missing (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS MissingFacility (
  facility_id INTEGER,
  opt_id INTEGER NOT NULL,
  FOREIGN KEY (facility_id) REFERENCES Facility(facility_id),
  FOREIGN KEY (opt_id) REFERENCES Missing(id)
);

CREATE TABLE IF NOT EXISTS UpdateRefdir (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS ReferralToOptions (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS ChooseTreatment (
  id INTEGER PRIMARY KEY,
  name TEXT
);

CREATE TABLE IF NOT EXISTS ServiceType (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS ServicetypeFacility (
  facility_id INTEGER,
  opt_id INTEGER NOT NULL,
  FOREIGN KEY (facility_id) REFERENCES Facility(facility_id),
  FOREIGN KEY (opt_id) REFERENCES ServiceType(id)
);

CREATE TABLE IF NOT EXISTS Hftype (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS HealthServices (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY(id)
);


CREATE TABLE IF NOT EXISTS HealthservicesFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES HealthServices(id)
);

CREATE TABLE IF NOT EXISTS CostOpts (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS Medicines (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS MedicinesFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES Medicines(id)
);

CREATE TABLE IF NOT EXISTS Elements (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS ElementsFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES Elements(id)
);

CREATE TABLE IF NOT EXISTS HealthForm (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS HealthFormFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES HealthForm(id)
);

CREATE TABLE IF NOT EXISTS TrainedHealth (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS TrainedHealthFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES TrainedHealth(id)
);

CREATE TABLE IF NOT EXISTS Health (
  id INTEGER,
  facility_id INTEGER,
  hftype_id INTEGER,
  hf_other TEXT,
  describe_other_srvhealth TEXT,
  health_num INTEGER,
  pep_72hr INTEGER,
  pep_72hr_not TEXT,
  contra_120 INTEGER,
  contra_120_not TEXT,
  healthfees_id INTEGER,
  srv_access TEXT,
  healthfee_clin REAL,
  healthfee_inj REAL,
  healthfee_pep REAL,
  healthfee_contra REAL,
  healthfee_hiv REAL,
  healthfee_sti REAL,
  healthfee_foren REAL,
  healthfee_psych REAL,
  healthfee_case REAL,
  healthfee_basic REAL,
  healthfee_other REAL,
  forms_yes INTEGER,
  comment_elem TEXT,
  comment_suppl TEXT,
  describe_other_hlthtrain TEXT,
  qual_staff TEXT,
  PRIMARY KEY (id AUTOINCREMENT),
  FOREIGN KEY (hftype_id) REFERENCES Hftype(id),
  FOREIGN KEY (healthfees_id) REFERENCES HealthCost(id),
  FOREIGN KEY (facility_id) REFERENCES Facility(factility_id)
);

CREATE TABLE IF NOT EXISTS LegalServices (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS LegalservicesFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES LegalServices(id)
);

CREATE TABLE IF NOT EXISTS ActionNoresrc (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS Legal (
  id INTEGER,
  facility_id INTEGER,
  describe_other_srvleg TEXT,
  legal_num INTEGER,
  legalfees_id INTEGER,
  legal_access TEXT,
  legalfee_consult REAL,
  legalfee_rep REAL,
  legalfee_court REAL,
  legalfee_med REAL,
  legalfee_secur REAL,
  legalfee_counsel REAL,
  legalfee_other REAL,
  support_court INTEGER,
  noresource1_id INTEGER,
  other_action TEXT,
  PRIMARY KEY (id AUTOINCREMENT),
  FOREIGN KEY (facility_id) REFERENCES Facility(factility_id),
  FOREIGN KEY (legalfees_id) REFERENCES LegalCost(id),
  FOREIGN KEY (noresource1_id) REFERENCES ActionNoresrc(id)
);


CREATE TABLE IF NOT EXISTS PsychoServices (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS PsychoservicesFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES PsychoServices(id)
);

CREATE TABLE IF NOT EXISTS PsychoTrain (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS PsychotrainFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES PsychoTrain(id)
);

CREATE TABLE IF NOT EXISTS Psychosocial (
  id INTEGER,
  facility_id INTEGER,
  describe_other_srvpsy TEXT,
  psych_num INTEGER,
  psychfees_id INTEGER,
  psych_access TEXT,
  psychfee_counsel REAL,
  psychfee_case REAL,
  psychfee_therapy REAL,
  psychfee_safety REAL,
  psychfee_other REAL,
  acttion_noresrc INTEGER,
  other_action TEXT,
  describe_other_psytrain TEXT,
  id_qualstaff TEXT,
  PRIMARY KEY (id),
  FOREIGN KEY (psychfees_id) REFERENCES CostOpts(id)
);



CREATE TABLE IF NOT EXISTS PoliceServices (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);



CREATE TABLE IF NOT EXISTS PoliceservicesFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES PoliceServices(id)
);

CREATE TABLE IF NOT EXISTS TrainedPolice (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS TrainedpoliceFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES TrainedPolice(id)
);

CREATE TABLE IF NOT EXISTS PoliceResources (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS PoliceresourcesFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES PoliceResources(id)
);

CREATE TABLE IF NOT EXISTS Police (
  id INTEGER,
  facility_id INTEGER,
  describe_other_srvpol TEXT,
  gbvpolice INTEGER,
  gbvpolice_who TEXT,
  ref_otherpol INTEGER,
  id_trainedpolice TEXT,
  polnum_rape INTEGER,
  polnum_ipv INTEGER,
  polnum_csa  INTEGER,
  polnum_fgm INTEGER,
  polnum_other INTEGER,
  describe_other_polnum TEXT,
  policefees_id INTEGER,
  pol_access TEXT,
  policefee_case REAL,
  policefee_safety REAL,
  policefee_other REAL,
  describe_other_polresrc TEXT,
  pol_ffup INTEGER,
  pol_ffup_conf TEXT,
  PRIMARY KEY (id AUTOINCREMENT),
  FOREIGN KEY (policefees_id) REFERENCES CostOpts(id)
);

CREATE TABLE IF NOT EXISTS ShelterServices (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS ShelterservicesFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES PoliceServices(id)
);

CREATE TABLE IF NOT EXISTS ShelterPrivacy (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS ShelterprivacyFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES ShelterPrivacy(id)
);

CREATE TABLE IF NOT EXISTS ShelterAmenities (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS ShelteramenitiesFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES ShelterAmenities(id)
);

CREATE TABLE IF NOT EXISTS ElectricWater (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS Shelter (
  id INTEGER,
  facility_id INTEGER,
  descr_health_srvshelt TEXT,
  descr_oth_srvshelt TEXT,
  shelt_fam INTEGER,
  shelt_childfrndly INTEGER,
  describe_other_sheltpriv TEXT,
  describe_other_sheltamen TEXT,
  elecwater_id INTEGER,
  shelt_num_f INTEGER,
  shelt_num_m INTEGER,
  shelt_givsupp INTEGER,
  shelt_capsupp INTEGER,
  PRIMARY KEY (id AUTOINCREMENT),
  FOREIGN KEY (elecwater_id) REFERENCES ElectricWater(id)
  
);

CREATE TABLE IF NOT EXISTS EconServices (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS EconservicesFacility (
  facility_id INTEGER,
  opt_id INTEGER,
  FOREIGN KEY (facility_id) REFERENCES Facility(facilitY_id),
  FOREIGN KEY (opt_id) REFERENCES EconServices(id)
);

/*
CREATE TABLE IF NOT EXISTS Precautions (
  id INTEGER,
  name TEXT UNIQUE NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS PrecautionsFacility (
  id INTEGER,
  precaution_id INTEGER,
  shelter_id INTEGER,
  PRIMARY KEY (id),
  FOREIGN KEY (precaution_id) REFERENCES Precautions(id),
  FOREIGN KEY (shelter_id) REFERENCES Shelter(id)
);
*/

CREATE TABLE IF NOT EXISTS Economic (
  id INTEGER,
  facility_id INTEGER,
  describe_other_srvecon TEXT,
  econ_num INTEGER,
  econ_areas INTEGER,
  econ_reject INTEGER,
  PRIMARY KEY (id AUTOINCREMENT)
);
