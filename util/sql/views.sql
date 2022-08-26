-- Source file: views.sql
--
-- License: MIT
--
-- Copyright (c) Victor Ordu 2022

CREATE VIEW IF NOT EXISTS FacilitiesBasic AS
SELECT facility_id,
  Projects.name AS Project, 
  States.name AS State, 
  LGAs.name AS LGA,
  Ward,
  gps_long,
  gps_lat,
  gps_alt,
  gps_prec,
  Orgtypes.name AS orgtype,
  OpenAccess.name AS open247,
  AgeGrp.name AS age_grp,
  started_ops AS opstart,
  started_gbv AS gbvstart,
  open_time AS open,
  close_time AS close,
  num_fulltime AS no_fulltime,
  num_parttime AS no_parttime,
  num_female AS no_female
FROM Facility 
LEFT JOIN Projects 
  ON Facility.proj_id=Projects.id
LEFT JOIN States 
  ON Facility.state_id=States.id
LEFT JOIN LGAs
  ON Facility.lga_id=LGAs.id
LEFT JOIN AgeGrp
  ON Facility.agegrp_id=AgeGrp.id
LEFT JOIN Orgtypes
  ON Facility.orgtype_id=OrgTypes.id
LEFT JOIN OpenAccess 
  ON Facility.open247_id=OpenAccess.id;