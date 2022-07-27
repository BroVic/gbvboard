-- Source file: views.sql
--
-- License: MIT
--
-- Copyright (c) Victor Ordu 2022

CREATE VIEW IF NOT EXISTS FacilitiesBasic AS
SELECT facility_id,
  started_ops AS open_date,
  started_gbv AS gbv_date,
  States.name AS State,
  LGAs.name AS LGA,
  Ward,
  gps_long,
  gps_lat,
  gps_alt,
  gps_prec,
  Orgtypes.name AS org_type,
  OpenAccess.name AS open_247,
  open_time,
  close_time,
  AgeGrp.name AS age_group,
  num_fulltime,
  num_parttime,
  num_female
FROM Facility, States, LGAs, OrgTypes, AgeGrp, OpenAccess
WHERE Facility.state_id=States.id
  AND Facility.lga_id=LGAs.id 
  AND Facility.agegrp_id=AgeGrp.id 
  AND Facility.orgtype_id=OrgTypes.id 
  AND Facility.open247_id=OpenAccess.id;
  