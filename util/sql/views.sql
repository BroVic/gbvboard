-- Source file: views.sql
--
-- License: MIT
--
-- Copyright (c) Victor Ordu 2022

CREATE VIEW IF NOT EXISTS FacilitiesBasic AS
SELECT facility_id,
  Projects.name AS Projects,
  States.name AS State,
  LGAs.name AS LGA,
  Facility.Ward AS Ward,
  Facility.gps_long AS gps_long,
  Facility.gps_lat AS gps_lat,
  Facility.gps_alt AS gps_alt,
  Facility.gps_prec AS gps_prec,
  Orgtypes.name AS orgtype,
  OpenAccess.name AS open247,
  AgeGrp.name AS age_grp,
  Facility.started_ops AS opstart,
  Facility.started_gbv AS gbvstart,
  Facility.open_time AS open,
  Facility.close_time AS close,
  Facility.num_fulltime AS no_fulltime,
  Facility.num_parttime AS no_parttime,
  Facility.num_female AS no_female
FROM Facility, Projects, States, LGAs, OrgTypes, AgeGrp, OpenAccess
WHERE Facility.proj_id=Projects.id
  AND Facility.state_id=States.id
  AND Facility.lga_id=LGAs.id 
  AND Facility.agegrp_id=AgeGrp.id 
  AND Facility.orgtype_id=OrgTypes.id 
  AND Facility.open247_id=OpenAccess.id;
  