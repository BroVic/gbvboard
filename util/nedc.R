library(dplyr, warn.conflicts = FALSE)
nedc.states <- c("Adamawa", "Borno", "Yobe")

nedc.dfs <- lapply(nedc.states, function(s) {
  rdspath <- file.path("../NEDC/data/", s, "cleaned-services.rds")
  readRDS(rdspath)
})

nedc.data <-
  purrr::map_df(nedc.dfs, function(dt) {
    browser()
    dtnames <- names(dt)
    stateHdrStr <- "stateorigin"
    stateinds <- grep(stateHdrStr, dtnames)
    firstind <- stateinds[1]
    firstcol <- dtnames[firstind]
    if (firstcol != stateHdrStr)
      names(dt)[firstind] <- stateHdrStr
    
    if (length(stateinds) > 1L)
      dt <- select(dt, -stateinds[-1])
    
    dt <- dt %>% 
      mutate(
        stateorigin = state,
        today = as.character(today),
        `_GPS_location_of_the_rganization_facility_latitude` = as.double(`_GPS_location_of_the_rganization_facility_latitude`),
        `_GPS_location_of_the_rganization_facility_longitude` = as.double(`_GPS_location_of_the_rganization_facility_longitude`),
        `_GPS_location_of_the_rganization_facility_altitude` = as.double(`_GPS_location_of_the_rganization_facility_altitude`),
        `_GPS_location_of_the_rganization_facility_precision` = as.double(`_GPS_location_of_the_rganization_facility_precision`),
        Indicate_number_trained_staff = as.double(Indicate_number_trained_staff),
        Approximately_how_many_survivo = as.double(Approximately_how_many_survivo),
        How_many_cases_were_reported_i = as.double(How_many_cases_were_reported_i),
        How_many_cases_were_reported_i_001 = as.double(How_many_cases_were_reported_i_001),
        How_many_cases_were_reported_i_002 = as.double(How_many_cases_were_reported_i_002),
        How_many_cases_were_reported_i_003 = as.double(How_many_cases_were_reported_i_003),
        How_many_cases_were_reported_i_004 = as.double(How_many_cases_were_reported_i_004)
      )
  })
