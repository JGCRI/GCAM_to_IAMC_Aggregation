# ------------------------------------------------------------------------------
# Program Name: GCAM_to_IAMC_emissions_aggregation.R
# Author(s): Maridee Weber
# Date Last Updated: June 2, 2021
# Program Purpose: The script aggregates Non-CO2 emissions from GCAM sectors to
#   IAMC sectors
# Input Files: GCAM_nonCO2_emissions.csv
#              GCAM_IAMC_mapping.csv
# Output Files: GCAM_output.csv
#               diagnostic.csv
# Notes:
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Load libraries
library( readr )
library( tidyr )
library( dplyr )
library( xlsx )

# ------------------------------------------------------------------------------
# 0. Read in inputs
# -------------------------------------------------------
# Read in the emissions data and mapping file
GCAM_nonCO2_emissions_raw <- read.csv( "GCAM_nonCO2_emissions.csv" )
GCAM_IAMC_mapping <- read.csv( "GCAM_IAMC_mapping.csv" )

# ------------------------------------------------------------------------------
# 0.5. Define Constants
# -------------------------------------------------------
MIN_YEAR <- 2015 # minimum year to include
MAX_YEAR <- 2100 # minimum year to include
gasses_omit <- c( "C2F6", "CF4", "HFC125", "HFC134a", "HFC143a", "HFC152a",
                  "HFC227ea", "HFC23", "HFC236fa", "HFC245fa", "HFC32",
                  "HFC365mfc", "HFC43", "SF6" ) # species we will not include in the output, fluorinated gasses

# ------------------------------------------------------------------------------
# 1. Process the raw data inputs
# -------------------------------------------------------
GCAM_nonco2_emissions <- GCAM_nonCO2_emissions_raw %>%
  # remove unnecessary columns
  select( -c( "date", "file" ) ) %>% 
  # filter for years of interest
  # filter out pollutants (currently just f-gasses)
  filter( Year >= MIN_YEAR & Year <= MAX_YEAR,
          !( GHG %in% gasses_omit ) ) %>% 
  # change GHG names to those IAMC uses
  mutate( GHG = gsub( "SO2_1", "SO2", GHG ),
          GHG = gsub( "SO2_2", "SO2", GHG ),
          GHG = gsub( "SO2_3", "SO2", GHG ),
          GHG = gsub( "SO2_4", "SO2", GHG ),
          GHG = gsub( "NMVOC", "VOC", GHG ) )

# ------------------------------------------------------------------------------
# 2. Mapping
# -------------------------------------------------------
  # 2.1. AGR/AWB
# -----------------------------------
AGRAWB <- GCAM_nonco2_emissions %>%
  # filter for AGR and AWB emissions
  # Emissions from AGR and AWB have this as a suffix to their GHG
  filter( grepl( "AGR|AWB", GHG ) ) %>%
  # separate GHG column for mapping purposes
  separate( GHG, into = c( "GHG", "GCAM_sector" ), sep = "_", remove = F ) %>% 
  # join with mapping table
  left_join( GCAM_IAMC_mapping, by = c( "GCAM_sector" ) )
# -------------------------------------------------------
  # 2.2. Non-AGR/AWB
# -----------------------------------
    # 2.21. create a table that has all non-AGR/AWB emissions, aside from those that are
    # a special case (currently industrial processes, Unmanaged Land, and trn_pass)
    # We can identify special cases by checking which sectors have multiple mapping entries
special_sectors <- select( GCAM_IAMC_mapping, GCAM_sector )[duplicated( select( GCAM_IAMC_mapping, GCAM_sector ) ), ]

NonAGRAWB <- GCAM_nonco2_emissions %>%
  # filter for AGR and AWB emissions
  filter( !grepl( "AGR|AWB", GHG ),
          !( sector %in% special_sectors ) ) %>% 
  left_join( GCAM_IAMC_mapping, by = c( "sector" = "GCAM_sector" ) )
# -----------------------------------
    # 2.22. Special Case: industrial processes
    # industrial processes subsector "solvents" is mapped to its own output sector
solvents <- GCAM_nonco2_emissions %>%
  # filter for AGR and AWB emissions
  filter( !grepl( "AGR|AWB", GHG ),
          subsector == "solvents" ) %>% 
  # join with mapping table
  left_join( GCAM_IAMC_mapping, by = c( "sector" = "GCAM_sector", "subsector" = "GCAM_subsector" ) )

ind_processes <- GCAM_nonco2_emissions %>%
  # filter for AGR and AWB emissions
  filter( !grepl( "AGR|AWB", GHG ),
          sector == "industrial processes",
          subsector != "solvents" ) %>% 
  # join with mapping table
  left_join( ( GCAM_IAMC_mapping %>% filter( GCAM_subsector != "solvents" ) ), by = c( "sector" = "GCAM_sector" ) )
# -----------------------------------
    # 2.23. Special Case: trn_pass
    # trn_pass subsector "Domestic Aviation" is mapped to its own output sector
dom_aviation <- GCAM_nonco2_emissions %>%
  # filter for AGR and AWB emissions
  filter( !grepl( "AGR|AWB", GHG ),
          subsector == "Domestic Aviation" ) %>% 
  # join with mapping table
  left_join( GCAM_IAMC_mapping, by = c( "sector" = "GCAM_sector", "subsector" = "GCAM_subsector" ) )

trn_pass <- GCAM_nonco2_emissions %>%
  # filter for AGR and AWB emissions
  filter( !grepl( "AGR|AWB", GHG ),
          sector == "trn_pass",
          subsector != "Domestic Aviation" ) %>% 
  # join with mapping table
  left_join( ( GCAM_IAMC_mapping %>% filter( GCAM_subsector != "Domestic Aviation" ) ), by = c( "sector" = "GCAM_sector" ) )
# -----------------------------------
    # 2.24. Special Case: UnmanagedLand
    # UnmanagedLand subsectors map to different output sectors
forest <- GCAM_nonco2_emissions %>%
  # filter for AGR and AWB emissions
  filter( !grepl( "AGR|AWB", GHG ),
          grepl( "Deforest|ForestFire", subsector ) ) %>%
  # separate the subsector for mapping purposes
  separate( subsector, into = c( "subsector", "specific" ), sep = "_", remove = F ) %>% 
  # join with mapping table
  left_join( GCAM_IAMC_mapping, by = c( "sector" = "GCAM_sector", "subsector" = "GCAM_subsector" ) ) %>% 
  # remove "specific" column
  select( -specific )

grassland <- GCAM_nonco2_emissions %>%
  # filter for AGR and AWB emissions
  filter( !grepl( "AGR|AWB", GHG ),
          grepl( "GrasslandFire", subsector ) ) %>% 
  # separate the subsector for mapping purposes
  separate( subsector, into = c( "subsector", "specific" ), sep = "_", remove = F ) %>% 
  # join with mapping table
  left_join( GCAM_IAMC_mapping, by = c( "sector" = "GCAM_sector", "subsector" = "GCAM_subsector" ) ) %>% 
  # remove "specific" column
  select( -specific )

# ------------------------------------------------------------------------------
# 3. Binding Tables and Aggregating
# -------------------------------------------------------
# 3.1. Bind
# -----------------------------------
# Bind all of the separate tables together so we have all emissions, mapped to output sectors
all_emissions_not_agg <- AGRAWB %>% 
  bind_rows( NonAGRAWB, solvents, ind_processes, dom_aviation, trn_pass, forest, grassland )
# -------------------------------------------------------
# 3.2. Aggregate
# -----------------------------------
# aggregate based off of output sector and whether the aggregation should be regional or global
    # Regional
regional <- all_emissions_not_agg %>% 
  # filter for entries that will be aggregated regionally
  filter( Regional == 1 ) %>% 
  # aggregate based off of output sector
  group_by( scenario, region, Units, GHG, Year, Output_Sector ) %>% 
  mutate( value = sum( value ) ) %>% 
  distinct( scenario, region, Units, GHG, Year, Output_Sector, value )

    # Global
global <- all_emissions_not_agg %>% 
  # filter for entries that will be aggregated regionally
  filter( Regional != 1 ) %>% 
  # aggregate based off of output sector
  group_by( scenario, Units, GHG, Year, Output_Sector ) %>% 
  mutate( value = sum( value ) ) %>% 
  distinct( scenario, Units, GHG, Year, Output_Sector, value ) %>% 
  # add a "World" region for these entries
  mutate( "region" = "World" )

    # Bind the Regional and Global tables
all_emissions_agg <- regional %>% 
  bind_rows( global ) %>% 
  ungroup()

# ------------------------------------------------------------------------------
# 4. Formatting
# -------------------------------------------------------
# 4.1. Format Output Table
GCAM_output <- all_emissions_agg %>% 
  # add Model column
  mutate( "Model" = "GCAM52" ) %>% 
  # rename columns to match desired output
  rename( Scenario = scenario,
          Region = region,
          Variable = Output_Sector ) %>% 
  # make Variable column have the right prefix
  mutate( "prefix" = "CEDS+|9+ Sectors|Emissions|",
          "dash" = "|" ) %>% 
  unite( Variable, prefix, GHG, dash, Variable, sep = "", remove = F ) %>% 
  # change SO2 to Sulfur in Variable column
  mutate( Variable = gsub( "SO2", "Sulfur", Variable ),
  # make unit column "Unit Pollutant/yr"
          "per_year" = "/yr" ) %>% 
  unite( Unit, GHG, Units, sep = " " ) %>% 
  unite( Unit, Unit, per_year, sep = "" ) %>% 
  # select columns to retain
  select( -c( prefix, dash ) ) %>% 
  # make table wide
  spread( key = "Year", value = "value" )

# write the harmonized data table
write.xlsx( GCAM_output, file =  "../output/GCAM_output.xlsx", sheetName = "GCAM_output", 
           col.names = T, row.names = F, append = F )

# ------------------------------------------------------------------------------
# 5. Diagnostics
# -------------------------------------------------------
# 5.1. Ensure that all original emissions are accounted for
# -----------------------------------
  # 5.11. get original emissions by scenario, species, and year
GCAM_nonco2_emissions_original <- GCAM_nonco2_emissions %>% 
  separate( GHG, into = c( "GHG", "GCAM_sector" ), sep = "_", remove = F ) %>% 
  # filter out pollutants (currently just f-gasses)
  filter( !( GHG %in% gasses_omit ) ) %>% 
  group_by( scenario, GHG, Year ) %>% 
  mutate( value_orig = sum( value ) ) %>% 
  distinct( scenario, GHG, Year, value_orig )
# -----------------------------------
  # 5.12. get output emissions by species and year
GCAM_nonco2_emissions_output <- GCAM_output %>% 
  gather( key = "Year", value = "value", -c( Scenario, Region, Unit, Variable, Model ) ) %>% 
  separate( Unit, into = c( "GHG", "Other" ), sep = " ", remove = F ) %>% 
  group_by( Scenario, GHG, Year ) %>% 
  mutate( value = sum( value ) ) %>% 
  distinct( Scenario, GHG, Year, value ) %>% 
  ungroup() %>% 
  mutate( Year = as.numeric( Year ) )
# -----------------------------------
  # 5.13. diagnostic check
diagnostic_check <- GCAM_nonco2_emissions_original %>% 
  left_join( GCAM_nonco2_emissions_output, by = c( "scenario" = "Scenario", "Year", "GHG" ) ) %>% 
  mutate( original_minus_output = value_orig - value ) %>% 
  filter( original_minus_output != 0 )

if ( length( diagnostic_check$value > 0 ) ) {
  print( "Warning: output emissions are not equal to input emissions" )
} else {
  print ( "Checkpoint passed: output emissions are equal to input emissions" )
}

# 5.14. diagnostic output
diagnostic <- GCAM_nonco2_emissions_original %>% 
  left_join( GCAM_nonco2_emissions_output, by = c( "scenario" = "Scenario", "Year", "GHG" ) ) %>% 
  mutate( original_minus_output = value_orig - value )

# write the diagnostic table
write.csv( diagnostic, "../output/diagnostic.csv", row.names = F )

