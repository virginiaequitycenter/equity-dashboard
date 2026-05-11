# Charlottesville Regional Data Dashboard

README updated 2026-05-11

This repo hosts the R code to collect and prepare data and to create the equity dashboard for regional indicators for the Charlottesville region.

[Charlottesville Regional Data Dashboard](https://virginiaequitycenter.shinyapps.io/cville-region/)

## `dashboard-update-workflow.RMD`

Data workflow for collecting and preparing data for dashboard. This RMD file runs R scripts found in `/scripts` and saves data to specific dashboard year `/data` folder.

## `/scripts`

Script files are run from `dashboard-update-workflow.RMD`. Each script file contains a TOC of included measures. Scripts included:

-   `acs5-blockgroup-data.R`: blockgroup-level variables from US census ACS 5-year estimates
-   `acs5-county-data.R`: county-level variables from US census ACS 5-year estimates
-   `acs5-tract-data.R`: tract-level variables from US census ACS 5-year estimates
-   `alice-county-data.R`: (ALICE)[<https://www.unitedforalice.org/>] data
-   `helpers.R`: helper text for app interface
-   `life-exp-county-data.R`: life expectancy county-level data
-   `magdist-data.R`: magisterial district geometry data
-   `parks-osm-data.R`: local park polygons data
-   `schools-data.R`: school location and attendance zone geometry data
-   `seg-county-tract-data.R`: segregation measures for county and tract level

This folder also includes files required for the data workflow:

-   `regional_tractnames.csv`: defines tract names for all localities included in app
-   `prettytable_data_sources.xlsx`: metadata descriptions for all measures in app

## `/data`

All collected and cleaned data are saved in the specific update year folder in `/data` (not currently in Github repo). These include:

-   `acs5_blockgroup.RDS`
-   `acs5_county.RDS`
-   `acs5_tract.RDS`
-   `alice.RDS`
-   `county_life_exp.RDS`
-   `mcd_sf.geojson`:
-   `parks_OSM_sf.geojson`
-   `sabselem_sf.geojson`
-   `sabshigh_sf.geojson`
-   `schools_sf.geojson`
-   `seg_county.RDS`
-   `seg_tract.RDS`
-   `app_data_[dashboard year].Rdata`

## `/cville-region`

Files to create app

-   `app.R`: the file to create the app
-   `cville-atlas-template.html`: HTML app template file
-   `deploy.R`
-   `www/app_data_[dashboard year].qs2`: data for app generated from datacode
