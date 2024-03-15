# equity-dashboard
This repo hosts the R code to collect and prepare data and to create the app for the equity dashboard for regional indicators.

[Charlottesville Regional Equity Dashboard](https://virginiaequitycenter.shinyapps.io/cville-region/)

## datacode

Files to pull and prepare data

* county_codes.csv:	define localities to include
* acs_county_data.R:	get county-level variables from American Community Survey, 5-year estimates
* acs_tract_data.R:	get tract-level variables from American Community Survey, 5-year estimates
* acs_blockgroup_data.R:	get blockgroup-level variables from American Community Survey, 5-year estimates
* acs_county_data_time.R:	get select county-level variables from decennial/acs past years
* acs_tract_data_time.R:	get select tract-level variables from decennial/acs past years
* addl_county_data.R:	get non-census county-level variables
* addl_tract_data.R:	get non-census tract-level variables
* schools_data.R:	get school location and attendance zone data
* parks_data.R:	get local park polygons data
* combine_data.R:	pull data together, derive additional measures, create app_data.R used by app.R

## cville-region

Files to create app

* app.R: the file to create the app
* cville-atlas-template.html: HTML app template file
* deploy.R
* functions/utils.R: Utility functions for custom HTML styled elements in dashboard
* www/app_data.Rdata: data for app generated from datacode
* www/styles.css: Additional styles for dashboard

## stories

Files to create example stories for dashboard (Older Version)

* alb_sales/get_alb_data.R:	get real estate data from county website
* alb_sales/albemarle.R: generate sales by jurisdiction
* alb_sales/alb_story.R: working on summaries/figures to use in alb_homesales_school.Rmd
* alb_sales/alb_homesales_school.Rmd:	generate alb_homesales_school.html doc to include in app
* alb_sales/alb_homesales_school.html: file containing example story (copied to cville-region)
* alb_edequity/BAed_003.Rmd: generate BAed_003.html doc to include in app (built from work on [equity-indicators](https://github.com/commpaslab/equity-indicators))
* alb_edequity/BAed_003.html: file containing example story (copied to cville-region)

