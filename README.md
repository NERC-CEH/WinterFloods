# WinterFloods
Code for rainfall and peak flow analysis (EA project)

## Project: Winter Floods 2019-2021
Author: UKCEH
Version: 1.0 2024-04-30
Version: 1.1 2024-07-22

## Description of files in Final Code Directory

## Catchment Rainfall
 * /CatAvg_HUKG_parallel_FINAL.R
    * Script for use on JASMIN to get catchment averages from HadUK daily rainfall 1km dataset
 * /H24_events_RPs_extract_FINAL.R
   * Find event return periods for depths extracted by H24_event_totals.R
 * /H24_events_RPs_xml_FINAL.R
   * Find event return periods for depths extracted by H24_event_totals.R
 * /H24_events_totals_FINAL.R
   * Extraction of rainfall depths
 * /H24_extract_CatAvg_FEH22_rainfall_FINAL.R
   * Extract rainfall AEP levels from FEH22 grids

## Clustering
* /CLUSTER_ANALYSIS_FINAL.R
  * Script to perform cluster analysis of POT series.
* /cluster_analysis_functions_FINAL.R
  * Functions for use in Cluster Analysis
		
## Context
* /Aligning_events_FINAL.R
  * Matching storms with antecedent conditions
* /SummarisingFunctions_FINAL.R
  * Key functions for context analysis
* /trendvsUrbanvsChange_FINAL.R
  * Matching Land cover to catchments
		
## COSMOS
* /Closest_COSMOS_to_each_station.R
  * Find closest COSMOS site to each river gauging station and export table
* /cosmos_at_event_FINAL.R
  * Matching storms with antecedent conditions
* /Highest_VWC_per_site_FINAL.R
  * Find single highest COSMOS volumetric water content for each site
* /Plot_highest_7-day_VWC_per_site_FINAL.R
  * Plot highest COSMOS volumetric water content for each site
* /Rank_days_FINAL.R
  * Rank COSMOS volumetric water content on each calendar day
* /Rank_month_quarters_FINAL.R
  * Rank COSMOS volumetric water content on each calendar monthquarter
		
## Groundwater
* /Extract_GW_for_events_FINAL.R
  * Extract highest groundwater within 5 days (BEFORE only) of given events at the closest GW station to the flow station recording the event
* /GW_AMAX_FINAL.R
  * Load each groundwater record, extract WY AMAX
* /GW_AMIN_FINAL.R
  * Load each groundwater record, extract WY AMIN
* /GW_event_RPs.R
  * Fit GEV distribution to groundwater AMAX/AMIN
* /GW_FullRec_FINAL.R
  * Load each groundwater record, extract WY AMAX for full record
		
## Nonstationary
* /nonstat_run_FINAL.R
  * Nonstationary flood frequency analysis from nonstat package
* /nonstat_run_phys.R
  * Extra plotting functions for nonstat data.
* /CovariatesForNonst_FINAL.R
  * Getting covariates for nonstationary analysis
		
## Flow
* /all_amax_with_ranks_FINAL.R
  * Get all AMAX and bundle into a single file for extra analysis
* /Event_ranks_from_POT_files_FINAL.R
  * Load compute ranks from annual maxima at NRFA stations from .pt files.
* /Event_RPs_from_AMAX_files_FINAL.R
  * Load compute return periods from annual maxima at NRFA stations.
* /peakflow_uncertainty.R
  * Compute confidence intervals on peak flow using bootstrapping methods
		
## Level
* /level_ranking_compute_compile_FINAL.R
  * Compute ranks of stage measurements for key events
* /LevelRanking_FINAL.R
  * Stage extraction for event ranking of stage for all data sources
		
## Rainfall_monthly
* /Add_rank_&_RP_to_period_AMAX_FINAL.R
  * Read 30-, 60-, 90- and 180-day rainfall maxima, and add ranks and return periods
* /Add_rank_&_RP_to_WF_AMAX_FINAL.R
  * Read 30-, 60-, 90- and 180-day rainfall maxima, and add ranks and return periods
* /Distribution_Rain_MDay.R
  * Read M-day rainfall maxima, choose best and acceptable distributions for each catchment, hence overall best
* /Extract_monthly_AMAX_FINAL.R
  * Read catchment-average daily rainfalls, assign to water years, aggregate monthly totals, find 1, 2, 3 and 6-month maxima
* /Extract_WF_period_AMAX_FINAL.R
  * Read catchment-average daily rainfalls, find 30-, 60-, 90- and 180-day maxima with arbitrary start times within June 2019-June 2021
* /Generate_L-moments_FINAL.R
  * Read 1, 2, 3 and 6-month AMAX, calculate and write L-moments, median, and other derived statistics
		
## Rainfall_pt
* /I4PLA.R
  * FEH22 internal functions
* /Pluvial_event_RPs_FINAL.R
  * Use FEH22 to find event return periods for depths extracted by Pluvial_event_totals_FINAL.R
* /Pluvial_event_totals_FINAL.R
  * Find pluvial event totals for given events

## Rate_of_rise
* /PlotsForRoR.R
  * Rate of rise plots
* /Rate_of_rise_FINAL.R
  * Rate of rise calculations
* /Trends_of_ror_FINAL.R
  * Compute trends in rate of rise at all stations
		
## Volumes
* /Extract_max_week_volumes_NWeek_FINAL.R
  * Combine 15-minute records from Q 15 and Q 15 IR,  Extract maximum volumes per water year, 1, 2, 4, 6, 8-week maximum volumes with START DATE
* /volume_ranking_FINAL.R
  * Ranking volumes within volume AMAX series
* /VolumeDDFs_FINAL.R
  * Plot DDF curves for volume relationships
		
