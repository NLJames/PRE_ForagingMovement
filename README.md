# CH5_PREForaging

Analysis code and manuscript for PhD thesis Chapter 5:

**"Seascape-Driven Space Use in a Coastal Specialist: Tracking Eastern Reef Egrets in a Coral Reef Environment"**
Nicholas L. James, Martijn van der Pol, Graeme S. Cumming
ARC Centre of Excellence for Coral Reef Studies / James Cook University

## Overview

This project analyses GPS/Argos tracking data from Eastern Reef Egrets (*Egretta sacra*, "PRE"/"ERE") breeding on Heron and One Tree Islands, Capricorn Bunker Group, southern Great Barrier Reef. Fourteen birds were tracked (2019–2023, locations every 4 hours) to examine how tides, light, elevation, benthic habitat and sex/morph shape foraging space use, home range size, and colony attendance — with a further scenario analysis of how sea level rise may compress future foraging opportunity.

Key methods: AKDE home range estimation (`ctmm`), GAMMs of seasonal home range variation (`mgcv`), binomial GLMMs of colony attendance/foraging (`glmmTMB`), and tide-offset scenarios for sea level rise.

## Repository structure

| Path | Contents |
|---|---|
| `src/` | Numbered R analysis pipeline (see below) |
| `src_outputs/` | Intermediate `.rds` outputs produced by `src/` scripts |
| `data/` | Raw and reference input data (tracking, tides, DEMs, benthic maps, temperature, etc.) |
| `data/DFs/` | Processed/derived data frames used downstream |
| `WriteUp/` | Manuscript, supplementary materials, and figures |
| `WriteUp/MS/` | Main manuscript (`CH5_PRE_MS.qmd`), SI figures/tables, journal style files, bibliography |
| `figures/`, `tables/` | Final figures and tables (as `.rds`/`.csv`) for the manuscript |
| `archive/` | Superseded scripts and earlier manuscript drafts |
| `runDown.qmd` / `.Rmd` | Analysis walkthrough/summary document |
| `PRE_PrelimData.Rmd` | Preliminary data exploration |
| `CH5_PREForaging.Rproj` | RStudio project file |

## Analysis pipeline (`src/`)

Scripts below are the ones that actually feed the current manuscript (`WriteUp/MS/`), either by being `source()`-d directly or by producing an `.rds`/figure that a manuscript document reads:

1. `1.egTracks_HI_Wist.R`, `2.egTracks_OTI.R`, `2.1combineTracks.R` — import and clean raw Argos tracks for Heron/Wistari and One Tree Island, merge into `cleanTracks.rds`
2. `3.demData.R` — extract elevation from hierarchical DEM sources (Allen Coral Atlas, QLD Govt, DEA, GBR DEM)
3. `4.tides.R` — join BOM tide heights, classify Low/Mid/High tide
4. `5.LightDark.R` — compute day/night, dawn/dusk, moon illumination
5. `6.BenthosType.R` — classify locations by Allen Coral Atlas geomorphic/benthic zone
6. `AIMSloggerData.R`, `7.waterTemps.R` — join in-situ AIMS logger water temperatures
7. `8.colonyDistances.R`, `8.forgKDE.R`, `8.1akdeMonthly.R` — distance-to-colony metrics and monthly AKDE home range estimation
8. `12.ForagingModel.R` — binomial GLMM(s) of colony attendance/foraging vs. tide, light, moon, sex, etc.
9. `14.2forecastForagingOpportunity.R` — sea level rise scenario forecasting of foraging opportunity
10. `space_use_analysis20250610.R` — seasonal/tidal KDE and site-fidelity GAMM plots
11. `kde_monthlyAnimation.R` — monthly KDE overlay animation (embedded in the SI as a gif)

### Archived scripts (`archive/src/`)

The following were moved out of the active pipeline in 2026-09 after an audit confirmed none of their outputs are read by any manuscript document (`WriteUp/MS/*.qmd`/`*.Rmd`) — either they save nothing, their output is only referenced in commented-out code, or their only consumer was an exploratory scratch document (`WriteUp/spaceUse_kdeOverlap.Rmd`): `8.2akdeWeekly.R`, `9.1AKDE_monthlyGAMM.R`, `9.2AKDE_weeklyGAMM.R`, `9.foragingBehaviour.R`, `13.plotFinalModel.R`, `9.socialDistances.R`, `9.2socialDistances_offIsland.R`, `9.3socialDistances_onIsland.R`, `territoryOverlap.R`, `turbidity.R`, `RSLR_plot.R`, `sentinelBasemap.R`, `shorelineLocs.R`. Revive from `archive/src/` if any of these analyses are reinstated into the manuscript.

## Data sources

- Argos/GPS tracking data (Lotek Solar PTT tags)
- BOM tide records (Heron & One Tree Islands)
- Allen Coral Atlas (bathymetry, benthic/geomorphic maps, turbidity)
- Queensland Government and DEA digital elevation models
- AIMS in-situ temperature loggers (Heron Island reef flat & slope)
- AIMS GIS Great Barrier Reef feature boundaries

Raw data licensing/attribution details are documented in `data/ACA_HERON/1-License-and-Documentation/`.

## Outputs

- Manuscript: `WriteUp/MS/CH5_PRE_MS.qmd` (Word output via MEPS journal style)
- Supplementary figures/tables: `WriteUp/MS/CH5_PRE_SI_Figures.Rmd`, `CH5_PRE_SI_Tables.Rmd`
- Final figures/tables for publication: `figures/`, `tables/`

## Known reproducibility gaps

Two inputs consumed by active parts of the pipeline have no script in this repo (or `archive/`) that produces them — they exist only as files already on disk:

- `data/DFs/monthlyKDE.rds`, read by `WriteUp/MS/MS_final_plots.qmd` to build manuscript Figure 1. The file is dated July 2023; only archived/superseded scripts reference that exact filename, and the current pipeline (`8.forgKDE.R`) writes a similarly named but different file (`data/DFs/monthlyAllLocsKDE.RDS`).
- `src_outputs/tidesLight.rds`, read by `space_use_analysis20250610.R` (which does feed manuscript figures). No script anywhere writes this file.

A clean re-run of the pipeline from raw data will not regenerate these two files as things stand. Worth resolving before submission/archiving to Dryad/Zenodo — either recover/rewrite the script that produced them, or regenerate them under their expected filenames from the current pipeline.

## Requirements

R with: `tidyverse`, `sf`, `terra`/`raster`, `ctmm`, `amt`, `mgcv`, `gratia`, `glmmTMB`, `DHARMa`, `MuMIn`, `geojsonsf`, `suncalc`-equivalent (dawn/dusk/moon), `lubridate`, `patchwork`, `janitor`, `skimr`.

## Status

Manuscript in preparation for submission (working title above; target journal styled via MEPS reference docx, with PNAS/Nature EE style files also present).
