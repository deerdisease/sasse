# sasse

**Project:** Surveillance Analysis and Sample Size Explorer (SASSE)

SASSE is a module-based teaching tool developed to reach a broad audience in the wildlife field. The tool presents an interactive graphical interface for learning how to identify effecient sampling designs for specific surveillance objectives. SASSE currently covers modules on the key surveillance objectives of detection, prevalence, and epidemiological dynamics in Version 1. Ongoing work is being done to develop modules on additional objectives related to seasonality or spatial trends. This repository contains training materials and documentation, links to the live app, app code, and model code.

*note: The disease freedom portion of SASSE utilizes similar models to those in the simple random sampling apps published by Hanley et al. (https://doi.org/10.7298/ka5p-bj90 ; https://doi.org/10.7298/smzr-1a70). Models used in the Hanley et al. apps (Booth et al., 2023, 2025) assume that hosts naturally cluster into social groups that share disease more readily among group members, while models used in SASSE incorporate a stratification component, using sampling strata to represent a set of hosts that share a common likelihood of being sampled. Users should note these differences and how they align with the proposed study when interpreting planning recommendations from the two apps.*

**Shiny App:** https://deerdisease.shinyapps.io/Wildlife-surveillance-design-tools/

*note: If you are experiencing slow speeds it is because the Shiny subscription for hosting this application has been downgraded as it is being converted to a downloadable version for running locally. SASSE app code has been uploaded as a repository. Additional packaging to convert SASSE into a package for running locally in a function is under development.*

## Training materials and documentation

- Slides, app, and technical documentation from TWS workshop 2024
    - [workshop slides](TWS%20Workshop.pdf)
    - technical documentation for [detection and prevalence modules](model_details.pdf)
    - technical documentation for [SIBR epidemiological model](epi_model_details.pdf)

## App code

- App code and dependencies can be found in the folder 'shiny'
- appDraft.R contains ui and server scripts, along with the call to runApp
- 'shiny/utils' contains model and plotting functions called in appDraft.R. App code will load utility functions, but user must ensure all packages in the utility functions are loaded.
- power study data is pre-simulated for the app and must be generated using power study model code or downloaded from [Power Study Data](https://doi.org/10.5281/zenodo.16646282)

## Model code

- Code to run just the models from SASSE (without app output) can be found in the folder 'model code'
- Some model scripts may require utility functions from 'shiny/utils'
- Please note these are not final scripts for the models, they are sketches used to derrive our tool

  
