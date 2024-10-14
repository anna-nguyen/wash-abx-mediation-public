# WASH Benefits and Antibiotic Use Mediation Analysis 

This repository contains replication scripts for the manuscript "Pathways through which water, sanitation, hygiene, and nutrition interventions 
reduce antibiotic use in young children: a mediation analysis of a cluster-randomized trial."

## Directory Structure

**`0-config.R` :** configuration file that sets data directories, sources base functions, and loads required libraries.

**`0-utils` :** folder containing scripts for generating estimates and confidence intervals, functions to process results for plots/tables, and other general functions. 

**`1-dm` :** folder containing scripts to clean, process, and merge data from the WASH Benefits Bangaldesh trial.

**`2-analysis` :** folder containing scripts fit intervention-outcome, intervetion-mediator, mediator-outcome models; estimate indirect intervention effects.

**`3-figures` :** folder containing scripts to produce plots.

**`4-tables` :** folder containing scripts to produce tables.

**`figures` :** folder containing figure files. 

**`tables` :** folder containing table files.

**`results` :** folder containing various analysis result objects.

**`renv` :** folder containing materials for renv, a package version control tool
