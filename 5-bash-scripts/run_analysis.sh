#!/bin/bash

#SBATCH --job-name=run_diarrhea_analysis
#SBATCH --begin=now
#SBATCH --dependency=singleton
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=16
#SBATCH --mem=64G
#SBATCH --mem=64G
#SBATCH --output=01-wbb-em.out
#SBATCH --time=2-00:00:00

#module purge

#module load R/4.0.2

#cd $HOME/Documents/Research/wash-abx-mediation

R CMD BATCH --no-save 1-dm/1-process_wash_data.R x-bash-scripts/outputs/1-dm/1-process_wash_data.out

R CMD BATCH --no-save 2-analysis/0-model_tr_outcome.R x-bash-scripts/outputs/2-analysis/0-model_tr_outcome.out
R CMD BATCH --no-save 2-analysis/1-model_tr_mediator.R x-bash-scripts/outputs/2-analysis/1-model_tr_mediator.out
R CMD BATCH --no-save 2-analysis/2-model_mediator_outcome.R x-bash-scripts/outputs/2-analysis/2-model_mediator_outcome.out
R CMD BATCH --no-save 2-analysis/3-complete_mediation_analysis.R x-bash-scripts/outputs/2-analysis/3-complete_mediation_analysis.out

R CMD BATCH --no-save 3-fig-tab/1-plot_tr_outcome_effects.R x-bash-scripts/outputs/3-fig-tab/1-plot_tr_outcome_effects.out
R CMD BATCH --no-save 3-fig-tab/2-plot_tr_mediator_effects.R x-bash-scripts/outputs/3-fig-tab/2-plot_tr_mediator_effects.out
R CMD BATCH --no-save 3-fig-tab/3-plot_mediator_outcome_effects.R x-bash-scripts/outputs/3-fig-tab/3-plot_mediator_outcome_effects.out
R CMD BATCH --no-save 3-fig-tab/4-plot_indirect_effects.R x-bash-scripts/outputs/3-fig-tab/4-plot_indirect_effects.out


