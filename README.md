Supplementary numeric records and figure code

Fixed-Step Black-Box Compressor Parameter Recovery via Shared-Potential Difference Networks and Geometry-Constrained Training
Haoyan Wang

Contents

figure_source_data.json contains the archived numeric values used for the six main figures. The loop1 and loop2 comparisons contain feedback trajectories and endpoints. The angular comparison contains the exact-angle comparison. The ratio comparison contains the compression-ratio weighting comparison. The math examples contain aligned-vector loss and gradient checks. The derivative and documented sections contain the local derivative diagnostics and rounded values retained in the experimental reports.

final_task_data.csv contains 32 endpoint records, comprising two models in two groups of eight tasks. The ratio_weight label denotes the final model, and sobolev denotes the original Sobolev-trained baseline. Task identifiers are zero-based. final_mae is the mean absolute normalized error across the four controls. final_max_axis_error is the largest absolute normalized error across those controls. Every endpoint uses the fourth feedback step.

feedback_summary.csv reports group endpoint means and all-axis success counts. A task succeeds when final_max_axis_error is at most the stated threshold. The error-reduction factors divide the baseline group means by the final-model group mean. Parameter normalization and the plugin configuration are defined in Section II of the manuscript.

Each feedback group contains eight tasks inspected during engineering development. Directed training pairs can share endpoints. These group and pair identifiers describe the recorded evaluation structure.

Recalculate the reported endpoint summaries using Python 3

    python reproduce_summary.py

This script uses the Python standard library and checks the CSV against the archived JSON. It verifies the reported ratios of 18.39 and 10.95 and the combined final-model success counts of 15/16 at 0.02 and 12/16 at 0.005.

Regenerate the main figure layouts using R

    Rscript draw_six_figures.R

The plotting code requires ggplot2, patchwork, and jsonlite, plus Cairo support and the Arial font. The included panel_alignment.R and audit_panel_alignment.py check panel layout during export. The Python executable can be selected through the FXCOMP_PYTHON environment variable. Run from this directory. The code writes Figure-01 through Figure-06 in PNG, PDF, and SVG formats, together with layout diagnostics in qa.

The package contains the numeric records and plotting code used to document the reported figures. The trained controller and plugin rendering environment are described in the manuscript.
