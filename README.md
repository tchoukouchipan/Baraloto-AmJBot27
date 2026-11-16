# Revisiting Gentry's Neotropical Plant Family Importance after 40 Years

**Christopher Baraloto** (Florida International University)

*American Journal of Botany*, 2027 — DOI: [to be added after publication]

**Code and data:** https://doi.org/10.5281/zenodo.24283494

---

## Overview

This repository contains the R analysis script and supplementary data table that reproduce all five manuscript figures in Baraloto (2027). The study develops a dual-axis Ecological × Economic Index to identify priority Neotropical plant families where conservation and commercial importance converge, revisiting the landmark floristic surveys of Alwyn Gentry (1988).

## Repository contents

| File | Description |
|------|-------------|
| `Baraloto AmJBot27 script.R` | R script reproducing Figures 1–5 |
| `Appendix S1.xlsx` | Source data: economic scores, ecological axes, combined rankings (118 families) |

## Requirements

R (≥ 4.2) with the following packages:

```r
install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "ggrepel"))
```

## Usage

1. Place both files in the same folder.
2. Open R and set your working directory to that folder, or run from the terminal:

```bash
Rscript "Baraloto AmJBot27 script.R"
```

Output PDFs (`Figure_1.pdf` through `Figure_5.pdf`) are written to a `ms_figures/` subfolder created automatically.

## Data (Appendix S1.xlsx)

**Sheet 1 — Rankings & Revenue:** Annual revenue estimates (USD millions) by economic sector, sector ranks, and log₁₀-normalised scores for 118 angiosperm families across six sectors: Cosmeceuticals, Food & Beverage, Pharmaceuticals, Timber, Fibers, and Horticulture.

**Sheet 2 — Ecological Data:** Six log₁₀-normalised ecological axes (ATDN abundance and richness from the Amazon Tree Diversity Network; REDGEN abundance and richness; Neotropical and global species richness from Angiosperm Phylogeny Group 2016) plus composite Ecological, Economic, and Combined scores.

Revenue sources and correction rationale are documented in the manuscript Methods and Supplementary Data legends.

## Figures produced

| Figure | Description |
|--------|-------------|
| Figure 1 | PCA biplot of six ecological axes, coloured by APG IV phylogenetic group |
| Figure 2 | REDGEN Abundance vs. Global Richness — the two PCA-selected axes for the Ecological Score |
| Figure 3 | Combined Index scatter: Ecological Score vs. Economic Score, with qualification thresholds and quadrant crosshairs |
| Figure 4 | Top-40 lollipop chart showing Combined, Ecological, and Economic scores |
| Figure 5 | Radar profiles of the 11 qualifying families |

## Qualification criteria

Families qualify for the Index (Top10_Flag) via either path:

- **Path 1 (dual threshold):** Economic Score ≥ 31 AND Ecological Score ≥ 75
- **Path 2 (ecological excellence):** Ecological Score ≥ 83 AND Economic Score ≥ 20

Thresholds were identified by natural-break analysis of score distributions. See Methods for full derivation.

## Citation

Baraloto, C. 2027. Revisiting Gentry's Neotropical plant family importance after 40 years. *American Journal of Botany* [volume/pages]. DOI: [to be added]

Code and data: https://doi.org/10.5281/zenodo.24283494

## License

Code: [MIT License](https://opensource.org/licenses/MIT)  
Data: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
