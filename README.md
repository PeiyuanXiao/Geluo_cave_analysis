<samp>RESEARCH COMPENDIUM</samp>

<h1><b><i>Technological behaviors of Upper Paleolithic hominins at the Geluo Cave, southeastern Tibetan Plateau</i></b></h1>

<hr />

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![R 4.5.2](https://img.shields.io/badge/R-4.5.2-blue.svg)](https://www.r-project.org/)

This repository contains the data and R code used to reproduce the lithic analysis figures for the Geluo Cave (GLD; 各洛洞) assemblage from the southeastern Tibetan Plateau.

The compendium is organized so that reviewers can run the analysis from the project root and regenerate all figures in `output/`.

------------------------------------------------------------------------

### :busts_in_silhouette: Authors and affiliations

**Qijun Ruan**<sup>1\*</sup>, **Jinkai Wei**<sup>1\*</sup>, **Peiyuan Xiao**<sup>2,3</sup>, **Yue Meng**<sup>1</sup>, **Jianyu Guan**<sup>1</sup>, **Jianhui Liu**<sup>1</sup>, **Junyi Li**<sup>1</sup>, **Qionghui He**<sup>4</sup>, **Zhenxiu Jia**<sup>2</sup>, **Ben Marwick**<sup>5</sup>, **Zhongping Lai**<sup>6\*</sup>

- <sup>1</sup> *Yunnan Provincial Institute of Cultural Relics and Archaeology, Kunming, 650118, China.*
- <sup>2</sup> *Group of Alpine Paleoecology and Human Adaptation, State Key Laboratory of Tibetan Plateau Earth System Science, Institute of Tibetan Plateau Research, Chinese Academy of Sciences, Beijing, 100101, China.*
- <sup>3</sup> *University of Chinese Academy of Sciences, Beijing, 101408, China.*
- <sup>4</sup> *Weixi Lisu Autonomous County Cultural Relics Management Office, Weixi, 674600, China.*
- <sup>5</sup> *(affiliation to be added)*
- <sup>6</sup> *(affiliation to be added)*

<sup>\*</sup> Corresponding authors: Qijun Ruan, Jinkai Wei, Zhongping Lai *(contact emails to be added)*.

------------------------------------------------------------------------

### :page_with_curl: Abstract

The Geluo Cave is a newly discovered Upper Paleolithic site located in the northwest region of Yunnan Province, southeastern Tibetan Plateau. Two test excavations were conducted at the site in 2022 and 2024, and in total, 336 stone artifacts have been retrieved, along with a small number of faunal remains. Three cultural layers have been identified at the site, and the radiocarbon and optically stimulated luminescence dating have assigned these cultural layers into an age range of ~35–11 ka, corresponding to the late MIS 3, the LGM, and the deglacial periods. Lithic assemblage at the site shows the diverse exploitation of raw materials, including spilite, andesite, quartz, chert, and other types of igneous rocks, all of them were easily available from nearby river gravel. Flakes are the primary type in the assemblage, accompanied by a small number of cores and retouched tools. Overall, lithic industry at the Geluo Cave is characterized by a small sized core and flake technology, which shows similarity with lithic assemblages found in some contemporary sites in East Asia, likely indicating the technological and/or population dispersal across regions.

------------------------------------------------------------------------

### :label: Keywords

Southeastern Tibetan Plateau; Upper Paleolithic; Geluo Cave; Core and flake technology; Early modern human dispersal

------------------------------------------------------------------------

### :open_file_folder: Contents

- [:file_folder: data](data) — analysis-ready Excel workbooks read by the scripts:
  - `GLD_lithic_data.xlsx` — lithic artifact measurements and observations across five sheets: `Core`, `Complete_flake`, `Broken_flake`, `Tool`, and `Chunk`.
  - `GLD_lithic_coord.xlsx` — piece-plotted coordinates of stone artifacts and faunal remains: `x` = 东坐标 (east), `y` = 北坐标 (north), `z` = elevation (m a.s.l.), plus `Trench` and `Type`.
- [:file_folder: R](R) — analysis scripts (run from the project root):
  - [`famd_flakes.R`](R/famd_flakes.R) — FAMD of complete flakes (Layer as a supplementary variable) + PERMANOVA on flake attributes.
  - [`flake_tech_attributes.R`](R/flake_tech_attributes.R) — technological attributes of complete flakes by Toth type (boxplots + bubble composition).
  - [`flake_tech_attributes_stacked.R`](R/flake_tech_attributes_stacked.R) — stacked-bar variant of the attribute composition panels.
  - [`boxplot_by_type.R`](R/boxplot_by_type.R) — size (length/width/thickness/mass) of the main artifact classes.
  - [`spatial_distribution.R`](R/spatial_distribution.R) — plan view (X–Y) and profile (X–Z) of piece-plotted finds by trench and type.
  - [`plot_style.R`](R/plot_style.R) — shared house-style (colours, palettes); sourced by the four non-spatial scripts.
- [:file_folder: output](output) — generated PNG figures, plus `famd_summary.txt` (FAMD/PERMANOVA numeric summary) and `figure_captions.md` (draft captions).
- [:file_folder: Original_raw_data](Original_raw_data) — original field workbooks and the manuscript draft (see [Provenance](#seedling-data-provenance) below); the `data/` workbooks are derived from these.

At the project root, [`Geluo_cave_analysis.Rproj`](Geluo_cave_analysis.Rproj) opens the project in RStudio with the working directory set to the repository root.

> **Note:** the scripts read files using paths relative to the **project root** (for example, `data/GLD_lithic_data.xlsx`) and write to `output/`. Always run them from the repository root — opening `Geluo_cave_analysis.Rproj` in RStudio sets the expected context.

------------------------------------------------------------------------

### :rocket: How to reproduce

1.  Open [`Geluo_cave_analysis.Rproj`](Geluo_cave_analysis.Rproj) in RStudio (this sets the working directory to the repository root), or set it manually with `setwd()`.

2.  Install the required packages (once):

    ``` r
    install.packages(c(
      "readxl", "dplyr", "tidyr", "ggplot2", "patchwork",
      "FactoMineR", "factoextra", "cluster", "vegan", "ggrepel"
    ))
    ```

3.  Source the figure scripts from the project root:

    ``` r
    source("R/famd_flakes.R")
    source("R/flake_tech_attributes.R")
    source("R/flake_tech_attributes_stacked.R")
    source("R/boxplot_by_type.R")
    source("R/spatial_distribution.R")
    ```

4.  The regenerated figures are written to `output/` (existing files with the same name are overwritten).

> A locked package environment (`renv`), a one-shot `run_all.R` driver, and a `Dockerfile` for a fully pinned container are **not yet set up** for this compendium; they can be added later for stricter reproducibility.

------------------------------------------------------------------------

### :bar_chart: Outputs

| Output | Script | Description |
|----|----|----|
| `fig_famd_combined.png` | `famd_flakes.R` | FAMD of complete flakes: individuals by layer, variable correlation circle, and category map. |
| `fig_flake_tech_attributes.png` | `flake_tech_attributes.R` | Technological attributes of complete flakes by Toth type (elongation, IPA, dorsal scar count + composition bubbles). |
| `fig_flake_tech_attributes_stacked.png` | `flake_tech_attributes_stacked.R` | Same attributes with stacked-bar composition panels. |
| `fig_size_by_type_logmass.png` | `boxplot_by_type.R` | Length / width / thickness / mass (log₁₀) of cores, percussion flakes, bipolar-on-anvil flakes, and retouched flakes. |
| `fig_spatial_plan.png` | `spatial_distribution.R` | Plan view (X–Y) of piece-plotted finds by trench (fill) and type (shape). |
| `fig_spatial_profile.png` | `spatial_distribution.R` | Profile (X–Z, elevation) of piece-plotted finds. |
| `fig_spatial_combined.png` | `spatial_distribution.R` | Combined plan + profile panel. |

Draft captions for the main figures are in [`output/figure_captions.md`](output/figure_captions.md). `output/diag_trench_layout_check.png` is a diagnostic (not a manuscript figure) comparing the raw 2024 trench coordinates against the trench-labelling scheme.

------------------------------------------------------------------------

### :computer: Computational environment

- **R:** the analysis was run under R 4.5.2 (Windows).

- **R packages:**

  | Package      | Role                                            |
  |--------------|-------------------------------------------------|
  | `readxl`     | Reading Excel raw-data files.                   |
  | `dplyr`      | Data wrangling.                                 |
  | `tidyr`      | Reshaping data.                                 |
  | `ggplot2`    | Graphics.                                        |
  | `patchwork`  | Composing multipanel figures.                   |
  | `FactoMineR` | Factor analysis of mixed data (FAMD).           |
  | `factoextra` | Extracting and plotting FAMD results.           |
  | `cluster`    | Gower distances for the flake-attribute PERMANOVA. |
  | `vegan`      | PERMANOVA on flake attributes.                  |
  | `ggrepel`    | Non-overlapping plot labels.                    |

To record the exact local environment used for a run, execute `sessionInfo()` after sourcing the scripts.

------------------------------------------------------------------------

### :seedling: Data provenance

The `data/` workbooks are derived from the original field records in [`Original_raw_data/`](Original_raw_data):

- `2022YWT TG1标本初步登记表.xlsx` — 2022 test-trench (TG1) specimen register (relative find positions only).
- `2024GLD-T1遗物坐标.xlsx`, `2024GLD-T2遗物坐标.xlsx`, `2024GLD-T3遗物坐标.xlsx` — 2024 total-station artifact coordinates (北坐标 / 东坐标 / 高程) for the three 2024 excavation areas.
- `石制品测量观察表（地层合并）.xls` — lithic measurement/observation table (layers merged); source for `GLD_lithic_data.xlsx`.
- `Technological behaviors ... Geluo Cave ....docx` — manuscript draft.

------------------------------------------------------------------------

### :clipboard: Notes for reviewers

- **Trench labels and orientation in `spatial_distribution.R`.** Trenches are re-labelled by spatial cluster from the source specimen ID (not the sheet's `Trench` column): `2022YWT…` → **T1**; `24GLD_1/2/4…` (northwest cluster) → **T2**; `24GLD_T1/T2…` (southeast cluster) → **T3**. Both plan-view axes are reversed so that T1 plots at the lower-left, with T2 to the east and T3 to the north, matching the excavation description. The 2022 (T1) finds have no total-station coordinates in the raw records; their positions are reconstructed and are provisional.
- **Layers.** `GLD_lithic_coord.xlsx` has no cultural-layer field, and its specimen IDs do not match those in `GLD_lithic_data.xlsx`, so the spatial plots are grouped by trench (the spatial excavation unit), not by layer.
- **Lithic sheets.** Common categorical fields are normalized to character before the sheets of `GLD_lithic_data.xlsx` are combined, to avoid type conflicts from Excel storing the same field differently across sheets.

------------------------------------------------------------------------

### :memo: License

License to be determined. Please cite the associated manuscript when using or adapting these materials.
