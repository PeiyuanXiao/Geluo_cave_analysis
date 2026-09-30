<samp>RESEARCH COMPENDIUM</samp>

<h1><b><i>Technological behaviors of Upper Paleolithic hominins at the Geluo Cave, southeastern Tibetan Plateau</i></b></h1>

<hr />

[![Project Status: WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip) [![Licenses: MIT + CC BY 4.0 + CC0](https://img.shields.io/badge/Licenses-MIT%20%2B%20CC--BY--4.0%20%2B%20CC0-lightgrey.svg)](LICENSE.md) [![R 4.6.1](https://img.shields.io/badge/R-4.6.1-blue.svg)](https://www.r-project.org/)

This repository contains the data and code for our manuscript, **in preparation**:

> **Ruan, QJ., Wei, JK., Xiao, PY., Meng, Y., Guan, JY., Wen, JX., Liu, JH., Li, JY., He, QH., Jia, ZX., Marwick, B., & Lai, ZP. (in prep.). Technological behaviors of Upper Paleolithic hominins at the Geluo Cave, southeastern Tibetan Plateau.**

The analyses concern the lithic assemblage from the Geluo Cave (GLD; 各洛洞) in northwestern Yunnan. Each script in [`R/`](R) stands on its own: it reads the tables in [`data/`](data) and writes its figures to [`output/`](output).

------------------------------------------------------------------------

### 👥 Authors and Affiliations

**Qijun Ruan**<sup>a</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0009-0000-2143-5335), **Jinkai Wei**<sup>a</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0009-0000-4482-435X)✉, **Peiyuan Xiao**<sup>b,c</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0009-0000-9733-5875), **Yue Meng**<sup>a</sup>, **Jianyu Guan**<sup>a</sup>, **Jiaxin Wen**<sup>a</sup>, **Jianhui Liu**<sup>a</sup>, **Junyi Li**<sup>a</sup>, **Qionghui He**<sup>d</sup>, **Zhenxiu Jia**<sup>b</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0000-0002-7514-4514), **Ben Marwick**<sup>e</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0000-0001-7879-4531)✉, **Zhongping Lai**<sup>f</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0000-0002-0139-9346)✉

- <sup>a</sup> *Yunnan Provincial Institute of Cultural Relics and Archaeology, Kunming, 650118, China.*
- <sup>b</sup> *Alpine Paleoecology and Human Adaptation Group (ALPHA Group), State Key Laboratory of Tibetan Plateau Earth System, Environment and Resources, Institute of Tibetan Plateau Research, Chinese Academy of Sciences, Beijing, 100101, China.*
- <sup>c</sup> *University of Chinese Academy of Sciences, Beijing, 101408, China.*
- <sup>d</sup> *Weixi Lisu Autonomous County Cultural Relics Management Office, Weixi, 674600, China.*
- <sup>e</sup> *Department of Anthropology, University of Washington, Seattle, WA, USA.*
- <sup>f</sup> *Institute of Marine Science, Shantou University, Shantou, Guangdong Province, 515063, China.*

**✉ Corresponding Authors:** Jinkai Wei ([weijinkai7\@163.com](mailto:weijinkai7@163.com)) \* Ben Marwick ([bmarwick\@uw.edu](mailto:bmarwick@uw.edu)) \* Zhongping Lai ([zhongpinglai\@stu.edu.cn](mailto:zhongpinglai@stu.edu.cn))

🔧 **Maintainers:** [Peiyuan Xiao](mailto:xiaopeiyuan@itpcas.ac.cn) & [Jinkai Wei](mailto:weijinkai7@163.com)

------------------------------------------------------------------------

### 📁 Contents

- [:file_folder: data](data): the analysis-ready tables read by the scripts.

  - `GLD_lithic_data.xlsx`: the measurements and technological observations of the lithic assemblage, one sheet per artifact class (`Core`, `Complete_flake`, `Broken_flake`, `Tool`, `Chunk`).
  - `GLD_lithic_coord.xlsx`: the piece-plotted positions of the stone artifacts and faunal remains from the 2022 and 2024 excavations, with `x` east, `y` north and `z` elevation (m a.s.l.), plus the trench and the type of find.
  - `GLD_lithic_coord_T1updated.xlsx`: the same table with one column added, the relative positions (distance from the trench walls, and height) recorded in the field for the 22 finds from the 2022 trench, from which their coordinates were reconstructed. No script reads it; it documents that reconstruction.
  - `site_date.csv`: the coordinates of Geluo and seven comparative sites in Yunnan, in degrees, minutes and seconds.

- [:file_folder: R](R): the analysis scripts.

  - [`famd_flakes.R`](R/famd_flakes.R): factor analysis of mixed data (FAMD) of the complete flakes, with layer as a supplementary variable, and a PERMANOVA on Gower distances testing whether flake attributes differ between layers. It sets the seed (42) and writes its numeric results to `output/famd_summary.txt`.
  - [`flake_tech_attributes.R`](R/flake_tech_attributes.R): the technological attributes of the complete flakes by Toth flake type, as boxplots of elongation, platform angle and dorsal scar count, and bubble charts of cortex, platform type and scar pattern.
  - [`flake_tech_attributes_stacked.R`](R/flake_tech_attributes_stacked.R): the same figure with stacked bars in place of the bubble charts, kept alongside it for comparison.
  - [`boxplot_by_type.R`](R/boxplot_by_type.R): the length, width, thickness and mass of cores, percussion flakes, bipolar-on-anvil flakes and retouched flakes.
  - [`spatial_distribution.R`](R/spatial_distribution.R): the plan (X–Y) and profile (X–Z) of the piece-plotted finds, by trench and type of find.
  - [`plot_style.R`](R/plot_style.R): the shared colours and palettes, sourced by the four non-spatial scripts.
  - [`ET_WT_SR_NPP_analysis.R`](R/ET_WT_SR_NPP_analysis.R): palaeoclimate maps of the region at 34, 20 and 12 ka from the pastclim monthly reconstructions, of effective temperature, mean winter temperature, summer precipitation and annual net primary productivity, with the sites marked.
  - [`Site Occupation Periods and Stone Artifact Counts.R`](R/Site%20Occupation%20Periods%20and%20Stone%20Artifact%20Counts.R): the occupation spans of Geluo and six other sites in Yunnan against the climatic stages from late MIS 3 to early MIS 1, with line width scaled to the number of stone artifacts and colour to the technocomplex. Its site data are entered in the script.

- [:file_folder: output](output): the figures the scripts write (listed under [Outputs](#-outputs)), `famd_summary.txt`, and `figure_captions.md`, the draft captions for the manuscript figures.

- [`Geluo_cave_analysis.Rproj`](Geluo_cave_analysis.Rproj), at the project root, opens the project in RStudio with the working directory set to the root.

- The package environment, at the project root:

  - [`renv.lock`](renv.lock): every R package version the analysis was run under, the two newest scripts included. `renv::restore()` reproduces the library.
  - [`.Rprofile`](.Rprofile) and [`renv/`](renv): activate that library whenever R is started in the project. The installed packages themselves (`renv/library/`) are git-ignored.

------------------------------------------------------------------------

### 🚀 How to Reproduce

There is no container or pipeline for this compendium yet; the scripts are run directly in a local installation of R, with the package versions pinned by `renv.lock`. They were run under R 4.6.1 on Windows.

``` sh
git clone https://github.com/PeiyuanXiao/Geluo_cave_analysis.git
cd Geluo_cave_analysis
```

Open `Geluo_cave_analysis.Rproj` in RStudio, then:

``` r
renv::restore()                      # the package versions in renv.lock

source("R/famd_flakes.R")
source("R/flake_tech_attributes.R")
source("R/flake_tech_attributes_stacked.R")
source("R/boxplot_by_type.R")
source("R/spatial_distribution.R")
```

The five scripts are independent of one another and can be run in any order. Each overwrites its own figures in `output/`.

**The two newest scripts are not yet part of this.** `ET_WT_SR_NPP_analysis.R` reads the pastclim monthly climate reconstructions, which are not in this repository, and a decimal-degree copy of the site coordinates (`site_date_clean.csv`), both from absolute paths on the machine it was written on, and saves its twelve maps there as well. It also downloads the Natural Earth river lines, so it needs a network connection. Its packages are in `renv.lock` with the rest. `Site Occupation Periods and Stone Artifact Counts.R` runs as it is, but draws its figure in the plot window without saving it to `output/`.

------------------------------------------------------------------------

### 📊 Outputs

| Output | Script | Description |
|----|----|----|
| `fig_famd_combined.png` | `famd_flakes.R` | FAMD of complete flakes: individuals by layer, variable correlation circle, and category map. |
| `fig_flake_tech_attributes.png` | `flake_tech_attributes.R` | Technological attributes of complete flakes by Toth type (elongation, IPA, dorsal scar count + composition bubbles). |
| `fig_flake_tech_attributes_stacked.png` | `flake_tech_attributes_stacked.R` | Same attributes with stacked-bar composition panels. |
| `fig_size_by_type_logmass.png` | `boxplot_by_type.R` | Length / width / thickness / mass (log₁₀) of cores, percussion flakes, bipolar-on-anvil flakes, and retouched flakes. |
| `fig_spatial_plan.png` | `spatial_distribution.R` | Plan view (X–Y) of piece-plotted finds by trench (fill) and type (shape). |
| `fig_spatial_profile.png` | `spatial_distribution.R` | Profile (X–Z, elevation) of piece-plotted finds. |
| `fig_spatial_combined.png` | `spatial_distribution.R` | Combined plan + profile panel. |

Draft captions for the main figures are in [`output/figure_captions.md`](output/figure_captions.md). `output/diag_trench_layout_check.png` is a diagnostic, not a manuscript figure, comparing the raw 2024 trench coordinates against the trench-labelling scheme.

------------------------------------------------------------------------

### ⚖️ Licenses

**Text and figures:** [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code:** [MIT](https://opensource.org/licenses/MIT)

**Data:** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/), attribution requested upon reuse

[`LICENSE.md`](LICENSE.md) says which files each one covers.
