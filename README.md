# hicontrol

**Control chart rules for influenza HI titre data**

[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

`hicontrol` implements eight statistical process control rules adapted for
repeated influenza haemagglutination inhibition (HI) titre measurements. It
is designed for laboratory quality control of reference serum panels in
large-scale surveillance programmes.

Two properties of HI titres make standard SPC software awkward to apply:

- **Interval censoring.** Titres are reported on a doubling-dilution scale
  (10, 20, 40, 80, 160, …), so each reading is known only to the nearest
  log2 unit. The package works in log2 units throughout.
- **Left censoring.** Readings below the detection limit are reported as
  `"<10"`, `"<20"`, etc. These are converted to half the detection limit
  before log-transformation.

## Installation

```r
# From CRAN (once released):
install.packages("hicontrol")

# Development version from GitHub:
# install.packages("pak")
pak::pak("drserajames/hicontrol")
```

## Quick start

```r
library(hicontrol)

# Convert raw HI titre strings to log2 scale
raw <- c("80", "160", "40", "80", "<10", "160")
suppressWarnings(log_num(raw))
#> [1]  3  4  2  3 -1  4

# Simulate a reference serum time series with an abrupt step change at run 11
centre    <- 3       # target log2 titre (= titre 80)
threshold <- 1       # 1 SD unit on log2 scale
dat_ooc <- c(3, 4, 2, 3, 4, 3, 2, 4, 3, 3, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7)

# Apply all eight rules in one call
res <- hi_rules(dat_ooc, centre, threshold)

# False-positive rate per rule (0 = no violation)
res[[2]]
#> [1] 0.10 0.00 0.50 0.50 0.00 0.00 0.00 0.05

# Visualise
n_length <- c(1, 3, 8, 9, 25, 10, 4, 2)
plot_nelson(dat_ooc, res[[1]], centre, threshold, n_length)
```

## The eight control rules

| Rule | Description | Signals |
|------|-------------|---------|
| 1 | 1 point >= 3 SD from centre | Extreme outlier; transcription or pipetting error |
| 2 | 2 of 3 consecutive points >= 2 SD from centre | Incipient shift or persistent mild outlier |
| 3 | 8 consecutive points on the same side of centre | Sustained systematic bias |
| 4 | 9 consecutive points >= 1 SD (either side) | Systematic bias including zone-boundary values |
| 5 | 25 consecutive identical values | Frozen assay; discretisation artefact |
| 6 | 10 consecutive alternating up-down points | Over-correction between runs |
| 7 | 4 consecutive points in a monotone trend | Progressive drift (reagent degradation) |
| 8 | Single-step difference >= 3 log2 units | Abrupt process change between consecutive runs |

Rules 1-4 and 8 are adapted from Westgard rules; rules 5-7 are from Nelson
(1984).

## Functions

| Function | Description |
|----------|-------------|
| `log_num()` | Convert HI titre strings to log2 scale |
| `hi_rules()` | Apply all 8 rules; return raw results and false-positive rates |
| `hi_rules2()` | Apply all 8 rules; return raw results and flagged counts |
| `plot_nelson()` | Nelson-style control chart with coloured rule violations |
| `plot_hi()` | Same chart with a symmetric y-axis |
| `plot_hi2()` | Compact chart for multi-panel layouts (pink background on any violation) |
| `ref_panel_plot()` | Batch PDF of all antigen/serum pairs in a Racmacs map |
| `rule_xyz()` | x-of-y points beyond a zone threshold |
| `rule_trend()` | Monotone trend detection |
| `rule_noC()` | Run of points outside zone C (strict) |
| `rule_noCrelax()` | Run of points outside zone C (relaxed, includes boundary) |
| `rule_onlyC()` | Run of points confined to zone C |
| `rule_alt()` | Alternating (zig-zag) pattern detection |
| `rule_nodiff()` | Run of identical consecutive values |
| `rule_diff()` | Large single-step difference detection |

All individual rule functions are exported and can be used independently of
`hi_rules()`.

## Violation colour scheme

The plot functions use a consistent colour scheme across all chart variants:

| Colour | Rule |
|--------|------|
| Red | 1 — point >= 3 SD |
| Orange | 2 — 2 of 3 points >= 2 SD |
| Yellow | 3 — 8 points on one side |
| Tan | 4 — 9 points >= 1 SD (relaxed) |
| Sea green | 5 — 25 identical values |
| Purple | 6 — 10 alternating points |
| Blue | 7 — monotone trend |
| Magenta | 8 — large single step |

## Reference panel charts

For datasets stored as Acmacs map objects (from the
[Racmacs](https://acorg.github.io/Racmacs/) package),
`ref_panel_plot()` iterates over every antigen/serum pair with sufficient
repeat measurements, applies `hi_rules2()`, and writes a multi-panel PDF:

```r
library(Racmacs)
map <- read.acmap("my_map.ace")
ref_panel_plot(map, name = "2024H3", min_n = 5,
               file = "qc_reference_panel_2024H3.pdf")
```

## License

GPL-3. See [LICENSE](https://www.gnu.org/licenses/gpl-3.0) for details.
