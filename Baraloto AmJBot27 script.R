# =============================================================================
# ECOLOGICAL × ECONOMIC INDEX SCRIPT
# Reproduces Figures 1–5 of Baraloto (2027), American Journal of Botany
# "Revisiting Gentry's Neotropical Plant Family Importance after 40 Years"
#
# AUTHOR:  Christopher Baraloto / ICTB  <cbaraloto@fiu.edu>
# CREATED: May 2026; revised November 2026
# ZENODO:  https://doi.org/10.5281/zenodo.24283494
#
# INPUT:  Appendix S1.xlsx (deposited with this script; two sheets)
#   Sheet 1 "Rankings & Revenue" — revenue, sector scores, economic rank
#   Sheet 2 "Ecological Data"    — log10-normalised eco axes, combined scores
#
# OUTPUT: ms_figures/Figure_1.pdf through Figure_5.pdf
#   Figure 1 — PCA biplot of six ecological axes
#   Figure 2 — REDGEN Abundance vs Global Richness (chosen eco axes)
#   Figure 3 — Combined Index scatter with threshold and quadrant lines
#   Figure 4 — Top-40 Combined Index lollipop plot
#   Figure 5 — Radar plots of 11 qualifying families
#
# QUALIFICATION CRITERIA (dual-threshold or eco-excellence exception):
#   Path 1 (dual threshold): Economic Score >= 31  AND  Ecological Score >= 75
#   Path 2 (eco excellence): Ecological Score >= 83 AND  Economic Score >= 20
#   Thresholds derived from natural-break analysis of score distributions.
#
# DEPENDENCIES:
#   readxl, dplyr, tidyr, ggplot2, ggrepel, corrplot, RColorBrewer
#
# USAGE:
#   Rscript "Baraloto AmJBot27 script.R"
#   — or — source() with working directory set to the Manuscripts/Top10 folder
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

setwd(".")  # run from the folder containing Appendix S1.xlsx

OUT_MS <- "ms_figures"
dir.create(OUT_MS, showWarnings = FALSE)

# =============================================================================
# QUALIFICATION THRESHOLDS
# =============================================================================

THRESH_ECON <- 31 # Economic Score >= 31: natural break above Meliaceae
THRESH_ECO <- 75 # Ecological Score >= 75: top-of-curve cluster boundary
THRESH_ECO_EXCEL <- 83 # Eco floor for exception path (Annonaceae)
THRESH_ECON_MIN <- 20 # Min Economic Score for exception path

# =============================================================================
# PHYLOGENETIC COLOUR SCHEME (APG IV evolutionary order, consistent with guides)
# =============================================================================

PHYLO_BG <- c(
  Gymnosperms    = "#b8d4b8",
  Magnoliids     = "#e8b8e9",
  Monocots       = "#c9bdf0",
  EarlyEudicots  = "#9ecde0",
  COM            = "#a8dba0",
  Fabids         = "#90d4d2",
  Malvids        = "#c2d09a",
  Caryophyllales = "#f5ef9a",
  Ericales       = "#fcd9a0",
  Lamiids        = "#f5c4a0",
  Campanuliids   = "#f0a8a8"
)
PHYLO_ORDER <- names(PHYLO_BG)

ORDER_PHYLO <- c(
  Pinales = "Gymnosperms", Cupressales = "Gymnosperms", Podocarpales = "Gymnosperms",
  Taxales = "Gymnosperms", Ephedrales = "Gymnosperms", Gnetales = "Gymnosperms",
  Magnoliales = "Magnoliids", Laurales = "Magnoliids", Piperales = "Magnoliids",
  Canellales = "Magnoliids",
  Alismatales = "Monocots", Asparagales = "Monocots", Arecales = "Monocots",
  Poales = "Monocots", Zingiberales = "Monocots", Dioscoreales = "Monocots",
  Commelinales = "Monocots",
  Ranunculales = "EarlyEudicots", Proteales = "EarlyEudicots",
  Saxifragales = "EarlyEudicots",
  Fabales = "Fabids", Rosales = "Fabids", Fagales = "Fabids",
  Cucurbitales = "Fabids", Oxalidales = "Fabids", Celastrales = "Fabids",
  Malpighiales = "COM",
  Caryophyllales = "Caryophyllales", Santalales = "Caryophyllales",
  Geraniales = "Malvids", Myrtales = "Malvids", Malvales = "Malvids",
  Sapindales = "Malvids", Brassicales = "Malvids",
  Cornales = "Ericales", Ericales = "Ericales", Lecythidales = "Ericales",
  Solanales = "Lamiids", Gentianales = "Lamiids", Lamiales = "Lamiids",
  Boraginales = "Lamiids", Valerianales = "Lamiids",
  Asterales = "Campanuliids", Dipsacales = "Campanuliids", Apiales = "Campanuliids"
)

get_phylo_grp <- function(order) {
  g <- ORDER_PHYLO[order]
  ifelse(is.na(g), "Other", g)
}

# =============================================================================
# 1. LOAD DATA FROM APPENDIX S1
# =============================================================================

# skip = 3: first three rows are running head, legend, and blank spacer
rev_raw <- read_excel("Appendix S1.xlsx", sheet = "Rankings & Revenue", skip = 3)
eco_raw <- read_excel("Appendix S1.xlsx", sheet = "Ecological Data", skip = 3)

rev_df <- rev_raw %>%
  rename(
    Combined_Rank = `Combined Rank`,
    Guides        = `N Sectors`,
    SynIdx        = `Economic Score`,
    CosmeScore    = `Cosmeceuticals Score`,
    FruitsScore   = `Food & Beverage Score`,
    PharmaScore   = `Pharmaceuticals Score`,
    TimberScore   = `Timber Score`,
    FibersScore   = `Fibers Score`,
    HortScore     = `Horticulture Score`
  ) %>%
  filter(!is.na(Family))

eco_df <- eco_raw %>%
  rename(
    Combined_Rank = `Combined Rank`,
    Guides        = `N Sectors`,
    ATDN_ind_n    = `ATDN Abundance (log norm.)`,
    ATDN_spp_n    = `ATDN Richness (log norm.)`,
    REDGEN_N_n    = `REDGEN Abundance (log norm.)`,
    REDGEN_spp_n  = `REDGEN Richness (log norm.)`,
    Ntrop_spp_n   = `Neotropical Richness (log norm.)`,
    Global_spp_n  = `Global Richness (log norm.)`,
    Eco_Score     = `Ecological Score`,
    SynIdx        = `Economic Score`,
    Combined      = `Combined Score`
  ) %>%
  filter(!is.na(Family))

# Merge on Family; eco axes and Order come from Ecological Data sheet
df <- rev_df %>%
  left_join(
    eco_df %>% select(
      Family, Order,
      ATDN_ind_n, ATDN_spp_n, REDGEN_N_n,
      REDGEN_spp_n, Ntrop_spp_n, Global_spp_n,
      Eco_Score, Combined, Combined_Rank
    ),
    by = "Family"
  ) %>%
  mutate(
    across(c(
      SynIdx, Eco_Score, Combined,
      CosmeScore, FruitsScore, PharmaScore,
      TimberScore, FibersScore, HortScore,
      ATDN_ind_n, ATDN_spp_n, REDGEN_N_n,
      REDGEN_spp_n, Ntrop_spp_n, Global_spp_n
    ), as.numeric),
    Phylo_Grp = get_phylo_grp(Order),
    Top10_Flag = (SynIdx >= THRESH_ECON & Eco_Score >= THRESH_ECO) |
      (Eco_Score >= THRESH_ECO_EXCEL & SynIdx >= THRESH_ECON_MIN),
    Exception_Flag = (Eco_Score >= THRESH_ECO_EXCEL & SynIdx >= THRESH_ECON_MIN) &
      !(SynIdx >= THRESH_ECON & Eco_Score >= THRESH_ECO)
  )

# Restrict to families with eco data for all manuscript figures
df_eco <- df %>%
  filter(!is.na(Eco_Score), Eco_Score > 0, !is.na(Combined)) %>%
  arrange(Combined_Rank) %>%
  mutate(label_c = ifelse(rank(-Combined, ties.method = "min") <= 25, Family, ""))

message(sprintf(
  "Loaded: %d families total, %d with ecological data",
  nrow(df), nrow(df_eco)
))

n_qual <- sum(df_eco$Top10_Flag, na.rm = TRUE)
message(sprintf("Qualifying families: %d", n_qual))
print(
  df_eco %>% filter(Top10_Flag) %>%
    select(Family, SynIdx, Eco_Score, Combined, Combined_Rank, Exception_Flag),
  n = Inf
)

# Medians for quadrant crosshairs (Figure 3)
eco_med <- median(df_eco$Eco_Score)
econ_med <- median(df_eco$SynIdx, na.rm = TRUE)

# =============================================================================
# SHARED PLOT THEME — no title/subtitle, clean bw base for publication
# =============================================================================

MS_BASE <- theme_bw(base_size = 11) +
  theme(
    plot.title       = element_blank(),
    plot.subtitle    = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = "right"
  )

# =============================================================================
# 2. FIGURE 1 — PCA BIPLOT OF SIX ECOLOGICAL AXES
# =============================================================================

pca_dat <- df_eco %>%
  select(
    Family, ATDN_ind_n, ATDN_spp_n, REDGEN_N_n,
    REDGEN_spp_n, Ntrop_spp_n, Global_spp_n
  ) %>%
  filter(complete.cases(.))

pca_mat <- pca_dat %>%
  select(-Family) %>%
  as.matrix()
rownames(pca_mat) <- pca_dat$Family

# scale = TRUE: standardise even though axes are already 0–100 (variances differ)
pca_res <- prcomp(pca_mat, scale. = TRUE)
pca_pct <- round(summary(pca_res)$importance[2, 1:2] * 100, 1)

cat("\n=== PCA summary ===\n")
print(summary(pca_res))
cat("\n=== Loadings ===\n")
print(round(pca_res$rotation, 3))

fig1_scores <- as.data.frame(pca_res$x[, 1:2]) %>%
  tibble::rownames_to_column("Family") %>%
  left_join(df_eco %>% select(Family, Order), by = "Family") %>%
  mutate(Phylo_Grp = get_phylo_grp(Order))

arrow_scale <- max(abs(fig1_scores[, c("PC1", "PC2")])) * 0.45

fig1_arrows <- as.data.frame(pca_res$rotation[, 1:2]) %>%
  tibble::rownames_to_column("var") %>%
  mutate(
    xend = PC1 * arrow_scale * 2.2,
    yend = PC2 * arrow_scale * 2.2,
    label = recode(var,
      ATDN_ind_n   = "ATDN Abund.",
      ATDN_spp_n   = "ATDN Rich.",
      REDGEN_N_n   = "REDGEN Abund.",
      REDGEN_spp_n = "REDGEN Rich.",
      Ntrop_spp_n  = "Neotrop. Rich.",
      Global_spp_n = "Global Rich."
    )
  )

fig1 <- ggplot(fig1_scores, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept = 0, colour = "grey75", linewidth = 0.35) +
  geom_vline(xintercept = 0, colour = "grey75", linewidth = 0.35) +
  geom_point(aes(fill = Phylo_Grp),
    shape = 21, colour = "white", size = 2.8, alpha = 0.85, stroke = 0.35
  ) +
  geom_segment(
    data = fig1_arrows,
    aes(x = 0, y = 0, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    colour = "#333333", linewidth = 0.7, inherit.aes = FALSE
  ) +
  geom_text_repel(
    data = fig1_arrows,
    aes(x = xend * 1.12, y = yend * 1.12, label = label),
    size = 2.5, fontface = "bold", colour = "#333333",
    max.overlaps = 20, seed = 7, inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = PHYLO_BG, breaks = PHYLO_ORDER,
    na.value = "#CCCCCC", name = "Clade"
  ) +
  labs(
    x = sprintf("PC1 (%.1f%% variance)", pca_pct[1]),
    y = sprintf("PC2 (%.1f%% variance)", pca_pct[2])
  ) +
  MS_BASE +
  theme(
    legend.text = element_text(size = 8, face = "italic"),
    legend.title = element_text(size = 9)
  )

ggsave(file.path(OUT_MS, "Figure_1.pdf"), fig1, width = 7, height = 5)
message("Figure 1 saved")

# =============================================================================
# 3. FIGURE 2 — REDGEN ABUNDANCE vs GLOBAL RICHNESS
# The two axes chosen by PCA (PC1-selected) for the composite Ecological Score
# =============================================================================

fig2_dat <- df_eco %>%
  filter(!is.na(REDGEN_N_n), !is.na(Global_spp_n)) %>%
  mutate(lbl = ifelse(
    rank(-REDGEN_N_n, ties.method = "min") <= 15 |
      rank(-Global_spp_n, ties.method = "min") <= 15,
    Family, ""
  ))

fig2 <- ggplot(fig2_dat, aes(x = REDGEN_N_n, y = Global_spp_n)) +
  geom_point(aes(fill = Phylo_Grp),
    shape = 21, colour = "white", size = 3, alpha = 0.85, stroke = 0.4
  ) +
  geom_text_repel(aes(label = lbl),
    size = 2.0, fontface = "italic",
    max.overlaps = 30, seed = 42,
    segment.color = "grey60", segment.size = 0.3
  ) +
  scale_fill_manual(
    values = PHYLO_BG, breaks = PHYLO_ORDER,
    na.value = "#CCCCCC", name = "Clade"
  ) +
  labs(
    x = "REDGEN Abundance (log₁₀-norm, 0–100)",
    y = "Global Species Richness — Angiosperm Phylogeny Group (log₁₀-norm, 0–100)"
  ) +
  MS_BASE +
  theme(
    legend.text = element_text(size = 8, face = "italic"),
    legend.title = element_text(size = 9)
  )

ggsave(file.path(OUT_MS, "Figure_2.pdf"), fig2, width = 7, height = 5.5)
message("Figure 2 saved")

# =============================================================================
# 4. FIGURE 3 — COMBINED INDEX SCATTER WITH THRESHOLD + QUADRANT LINES
# Green lines = qualification thresholds; grey dashed = medians
# Point size = Combined Score (no legend: size is redundant with y-axis order)
# =============================================================================

eco_max <- max(df_eco$Eco_Score, na.rm = TRUE)
econ_max <- max(df_eco$SynIdx, na.rm = TRUE)
eco_min <- min(df_eco$Eco_Score, na.rm = TRUE)

fig3_corners <- data.frame(
  x = c(econ_max * 0.98, 1, econ_max * 0.98, 1),
  y = c(eco_max * 0.97, eco_max * 0.97, eco_min + 1, eco_min + 1),
  hjust = c(1, 0, 1, 0),
  label = c(
    "High Eco / High Econ", "High Eco / Low Econ",
    "Low Eco / High Econ", "Low Eco / Low Econ"
  )
)

fig3 <- ggplot(df_eco, aes(x = SynIdx, y = Eco_Score)) +
  annotate("rect",
    xmin = THRESH_ECON, xmax = Inf, ymin = THRESH_ECO, ymax = Inf,
    fill = "#2d7d46", alpha = 0.06
  ) +
  geom_vline(
    xintercept = THRESH_ECON, linetype = "solid",
    colour = "#2d7d46", linewidth = 0.45, alpha = 0.7
  ) +
  geom_hline(
    yintercept = THRESH_ECO, linetype = "solid",
    colour = "#2d7d46", linewidth = 0.45, alpha = 0.7
  ) +
  annotate("text",
    x = THRESH_ECON + 0.5, y = eco_min + 1,
    label = sprintf("econ ≥ %g", THRESH_ECON),
    colour = "#2d7d46", size = 1.9, hjust = 0, alpha = 0.85
  ) +
  annotate("text",
    x = 1, y = THRESH_ECO + 1,
    label = sprintf("eco ≥ %g", THRESH_ECO),
    colour = "#2d7d46", size = 1.9, hjust = 0, alpha = 0.85
  ) +
  geom_vline(
    xintercept = econ_med, linetype = "dashed",
    colour = "grey60", linewidth = 0.4
  ) +
  geom_hline(
    yintercept = eco_med, linetype = "dashed",
    colour = "grey60", linewidth = 0.4
  ) +
  geom_text(
    data = fig3_corners,
    aes(x = x, y = y, label = label, hjust = hjust),
    size = 2.2, colour = "grey55", fontface = "italic",
    inherit.aes = FALSE
  ) +
  annotate("text",
    x = econ_max * 0.97, y = eco_med + 2,
    label = "median eco", colour = "grey60", size = 2.0, hjust = 1
  ) +
  annotate("text",
    x = econ_med + 1, y = eco_max * 0.97,
    label = "median\necon", colour = "grey60", size = 2.0, hjust = 0
  ) +
  geom_point(aes(fill = Phylo_Grp, size = Combined),
    shape = 21, colour = "white", alpha = 0.87, stroke = 0.4
  ) +
  geom_text_repel(aes(label = label_c),
    size = 2.2, fontface = "italic",
    max.overlaps = 35, seed = 42,
    segment.color = "grey60", segment.size = 0.3
  ) +
  scale_fill_manual(
    values = PHYLO_BG, breaks = PHYLO_ORDER,
    na.value = "#CCCCCC", name = "Clade"
  ) +
  scale_size_continuous(range = c(2, 9), guide = "none") +
  labs(
    x = "Economic Score (0–100)",
    y = "Ecological Score (0–100)"
  ) +
  MS_BASE +
  theme(
    legend.text = element_text(size = 8, face = "italic"),
    legend.title = element_text(size = 9)
  )

ggsave(file.path(OUT_MS, "Figure_3.pdf"), fig3, width = 7, height = 5.5)
message("Figure 3 saved")

# =============================================================================
# 5. FIGURE 4 — TOP-40 COMBINED INDEX LOLLIPOP
# scale_y_discrete(limits=) is authoritative over factor levels.
# =============================================================================

nonqual_levels <- df_eco %>%
  filter(rank(-Combined, ties.method = "min") <= 40, !Top10_Flag) %>%
  arrange(Combined) %>%
  pull(Family)

qual_levels <- df_eco %>%
  filter(Top10_Flag) %>%
  arrange(Combined) %>%
  pull(Family)

lollipop_levels <- c(nonqual_levels, qual_levels)

top40_ms <- df_eco %>%
  filter(Family %in% lollipop_levels) %>%
  mutate(Family = factor(Family, levels = lollipop_levels))

fig4 <- ggplot(top40_ms, aes(y = Family)) +
  geom_tile(
    data = top40_ms %>% filter(Top10_Flag),
    aes(x = 50, y = Family, width = 100, height = 0.9),
    fill = "#2d7d46", alpha = 0.08, inherit.aes = FALSE
  ) +
  geom_vline(
    xintercept = THRESH_ECON, linetype = "dashed",
    colour = "#c0392b", linewidth = 0.45, alpha = 0.75
  ) +
  geom_vline(
    xintercept = THRESH_ECO, linetype = "dashed",
    colour = "#1a6eab", linewidth = 0.45, alpha = 0.75
  ) +
  annotate("text",
    x = THRESH_ECON + 0.8, y = 0.6,
    label = sprintf("econ ≥ %g", THRESH_ECON),
    size = 2.2, hjust = 0, colour = "#c0392b", alpha = 0.85
  ) +
  annotate("text",
    x = THRESH_ECO + 0.8, y = 0.6,
    label = sprintf("eco ≥ %g", THRESH_ECO),
    size = 2.2, hjust = 0, colour = "#1a6eab", alpha = 0.85
  ) +
  geom_segment(aes(x = 0, xend = Combined, yend = Family),
    colour = "grey80", linewidth = 0.5
  ) +
  geom_point(aes(x = Eco_Score),
    shape = 4, size = 1.8,
    colour = "#1a6eab", stroke = 0.8
  ) +
  geom_point(aes(x = SynIdx),
    shape = 3, size = 1.8,
    colour = "#c0392b", stroke = 0.8
  ) +
  geom_point(
    data = top40_ms %>% filter(!Top10_Flag),
    aes(x = Combined, fill = Phylo_Grp),
    shape = 21, size = 3, colour = "white", stroke = 0.35
  ) +
  geom_point(
    data = top40_ms %>% filter(Top10_Flag),
    aes(x = Combined, fill = Phylo_Grp),
    shape = 21, size = 3, colour = "#2d7d46", stroke = 1.2
  ) +
  scale_fill_manual(
    values = PHYLO_BG, breaks = PHYLO_ORDER,
    na.value = "#CCCCCC", name = "Clade"
  ) +
  scale_y_discrete(limits = lollipop_levels) +
  scale_x_continuous(limits = c(0, 100), expand = c(0.01, 0)) +
  annotate("text",
    x = 97, y = 2.4,
    label = "● Combined\n✕ Ecological\n+ Economic",
    size = 2.5, hjust = 1, colour = "grey35"
  ) +
  labs(x = "Score (0–100)", y = NULL) +
  theme_bw(base_size = 10) +
  theme(
    plot.title         = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(face = "italic", size = 8),
    legend.position    = "right"
  )

ggsave(file.path(OUT_MS, "Figure_4.pdf"), fig4, width = 7, height = 10)
message("Figure 4 saved")

# =============================================================================
# 6. FIGURE 5 — RADAR PROFILES FOR QUALIFYING FAMILIES
# 8 axes (top → clockwise): REDGEN Abund., Global Rich.,
#   Cosme, Fibers, Fruits, Hort, Pharma, Timber
# Families labelled by Priority_Rank (1-N among qualifiers only, not global rank)
# =============================================================================

axes_vars <- c(
  "REDGEN_N_n", "Global_spp_n",
  "CosmeScore", "FibersScore", "FruitsScore",
  "HortScore", "PharmaScore", "TimberScore"
)
axes_labels <- c(
  "REDGEN\nAbund.", "Global\nRich.",
  "Cosme", "Fibers", "Fruits", "Hort", "Pharma", "Timber"
)
n_ax <- length(axes_vars)
angles <- seq(0, 2 * pi * (1 - 1 / n_ax), length.out = n_ax)

top_qual <- df_eco %>%
  filter(Top10_Flag) %>%
  arrange(Combined_Rank) %>%
  mutate(
    Priority_Rank = row_number(),
    REDGEN_N_n = replace_na(REDGEN_N_n, 0),
    Global_spp_n = replace_na(Global_spp_n, 0),
    fam_label = factor(
      paste0("#", Priority_Rank, "  ", Family),
      levels = paste0("#", Priority_Rank, "  ", Family)
    )
  ) %>%
  mutate(across(
    c(
      CosmeScore, PharmaScore, FruitsScore, TimberScore,
      FibersScore, HortScore
    ),
    ~ replace_na(as.numeric(.), 0)
  ))

poly_data <- top_qual %>%
  select(fam_label, Phylo_Grp, all_of(axes_vars)) %>%
  pivot_longer(all_of(axes_vars), names_to = "axis_var", values_to = "score") %>%
  mutate(
    axis_idx = match(axis_var, axes_vars),
    angle    = angles[axis_idx],
    x        = score / 100 * sin(angle),
    y        = score / 100 * cos(angle)
  )

# Close each polygon by appending its first vertex
poly_closed <- bind_rows(
  poly_data,
  poly_data %>% group_by(fam_label, Phylo_Grp) %>%
    arrange(axis_idx) %>% slice(1) %>% ungroup() %>%
    mutate(axis_idx = n_ax + 1)
) %>% arrange(fam_label, axis_idx)

grid_df <- expand.grid(
  r = c(0.25, 0.5, 0.75, 1.0),
  theta = seq(0, 2 * pi, length.out = 300)
) %>%
  mutate(x = r * sin(theta), y = r * cos(theta))

spoke_df <- data.frame(
  angle = angles, label = axes_labels,
  x_end = sin(angles), y_end = cos(angles),
  x_lbl = 1.25 * sin(angles), y_lbl = 1.25 * cos(angles)
)

grid_lbl <- data.frame(
  x = 0.03, y = c(0.25, 0.5, 0.75, 1.0),
  label = c("25", "50", "75", "100")
)

fig5 <- ggplot() +
  geom_path(
    data = grid_df, aes(x = x, y = y, group = r),
    colour = "grey88", linewidth = 0.3
  ) +
  geom_segment(
    data = spoke_df,
    aes(x = 0, y = 0, xend = x_end, yend = y_end),
    colour = "grey75", linewidth = 0.35
  ) +
  geom_text(
    data = spoke_df,
    aes(x = x_lbl, y = y_lbl, label = label),
    size = 2.3, lineheight = 0.85, colour = "#444444"
  ) +
  geom_text(
    data = grid_lbl, aes(x = x, y = y, label = label),
    size = 1.8, colour = "grey60", hjust = 0
  ) +
  geom_polygon(
    data = poly_closed,
    aes(x = x, y = y, group = fam_label, fill = Phylo_Grp),
    alpha = 0.28
  ) +
  geom_path(
    data = poly_closed,
    aes(x = x, y = y, group = fam_label, colour = Phylo_Grp),
    linewidth = 0.65
  ) +
  geom_point(
    data = poly_data,
    aes(x = x, y = y, colour = Phylo_Grp), size = 1.4
  ) +
  scale_fill_manual(
    values = PHYLO_BG, breaks = PHYLO_ORDER,
    na.value = "#CCCCCC", guide = "none"
  ) +
  scale_colour_manual(
    values = PHYLO_BG, breaks = PHYLO_ORDER,
    na.value = "#CCCCCC", guide = "none"
  ) +
  coord_equal(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5)) +
  facet_wrap(~fam_label, ncol = 4) +
  theme_void(base_size = 9) +
  theme(
    strip.text = element_text(
      face = "bold.italic", size = 9,
      colour = "#222222", margin = margin(b = 3)
    ),
    panel.spacing = unit(0.9, "cm")
  )

ggsave(file.path(OUT_MS, "Figure_5.pdf"), fig5, width = 7, height = 9)
message("Figure 5 saved")
message("All manuscript figures written to: ", OUT_MS)
