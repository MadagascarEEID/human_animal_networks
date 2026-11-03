library(igraph)
library(bipartite)

# assumes you have already loaded your network objects! Mine are iGraph objects,
# called things like bg_hr_man, bg_full_man, etc. You may need to adjust my code a bit to 
# work with statnet objects

# ── Display names ───────────────────────────────────────────── # so for spatial, make cow/pig/etc.
animal_display <- c(
  rodents       = "Rodents",
  poultry       = "Poultry",
  dogs          = "Dogs",
  cats          = "Cats",
  domestic_pigs = "Dom. Pigs",
  goats_sheep   = "Goats/Sheep",
  cows          = "Cows",
  wild_birds    = "Wild Birds",
  bush_pigs     = "Bush Pigs",
  lemurs        = "Lemurs",
  tenrecs       = "Tenrecs",
  carnivores    = "Carnivores"
)


# Okabe-Ito colorblind-safe palette
# https://jfly.uni-koeln.de/color/ # you can pick new colors for spatial if you want!
OI_orange  <- "#E69F00"   # domesticated (full) / dom. (common) (hr)
OI_skyblue <- "#56B4E9"   # dom. (other) (hr only)
OI_red     <- "#D55E00"   # rodent
OI_green   <- "#009E73"   # wild
OI_blue    <- "#0072B2"   # humans

# Category → color
full_cat_col <- c(
  poultry = OI_orange, dogs = OI_orange, cats = OI_orange,
  domestic_pigs = OI_orange, goats_sheep = OI_orange, cows = OI_orange,
  rodents = OI_red,
  wild_birds = OI_green, bush_pigs = OI_green, lemurs = OI_green,
  tenrecs = OI_green, carnivores = OI_green
)

hr_cat_col <- c(
  poultry = OI_orange, domestic_pigs = OI_orange, cows = OI_orange,
  dogs = OI_skyblue, goats_sheep = OI_skyblue, cats = OI_skyblue,
  rodents = OI_red,
  wild_birds = OI_green, bush_pigs = OI_green, lemurs = OI_green,
  tenrecs = OI_green, carnivores = OI_green
)

cat_order <- c("cows", "domestic_pigs", "poultry",
               "dogs", "goats_sheep", "cats",
               "rodents",
               "bush_pigs", "carnivores", "lemurs", "tenrecs", "wild_birds") # making consistent

# ── Build proper animal × human adjacency matrix ────────────── # 
## I use igraph, so converting to matrix object for 'bipartite' package. 
build_web_matrix <- function(g) {
  animal_vids <- which(!V(g)$type)
  human_vids  <- which( V(g)$type)

  animal_names <- V(g)$name[animal_vids]
  human_names  <- V(g)$name[human_vids]

  # Full adjacency then slice out the bipartite block
  adj <- as_adjacency_matrix(g, sparse = FALSE)
  mat <- adj[animal_names, human_names, drop = FALSE]  # animals × humans

  # Reorder animals by category
  present <- intersect(cat_order, animal_names)
  mat     <- mat[present, , drop = FALSE]

  rownames(mat) <- animal_display[rownames(mat)]
  mat
}


# ── Draw one web panel ────────────────────────────────────────
draw_web <- function(g, village_title, cat_col_map) {
  mat      <- build_web_matrix(g)
  n_humans <- sum(V(g)$type)
  animals_raw <- intersect(cat_order, V(g)$name[!V(g)$type])
  lower_cols  <- cat_col_map[animals_raw]
  names(lower_cols) <- animal_display[animals_raw]
  
  # Named label vector matching rownames(mat) (already display names)
  animal_labels_named <- rownames(mat)
  names(animal_labels_named) <- rownames(mat)
  
  plotweb(
    mat,
    sorting       = "normal",
    horizontal    = TRUE,
    srt           = 0,
    lower_color   = lower_cols,
    higher_color  = paste0(OI_blue, "80"),   # semi-transparent
    lower_border  = "same",
    higher_border = OI_blue,
    lower_labels  = animal_labels_named,                    # species name next to each animal bar
    higher_labels = rep("", ncol(mat)),                     # suppress individual human IDs
    link_color    = "lower",
    link_alpha    = 0.03,
    curved_links  = TRUE,
    text_size     = 0.72,
    lab_distance  = 0.06,
    box_size      = 0.08,
    spacing       = 0.25,
    mar           = c(1, 1, 2.5, 1)
  )
  
  title(
    main     = village_title,
    sub      = paste0("n = ", n_humans, " participants"),
    cex.main = 1.0, font.main = 2,
    cex.sub  = 0.75, col.sub  = "grey40"
  )
}


# ── Legend ────────────────────────────────────────────────────
draw_legend <- function(cat_cols, cat_names) {

   par(mar = c(2, 0, 2, 1)) # you may need to fiddle with this to get legend in right place
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))

  
  legend(
    x        = 0.02, y = 0.97,       # top-left anchor with padding
    bty      = "n",
    title    = expression(bold("Animal category")),
    title.adj = 0,
    legend   = c(cat_names, "Humans"),
    fill     = c(cat_cols, OI_blue),
    border   = NA,
    cex      = 0.88,
    y.intersp = 1.5,
    x.intersp = 0.8,
    xpd      = TRUE                   
  )

}

# ── Render & save ─────────────────────────────────────────────
png("networks_full.png", width = 1450, height = 800, res = 300)
layout(matrix(1:4, nrow = 1), widths = c(3, 3, 3, 2))   # wider legend col
par(oma = c(0, 0, 3, 0))
draw_web(bg_full_amp, "Village A", full_cat_col)
draw_web(bg_full_man, "Village M",                  full_cat_col)
draw_web(bg_full_sar, "Village S",             full_cat_col)
draw_legend(c(OI_orange, OI_red, OI_green),
            c("Domesticated", "Rodent", "Wild"))
mtext("Full Self-Reported Networks",
      outer = TRUE, side = 3, font = 2, cex = 1.3, line = 0.8)
dev.off()

png("networks_high_risk.png", width = 1470, height = 800, res = 300)
layout(matrix(1:4, nrow = 1), widths = c(3, 3, 3, 2.2))
par(oma = c(0, 0, 3, 0))
draw_web(bg_hr_amp, "Village A", hr_cat_col)
draw_web(bg_hr_man, "Village M",                  hr_cat_col)
draw_web(bg_hr_sar, "Village S",             hr_cat_col)
draw_legend(c(OI_orange, OI_skyblue, OI_red, OI_green),
            c("Dom. (common)", "Dom. (other)", "Rodent", "Wild"))
mtext("High-Risk Self-Reported Networks",
      outer = TRUE, side = 3, font = 2, cex = 1.3, line = 0.8)
dev.off()

