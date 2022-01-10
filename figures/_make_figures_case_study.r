# CASE STUDIES FOR CHEEM ----
## Setup ----
{
  require("cheem")
  require("spinifex")
  require("dplyr")
  require("ggplot2")
  require("cowplot")
  require("patchwork")
  
  wd <- getwd()
  if(substr(wd, nchar(wd)-10, nchar(wd)) != "cheem_paper")
    warning("Expected work directory to be cheem_paper.")
  fp <- "../cheem/inst/shiny_apps/cheem_initial/data/"
  if(F)
    dir("../cheem/inst/shiny_apps/cheem_initial/data/")
  
  ## Load data:
  penguins_ls   <- readRDS(paste0(fp, "preprocess_penguins.rds"))
  fifa_ls       <- readRDS(paste0(fp, "preprocess_fifa.rds"))
  ames2018_ls   <- readRDS(paste0(fp, "preprocess_ames2018.rds"))
  chocolates_ls <- readRDS(paste0(fp, "preprocess_chocolates.rds"))
  
  .x_title      <- "        x: PC1, y: PC2          x: PC1, y: PC2  x: predicted, y: observed"
  .x_title_reg  <- "      x: PC1, y: PC2               x: PC1, y: PC2     x: predicted, y: observed"
  .x_title_reg2 <- "        x: PC1, y: PC2        x: PC1, y: PC2    x: predicted, y: observed"
}

## Penguins classification ------
{
  ## Data setup, spinifex::penguins
  names(penguins_ls)
  prim_obs <- 124L
  comp_obs <- 76L
  
  ### Global view and cheem tour stills
  .glob_view <- global_view(
    penguins_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    labs(color = 'Predicted class', shape = 'Predicted class',x = .x_title) +
    ggtitle("Global view") + theme( 
      plot.margin      = margin(0,0,0,0),
      legend.margin    = margin(0,0,0,0),
      legend.position  = "bottom",
      legend.direction = "horizontal")
  .bas <- basis_attr_df(penguins_ls$attr_df, prim_obs)
  .mv  <- 1 ## seems better than the 3 returned from
  ##manip_var_of_attr_df(penguins_ls$attr_df, prim_obs, comp_obs)
  ## Cheem tour for stills
  mt_interp <- manual_tour(basis = .bas, manip_var = .mv) %>% 
    spinifex:::interpolate_manual_tour(angle = .15) ## app is .15
  dim(mt_interp)
  .ggt1 <- radial_cheem_tour(
    penguins_ls, basis = mt_interp[,,1, drop=FALSE], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 0) + 
    theme(legend.position = "off", aspect.ratio = 2) +
    ggtitle("Radial tour, select frames")
  .ggt2 <- radial_cheem_tour(
    penguins_ls, basis = mt_interp[,,18, drop=FALSE], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 0) + 
    theme(legend.position = "off", aspect.ratio = 2)
  ## Using patchwork:
  .pw <- .ggt1 + .ggt2
  .cp <- cowplot::plot_grid(
    .glob_view, .pw,
    labels = c("a)", "b)"),
    ncol = 1, rel_heights = c(1, 1.6),
    align = 'v', axis = 'l')
}

### Save still shots for paper
ggplot2::ggsave(
  "./figures/case_penguins.png",
  plot = .cp, device = "png",
  width = 4.5, height = 7, units = "in")
.m <- gc()

### Save .mp4, add GitHub urls to paper
.ggt <- radial_cheem_tour(
  penguins_ls, basis = .bas, manip_var = .mv,
  primary_obs = prim_obs, comparison_obs = comp_obs,
  do_add_pcp_segments = TRUE,
  pcp_shape = 124, angle = .15) + ## app angle
  theme(legend.position = "top", legend.direction = "horizontal")
.anim <- animate_gganimate(
  .ggt, fps = 6, res = 100, ## resolution, not the same as dpi, 100 seems about 1x zoom
  height = 800, width = 600, units = "px", ## "px", "in", "cm", or "mm."
  render = gganimate::av_renderer("./figures/case_penguins.mp4")) ## Alternative render
## https://github.com/nspyrison/cheem_paper/blob/main/figures/case_penguins.mp4


## Chocolates classification -----
{
  names(chocolates_ls)
  prim_obs <- 22L #"Dark Chocolate Bar, Lindt, Switzerland"
  comp_obs <- 34L #"85% Cocoa Dark French Chocolate, Thorntos, UK (2nd) 
  if(F)
    global_view(chocolates_ls,
                prim_obs,
                comp_obs)
  
  ### Global view
  .glob_view <- global_view(
    chocolates_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    labs(color = 'Predicted class', shape = 'Predicted class',x = .x_title) +
    ggtitle("Global view") + theme(
      plot.margin      = margin(0,0,0,0),
      legend.margin    = margin(0,0,0,0),
      legend.position  = "bottom",
      legend.direction = "horizontal")
  .inc_var_nms <- c("Calories", "SatFat", "Chol", "Na", "Fiber", "Sugars")
  ## Removed 4 with lowest contribution for the prim_obs.
  .bas <- basis_attr_df(chocolates_ls$attr_df[, .inc_var_nms], prim_obs)
  .mv  <- which(.inc_var_nms == "Sugars")
  mt_interp <- manual_tour(.bas, .mv) %>%
    spinifex:::interpolate_manual_tour(.15) ## App angle.
  dim(mt_interp)
  ## Cheem tour stills for paper
  .ggt1 <- radial_cheem_tour(
    chocolates_ls, basis = mt_interp[,,1], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + theme(legend.position = "off", aspect.ratio = 2) +
    ggtitle("Radial tour, select frames")
  .ggt2 <- radial_cheem_tour(
    chocolates_ls, basis = mt_interp[,,12], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + theme(legend.position = "off", aspect.ratio = 2)
  pw <- .ggt1 + .ggt2
  .cp <- cowplot::plot_grid(
    .glob_view, pw,
    labels = c("a)", "b)"),
    ncol = 1, rel_heights = c(1, 1.6))
}

### Save Stills
ggplot2::ggsave(
  "./figures/case_chocolates.png",
  plot = .cp, device = "png",
  width = 5, height = 7, units = "in")
.m <- gc()

### Save .mp4, add GitHub urls to paper
.ggt <- radial_cheem_tour(
  chocolates_ls, basis = .bas, manip_var = .mv,
  primary_obs = prim_obs, comparison_obs = comp_obs,
  do_add_pcp_segments = TRUE, inc_var_nms = .inc_var_nms,
  pcp_shape = 124, angle = .15) + ## realistic angle
  theme(legend.position = "top", legend.direction = "horizontal")
.anim <- animate_gganimate(
  .ggt, fps = 6, res = 100, ## resolution, not the same as dpi, 100 seems about 1x zoom
  height = 800, width = 600, units = "px", ## "px", "in", "cm", or "mm."
  render = gganimate::av_renderer("./figures/case_chocolates.mp4")) ## Alternative render
## https://github.com/nspyrison/cheem_paper/blob/main/figures/case_chocolates.mp4

## FIFA 2020 wage regression ------
{
  ## load saved cheem_ls
  names(fifa_ls)
  prim_obs <- 1L
  comp_obs <- 8L
  
  ### global view and tours
  .glob_view <- global_view(
    fifa_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    labs(color = 'Predicted class', shape = 'Predicted class',x = .x_title_reg) +
    ggtitle("Global view") + theme(
      plot.margin      = margin(0,0,0,0),
      legend.margin    = margin(0,0,0,0),
      legend.position  = "bottom",
      legend.direction = "horizontal")
  .inc_var_nms <- c("BMI", "react", "off", "def", "mvm", "pwr")
  ## Removed 4 with lowest contribution for the prim_obs.
  .bas <- basis_attr_df(fifa_ls$attr_df[, .inc_var_nms], prim_obs)
  .mv  <- which(.inc_var_nms == "def")
  mt_interp <- manual_tour(.bas, .mv) %>%
    spinifex:::interpolate_manual_tour(.15) ## App angle.
  dim(mt_interp)
  .ggt1 <- radial_cheem_tour(
    fifa_ls, basis = mt_interp[,,1], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + theme(legend.position = "off", aspect.ratio = 1) +
    ggtitle("Radial tour, select frames")
  .ggt2 <- radial_cheem_tour(
    fifa_ls, basis = mt_interp[,,9], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + theme(legend.position = "off", aspect.ratio = 1)
  pw <- .ggt1 / .ggt2
  .cp <- cowplot::plot_grid(
    .glob_view, pw,
    labels = c("a)", "b)"),
    ncol = 1, rel_heights = c(1, 1.4))
}
### Save
ggplot2::ggsave(
  "./figures/case_fifa.png",
  plot = .cp, device = "png",
  width = 6, height = 8, units = "in")
.m <- gc()

### Save .mp4, add GitHub urls to paper
.ggt <- radial_cheem_tour(
  fifa_ls, basis = .bas, manip_var = .mv,
  primary_obs = prim_obs, comparison_obs = comp_obs,
  do_add_pcp_segments = TRUE, inc_var_nms = .inc_var_nms,
  pcp_shape = 124, angle = .15) + ## realistic angle
  theme(legend.position = "off")
.anim <- animate_gganimate(
  .ggt, fps = 6, res = 100, ## resolution, not the same as dpi, 100 seems about 1x zoom
  height = 800, width = 800, units = "px", ## "px", "in", "cm", or "mm."
  render = gganimate::av_renderer("./figures/case_fifa.mp4")) ## Alternative render
## https://github.com/nspyrison/cheem_paper/blob/main/figures/case_fifa.mp4


## Ames Housing 2018 (North Ames) ----
{
  names(ames2018_ls)
  prim_obs <- 311L ## Large lot area to living area ratio ## small house on big lot
  comp_obs <- 220L ## Small on that ratio ## large house small lot
  if(F)
    global_view(ames2018_ls, prim_obs, comp_obs,
                color = ames2018_ls$decode_df$residual,
                shape = factor(ames2018_ls$decode_df$class))
  
  .glob_view <- global_view(
    ames2018_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    labs(color = 'Predicted class', shape = 'Predicted class',x = .x_title_reg) +
    ggtitle("Global view") + theme(
      plot.margin      = margin(0,0,0,0),
      legend.margin    = margin(0,0,0,0),
      legend.position  = "bottom",
      legend.direction = "horizontal")
  .inc_var_nms <- c("LtA", "Qlt", "LvA", "Rms", "GYB", "GrA")
  ## Removed 4 with lowest contribution for the prim_obs.
  .bas <- basis_attr_df(ames2018_ls$attr_df[, .inc_var_nms], prim_obs)
  .mv  <- which(.inc_var_nms == "LvA")
  mt_interp <- manual_tour(.bas, .mv) %>%
    spinifex:::interpolate_manual_tour(.15) ## App angle.
  dim(mt_interp)
  .ggt1 <- radial_cheem_tour(
    ames2018_ls, basis = mt_interp[,,1], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + theme(legend.position = "off", aspect.ratio = 1) +
    ggtitle("Radial tour, select frames")
  # .ggt2 <- radial_cheem_tour( ## Max difference
  #   ames2018_ls, basis = mt_interp[,,8], manip_var = .mv,
  #   primary_obs = prim_obs, comparison_obs = comp_obs,
  #   do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
  #   pcp_shape = 124, angle = 0) + theme(legend.position = "off", aspect.ratio = 1)
  .ggt3 <- radial_cheem_tour(
    ames2018_ls, basis = mt_interp[,,19], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + theme(legend.position = "off", aspect.ratio = 1)
  pw <- .ggt1 / .ggt3
  .cp <- cowplot::plot_grid(
    .glob_view, pw,
    labels = c("a)", "b)"),
    ncol = 1, rel_heights = c(1, 1.4))
}
### Save
ggplot2::ggsave(
  "./figures/case_ames2018.png",
  plot = .cp, device = "png",
  width = 6, height = 8, units = "in")
.m <- gc()

### Save .mp4, add GitHub urls to paper
.ggt <- radial_cheem_tour(
  ames2018_ls, basis = .bas, manip_var = .mv,
  primary_obs = prim_obs, comparison_obs = comp_obs,
  do_add_pcp_segments = FALSE,
  pcp_shape = 124, angle = .15) + ## realistic angle
  theme(legend.position = "off")
.anim <- animate_gganimate(
  .ggt, fps = 6, res = 100, ## resolution, not the same as dpi, 100 seems about 1x zoom
  #height = 10, width = 12, units = "cm", ## "px", "in", "cm", or "mm."
  render = gganimate::av_renderer("./figures/case_ames2018.mp4")) ## Alternative render
## https://github.com/nspyrison/cheem_paper/blob/main/figures/case_ames2018.mp4


### rejected -- Tidy Tuesday coffee -----
if(F){
  if(F) ## Regress score
    browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-07/readme.md")
  coffee <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')
  (skimr::skim(coffee))
  Y <- coffee$total_cup_points
  hist(Y) ## exactly 1 with score 0 will remove
  summary(Y)
  r_idx <- Y > 20
  Y <- Y[r_idx]
  clas <- coffee$species[r_idx]
  X <- coffee[r_idx, c(
    "aroma", "flavor", "aftertaste", "acidity", "body", "balance",
    "uniformity", "clean_cup", "sweetness", "cupper_points", "moisture")]
  
  ## would need modern copy of workflow.
}
