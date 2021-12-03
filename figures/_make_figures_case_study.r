# CASE STUDIES FOR CHEEM ----
## Setup ----
{
  require("cheem")
  require("spinifex")
  require("dplyr")
  require("ggplot2")
  require("cowplot")
  
  setwd("~/R/cheem_paper")
  fp <- "../cheem/inst/shiny_apps/cheem_initial/data/"
  if(F)
    dir("../cheem/inst/shiny_apps/cheem_initial/data/")
  
  ## Load data:
  penguins_ls   <- readRDS(paste0(fp, "preprocess_penguins.rds"))
  fifa_ls       <- readRDS(paste0(fp, "preprocess_fifa.rds"))
  ames2018_ls   <- readRDS(paste0(fp, "preprocess_ames2018.rds"))
  chocolates_ls <- readRDS(paste0(fp, "preprocess_chocolates.rds"))
  

  .x_axis_title     <- "          x: PC1, y: PC2        x: PC1, y: PC2  x: predicted, y: observed"
  .x_axis_title_reg <- "          x: PC1, y: PC2             x: PC1, y: PC2     x: predicted, y: observed"
}

## Penguins classification ------
{
  ## Data setup, spinifex::penguins
  names(penguins_ls)
  prim_obs <- 124L
  comp_obs <- 86L
  
  ### Global view and cheem tour stills
  .glob_view <- global_view(
    penguins_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    theme(legend.position = "right") +
    labs(color = 'Predicted class', shape = 'Predicted class',
         x = .x_axis_title) + 
    ggtitle("Global view")
  .bas <- basis_attr_df(penguins_ls$attr_df, prim_obs)
  .mv  <- manip_var_of_attr_df(penguins_ls$attr_df, prim_obs, comp_obs)
  .ggt <- radial_cheem_tour(
    penguins_ls, basis = .bas, manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 6) + theme(legend.position = "")
  .cheem_stills <- spinifex::filmstrip(.ggt, nrow = 1) +
    ggtitle("Cheem tour, extrema contributions")# +
    #theme(plot.title = element_text(hjust = 0.05))
  .cp <- cowplot::plot_grid(
    .glob_view, .cheem_stills,
    labels = c("a)", "b)"),
    ncol = 1)#, rel_heights = c(1.5, 1))
}

### Save
ggplot2::ggsave(
  "./figures/case_penguins.png",
  plot = .cp, device = "png",
  width = 6, height = 5, units = "in")
.m <- gc()



## Chocolates classification -----
{
  names(chocolates_ls)
  prim_obs <- 22L
  comp_obs <- 34L
  if(F)
    global_view(chocolates_ls, 
                22, #"Dark Chocolate Bar, Lindt, Switzerland"
                49) #"Pure Dark Chocolate, Hershey's, US"
  
  ### Global view and cheem tour stills
  .glob_view <- global_view(
    chocolates_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    theme(legend.position = "right") +
    labs(color = 'Predicted class', shape = 'Predicted class',
         x = .x_axis_title) +
    ggtitle("Global view")
  .bas <- basis_attr_df(chocolates_ls$attr_df, prim_obs)
  .mv  <- manip_var_of_attr_df(chocolates_ls$attr_df, prim_obs, comp_obs)
  .ggt <- radial_cheem_tour(
    chocolates_ls, basis = .bas, manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 6) + theme(legend.position = "")
  .cheem_stills <- spinifex::filmstrip(.ggt, nrow = 1) +
    ggtitle("Cheem tour, extrema contributions")
  .cp <- cowplot::plot_grid(
    .glob_view, .cheem_stills,
    labels = c("a)", "b)"),
    ncol = 1)#, rel_heights = c(1.5, 1))
}
### Save
ggplot2::ggsave(
  "./figures/case_chocolates.png",
  plot = .cp, device = "png",
  width = 6, height = 5, units = "in")
.m <- gc()


## FIFA 2020 wage regression ------
{
  ## load saved cheem_ls
  names(fifa_ls)
  prim_obs <- 1L
  comp_obs <- 8L
  
  ### global view and tours
  .glob_view <- global_view(
    fifa_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    theme(legend.position = "off") +
    labs(x = .x_axis_title_reg) +
    ggtitle("Global view")
  ## Without reaction and movement
  .bas <- basis_attr_df(fifa_ls$attr_df, prim_obs)[-c(3,7),, drop=FALSE]
  .mv  <- 3 ## "offense, that is what is talked about in the paper.
  .ggt <- radial_cheem_tour(
    fifa_ls, basis = .bas, manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 6, inc_var_nms = colnames(fifa_ls$attr_df)[-c(3,7)]) +
    theme(legend.position = "")
  .cheem_stills <- spinifex::filmstrip(.ggt, nrow = 3) +
    ggtitle("Cheem tour, extrema contributions")
  .cp <- cowplot::plot_grid(
    .glob_view, .cheem_stills,
    labels = c("a)", "b)"),
    ncol = 1, rel_heights = c(1, 2))
}
### Save
ggplot2::ggsave(
  "./figures/case_fifa.png",
  plot = .cp, device = "png",
  width = 5.5, height = 8, units = "in")
.m <- gc()


## Ames Housing 2018 (North Ames) ----
{
  names(ames2018_ls)
  prim_obs <- 170L
  comp_obs <- 220L
  if(F)
    global_view(ames2018_ls, prim_obs, comp_obs,
                color = ames2018_ls$decode_df$residual,
                shape = factor(ames2018_ls$decode_df$class))
  
  ### global view and tours
  .glob_view <- global_view(
    ames2018_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    theme(legend.position = "off") +
    labs(x = .x_axis_title_reg) +
    ggtitle("Global view")
  .bas <- basis_attr_df(ames2018_ls$attr_df, prim_obs)
  .mv  <- manip_var_of_attr_df(ames2018_ls$attr_df, prim_obs, comp_obs)
  .ggt <- radial_cheem_tour(
    ames2018_ls, basis = .bas, manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 6) + theme(legend.position = "")
  .cheem_stills <- spinifex::filmstrip(.ggt, nrow = 3) +
    ggtitle("Cheem tour, select frames")
  .cp <- cowplot::plot_grid(
    .glob_view, .cheem_stills,
    labels = c("a)", "b)"),
    ncol = 1, rel_heights = c(1, 2))
}
### Save
ggplot2::ggsave(
  "./figures/case_ames2018.png",
  plot = .cp, device = "png",
  width = 8, height = 8, units = "in")
.m <- gc()


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
