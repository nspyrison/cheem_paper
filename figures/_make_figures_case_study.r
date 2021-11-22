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
  penguins_ls   <- readRDS(paste0(fp, "1preprocess_penguins.rds"))
  fifa_ls       <- readRDS(paste0(fp, "3preprocess_fifa.rds"))
  ames2018_ls   <- readRDS(paste0(fp, "7preprocess_ames2018.rds"))
  chocolates_ls <- readRDS(paste0(fp, "preprocess_chocolates.rds"))
}

## Penguins classification ------
{
  ## Data setup, spinifex::penguins
  names(penguins_ls)
  prim_obs <- 15L
  comp_obs <- 282L
  
  ### Global view and cheem tour stills
  .glob_view <- global_view(
    penguins_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    theme_bw() +
    labs(x = "PC1") +
    theme(legend.position = "off",
          axis.text  = element_blank(),
          axis.ticks = element_blank())
  # legend.direction = "vertical", ## Levels within aesthetic
  # legend.box = "horizontal",     ## Between aesthetic
  # legend.margin = ggplot2::margin(1L,1L,1L,1L, "mm")) +
  # labs(color='Predicted class', shape='Predicted class')
  
  .bas <- basis_attr_df(penguins_ls$attr_df, prim_obs)
  .mv  <- manip_var_of_attr_df(penguins_ls$attr_df, prim_obs, comp_obs)
  .ggt <- radial_cheem_tour(
    penguins_ls, basis = .bas, manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 6) + theme(legend.position = "")
  .cheem_stills <- spinifex::filmstrip(.ggt, nrow = 1)
  
  cp <- cowplot::plot_grid(
    .glob_view + ggtitle("Global view"),
    .cheem_stills + ggtitle("Cheem tour, select frames"),
    labels = paste0(letters[1:2], ")"),
    ncol = 1)#, rel_heights = c(1.5, 1))
}
### Save
ggplot2::ggsave(
  "./figures/case_penguins.pdf",
  plot = cp, device = "pdf",
  width = 8, height = 5, units = "in")
.m <- gc()



## Chocolates classification -----
{
  names(chocolates_ls)
  prim_obs <- 22L
  comp_obs <- 49L
  if(F)
    global_view(chocolates_ls, 
                22, #"Dark Chocolate Bar, Lindt, Switzerland"
                49) #"Pure Dark Chocolate, Hershey's, US"
  
  ### Global view and cheem tour stills
  .glob_view <- global_view(
    chocolates_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    theme_bw() +
    labs(x = "PC1") +
    theme(legend.position = "off",
          axis.text  = element_blank(),
          axis.ticks = element_blank())
  # legend.direction = "vertical", ## Levels within aesthetic
  # legend.box = "horizontal",     ## Between aesthetic
  # legend.margin = ggplot2::margin(1L,1L,1L,1L, "mm")) +
  # labs(color='Predicted class', shape='Predicted class')
  
  .bas <- basis_attr_df(chocolates_ls$attr_df, prim_obs)
  .mv  <- manip_var_of_attr_df(chocolates_ls$attr_df, prim_obs, comp_obs)
  .ggt <- radial_cheem_tour(
    chocolates_ls, basis = .bas, manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 6) + theme(legend.position = "")
  .cheem_stills <- spinifex::filmstrip(.ggt, nrow = 1)
  
  cp <- cowplot::plot_grid(
    .glob_view + ggtitle("Global view"),
    .cheem_stills + ggtitle("Cheem tour, select frames"),
    labels = paste0(letters[1:2], ")"),
    ncol = 1)#, rel_heights = c(1.5, 1))
}

### Save
ggplot2::ggsave(
  "./figures/case_chocolates.pdf",
  plot = cp, device = "pdf",
  width = 8, height = 5, units = "in")
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
    theme_bw() +
    labs(x = "PC1") +
    theme(legend.position = "off",
          axis.text  = element_blank(),
          axis.ticks = element_blank())
  
  .bas <- basis_attr_df(fifa_ls$attr_df, prim_obs)
  .mv  <- manip_var_of_attr_df(fifa_ls$attr_df, prim_obs, comp_obs)
  .ggt <- radial_cheem_tour(
    fifa_ls, basis = .bas, manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    pcp_shape = 124, angle = 15) + theme(legend.position = "off")
  .cheem_stills <- spinifex::filmstrip(.ggt, ncol = 3, nrow =3)
  
  cp <- cowplot::plot_grid(
    .glob_view + ggtitle("Global view"),
    .cheem_stills + ggtitle("Cheem tour, select frames"),
    labels = paste0(letters[1:2], ")"),
    ncol = 1)#, rel_heights = c(1.5, 1))
}

### Save
ggplot2::ggsave(
  "./figures/case_fifa.pdf",
  plot = cp, device = "pdf",
  width = 8, height = 8, units = "in")
.m <- gc()






## Ames Housing 2018 (North Ames) ----
names(ames2018_ls)
prim_obs <- 270L
comp_obs <- 130L
if(F)
  global_view(ames2018_ls, 
              color = ames2018_ls$decode_df$residual,
              shape = factor(ames2018_ls$decode_df$class))

{
  ### global view and tours
  .glob_view <- global_view(
    ames2018_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    theme_bw() +
    labs(x = "PC1") +
    theme(legend.position = "off",
          axis.text  = element_blank(),
          axis.ticks = element_blank())
  
  .bas <- basis_attr_df(ames2018_ls$attr_df, prim_obs)
  .mv  <- manip_var_of_attr_df(ames2018_ls$attr_df, prim_obs, comp_obs)
  .ggt <- radial_cheem_tour(
    ames2018_ls, basis = .bas, manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    pcp_shape = 124, angle = 15) + theme(legend.position = "off")
  .cheem_stills <- spinifex::filmstrip(.ggt, ncol = 3, nrow =4)
  
  cp <- cowplot::plot_grid(
    .glob_view + ggtitle("Global view"),
    .cheem_stills + ggtitle("Cheem tour, select frames"),
    labels = paste0(letters[1:2], ")"),
    ncol = 1)#, rel_heights = c(1.5, 1))
}

### Save
ggplot2::ggsave(
  "./figures/case_ames2018.pdf",
  plot = cp, device = "pdf",
  width = 8, height = 8, units = "in")
.m <- gc()


### rejected -- Tidy Tuesday coffee -----
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

cheem_ls_cof <- cheem_ls(## cheem_ls total: 3.51 sec elapsed
  x = X, y = Y, basis_type = "pca", class = clas)
names(cheem_ls_cof)
prim_obs <- 1L
comp_obs <- 2L

### Global view and cheem tour stills
cheem::global_view(cheem_ls_cof, prim_obs, comp_obs)
glob_view <- static_global_view(
  cheem_ls_peng, prim_obs, comp_obs)

