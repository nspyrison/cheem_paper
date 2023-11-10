# CASE STUDIES FOR CHEEM ----
## Setup ----
{
  require("cheem")
  require("spinifex")
  require("dplyr")
  require("ggplot2")
  require("cowplot")
  require("patchwork")
  
  ## Load data:
  # ## ORIGINAL SUBMISSION:
  # penguins_ls   <- readRDS("data/preprocess_penguins.rds")
  # fifa_ls       <- readRDS("data/preprocess_fifa.rds")
  # ames2018_ls   <- readRDS("data/preprocess_ames2018.rds")
  # chocolates_ls <- readRDS("data/preprocess_chocolates.rds")
  
  ## R&R SUBMISSION:
  penguins_ls   <- cheem::
  fifa_ls       <- readRDS("data/preprocess_fifa.rds")
  ames2018_ls   <- readRDS("data/preprocess_ames2018.rds")
  chocolates_ls <- readRDS("data/preprocess_chocolates.rds")
  
  .t <- theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
              axis.text   = element_blank(),
              axis.title  = element_blank(),
              axis.ticks  = element_blank(),
              legend.position = "off", aspect.ratio = 1)
}

## Penguins classification ------
{
  ## Data setup, spinifex::penguins
  names(penguins_ls)
  prim_obs <- 243
  comp_obs <- 169
  
  ### Global view and cheem tour stills
  .glob_view <- global_view(
    penguins_ls, prim_obs, comp_obs, as_ggplot = TRUE) + .t +
    labs(color = "Predicted class", shape = "Predicted class", x = element_blank()) +
    ggtitle("Global view") + theme(legend.position  = "bottom",
                                   legend.direction = "horizontal")
  .bas <- sug_basis(penguins_ls$attr_df, prim_obs)
  .mv  <- which(colnames(penguins_ls$attr_df) == "fl")
  ## Cheem tour for stills
  mt_interp <- manual_tour(basis = .bas, manip_var = .mv) %>%
    spinifex:::interpolate_manual_tour(angle = .15) ## app is .15
  dim(mt_interp)
  .ggt1 <- radial_cheem_tour(
    penguins_ls, basis = mt_interp[,,1, drop=FALSE], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 0) + .t +
    theme(plot.title = element_text(hjust = 0.22)) +
    ggtitle("Radial tour, select frames")
  .ggt2 <- radial_cheem_tour(
    penguins_ls, basis = mt_interp[,,8, drop=FALSE], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 0) + .t
  ## Using patchwork:
  .pw <- .ggt1 + .ggt2
  .cp <- cowplot::plot_grid(
    .glob_view, .pw, labels = c("a", "b"),
    ncol = 1, align = "v", axis = "l", rel_heights = c(1, 1.1))
}

### Save still shots for paper
ggplot2::ggsave(
  "./figures/case_penguins.png",
  plot = .cp, device = "png",
  width = 6, height = 6, units = "in")
.m <- gc()


## Penguins orthogonal BlFl ----
(
  .g <- ggplot(penguins_na.rm, aes(x = bill_length_mm,
                                   y = flipper_length_mm),
               alpha = logistic_tform(nrow(penguins_na.rm))) +
    geom_point(aes(colour = species), size = 1) +
    ## Prim obs
    geom_point(data=penguins_na.rm[243,], shape=8, size=5, alpha=0.8) +
    ## Comparison obs
    geom_point(data=penguins_na.rm[169,], shape=4, size=3, alpha=0.6) +
    theme_bw() +
    theme(aspect.ratio = 1) +
    
    scale_color_brewer(palette = "Dark2") +
    labs(y = "Flipper length [mm]", x = "Bill length [mm]", color = "Observed species"))
## Save
ggplot2::ggsave(
  "./figures/case_penguins_BlFl.png",
  plot = .g, device = "pdf",
  height = 2.1, units = "in")

### Save .mp4, add GitHub urls to paper
message("NOTE: Manually capturing view from app with Screen to GIF (.mp4)")
## https://github.com/nspyrison/cheem_paper/blob/main/figures/case_penguins.mp4



## Chocolates classification -----
{
  names(chocolates_ls)
  prim_obs <- 22L #"Dark Chocolate Bar, Lindt, Switzerland"
  comp_obs <- 7L  #"70% Cocoa, Columbia
  if(F)
    global_view(chocolates_ls, prim_obs, comp_obs)
  
  ### Global view
  .glob_view <- global_view(
    chocolates_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    labs(color = "Predicted class", shape = "Predicted class", x = element_blank()) +
    ggtitle("Global view") + .t + theme(
      legend.margin    = margin(0,0,0,0),
      legend.position  = "bottom",
      legend.direction = "horizontal")
  .inc_var_nms <- c("Calories", "SatFat", "Chol", "Na", "Fiber", "Sugars")
  ## Removed 4 with lowest contribution for the prim_obs.
  .bas <- sug_basis(chocolates_ls$attr_df[, .inc_var_nms], prim_obs)
  .mv  <- which(.inc_var_nms == "Sugars")
  mt_interp <- manual_tour(.bas, .mv) %>%
    spinifex:::interpolate_manual_tour(.15) ## App angle.
  dim(mt_interp)
  ## Cheem tour stills for paper
  .ggt1 <- radial_cheem_tour(
    chocolates_ls, basis = mt_interp[,,1], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t +
    theme(plot.title = element_text(hjust = 0.18)) +
    ggtitle("Radial tour, select frames")
  .ggt2 <- radial_cheem_tour(
    chocolates_ls, basis = mt_interp[,,20], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t
  .pw <- .ggt1 + .ggt2
  .cp <- cowplot::plot_grid(
    .glob_view, .pw, labels = c("a", "b"),
    ncol = 1, align = "v", axis = "l", 
    rel_heights = c(1, 1.2))
}

### Save Stills
ggplot2::ggsave(
  "./figures/case_chocolates.png",
  plot = .cp, device = "png",
  width = 6, height = 6, units = "in")
.m <- gc()

### Save .mp4, add GitHub urls to paper
message("NOTE: Manually capturing view from app with Screen to GIF (.mp4)")
## https://github.com/nspyrison/cheem_paper/blob/main/figures/case_chocolates.mp4


## Chocolates inverse case -----
{
  names(chocolates_ls)
  prim_obs <- 84L #"Milk Chocolate Square, Ghiradelli, US"
  comp_obs <- 71L #"Classic Milk Chocolate Bar, Nestle, Switzerland"
  if(F)
    global_view(chocolates_ls, prim_obs, comp_obs)
  
  ### Global view
  .glob_view <- global_view(
    chocolates_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    labs(color = "Predicted class", shape = "Predicted class", x = element_blank()) +
    ggtitle("Global view") + .t + theme(
      plot.margin      = margin(0,0,0,0),
      legend.margin    = margin(0,0,0,0),
      legend.position  = "bottom",
      legend.direction = "horizontal")
  .inc_var_nms <- c("CalFat", "TotFat", "SatFat", "Na", "Fiber", "Sugars")
  ## Removed 4 with lowest contribution for the prim_obs.
  .bas <- basis_attr_df(chocolates_ls$attr_df[, .inc_var_nms], prim_obs)
  .mv  <- which(.inc_var_nms == "Na")
  mt_interp <- manual_tour(.bas, .mv) %>%
    spinifex:::interpolate_manual_tour(.15) ## App angle.
  dim(mt_interp)
  ## Cheem tour stills for paper
  .ggt1 <- radial_cheem_tour(
    chocolates_ls, basis = mt_interp[,,1], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t +
    theme(plot.title = element_text(hjust = 0.18)) +
    ggtitle("Radial tour, select frames")
  .ggt2 <- radial_cheem_tour(
    chocolates_ls, basis = mt_interp[,,19], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t
  .pw <- .ggt1 + .ggt2
  .cp <- cowplot::plot_grid(
    .glob_view, .pw, labels = c("a", "b"),
    rel_heights = c(1, 1.2),
    ncol = 1, align = "v", axis = "l")
}

### Save Stills
ggplot2::ggsave(
  "./figures/case_chocolates_inverse.png",
  plot = .cp, device = "png",
  width = 6, height = 6, units = "in")
.m <- gc()

### Save .mp4, add GitHub urls to paper
message("NOTE: Manually capturing view from app with Screen to GIF (.mp4)")
## https://github.com/nspyrison/cheem_paper/blob/main/figures/case_chocolates_inverse.mp4



## ONE OFF FUNC; ----
## regression case basis below scatterplot
THIS_REG_radial_cheem_tour  <- function(
  cheem_ls, basis, manip_var, 
  primary_obs         = NULL,
  comparison_obs      = NULL,
  do_add_pcp_segments = TRUE,
  pcp_shape           = c(142, 124, 3), ## '|' plotly and gganimate, or '+' respectively
  angle               = .15,
  row_index           = NULL,
  inc_var_nms         = NULL,
  do_center_frame     = TRUE,
  do_add_residual     = FALSE
){
  if(is.null(row_index) == FALSE)
    if(sum(row_index) == 0L) 
      stop("radial_cheem_tour: sum of row_index was 0.")
  
  ## Initialize
  x <- y <- NULL
  decode_df <- cheem_ls$decode_df
  .prim_obs <- primary_obs    ## Proto_basis1d_distribution EXPECTS NUMERIC INDEX;
  .comp_obs <- comparison_obs ## Don't coerce to logical index
  .n        <- nrow(decode_df)
  ## column & row indexes
  if(is.null(inc_var_nms)) inc_var_nms <- colnames(cheem_ls$attr_df)
  .col_idx <- colnames(decode_df) %in% inc_var_nms
  if(is.null(row_index) == FALSE){
    ## Change row_index from numeric to logical if needed and replicate
    row_index <- as_logical_index(row_index, .n)
    row_index[c(.prim_obs, .comp_obs)] <- TRUE
  }
  ## Subset columns and scale plot data
  .dat <- decode_df[, .col_idx] %>% spinifex::scale_sd() %>%
    spinifex::scale_01() %>% as.data.frame()
  ## Manual (radial) tour 1d
  .mt_path <- spinifex::manual_tour(basis, manip_var)
  
  ## Problem type & aesthetics
  .prob_type <- cheem_ls$type ## Either "classification" or "regression"
  .alpha <- logistic_tform(.n)
  
  ### Classification case ---
  if(.prob_type == "classification")
    stop("NS: DO NOT USE THIS ONE OFF FUNCTION FOR CLASSIFICATION, 
         it was made to make the regression case basis below the scatterplot")
  
  ### Regression case ---
  ## Doubling data to facet on obs and residual.
  if(.prob_type == "regression"){
    ## Scale obs y, resid, df_hline
    .y        <- decode_df$y %>% spinifex::scale_sd() %>% spinifex::scale_01()
    .resid    <- decode_df$residual %>% spinifex::scale_sd() %>% spinifex::scale_01()
    .df_hline <- data.frame(x = FALSE, y = mean(.resid), facet_var = "residual")
    
    # Aesthetics setup
    .class    <- factor(FALSE) #decode_df$class|predicted_class
    .pts_prim_obs <- .pts_comp_obs <- NULL
    ## Condition handle adding residual facet or not
    if(do_add_residual){
      ## Double up data; observed y and residual
      if(is.null(.prim_obs) == FALSE)
        .pts_prim_obs <- c(.prim_obs, .n + .prim_obs)
      if(is.null(.comp_obs) == FALSE)
        .pts_comp_obs <- c(.comp_obs, .n + .comp_obs)
      if(length(.class) > 1L){.class_fore <- c(.class, .class)
      } else .class_fore <- .class ## could be dummy factor(FALSE)
      ## Foreground:
      .dat_fore   <- rbind(.dat, .dat)
      .idx_fore   <- c(row_index, row_index)
      .facet_fore <- factor(rep(c("observed y", "residual"), each = 2L * .n))
      .fixed_y    <- c(.y, .resid)
    } else {
      ## not doubled up data; just fixed_observed y
      if(is.null(.prim_obs) == FALSE)
        .pts_prim_obs <- .prim_obs
      if(is.null(.comp_obs) == FALSE)
        .pts_comp_obs <- .comp_obs
      ## Foreground:
      .dat_fore   <- .dat
      .idx_fore   <- row_index
      .facet_fore <- rep("observed y", each = .n)
      .class_fore <- .class
      .fixed_y    <- .y
    }
    
    ## ggtour
    ggt <- spinifex::ggtour(.mt_path, .dat_fore, angle = angle,
                            do_center_frame = do_center_frame) +
      ### FACET REMOVED HERE ------
      ## changing to vertical display results in basis on top, try to change map_relative
      #spinifex::facet_wrap_tour(facet_var = .facet_fore, nrow = 1L) +
      spinifex::append_fixed_y(fixed_y = .fixed_y) +
      ## Plotly doesn't rotate text in geom_text/annotate.
      ggplot2::theme(legend.position = "off",
                     axis.title.y = ggplot2::element_text(
                       angle = 90L, vjust = 0.5)) +
      ## Exasperates issues with plotly & geom presence issue.
      #spinifex::proto_frame_cor2(row_index = .idx_fore, position = c(.5, 1.1)) +
      ## Points; 1D proj & fixed y
      spinifex::proto_point(
        aes_args = list(color = .class_fore, shape = .class_fore),
        identity_args = list(alpha = .alpha), row_index = .idx_fore) +
      proto_basis1d_distribution(
        cheem_ls$attr_df, 
        primary_obs = .prim_obs, comparison_obs = .comp_obs,
        position = "bottom1d", group_by = .class, pcp_shape = pcp_shape,
        do_add_pcp_segments = as.logical(do_add_pcp_segments),
        inc_var_nms = inc_var_nms, row_index = row_index) +
      spinifex::proto_basis1d(position = "bottom1d", manip_col = "black") +
      ## Highlight comparison obs
      spinifex::proto_highlight(
        row_index = .pts_comp_obs,
        identity_args = list(size = 3L, shape = 4L, alpha = 0.6, color = "black")) +
      ## Highlight primary obs
      spinifex::proto_highlight(
        row_index = .pts_prim_obs,
        identity_args = list(size = 5L, shape = 8L, alpha = .8, color = "black"))
    if(do_add_residual){
      ggt <- ggt +
        ## Use manual geom_hline as proto_hline0 is on all facets.
        ggplot2::geom_hline(ggplot2::aes(yintercept = y), .df_hline, color = "grey40")
    }
  }
  ## Return the static ggtour, animate in app
  ggt
}



## FIFA 2020 wage regression ------
{
  ## load saved cheem_ls
  names(fifa_ls)
  prim_obs <- 1L
  comp_obs <- 8L
  
  ### global view and tours
  .glob_view <- global_view(
    fifa_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    labs(color = "Predicted class", shape = "Predicted class", x = element_blank()) +
    ggtitle("Global view") + .t + theme(
      legend.margin    = margin(0,0,0,0),
      legend.position  = "off",
      legend.direction = "horizontal")
  .inc_var_nms <- c("BMI", "react", "off", "def", "mvm", "pwr")
  ## Removed 4 with lowest contribution for the prim_obs.
  .bas <- sug_basis(fifa_ls$attr_df[, .inc_var_nms], prim_obs)
  .mv  <- which(.inc_var_nms == "def")
  mt_interp <- manual_tour(.bas, .mv) %>%
    spinifex:::interpolate_manual_tour(.15) ## App angle.
  dim(mt_interp)
  .ggt1 <- THIS_REG_radial_cheem_tour(
    fifa_ls, basis = mt_interp[,,1], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t +
    theme(plot.title = element_text(hjust = 0.18)) +
    ggtitle("Radial tour, select frames")
  .ggt2 <- THIS_REG_radial_cheem_tour(
    fifa_ls, basis = mt_interp[,,9], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t
  .pw <- .ggt1 + .ggt2
  .cp <- cowplot::plot_grid(
    .glob_view, .pw, labels = c("a", "b"),
    rel_heights = c(1, 1.2), ncol = 1)
}
### Save
ggplot2::ggsave(
  "./figures/case_fifa.png",
  plot = .cp, device = "png",
  width = 6, height = 7, units = "in") ## !!CHANGE here
.m <- gc()

### Save .mp4, add GitHub urls to paper
message("NOTE: Manually capturing view from app with Screen to GIF (.mp4)")
## https://github.com/nspyrison/cheem_paper/blob/main/figures/case_fifa.html


## Ames Housing 2018 (North Ames) ----
{
  names(ames2018_ls)
  prim_obs <- 74 ## Large lot area to living area ratio ## small house, big lot
  comp_obs <- 192 ## Small on that ratio ## large house, small lot
  if(F)
    global_view(ames2018_ls, prim_obs, comp_obs,
                color = ames2018_ls$decode_df$residual,
                shape = factor(ames2018_ls$decode_df$class))
  
  .glob_view <- global_view(
    ames2018_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    labs(color = "Predicted class", shape = "Predicted class", x = element_blank()) +
    ggtitle("Global view") + .t + theme(
      plot.margin      = margin(0,0,0,0),
      legend.margin    = margin(0,0,0,0),
      legend.position  = "off",
      legend.direction = "horizontal")
  .inc_var_nms <- c("LtA", "Qlt", "YrB", "LvA", "Rms", "GYB", "GrA")
  ## Removed 4 with lowest contribution for the prim_obs.
  .bas <- sug_basis(ames2018_ls$attr_df[, .inc_var_nms], prim_obs)
  .mv  <- which(.inc_var_nms == "LtA")
  mt_interp <- manual_tour(.bas, .mv) %>%
    spinifex:::interpolate_manual_tour(.15) ## App angle.
  dim(mt_interp)
  .ggt1 <- THIS_REG_radial_cheem_tour(
    ames2018_ls, basis = mt_interp[,,1], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t +
    theme(plot.title = element_text(hjust = 0.18)) +
    ggtitle("Radial tour, select frames")
  .ggt2 <- THIS_REG_radial_cheem_tour(
    ames2018_ls, basis = mt_interp[,,17], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t
  .pw <- .ggt1 + .ggt2
  .cp <- cowplot::plot_grid(
    .glob_view, .pw, labels = c("a", "b"),
    rel_heights = c(1, 1.2), ncol = 1)
}
### Save
ggplot2::ggsave(
  "./figures/case_ames2018.png",
  plot = .cp, device = "png",
  width = 6, height = 7, units = "in") ## !!Change here
.m <- gc()

### Save .mp4, add GitHub urls to paper
message("NOTE: Manually capturing view from app with Screen to GIF (.mp4)")
## https://github.com/nspyrison/cheem_paper/blob/main/figures/case_ames2018.mp4


