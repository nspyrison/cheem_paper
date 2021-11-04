# CASE STUDIES FOR CHEEM ----
require("cheem")
require("spinifex")
require("dplyr")
require("ggplot2")

## Local function variants:

static_global_view <- function(
  cheem_ls,
  primary_obs = NULL,
  comparison_obs = NULL
){
  ## Prevent global variable warnings:
  V1 <- V2 <- ggtext <- projection_nm <- layer_name <- tooltip <- NULL
  .alpha <- logistic_tform(nrow(cheem_ls$decode_df), mid_pt = 500L)
  global_view_df <- cheem_ls$global_view_df ## Init
  is_classification <- cheem_ls$problem_type == "classification"
  pred_clas <- as.factor(FALSE) ## If regression; dummy pred_clas
  if(is_classification == TRUE) pred_clas <-
    cheem_ls$decode_df$predicted_class %>%
    rep_len(nrow(global_view_df)) %>%
    as.factor()
  
  pts_highlight <- list()
  ## Red misclassified points, if present
  if(is_classification == TRUE){
    .rn_misclass <- which(cheem_ls$decode_df$is_misclassified == TRUE)
    .idx_misclas <- global_view_df$rownum %in% .rn_misclass
    if(sum(.idx_misclas) > 0L){
      .df <- global_view_df[.idx_misclas, ] # %>% highlight_key(~rownum)
      pts_highlight <- c(
        pts_highlight,
        ggplot2::geom_point(ggplot2::aes(V1, V2), .df,
                            color = "red", fill = NA,
                            shape = 21L, size = 3L, alpha = .alpha)
      )
    }
  }
  ## Highlight comparison obs, if passed
  if(is.null(comparison_obs) == FALSE){
    .idx_comp <- global_view_df$rownum == comparison_obs
    if(sum(.idx_comp) > 0L){
      .df <- global_view_df[.idx_comp, ]
      pts_highlight <- c(
        pts_highlight,
        ## Highlight comparison obs
        ggplot2::geom_point(ggplot2::aes(V1, V2), #, color = pred_clas[.idx_comp]),
                            .df, size = 3L, shape = 4L, color = "black")
      )
    }
  }
  ## Highlight shap obs, if passed
  if(is.null(primary_obs) == FALSE){
    .idx_shap <- global_view_df$rownum == primary_obs
    if(sum(.idx_shap) > 0L){
      .df <- global_view_df[.idx_shap, ] # %>% highlight_key(~rownum)
      pts_highlight <- c(
        pts_highlight,
        ggplot2::geom_point(ggplot2::aes(V1, V2),#, color = pred_clas[.idx_shap]),
                            .df, size = 5L, shape = 8L, color = "black")
      )
    }
  }
  
  .bas_data <- cbind(
    as.data.frame(cheem_ls$basis_ls$data_basis), layer_name = "data")
  .map_to_data <- global_view_df[global_view_df$layer_name == "data", c("V1", "V2")]
  .map_to_data[,1L] <-  .map_to_data[,1L] / 3L
  .bas_attr <- cbind(
    as.data.frame(cheem_ls$basis_ls$attribution_basis), layer_name = "SHAP")
  .map_to_attr <- global_view_df[global_view_df$layer_name == "SHAP", c("V1", "V2")]
  .map_to_attr[,1L] <- .map_to_attr[,1L] / 3L
  ## ggplot
  gg <- ggplot2::ggplot(
    data = plotly::highlight_key(global_view_df, ~rownum),
    mapping = ggplot2::aes(V1, V2)) +
    suppressWarnings(ggplot2::geom_point(
      ggplot2::aes(V1, V2, color = pred_clas, shape = pred_clas,
                   tooltip = tooltip), alpha = .alpha)) +
    pts_highlight +
    spinifex::draw_basis(.bas_data, .map_to_data, "bottomleft") +
    spinifex::draw_basis(.bas_attr, .map_to_attr, "bottomleft") +
    ggplot2::facet_grid(rows = ggplot2::vars(projection_nm),
                        cols = ggplot2::vars(layer_name)) +#, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "PC1", y = "PC2") +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme(axis.text  = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   legend.position = "off")
  return(gg)
}


## Penguins species classification ------
## Data setup, spinifex::penguins
{
  raw <- spinifex::penguins
  clas <- raw$species
  lvls <- levels(clas)
  # ## Filter to closest 2 classes
  # raw <- raw[raw$species %in% lvls[1:2], ]
  # clas <- factor(raw$species, levels = lvls[1:2]) ## Manually remove 3rd lvl
  X <- raw[, 1:4] %>% as.data.frame() ## X's not scaled.
  colnames(X) <- c("b_l", "b_d", "f_l", "wgt")
  Y <- as.integer(raw$species)
}

### cheem_ls -----
cheem_ls_peng <- cheem_ls( ## cheem_ls total: 3.51 sec elapsed
  x = X, y = Y, basis_type = "pca", class = clas)
names(cheem_ls_peng)
prim_obs <- 15L
comp_obs <- 282L

### Still shots of global and tours
glob_view <- static_global_view(
  cheem_ls_peng, prim_obs, comp_obs)

bas <- basis_local_attribution(
  cheem_ls_peng$attr_df, rownum = prim_obs)
ggt <- radial_cheem_ggtour(
  cheem_ls_peng, basis=bas, mv_name=colnames(X)[1], 
  primary_obs=prim_obs, comparison_obs=comp_obs)

### ch5_fig2_global_space -----


### Save -----
ggplot2::ggsave(
  "./figures_from_script/ch5_fig2_global_space.pdf",
  plot = p + theme(aspect.ratio=1), device = "pdf",
  width = 8, height = 4.8, units = "in")

## Save interactive html widget
ggp <- ggplotly(p, tooltip = "tooltip") %>%
  config(displayModeBar = FALSE) %>% ## Remove html buttons
  layout(dragmode = "select", showlegend = FALSE,
         width = 640, height = 320) %>% ## Set drag left mouse
  event_register("plotly_selected") %>% ## Reflect "selected", on release of the mouse button.
  highlight(on = "plotly_selected", off = "plotly_deselect")
htmlwidgets::saveWidget(ggp, "./figures_from_script/ch5_fig2_global_space.html",
                        selfcontained = TRUE)

## FIFA 2020 wage regression ------

## load saved cheem_ls 
cheem_ls_fifa <- readRDS(
  "../cheem/inst/shiny_apps/cheem_initial/data/3preprocess_fifa.rds")
names(cheem_ls_fifa)
cheem_ls_fifa$runtime_df
prim_obs <- 1L
comp_obs <- 8L

### Still shots of global and tours -----
glob_view <- static_global_view(
  cheem_ls_fifa, prim_obs, comp_obs)


### ch5_fig2_global_space -----


### Save -----