# ch5_fig1_shap_distr_bd ----
{
  require("DALEX")
  require("dplyr")
  require("ggplot2")
  set.seed(2022)
  
  ## Local func
  my_parts_boxplot_df <- function(pred_parts, player_tag = "<tag unused>"){
    ## Remade from: iBreakDown:::print.break_down_uncertainty
    data.frame(
      player   = player_tag,
      label    = tapply(pred_parts$label, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE),
      variable = tapply(pred_parts$variable_name, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE),
      value    = tapply(pred_parts$variable_value, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE), ## oos variable value
      ## Of the distribution of local attributions:
      # min      = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), min, na.rm = TRUE),
      # q1       = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), quantile, 0.25, na.rm = TRUE),
      mean     = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), mean, na.rm = TRUE),
      median   = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), median, na.rm = TRUE)#,
      # q3       = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), quantile, 0.75, na.rm = TRUE),
      # max      = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), max, na.rm = TRUE)
    )
  }
  my_parts_distribution <- function(pred_parts, player_tag = "<tag unused>"){
    df <- data.frame(
      player       = player_tag,
      label        = pred_parts$label,
      variable     = pred_parts$variable_name,
      value        = pred_parts$variable_value, ## Obs value of X
      contribution = pred_parts$contribution,   ## SHAP contribution
      B_perm_num   = pred_parts$B
    )
    rownames(df) <- paste(pred_parts$label, pred_parts$variable, pred_parts$B, sep = ": ")
    return(df)
  }
  .lvl_ord <- c("reaction", "offense", "movement", "defense", "power", "accuracy", "BMI", "age", "goalkeeping")
  my_bd_df <- function(break_down, player_tag = "<tag unused>", seq_name = "<seq_name unused>", seq_ord = .lvl_ord){
    df <- data.frame(
      player       = player_tag,
      seq_name     = seq_name,
      label        = break_down$label,
      variable     = break_down$variable_name,
      contribution = break_down$contribution, ## SHAP contribution
      cumulative   = break_down$cumulative,   ## Cumulative SHAP contribution
      sign         = break_down$sign
    )
    .n <- nrow(df)
    df$variable[is.na(df$variable)|df$variable==""] <- "prediction"
    df$variable <- factor(
      df$variable, rev(c("intercept", seq_ord, "prediction")))
    df$cumulative <- (df$cumulative - min(df$cumulative)) /
      (max(df$cumulative) - min(df$cumulative))
    df$last_cumulative <- c(NA, df$cumulative[-.n])
    df$variable_num <- 1:.n
    df$next_variable_num <- c(2:.n, NA)
    rownames(df) <- paste(break_down$label, break_down$variable, break_down$B, sep = ": ")
    df <- df[!(df$variable %in% c("intercept", "prediction")), ]
    return(df)
  }
  my_bd_plot <- function(df){
    ggplot() + 
      ## horizontal bars
      geom_segment(
        aes(x = cumulative, xend = last_cumulative,
            y = variable, yend = variable, color = player),
        data = df, size = 1.5, alpha = .8) +
      facet_grid(rows = vars(seq_name), col = vars(player)) +
      theme_bw() +
      scale_color_brewer(palette = "Dark2") +
      labs(title = "Breakdown plot", color = "Players",
           y = "", x = "Normalized contribution to prediction") +
      theme(legend.margin   = margin(0, 0, 0, 0),
            legend.position = "off",
            axis.text.x     = element_blank(),
            axis.ticks.x    = element_blank())
  }
}

### Create FIFA x ------
{
  .raw <- DALEX::fifa
  .dat_less_ys <- .raw %>%
    dplyr::select(-c(`nationality`, ## useless class
                     `overall`, `potential`, `value_eur`, `wage_eur`)) %>% ## potential target vars.
    as.data.frame()
  
  if(F) ## View corrplot?
    corrplot::corrplot(cor(.dat_less_ys),
                       method = "circle", ## geom
                       type = "upper", ## only upper triangle
                       diag = F, ## remove auto correlation
                       order = "FPC", ## First principal component
                       tl.col = "black", tl.srt = 90, ## Text label color and rotation
                       tl.pos = "td")
  
  ## Munging aspects
  #### Agg some highly correlated vars.
  dat <- .dat_less_ys %>%
    dplyr::mutate(
      .keep    = "none",
      BMI      = (weight_kg+(height_cm/100L)^2L),
      age      = age,
      reaction = movement_reactions,
      offense  = (attacking_finishing+skill_long_passing+attacking_volleys+
                    power_long_shots+skill_curve+mentality_positioning+attacking_crossing+
                    attacking_short_passing+skill_dribbling+skill_ball_control)/10L,
      defense  = (defending_sliding_tackle+mentality_interceptions+
                    defending_standing_tackle+defending_marking+mentality_aggression)/5L,
      accuracy = (attacking_heading_accuracy+power_shot_power)/2L,
      movement = (movement_sprint_speed+movement_balance+movement_acceleration+
                    mentality_vision+mentality_composure+movement_agility+
                    mentality_penalties+skill_fk_accuracy+power_stamina+movement_reactions)/10L,
      power    = (power_strength+power_jumping)/2L,
      goalkeeping = (goalkeeping_diving+goalkeeping_positioning+goalkeeping_reflexes+
                       goalkeeping_handling+goalkeeping_kicking)/5L
    )
  ## Class for the position of the player, either "fielder" or "goalkeeper"
  position <- clas <- dplyr::case_when(
    dat$gk <= 40L ~ "fielder",
    dat$gk >  40L ~ "goalkeeper") %>%
    factor(levels = c("fielder", "goalkeeper"))
  
  ## Starting with 42 variables, we remove `nationality`, and some potential Y vars,
  #### and aggregate into 9 aggregate 'aspect' dimensions based on var correlation
  X <- dat ## 9 aspects of the X's
  Y <- .raw$wage_eur ## unscaled wages in Euros, assumed 2020 valuation.
  
  ## Create same RF used downstream -----
  .is_y_disc <- FALSE ## regressing on continuous wages
  .hp_ntrees <- sqrt(nrow(X))
  .hp_mtry <- if(.is_y_disc == TRUE) sqrt(ncol(X)) else ncol(X) / 3L
  .hp_node <- if(.is_y_disc == TRUE) 1L else 5L
  .hp_node <- max(.hp_node, nrow(X) / 500L)
  
  
  ## NOTE: this DALEX::predict_parts("SHAP")
  ## NOT theeshap::treeshap()
  rf_mod <- randomForest::randomForest(
    Y~., data = data.frame(Y, X),
    mtry = .hp_mtry, nodesize = .hp_node, ntrees = .hp_ntrees)
  ## DALEX parts
  rf_expl <- DALEX::explain(model = rf_mod,
                            data  = X,
                            y     = Y,
                            label = "Random Forest")
}

## SHAP values & plot -----
## Messi SHAP
messi <- X[1, ]
shap_messi <- predict_parts(explainer       = rf_expl,
                            new_observation = messi,
                            type            = "shap",
                            B               = 25L) %>%
  as.data.frame()
B_norms_messi <- shap_messi %>% 
  group_by(B) %>%
  summarize(B_norm = norm(matrix(contribution))) %>%
  as.data.frame()
shap_messi <- left_join(shap_messi, B_norms_messi, by = "B") %>%
  mutate(contribution = contribution / B_norm) ## Normalize by the norm within B of each player.
box_df_messi <- my_parts_boxplot_df(shap_messi, "Messi (offense)")

## Virgil van Dijk SHAP
dijk      <- X[8, ]
shap_dijk <- predict_parts(explainer       = rf_expl,
                           new_observation = dijk,
                           type            = "shap",
                           B               = 25L,
                           order = .lvl_ord) %>%
  as.data.frame()
B_norms_dijk <- shap_dijk %>%
  group_by(B) %>%
  summarize(B_norm = norm(matrix(contribution))) %>%
  as.data.frame()
shap_dijk <- left_join(shap_dijk, B_norms_dijk, by = "B") %>%
  mutate(contribution = contribution / B_norm) ## Normalize by the norm within B of each player.
box_df_dijk <- my_parts_boxplot_df(shap_dijk, "van Dijk (defense)")

## Bind shap aggs:
boxplot_df          <- rbind(box_df_messi, box_df_dijk)
boxplot_df$variable <- factor(boxplot_df$variable, levels = rev(.lvl_ord))

## B Distributions of the SHAPS
dist_shap_messi  <- my_parts_distribution(shap_messi, "Messi (offense)")
dist_shap_dijk   <- my_parts_distribution(shap_dijk, "van Dijk (defense)")
dist_df          <- rbind(dist_shap_messi, dist_shap_dijk)
dist_df$variable <- factor(dist_df$variable, levels = rev(.lvl_ord))

(g_shap <- ggplot(boxplot_df) +
    # Connecting grey line
    geom_segment(aes(x=`Messi (offense)`, xend=`van Dijk (defense)`, y=variable, yend=variable),
                 alpha =.7, size = 2L, color = "grey", fill = NA,
                 data = boxplot_df %>% select(player, variable, mean) %>%
                   tidyr::pivot_wider(names_from = "player", values_from = "mean") %>%
                   mutate(sum = `Messi (offense)` + `van Dijk (defense)`)) +
    geom_point(aes(x=mean, y=variable, color=player, fill=player),
               alpha =.7, size = 5L) +
    ## Shap distributions
    geom_point(aes(x=contribution, y=variable, color=player, fill=player),
               dist_df, alpha =.8, size = 3, shape = 124,
               position = position_dodge(-.5)) +
    theme_bw() +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    labs(title = "SHAP",
         y = "", x = "Normalized SHAP values") +
    theme(legend.margin   = margin(0, 0, 0, 0),
          legend.position = "bottom"))

## Breakdowns & plot ----
## add alternate sequences -----
shap_byplayer <- boxplot_df %>% select(player, variable, mean) %>%
  tidyr::pivot_wider(names_from = "player", values_from = "mean")
.seq_messi <-
  shap_byplayer$variable[order(abs(shap_byplayer$`Messi (offense)`), decreasing = TRUE)] %>%
  as.character()
.seq_dijk  <-
  shap_byplayer$variable[order(abs(shap_byplayer$`van Dijk (defense)`), decreasing = TRUE)] %>%
  as.character()

## seq_1, desc messi shap
bd <- predict_parts(explainer       = rf_expl,
                    new_observation = messi,
                    type            = "break_down",
                    order           = .seq_messi)
bd_df_messi1 <- my_bd_df(bd, "Messi (offense)", "sequence 1", .seq_messi)
bd <- predict_parts(explainer       = rf_expl,
                    new_observation = dijk,
                    type            = "break_down",
                    order           = .seq_messi)
bd_df_dijk1  <- my_bd_df(bd, "van Dijk (defense)", "sequence 1", .seq_messi)
## seq_2, desc dijk shap
bd <- predict_parts(explainer       = rf_expl,
                    new_observation = messi,
                    type            = "break_down",
                    order           = .seq_dijk)
bd_df_messi2 <- my_bd_df(bd, "Messi (offense)", "sequence 2", .seq_dijk)
bd <- predict_parts(explainer       = rf_expl,
                    new_observation = dijk,
                    type            = "break_down",
                    order           = .seq_dijk)
bd_df_dijk2  <- my_bd_df(bd, "van Dijk (defense)", "sequence 2", .seq_dijk)
## seq_3, desc sum shap
bd <- predict_parts(explainer       = rf_expl,
                    new_observation = messi,
                    type            = "break_down",
                    order           = .lvl_ord)
bd_df_messi3 <- my_bd_df(bd, "Messi (offense)", "sequence 3", .lvl_ord)
bd <- predict_parts(explainer       = rf_expl,
                    new_observation = dijk,
                    type            = "break_down",
                    order           = .lvl_ord)
bd_df_dijk3 <- my_bd_df(bd, "van Dijk (defense)", "sequence 3", .lvl_ord)

bd_seq1 <- my_bd_plot(rbind(bd_df_messi1, bd_df_dijk1)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  labs(x = element_blank(), title = "Breakdown plots")
bd_seq2 <- my_bd_plot(rbind(bd_df_messi2, bd_df_dijk2)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        strip.text.x = element_blank()) +
  labs(title = element_blank(), x = element_blank())
bd_seq3 <- my_bd_plot(rbind(bd_df_messi3, bd_df_dijk3)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        strip.text.x = element_blank()) +
  labs(title = element_blank())
(g_bd <- cowplot::plot_grid(bd_seq1, bd_seq2, bd_seq3, ncol = 1, rel_heights = c(1.25, 1, 1.08)))

### Plot together
require("cowplot")
(cp <- cowplot::plot_grid(
  g_bd, g_shap, ncol = 1, rel_heights = c(3, 2), labels = c("a)", "b)")))

## SAVE -----
ggplot2::ggsave(
  "./figures/shap_distr_bd.png",
  cp, device = "png", width = 6.1, height = 8, units = "in")
