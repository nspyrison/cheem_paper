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
      min      = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), min, na.rm = TRUE),
      q1       = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), quantile, 0.25, na.rm = TRUE),
      median   = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), median, na.rm = TRUE),
      q3       = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), quantile, 0.75, na.rm = TRUE),
      max      = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), max, na.rm = TRUE))
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
  my_bd_df <- function(break_down, player_tag = "<tag unused>"){
    df <- data.frame(
      player       = player_tag,
      label        = break_down$label,
      variable     = break_down$variable_name,
      contribution = break_down$contribution, ## SHAP contribution
      cumulative   = break_down$cumulative,   ## Cumulative SHAP contribution
      sign         = break_down$sign
    )
    .n <- nrow(df)
    df$variable[is.na(df$variable)|df$variable==""] <- "prediction"
    df$variable <- factor(
      df$variable, rev(c("intercept", .lvl_ord, "prediction")))
    df$cumulative <- (df$cumulative - min(df$cumulative)) /
      (max(df$cumulative) - min(df$cumulative))
    df$last_cumulative <- c(NA, df$cumulative[-.n])
    df$variable_num <- 1:.n
    df$next_variable_num <- c(2:.n, NA)
    rownames(df) <- paste(break_down$label, break_down$variable, break_down$B, sep = ": ")
    return(df)
  }
}

### Create FIFA x ------
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

## SHAP values & plot ----
## Messi SHAP
messi <- X[1, ]
shap_messi <- predict_parts(explainer       = rf_expl,
                            new_observation = messi,
                            type            = "shap",
                            B               = 25L) %>% as.data.frame()
## Originally divide all 
var_norm_messi <- shap_messi %>% 
  group_by(variable) %>% 
  summarize(median = median(contribution)) %>%
  pull(median) %>%
  matrix(ncol = 1) %>%
  norm()
shap_messi$contribution2_var_norm <- shap_messi$contribution / var_norm_messi
shap_messi <- data.frame(shap_messi, var_norm = var_norm_messi)
B_norms_messi <- shap_messi %>% 
  group_by(B) %>% 
  summarize(B_norm = norm(matrix(contribution))) %>%
  as.data.frame()
lj <- left_join(shap_messi, B_norms_messi, by = "B") %>%
  mutate(contribution3_B_norm = contribution / B_norm) %>% 
  select(variable_name, B, variable, var_norm, B_norm, contribution, contribution2_var_norm, contribution3_B_norm)

lj %>% group_by(B, var_norm, B_norm) %>% 
  summarise(
    sum_c           = sum(contribution), 
    sum_c2_var_norm = sum(contribution2_var_norm), 
    sum_c3_B_norm   = sum(contribution3_B_norm)
  )
Y[1]; predict(rf_mod)[1] #Obs and pred

