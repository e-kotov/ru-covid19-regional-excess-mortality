options(tidyverse.quiet = TRUE)
options(rgl.useNULL = TRUE)
options(scipen = 999)
if( !"here" %in% installed.packages()[,'Package'] )  { install.packages("here") }
source(here::here("R", "package_list.R"))
source(here::here("R", "install_packages.R"))



# Correlation tables and plots --------------------------------------------


correlation_table <- function(data_sf) {
  
  cor_table <- data.table(`Variable Name` = all_x_vars,
                          `Correlation` = all_x_vars %>%
                            map_dbl(~ cor(x = data_sf %>%
                                            dplyr::pull(Excess_mortality_apr_feb_per_capita),
                                          y = data_sf %>% dplyr::pull(.x)) %>% 
                                      round(., digits = 4)
                            ),
                          `LM R2` = all_x_vars %>% 
                            map_dbl(~ lm(formula = as.formula( paste0("Excess_mortality_apr_feb_per_capita ~ ", .x) ),
                                         data = data_sf) %>% 
                                      broom::glance() %>% 
                                      dplyr::pull(r.squared) %>%
                                      round(., digits = 4)
                            ),
                          `LM p-value` = all_x_vars %>% 
                            map_dbl(~ lm(formula = as.formula( paste0("Excess_mortality_apr_feb_per_capita ~ ", .x) ),
                                         data = data_sf) %>% 
                                      broom::glance() %>% 
                                      dplyr::pull(p.value) %>%
                                      round(., digits = 10)
                            )
  )
  
  cor_table <- cor_table[order(-`LM R2`)]
  
  return(cor_table)
}

yx_vars_correlation_matrix <- function(y_var, x_vars, data_sf) {
  
  data_dt <- copy(data_sf %>% st_drop_geometry() %>% setDT() )
  
  setnames(data_dt, old = names(data_dt), new = gsub("_", " ", names(data_dt)))
  y_var <- gsub("_", " ", y_var)
  x_vars <- gsub("_", " ", x_vars)
  
  pairs_plot <- data_dt %>%
    dplyr::select(all_of( c(y_var, x_vars) ) ) %>% 
    ggpairs(., 
            upper = list(continuous = colour_cor),
            lower = list(continuous = wrap(scatter_smooth, method = "lm"),
                         combo = wrap("dot", alpha = 0.1, size = 0.4)),
            labeller = label_wrap_gen(10)
    )
  
  return(pairs_plot)
}



# Helper Functions and Variables ------------------------------------------





# crs definition for the spatial data set
crs_albers_siberia <- "+proj=aea +lat_1=52 +lat_2=64 +lat_0=0 +lon_0=105 +x_0=18500000 +y_0=0 +ellps=krass +units=m +towgs84=28,-130,-95,0,0,0,0 +no_defs"

colour_cor <- function(data, mapping, method="p", use="pairwise", ...){
  
  # the function code is from https://stackoverflow.com/questions/45873483/ggpairs-plot-with-heatmap-of-correlation-values
  
  # grab data
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  # calculate correlation
  corr <- cor(x, y, method=method, use=use)
  
  # calculate colour based on correlation value
  # Here I have set a correlation of minus one to blue, 
  # zero to white, and one to red 
  # Change this to suit: possibly extend to add as an argument of `my_fn`
  colFn <- colorRampPalette(c("blue", "white", "red"), interpolate ='spline')
  fill <- colFn(100)[findInterval(corr, seq(-1, 1, length=100))]
  
  ggally_cor(data = data, mapping = mapping, ...) + 
    theme_void() +
    theme(panel.background = element_rect(fill=fill))
}

scatter_smooth <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(alpha = 0.6, size = 0.3) + 
    geom_smooth(method=method, ...) +
    theme_pubr()
  p
}



var_test_spatial <- function(dataset,
                             target_var_name,
                             weights,
                             ln_transform = F,
                             local_moran_signif = 0.05,
                             norm_plots = F) {
  
  
  ## check if log_transform option is set to TRUE
  if ( ln_transform == T ) { ## if TRUE
    dataset[,target_var_name] <- log(dataset %>% pull(target_var_name) +1)
    
    dataset <- dataset %>%
      mutate(target_var_scaled = as.vector(scale( !!rlang::sym(target_var_name) )) )
  }
  
  if ( ln_transform == F ) { ## if FALSE
    dataset <- dataset %>%
      mutate(target_var_scaled = as.vector(scale( !!rlang::sym(target_var_name )) ) )
  }
  
  
  
  dataset <- dataset %>%
    mutate(target_var_scaled_lag = lag.listw(x = weights, var = target_var_scaled))
  
  
  moran_permutation_test <- moran.mc(dataset %>% pull(target_var_scaled),
                                     listw = weights, nsim = 999)
  
  
  
  # moran_permutation_test$statistic
  # moran_permutation_test$p.value
  
  gg_moran <- dataset %>%
    ggplot() +
    aes(x = target_var_scaled, y = target_var_scaled_lag) +
    geom_point(shape = 1, alpha = 0.5) +
    geom_hline(yintercept = mean(dataset %>% pull(target_var_scaled)), lty=2) +
    geom_vline(xintercept = mean(dataset %>% pull(target_var_scaled_lag)), lty=2) +
    geom_smooth(method = "lm") +
    labs(title = paste0("Variable: ", target_var_name),
         subtitle = paste0(" Moran I: ", round(moran_permutation_test$statistic, 4),
                           " p: ", round(moran_permutation_test$p.value, 4))) +
    theme_pubclean(base_size = 10) +
    coord_cartesian(clip = "off") +
    if (ln_transform == T) {
      labs(x = paste0(target_var_name, "\n (scaled and centered)"),
                                   y = paste0("lag value based on\n the neighbourhood"))
    } else {
      labs(x = target_var_name,
           y = paste0("lag value based on\n the neighbourhood"))
    }
    
  # gg_moran
  
  local_moran <- localmoran(x = dataset %>% pull(target_var_scaled), listw = weights)
  
  dataset <- dataset %>%
    mutate(target_var_signif = local_moran[,5]) # get the 5th column of the matrix - the `Pr(z > 0)` column
  
  
  dataset <- dataset %>% mutate(target_var_cluster = case_when(target_var_signif > local_moran_signif ~ "non-significant",
                     target_var_scaled > 0 & target_var_scaled_lag > 0 & target_var_signif <= local_moran_signif ~ "high-high",
                     target_var_scaled < 0 & target_var_scaled_lag < 0 & target_var_signif <= local_moran_signif ~ "low-low",
                     target_var_scaled > 0 & target_var_scaled_lag < 0 & target_var_signif <= local_moran_signif ~ "high-low",
                     target_var_scaled < 0 & target_var_scaled_lag > 0 & target_var_signif <= local_moran_signif ~ "low-high"))
  
  
  local_moran_map <- dataset %>%
    ggplot() +
    aes(fill = target_var_cluster) +
    geom_sf(lwd = 0.2) +
    scale_fill_manual(values = c("low-low" = "blue", "high-high" = "red", "low-high" = "dodgerblue1", "high-low" = "salmon1", "non-significant" = "grey90")) +
    labs(fill = "Cluster") +
    theme_pubclean() +
    theme(legend.position = "right")
  local_moran_map
  
  
  
  ## set axis limits vairable to apply to all plots
  axis_lim <- range(dataset %>% pull(target_var_name))
  
  ## create historgram
  x_hist <- dataset %>%
    ggplot() + ## create ggplot object
    aes_string(x = target_var_name) + ## set aesthetics to x, our numeric vector
    geom_histogram() + ## plot histogram
    xlim(axis_lim) + ## set limits of the x axis to the predefined variable value
    theme_pubclean() ## apply theme to the plot
  
  
  ## create qqnorm
  x_qqnorm <- dataset %>%
    ggplot() +
    stat_qq(aes_string(sample = target_var_name)) +
    geom_abline(intercept = mean(dataset %>% pull(target_var_name), na.rm = T),
                slope = sd(dataset %>% pull(target_var_name), na.rm = T)) +
    ylim(axis_lim) +
    labs(title = paste0 ("Distribution of ",
                         target_var_name,
                         ifelse(ln_transform == T, "\nLog-transformed", "\nNot log-transformed"))) +
    theme_pubclean()
  
  if(norm_plots == F){
    final_plot <- (gg_moran + local_moran_map)
  } else {
    final_plot <- (gg_moran + local_moran_map) / (x_hist + x_qqnorm)
  }
  
  return(final_plot)
}


# Build lm and spatial models --------------------------------------------

build_models <- function(data_sf, W, formula_string, standardise = FALSE) {

  model_formula <- as.formula(formula_string)
  y_var <- str_split(formula_string, pattern = " ~ ")[[1]][[1]]
  # data_sf <- regions_sf_transformed
  
  
  if( standardise == TRUE) {
    data_sf <- standardise_sf(data_sf = data_sf, y_var = y_var)
  }
  
  
  
  
  
  set.seed(314L)
  mod_lm <- lm(data = data_sf %>% st_drop_geometry(), formula = model_formula)
  
  set.seed(314L)
  mod_slx <- lmSLX(formula = model_formula, data = data_sf, listw = W)
 
  set.seed(314L)
  mod_lag <- lagsarlm(model_formula, data = data_sf, listw = W)
 
  set.seed(314L)
  mod_sem <- errorsarlm(model_formula, data = data_sf, listw = W)
 
  # SDEM Spatial Durbin Error Model
  set.seed(314L)
  mod_sdem <- errorsarlm(model_formula, data=data_sf, listw = W, etype = "emixed")
  
  # SDM Spatial Durbin Model
  set.seed(314L)
  mod_sdm <- lagsarlm(model_formula, data=data_sf, listw = W, type="mixed")
  
  # Spatial Autoregressive Combined Mixed Model
  set.seed(314L)
  mod_gns <- sacsarlm(model_formula, data=data_sf, listw = W, type="sacmixed")
  
  # SARAR
  set.seed(314L)
  mod_sarar <- sacsarlm(model_formula, data=data_sf, listw = W, type="sac") 
  
  model_set <- list(mod_lm = mod_lm,
                    mod_slx = mod_slx,
                    mod_lag = mod_lag,
                    mod_sem = mod_sem,
                    mod_sdem = mod_sdem,
                    mod_sdm = mod_sdm,
                    mod_gns = mod_gns,
                    mod_sarar = mod_sarar)
  
  return(model_set)
}


adj_r2 <- function(model_obj, truth_vec) {
  r2 <- suppressMessages( rsq_trad_vec(truth = truth_vec, estimate = fitted(model_obj)) )
  n_obs <- length(truth_vec)
  n_params <- length(model_obj$coefficients)
  return (1 - ( (1 - r2 ) * ( n_obs - 1) / (n_obs - n_params - 1) ) )
}



build_model_summaries <- function(model_set, data_sf) {
  
  data_dt <- copy(data_sf %>% st_drop_geometry() %>% setDT() )
  
  mod_dt <- data.table(model = c("OLS", "SLX", "LAG", "SEM", "SDEM", "SDM", "GNS", "SARAR"),
                       lapply(model_set, glance) %>%
                         rbindlist(fill = T)
  )
  
  
  mod_dt[ model == "OLS", rmse := yardstick::rmse_vec(data_dt$Excess_mortality_apr_feb_per_capita,
                                                     estimate = model_set$mod_lm$fitted.values), ]
  mod_dt[ model == "SLX", rmse := yardstick::rmse_vec(data_dt$Excess_mortality_apr_feb_per_capita,
                                                      estimate = model_set$mod_slx$fitted.values), ]
  mod_dt[ model == "LAG", rmse := yardstick::rmse_vec(data_dt$Excess_mortality_apr_feb_per_capita,
                                                      estimate = model_set$mod_lag$fitted.values), ]
  mod_dt[ model == "SEM", rmse := yardstick::rmse_vec(data_dt$Excess_mortality_apr_feb_per_capita,
                                                      estimate = model_set$mod_sem$fitted.values), ]
  mod_dt[ model == "SDEM", rmse := yardstick::rmse_vec(data_dt$Excess_mortality_apr_feb_per_capita,
                                                       estimate = model_set$mod_sdem$fitted.values), ]
  mod_dt[ model == "SDM", rmse := yardstick::rmse_vec(data_dt$Excess_mortality_apr_feb_per_capita,
                                                      estimate = model_set$mod_sdm$fitted.values), ]
  mod_dt[ model == "GNS", rmse := yardstick::rmse_vec(data_dt$Excess_mortality_apr_feb_per_capita, 
                                                         estimate = model_set$mod_gns$fitted.values), ]
  mod_dt[ model == "SARAR", rmse := yardstick::rmse_vec(data_dt$Excess_mortality_apr_feb_per_capita,
                                                        estimate = model_set$mod_sarar$fitted.values), ]
  
  mdt <- mod_dt[ , .(model, R2 = round(r.squared, 2), AIC = round(AIC, 0), RMSE = round(rmse, 2)), ]
  
  mdt[ , AdjR2 := model_set %>% map_dbl( ~ adj_r2(.x, truth_vec = data_dt$Excess_mortality_apr_feb_per_capita) ) %>% round(2), ]
  
  mdt[ , AICc := model_set %>% map_dbl(~ qpcR::AICc(.x)) %>% round(2), ]
  
  mdt <- cbind(mdt, as.data.table(akaike.weights(mdt$AICc)) )
  
  mdt <- mdt[ , .(Model = model, R2, AdjR2, RMSE, AICc, delta_AICc = round(deltaAIC, 3), AICc_weight = round(weights, 3)), ]
  
  return(mdt)
}


build_OLS_summaries <- function(model_set, data_sf) {
  
  data_dt <- copy(data_sf %>% st_drop_geometry() %>% setDT() )
  
  
  
  mod_dt <- data.table(model = names(model_set),
                       lapply(model_set, glance) %>%
                         rbindlist(fill = T)
  )
  
  mod_dt[ , rmse := model_set %>% map_dbl(~ yardstick::rmse_vec(truth = data_dt$Excess_mortality_apr_feb_per_capita,
                                                                estimate = fitted.values(.x))), ]
  
  mdt <- mod_dt[ , .(model, R2 = round(r.squared, 2), AIC = round(AIC, 0), RMSE = round(rmse, 2)), ]
  
  mdt[ , AdjR2 := model_set %>% map_dbl( ~ adj_r2(.x, truth_vec = data_dt$Excess_mortality_apr_feb_per_capita) ) %>% round(2), ]
  
  mdt[ , AICc := model_set %>% map_dbl(~ qpcR::AICc(.x)) %>% round(2), ]
  
  mdt <- cbind(mdt, as.data.table(akaike.weights(mdt$AICc)) )
  
  mdt <- mdt[ , .(Model = model, R2, AdjR2, RMSE, AICc, delta_AICc = round(deltaAIC, 3), AICc_weight = round(weights, 3)), ]
  
  return(mdt)
}




plot_model_comparison <- function(mdt) {
  compare_models_plot <- mdt %>%
    melt.data.table(id.vars = "Model", measure.vars = c("AICc", "delta_AICc", "AICc_weight", "R2", "AdjR2", "RMSE")) %>%
    ggplot(aes(x = value, y = reorder(Model, -value))) +
    geom_point() +
    geom_text(aes(label = value), nudge_y = 0.3, nudge_x = -0.1) +
    geom_segment(aes(x = 0, y = reorder(Model, -value), xend = value, yend = reorder(Model, -value))) +
    labs(x = "", y = "") +
    facet_wrap(~variable, nrow = 1, scales = "free") +
    coord_cartesian(clip = "off") +
    theme_pubclean() +
    theme(legend.position = "none")
  
 return(compare_models_plot)
}


total_impacts <- function(mod, W) {
  model_imps <- impacts(mod, listw = W, R = 1000)
  model_effects_summary <- summary(model_imps, zstats = T, short = T)
  total_effects_table <- cbind(
    Variable = rownames(model_effects_summary$pzmat),
    as.data.table(model_effects_summary$res) %>% rename_all(~paste0("Coef_", .x)) %>% mutate_all(~round(.x, 4)), 
    as.data.table(model_effects_summary$pzmat) %>% rename_all(~paste0("p-value_", tolower(.x))) %>% mutate_all(~round(.x, 8)))
  
  return(total_effects_table)
}


standardise_sf <- function(data_sf, y_var){
  
  y_var_vals <- data_sf %>% dplyr::select(dplyr::all_of(y_var)) %>% st_drop_geometry()
  data_sf <- data_sf %>%
    mutate(across(is.numeric, ~ as.numeric(scale(.)))) %>% 
    mutate(y_var = y_var_vals)
  
  return(data_sf)
}



# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  require(xtable)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 