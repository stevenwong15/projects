#======================================================================================================================
# initilization
rm(list = ls())

# general purpose
library(magrittr)
library(data.table)
library(tidyverse)
library(lubridate)
library(stringr)
library(reticulate)

# modeling
library(caret)
library(rpart)
library(glmnet)
library(ranger)
library(randomForest)
library(xgboost)
library(pROC)
library(pwr)

# plotting
library(knitr)
library(gridExtra)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(GGally)
library(plotly)
library(ggrepel)
library(DT)
library(rpart.plot)

source("ggplot2_theme.R")
wd <- getwd()
my_palette <- c(brewer.pal(9, "Set1")[c(1, 2, 3, 4, 5, 7, 9)], brewer.pal(8, "Set2")[c(1, 2, 3, 4, 5, 7)])

#======================================================================================================================
# function definition 

# get column names that are numbers
get_numeric_col <- function(data_table) {
  names(data_table)[which(sapply(data_table, class) %in% c("numeric", "integer"))]
}

# get table() in percentage
get_table_per <- function(data_table, y = NA, x = NA) {
  call <- substitute(
    data_table[, .(
      n = .N), 
      by = .(y, x)][, .(
      y, 
      per = n/sum(n)), 
      by = .(x)] %>% 
      dcast(y ~ x, value.var = "per"))
  eval(call)
}

# plot ecdf of x by the classes in y
plot_ecdf_x_by_y <- function(data_table, y = NA) {
  call <- substitute(
    data_table %>%
    melt(id.vars = toString(substitute(y))) %>%
    ggplot(aes(x = value, color = factor(y))) + 
    stat_ecdf() + 
    facet_wrap(~variable, scales = "free_x") +
    labs(color = toString(substitute(y))) +
    scale_color_manual(values = my_palette)
    )
  eval(call)
}

# plot ecdf of y by the classes in x
plot_ecdf_y_by_x <- function(data_table, y = NA) {
  call <- substitute(
    data_table %>%
    melt(id.vars = toString(substitute(y))) %>%
    ggplot(aes(x = y, color = value)) + 
    stat_ecdf() + 
    facet_wrap(~variable, scales = "free_x") +
    scale_color_manual(values = my_palette)
    )
  eval(call)
}

# get confusion matrix for any given table of truth and predicted, across various threshold
get_confusion_matrix <- function(threshold = NA, fit_pred = NA) {
  cm <- roc(fit_pred$truth, fit_pred$predicted, algorithm = 2) %>%
    coords(., threshold, ret = c("threshold", "tp", "tn", "fp", "fn")) %>%
    t() %>%
    as.data.frame() %>% 
    as.data.table(keep.rownames = FALSE)
  cm[, .(
    threshold, tp, tn, fp, fn,
    error_rate = (fp + fn)/(tp + tn + fp + fn),
    pp_value = tp/(tp + fp),  # precision
    tp_rate = tp/(tp + fn),  # recall, y-roc
    fp_rate = fp/(fp + tn)  # x-roc
  )]
}

#----------------------------------------------------------------------------------------------------------------------
# stats

plot_cor <- function(DT, method = "pearson"){
  DT %>% 
  cor(method = method) %>%
  melt() %>%
  data.table() %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + 
  geom_text(aes(label = round(value, 2))) + 
  scale_fill_distiller(palette = "Spectral") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "", y = "", title = sprintf("%s correlation", method),fill = "cor")
}

#----------------------------------------------------------------------------------------------------------------------
# random forest

# plot importance for rf, using library(ranger)
plot_ranger_importance <- function(fit) {
  data.table(
    feature = names(fit$variable.importance),
    importance = fit$variable.importance) %>%
  ggplot(aes(x = reorder(feature, importance), y = importance)) +
  geom_col() +
  coord_flip() + 
  labs(y = "")
}

# plot importance for rf, using library(randomForest)
plot_randomForest_importance <- function(fit, type = MeanDecreaseAccuracy) {
  melt(
    data.table(
      feature = rownames(fit$importance),
      data.table(fit$importance)[, .(MeanDecreaseAccuracy)],
      data.table(fit$importance)[, .(MeanDecreaseGini)]),
    id.vars = "feature", variable.name = "measure", value.name = "importance")[
    measure == type] %>%
  ggplot(aes(x = reorder(feature, importance), y = importance)) +
  geom_col() +
  coord_flip() + 
  facet_wrap(~measure, scales = "free") + 
  labs(y = "", x = "")
}

# get data point required for partial dependency
get_rf_partial_dependency <- function(fit, x, feature, response, n_percentiles = 10, package = "ranger") {

  is_feature_numeric <- class(unlist(x[, deparse(substitute(feature)), with = FALSE])) %in% c("integer", "numeric")
  is_model_regression <- class(unlist(x[, deparse(substitute(response)), with = FALSE])) %in% c("integer", "numeric")

  # get percentiles or unique values
  call <- substitute(
    if(is_feature_numeric) {
      x_percentiles <- x[, .(
        value = as.double(feature))][, .(
        feature = seq(min(value), max(value), length.out = min(uniqueN(value), n_percentiles)))]
    } else {
      x_percentiles <- x[, .(
        value = as.factor(feature))][, .(
        feature = unique(value))]
    }
  )
  eval(call)
  x_percentiles$percentile_id <- 1:nrow(x_percentiles)
  setnames(x_percentiles, "feature", deparse(substitute(feature)))
  
  # create inputs 
  x_all <- Reduce(merge, list(
    list(x_percentiles), 
    list(x[, -deparse(substitute(feature)), with = FALSE], observation_id = 1:nrow(x)))) %>%
    as.data.table()
  setkeyv(x_all, c("observation_id", "percentile_id"))

  # predict
  if(package == "ranger") {
      y_hat <- predict(fit, x_all)$predictions
  } else {
    if(is_model_regression) {
      y_hat <- predict(fit, x_all)
    } else {
      y_hat <- predict(fit, x_all, type = "prob")
    }
  }

  return(list(y_hat, x_all, deparse(substitute(feature)), deparse(substitute(response))))
}

# plot partial dependency
plot_rf_partial_dependency <- function(pd_object, class = 2) {

  is_feature_numeric <- class(unlist(pd_object[[2]][, pd_object[[3]], with = FALSE])) %in% c("integer", "numeric")
  is_model_regression <- class(unlist(pd_object[[2]][, pd_object[[4]], with = FALSE])) %in% c("integer", "numeric")

  if(is_model_regression) {
    dt_plot <- data.table(y = pd_object[[1]], pd_object[[2]])[, .(
      y = mean(y)), 
      by = c(pd_object[[3]], "percentile_id")]
  } else {
    dt_plot <- data.table(y = pd_object[[1]][, class], pd_object[[2]])[, .(
      y = mean(y)), 
      by = c(pd_object[[3]], "percentile_id")]      
  }

  if(is_feature_numeric) {
    dt_plot %>%
    ggplot(aes(x = get(pd_object[[3]]), y)) +
    geom_line() +
    labs(x = pd_object[[3]], y = sprintf("%s hat", pd_object[[4]]), title = "partial dependency plot")
  } else {
    dt_plot %>%
    ggplot(aes(x = get(pd_object[[3]]), y)) +
    geom_col() +
    labs(x = pd_object[[3]], y = sprintf("%s hat", pd_object[[4]]), title = "partial dependency plot")
  }
}

# plot contour
plot_rf_contour <- function(pd_object, class = 2, n_percentiles = 10) {

  is_feature_numeric <- class(unlist(pd_object[[2]][, pd_object[[3]], with = FALSE])) %in% c("integer", "numeric")
  is_model_regression <- class(unlist(pd_object[[2]][, pd_object[[4]], with = FALSE])) %in% c("integer", "numeric")

  if(is_model_regression) {
    dt_diff <- data.table(y = pd_object[[1]], pd_object[[2]])
  } else {
    dt_diff <- data.table(y = pd_object[[1]][, class], pd_object[[2]])
  }
  dt_diff[, diff := y - shift(y), by = .(observation_id)]
  dt_diff <- dt_diff[!is.na(diff)][order(percentile_id, diff)]
  dt_diff_plot <- dt_diff[dt_diff[, .I[round(seq(1, .N, length.out = n_percentiles))], by = .(percentile_id)]$V1]
  dt_diff_plot[, diff_percentile := rowid(percentile_id)]
  dt_diff_plot[, diff_percentile := diff_percentile/max(diff_percentile)]

  if(is_feature_numeric) {
    dt_diff_plot %>%
    ggplot(aes(x = get(pd_object[[3]]), y = diff, group = diff_percentile)) +
    geom_line(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "#E41A1C") + 
    labs(x = pd_object[[3]], y = sprintf("diff in %s hat", pd_object[[4]]), title = "contour plot")
  } else {
    stop("feature must be numerical\n")
  }
}

#----------------------------------------------------------------------------------------------------------------------
# lasso

# plot lasso betas, ggplot
plot_lasso_ggplot <- function(fit, type = NA) {

  fit_lasso_beta <- fit_lasso$beta %>% as.matrix() 
  fit_lasso_beta <- data.table(fit_lasso_beta, feature = rownames(fit_lasso_beta)) %>% 
    melt(id.vars = "feature", variable.name = "alpha_id", value.name = "beta")
  fit_lasso_importance <- data.table(
    alpha_id = paste0("s", 1:length(fit_lasso$lambda)),
    lambda_log = log(fit_lasso$lambda),
    dev_ratio = fit_lasso$dev.ratio)
  fit_lasso_beta <- fit_lasso_beta %>% merge(fit_lasso_importance)

  if(type == "lambda") {
    fit_lasso_beta_label <- fit_lasso_beta[lambda_log == min(lambda_log)]
    fit_lasso_beta %>%
    ggplot(aes(x = lambda_log, y = beta, color = feature)) +
    geom_line(alpha = 0.75) + 
    geom_text(aes(label = feature, x = lambda_log), color = "black", hjust = -.1, 
      data = fit_lasso_beta_label) + 
    theme(legend.position = "none")
  } else {  
    fit_lasso_beta_label <- fit_lasso_beta[dev_ratio == max(dev_ratio)]
    fit_lasso_beta %>%
    ggplot(aes(x = dev_ratio, y = beta, color = feature)) +
    geom_line(alpha = 0.75) + 
    geom_text(aes(label = feature, x = dev_ratio), color = "black", hjust = 1.1, 
      data = fit_lasso_beta_label) + 
    theme(legend.position = "none")
  }
}

# plot lasso betas, base plot
plot_lasso_base <- function(fit, type = NA) {
  vnat <- coef(fit)
  vnat <- vnat[-1, ncol(vnat)]
  plot(fit, xvar = type, label = FALSE) 
  if(type == "lambda") {
    axis(side = 2, at = vnat, line = -5, label = names(vnat), las = 1, tick = FALSE, cex.axis = 0.75)
  } else { 
    axis(side = 4, at = vnat, line = -5, label = names(vnat), las = 1, tick = FALSE, cex.axis = 0.75)
  }
}
