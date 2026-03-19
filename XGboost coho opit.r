# ---- packages ----
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(xgboost)
  library(readr)
})


dat<-read_csv("data/dat.csv")[,-1] |> rename(OPIH=abundance) |> filter(year>=1984) %>%
  mutate(across(where(is.character), trimws)) %>%
  arrange(year) # don't subset years here—rolling window needs full history



# -------------------------
# Settings
# -------------------------
USE_LOG        <- TRUE   # fit on log1p(OPIH) + Duan smearing back-transform
SEED           <- 42
K_FOLDS        <- 5
ETA            <- 0.05
MAX_ROUNDS     <- 5000
EARLY_STOP     <- 75
HALF_LIFE_GRID <- c(5, 8, 10, 12, 15, 20)

stopifnot(all(c("year", "OPIH") %in% names(dat)))

# Predictors: drop response & constant species; keep year as a feature
predictors <- setdiff(names(dat), c("OPIH", "species"))

to_numeric <- function(df, cols) {
  for (nm in cols) if (!is.numeric(df[[nm]])) df[[nm]] <- as.numeric(df[[nm]])
  df
}

# Time-decay weights
make_decay <- function(year_vec, half_life) {
  age <- max(year_vec, na.rm = TRUE) - year_vec
  w <- 2^(-age / half_life)
  pmax(w, 1e-6)
}

# Contiguous time-blocked folds
make_block_folds <- function(n, k) {
  k <- max(3, min(k, floor(n / 6))) # avoid tiny folds
  cuts <- floor(seq(1, n + 1, length.out = k + 1))
  lapply(seq_len(k), function(i) seq(cuts[i], cuts[i + 1] - 1))
}

base_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = ETA,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  seed = SEED          # must live in params, not as a top-level xgb.cv() argument
)

# -------------------------
# Rolling one-year-ahead backtest for last 15 observed years
# -------------------------
obs_years <- dat %>% filter(!is.na(OPIH)) %>% pull(year)
targets   <- tail(sort(obs_years), 15)  # last 15 with observed OPIH

results <- list()
set.seed(SEED)

for (t in targets) {
  print(t)
  train_df <- dat %>% filter(year <= t - 1, !is.na(OPIH)) %>% arrange(year)
  test_df  <- dat %>% filter(year == t) %>% arrange(year)
  
  if (nrow(test_df) != 1L) {
    message(sprintf("No unique test row for year %d; skipping.", t))
    results[[as.character(t)]] <- NULL
    next
  }
  if (nrow(train_df) < 20L) {
    message(sprintf("Not enough training rows for year %d (n=%d); skipping.", t, nrow(train_df)))
    results[[as.character(t)]] <- NULL
    next
  }
  
  # Coerce numerics
  train_df <- to_numeric(train_df, predictors)
  test_df  <- to_numeric(test_df,  predictors)
  
  X_tr <- as.matrix(train_df[, predictors])
  y_tr <- if (USE_LOG) log1p(train_df$OPIH) else train_df$OPIH
  
  # Tune half-life & best_iter IN-WINDOW (no leakage)
  folds <- make_block_folds(nrow(train_df), K_FOLDS)
  
  best <- list(score = Inf, half_life = NA, best_iter = NA)
  
  for (hl in HALF_LIFE_GRID) {
    w <- make_decay(train_df$year, hl)
    dtrain <- xgb.DMatrix(data = X_tr, label = y_tr, weight = w)
    
    cv <- xgb.cv(
      params = base_params,
      data = dtrain,
      folds = folds,
      nrounds = MAX_ROUNDS,
      early_stopping_rounds = EARLY_STOP,
      verbose = 0
    )
    
    # cv$best_iteration can be NULL in some xgboost versions; fall back to eval log
    best_iter_cv <- cv$best_iteration
    if (is.null(best_iter_cv) || length(best_iter_cv) != 1L || is.na(best_iter_cv)) {
      idx <- which.min(cv$evaluation_log$test_rmse_mean)
      best_iter_cv <- if (length(idx) == 1L) as.integer(cv$evaluation_log$iter[idx]) else NA_integer_
    }
    if (is.na(best_iter_cv)) next
    
    score <- cv$evaluation_log %>%
      dplyr::filter(iter == best_iter_cv) %>%
      dplyr::pull(test_rmse_mean)
    
    if (length(score) == 1L && !is.na(score) && (score < best$score)) {
      best <- list(score = score, half_life = hl, best_iter = best_iter_cv)
    }
  }
  
  if (is.na(best$half_life)) {
    message(sprintf("CV failed to pick half-life for year %d; skipping.", t))
    results[[as.character(t)]] <- NULL
    next
  }
  
  # Fit final on full train window with chosen half-life
  w_final <- make_decay(train_df$year, best$half_life)
  dtrain_final <- xgb.DMatrix(data = X_tr, label = y_tr, weight = w_final)
  
  final_mod <- xgb.train(
    params = base_params,
    data = dtrain_final,
    nrounds = best$best_iter,
    verbose = 0
  )
  
  # Weighted Duan smearing (for log fits)
  smear <- 1
  if (USE_LOG) {
    yhat_log <- predict(final_mod, dtrain_final)
    res_log  <- y_tr - yhat_log
    smear    <- sum(exp(res_log) * w_final) / sum(w_final)
  }
  
  # Predict year t using year-t covariates (but NOT year-t OPIH)
  X_te <- as.matrix(test_df[, predictors])
  dtest <- xgb.DMatrix(data = X_te)
  pred_base <- predict(final_mod, dtest)
  pred_opih <- if (USE_LOG) (smear * exp(pred_base) - 1) else pred_base
  
  actual <- test_df$OPIH
  ape <- abs(pred_opih - actual) / abs(actual) * 100
  
  results[[as.character(t)]] <- data.frame(
    year        = t,
    OPIH_actual = as.numeric(actual),
    OPIH_pred   = as.numeric(pred_opih),
    APE_percent = as.numeric(ape),
    half_life   = best$half_life,
    best_iter   = best$best_iter,
    cv_rmse     = best$score
  )
}

# Bind safely, even if some years were skipped
mape_tbl <- results %>%
  purrr::compact() %>%
  dplyr::bind_rows() %>%
  dplyr::arrange(year)

print(mape_tbl)

mape_val <- mean(mape_tbl$APE_percent, na.rm = TRUE)
cat(sprintf("\nMAPE over last %d observed years = %.2f%%\n",
            nrow(mape_tbl), mape_val))

# -------------------------
# Optional: predict 2025 using a model trained only on years <= 2024
# -------------------------
if (any(is.na(dat$OPIH))) {
  pred_row <- dat %>% filter(is.na(OPIH)) %>% arrange(year)
  if (nrow(pred_row) >= 1L) {
    # Use *all* observed years up to year t-1 for this future prediction
    t_future <- pred_row$year[1]
    train_df <- dat %>% filter(!is.na(OPIH), year <= (t_future - 1)) %>% arrange(year)
    
    # Tune in-window (≤ t_future - 1)
    train_df <- to_numeric(train_df, predictors)
    pred_row <- to_numeric(pred_row,  predictors)
    
    X_tr <- as.matrix(train_df[, predictors])
    y_tr <- if (USE_LOG) log1p(train_df$OPIH) else train_df$OPIH
    folds <- make_block_folds(nrow(train_df), K_FOLDS)
    
    best <- list(score = Inf, half_life = NA, best_iter = NA)
    for (hl in HALF_LIFE_GRID) {
      w <- make_decay(train_df$year, hl)
      dtrain <- xgb.DMatrix(data = X_tr, label = y_tr, weight = w)
      cv <- xgb.cv(
        params = base_params, data = dtrain, folds = folds,
        nrounds = MAX_ROUNDS, early_stopping_rounds = EARLY_STOP,
        verbose = 0
      )
      best_iter_cv <- cv$best_iteration
      if (is.null(best_iter_cv) || length(best_iter_cv) != 1L || is.na(best_iter_cv)) {
        idx <- which.min(cv$evaluation_log$test_rmse_mean)
        best_iter_cv <- if (length(idx) == 1L) as.integer(cv$evaluation_log$iter[idx]) else NA_integer_
      }
      if (is.na(best_iter_cv)) next
      score <- cv$evaluation_log %>% filter(iter == best_iter_cv) %>% pull(test_rmse_mean)
      if (length(score) == 1L && !is.na(score) && score < best$score) best <- list(score = score, half_life = hl, best_iter = best_iter_cv)
    }
    if (!is.na(best$half_life)) {
      w_final <- make_decay(train_df$year, best$half_life)
      dtrain_final <- xgb.DMatrix(data = X_tr, label = y_tr, weight = w_final)
      final_mod <- xgb.train(params = base_params, data = dtrain_final, nrounds = best$best_iter, verbose = 0)
      
      smear <- 1
      if (USE_LOG) {
        yhat_log <- predict(final_mod, dtrain_final)
        res_log  <- y_tr - yhat_log
        smear    <- sum(exp(res_log) * w_final) / sum(w_final)
      }
      
      X_new <- as.matrix(pred_row[, predictors])
      dtest <- xgb.DMatrix(data = X_new)
      pred_base <- predict(final_mod, dtest)
      pred_opih <- if (USE_LOG) (smear * exp(pred_base) - 1) else pred_base
      
      pred_out <- pred_row %>% transmute(year, OPIH_pred = as.numeric(pred_opih))
      print(pred_out)
    } else {
      message("Could not tune a model for the future prediction window; skipping future prediction.")
    }
  }
}
