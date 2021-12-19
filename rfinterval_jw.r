rfinterval_jw <- function (formula = NULL, train_data = NULL, test_data = NULL, 
          method = c("oob", "split-conformal", "quantreg"), 
          alpha = 0.1, symmetry = TRUE, seed = NULL, params_ranger = NULL) 
{
  if (is.null(params_ranger)) {
    params_ranger <- list()
  }
  else {
    if (!("quantreg" %in% method)) {
      params_ranger[["quantreg"]] <- FALSE
    }
  }
  response_name <- as.character(terms(as.formula(formula), 
                                      data = train_data)[[2]])
  if ("oob" %in% method) {
    params_ranger[["oob.error"]] <- TRUE
    trainRF <- do.call(ranger::ranger, c(list(formula = formula), 
                                         list(data = train_data), params_ranger))
    testPred <- predict(trainRF, test_data)$predictions
    oob_error <- train_data[, response_name] - trainRF$predictions
    oob_abs_error <- abs(oob_error)
    if (isTRUE(symmetry)) {
      upperPred <- testPred + quantile(oob_abs_error, 1 - 
                                         alpha)
      lowerPred <- testPred - quantile(oob_abs_error, 1 - 
                                         alpha)
    }
    else {
      upperPred <- testPred + quantile(oob_error, 1 - alpha/2)
      lowerPred <- testPred + quantile(oob_error, alpha/2)
    }
    oob_interval <- data.frame(lower = lowerPred, upper = upperPred)
  }
  else {
    oob_interval <- NULL
    testPred <- NULL
  }
  if ("split-conformal" %in% method) {
    params_ranger[c("oob.error", "keep.inbag", 
                    "quantreg")] <- list(FALSE, FALSE, FALSE)
    if (!is.null(seed)) {
      set.seed(seed)
    }
    nrow_traindata <- dim(train_data)[1]
    subset1 <- sample.int(n = nrow_traindata, size = floor(nrow_traindata/2))
    subset2 <- setdiff(1:nrow_traindata, subset1)
    trainRF <- do.call(ranger::ranger, c(list(formula = formula), 
                                         list(data = train_data[subset1, ]), params_ranger))
    sc_error <- abs(train_data[subset2, response_name] - 
                      predict(trainRF, train_data[subset2, ])$predictions)
    sc_error <- sort(sc_error, decreasing = FALSE)
    d <- sc_error[ceiling((nrow_traindata/2 + 1) * (1 - alpha))]
    sc_testPred <- predict(trainRF, test_data)$predictions
    upperPred <- sc_testPred + d
    lowerPred <- sc_testPred - d
    sc_interval <- data.frame(lower = lowerPred, upper = upperPred)
  }
  else {
    sc_interval <- NULL
  }
  if ("quantreg" %in% method) {
    params_ranger[c("oob.error", "keep.inbag", 
                    "quantreg")] <- list(FALSE, TRUE, TRUE)
    trainRF <- do.call(ranger::ranger, c(list(formula = formula), 
                                         list(data = train_data), params_ranger))
    quantreg_testPred <- predict(trainRF, test_data, type = "quantiles", 
                                 quantiles = c(alpha/2, 1 - alpha/2))
    quantreg_interval <- data.frame(quantreg_testPred$predictions)
    colnames(quantreg_interval) <- c("lower", "upper")
  }
  else {
    quantreg_interval <- FALSE
  }
  if (is.null(testPred)) {
    trainRF <- do.call(ranger::ranger, c(list(formula = formula), 
                                         list(data = train_data), params_ranger))
    testPred <- predict(trainRF, test_data)$predictions
  }
  result <- list(oob_interval = oob_interval, sc_interval = sc_interval, 
                 quantreg_interval = quantreg_interval, alpha = alpha, 
                 testPred = testPred, train_data = train_data, test_data = test_data, trainRF = trainRF)
  return(result)
}
