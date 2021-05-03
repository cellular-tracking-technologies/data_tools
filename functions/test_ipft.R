ipfDistance <- function(train, test, method = 'euclidean', subset = NULL, norm = 2,
                        sd = 10, epsilon = 1e-30, alpha = 20, threshold = 20) {
  if (is.null(subset)) {
    m_train <- as.matrix(train)
    m_test  <- as.matrix(test)
  } else {
    m_train <- as.matrix(train[, subset])
    m_test  <- as.matrix(test[, subset])
  }
  if (ncol(m_train) == 1) {m_train <- t(m_train)}
  if (ncol(m_test) == 1) {m_test <- t(m_test)}
  switch(method,
         'euclidean' = dist <- ipfEuclidean(m_train, m_test),
         'manhattan' = dist <- ipfManhattan(m_train, m_test),
         'norm'      = dist <- ipfNormDistance(m_train, m_test, norm),
         'LGD'       = dist <- ipfLGD(m_train, m_test, sd, epsilon),
         'PLGD'      = dist <- ipfPLGD(m_train, m_test, sd, epsilon, alpha, threshold),
         stop("invalid distance method.")
  )
  return(dist)
}

nr <- nrow(train_fgp)
#if (nr < 3) stop("k can not be greater than the number of rows in training set")

params <- list(name = 'knn',
              k = 3,
              method = "euclidean",
              norm = 2,
              weights = "distance",
              sd = 5,
              epsilon = 1e-3,
              alpha = 1,
              threshold = 20,
              FUN = NULL,
              extra_params = NULL,
data   = list(fingerprints = as.data.frame(train_fgp),
              positions = as.data.frame(tr_positions)))
est <- knn(
  tr_fgp = ipfmodel$data$fingerprints,
  ts_fgp = test_fgp,
  k = ipfmodel$params$k,
  method = ipfmodel$params$method,
  weights = ipfmodel$params$weights,
  norm = ipfmodel$params$norm,
  sd = ipfmodel$params$sd,
  epsilon = ipfmodel$params$epsilon,
  alpha = ipfmodel$params$alpha,
  threshold = ipfmodel$params$threshold,
  FUN = ipfmodel$params$FUN)

ts_fgp <- as.matrix(testRSSI)
tr_fgp <- as.matrix(tr_fingerprints)

ne <- matrix(0, nrow = nrow(ts_fgp), ncol = k)
ws <- matrix(0, nrow = nrow(ts_fgp), ncol = k)
for (i in 1:nrow(ts_fgp)) {
  ne[i,] <- order(dm[i, ])[1:k]
  if (weights == 'distance') {
    w <- (1 / (1 + dm[i, ne[i,]]))
    ws[i,] <- w / sum(w)
  } else if (weights == 'uniform') {
    ws[i,] <- 1 / k
  }
}

tr_pos <- tr_positions

col_errors <- rep(0, nrow(ts_fgp))
mloc <- matrix(0, nrow(ts_fgp), ncol(tr_pos))
mloc <- data.frame(mloc)
n_factors <- sum(sapply(tr_pos, function(x) is.factor(x)))
nc <- 1

for (i in 1:ncol(mloc)) {
  if (is.factor(tr_pos[, i])) {
    fm <- matrix(tr_pos[ne, i], ncol = 3)
    mloc[, i] <- factor(apply(fm, 1, function(x) names(which.max(table(x)))))
    if (!is.null(test_pos)) {
      name <- names(tr_pos)[i]
      if (is.null(name)) {
        name = paste0('confusion', nc)
      }
      confusion[[nc]] <- table(test_pos[, i], mloc[, i])
      nms[nc] <- name
      nc <- nc + 1
    }
  } else {
    mloc[, i] <- rowSums(tr_pos[ne[,i], i] * ws)
    if (!is.null(test_pos)) {
      col_errors <- col_errors + (mloc[, i] - test_pos[, i]) ^ 2
    }
  }
}
