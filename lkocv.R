# given a model, calculates mean-squared test error for leave-k-out cross validation

lkocv <- function(lm_model, folds){
        n_folds <- length(folds)
        modframe <- model.frame(lm_model)
        n <- names(modframe)
        if (length(n) > 1){
                f <- as.formula(paste("mpg ~", paste(n[!n %in% "mpg"], collapse = " + ")))
                } else { 
                f <- as.formula("mpg ~ 1")
        }
        sse <- numeric(n_folds)
        for (i in 1:n_folds){
                # iterate over all the folds, i; for each i...
                # create model based on dataset leaving out fold i
                trnd_model <- lm(formula = f, data = data.frame(modframe[ -folds[[i]], ]) )
                # calculate and store sum of squared errors for each fold i
                sse[i] <- sum((modframe[folds[[i]], "mpg"] -
                                       predict(trnd_model, newdata = data.frame(modframe[folds[[i]],])))^2)
        }
        # sum up the squared errors across all folds, divide by n, get MSE
        sum(sse)/nrow(modframe)
}

lkocv_multi <- function(lm_model, n_sample=nobs(lm_model), k=4, n_multi=50, rseed = 1234){
        set.seed(rseed)
        mse_multi <- numeric(n_multi)
        for (i in 1:n_multi) {
                folds <- split(sample(n_sample), gl(ceiling(n_sample/k), k, n_sample))
                mse_multi[i] <- lkocv(lm_model = lm_model, folds = folds)
        }
        mpg <- model.frame(lm_model)$mpg
        sum_dev_mean <- mpg - mean(mpg)
        # out-of-sample R-squared
        1 - mean((mse_multi)^2)/mean(sum_dev_mean^2)
}
        
calc_diffs <- function(lm_model, n_iter = 100, rseed = 1529){
        set.seed(rseed)
        seeds <- floor(1e6*runif(n_iter))
        out_of_smpl_Rsq <- sapply(seeds, function(x) lkocv_multi(lm_model, rseed = x)) 
}        
        