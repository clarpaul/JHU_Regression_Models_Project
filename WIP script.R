

bmdf <- as.data.frame(bestmodels[1:5, ])
bmdf$disp <- NULL
bmdf$drat <- NULL
bmdf$gear <- NULL
bmdf[,c(1:4,6:12)] <- round(bmdf[,c(1:4,6:12)],1)
bmdf$hp <- round(bmdf$hp,2)
names(bmdf)[1] <- "(Int)"
bmdf <- format(bmdf)
bmdf[which(bmdf == "   NA", arr.ind = TRUE)] <- "     "
bmdf[which(bmdf == "  NA", arr.ind = TRUE)] <- "    "
bmdf[which(bmdf == " NA", arr.ind = TRUE)] <- "   "
bmdf


bm_uncondf <- as.data.frame(best_unconmodels[1:10, ])
bm_uncondf$gear <- NULL
bm_uncondf[,c(1:4,6:11)] <- round(bm_uncondf[,c(1:4,6:11)],1)
bm_uncondf$weight <- round(bm_uncondf$weight,2)
bm_uncondf$hp <- round(bm_uncondf$hp,2)
names(bm_uncondf)[1] <- "(Int)"
bm_uncondf <- format(bm_uncondf)
bm_uncondf[which(bm_uncondf == "   NA", arr.ind = TRUE)] <- "     "
bm_uncondf[which(bm_uncondf == "  NA", arr.ind = TRUE)] <- "    "
bm_uncondf[which(bm_uncondf == " NA", arr.ind = TRUE)] <- "   "
bm_uncondf


loocv <- function(lm_model){
        h <- lm.influence(lm_model)$hat
        sqrt(mean((residuals(lm_model)/(1 - h))^2))
}

# randomly define the folds; k data points each except sometimes the last fold
# let fn lkocv look for the fold definition in the calling environment
n_multi <- 10



bestmodels_loocv <- dredge(globalmodel, subset = ~ am, rank = loocv)


str(mtcars)

cotbls_loocv_uncon <- coefTable(bestmodels_loocv_uncon)
round(cotbls_loocv_uncon[["263"]][,1:2],2)

round(cotbls_loocv_uncon[["325"]][,1:2],2)

topmodel_loocv_uncon <- lm(mpg ~ carb + cyl + wt, mtcars)
