mlda.sig <-
function (score.obj, dataset, loci.ID = NULL, pvalue = 0.01, 
    filter = TRUE) 
{
    sr.obj <- score.obj$sr.obj
    score <- score.obj$score
    if (is.null(loci.ID)) {
        loci.ID <- seq(1, nrow(dataset$R), 1)
    }
    cc <- 1
    while (!is.na(sr.obj$fdr.pos[cc, 1]) && cc < 500 && sr.obj$fdr.pos[cc, 
        1] > pvalue) {
        cc <- cc + 1
        if (cc <= nrow(sr.obj$fdr.pos)) {
            pos.cut <- sr.obj$fdr.pos[cc, "thres"]
            pos.count <- sr.obj$fdr.pos[cc, "count"]
            pos.p <- sr.obj$fdr.pos[cc, "fdr.median"]
            up.flag <- sr.obj$SR >= pos.cut
        }
        else {
            pos.cut <- NULL
            pos.count <- 0
            pos.p <- NULL
            up.flag <- rep(FALSE, length(sr.obj$SR))
            break
        }
    }
    cc <- 1
    while (!is.na(sr.obj$fdr.neg[cc, 1]) && cc < 500 && sr.obj$fdr.neg[cc, 
        1] > pvalue) {
        cc <- cc + 1
        if (cc <= nrow(sr.obj$fdr.neg)) {
            neg.cut <- sr.obj$fdr.neg[cc, "thres"]
            neg.count <- sr.obj$fdr.neg[cc, "count"]
            neg.p <- sr.obj$fdr.neg[cc, "fdr.median"]
            down.flag <- sr.obj$SR <= neg.cut
        }
        else {
            neg.cut <- NULL
            neg.count <- 0
            neg.p <- NULL
            down.flag <- rep(FALSE, length(sr.obj$SR))
            break
        }
    }
    min.score <- apply(score, 1, min)
    if (filter) {
        flag <- min.score == 0
    }
    else {
        flag <- rep(FALSE, nrow(score))
    }
    pos.loci.sig <- cbind(loci.ID[up.flag & !flag], sr.obj$score.se[up.flag & 
        !flag], sr.obj$score.re[up.flag & !flag])
    neg.loci.sig <- cbind(loci.ID[down.flag & !flag], sr.obj$score.se[down.flag & 
        !flag], sr.obj$score.re[down.flag & !flag])
    sd.re.up <- sr.obj$SR[up.flag & !flag]
    sd.re.down <- sr.obj$SR[down.flag & !flag]
    index1 <- sort(sd.re.up, index.return = TRUE, decreasing = TRUE)
    index2 <- sort(sd.re.down, index.return = TRUE)
    pos.sig <- pos.loci.sig[index1$ix, ]
    neg.sig <- neg.loci.sig[index2$ix, ]
    if (neg.count > 1 & pos.count > 1) 
        colnames(pos.sig) <- colnames(neg.sig) <- c("loci ID", 
            "sensitive score", "resistant score")
    if (neg.count == 1) 
        names(neg.sig) <- c("loci ID", "sensitive score", "resistant score")
    if (pos.count == 1) 
        names(pos.sig) <- c("loci ID", "sensitive score", "resistant score")
    result <- list(loci.up = pos.sig, loci.down = neg.sig, pos.p = pos.p, 
        neg.p = neg.p)
    result
}
