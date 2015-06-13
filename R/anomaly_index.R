##' Anomalie-Index
##'
##' Berechnet den Anomalie-Index. Details in Vorlesungs-Unterlagen.
##' @title Anomalie-Index
##' @param x ein data.frame mit Jahrringmessungen
##' @param ref ein data.frame mit Referenz-Jahrringmessungen (optional)
##' @return ein data.frame
##' @importFrom dplR combine.rwl
##' @export
ai <- function(x, ref = NULL) {

    require(dplR)

    if (!is.null(ref)) {
        refm <- data.frame(ref = rowMeans(ref, na.rm = TRUE))
        X <- combine.rwl(refm, x)
        X <- apply(X, 2, function(x) x - X[,1])
        x <- X[,-1]
    }

    n <- ncol(x)
    yrs <- as.numeric(rownames(x))
        
    ## only the "treatment" series

    for (i in 1:n) {

        series <- na.omit(data.frame(year = yrs, series = x[,i]))
        n_no <- nrow(series)
        out <- data.frame(NULL)

        for (j in 1:(n_no - 4)) {
            end <- j
            start <- j + 4
            sel <- series[start:end,]
            pre_av <- mean(sel$series[2:5]) + 0.0001
            perc <- round(sel$series[1]/pre_av * 100 - 100, 0)
            perc_year <- sel$year[1]
            out <- rbind(out, data.frame(year = perc_year, perc = perc))
        }

        out_dplr <- data.frame(out$perc)
        names(out_dplr) <- paste0(names(x1)[i], "_perc")
        rownames(out_dplr) <- out$year
        
        if (i == 1) {
            OUT <- out_dplr
        } else {
            OUT <- combine.rwl(OUT, out_dplr)
        }
    }
    
    ## closure style identification of event years...
    smaller_than <- function(perc1, perc2) {
        function(x) {
            apply(x, 1, function(x) {
                crit <- which(x < perc1 & x > perc2)
                n <- length(crit)
                m <- length(x)
                round(n/m * 100, 0)
            })
        }
    }
    
    smaller_40 <- smaller_than(-40, -55)
    smaller_55 <- smaller_than(-55, -70)
    smaller_70 <- smaller_than(-70, -1000)
    
    perc40 <- smaller_40(OUT)
    perc55 <- smaller_55(OUT)
    perc70 <- smaller_70(OUT)
    
    percs <- data.frame(perc40, perc55, perc70)
    class(percs) <- c("aindex", "data.frame")
    percs
}

##' @export plot aindex
plot.aindex <- function(x, ...) {
    yrs <- as.numeric(rownames(x))
    ymax <- max(unlist(x)) + 10
    plot(yrs, x$perc40, type = "l", col = "blue", ylim = c(0, ymax),
         ylab = "Anomalie-Index [%]", xlab = "Jahr", ...)
    lines(yrs, x$perc55, col = "orange")
    lines(yrs, x$perc70, col = "red")
    order70 <- order(x$perc70, decreasing = TRUE)
    xorder70 <- x[order70, ]
    max70 <- head(xorder70, 5)
    maxyears <- as.numeric(rownames(max70))
    text(maxyears, max70$perc70 + 5, maxyears, srt = 90, col = "red")
    order55 <- order(x$perc55, decreasing = TRUE)
    xorder55 <- x[order55, ]
    max55 <- head(xorder55, 5)
    maxyears <- as.numeric(rownames(max55))
    text(maxyears, max55$perc55 + 5, maxyears, srt = 90, col =
        "orange")
    order40 <- order(x$perc40, decreasing = TRUE)
    xorder40 <- x[order40, ]
    max40 <- head(xorder40, 5)
    maxyears <- as.numeric(rownames(max40))
    text(maxyears, max40$perc40 + 5, maxyears, srt = 90, col = "blue")
    legend("topleft", legend = c("< -40%", "< -55%", "< -70%"),
           col = c("blue", "orange", "red"), lty = 1, bty = "n")
}
