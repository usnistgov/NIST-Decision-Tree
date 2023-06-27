## ===================================================================

## AUTHOR:            Antonio Possolo
## MODIFICATION DATE: 2022 May 24

## ===================================================================

KCplot = function (val, unc, lab=NULL,
                   ref.val=NULL, ref.unc=NULL,
                   kcrv=NULL, kcrv.unc=NULL, tau=NULL, 
                   title=NULL, title.position="right", title.line=0,
                   xlab.line=NULL, xlab.cex=NULL,
                   ylab=NULL, ylim=NULL, yaxis.labels=NULL,
                   exclude=NULL)
{
    ## INPUTS
    ## val = Measured values
    ## unc = Standard uncertainties associated with the measured values
    ## lab = Labels for the measurement results
    ## ref.val = Reference value that may have been previously used as
    ##    KCRV, computed by a third party for the same measurement results 
    ## ref.unc = Std. unc. associated with the ref.val (will be
    ##    depicted as a light blue band around the ref.val)
    ## kcrv = Key comparison reference value (KCRV)
    ## kcrv.unc = Std. unc. associated with the KCRV (will be depicted
    ##    as a light yellow and around the KCRV)
    ## tau = Dark uncertainty detected during the computation of the KCRV
    ## title = Title for the plot
    ## title.position = Position for the title ("right", "center", "left")
    ## title.line = Line placement relative to the plot (positive away from the
    ##    top side of the plotting area, negative into the plotting area)
    ## xlab.line = Line placement of bottom (or single) line for
    ##    labels along x-axis, relative to the plot (positive away
    ##    from the bottom side of the plotting area, negative into the
    ##    plotting area) 
    ## xlab.cex = Character size expansion for labels along x-axis
    ## ylab = Label for the vertical axis (can be an expression)
    ## ylim = Range of values for the vertical axis
    ## yaxis.labels = Character vector with numeric labels for y-axis
    ##    (these are converted to numbers for placement of the tick marks
    ##    that will be marked with these labels)
    ## exclude = Character strings from among those in "lab",
    ##    indicating the measurement results to be marked as having
    ##    been excluded from the calculation of the KCRV
    ## VALUE -- NULL
    ## OUTPUT -- Plot

    
    colINCLUDE = viridis::viridis(n=4)
    names(colINCLUDE) = c("val", "tau", "unc", "kcrv.unc")
    colEXCLUDE = viridis::viridis(n=4, option="B")[c(2,3)]
    names(colEXCLUDE) = c("val", "unc")
    if (is.null(lab)) {lab = LETTERS[1:length(val)]} 
    lab = as.character(lab)
    names(val) = names(unc) = names(lab) = lab
    if (any(is.na(val)) || any(is.na(unc))) {
        print('KCplot: There are missing values in "val" or in "unc"')
        return(NULL) }
    include = setdiff(lab, exclude)
    x = c(val[include], val[exclude])
    u = c(unc[include], unc[exclude])
    lab = c(lab[include], lab[exclude])
    xlt = xl = x-u
    xut = xu = x+u
    names(xl) = names(xlt) = names(xu) = names(xut) = lab
    if (!is.null(tau)) {
        xlt = x-sqrt(u^2 + tau^2)
        xut = x+sqrt(u^2 + tau^2) }
    n = length(x)
    yRANGE = if (is.null(ylim)) {
                 if (!is.null(ref.val) & !is.null(ref.unc)) {
                     range(c(ref.val-ref.unc, ref.val+ref.unc,
                             xlt, xut), na.rm=TRUE)
                 } else { range(c(xlt, xut), na.rm=TRUE) }} else {ylim}
    plot(c(0.5,n+0.5), yRANGE,
         ylab=if (is.null(ylab)) {""} else {ylab},
         axes=FALSE, xlab="", type="n", bty="n")
    if (is.null(yaxis.labels)) { axis(2, lwd=0.5)
    } else { yaxis.at = as.numeric(yaxis.labels)
        axis(2, at=yaxis.at, labels=yaxis.labels, lwd=0.5) }
    adjVALUE = 0.95
    if (title.position=="right") {adjVALUE=0.95}
    if (title.position=="center") {adjVALUE=0.5}
    if (title.position=="left") {adjVALUE=0.05}
    mtext(title, side=3, adj=adjVALUE,
          line=title.line, col="Black", font.lab=2, cex=1)
    if (!is.null(kcrv)) {
        polygon(c(0.5, n+0.5, n+0.5, 0.5),
                c(kcrv-kcrv.unc, kcrv-kcrv.unc, kcrv+kcrv.unc, kcrv+kcrv.unc), 
                border=colINCLUDE["kcrv.unc"], col=colINCLUDE["kcrv.unc"]) 
        segments(0.5, kcrv, n+0.5, kcrv, lwd=1, col=colINCLUDE["val"],
                 lend="square")
    }
    if (!is.null(ref.val) & !is.null(ref.unc)) {
        polygon(c(0.25, n+0.25, n+0.25, 0.25),
                c(ref.val-ref.unc, ref.val-ref.unc,
                  ref.val+ref.unc, ref.val+ref.unc),
                col="#43BBAD20", border="#43BBAD20")
        segments(0.25, ref.val, n+0.25, ref.val, col="Blue",
                 lwd=0.5, lend="square")
    }
    if (!is.null(ref.val) & is.null(ref.unc)) {
        segments(0.25, ref.val, n+0.25, ref.val, col="Blue",
                 lwd=0.5, lend="square")
    }
    nInclude = length(include)
    nExclude = length(exclude)
    segments(1:nInclude, xl[include], 1:nInclude, xu[include],
             col=colINCLUDE["unc"], lwd=6, lend="square")
    if (nExclude > 0) {
        segments(nInclude+(1:nExclude), xl[exclude],
                 nInclude+(1:nExclude), xu[exclude],
                 col=colEXCLUDE["unc"], lwd=6, lend="square") }
    if (!is.null(tau) && (tau > 0)) {
        segments(1:nInclude, xlt[include],
                 1:nInclude, xut[include], col=colINCLUDE["tau"], lwd=2,
                 lend="square") }
    points(1:nInclude, x[include], pch=5, lwd=1, cex=1.5,
           col=colINCLUDE["val"])
    if (nExclude > 0) {
        points(nInclude+(1:nExclude), x[exclude], pch=5, lwd=1, cex=1.5,
               col=colEXCLUDE["val"]) }
    xlab.cex = if (is.null(xlab.cex)) {0.85} else {xlab.cex}
    xlab.line = if (is.null(xlab.line)) {1} else {xlab.line}
    if (n > 2) {
        mtext(lab[seq(from=1, to=n, by=2)], side=1, line=xlab.line,
              at=seq(from=1, to=n, by=2), cex=xlab.cex)
        mtext(lab[seq(from=2, to=n, by=2)], side=1, line=xlab.line-1,
              at=seq(from=2, to=n, by=2), cex=xlab.cex)
    } else {
        mtext(lab, side=xlab.line, line=1, at=1:n, xlab.cex)
    }
    return(invisible(NULL))
}

## ===================================================================

DoEplot = function (DoE, title=NULL, title.position="left", title.line=0,
                    ylab=NULL, ylim=NULL, exclude=NULL)
{
    colINCLUDE = viridis::viridis(n=5)[c(3,2)]
    names(colINCLUDE) = c("val", "unc")
    colEXCLUDE = viridis::viridis(n=4, option="B")[c(2,3)]
    names(colEXCLUDE) = c("val", "unc")
    ## Remove any initial minus sign in the names of the labs
    lab = gsub("^-", "", DoE$Lab)
    x = DoE$DoE.x
    Lwr = DoE$DoE.Lwr
    Upr = DoE$DoE.Upr
    names(x) = names(Lwr) = names(Upr) = names(lab) = lab
    include = setdiff(lab, exclude)
    x = c(x[include], x[exclude])
    lab = c(lab[include], lab[exclude])
    Lwr = c(Lwr[include], Lwr[exclude])
    Upr = c(Upr[include], Upr[exclude])
    n = nrow(DoE)
    yRANGE = if (is.null(ylim)) {
                 c(min(Lwr, na.rm=TRUE), max(Upr, na.rm=TRUE))
             } else {ylim}
    plot(c(0.5,n+0.5), yRANGE,
         ylab=if (is.null(ylab)) {""} else {ylab},
         axes=FALSE, xlab="", type="n", bty="n")
    axis(2, lwd=0.5)
    if (title.position=="right") {adjVALUE=0.95}
    if (title.position=="center") {adjVALUE=0.5}
    if (title.position=="left") {adjVALUE=0.05}
    mtext(title, side=3, adj=adjVALUE, line=title.line,
          col="Black", font.lab=2, cex=1)
    nInclude = length(include)
    nExclude = length(exclude)
    segments(0.5, 0, n+0.5, 0, lwd=1, col="DarkGray", lend="square")
    segments(1:nInclude, Lwr[include], 1:nInclude, Upr[include],
             col=colINCLUDE["unc"], lwd=3, lend="square")
    if (nExclude > 0) {
        segments(nInclude+(1:nExclude), Lwr[exclude],
                 nInclude+(1:nExclude), Upr[exclude],
                 col=colEXCLUDE["unc"], lwd=3, lend="square") }
    points(1:nInclude, x[include], pch=5, lwd=1, cex=1.5,
           col=colINCLUDE["val"])
    if (nExclude > 0) {
        points(nInclude+(1:nExclude), x[exclude], pch=5, lwd=1, cex=1.5,
               col=colEXCLUDE["val"]) }
    if (n > 2) {
        mtext(lab[seq(from=1, to=n, by=2)], side=1, line=1,
              at=seq(from=1, to=n, by=2), cex=0.85)
        mtext(lab[seq(from=2, to=n, by=2)], side=1, line=0,
              at=seq(from=2, to=n, by=2), cex=0.85)
    } else {
        mtext(lab, side=1, line=1, at=1:n, cex=1)
    }
    return(invisible(NULL))
}


