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

    require(viridis)
    colINCLUDE = viridis(n=4)
    names(colINCLUDE) = c("val", "tau", "unc", "kcrv.unc")
    colEXCLUDE = viridis(n=4, option="B")[c(2,3)]
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
    require(viridis)
    colINCLUDE = viridis(n=5)[c(3,2)]
    names(colINCLUDE) = c("val", "unc")
    colEXCLUDE = viridis(n=4, option="B")[c(2,3)]
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

## ## ===================================================================
## ## EXAMPLE A
## 
## z = read.table(header=TRUE, text="
## lab          x      U       n  k
## BAM          0.0978 0.0006  6  2
## IAEA         0.099  0.003   5  2
## LNE          0.0930 0.0026  6  2
## NIM          0.0975 0.0015  6  2
## NIST         0.0966 0.0013 13  2
## NMIA         0.1000 0.0062  6  2.18
## NMIJ         0.0963 0.0021  5  2
## TUBITAK      0.110  0.0025  5  2")
## z$ux = z$U/z$k
## z$nu = c(rep(60, 5), 12, rep(60,2)) 
## 
## ## HIERARCHICAL LAPLACE+GAUSS
## ## The consensus estimate is: 0.09758
## ##    (where 4 significant digits are believed to be reliable)
## ## The standard uncertainty is: 0.001148
## ## The credible interval ranges from: 0.09536 to 0.09997
## ## The dark uncertainty (tau) is: 0.004382
## 
## KCplot(val=z$x, unc=z$ux, ylim=c(0.0875, 0.1165),
##        ref.val=0.0976, ref.unc=0.0016,
##        kcrv=0.09758, kcrv.unc=0.001148, tau=0.004382,
##        lab=z$lab, title="CCQM-K49 (Cd)\nLaplace+Gauss",
##        title.position="left", title.line=-2,
##        ylab="Mass Fraction / (mg/kg)")
## 
## ## Unilateral Degrees of Equivalence
## DoE.LG = read.table(header=TRUE, as.is=TRUE, text="
## Lab       DoE.x       DoE.U95   DoE.Lwr     DoE.Upr
## BAM       0.0002222   0.01029   -0.009809   0.01081
## IAEA      0.001422    0.01027   -0.008864   0.0117
## LNE      -0.004578    0.01035   -0.0151     0.005626
## NIM      -0.00007782  0.01029   -0.01048    0.01014
## NIST     -0.0009778   0.01018   -0.01102    0.009328
## NMIA      0.002422    0.01139   -0.008915   0.01387
## NMIJ     -0.001278    0.01026   -0.0115     0.009021
## TUBITAK   0.01242     0.01037    0.001855   0.02259")
## 
## DoEplot(DoE.LG, title="CCQM-K49 (Cd)\nLaplace+Gauss",
##         ylim=c(-0.0135, 0.0245),
##        title.position="left", title.line=-2,
##        ylab="DoE / (mg/kg)")

## ## ===================================================================
## ## EXAMPLE B
## 
## K155.A.Ni = read.table(header=TRUE, as.is=TRUE, text="
##      lab     w    uw   nu
##     FTMC 4.280 0.650  9.0
##     NIMT 4.320 0.071 60.4
##     RISE 4.480 0.150 60.4
##      NRC 4.522 0.022 60.4
##    KRISS 4.534 0.020  7.9
##      UME 4.568 0.019 60.4
##     NMIA 4.580 0.070 40.7
##     NMIJ 4.620 0.060 60.4
##    UNIIM 4.700 0.450 60.4
##      NIM 4.744 0.090 60.4
## VNIIFTRI 6.670 0.380 60.4")
## 
## ## NICOB Hierarchical Bayes (Gaussian)
## ## The consensus estimate is: 4.547
## ## The standard uncertainty is: 0.03224
## ## The 95% credible interval ranges from: 4.481 to 4.614
## ## The dark uncertainty (tau) is: 0.0615
## 
## KCplot(val=K155.A.Ni$w, unc=K155.A.Ni$uw, lab=K155.A.Ni$lab,
##        title="CCQM-K155-SampleA-Ni", exclude=c("FTMC", "VNIIFTRI"),
##        title.position="left",
##        kcrv=4.547, kcrv.unc=0.03224, tau=0.0615,
##        ylab=expression(italic(w)~~{}/{}~~(plain(mg)/plain(kg))))
## 
## ## Unilateral Degrees of Equivalence
## DoE = read.table(header=TRUE, as.is=TRUE, text="
## Lab       DoE.x      DoE.U95   DoE.Lwr   DoE.Upr
## NIMT      -0.2268    0.2213    -0.4511   -0.009
## RISE      -0.06682   0.3413    -0.4046    0.2771
## NRC       -0.02482   0.179     -0.2039    0.1539
## KRISS     -0.01282   0.177     -0.1891    0.1653
## UME        0.02118   0.1735    -0.1566    0.1901
## NMIA       0.03318   0.2164    -0.1858    0.2466
## NMIJ       0.07318   0.2041    -0.1337    0.2747
## UNIIM      0.1532    0.9124    -0.7448    1.078
## NIM        0.1972    0.2444    -0.04838   0.4407
## FTMC      -0.2668    1.286     -1.553     1.019
## VNIIFTRI   2.123     0.772      1.351     2.895")
## 
## DoEplot(DoE, title="CCQM-K155-A-Ni DoEs", title.position="left",
##         exclude=c("FTMC", "VNIIFTRI"),
##    ylab=expression(plain(DoE)(plain(Ni))~~{}/{}~~(mu*plain(g)/plain(kg))))

## ## ===================================================================
## ## EXAMPLE C
## 
## PCB28 = data.frame(lab=c("IRMM", "KRISS", "NARL", "NIST", "NMIJ", "NRC"),
##                    x=c(34.3, 32.9, 34.53, 32.42, 31.9, 35.8),
##                    u=c(1.03, 0.69, 0.83, 0.29, 0.4, 0.38),
##                    nu=c(60, 4, 18, 2, 13, 60))
## 
## ## NICOB Hierarchical Bayes (Gaussian)
## ## The consensus estimate is: 33.6
## ## The standard uncertainty is: 0.798
## ## The 95% credible interval ranges from: 32 to 35.2
## ## The dark uncertainty (tau) is: 1.69
## 
## fileHOME.PDF = "~/NIST/METROLOGY/MetrologyBasics/PRESENTATION/"
## fileNAME.PDF = paste0("CCQM-K25-PCB28-MeasurementResults.pdf")
## pdf(file=paste0(fileHOME.PDF, fileNAME.PDF), width=6, height=3,
##     pointsize=12, bg="White")
## par(mar=c(2, 4.5, 1, 0.5))
## KCplot(val=PCB28$x, unc=PCB28$u, lab=PCB28$lab,
##        title="CCQM-K25-PCB28", title.position="left",
##        ylab=expression(italic(w)~~{}/{}~~(plain(ng)/plain(g))))
## dev.off()
## 
## fileHOME.PDF = "~/NIST/METROLOGY/MetrologyBasics/PRESENTATION/"
## fileNAME.PDF = paste0("CCQM-K25-PCB28-ConsensusValue.pdf")
## pdf(file=paste0(fileHOME.PDF, fileNAME.PDF), width=6, height=3,
##     pointsize=12, bg="White")
## par(mar=c(2, 4.5, 1, 0.5))
## KCplot(val=PCB28$x, unc=PCB28$u, lab=PCB28$lab,
##        title="CCQM-K25-PCB28", title.position="left",
##        kcrv=33.6, kcrv.unc=0.798, tau=1.69,
##        ylab=expression(italic(w)~~{}/{}~~(plain(ng)/plain(g))))
## dev.off()
## 
## ## Unilateral Degrees of Equivalence
## DoE = read.table(header=TRUE, as.is=TRUE, text="
## Lab       DoE.x      DoE.U95   DoE.Lwr   DoE.Upr
## IRMM	0.685	4.4	-3.75	5.03
## KRISS	-0.715	4.37	-5.13	3.62
## NARL	0.915	4.36	-3.45	5.27
## NIST	-1.19	4.11	-5.33	2.89
## NMIJ	-1.71	4.03	-5.77	2.32
## NRC	2.19	4.09	-1.89	6.28")
## 
## DoEplot(DoE, title="CCQM-K25-PCB28 DoEs", title.position="left",
##    ylab=expression(plain(DoE)~~{}/{}~~(plain(ng)/plain(g))))

## ===================================================================
