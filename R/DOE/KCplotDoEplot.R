## ===================================================================

## AUTHOR:            Antonio Possolo
## MODIFICATION DATE: 2021 May 06

## ===================================================================

KCplot = function (val, unc, tau=NULL, kcrv=NULL, kcrv.unc=NULL,
                   lab=NULL, title=NULL, title.placement="right",
                   ylab=NULL, exclude=NULL)
{
    require(viridis)
    colINCLUDE = viridis(n=4)
    names(colINCLUDE) = c("val", "tau", "unc", "kcrv.unc")
    colEXCLUDE = viridis(n=4, option="B")[c(2,3)]
    names(colEXCLUDE) = c("val", "unc")
    if (is.null(lab)) {lab = LETTERS[1:length(val)]} 
    names(val) = names(unc) = names(lab) = lab
    if (any(is.na(val)) || is.na(unc)) {
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
    plot(c(0.5,n+0.5), c(min(xlt, na.rm=TRUE), max(xut, na.rm=TRUE)),
         ylab=if (is.null(ylab)) {""} else {ylab},
         axes=FALSE, xlab="", type="n", bty="n")
    axis(2, lwd=0.5)
    adjVALUE = 0.95
    if (title.placement=="right") {adjVALUE=0.95}
    if (title.placement=="center") {adjVALUE=0.5}
    if (title.placement=="left") {adjVALUE=0.05}
    mtext(title, side=3, adj=adjVALUE,
          line=0, col="Black", font.lab=2, cex=1)
    if (!is.null(kcrv)) {
        polygon(c(0.5, n+0.5, n+0.5, 0.5),
                c(kcrv-kcrv.unc, kcrv-kcrv.unc, kcrv+kcrv.unc, kcrv+kcrv.unc), 
                border=colINCLUDE["kcrv.unc"], col=colINCLUDE["kcrv.unc"]) 
        segments(0.5, kcrv, n+0.5, kcrv, lwd=1, col=colINCLUDE["val"],
                 lend="square")
    }
    nInclude = length(include)
    nExclude = length(exclude)
    segments(1:nInclude, xl[include], 1:nInclude, xu[include],
             col=colINCLUDE["unc"], lwd=6, lend="square")
    if (nExclude > 0) {
        segments(nInclude+(1:nExclude), xl[exclude],
                 nInclude+(1:nExclude), xu[exclude],
                 col=colEXCLUDE["unc"], lwd=6, lend="square") }
    if (!is.null(tau)) {
        segments(1:n, xlt, 1:n, xut, col=colINCLUDE["tau"], lwd=2,
                 lend="square") }
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

## ===================================================================

DoEplot = function (DoE, title, title.placement="left", ylab=NULL,
                    exclude=NULL)
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
    plot(c(0.5,n+0.5), c(min(Lwr, na.rm=TRUE), max(Upr, na.rm=TRUE)),
         ylab=if (is.null(ylab)) {""} else {ylab},
         axes=FALSE, xlab="", type="n", bty="n")
    axis(2, lwd=0.5)
    if (title.placement=="right") {adjVALUE=0.95}
    if (title.placement=="center") {adjVALUE=0.5}
    if (title.placement=="left") {adjVALUE=0.05}
    mtext(title, side=3, adj=adjVALUE, line=0, col="Black", font.lab=2, cex=1)
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
## K144.Fe = read.table(header=TRUE, as.is=TRUE, text="
##   lab    w    uw   nu
##   JSI 2.25 0.090 60.4
## KRISS 2.14 0.096  7.1
##   NIM 2.90 0.200 60.4
## UNIIM 2.24 0.205 60.4")
## 
## ## NICOB Hierarchical Bayes (Gaussian)
## ## The consensus estimate is: 2.32
## ## The standard uncertainty is: 0.156
## ## The 95% credible interval ranges from: 2.07 to 2.64
## ## The dark uncertainty (tau) is: 0.189
## 
## KCplot(val=K144.Fe$w, unc=K144.Fe$uw, lab=K144.Fe$lab,           
##        title="CCQM-K144-Fe", kcrv=2.32, kcrv.unc=0.156, tau=0.189, 
##        ylab=expression(italic(w)(plain(Fe))~~{}/{}~~(plain(mg)/plain(kg))))
## 
## ## Unilateral Degrees of Equivalence
## DoE = read.table(header=TRUE, as.is=TRUE, text="
## Lab   DoE.x   DoE.U95   DoE.Lwr   DoE.Upr
## JSI   -0.0686  0.619   -0.721     0.509
## KRISS -0.179   0.627   -0.855     0.407
## NIM    0.581   0.707   -0.164     1.25
## UNIIM -0.0786  0.7     -0.816     0.59")
## 
## DoEplot(DoE, title="CCQM-K144-Fe DoEs", title.placement="left",
##    ylab=expression(plain(DoE)(plain(Fe))~~{}/{}~~(plain(mg)/plain(kg))))
## 
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
##        title.placement="left",
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
## DoEplot(DoE, title="CCQM-K155-A-Ni DoEs", title.placement="left",
##         exclude=c("FTMC", "VNIIFTRI"),
##    ylab=expression(plain(DoE)(plain(Ni))~~{}/{}~~(mu*plain(g)/plain(kg))))

## ===================================================================
