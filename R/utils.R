select_test = function(chosen_test,rvs) {
  # changes all reactive values to 0 except for the selected proceudure
  # e.g. select_test('hgg', <reactive values>)
  test_names = c('awa','wmed','hgg','hlg','hssg')
  match = chosen_test == test_names
  
  for(ii in 1:length(test_names)) {
    if(match[ii]) {
      rvs[[ test_names[ii] ]] = 1
    } else {
      rvs[[ test_names[ii] ]] = 0
    }
  }
  
  return(rvs)
}

get_test_name = function(which_test) {
  test_names = c('awa','wmed','hgg','hlg','hssg')
  
  if(which_test[[ test_names[1] ]] == 1) {
    return("Adaptive Weighted Average")
  } else if(which_test[[ test_names[2] ]] == 1) {
    return("Weighted Median")
  } else if(which_test[[ test_names[3] ]] == 1) {
    return("Hierarchical Gauss-Gauss")
  } else if(which_test[[ test_names[4] ]] == 1) {
    return("Hierarchical Laplace-Gauss")
  } else if(which_test[[ test_names[5] ]] == 1) {
    return("Hierarchical Skew Student-Gauss")
  } else {
    return("No test recommended.")
  }
}

all_false = function(which_test) {
  test_names = c('awa','wmed','hgg','hlg','hssg')
  
  if(which_test[[ test_names[1] ]] == 1) {
    return(FALSE)
  } else if(which_test[[ test_names[2] ]] == 1) {
    return(FALSE)
  } else if(which_test[[ test_names[3] ]] == 1) {
    return(FALSE)
  } else if(which_test[[ test_names[4] ]] == 1) {
    return(FALSE)
  } else if(which_test[[ test_names[5] ]] == 1) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

unselect_test = function(test,rvs) {
  rvs[[ test ]] = 0
  return(rvs)
}

clear_selections = function(rvs) {
  test_names = c('awa','wmed','hgg','hlg','hssg')
  
  for(ii in 1:length(test_names)) {
    rvs[[ test_names[ii] ]] = 0
  }
  
  return(rvs)
}

has_correct_format = function(input) {
  grepl("^\\d+,\\d+(,\\d+)+$",input)
}


KCplot = function (val, unc, tau=NULL, kcrv=NULL, kcrv.unc=NULL,
                   lab=NULL, title=NULL, title.placement="right",
                   ylab=NULL, exclude=NULL) {
  require(viridis)
  colVALUEs = viridis(n=4)
  names(colVALUEs) = c("val", "tau", "unc", "kcrv.unc")
  if (is.null(lab)) {lab = LETTERS[1:length(val)]} 
  names(val) = names(unc) = names(lab) = lab
  keep = setdiff(lab, exclude)
  x = val[keep]
  u = unc[keep]
  lab = lab[keep]
  iNA = is.na(x)
  x = x[!iNA]
  u = u[!iNA]
  lab = lab[!iNA]
  xlt = xl = x-u
  xut = xu = x+u 
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
            border=colVALUEs["kcrv.unc"], col=colVALUEs["kcrv.unc"]) 
    segments(0.5, kcrv, n+0.5, kcrv, lwd=1, col=colVALUEs["val"],
             lend="square")
  }
  segments(1:n, xl, 1:n, xu, col=colVALUEs["unc"], lwd=6, lend="square")
  if (!is.null(tau)) {
    segments(1:n, xlt, 1:n, xut, col=colVALUEs["tau"], lwd=2,
             lend="square") }
  points(1:n, x, pch=5, lwd=1, cex=1.5, col=colVALUEs["val"])
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
