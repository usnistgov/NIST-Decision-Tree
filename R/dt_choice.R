######################################################################

## DecisionTree.R
##
## AUTHOR:            Antonio Possolo
## MODIFICATION DATE: 2021 Nov 09

######################################################################

get_DT_decision <- function(x,u,
                            sizeHetero=0.10, sizeGauss=0.05, sizeSym=0.05,
                            exclude=NULL) {
    
    # x: observed lab means
    # u: observed lab uncertainties (std errors of x)
    # size*: alpha levels for various test
    # exclude: boolean vector of which labs to include
    
    if(!is.null(exclude)) {
        x = x[!exclude]
        u = u[!exclude]
    }
    
    if(length(x) < 3) {
        stop("Number of included labs is less than 3.")
    
    }
    
    # run all of the tests at once
    homogeneity = NA
    normality = NA
    symmetry = NA
    
    ### 1: homogeneity
    res = metafor::rma(yi = x,sei = u,method = "DL")
    pval = res$QEp
    homogeneity = ifelse(pval < sizeHetero, FALSE, TRUE)
    
    ### 2: normality
    res = stats::shapiro.test((x-median(x))/u) 
    pval = res$p.value
    normality = ifelse(pval < sizeGauss, FALSE, TRUE)
    
    ### 3: symmetry
    res = symmetry::symmetry_test(x,stat='MGG',bootstrap=TRUE, B=5000) 
    pval = res$p.value
    symmetry = ifelse(pval < sizeSym, FALSE, TRUE)
    
    # left side of tree
    if(homogeneity) {
        if(normality) {
            return("AWA")
        } else{
            return("WM")
        }
        
        # right side of tree 
    } else {
        if(symmetry) {
            
            if(normality) {
                return("HGG")
                
            } else{
                return("HLG")
            }
            
        } else {
            return("HSSG")
        }
    }
    
}


DecisionTree = function (x, ux, lab,
                         sizeHetero=0.10, sizeGauss=0.05, sizeSym=0.05,
                         exclude=NULL)
{
    ## INPUTS

    ## x   = Numerical vector with measured values
    ## ux  = Numerical vector with standard uncertainties
    ## lab = Character vector of labels for measurement results, often
    ##       denoting laboratories where the measurements were made

    ## sizeHetero, sizeGauss, sizeSym = Sizes (Type I errors) for
    ## Cochran's Q test of homogeneity, for the Anderson-Darling or
    ## Shapiro-Wilk tests of Gaussian shape, and for the test of
    ## symmetry, respectively

    ## exclude = Vector of character strings (either NULL or including
    ## a subset of vector lab) with labels of measured values
    ## that the Decision Tree should exclude from consideration when
    ## it makes a recommendation of model for the measurement results

    ## OUTPUT

    ## A printed recommendation of the name of the model (from among
    ## those represented as leaves in the Decision Tree) that seems
    ## most appropriate for the measurement results, for the purpose
    ## of computing a key comparison reference value (consensus value)
    ## and degrees of equivalence
    
    browser()

    z = data.frame(w=x, uw=ux, lab=lab)

    ## STEP 1: Data Selection
    keep = setdiff(as.character(z$lab), exclude)
    w = z$w; uw = z$uw; lab = z$lab
    names(w) = names(uw) = names(lab) = lab
    if (!is.null(z$nu)) {nu = z$nu; names(nu) = lab; nu = nu[keep]}
    x = w[keep]
    u = uw[keep]
    lab = lab[keep]
    n = length(x)

    if (is.null(exclude)) {
        xExclude = NULL; uExclude = NULL; labExclude = NULL
        
        if (!is.null(z$nu)) {
            nuExclude = NULL
        }
    } else {
        
        iExclude = match(exclude, z$lab)
        xExclude = z$w[iExclude] 
        uExclude = z$uw[iExclude]
        labExclude = z$lab[iExclude]
        names(xExclude) = names(uExclude) = names(labExclude) = labExclude
        
        if (!is.null(z$nu)) {
            nuExclude = z$nu[iExclude]
            names(nuExclude) = z$lab[iExclude]
            nuExclude = z$nu[iExclude]
        }
    }

    ## STEP 2: Heterogeneity
    require(metafor)
    x.rma = rma(yi=x, sei=u, slab=lab, method="DL")
    cat("\n## Cochran's Q Test p-value =", x.rma$QEp, "\n")

    ## STEP 3: Symmetry
    require(symmetry)
    x.sym = symmetry_test(x, stat="MGG", bootstrap=TRUE, B=500000)
    cat("## Symmetry Test p-value =", x.sym$p.value, "\n")

    ## STEP 4: Gaussian Shape
    require(nortest)
    x.ad = tryCatch(ad.test((x-median(x))/u), error=function(e){})
    if (class(x.ad) == "NULL") {
        cat("## Anderson-Darling Test not possible\n")
        x.ad = data.frame(p.value=NA)
    } else {
        cat("## Anderson-Darling Test p-value =", x.ad$p.value, "\n") }
    x.sw = tryCatch(shapiro.test((x-median(x))/u), error=function(e){})
    if (class(x.sw) == "NULL") {
        cat("## Shapiro-Wilk Test not possible\n")
    } else {
        cat("## Shapiro-Wilk Test p-value =", x.sw$p.value, "\n") }

    if ((x.rma$QEp > sizeHetero) &
        (min(c(x.ad$p.value, x.sw$p.value), na.rm=TRUE) > sizeGauss)) {
        cat("## ==>> Adaptive Weighted Average\n") 
        if (is.null(exclude)) { cat("##", paste(lab, collapse=", "),"\n")
        } else {
            cat("##", paste(c(lab, paste("-", labExclude, sep="")),
                      collapse=", "),"\n") }
        cat("##", paste(c(x, xExclude), collapse=", "), "\n")
        cat("##", paste(c(u, uExclude), collapse=", "), "\n")
        if (!is.null(z$nu)) {
            cat("##", paste(round(c(nu, nuExclude),1), collapse=", "), "\n")}
    }
    if ((x.rma$QEp > sizeHetero) &
        (min(c(x.ad$p.value, x.sw$p.value), na.rm=TRUE) <= sizeGauss)) {
        
        cat("## ==>> Weighted Median\n") 
    }
    
    if ((x.rma$QEp <= sizeHetero) &
        (x.sym$p.value > sizeSym) &
        (min(c(x.ad$p.value, x.sw$p.value), na.rm=TRUE) > sizeGauss)) {
        
        cat("## ==>> Hierarchical Gauss+Gauss\n")
        if (is.null(exclude)) { cat("##", paste(lab, collapse=", "),"\n")
        } else {
            cat("##", paste(c(lab, paste("-", labExclude, sep="")),
                      collapse=", "),"\n") }
        cat("##", paste(c(x, xExclude), collapse=", "), "\n")
        cat("##", paste(c(u, uExclude), collapse=", "), "\n")
        if (!is.null(z$nu)) {
            cat("##", paste(round(c(nu, nuExclude),1), collapse=", "), "\n")}
    }
    
    if ((x.rma$QEp <= sizeHetero) &
        (x.sym$p.value > sizeSym) &
        (min(c(x.ad$p.value, x.sw$p.value), na.rm=TRUE) <= sizeGauss)) {
        cat("## ==>> Hierarchical Laplace+Gauss\n") 
        
        if (is.null(exclude)) { 
            cat("##", paste(lab, collapse=", "),"\n")
            
        } else {
            cat("##", paste(c(lab, paste("-", labExclude, sep="")),
                      collapse=", "),"\n") 
        }
        cat("##", paste(c(x, xExclude), collapse=", "), "\n")
        cat("##", paste(c(u, uExclude), collapse=", "), "\n")
        if (!is.null(z$nu)) {
            cat("##", paste(round(c(nu, nuExclude),1), collapse=", "), "\n")}
    }
    if ((x.rma$QEp <= sizeHetero) &
        (x.sym$p.value <= sizeSym)) {
        cat("## ==>> Hierarchical SkewStudent+Gauss\n") 
        decision = 'hgg'
        
        }
    return(decision)
}

## ## EXAMPLE ====================================================
## 
# PCB28 = data.frame(lab=c("IRMM", "KRISS", "NARL", "NIST", "NMIJ", "NRC"),
#     w=c(34.3, 32.9, 34.53, 32.42, 31.9, 35.8),
#     uw=c(1.03, 0.69, 0.83, 0.29, 0.4, 0.38),
#     nu=c(60, 4, 18, 2, 13, 60))
## 
## DecisionTree(PCB28)
## DecisionTree(PCB28, exclude="NRC")

######################################################################
## ================================================================ ##
######################################################################
