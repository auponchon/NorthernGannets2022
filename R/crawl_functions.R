######################################################################################
## Function to remove duplicated date/time values with a 50s buffer
######################################################################################

make_unique <- function(x) {
    xts::make.time.unique(x$datetime,eps = 30)
}



######################################################################################
## Function to fit the crawl model
######################################################################################
fit_crawl <- function(d, fixpar,seed=T) {
    
    ## if relying on a prior for location quality
    ## replace this with the function described previously
    prior <- function(p) {
        dnorm(p[2], -4, 2, log = TRUE)
    } 
    
    fit <- crawl::crwMLE(
        mov.model =  ~ 1,
        err.model = list(
            x =  ~ ln.sd.x - 1,
            y =  ~ ln.sd.y - 1,
            rho =  ~ error.corr
        ),
        if (any(colnames(d) == "activity")) {
            activity <- ~ I(activity)
        } else {activity <- NULL},
        fixPar = fixpar,
        data = d,
        method = "Nelder-Mead",
        Time.name = "datetime",
        prior = prior,
        attempts = 8,
        control = list(
            trace = 0
        ),
        initialSANN = list(
            maxit = 1500,
            trace = 0
        )
    )
    fit
}
