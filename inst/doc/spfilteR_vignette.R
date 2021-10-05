## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE, results = "hide", eval=FALSE--------------
#  # CRAN release
#  install.packages("spfilteR")
#  
#  # GitHub
#  library(devtools)
#  devtools::install_github("sjuhl/spfilteR")

## ----message=FALSE, warning=FALSE---------------------------------------------
# load the package
library(spfilteR)

# load the dataset
data("fakedata")

# take a look at the connectivity matrix and the variables
dim(W)

head(fakedataset)

## -----------------------------------------------------------------------------
# select continuous variables
cont <- cbind(fakedataset$x1, fakedataset$x2, fakedataset$x3, fakedataset$negative)
colnames(cont) <- c("x1", "x2", "x3", "negative")

# Moran test of spatial autocorrelation
(I <- MI.vec(x = cont, W = W, alternative = 'greater'))

## -----------------------------------------------------------------------------
MI.vec(x = fakedataset$negative, W = W, alternative = 'lower')

## -----------------------------------------------------------------------------
# decompose Moran's I
(I.dec <- MI.decomp(x = cont, W = W, nsim=100))

## -----------------------------------------------------------------------------
# I = 'I+' + 'I-'
cbind(I[, "I"], I.dec[, "I+"] + I.dec[, "I-"])

## -----------------------------------------------------------------------------
# define variables
y <- fakedataset$x1
X <- cbind(1, fakedataset$x2)

# OLS regression
ols.resid <- resid(lm(y ~ X))

# Moran test of residual spatial autocorrelation
MI.resid(resid = ols.resid, W = W, alternative = 'greater')

## -----------------------------------------------------------------------------
# ESF model
(lm.esf <- lmFilter(y = y, x = X, W = W, objfn = 'MI', positive = TRUE,
                    ideal.setsize = TRUE, tol = .2))

summary(lm.esf, EV = TRUE)

## ----fig_out, out.width = '50%', fig.align='center'---------------------------
plot(lm.esf)

## -----------------------------------------------------------------------------
### Alternative selection criteria:
# maximization of model fit
lmFilter(y = y, x = X, W = W, objfn = 'R2', positive = TRUE, ideal.setsize = TRUE)

# significance of residual autocorrelation
lmFilter(y = y, x = X, W = W, objfn = 'pMI', sig = .1, bonferroni = FALSE,
         positive = TRUE, ideal.setsize = TRUE)

# significance of eigenvectors
lmFilter(y = y, x = X, W = W, objfn = 'p', sig = .1, bonferroni = TRUE,
         positive = TRUE, ideal.setsize = TRUE)

# all eigenvectors in the candidate set
lmFilter(y = y, x = X, W = W, objfn = 'all', positive = TRUE, ideal.setsize = TRUE)

## -----------------------------------------------------------------------------
# define outcome variables
y.bin <- fakedataset$indicator
y.count <- fakedataset$count

# ESF logit model
(logit.esf <- glmFilter(y = y.bin, x = NULL, W = W, objfn = 'p', model = 'logit',
                        optim.method = 'BFGS', sig = .1, bonferroni = FALSE,
                        positive = TRUE, ideal.setsize = FALSE, alpha = .25,
                        resid.type = 'deviance', boot.MI = 100))

# ESF probit model
(probit.esf <- glmFilter(y = y.bin, x = NULL, W = W, objfn = 'p', model = 'probit',
                         optim.method = 'BFGS', sig = .1, bonferroni = FALSE,
                         positive = TRUE, ideal.setsize = FALSE, alpha = .25,
                         resid.type = 'deviance', boot.MI = 100))

# ESF poisson model
(poisson.esf <- glmFilter(y = y.count, x = NULL, W = W, objfn = 'BIC', model = 'poisson',
                          optim.method = 'BFGS', positive = TRUE, ideal.setsize = FALSE,
                          alpha = .25, resid.type = 'deviance', boot.MI = 100))

