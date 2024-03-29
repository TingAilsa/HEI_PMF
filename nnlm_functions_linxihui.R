
### https://github.com/linxihui/NNLM/tree/master/R

#### 1. NNLM ####

#' Non-negative linear model/regression (NNLM)
#'
#' Solving non-negative linear regression problem as \cr
#' 		\deqn{argmin_{\beta \ge 0} L(y - x\beta) + \alpha_1 ||\beta||_2^2 +
#' 		\alpha_2 \sum_{i < j} \beta_{\cdot i}^T \beta_{\cdot j}^T + \alpha_3 ||\beta||_1}
#' 	where \eqn{L} is a loss function of either square error or Kullback-Leibler divergence.
#'
#' The linear model is solve in column-by-column manner, which is parallelled. When \eqn{y_{\cdot j}} (j-th column) contains missing values,
#' only the complete entries are used to solve \eqn{\beta_{\cdot j}}. Therefore, the minimum complete entries of each column should be
#' not smaller than number of columns of \code{x} when penalty is not used.
#'
#' \code{method = 'scd'} is recommended, especially when the solution is probably sparse. Though both "mse" and "mkl" loss are supported for
#' non-negative \code{x} and \code{y}, only "mse" is proper when either \code{y} or \code{x} contains negative value. Note that loss "mkl"
#' is much slower then loss "mse", which might be your concern when \code{x} and \code{y} is extremely large.
#'
#' \code{mask} is can be used for hard regularization, i.e., forcing entries to their initial values (if \code{init} specified) or 0 (if
#' \code{init} not specified). Internally, \code{mask} is achieved by skipping the masked entries during the element-wse iteration.
#'
#' @param x             Design matrix
#' @param y             Vector or matrix of response
#' @param alpha         A vector of non-negative value length equal to or less than 3, meaning [L2, angle, L1] regularization on \code{beta}
#'                      (non-masked entries)
#' @param method        Iteration algorithm, either 'scd' for sequential coordinate-wise descent or 'lee' for Lee's multiplicative algorithm
#' @param loss          Loss function to use, either 'mse' for mean square error or 'mkl' for mean KL-divergence. Note that if \code{x}, \code{y}
#'                      contains negative values, one should always use 'mse'
#' @param init          Initial value of \code{beta} for iteration. Either NULL (default) or a non-negative matrix of
#' @param mask          Either NULL (default) or a logical matrix of the same shape as \code{beta}, indicating if an entry should be fixed to its initial
#'                      (if \code{init} specified) or 0 (if \code{init} not specified).
#' @param check.x       If to check the condition number of \code{x} to ensure unique solution. Default to \code{TRUE} but could be slow
#' @param max.iter      Maximum number of iterations
#' @param rel.tol       Stop criterion, relative change on x between two successive iteration. It is equal to \eqn{2*|e2-e1|/(e2+e1)}.
#'                      One could specify a negative number to force an exact \code{max.iter} iteration, i.e., not early stop
#' @param n.threads     An integer number of threads/CPUs to use. Default to 1 (no parallel). Use 0 or a negative value for all cores
#' @param show.warning  If to shown warnings if exists. Default to TRUE
#'
#' @return An object of class 'nnlm', which is a list with components \itemize{
#' 	\item coefficients : a matrix or vector (depend on y) of the NNLM solution, i.e., \eqn{\beta}
#' 	\item n.iteration  : total number of iteration (sum over all column of \code{beta})
#' 	\item error        : a vector of errors/loss as c(MSE, MKL, target.error) of the solution
#' 	\item options      : list of information of input arguments
#' 	\item call         : function call
#' 	}
#'
#' @references
#'
#' Franc, V. C., Hlavac, V. C., Navara, M. (2005). Sequential Coordinate-Wise Algorithm for the Non-negative Least Squares Problem.
#' Proc. Int'l Conf. Computer Analysis of Images and Patterns. Lecture Notes in Computer Science 3691. p. 407.
#'
#' Lee, Daniel D., and H. Sebastian Seung. 1999. "Learning the Parts of Objects by Non-Negative Matrix Factorization."
#' Nature 401: 788-91.
#'
#' Pascual-Montano, Alberto, J.M. Carazo, Kieko Kochi, Dietrich Lehmann, and Roberto D.Pascual-Marqui. 2006.
#' "Nonsmooth Nonnegative Matrix Factorization (NsNMF)." IEEE Transactions on Pattern Analysis and Machine Intelligence 28 (3): 403-14.
#'
#' @author Eric Xihui Lin, \email{xihuil.silence@@gmail.com}
#' @examples
#'
#' # without negative value
#' x <- matrix(runif(50*20), 50, 20);
#' beta <- matrix(rexp(20*2), 20, 2);
#' y <- x %*% beta + 0.1*matrix(runif(50*2), 50, 2);
#' beta.hat <- nnlm(x, y, loss = 'mkl');
#'
#' # with negative values
#' x2 <- 10*matrix(rnorm(50*20), 50, 20);
#' y2 <- x2 %*% beta + 0.2*matrix(rnorm(50*2), 50, 2);
#' beta.hat2 <- nnlm(x, y);
#'
#' @export
nnlm <- function(x, y, alpha = rep(0, 3), method = c('scd', 'lee'),
                 loss = c('mse', 'mkl'), init = NULL, mask = NULL, check.x = TRUE,
                 max.iter = 10000L, rel.tol = 1e-12, n.threads = 1L,
                 show.warning = TRUE) {
  
  method <- match.arg(method);
  loss <- match.arg(loss);
  
  if (show.warning && 'mkl' == loss && (any(x < 0) || any(y < 0)))
    warning("x or y have negative values. One should instead use method == 'mse'.");
  
  is.y.vector <- is.vector(y) && is.atomic(y) && is.numeric(y);
  y <- as.matrix(y);
  check.matrix(y, check.na = FALSE);
  check.matrix(x, check.na = TRUE);
  
  if (nrow(x) != nrow(y))
    stop("Dimensions of x and y do not match.");
  
  if (!is.double(x)) storage.mode(x) <- 'double';
  if (!is.double(y)) storage.mode(y) <- 'double';
  
  if (max.iter <= 0L) stop("max.iter must be positive.");
  if (n.threads < 0L) n.threads <- 0L; # use all free cores
  
  if (check.x) {
    if (nrow(x) < ncol(x) || rcond(x) < .Machine$double.eps)
      warning("x does not have a full column rank. Solution may not be unique.");
  }
  
  alpha <- c(alpha, rep(0, 3))[1:3];
  if (show.warning && alpha[1] < alpha[2])
    warning("If alpha[1] < alpha[2], be aware that that algorithm may not converge or unique.");
  
  check.matrix(mask, dm = c(ncol(x), ncol(y)), mode = 'logical', check.na = TRUE);
  check.matrix(init, dm = c(ncol(x), ncol(y)), check.na = TRUE, check.negative = TRUE);
  if (length(mask) == 0)
    mask <- matrix(FALSE, 0, ncol(y));
  if (length(init) == 0)
    init <- matrix(0.0, 0, ncol(y));
  # if masked but no initialized, masked entries are thought to fix to 0
  if (length(mask) != 0 && length(init) == 0)
    init <- as.double(!mask);
  if (!is.double(init))
    storage.mode(init) <- 'double';
  
  method.code <- get.method.code(method, loss);
  
  # x, y are passed to C++ function by reference (const arma::mat & type)
  sol <- c_nnlm(x, y, as.double(alpha), mask, init, as.integer(max.iter),
                as.double(rel.tol), as.integer(n.threads), method.code);
  
  names(sol) <- c('coefficients', 'n.iteration');
  if (!is.null(colnames(x)))
    rownames(sol$coefficients) <- colnames(x);
  if (!is.null(colnames(y)))
    colnames(sol$coefficients) <- colnames(y);
  if (is.y.vector)
    sol$coefficients <- sol$coefficients[, seq_len(ncol(sol$coefficients)), drop = TRUE];
  
  error <- mse.mkl(y, x %*% sol$coefficients, na.rm = TRUE, show.warning = FALSE);
  N.complete <- length(y) - sum(is.na(y));
  
  target.error <- switch(loss,
                         'mse' = unname(0.5*error[1]),
                         'mkl' = unname(error[2]));
  
  target.error <- target.error + (alpha[1] - alpha[2])*sum(sol$coefficients^2) +
    alpha[2]*sum(colSums(sol$coefficients)^2) + alpha[3]*sum(sol$coefficients);
  
  sol$error <- c(error, 'target.error' = target.error);
  sol$options <- list('method' = method, 'loss' = loss, 'max.iter' = max.iter, 'rel.tol' = rel.tol);
  sol$call <- match.call();
  
  return(structure(sol, class = 'nnlm'));
}


#### 2. RcppExports ####

# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

c_nnlm <- function(x, y, alpha, mask, beta0, max_iter, rel_tol, n_threads, method) {
  .Call(`_NNLM_c_nnlm`, x, y, alpha, mask, beta0, max_iter, rel_tol, n_threads, method)
}

c_nnmf <- function(A, k, W, H, Wm, Hm, alpha, beta, max_iter, rel_tol, n_threads, verbose, show_warning, inner_max_iter, inner_rel_tol, method, trace) {
  .Call(`_NNLM_c_nnmf`, A, k, W, H, Wm, Hm, alpha, beta, max_iter, rel_tol, n_threads, verbose, show_warning, inner_max_iter, inner_rel_tol, method, trace)
}


##### 3. misc #######

#' Compute mean square error(MSE) and mean kL divergence (MKL)
#' 
#' @param obs          observed value
#' @param pred         prediction/estimate
#' @param na.rm        if to remove NAs
#' @param show.warning if to show warning if any
#' @return A vector of c(MSE, MKL)
#' @export
mse.mkl <- function(obs, pred, na.rm = TRUE, show.warning = TRUE) {
  mkl <- ifelse(
    !show.warning && (any(obs < 0) || any(pred < 0)), NaN, 
    mean((obs + 1e-16)*log((obs+1e-16) / (pred+1e-16)) - obs + pred, na.rm = na.rm)
  );
  mse <- mean((obs - pred)^2, na.rm = na.rm)
  return(c(MSE = mse, MKL = mkl));
}

# Get method code passed to C++ functions
# 	1 = "scd" + "mse"
# 	2 = "lee" + "mse"
# 	3 = "scd" + "mkl"
# 	4 = "lee" + "mkl"
#
# @param method Either sequential coordinate-wise descent (SCD) or Lee's multiplicative algorithm
# @param loss Loss function, either mean square error (MSE) or mean KL-divergence (MKL)
# @return method code from 1L - 4L
#
get.method.code <- function(method = c('scd', 'lee'), loss = c('mse', 'mkl')) {
  method <- match.arg(method);
  loss <- match.arg(loss);
  code <- 1L;
  if ('mkl' == loss) code <- code + 2L;
  if ('lee' == method) code <- code + 1L;
  return(code);
}


check.matrix <- function(A, dm = NULL, mode = 'numeric', check.na = FALSE, input.name = '', check.negative = FALSE) {
  if (is.null(A)) return(invisible(NULL));
  if (!is.null(dm) && any(dim(A) != dm, na.rm = TRUE))
    stop(sprintf("Dimension of matrix %s is expected to be (%d, %d), but got (%d, %d)", input.name, nrow(A), ncol(A), dm[1], dm[2]));
  if (mode(A) != mode) stop(sprintf("Matrix %s must be %s.", input.name, mode));
  if (check.negative && any(A[!is.na(A)] < 0)) stop(sprintf("Matrix %s must be non-negative.", input.name));
  if (check.na && any(is.na(A))) stop(sprintf("Matrix %s contains missing values.", input.name));
}


reformat.input <- function(init, mask, n, m, k) {
  if (is.null(mask)) mask <- list();
  if (is.null(init)) init <- list();
  stopifnot(is.list(mask));
  stopifnot(is.list(init));
  
  known.W <- !is.null(init[['W0']]);
  known.H <- !is.null(init[['H0']]);
  kW0 <- kH0 <- 0;
  
  is.empty <- function(x) 0 == length(x);
  
  if (known.W) {
    if(!is.matrix(init[['W0']]))
      init[['W0']] <- as.matrix(init[['W0']]);
    kW0 <- ncol(init[['W0']]);
    mask[['W0']] <- matrix(TRUE, n, kW0);
  }
  else {
    mask[['W0']] <- NULL;
    mask[['H1']] <- NULL;
    init[['H1']] <- NULL;
  }
  
  if (known.H) {
    if(!is.matrix(init$H0))
      init[['H0']] <- as.matrix(init[['H0']]);
    kH0 <- nrow(init[['H0']]);
    mask[['H0']] <- matrix(TRUE, kH0, m);
  }
  else {
    mask[['H0']] <- NULL;
    mask[['W1']] <- NULL;
    init[['W1']] <- NULL;
  }
  
  K <- k + kW0 + kH0;
  
  ew <- !all(sapply(mask[c('W', 'W0', 'W1')], is.empty));
  eh <- !all(sapply(mask[c('H', 'H0', 'H1')], is.empty));
  dim.mask <- list(
    'W' = c(n, k*ew), 'W0' = c(n, kW0*ew), 'W1' = c(n, kH0*ew), 
    'H' = c(k*eh, m), 'H1' = c(kW0*eh, m), 'H0' = c(kH0*eh, m)
  );
  
  for (mat in c('W', 'W0', 'W1', 'H' ,'H0', 'H1')) {
    check.matrix(mask[[mat]], dim.mask[[mat]], 'logical', TRUE, paste0('mask$', mat));
    if (is.empty(mask[[mat]]))
      mask[[mat]] <- matrix(FALSE, dim.mask[[mat]][[1]], dim.mask[[mat]][[2]]);
  }
  
  ew <- !all(sapply(init[c('W', 'W0', 'W1')], is.empty));
  eh <- !all(sapply(init[c('H', 'H0', 'H1')], is.empty));
  dim.init <- list(
    'W' = c(n, k*ew), 'W0' = c(n, kW0*ew), 'W1' = c(n, kH0*ew), 
    'H' = c(k*eh, m), 'H1' = c(kW0*eh, m), 'H0' = c(kH0*eh, m)
  );
  for (mat in c('W', 'W0', 'W1', 'H' ,'H0', 'H1')) {
    check.matrix(init[[mat]], dim.init[[mat]], 'numeric', TRUE, paste0('init$', mat));
    if (is.empty(init[[mat]])) {
      init[[mat]] <- matrix(
        runif(prod(dim.init[[mat]])),
        dim.init[[mat]][[1]],
        dim.init[[mat]][[2]]
      );
      #init[[mat]][mask[[mat]]] <- 0;
    }
    if (!is.double(init[[mat]]))
      storage.mode(init[[mat]]) <- 'double';
  }
  
  return(
    list(
      Wm = do.call(cbind, mask[c('W', 'W0', 'W1')]),
      Hm = do.call(rbind, mask[c('H', 'H1', 'H0')]),
      Wi = do.call(cbind, init[c('W', 'W0', 'W1')]),
      Hi = do.call(rbind, init[c('H', 'H1', 'H0')]),
      kW0 = kW0,
      kH0 = kH0,
      K = K
    ));
}


##### 4. nnmf #######
#' Non-negative matrix factorization
#'
#' Non-negative matrix factorization(NMF or NNMF) using sequential coordinate-wise descent or multiplicative updates
#'
#' The problem of non-negative matrix factorization is to find \eqn{W, H, W_1, H_1}, such that \cr
#' \deqn{A = W H + W_0 H_1 + W_1 H_0 + \varepsilon = [W W_0 W_1] [H' H'_1 H'_0]' + \varepsilon}\cr
#' where \eqn{W_0}, \eqn{H_0} are known matrices, which are NULLs in most application case and \eqn{\varepsilon} is noise..
#' In tumor content deconvolution, \eqn{W_0} can be thought as known healthy profile, and \eqn{W}
#' is desired pure cancer profile. One also set \eqn{H_0} to a row matrix of 1, and thus \eqn{W_1}
#' can be treated as common profile among samples. Use \code{init} to specify \eqn{W_0} and \eqn{H_0}.
#'
#' Argument \code{init}, if used, must be a list with entries named as 'W', 'H', 'W0', 'W1', 'H1', 'H0'.
#' One could specify only a few of them. Only use 'W0' (and its correspondent 'H1') or 'H0' (and its correspondent 'W1')
#' for known matrices/profiles.
#'
#' Similarly, argument \code{mask}, if used, must be a list entries named as 'W', 'H', 'W0', 'W1', 'H1', 'H0',
#' and they should be either NULL (no specified) or a logical matrix. If a masked for matrix is specified, then
#' masked entries will be fixed to their initial values if initialized (skipped during iteration), or 0 if not
#' initialized.
#'
#' To simplify the notations, we denote right hand side of the above equation as \eqn{W H}.
#' The problem to solved using square error is\cr
#' \deqn{argmin_{W \ge 0, H \ge 0} L(A, W H) + J(W, \alpha) + J(H', \beta)}\cr
#' where \eqn{L(x, y)} is a loss function either a square loss
#' \deqn{\frac{1}{2} ||x-y||_2^2}
#' or a Kullback-Leibler divergence
#' \deqn{x \log (x/y) - x - y,}
#'
#' and
#' \deqn{J(X, \alpha) = \alpha_1 J_1(X) + \alpha_2 J_2(X) + \alpha_3 J_3(X),}
#' \deqn{J_1(X) = \frac12 ||X||_F^2 = \frac{1}{2} tr(XX^T),}
#' \deqn{J_2(X) = \sum_{i < j} (X_{\cdot i})^TX_{\cdot j} = \frac12 tr(X(E-I)X^T),}
#' \deqn{J_3(X) = \sum_{i,j} |x_{ij}| = tr(XE).}
#'
#' The formal one is usually better for symmetric distribution, while
#' the later one is more suitable for skewed distribution, especially for count data as it can be derived from
#' Poisson distributed observation. The penalty function \eqn{J} is a composition of three types of penalties,
#' which aim to minimizing L2 norm, maximizing angles between hidden features (columns of W and rows of H) and
#' L1 norm (sparsity).  The parameters \eqn{\alpha}, \eqn{\beta} of length 3 indicates the amount of penalties.
#'
#' When \code{method == 'scd'}, a sequential coordinate-wise descent algorithm is used when solving \eqn{W}
#' and \eqn{H} alternatively, which are non-negative regression problem. The \code{inner.max.iter} and
#' \code{inner.rel.tol} is used to control the number of iteration for these non-negative regressions.
#' This is also applicable to \code{method == 'lee'} (the original algorithm only iteration through all entries
#' once for each iteration), which is usually faster than the original algorithm when \code{loss == 'mse'}.
#' When \code{loss == 'mkl'}, a quadratic approximation to the KL-divergence is used when \code{method == 'scd'}.
#' Generally, for run time, 'scd' is faster than 'lee' and 'mse' is faster than 'mkl'.
#'
#' @param A              A matrix to be decomposed
#' @param k              An integer of decomposition rank
#' @param alpha          [L2, angle, L1] regularization on W (non-masked entries)
#' @param beta           [L2, angle, L1] regularization on H (non-masked entries)
#' @param method         Decomposition algorithms, either 'scd' for sequential coordinate-wise descent(default)
#'                       or 'lee' for Lee's multiplicative algorithm
#' @param loss           Loss function to use, either 'mse' for mean square error (default) or 'mkl' for mean KL-divergence
#' @param init           A list of initial matrices for W and H. One can also supply known matrices W0, H0, and initialize
#'                       their correspondent matrices H1 and W1. See details
#' @param mask           A list of mask matrices for W, H, H1 (if \code{init$W0} supplied), W1 (if \code{init$H0} supplied), which
#'                       should have the same shapes as W, H, H1 and W1 if specified. If initial matrices not specified, masked entries
#'                       are fixed to 0. See details
#' @param W.norm         A numeric value 'p' indicating the \eqn{L_p}-norm (can be infinity) used to normalized the outcome \code{W} matrix.
#'                       No normalization will be performed if 0 or negative. This argument has no effect on outcome correspondent to
#'                       known profiles \code{W0, H0}. Default to 1, i.e. sum of W should sum up to 1, which can be interpreted as
#'                       "distribution" or "proportion"
#' @param check.k        If to check whether \eqn{k \le nm/(n+m)}, where (n,m)=dim(A), or k is smaller than the column-wise
#'                       and row-wise minimum numbers of complete observation
#' @param max.iter       Maximum iteration of alternating NNLS solutions to H and W
#' @param rel.tol        Stop criterion, which is relative tolerance between two successive iterations, = |e2-e1|/avg(e1, e2)
#' @param n.threads      An integer number of threads/CPUs to use. Default to 1(no parallel). Specify 0 for all cores
#' @param trace          An integer indicating how frequent the error should be checked. MSE and MKL error will computed every
#'                       \code{trace} iterations. If 0 or negative, trace is set to a very large number and only errors of the
#'                       first and last iterations are checked.
#' @param verbose        Either 0/FALSE, 1/TRUE or 2, with 0/FALSE/else = no any tracking, 1 == progression bar, 2 == print iteration info.
#' @param show.warning   If to show warnings when targeted \code{rel.tol} is not reached
#' @param inner.max.iter Maximum number of iterations passed to each inner W or H matrix updating loop
#' @param inner.rel.tol  Stop criterion for the inner loop, which is relative tolerance passed to inner W or H matrix updating
#'                       i.e., |e2-e1|/avg(e1, e2)
#' @return A list with components
#' 	\itemize{
#' 		\item W              : left matrix, including known W0 and W1 if available, i.e., column stacked as \eqn{[W, W0, W1]}
#' 		\item H              : right matrix, including H1 and known H0 if available, i.e. row stacked as \eqn{[H', H1', H0']'}
#' 		\item mse            : a vector of mean squared errors through iterations
#' 		\item mkl            : a vector of mean KL-divergence through iterations
#' 		\item target.loss    : target for minimization, which is mean KL-divergence (if \code{loss == 'mkl'}) or half of mean squared error
#'                             if \code{loss == 'mse'} plus penalties
#' 		\item average.epochs : a vector of average epochs (one complete swap over W and H)
#' 		\item n.iteration    : total number of iteration (sum over all column of \code{beta})
#' 		\item run.time       : running time
#' 		\item options        : list of information of input arguments
#' 		\item call           : function call
#' 	}
#'
#' @references
#'
#' Franc, V. C., Hlavac, V. C., Navara, M. (2005). Sequential Coordinate-Wise Algorithm for the Non-negative Least Squares Problem.
#' Proc. Int'l Conf. Computer Analysis of Images and Patterns. Lecture Notes in Computer Science 3691. p. 407.\cr
#' \cr
#' Lee, Daniel D., and H. Sebastian Seung. 1999. "Learning the Parts of Objects by Non-Negative Matrix Factorization."
#' Nature 401: 788-91.\cr
#' \cr
#' Pascual-Montano, Alberto, J.M. Carazo, Kieko Kochi, Dietrich Lehmann, and Roberto D.Pascual-Marqui. 2006.
#' "Nonsmooth Nonnegative Matrix Factorization (NsNMF)." IEEE Transactions on Pattern Analysis and Machine Intelligence 28 (3): 403-14.\cr
#'
#' @author Eric Xihui Lin, \email{xihuil.silence@@gmail.com}
#' @seealso \code{\link{nnlm}}, \code{\link{predict.nnmf}}
#'
#' @examples
#'
#' # Pattern extraction, meta-gene
#' set.seed(123);
#'
#' data(nsclc, package = 'NNLM')
#' str(nsclc)
#'
#' decomp <- nnmf(nsclc[, 1:80], 3, rel.tol = 1e-5);
#'
#' heatmap(decomp$W, Colv = NA, xlab = 'Meta-gene', ylab = 'Gene', margins = c(2,2),
#' 	labRow = '', labCol = '', scale = 'column', col = cm.colors(100));
#' heatmap(decomp$H, Rowv = NA, ylab = 'Meta-gene', xlab = 'Patient', margins = c(2,2),
#' 	labRow = '', labCol = '', scale = 'row', col = cm.colors(100));
#'
#' # missing value imputation
#' set.seed(123);
#' nsclc2 <- nsclc;
#' index <- sample(length(nsclc2), length(nsclc2)*0.3);
#' nsclc2[index] <- NA;
#'
#' # impute using NMF
#' system.time(nsclc2.nmf <- nnmf(nsclc2, 2));
#' nsclc2.hat.nmf <- with(nsclc2.nmf, W %*% H);
#'
#' mse.mkl(nsclc[index], nsclc2.hat.nmf[index])
#'
#' @export
nnmf <- function(
    A, k = 1L, alpha = rep(0,3), beta = rep(0,3), method = c('scd', 'lee'),
    loss = c('mse', 'mkl'), init = NULL, mask = NULL, W.norm = -1L, check.k = TRUE,
    max.iter = 500L, rel.tol = 1e-4, n.threads = 1L, trace = 100/inner.max.iter,
    verbose = 1L, show.warning = TRUE, inner.max.iter = ifelse('mse' == loss, 50L, 1L),
    inner.rel.tol = 1e-9
) {
  method <- match.arg(method);
  loss <- match.arg(loss);
  check.matrix(A, input.name = 'A');
  if (!is.double(A))
    storage.mode(A) <- 'double';
  n <- nrow(A);
  m <- ncol(A);
  
  init.mask <- reformat.input(init, mask, n, m, k);
  k <- init.mask$K;
  
  alpha <- c(as.double(alpha), rep(0., 3))[1:3];
  beta <- c(as.double(beta), rep(0., 3))[1:3];
  method.code <- get.method.code(method, loss);
  
  min.k <- min(dim(A));
  A.isNA <- is.na(A);
  A.anyNA <- any(A.isNA); # anyNA is depreciated in new version of R
  if (A.anyNA) {
    min.k <- min(min.k, ncol(A) - rowSums(A.isNA), nrow(A) - colSums(A.isNA));
  }
  rm(A.isNA);
  if (check.k && k > min.k && all(c(alpha, beta) == 0))
    stop(paste("k larger than", min.k, "is not recommended, unless properly masked or regularized.
				Set check.k = FALSE if you want to skip this checking."));
  
  if (n.threads < 0L) n.threads <- 0L; # let openMP decide
  if (is.logical(verbose)) {
    verbose <- as.integer(verbose);
  }
  if (trace <= 0) {
    trace <- 999999L; # only compute error of the 1st and last iteration
  }
  
  run.time <- system.time(
    out <- c_nnmf(A, as.integer(k),
                  init.mask$Wi, init.mask$Hi, init.mask$Wm, init.mask$Hm,
                  alpha, beta, as.integer(max.iter), as.double(rel.tol),
                  as.integer(n.threads), as.integer(verbose), as.logical(show.warning),
                  as.integer(inner.max.iter), as.double(inner.rel.tol), as.integer(method.code),
                  as.integer(trace))
  );
  names(out) <- c('W', 'H', 'mse', 'mkl', 'target.loss', 'average.epochs', 'n.iteration');
  out$mse <- as.vector(out$mse);
  out$mkl <- as.vector(out$mkl);
  out$target.loss <- as.vector(out$target.loss);
  out$average.epochs <- as.vector(out$average.epochs);
  
  # add row/col names back
  colnames(out$W) <- colnames(init.mask$Wi);
  rownames(out$H) <- rownames(init.mask$Hi);
  if (!is.null(rownames(A))) rownames(out$W) <- rownames(A);
  if (!is.null(colnames(A))) colnames(out$H) <- colnames(A);
  rm(init.mask);
  
  if (W.norm > 0) {
    if (is.finite(W.norm)) {
      W.scale <- apply(out$W, 2, function(x) sum(x^W.norm)^(1./W.norm));
    } else {
      W.scale <- apply(out$W, 2, max);
    }
    out$W <- out$W %*% diag(1./W.scale);
    out$H <- diag(W.scale) %*% out$H
  }
  
  out$run.time <- run.time;
  out$options <- list(
    method = method,
    loss = loss,
    alpha = alpha,
    beta = beta,
    init = init,
    mask = mask,
    n.threads = n.threads,
    trace = trace,
    verbose = verbose,
    max.iter = max.iter,
    rel.tol = rel.tol,
    inner.max.iter = inner.max.iter,
    inner.rel.tol = inner.rel.tol
  );
  out$call <- match.call();
  return(structure(out, class = 'nnmf'));
}

##### 5. nnmf_methods #######

#' Methods for nnmf object returned by \code{nnmf}
#'
#' @param object        An NNMF object returned by \code{\link{nnmf}}
#' @param newdata       A new matrix of x. No required when \code{which == 'A'}
#' @param which         Either 'A' (default), 'W' or 'H'
#' @param method        Either 'scd' or 'lee'. Default to \code{object$options$method}
#' @param loss          Either 'mse' or 'mkl'. Default to \code{object$options$loss}
#' @param x             An NNMF object returned by \code{\link{nnmf}}
#' @param ...           Further arguments passed to 'nnlm' or 'print'
#' @return 'A' or a class of 'nnlm' for 'predict.nnmf' and no return for 'print'.
#'
#' @examples
#'
#' x <- matrix(runif(50*20), 50, 20)
#' r <- nnmf(x, 2)
#' r
#' newx <- matrix(runif(50*30), 50, 30)
#' pred <- predict(r, newx, 'H')
#'
#' @seealso \code{\link{nnmf}}, \code{\link{nnlm}}
#' @export
predict.nnmf <- function(
    object, newdata, which = c('A', 'W', 'H'),
    method = object$options$method,
    loss = object$options$loss,
    ...) {
  
  which <- match.arg(which);
  if (which != 'A') {
    if('W' == which)
      check.matrix(newdata, dm = c(NA, ncol(object$H)));
    if('H' == which)
      check.matrix(newdata, dm = c(nrow(object$W),NA ));
    if (!is.double(newdata))
      storage.mode(newdata) <- 'double';
  }
  
  out <- switch(which,
                'A' = object$W %*% object$H,
                'W' = nnlm(t(object$H), t(newdata), method = method, loss = loss, ...),
                'H' = nnlm(object$W, newdata, method = method, loss = loss, ...)
  );
  
  if ('W' == which)
    out$coefficients <- t(out$coefficients);
  
  return(out);
}


#' @rdname predict.nnmf
#' @export
print.nnmf <- function(x, ...) {
  if (x$n.iteration < 2) {
    rel.tol <- NA_real_;
  } else {
    err <- tail(x$target.loss, 2);
    rel.tol <- diff(err)/mean(err); 
  }
  cat("Non-negative matrix factorization:\n")
  if (x$options$method == 'scd') {
    cat("   Algorithm: Sequential coordinate-wise descent\n");
  } else {
    cat("   Algorithm: Lee's multiplicative algorithm\n");
  }
  if (x$options$loss == 'mse') {
    cat("        Loss: Mean squared error\n");
  } else {
    cat("        Loss: Mean Kullback-Leibler divergence\n");
  }
  cat("         MSE: ", tail(x$mse, 1), '\n', sep = '');
  cat("         MKL: ", tail(x$mkl, 1), '\n', sep = '');
  cat("      Target: ", tail(x$target.loss, 1), '\n', sep = '');
  cat("   Rel. tol.: ", sprintf("%.3g", abs(rel.tol)), '\n', sep = '');
  cat("Total epochs: ", as.integer(sum(x$average.epochs)), '\n', sep = '');
  cat("# Interation: ", x$n.iteration, '\n', sep = '');
  cat("Running time:\n");
  print(x$run.time);
  invisible(NULL);
}

##### 6. others #######

# #' Fast NNLS and NNMF
# #'
# #' This package is built for fast non-negative least square (NNLS) regression and non-negative matrix factorization (NNMF).
# #'
# #' @docType package
# #' @name NNLM
# NULL

#' @import Rcpp
#' @importFrom stats runif
#' @importFrom utils tail
#' @useDynLib NNLM, .registration = TRUE
#' 
#' 

#' Micro-array data of NSCLC patients
#'
#' This dataset is a random subset (matrix) of micro-array data from a group of Non-Small Cell Lung Caner (NSCLC) patients.
#' It contains 200 probes / genes (row) for 100 patients / samples (column).
#'
#' @name nsclc
#' @docType data
#' @keywords data



