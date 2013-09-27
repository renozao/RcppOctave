<<setup, include=FALSE>>=
library(knitr)
knit_theme$set('edit-emacs')
@

\title{RcppOctave: Running Octave from R}
\author{by Dirk Eddelbuettel and Renaud Gaujoux}

\maketitle

\abstract{
  The \pkg{RcppOctave} package connects Octave to R, allowing R user to
  access another numerical computing language from within R.
  \dirk{TODO: Expand}
}

\section{Introduction}

Octave \citep{Octave:2012} is an interactive language that is primarily
intended for numerical computations. It is mostly compatible with Matlab
\citep{MATLAB:2010} and has found widespread adoption across different
disciplines. This include domains in which Matlab has historically been
pre-eminent such as applied mathematics, electrical engineering and signal
processing, but also in other fields such as machine learning,
bioinformatics, and finance.  Consequently, a large corpus of application
programs are available as \code{.m}-files (named after the commonly-chose
file extension) for which Octave provides an open source engine.

R \citep{R:2012}, a language and environment for statistical
computing and graphics, has become the dominant language for statistical
research, and a widely-used environment for empirical work in a variety of
fields.

While both languages share commonalities, their respective focus is different
making a combination of both environments an even more compelling choice.
This short paper illustrates the \pkg{RcppOctave} package by
\cite{CRAN:RcppOctave} which implements an interface between both these
environments.
\dirk{TODO: Better split between intro and next section}

\section{Octave}

Octave \citep{Octave:2012} is a powerful interactive language ``not unlike
Matlab'' and described in detail by \citet{Eaton:2008}. Octave can do
arithmetic for real, complex or integer-valued scalars and matrices, solve
sets of nonlinear algebraic equations, integrate functions over finite and
infinite intervals, and integrate systems of ordinary differential and
differential-algebraic equations.  Given the popularity of Matlab, Octave has
always had a lot of appeal in fields in which Matlab is popular.

As a simple illustration, consider the function below which converts two
arguments `distance' (in miles) and `time' (in minutes and seconds) into a
pace expressed as minutes-per-mile:

{ \small
%% by hand:  %% highlight --enclose-pre --no-doc --out-format=latex --syntax=Matlab --style=edit-emacs
<<pace, engine='highlight', engine.opts='-e edit-emacs -S octave -O latex'>>=
## usage:  p = pace (dist, time)
function p = pace (dist, time)
  if (nargin < 2)
    usage("Call as pace (dist, time)")
  end

  ## total seconds
  a = floor(time)*60 + (time-floor(time))*100;
  ## seconds per miles
  b = a / dist;
  ## minutes per mile
  c = floor(b/60);
  ## seconds per mile
  d = b - c*60;

  p = c + d/100;
  printf("%.2f over %.2f is %d:%d aka %.2f\n",
         time, dist, c, d, p);
endfunction
@
}

The example shows a few subtle difference between Octave and Matlab. Comments
starts with hash sign. Default function help is provided before the function
definition. The \code{endfunction} keyword ends a function.

It also shows how the semicolon at the end of lines is used to suppress
output; expression not ending in a semi-colon print their final result.

%\section{Example: Matrix Operations}  % or something else

%TODO: Show two or three simple call illustrating the variable arguments etc.

Octave is particularly useful for linear algebra and calculations involving
matrices and vectors.  For multiplication, the \code{*} symbol is
overloaded. For example, for a row-vector $a$, the expressions \code{a * a'}
amd \code{a' * a} compute, respectively the (scalar) inner product and matrix
outer product as the apostrophe invokes a tranposition. Common matrix
functions such as \code{eig} or \code{det} are available as well.  For more
complete example in provided in the next section.

\renaud{TODO: A few words about RcppOctave, Design, Limitations, ...}

\section{Example: Kalman Filter}

\cite{Eddelbuettel+Sanderson:2013} introduce the \pkg{RcppArmadillo} package
and illustrate it via an example comparing a Kalman filter implementation in both R and
C++. As the code underlying this example was initially published for
Matlab\footnote{See
  \url{http://www.mathworks.com/products/matlab-coder/demos.html?file=/products/demos/shipping/coder/coderdemo_kalman_filter.html}.},
it can of course also be used with RcppOctave.

\dirk{TODO: Few words about the example}

{ \small
%% by hand:  %% highlight --enclose-pre --no-doc --out-format=latex --syntax=Matlab --style=edit-emacs
<<kalman, engine='highlight', engine.opts='-e edit-emacs -S matlab -O latex', results='asis'>>=
function Y = kalmanM(pos)
  dt=1;
  %% Initialize state transition matrix
  A=[ 1 0 dt 0 0 0;...     % [x  ]
      0 1 0 dt 0 0;...     % [y  ]
      0 0 1 0 dt 0;...     % [Vx]
      0 0 0 1 0 dt;...     % [Vy]
      0 0 0 0 1 0 ;...     % [Ax]
      0 0 0 0 0 1 ];       % [Ay]

  % Initialize measurement matrix
  H = [ 1 0 0 0 0 0; 0 1 0 0 0 0 ];

  Q = eye(6);
  R = 1000 * eye(2);

  x_est = zeros(6, 1);
  p_est = zeros(6, 6);

  numPts = size(pos,1);
  Y = zeros(numPts, 2);

  for idx = 1:numPts
    z = pos(idx, :)';

    %% Predicted state and covariance
    x_prd = A * x_est;
    p_prd = A * p_est * A' + Q;
    %% Estimation
    S = H * p_prd' * H' + R;
    B = H * p_prd';
    klm_gain = (S \ B)';
    %% Estimated state and covariance
    x_est = x_prd + klm_gain * (z - H * x_prd);
    p_est = p_prd - klm_gain * H * p_prd;
    %% Compute the estimated measurements
    Y(idx, :) = H * x_est;
  end                % of the function
end   % of the function
@
}

This function, along with several R implementations, is provided in the
\pkg{RcppOctave} package as \code{demo(Kalman)}.
Table~\ref{tab:benchmark} summaries the performance.

%                 test replications elapsed relative
% 7  KalmanOctave(pos)          100   3.943  1.00000
% 2      KalmanRC(pos)          100   5.987  1.51839
% 1       KalmanR(pos)          100   6.047  1.53360
% 4   KalmanRfunC(pos)          100   6.262  1.58813
% 3    KalmanRfun(pos)          100   6.648  1.68603
% 6 FirstKalmanRC(pos)          100   8.992  2.28050
% 5  FirstKalmanR(pos)          100   9.405  2.38524

\begin{table}[tb]
  \begin{center}
    \begin{small}
      \begin{tabular}{lrr}
        \toprule
        {\bf Implementation \phantom{XX}} & {\bf Time in sec.} & {\bf Rel.~ to best} \\
        \cmidrule(r){1-3}
        %% original times, Summer 2012
        %%  KalmanOctave  & 3.943 & 1.000 \\
        %%      KalmanRC  & 5.987 & 1.518 \\
        %%       KalmanR  & 6.047 & 1.534 \\
        %%   KalmanRfunC  & 6.262 & 1.588 \\
        %%    KalmanRfun  & 6.648 & 1.686 \\
        %% FirstKalmanRC  & 8.992 & 2.281 \\
        %%  FirstKalmanR  & 9.405 & 2.385 \\
         KalmanOctave  & 1.93 & 1.00 \\
          KalmanRfunC  & 4.99 & 2.59 \\
             KalmanRC  & 5.13 & 2.66 \\
           KalmanRfun  & 5.45 & 2.83 \\
              KalmanR  & 5.46 & 2.83 \\
        FirstKalmanRC  & 6.41 & 3.32 \\
         FirstKalmanR  & 6.84 & 3.54 \\
         \bottomrule
      \end{tabular}
      \caption{Performance comparison of various implementations of a Kalman filter.
        KalmanOctave is the \pkg{RcppOctave} based implementation shown above.
        KalmanR is the \R implementation using an environment; KalmanRfun use a
        function; FirstKalmanR is a direct translation of the original Matlab
        implementation; versions ending in C are the byte-compiled variants of
        the respective version.  Timings are averaged over 100 replications.
        The comparison was made using R version 2.15.2, Rcpp version 0.10.2 and
        RcppOctave version 0.8.14 on Ubuntu 12.10 running in 64-bit mode on a
        2.67 GHz Intel i7 processor.}
      \label{tab:benchmark}
    \end{small}
  \end{center}
\end{table}

\dirk{TODO: More about the example, performance comparison, knocking socks off R}


\section{Example: Reproducible Random Numbers}

\dirk{TODO: Reference from his repeated blog posts}
Wilkinson used a simple bivariate Gibbs Sampler as a basis for comparisons
between different programming languages such as C, Java, Python and R. His
example has been re-used in number of other presentations.

We can adapt this example here as it provides a suitable framework for
showing how \pkg{RcppOctave} can interact with the random number generators
in R.

%DE: Super-useful feature, but shall we have that here or as part of the Gibbs sampler comparison
\dirk{TODO: Make true Gibbs sampler example}
{\small
<<rng, tidy=FALSE, cache=TRUE>>=
library(RcppOctave)
set.seed(1)     # reset the R RNG
a <- runif(10)

# define octave function
o_source(text="
   function [x] = orng(n)
      x = rand(1, n);
   end
")
set.seed(1)     # reset the R RNG
identical(.O$orng(10), a)
@
}

In this example, we first draw ten random number distributed $U(0,1)$ using
the default generator in R. We then define an Octave function from R. Similar
to how the \pkg{inline} package \citep{CRAN:inline} handles C or C++ code, we
simply define a character string which then passed the Octave interpreter via
the \verb|o_source()| function.  We re-set the seed of the underlying RNG in
R and call our Octave function. As it actually calls back into the RNG from
R, our two vectors are the same.  This is a critically important feature to
ensure reproducibility.

We can also define functions which draws from the Gamma distribution:
{\small
%## we have  'randg(a, N)/b' == rgamma(a,b)
%## CORRECTION:  set.seed(42); .O$gam(2,3)
%##         ==   set.seed(42); rgamma(1,2,1,3)
%##         ==   set.seed(42); rgamma(1,2,1)*3
<<rngGamma, tidy=FALSE, cache=TRUE>>=
## define a Gamma RNG draw function
o_source(text="
  ## gamma(a,b) of dim. n by 1
  function x = orngg(a, b, n)
    x = b * randg(a, n, 1);
  end
")
set.seed(42)
N <- 50000
x1 <- c(.O$orngg(9,1,N))
x2 <- c(.O$orngg(4,1,N))
x3 <- c(.O$orngg(1,4,N))
plot(density(x1), main="Calling from Octave",
     col="orange", ylim=c(0,0.23), xlim=c(-1,22))
lines(density(x2), col='mediumblue')
lines(density(x3), col='brown')
@
}

As \pkg{RcppOctave} ensures that Octave calls back into the R random-number
generator, we can generate the same chart directly from R:

{\small
<<rngGammaR, tidy=FALSE, cache=TRUE>>=
set.seed(42)  # reset RNG
N <- 50000
plot(density(rgamma(N,9,1)), main="Calling rgamma from R",
     col="orange", ylim=c(0,0.23), xlim=c(-1,22))
lines(density(rgamma(N,4,1)), col='mediumblue')
lines(density(rgamma(N,1,1,4)), col='brown')
@
}

\bibliography{../inst/REFERENCES}

\address{Dirk Eddelbuettel \\
  Debian Project \\
  River Forest, IL\\
  USA}\\
\email{edd@debian.org}

\address{Renaud Gaujoux \\
  Computational Biology \\
  University of Cape Town \\
  Cape Town \\
  South Africa}\\
\email{renaud@cbio.uct.ac.za}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "RJwrapper"
%%% End:
