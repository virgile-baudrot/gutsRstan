`gutsRstan` is a library developped for calibration of TKTD
(Toxicokinetics-Toxicodynamics) models GUTS (General Unified Threshold models of
   Survival) with the Bayesian language Stan.

### Build status for development version

[![Build Status](https://api.travis-ci.org/virgile-baudrot/gutsRstan.svg?branch=master)](https://api.travis-ci.org/virgile-baudrot/gutsRstan.svg?branch=master)

## Report a problem

Use the [issue tracker](https://github.com/virgile-baudrot/gutsRstan/issues)
to report a problem.

# How to Install

## Installing gutsRstan on Windows

For further information about the installation, you can have a look at the installation of the `rstan` package:
- [github rstan wiki: installing RStan on Windows](https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Windows)

### R prerequisites

R version 3.0.2 or later is required (as it is for r).

The latest stable version of R is available from
- [https://www.r-project.org/](https://www.r-project.org/)

### Dowload and Install Rtools

Rtools can be downloaded from
- [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/)
Note that latest version of Rtools have compatibility with R version 3.3.x and later.

During installation, just take attention that the C++ compiler included in Rtools is allowed to be used by R.

Once reloaded a new R session, check that Rtools can be used in R:

```
> Sys.getenv("PATH")
```
Should return something like:
```
[1] "c:\\\\Rtools\\\\bin;c:\\\\Rtools\\\\gcc-4.6.3\\\\bin;...
```

Check also that `g++` can be call from R:
```
> system('g++ -v')
```
Should return something like:
```
Using built-in specs.
COLLECT_GCC=c:\Rtools\GCC-46~1.3\bin\G__~1.EXE
COLLECT_LTO_WRAPPER=c:/rtools/gcc-46~1.3/bin/../libexec/gcc/i686-w64-mingw32/4.6.3/lto-wrapper.exe
Target: i686-w64-mingw32
Configured with: /data/gannet/ripley/Sources/mingw-test3/src/gcc/configure --host=i686-w64-mingw32 --build=x86_64-linux-gnu --target=i686-w64-mingw32 --with-sysroot=/data/gannet/ripley/Sources/mingw-test3/mingw32mingw32/mingw32 --prefix=/data/gannet/ripley/Sources/mingw-test3/mingw32mingw32/mingw32 --with-gmp=/data/gannet/ripley/Sources/mingw-test3/mingw32mingw32/prereq_install --with-mpfr=/data/gannet/ripley/Sources/mingw-test3/mingw32mingw32/prereq_install --with-mpc=/data/gannet/ripley/Sources/mingw-test3/mingw32mingw32/prereq_install --disable-shared --enable-static --enable-targets=all --enable-languages=c,c++,fortran --enable-libgomp --enable-sjlj-exceptions --enable-fully-dynamic-string --disable-nls --disable-werror --enable-checking=release --disable-win32-registry --disable-rpath --disable-werror CFLAGS='-O2 -mtune=core2 -fomit-frame-pointer' LDFLAGS=
Thread model: win32
gcc version 4.6.3 20111208 (prerelease) (GCC)
```

### Installing gutsRstan

From the [github repository of gutsRstan](https://github.com/virgile-baudrot/gutsRstan), the following command should work:
```
install.packages("devtools")
library("devtools")
install_github("virgile-baudrot/gutsRstan")
```

Or from the source `tar.gz` file available in your download repository (`~\\` is the path to your repository `gutsRstan_0.1.0.tar.gz`).
```
install.packages("~\\gutsRstan_0.1.0.tar.gz", repos = NULL, type = "source")
```

```
# Or once the package will be on CRAN
# install.packages("gutsRstan")
```

## Installing gutsRstan on Mac and Linux

For further information about the installation, you can have a look at the installation of the `rstan` package:
- [github rstan wiki: installing RStan on Mac or Linux](https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Mac-or-Linux)

You should install Rcpp from CRAN:
```
install.packages("Rcpp")
```

Then, you can install `gutsRstan` from the [github repository of gutsRstan](https://github.com/virgile-baudrot/gutsRstan), the following command should work:
```
install.packages("devtools")
library("devtools")
install_github("virgile-baudrot/gutsRstan")
```

Or from the source `tar.gz` file available in your download repository (`~//` is the path to your repository `gutsRstan_0.1.0.tar.gz`):
```
install.packages("~//gutsRstan_0.1.0.tar.gz", repos = NULL, type = "source")
```

```
# Or once the package will be on CRAN
# install.packages("gutsRstan")
```


Restart R and check the toolchain:
```
fx <- inline::cxxfunction( signature(x = "integer", y = "numeric" ) , '
	return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
' )

fx( 2L, 5 ) # should be 10
```

# Compilation

## Compilation (for command-line users)

- `cd` to source directory
- Build from sources (creates vignette and archive)
  `R CMD build .`
- Build and install `R CMD INSTALL --build .`
- Build and install building vignettes with R interpreter:
  `devtools::install(build_vignettes = TRUE)`
- Check the package
  `R CMD check --as-cran gutsRstan_X.X.X.tar.gz`
- Update package description/NAMESPACE
  - under the R interpreter: `roxygen2::roxygenise(".")`
- Generate documentation
  - reference manual: `R CMD Rd2pdf --output=documentation .`
  - re-build reference manual: `rm documentation | R CMD Rd2pdf --output=documentation .`
  - vignettes (using the R interpreter):
    `devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))`
- Run unit tests
  - under the R interpreter: `devtools::test()`

## Compilation (with RStudio)

- need `devtools`, `ROxygen2 v5.0.1 or higgher`
- RStudio builder configuration:
  - Project Options :
      enable `Use devtools package...`
      enable `Generate documentation...`
      ROxygen options...`:
        all enable exept `Vignettes` and `Source and binary package build`
  - update Documentation (no vignette, only NAMESPACE and Rd)
      `Document` or Ctrl + Shift + D
  - build the source file :
    `More : Build Source Package`

