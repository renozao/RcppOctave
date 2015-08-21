# Project: RcppOctave
# 
# Author: Renaud Gaujoux
# Created: Jul 21, 2015
###############################################################################

slashes <- function(x) gsub("\\", "/", x, fixed = TRUE)

Sys.path <- function(normalize = TRUE){
    path <- strsplit(Sys.getenv('PATH'), .Platform$path.sep)[[1L]]
    if( normalize ) path <- normalizePath(path, mustWork = FALSE)
    path
}

#' Retrieve a Program Configuration Variable
ac_prog_var <- function(prog, varname, msg, intern = FALSE, slashes = FALSE){
	if( is.character(intern) ) intern <- nzchar(intern)
	if( !nzchar(msg) ) msg <- varname
	message(sprintf("Checking %s... ", msg), appendLF = FALSE)
    cmd <- sprintf('"%s" --print %s', prog, varname)
    value <- shell(cmd, intern = TRUE)
    if( slashes ) value <- slashes(value)
	if( !intern ){
        message(value)
        cat(value)
    }
    invisible(value)
}

#' Retrieve a Program Configuration Path
ac_prog_varpath <- function(...){
    ac_prog_var(..., slashes = TRUE)
}

#' Finds a Program Location 
ac_path_prog <- function(prog, notfound = '', path = "", msg = "", mode = c('first', 'all', 'deep'), strip.flags = FALSE, intern = FALSE){
    
    strip.flags <- !missing(strip.flags) || !strip.flags
    if( strip.flags ) prog <- strsplit(prog, ' -', fixed = TRUE)[[1L]]
        
    if( !nzchar(msg) ) msg <- prog
    if( !nzchar(path) ) path <- Sys.path()
    
    message(sprintf("Checking %s... ", msg), appendLF = FALSE)
    if( missing(mode) || !nzchar(mode) ) mode <- 'first'
    mode <- match.arg(mode)
    
    res <- sapply(path, function(p){
        if( mode == 'deep' ) f <- list.files(p, recursive = TRUE, pattern = prog, full.names = TRUE)
        else f <- Sys.which2(prog, p)
       
       # remove duplicates
       if( length(f) > 1L ){
            f <- f[order(nchar(basename(f)), decreasing = TRUE)]
            f <- f[!duplicated(tools::md5sum(f))]
        }
        f
    })

    # remove empty elements
    res <- unlist(res[lengths(res) > 0], use.names = FALSE)
    # slashes
    res <- slashes(res[nzchar(res)])
    
    if( mode == 'first' ) res <- head(res, 1L)
    
    res <- unname(res)
    if( !length(res) ){
        message('no')
        if( !intern ) cat(notfound)
    }else{
        if( length(res) == 1L ) message(res)
        else{
            message(sprintf('multiple [%i]', length(res)))
            lapply(paste(' *', res), message)
        }
        if( !intern ) cat(res, sep = "\n")
    }
    invisible(res)
    
}

Sys.which2 <- function(names, path = ""){
    
    if( !nzchar(path) ) path <- Sys.path()
    # backup and restore original PATH
    PATH <- Sys.getenv('PATH')
    on.exit( Sys.setenv(PATH = PATH) )
    
    res <- sapply(path, function(p){
                Sys.setenv(PATH=p)
                normalizePath(Sys.which(names), mustWork = FALSE)
        }
    )
    # slashes
    res <- slashes(res[nzchar(res)])
    unname(res)
}

#' Finds Rtools
ac_path_rtools <- function(gcc){
    
    message("Checking Rtools ... ", appendLF = FALSE)
    
    # look for file VERSION.txt that contains the string "rtools"
    parts <- strsplit(gcc, '/')[[1L]]
    path <- NULL
    for( i in 1:length(parts) ){
        p <- do.call(file.path, as.list(head(parts, i)))
        if( file.exists(version_file <- file.path(p, 'VERSION.txt')) && 
                length(grep('rtools', readLines(version_file), ignore.case = TRUE)) ){
            path <- p
            break
        }
    }
    
    if( !length(path) ){ # not found
        cat("")
        message("no")
    }else{ # found
        cat(path)
        message(path)
        invisible(path)
    }
}

ac_cc_compatible <- function(octave, rtools){
    
    path <- ac_path_prog('gcc[-0-9.]*.exe$', path = rtools, msg = "Rtools compiler(s)", mode = 'deep', intern = TRUE)
    
    .cc_spec <- function(cc){
        v <- shell(sprintf("\"%s\" -dumpversion", cc), intern = TRUE)
        m <- shell(sprintf("\"%s\" -dumpmachine", cc), intern = TRUE)
        res <- list(version = package_version(v), machine = m, full = paste0(m,'-', v))
#        print(res)
    }
    
    message("Checking for compatible compiler ... ", appendLF = FALSE)
    oct_spec <- .cc_spec(octave)
    for( p in path ){
        spec <- .cc_spec(p)
        if( spec$full == oct_spec$full || 
                ((spec$machine == oct_spec$machine) && 
                    spec$version$major ==  oct_spec$version$major &&
                    spec$version$minor ==  oct_spec$version$minor)
                ){
           bin <- dirname(p)
           message(sprintf("%s [%s]", basename(p), bin))
           
           # check compiler aliases exist therein
            # gcc
           message("Checking gcc alias... ", appendLF = FALSE)
           if( !file.exists(alias <- file.path(bin, 'gcc.exe')) || 
                   tools::md5sum(alias) !=  tools::md5sum(p) ){
               break
           }
           message(alias)
           
            # g++
           cxx <- ac_path_prog('g++.exe', bin, msg = "g++", intern = TRUE)
           if( !nzchar(cxx) ) break
           
           p <- gsub(rtools, '', bin, fixed = TRUE)
           cat(p)
           return(invisible(p))
        }
    }
    # not found
    message('none')
    cat('')
    invisible()
}
