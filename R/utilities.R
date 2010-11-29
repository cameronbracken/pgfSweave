makeExternalShellScriptName <- function(Rnwfile) {
    shellfile <- sub("\\.Rnw$", "\\.sh", Rnwfile)

    ## Don''t clobber
    if(identical(shellfile, Rnwfile))
        shellfile <- paste(Rnwfile, "sh", sep = ".")
    shellfile
}

## preserve comments in the R code and at the same time make them 'well-formatted'
tidy.source <- function(source = "clipboard", keep.comment = TRUE,
    keep.blank.line = TRUE, begin.comment, end.comment, output = TRUE,
    width.cutoff = 60L, ...) {
    if (source == "clipboard" && Sys.info()["sysname"] == "Darwin") {
        source = pipe("pbpaste")
    }
    tidy.block = function(block.text) {
        exprs = base::parse(text = block.text)
        n = length(exprs)
        res = character(n)
        for (i in 1:n) {
            dep = paste(base::deparse(exprs[i], width.cutoff),
                collapse = "\n")
            res[i] = substring(dep, 12, nchar(dep) - 1)
        }
        return(res)
    }
    text.lines = readLines(source, warn = FALSE)
    if (keep.comment) {
        identifier = function() "pgfSweaveCommentIdentifier__"
        #paste(sample(LETTERS), collapse = "")
        if (missing(begin.comment))
            begin.comment = identifier()
        if (missing(end.comment))
            end.comment = identifier()
        
        text.lines = gsub("^[[:space:]]+|[[:space:]]+$", "",
            text.lines)
        while (length(grep(sprintf("%s|%s", begin.comment, end.comment),
            text.lines))) {
            begin.comment = identifier()
            end.comment = identifier()
        }
        line.num.comment = substring(text.lines, 1, 5) == "#line" 
        text.lines = text.lines[!line.num.comment]
          head.comment = substring(text.lines, 1, 1) == "#"
          #grep("^[[:space:]]+|#",text.lines)
          #
        if ( length(head.comment) > 0 ) {
            text.lines[head.comment] = gsub("\"", "'", text.lines[head.comment])
            text.lines[head.comment] = gsub("^#", "  #", text.lines[head.comment])
            text.lines[head.comment] = sprintf("%s=\"%s%s\"",
                begin.comment, text.lines[head.comment], end.comment)
        }
        blank.line = text.lines == ""
        if (any(blank.line) & keep.blank.line)
            text.lines[blank.line] = sprintf("%s=\"%s\"", begin.comment,
                end.comment)
        text.mask = tidy.block(text.lines)
        text.tidy = gsub(sprintf("%s = \"|%s\"", begin.comment,
            end.comment), "", text.mask)
    }
    else {
        text.tidy = text.mask = tidy.block(text.lines)
        begin.comment = end.comment = ""
    }
    if (output)
        cat(paste(text.tidy, collapse = "\n"), "\n", ...)
    invisible(list(text.tidy = text.tidy, text.mask = text.mask,
        begin.comment = begin.comment, end.comment = end.comment))
}

## to replace the default parse()
parse2 <- function(text, ...) {
    zz = tempfile()
    enc = options(encoding = "native.enc")
    writeLines(text, zz)
    tidy.res = tidy.source(zz, out = FALSE, keep.blank.line = 
        ifelse(is.null(getOption('keep.blank.line')), TRUE,
            getOption('keep.blank.line')))
    options(enc)
    unlink(zz)
    options(begin.comment = tidy.res$begin.comment, 
        end.comment = tidy.res$end.comment)
    base::parse(text = tidy.res$text.mask)
}

## to replace the default deparse()
deparse2 <- function(expr, ...) {
    gsub(sprintf("%s = \"|%s\"", getOption("begin.comment"),
        getOption("end.comment")), "", base::deparse(expr, ...))
}

  # from the limma package on bioconductor
removeExt  <- function (x) 
{
    x <- as.character(x)
    n <- length(x)
    if (length(grep("\\.", x)) < n) 
        return(x)
    ext <- sub("(.*)\\.(.*)$", "\\2", x)
    if (all(ext[1] == ext)) 
        return(sub("(.*)\\.(.*)$", "\\1", x))
    else return(x)
}