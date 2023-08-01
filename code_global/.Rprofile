library(utils)
chooseCRANmirror(ind = 1)

# options(download.file.method = "wget"); # For Ubuntu 14.04
package <- function(p, version = NULL, remove = FALSE, github = '') {
  if (remove) {
    detach(paste0("package:", p), unload = T)
    remove.packages(p)
  }
  if (!is.element(p, installed.packages()[,1])) {
    if (missing(version)) {
      if (github != '') {
        package("devtools")
        install_github(paste0(github, '/', p))
      } else install.packages(p) # , repos='https://cran.rstudio.com/', type = 'source' may help in case of bug
    } else {
      try({detach("package:devtools", unload = T)})
      package("remotes")
      install_version(p, version = version, repos = "http://cran.us.r-project.org", upgrade = "never", dependencies = TRUE)
      package("devtools")
    }
  }
  else { if(!missing(version)) warning(paste("'", p, "' is already installed with a (potentially) newer version. You may want to install the required version (", version, ") to avoid bugs.", sep=""))}
  library(p, character.only = TRUE)
} # loads packages with automatical install if needed

package("stringr")
if (!is.element("iatgen", installed.packages()[,1])) {
  warning("/!\ Install iatgen manually from github!")
  # package("devtools")
  # devtools::install_github("iatgen/iatgen")
} else library(iatgen)
package("ggplot2")
package("ggalt") # maps
package("janitor") # heatmaps
package("ggdist") # nice confidence intervals in regression plots

# package("qualtRics") # https://cran.r-project.org/web/packages/qualtRics/vignettes/qualtRics.html
# For Antoine's account, API is not enabled (cf. Account Settings > Qualtrics ID > API or https://lse.eu.qualtrics.com/Q/QualtricsIdsSection/IdsSection) so we cannot retrieve the data directly using qualtRics.
# qualtrics_api_credentials(api_key = "<YOUR-QUALTRICS_API_KEY>", base_url = "https://lse.eu.qualtrics.com/", install = TRUE)

#'
#' chooseCRANmirror(ind = 1)
#' package("plyr")
package("tm") # wordcloud
package('tidyverse')
package("dplyr")
package("tidyr")
package("expss") # fre (for weighted frequency table)
package("GDAtools") # wtable (for weighted frequency crosstable)
package("beepr") # beep() makes sound
package("openxlsx")
package("cjoint") # conjoint analysis /!\ I fixed a bug in the program => to install my version, package("devtools"), clone repo, setwd(/cjoint/R), build(), install()
package("modelsummary")
package("xtable") # export latex table
package("list") # list experiment aka. item count technique: ictreg
package("weights") # wtd.t.test
package("raster") # merge boundaries in maps
package("sf") # merge boundaries in maps
package("maptools")
package("threadr", github = "skgrange") # dependency of gissr
package("gissr", github = "skgrange") # merge boundaries in maps
package("maps") # merge boundaries in maps
#' package("rms")
#' package('pwr')
#' package("foreign")
#' package("DT")
#' package("pastecs")
#' package("lsr")
#' package("ggplot2")
#' package("stringr")
#' package("survey")
#' Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "C:/RTools42/usr/bin", sep = .Platform$path.sep))
#' Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "/home/adrien/anaconda3/bin", sep = .Platform$path.sep))
#' Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "C:/Users/fabre/Anaconda3/pkgs/plotly-orca-1.3.1-1/orca_app", sep = .Platform$path.sep)) # to correct bug orca, add folder of orca.exe
Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "C:/Users/fabre/.conda/pkgs/plotly-orca-1.3.1-1/orca_app", sep = .Platform$path.sep)) # to correct bug orca, add folder of orca.exe
Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "C:/ProgramData/Anaconda3/pkgs/plotly-orca-1.3.1-1/orca_app", sep = .Platform$path.sep))
#' # /!\ To install plotly, you first need to install kaleido and orca. Run Anaconda in administrator mode and run: pip install kaleido; conda install -c plotly plotly-orca; then set orca path as above
if (!is.element("plotly", installed.packages()[,1])) install.packages("https://github.com/plotly/plotly.R/archive/refs/tags/v4.9.4.1.tar.gz", repos=NULL) else library(plotly) # If bug change .libPaths() (to /Program Files instead of Users/.../AppData)
# package("plotly") # in case of bug due to kaleido: "pip install kaleido" in the python console. À PA, version 4.10.0
#' # package("plotly", version = "4.9.4.1") # If bug, do instead: install.packages("https://github.com/plotly/plotly.R/archive/refs/tags/v4.9.4.1.tar.gz", repos=NULL) The last version of Plotly changes the place of Legend and makes it over several lines unless one increases width. If the install bugs as admin, try as simple user. If it still bugs, make sure Rtools is installed and found by R. If already installed (to check: package("installr"); install.Rtools()), try: write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE) then check that Sys.which("make") returns "C:\\rtools40\\usr\\bin\\make.exe". (cf. https://cran.r-project.org/bin/windows/Rtools/rtools40.html)
#' # On ARM Mac, if this doesn't work (runs infinitely). Download the archive and from the Terminal run `R CMD INSTALL plotly.R-4.9.4.1.tar.gz'
#' # package("reticulate")
#' if (!is.element("gdata", installed.packages()[,1])) package("memisc")
#' package('gdata')
package("Hmisc")
package("descr") # CrossTable
#' package("quantreg")
#' package("rcompanion")
#' package("DescTools")
#' # package("VCA")
#' package("glmnet")
#' # package("installr") # not for linux
#' package("processx")
#' package("readstata13")
#' package("permute")
#' package("AER")
#' package("ivmodel")
#' package("rattle")
#' package("data.table")
#' package("reshape2")
#' # package("rddtools") # not available
#' # package("rddapp") # not available
#' package("mets")
package("stargazer") # To fix the bug with is.na() on R 4.2, run the code below from https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53
# temp <- getwd()
# setwd(.libPaths()[2])
# detach("package:stargazer",unload=T)
# remove.packages("stargazer")
# download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# untar("stargazer_5.2.3.tar.gz")
# stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# stargazer_src[1990] <- stargazer_src[1995]
# stargazer_src[1995] <- ""
# writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# install.packages("stargazer", repos = NULL, type="source")
# setwd(temp)
#' package("clipr")
package("ergm") # wtd.median
#' package("mfx")
#' package("margins")
#' package("plotrix")
#' package("grDevices")
#' package("colorspace")
#' package("colorRamps")
#' package("ordinal")
#' package("oglmx")
#' package("logistf")
#' package("emmeans")
#' package("ggeffects")
#' package("snakecase")
#' package("rdd")
#' # detach("package:corrplot", unload = T)
#' # remove.packages("corrplot")
#' # package("prettydoc")
#' # package("seriation")
package("RColorBrewer")
package("corrplot") #, github = 'taiyun')#, version = "0.88") # 0.92 installed: is that an issue?
#' package("psy")
#' package("lavaan")
#' package("StatMatch")
#' package("np")
#' package("AMR")
#' package("KSgeneral")
#' package("dgof")
#' package("SnowballC")
package("wordcloud")
#' package("RCurl")
#' package("XML")
#' package("equivalence")
#' package("RMallow")
#' package("rpart")
#' package("readr")
#' package("caTools")
#' package("party")
#' package("partykit")
#' package("rpart.plot")
#' package("Peacock.test")
#' package("devtools")
#' package("janitor")
#' # LDA
package("quanteda") # stopwords
#' package("topicmodels")
#' package("broom")
#' package("tidytext")
#' package("modelsummary")
package("dplR") # latexify, used in table_mean_lines_save
#' package("ggpubr")
#' package("RStata")
#' package("relaimpo") # works well with 21 variables, not much more. install from: install.packages("https://prof.bht-berlin.de/fileadmin/prof/groemp/downloads/relaimpo_2.2-5.zip", repos = NULL)
#' package("missMDA") # PCA
#' package("forcats")
#' package("modi")
#' package("descr")
package("knitr") # plot_crop, representativeness_table
options(knitr.kable.NA = '')
package("kableExtra") # add_header_above in representativeness_table
#' # One needs a *patched* version of memisc version 0.99.22 (not a newer), hence the code below (cf. this issue: https://github.com/melff/memisc/issues/62)
if (!is.element("memisc", installed.packages()[,1])) {
  install.packages("https://github.com/melff/memisc/files/9690453/memisc_0.99.22.tar.gz", repos=NULL)
} else if (packageVersion("memisc")!="0.99.22") {
  detach("package:memisc", unload = TRUE)
  remove.packages("memisc")
  install.packages("https://github.com/melff/memisc/files/9690453/memisc_0.99.22.tar.gz", repos=NULL)
} else library(memisc)
#' # If this doesn't work (runs infinitely). Download the archive and from the Terminal run `R CMD INSTALL memisc_0.99.22.tar.gz  '
#' # The following will not work: package("memisc", version = "0.99.22") # in case of bug (endless loop), copy/paste folder /memisc in library and: install.packages("memisc", method = "win.binary") OR install.packages("https://github.com/melff/memisc/files/9690453/memisc_0.99.22.tar.gz", repos=NULL). If it still doesn't work, run library(utils); install.packages("https://github.com/melff/memisc/files/9690453/memisc_0.99.22.tar.gz", repos=NULL) from R (not RStudio)
#' # package("estimatr")
#'
#' # package("quanteda")
#' # package("haven")
#' # package("dplyr")
#' # package("textclean")
#' # package("tm")
#' # package("SnowballC")
#' # package("wordcloud")
#' # c("quanteda","forcats", "tidytext", "haven", "dplyr", "topicmodels", "textclean", "tm" , "SnowballC", "wordcloud", "quanteda",
#' #   "lmtest", "sandwich","ggplot2", "stargazer", "DBI", "readstata13", "haven", "dplyr", "xlsx", "textstem", "readxl",  "stringr", "ggrepel")
#' # install_github("rstudio/webshot2")
#' # package("webshot2")
#' package("htmlwidgets")
# package("magick") # Bug sur Ubuntu, ne surtout pas décommenter sur Ubuntu
library(magick) # image_write
#' # install_github(repo = "MatthieuStigler/RCompAngrist", subdir = "RCompAngrist")
#' # package("RCompAngrist")
#'
#' # package("psych") # library(psych, exclude = "describe")
#' # package("semTools")
#' # package("interplot")
#' # package("jtools")
#' # package("effects")
#' # package("sjplot")
#' # package("doMC") # for parallel computing, does not work on Windows
#'
#' # eval(parse(along)) !!along as.name(along) substitute(eval(along)) eval(along) substitute(temp) deparse(substitute(temp)) eval(as.symbol()) eval(str2expression(along))
#' # Fs <- function(QID) { s[QID][[1]] }
#' # Vs <- function(QID) { as.vector(Fs(QID))  }
d <- function(str, alt_data = eu, alt_var = "country") {
  if (exists(tolower(str)) && is.data.frame(eval(str2expression(tolower(str))))) return(eval(str2expression(tolower(str)))) # data from name
  else return(alt_data[alt_data[[alt_var]] == toupper(str),])
}
n <- function(var) { as.numeric(as.vector(var)) }
#' NSPs <- function(QID) { length(V(QID)[V(QID) == "NSP (Je ne veux pas répondre)"])/length(V(QID)) }
#' nsps <- function(id) { length(v(id)[v(id) == "NSP (Je ne veux pas répondre)"])/length(v(id)) }
match.nona <- function(v, t) {return(as.vector(na.omit(match(v, t))))} # returns match(v, t) purged from NAs, i.e. the (first) position of v elements (that are in t) in t (screened/ordered from v), cf. below
# so df$foo[match.nona(db$bar, df$bar)] <- db$foo[db$bar %in% df$bar] replaces elements of df$foo such that df$bar is in db$bar by corresponding db$foo
Label <- function(var) {
  if (length(annotation(var))==1) { annotation(var)[1] }
  else { label(var)  }
}
break_string <- function(string, max_length = 57, soft_max_length = T, sep = "<br>", max_lines = 3) {
  n <- nchar(string)
  nb_pieces <- min(ceiling(n/max_length), max_lines)
  max <- floor(n/nb_pieces)
  broken_string <- strwrap(string, max)
  if (soft_max_length) { # Ensures that there are nb_pieces lines, but a line can be longer than max_length
    while (length(broken_string) > nb_pieces) {
      max <- max + 1
      broken_string <- strwrap(string, max)
    }
  } # else no line is longer than max_length-1 but there might be more than nb_pieces lines.
  return(paste(broken_string, collapse = sep))
}

break_strings <- function(strings, max_length = 57, soft_max_length = T, sep = "<br>", max_lines = 3) {
  broken_strings <- strings
  for (s in seq_along(strings)) if (!grepl(sep, strings[s])) {
    broken_strings[s] <- break_string(strings[s], max_length = max_length, soft_max_length = soft_max_length, sep = sep, max_lines = max_lines) }
  return(broken_strings)
}
is.pnr <- function(variable, empty_as_pnr = T) {
  if (empty_as_pnr) {
    if (is.null(annotation(variable))) return(is.na(variable)|variable=="")
    else return(is.missing(variable)|variable=="")
  } else {
    if (is.null(annotation(variable))) return(is.na(variable))
    else return(is.missing(variable))
  }
}
no.na <- function(vec) return(replace_na(as.vector(vec), "na"))
gap <- function(vec) return(if (max(vec, na.rm = T) == 0) 0 else abs((max(vec, na.rm = T) - min(vec, na.rm = T))/max(vec, na.rm = T)))
max_gap <- function(vec1, vec2, epsilon = 1e-15) return(max(2*abs(vec1 - vec2)/(abs(vec1 + vec2) + epsilon), na.rm = T))
is.binary <- function(vec) { return((is.logical(vec) | all(unique(as.numeric(vec)) %in% c(0, 1, NA)))) }
majority <- function(vec) {
  if (is.binary(vec)) return(100*mean(vec, na.rm = T))
  else return(100 * sum(!is.na(vec) & vec > 0) / sum(!is.na(vec) & vec != 0))
}
positive <- function(vec) {
  if (is.binary(vec)) return(100*mean(vec, na.rm = T))
  else return(100 * sum(!is.na(vec) & vec > 0) / sum(!is.na(vec)))
}
agg_thresholds <- function(vec, thresholds, labels = NULL, sep = " - ", begin = "", end = "", shift = 0, strict_ineq_lower = T, return = "vec" # min = 0, max = Inf,
) { 
  # strict_ineq_lower == T means intervals 50,60 are of type ];] while == F means [;[.
  # shift = 1 (with strict_ineq_lower == T) means levels ]50;60] will be displayed as "[begin]51[sep]60[end]".
  # thresholds <- c(min, thresholds, max)
  min <- thresholds[1]
  max <- thresholds[length(thresholds)]
  shift_left <- ifelse(strict_ineq_lower, shift, 0)
  shift_right <- ifelse(strict_ineq_lower, 0, shift)
  vec_agg <- rep(NA, length(vec))
  values <- c()
  if (missing(labels)) levels <- c()
  else levels <- labels
  for (i in 2:length(thresholds)) {
    values <- c(values, (thresholds[i] + thresholds[i-1])/2)
    min_i <- ifelse(i > 2, thresholds[i-2], min)
    max_i <- ifelse(i <= length(thresholds) - 2, thresholds[i+2], max)
    next_i <- ifelse(i <= length(thresholds) - 1, thresholds[i+1], max)
    if (missing(labels)) levels <- c(levels, if (thresholds[i-1]==thresholds[i]) paste0(begin, thresholds[i], end) else paste0(begin, thresholds[i-1] + (shift_left*(i < length(thresholds)) + shift_right*(thresholds[i-1] == min_i))*(i > 2), sep,
                                                                                                                               thresholds[i] - (shift_right*(i > 2) + shift_left*(thresholds[i] == next_i))*(i < length(thresholds)), end))
    if (strict_ineq_lower) vec_agg[(vec <= thresholds[i] & vec < max_i & vec > thresholds[i-1]) | (vec == max_i & i == length(thresholds)) | (vec == thresholds[i] & i < length(thresholds) & vec < max_i) | (i == 2 & vec == min)] <- (thresholds[i] + thresholds[i-1])/2
    else vec_agg[(vec < thresholds[i] & vec >= thresholds[i-1] & vec > min_i) | (vec == min_i & i == 2) | (vec == thresholds[i-1] & i > 2 & vec > min_i) | (i == length(thresholds) & vec == max)] <- (thresholds[i] + thresholds[i-1])/2
  }
  if (min == -Inf & strict_ineq_lower) levels[1] <- sub(paste0("-Inf", sep), "≤ ", levels[1])
  if (min == -Inf & !strict_ineq_lower) levels[1] <- sub(paste0("-Inf", sep), "< ", levels[1])
  if (max == Inf & strict_ineq_lower) levels[length(levels)] <- sub(paste0("(.*)", sep, "Inf"), "> \\1", levels[length(levels)]) # sub(" ", "", sep)
  if (max == Inf & !strict_ineq_lower) levels[length(levels)] <- sub(paste0("(.*)", sep, "Inf"), "≥ \\1", levels[length(levels)]) # sub(" ", "", sep)
  levels <- gsub("000 ", ",000 ", gsub("-", "–", levels))
  vec_agg[is.na(vec)] <- NA
  vec_agg <- as.item(vec_agg, labels = structure(values, names = levels), missing.values = c("",NA), annotation=Label(vec))
  if (return == "vec") return(vec_agg)
  else if (return %in% c("levels", "labels")) return(levels)
  else if (return == "values") return(values)
}
decrit <- function(variable, data = e, miss = TRUE, weights = NULL, numbers = FALSE, which = NULL, weight = T) { # TODO!: allow for boolean weights
  # if (!missing(data)) variable <- data[[variable]]
  if (is.character(variable) & length(variable)==1) variable <- data[[variable]]
  if (!missing(which)) variable <- variable[which]
  if (weight) {
    # if (length(variable) > 1) warning("Field 'variable' is a vector instead of a character, weight will not be used.")
    if (missing(weights)) weights <- data[["weight"]]  #  if (missing(data)) warning("Field 'data' is missing, weight will not be used.") else {
    if (!missing(which)) weights <- weights[which]
    if (length(weights)!=length(variable)) {
      warning("Lengths of weight and variable differ, non-weighted results are provided")
      weights <- NULL
    } }
  if (length(annotation(variable))>0 & !numbers) {
    if (!miss) {
      # if (is.element("Oui", levels(as.factor(variable))) | grepl("(char)", annotation(variable)) | is.element("quotient", levels(as.factor(variable)))  | is.element("Pour", levels(as.factor(variable))) | is.element("Plutôt", levels(as.factor(variable))) ) { describe(as.factor(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
      # else { describe(variable[variable!="" & !is.na(variable)], weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
      if (length(which(!is.na(suppressWarnings(as.numeric(levels(as.factor(variable)))))))==0) { describe(as.factor(variable[variable!=""]), weights = weights[variable!=""], descript=Label(variable)) } # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
      else { describe(as.numeric(as.vector(variable[variable!=""])), weights = weights[variable!=""], descript=Label(variable)) } # avant:  & !is.na(variable)
    }
    else {
      if (length(which(suppressWarnings(!is.na(as.numeric(levels(as.factor(variable)))))))>10) describe(include.missings(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
      else describe(as.factor(include.missings(variable)[include.missings(variable)!="" & !is.na(variable)]), weights = weights[include.missings(variable)!="" & !is.na(variable)], descript=Label(variable)) }
  }
  else {
    if (length(annotation(variable))>0) {
      if (miss) describe(variable[variable!=""], weights = weights[variable!=""], descript=Label(variable))
      else describe(variable[variable!="" & !is.missing(variable)], weights = weights[variable!="" & !is.missing(variable)], descript=paste(length(which(is.missing(variable))), "missing obs.", Label(variable)))
    } else describe(variable[variable!=""], weights = weights[variable!=""])  }
}
# Levels_data <- function(var) { # I replaced it by Levels, haven't checked if it may create bugs
#   if (setequal(levels(var), c(T, F))) levels <- c(T) # before: not this line
#   else if (is.null(annotation(var))) levels <- levels(var)
#   else {
#     levels <- labels(var)@.Data
#     levels <- levels[levels %in% as.character(var)] # new, removes empty levels
#   }
#   return(levels)
# }
Levels <- function(variable, data = e, miss = TRUE, numbers = FALSE, values = TRUE, concatenate = FALSE, max_values = 13, names = FALSE) {
  if (values) {
    if (length(variable)==1 & is.character(variable)) variable <- data[[variable]]
    # if (length(annotation(variable))==1 & !is.null(labels(variable))) Levs <- as.character(labels(variable)) # old, gave numbers instead of labels for double.item
    if ("double.item" %in% class(variable) & !is.null(labels(variable))) Levs <- names(as.character(labels(variable)))
    else if (("character.item" %in% class(variable) | length(annotation(variable))==1) & !is.null(labels(variable))) Levs <- as.character(labels(variable))
    else if (is.factor(variable)) Levs <- levels(variable)
    else if (is.numeric(variable)) {
      if (length(unique(variable)) > max_values) {
        Levs <- round(c(min(variable, na.rm = T), max(variable, na.rm = T)), 3)
        names(Levs) <- c("min", "max")
      } else Levs <- round(sort(unique(variable)), 3) }
    else if (is.character(variable)) Levs <- if (length(unique(variable)) > max_values) "[string variable]" else as.character(unique(variable))
    else if (is.logical(variable)) Levs <- if (any(is.pnr(variable))) "TRUE / FALSE / NA" else "TRUE / FALSE"
    if (concatenate) {
      if (is.null(names(Levs))) Levs <- paste(Levs, collapse = " / ")
      else Levs <- paste(sapply(1:length(Levs), function(i) return(paste0(names(Levs)[i], ": ", Levs[i]))), collapse = " / ")
    }
  } else {
    Levs <- decrit(variable, miss = miss, numbers = numbers, data = data)$values$value
    if (is.null(Levs)) {
      if (is.character(variable) & length(variable)==1) variable <- data[[variable]]
      Levs <- unique(variable)
      if (concatenate) Levs <- paste(Levs, collapse = " / ") } }
  if (names & length(names(Levs)) == length(Levs)) Levs <- names(Levs)
  return(Levs)
  # if (is.character(var) & length(var)==1) var <- data[[var]]
  # if (length(annotation(var))>0) { # works but cubmbersome and doesn't allow to get rid of missings
  #   if (is.character(var)) levels(as.factor(include.missings(var)))
  #   else return(as.vector(labels(var))) }
  # else return(levels(as.factor(var))) # as.factor may cause issues as it converts to string
}
#' # decrit <- function(variable, miss = FALSE, weights = NULL, numbers = FALSE, data = e, which = NULL, weight = T) {
#' #   # if (!missing(data)) variable <- data[[variable]]
#' #   if (is.character(variable) & length(variable)==1) variable <- data[[variable]]
#' #   if (!missing(which)) variable <- variable[which]
#' #   if (weight) {
#' #     # if (length(variable) > 1) warning("Field 'variable' is a vector instead of a character, weight will not be used.")
#' #     if (missing(weights)) weights <- data[["weight"]]  #  if (missing(data)) warning("Field 'data' is missing, weight will not be used.") else {
#' #     if (!missing(which)) weights <- weights[which]
#' #     if (length(weights)!=length(variable)) {
#' #       warning("Lengths of weight and variable differ, non-weighted results are provided")
#' #       weights <- NULL
#' #     } }
#' #   if (length(annotation(variable))>0 & !numbers) {
#' #     if (!miss) {
#' #       # if (is.element("Oui", levels(as.factor(variable))) | grepl("(char)", annotation(variable)) | is.element("quotient", levels(as.factor(variable)))  | is.element("Pour", levels(as.factor(variable))) | is.element("Plutôt", levels(as.factor(variable))) ) { describe(as.factor(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#' #       # else { describe(variable[variable!="" & !is.na(variable)], weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#' #       if (length(which(!is.na(suppressWarnings(as.numeric(levels(as.factor(variable)))))))==0) { describe(as.factor(variable[variable!=""]), weights = weights[variable!=""], descript=Label(variable)) } # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
#' #       else { describe(as.numeric(as.vector(variable[variable!=""])), weights = weights[variable!=""], descript=Label(variable)) } # avant:  & !is.na(variable)
#' #     }
#' #     else {
#' #       if (length(which(suppressWarnings(!is.na(as.numeric(levels(as.factor(variable)))))))>10) describe(include.missings(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
#' #       else describe(as.factor(include.missings(variable[variable!="" & !is.na(variable)])), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#' #   }
#' #   else {
#' #     if (length(annotation(variable))>0) {
#' #       if (miss) describe(variable[variable!=""], weights = weights[variable!=""], descript=Label(variable))
#' #       else describe(variable[variable!="" & !is.missing(variable)], weights = weights[variable!="" & !is.missing(variable)], descript=paste(length(which(is.missing(variable))), "missing obs.", Label(variable)))
#' #     } else describe(variable[variable!=""], weights = weights[variable!=""])  }
#' # }
#' # decrit <- function(variable, miss = FALSE, weights = NULL, numbers = FALSE, data = e, which = NULL, weight = T) {
#' #   # if (!missing(data)) variable <- data[[variable]]
#' #   if (is.character(variable) & length(variable)==1) variable <- data[[variable]]
#' #   if (!missing(which)) variable <- variable[which]
#' #   if (weight) {
#' #     # if (length(variable) > 1) warning("Field 'variable' is a vector instead of a character, weight will not be used.")
#' #     if (missing(weights)) weights <- data[["weight"]]  #  if (missing(data)) warning("Field 'data' is missing, weight will not be used.") else {
#' #     if (!missing(which)) weights <- weights[which]
#' #     if (length(weights)!=length(variable)) {
#' #       warning("Lengths of weight and variable differ, non-weighted results are provided")
#' #       weights <- NULL
#' #     } }
#' #   if (length(annotation(variable))>0 & !numbers) {
#' #     if (!miss) {
#' #       # if (is.element("Oui", levels(as.factor(variable))) | grepl("(char)", annotation(variable)) | is.element("quotient", levels(as.factor(variable)))  | is.element("Pour", levels(as.factor(variable))) | is.element("Plutôt", levels(as.factor(variable))) ) { describe(as.factor(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#' #       # else { describe(variable[variable!="" & !is.na(variable)], weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#' #       if (length(which(!is.na(suppressWarnings(as.numeric(levels(as.factor(variable)))))))==0) { describe(as.factor(variable[variable!=""]), weights = weights[variable!=""], descript=Label(variable)) } # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
#' #       else { describe(as.numeric(as.vector(variable[variable!=""])), weights = weights[variable!=""], descript=Label(variable)) } # avant:  & !is.na(variable)
#' #     }
#' #     else {
#' #       if (length(which(suppressWarnings(!is.na(as.numeric(levels(as.factor(variable)))))))>10) describe(include.missings(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
#' #       else describe(as.factor(include.missings(variable[variable!="" & !is.na(variable)])), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#' #   }
#' #   else {
#' #     if (length(annotation(variable))>0) {
#' #       if (miss) describe(variable[variable!=""], weights = weights[variable!=""], descript=Label(variable))
#' #       else describe(variable[variable!="" & !is.missing(variable)], weights = weights[variable!="" & !is.missing(variable)], descript=paste(length(which(is.missing(variable))), "missing obs.", Label(variable)))
#' #     } else describe(variable[variable!=""], weights = weights[variable!=""])  }
#' # }
export_codebook <- function(data, file = "../data/codebook.csv", stata = TRUE, dta_file = NULL, csv_file = NULL, rds_file = NULL, keep = NULL, omit = NULL, folder = "../data/") {
  if (missing(keep)) keep <- 1:length(data)
  if (!missing(omit)) keep <- setdiff(keep, omit)

  if (stata) {
    data_stata <- janitor::clean_names(eval(data))
    names_stata <- c()
    for (i in seq_along(names(data_stata))) {
      names_stata[i] <- ifelse(nchar(names(data_stata)[i]) < 33, names(data_stata)[i], paste(substr(names(data_stata)[i], 1, 27), i, sep = "_"))
      # Shorten string values
      if (is.character(data_stata[[i]]) && any(nchar(data_stata[[i]]) > 127, na.rm = T)) data_stata[[i]] <- substr(data_stata[[i]], 1, 128) }
    names(data_stata) <- names_stata
    if (!missing(dta_file)) {
      # attr(data_stata, "label.table") <- list()
      # attr(data_stata, "val.labels") <- c()
      # attr(data_stata, "label") <- "data.frame"
      # for (i in seq_along(names(data))) {
      #   if (length(annotation(data[[i]]))==1) { # This is supposed to export value labels in Stata but it doesn't seem to work, because it relies on write.dta (note write_dta), which is deprecated
      #     attr(data_stata, "label.table") <- c(attr(data_stata, "label.table"), list(Levels(data[[i]])))
      #     attr(data_stata, "val.labels") <- c(attr(data_stata, "val.labels"), names_stata[i]) }
      # }
      haven::write_dta(data_stata[,keep], paste0(folder, dta_file, ".dta"))
    }
    if (!missing(csv_file)) write.csv(data_stata[,keep], paste0(folder, csv_file, ".csv"))
    if (!missing(rds_file)) saveRDS(data_stata[,keep], file = paste0(folder, rds_file, ".rds"))

    codebook <- data.frame(names(data), names_stata, sapply(names(data), function(n) return(Label(data[[n]]))), sapply(names(data), function(n) { Levels(data[[n]], concatenate = T) } ))
    names(codebook) <- c("Variable", "Variable Stata", "Label", "Levels")
  } else {
    data <- data[, keep]
    codebook <- data.frame(names(data), sapply(names(data), function(n) return(Label(data[[n]]))), sapply(names(data), function(n) { Levels(data[[n]], concatenate = T) } ))
    names(codebook) <- c("Variable", "Label", "Levels")
  }
  write_csv(codebook[keep,], file)
}
#' export_stats_desc <- function(data, file, miss = TRUE, sorted_by_n = FALSE, return = FALSE, fill_extern = FALSE) {
#'   original_width <- getOption("width")
#'   options(width = 10000)
#'   des <- des_miss <- nb_miss <- labels <- n <- c()
#'   for (i in 1:length(data)) {
#'     decrit_i <- capture.output(print(decrit(data[[i]], miss=TRUE)))
#'     n <- c(n, as.numeric(sub('[[:blank:]]*([[:digit:]]*).*', '\\1', decrit_i[3])))
#'     des_i <- gsub(".*<br>        n","        n",gsub(".*<br>       n","       n",gsub(".*<br>      n","      n",gsub("<br><br>lowest.*","",paste(capture.output(print(decrit(data[[i]]))), collapse='<br>')))))
#'     if (str_count(des_i, fixed("),"))>1) { des_i <- gsub("),",")<br>",des_i) }
#'     des_miss_i <-  gsub(".*<br>        n","        n",gsub(".*<br>       n","       n",gsub(".*<br>      n","      n",gsub("<br><br>lowest.*","",paste(decrit_i, collapse='<br>')))))
#'     if (str_count(des_miss_i, fixed("),"))>1) { des_miss_i <- gsub("),",")<br>",des_miss_i) }
#'     nb_miss_i <- gsub(".*<br>        n","        n",gsub(".*<br>       n","       n",gsub(".*<br>      n","      n",gsub("<br><br>lowest.*","",paste(decrit_i[1:3], collapse='<br>')))))
#'     des <- c(des, des_i)
#'     des_miss <- c(des_miss, des_miss_i)
#'     nb_miss <- c(nb_miss, nb_miss_i)
#'     if (Label(data[[i]])=='') label_i <- colnames(data)[i]
#'     else label_i <- paste(colnames(data)[i], sub('[^:]*:', '', Label(data[[i]])), sep=':')
#'     labels <- c(labels, label_i)
#'   }
#'
#'   if (miss=='both') output <- matrix(c(names(data), labels, des, des_miss), ncol=4)
#'   if (miss=='nb') output <- matrix(c(names(data), labels, nb_miss), ncol=3)
#'   else if (sorted_by_n) output <- matrix(c(names(data), labels, des_miss), ncol=3)[order(-n),]
#'   else if (miss) output <- matrix(c(names(data), labels, des_miss), ncol=3)
#'   else output <- matrix(c(names(data), labels, des), ncol=3)
#'   write.table(output, file=file, sep=";;;", row.names=FALSE, col.names=FALSE, quote=FALSE)
#'   options(width = original_width)
#'   if (fill_extern) {
#'     nb_reponses <<- n
#'     nb_manquants <<- des_miss     }
#'   if (return) return(output)
#' }
reg_formula <- function(dep_var, indep_vars) return(as.formula(paste(dep_var, "~", paste(indep_vars, collapse = '+'))))
desc_table <- function(dep_vars, filename = NULL, data = e, indep_vars = control_variables, indep_labels = NULL, weights = data$weight, add_lines = NULL, model.numbers = T, multicolumn = T, #!mean_above,
                       save_folder = "../tables/", dep.var.labels = NULL, dep.var.caption = c(""), digits= 3, mean_control = FALSE, logit = FALSE, atmean = T, robust_SE = T, omit = c("Constant", "Race: Other"),
                       mean_above = T, only_mean = F, keep = indep_vars, nolabel = F, indep_vars_included = T, no.space = T, print_regs = FALSE, replace_endAB = NULL, oecd_latex = FALSE) {
  # Wrapper for stargazer
  # /!\ always run first with nolabel = T to check that the order of indep_labels correspond to the one displayed
  # dep_vars: either a variable name (automatically repeated if needed) or a list of variable names (of length the number of columns)
  # (in)dep_vars accept expressions of type : var_name expression (e.g. "equal_quota %in% 0:1", but not "equal_quota == 0 | equal_quota==1)
  # /!\ Interaction terms should be added with :, not *. There is a bug if the interaction terms are not at the end of indep_vars, also a bug if the unused indep variables in the first regression are not at the end
  # /!\ To appear in the table, they should be written without parentheses and with due space, e.g. "var > 0" and not "(var>0)"
  # indep_vars is the vector of potential covariates, they are by default all included by
  # indep_vars_included can be set to a list (of length the number of columns) of booleans or variable names to specify which covariates to include in each column
  # keep is a vector of regular expressions allowing to specify which covariates to display (by default, all except the Constant)
  # mean_above=T displays the mean of the dependant var (for those which treatment=="control" if mean_control = T) at top rather than bottom of Table (only_mean=T only displays that)
  # logit is either a boolean or a boolean vector
  # data can be a list of data frames instead of a single one
  if (missing(dep.var.labels) & !(is.character(dep_vars))) dep.var.labels <- dep_vars
  dep.var.labels.include <- ifelse(is.null(dep.var.labels), F, T)
  names(indep_vars) <- indep_vars
  if (class(indep_vars_included)=="list") { if (length(dep_vars)==1) dep_vars <- rep(dep_vars[1], length(indep_vars_included))  }
  else { indep_vars_included <- rep(list(rep(T, length(indep_vars))), length(dep_vars)) }
  if (length(logit) == 1) logit <- rep(logit, length(dep_vars))
  # if (length(indep_labels) > length(indep_vars)) indep_labels <- indep_labels[indep_vars]
  models <- coefs <- SEs <- list()
  means <- c()
  for (i in seq_along(dep_vars)) {
    df <- if (is.data.frame(data)) data else data[[i]]
    if (is.character(weights) & !is.data.frame(data)) weight <- df[[weights]]
    if (is.character(indep_vars_included[[i]])) indep_vars_included[[i]] <- indep_vars %in% indep_vars_included[[i]]
    formula_i <- as.formula(paste(dep_vars[i], "~", paste("(", indep_vars[indep_vars_included[[i]] & covariates_with_several_values(data = df, covariates = indep_vars)], ")", collapse = ' + ')))
    if (logit[i]) {
      models[[i]] <- glm(formula_i, data = df, family = binomial(link='logit'))
      logit_margin_i <- logitmfx(formula_i, data = df, robust = robust_SE, atmean = atmean)$mfxest # TODO! weights with logit; NB: logitmfx computes White/robust SE when robust_SE == T
      coefs[[i]] <- logit_margin_i[,1]
      SEs[[i]] <- logit_margin_i[,2]
    }
    else {
      models[[i]] <- lm(formula_i, data = df, weights = weight)
      coefs[[i]] <- models[[i]]$coefficients
      if (robust_SE) SEs[[i]] <- coeftest(models[[i]], vcov = vcovHC(models[[i]], "HC1"))[,2]
      else SEs[[i]] <- summary(models[[i]])$coefficients[,2]
    }
    if (print_regs) print(summary(models[[i]]))
    if (mean_control==FALSE){
      means[i] <- round(wtd.mean(eval(parse(text = paste( "df$", parse(text = dep_vars[i]), sep=""))), weights = weight, na.rm = T), d = digits)
      mean_text <- "Mean"
    } else {
      # means[i] <- round(wtd.mean(eval(parse(text = paste( "(df$", parse(text = dep_vars[i]), ")[df$treatment=='None']", sep=""))), weights = weights[df$treatment=='None'], na.rm = T), d = digits)
      means[i] <- round(wtd.mean(eval(parse(text = paste( "(df$", parse(text = dep_vars[i]), ")[df$branch_gcs=='nothing']", sep=""))), weights = weight[df$branch_gcs=='nothing'], na.rm = T), d = digits)
      mean_text <- "Control group mean"
    }
  }
  if (missing(filename)) file_path <- NULL
  else file_path <- paste0(save_folder, filename, ".tex")
  keep <- gsub("(.*)", "\\\\\\Q\\1\\\\\\E", sub("^\\(", "", sub("\\)$", "", keep)))
  if (exists("labels_vars") & is.null(indep_labels)) {
    if (!is.data.frame(data)) data <- data[[1]]
    model_total <- lm(as.formula(paste(dep_vars[1], "~", paste("(", indep_vars[covariates_with_several_values(data = data, covariates = indep_vars)], ")", collapse = ' + '))), data = data)
    indep_labels <- create_covariate_labels(names(model_total$coefficients)[-1], regressors_names = labels_vars, keep = keep, omit = "Constant")

    # i_max <- max_i <- 0
    # for (i in seq_along(models)) {
    #   if (length(models[[i]]$coefficients) > max_i) {
    #     max_i <- length(models[[i]]$coefficients) # TODO!: /!\ pb: won't display the appropriate labels if some coefficients are missing in the regression with most covariates (e.g. due to keep or omit)
    #     i_max <- i } }
    # indep_labels <- labels_vars[names(models[[i_max]]$coefficients)[-1]]
    # names(indep_labels) <- names(models[[i_max]]$coefficients)[-1]
    # # if (!missing(keep)) indep_labels <- indep_labels[grepl(keep, indep_vars)]
  }
  if (only_mean) mean_above <- T
  table <- do.call(stargazer, c(models, list(out=file_path, header=F, model.numbers = model.numbers,
                                             covariate.labels = if (nolabel) NULL else gsub("\\{", "{", gsub("\\}", "}", gsub("\\$", "$", gsub("\\textbackslash ", "\\", latexify(indep_labels, doublebackslash = FALSE), fixed = T), fixed = T), fixed = T), fixed = T), 
                                             add.lines = if (!"\\QConstant\\E" %in% keep) list(c(mean_text, means)) else NULL,
                                             coef = coefs, se = SEs, 
                                             dep.var.labels = dep.var.labels, dep.var.caption = dep.var.caption, dep.var.labels.include = dep.var.labels.include,
                                             multicolumn = multicolumn, float = F, keep.stat = c("n", "rsq"), omit.table.layout = "n", keep=keep, no.space = no.space
  )))
  print(table)
  if (!missing(replace_endAB) & length(table) != 54) warning(paste0("Wrong specification for replacement of the last lines: table of length ", length(table)))
  if (!missing(replace_endAB) & length(table) == 54) table <- c(table[1:46], replace_endAB)
  if (!nolabel) table <- table_mean_lines_save(table, mean_above = mean_above, only_mean = only_mean, indep_labels = indep_labels, indep_vars = indep_vars, add_lines = add_lines, file_path = file_path, oecd_latex = oecd_latex, nb_columns = length(indep_vars_included), omit = omit)
  return(table)
}
multi_grepl <- function(patterns, vec) return(1:length(vec) %in% sort(unlist(lapply(patterns, function(x) which(grepl(x, vec))))))
table_mean_lines_save <- function(table, mean_above = T, only_mean = FALSE, indep_vars = NULL, indep_labels = indep_vars, add_lines = NULL, file_path = NULL, oecd_latex = FALSE, nb_columns = 2, omit = c("Constant", "Gender: Other", "econ_leaningPNR", "Race: Other")) {
  if (mean_above) {
    mean_line <- regmatches(table, regexpr('(Mean|Control group mean) &[^\\]*', table))
    first_lab <- ifelse(missing(indep_labels), latexify(indep_vars[1], doublebackslash = FALSE), paste0(latexify(indep_labels[1], doublebackslash = FALSE), " &")) # was: latexify(ifelse(missing(indep_labels), indep_vars[1], indep_labels[1]))
    if (only_mean) { # removes coefs and leaves only the mean in the table
      table <- write_clip(gsub(paste(first_lab, ".*"), paste(mean_line, '\\\\\\\\'), table), collapse=' ')
      table <- table[c(1:grep('(Mean|Control group mean) &[^\\]*', table)[1], (length(table)-3):length(table))]
    } else {
      table <- gsub('(Mean|Control group mean) &.*', '', table)
      table <- c(table[1:(grep(first_lab, table, fixed = T)[1]-1)], paste(mean_line, '\\\\ \\hline \\\\[-1.8ex]'), table[grep(first_lab, table, fixed = T)[1]:length(table)]) } # Before: fixed = F
  }
  for (l in add_lines) {
    line <- if (length(gregexpr("&", l[2])[[1]]) == nb_columns) l[2] else { if (grepl("\\hline", table[as.numeric(l[1])+0*mean_above-1])) c("\\\\[1ex]", l[2]) else c(" \\\\[1ex] \\hline \\\\[1ex]",  paste("\\multicolumn{", nb_columns + 1, "}{l}{\\textbf{", l[2], "}} \\\\")) }
    table <- c(table[1:(as.numeric(l[1])+0*mean_above-1)], line, table[(as.numeric(l[1])+0*mean_above):length(table)]) }
  if (length(omit) > 0) {
    omit_constant <- any(c("Constant", "(Intercept)") %in% omit)
    omit <- omit[!omit %in% c("Constant", "(Intercept)")]
    omitted_vars <- which(c(multi_grepl(omit, table), multi_grepl(indep_labels[omit], table)))
    if (omit_constant) for (i in 1:length(table)) if (multi_grepl(c("Constant", "(Intercept)"), table[i]) & grepl("^  \\& \\(", table[i+1])) omitted_vars <- c(omitted_vars, i)
    if (length(omitted_vars) > 0) table <- table[-c(omitted_vars, omitted_vars + 1)] }
  cat(paste(table, collapse="\n"), file = file_path)
  if (oecd_latex) cat(paste(table, collapse="\n"), file = sub("../", "../../oecd_latex/", file_path, fixed = T))
  return(table)
}
covariates_with_several_values <- function(data, covariates) { # data is a data.frame and covariates a string list of variables, allowing those of the form "(example > 0)"
  several_values <- c()
  for (c in seq_along(covariates)) {
    if (grepl("[:*]", covariates[c])) nb_values_c <- length(unique(eval(str2expression(sub(".*[:*](.*)", "data$\\1", covariates[c]))))) * length(unique(eval(str2expression(sub("(.*)[:*].*", "data$\\1", covariates[c])))))
    else nb_values_c <- length(unique(eval(str2expression(sub("((.*\\()*)(.*)", "\\1data$\\3", covariates[c])))))
    if (nb_values_c == 0 & j == 0) warning(paste(covariates[c], "is not in the dataset"))
    several_values[c] <- nb_values_c > 1 }
  return(several_values)
}
same_reg_subsamples <- function(dep.var, dep.var.caption = NULL, covariates = setA, data = all, data_list = NULL, along = "country3", along.levels = Levels(along, data), weights = "weight",
                                covariate.labels = NULL, nolabel = FALSE, include.total = FALSE, add_lines = NULL, mean_above = T, only_mean = FALSE, constant_instead_mean = T,
                                mean_control = T, dep.var.label = dep.var, logit = FALSE, robust_SE = T, atmean = T, keep = NULL, display_mean = FALSE, share_na_remove = 0.01,
                                omit = c("Constant", "Gender: Other", "econ_leaningPNR"), print_regs = FALSE, no.space = T, filename = dep.var, omit.note = FALSE, dep_var_labels = NULL,
                                folder = "../tables/regs_countries/", digits= 3, model.numbers = T, replace_endAB = NULL) {
  file_path <- paste(folder, filename, ".tex", sep="")
  if (constant_instead_mean) display_mean <- T
  # keep <- gsub("(.*)", "\\\\\\Q\\1\\\\\\E", sub("^\\(", "", sub("\\)$", "", keep)))
  # formula <- as.formula(paste(dep.var, "~", paste("(", covariates, ")", collapse = ' + ')))
  if (along == "country3") {
    dep.var <- sub("index_", "index_c_", dep.var)
    covariates <- gsub("index_", "index_c_", covariates)
    if (!missing(keep)) keep <- gsub("index_", "index_c_", keep) }
  models <- coefs <- SEs <- list()
  means <- c()
  if (is.null(data_list)) data_list <- c(list(data), lapply(along.levels, function(j) return(data[data[[along]] == j, ])))
  for (j in c(0:(length(data_list)-1))) {
    data_i <- data_list[[j+1]]
    covariates_i <- covariates[sapply(covariates, function(v) { mean(is.na(data_i[[v]])) <= share_na_remove })]
    covariates_i <- covariates_i[covariates_with_several_values(data = data_i, covariates = covariates_i)]
    formula_i <- as.formula(paste(dep.var, "~", paste("(", covariates_i, ")", collapse = ' + ')))
    i <- j+1
    if (logit) {
      models[[i]] <- glm(formula_i, data = data_i, family = binomial(link='logit'))
      logit_margin_i <- logitmfx(formula_i, data = data_i, robust = robust_SE, atmean = atmean)$mfxest # TODO! weights with logit; NB: logitmfx computes White/robust SE when robust_SE == T
      coefs[[i]] <- logit_margin_i[,1]
      SEs[[i]] <- logit_margin_i[,2]
    } else {
      models[[i]] <- lm(formula_i, data = data_i, weights = data_i[[weights]])
      coefs[[i]] <- models[[i]]$coefficients
      if (robust_SE) SEs[[i]] <- coeftest(models[[i]], vcov = vcovHC(models[[i]], "HC1"))[,2]
      else SEs[[i]] <- summary(models[[i]])$coefficients[,2]
    }
    if (print_regs) print(summary(models[[i]]))
    if (constant_instead_mean & (include.total | j > 0)) means <- c(means, round(coefs[[i]][["(Intercept)"]], digits))
    else if (include.total | j > 0) means <- c(means, ifelse(mean_control, round(wtd.mean(eval(parse(text = paste( "(data_i$", parse(text = dep.var), ")[data_i$treatment=='None']", sep=""))), weights = data_i[[weights]][data_i$treatment=='None'], na.rm = T), d = digits),
                                                        round(wtd.mean(eval(parse(text = paste( "data_i$", parse(text = dep.var), sep=""))), weights = data_i[[weights]], na.rm = T), d = digits)))
  }
  mean_text <- ifelse(mean_control, "Control group mean", ifelse(constant_instead_mean, "Constant", "Mean"))
  if (exists("labels_vars")) omit <- c(omit, labels_vars[omit], names(labels_vars)[which(labels_vars %in% omit)])
  if (exists("labels_vars")) omit <- omit[!is.na(omit)]
  if (exists("labels_vars") & missing(covariate.labels)) covariate_labels <- create_covariate_labels(unique(names(unlist(lapply(1:length(models), function(i) return(models[[i]]$coefficients))))), regressors_names = labels_vars, keep = keep, omit = omit)
  if (nolabel) covariate_labels <- NULL
  if (!nolabel & !missing(covariate.labels)) covariate_labels <- covariate.labels
  if (is.null(keep)) keep <- c(c(covariates)[!covariates %in% omit], if (any(c("(Intercept)", "Intercept", "Constant") %in% omit)) NULL else "Constant")
  dep.var.caption <- ifelse(missing(dep.var.caption), ifelse(exists("labels_vars") && dep.var %in% names(labels_vars), labels_vars[dep.var], gsub("_", "\\_", dep.var, fixed = T)), dep.var.caption)
  
  table <- do.call(stargazer, c(if (include.total) models else models[-1], list(out=NULL, header=F, model.numbers = model.numbers, 
                covariate.labels = covariate_labels, coef = if (include.total) coefs else coefs[-1], se = if (include.total) SEs else SEs[-1], add.lines = if (display_mean) list(c(mean_text, means)) else NULL,
                dep.var.labels = if (is.null(dep_var_labels)) {if (include.total) c("All", along.levels) else along.levels} else dep_var_labels, 
                dep.var.caption = dep.var.caption, multicolumn = F, float = F, keep.stat = c("n", "rsq"), 
                omit.table.layout = if (omit.note) "n" else NULL, keep=keep, omit = omit, no.space = no.space)))

  if (!missing(replace_endAB) & length(table) != 50) warning(paste0("Wrong specification for replacement of the last lines: table of length ", length(table)))
  if (!missing(replace_endAB) & length(table) == 50) table <- c(table[1:43], replace_endAB)
  table <- table_mean_lines_save(table, omit = omit, mean_above = mean_above, only_mean = only_mean, indep_labels = covariate_labels, indep_vars = covariates, add_lines = add_lines, file_path = file_path, oecd_latex = FALSE, nb_columns = length(along.levels) + include.total)
  return(table)
}
create_covariate_labels <- function(coefs_names, regressors_names = labels_vars, keep = NULL, omit = "Constant") {
  if (any(c("Constant", "Intercept", "(Intercept)") %in% omit) & coefs_names[1] %in% c("Constant", "Intercept", "(Intercept)")) coefs_names <- coefs_names[-1]
  missing_names <- setdiff(coefs_names, names(labels_vars))
  names(missing_names) <- missing_names
  if (length(missing_names) > 0) warning(paste("The following variables are missing from labels_vars (so their name will appear instead of their label):", paste(missing_names, collapse = ", ")))
  covariate.labels <- c(labels_vars, missing_names)[coefs_names]
  names(covariate.labels) <- coefs_names
  if (is.null(keep)) covariate_labels <- covariate.labels
  else covariate_labels <- c()
  for (k in keep) covariate_labels <- c(covariate_labels, covariate.labels[(grepl(k, covariate.labels) | grepl(k, names(covariate.labels)))])
  for (k in omit) covariate_labels <- covariate_labels[!grepl(k, covariate_labels) & !grepl(k, names(covariate_labels))]
  covariate_labels <- unique(covariate_labels[c(which(!grepl("[:*]", names(covariate_labels))), which(grepl("[:*]", names(covariate_labels))))]) # unique is not necessary, it unnames the vector
  if (covariate_labels[1] %in% c("Constant", "Intercept", "(Intercept)")) covariate_labels <- c(covariate_labels[-1], covariate_labels[1])
  return(covariate_labels)
}
#' CImedian <- function(vec) { # 95% confidence interval
#'   res <- tryCatch(unlist(ci.median(vec[!is.na(vec) & vec!=-1])), error=function(e) {print('NA')})
#'   return(paste(res[paste('ci.lower')], res[paste('ci.median')], res[paste('ci.upper')], length(which(!is.na(vec) & vec!=-1))))
#' }
#' clean_number <- function(vec, high_numbers='') {
#'   numeric_vec <- as.numeric(gsub(",", ".", gsub("[[:alpha:]  !#$%&')?/(@:;€_-]","",vec)))
#'   if (high_numbers=='remove') { is.na(numeric_vec) <- numeric_vec>10000 }
#'   else if (high_numbers=='divide') { numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)] <- numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)]/12 }
#'   else if (high_numbers=='divide&remove') {
#'     numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)] <- numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)]/12
#'     is.na(numeric_vec) <- numeric_vec>6000 }
#'   return(numeric_vec)
#' }
#' uc <- function(nb_pers, nb_14_et_plus) {
#'   # https://www.insee.fr/fr/metadonnees/definition/c1802
#'   return(1 + 0.5 * pmax(0, nb_14_et_plus - 1) + 0.3 * pmax(0, nb_pers - nb_14_et_plus))
#' }
#' quotient <- function(nb_pers, nb_adultes) {
#'   # https://droit-finances.commentcamarche.com/contents/907-quotient-familial-calcul-du-nombre-de-parts-fiscales
#'   # nb de parts fiscales en fonction de la situation et de nb_pers = 1 / 2 / 3 / 4 / 5
#'   # marie: x / 2 / 2.5 / 3 / 4 --- en concubinage: x / 1 / 1.5 /2 / 3 (= marie - 1) --- seul: 1 / 2 / 2.5 / 3.5 / 4.5
#'   return((nb_pers == 1) + (nb_pers == 2)*2 + (nb_pers == 3)*2.5 + (nb_pers == 4)*3 + (nb_pers > 4)*pmin(6, nb_pers - 1) + (nb_adultes==1)*(nb_pers > 3)*0.5 )
#' }
#' irpp <- function(rev, nb_adultes, nb_pers) {
#'   # quotient <- (nb_pers < 2) + (nb_pers == 2) * 2 + (nb_pers == 3) * 2.5 + (nb_pers == 4) * 3 + (nb_pers > 4) * pmin(6, nb_pers - 1)
#'   income <- 0.9334 * rev / quotient(nb_pers, nb_adultes) # (1 + (0.029 * 1.28))*0.9 : passage au brut (+28% en moyenne), CSG+CRDS non déductibles (2,90%), puis abattement de 10%
#'   ir <- 0
#'   ir <- ir + (income - 12815.25*12) * 0.45 * (income > 12815.25*12)
#'   ir <- ir + (pmin(income, 12676*12) - 6051.42*12) * 0.41  * (income > 6051.42*12)
#'   ir <- ir + (pmin(income, 6051.42*12) - 2257.17*12) * 0.3  * (income > 2257.17*12)
#'   ir <- ir + (pmin(income, 2257.17*12) - 817.25*12) * 0.14  * (income > 817.25*12)
#'
#'   ir <- quotient(nb_pers, nb_adultes) * ir
#'   seuil_decote <- (nb_adultes>1)*2585/12 + (nb_adultes<=1)*1569/12
#'   # decote <- (1920 - 0.75 * ir) * (marie & ir<2560) + (1165 - 0.75 * ir) * (!(marie) & ir<1553)
#'   decote <- (ir < seuil_decote) * 0.75 * (seuil_decote - ir)
#'   return(pmax((ir-decote),0)) # vrai calcul
#' }
representativity_index <- function(weights, digits = 3) { return(round(sum(weights)^2/(length(weights)*sum(weights^2)), 3)) }
#'
#'
#' ##### Graphiques #####
#' stack_bars <- function(vars, data=s, miss=T, labels=NA, title=NA, accord=FALSE, en = FALSE, margin=c(2.5,17,0,3), cex=1, width=0.77/length(vars), weights=FALSE) {
#'   matrice <- c()
#'   colors <-   c(rainbow(4, end=4/15)[1:3], "green", "forestgreen") # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
#'   for (var in vars) {
#'     if (miss) {
#'       mat <- c(length(which(data[[var]]==-2))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==-1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==0))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==2))/length(which(!is.missing(data[[var]]))),length(which(is.missing(data[[var]]) & !is.na(data[[var]])))/length(which(!is.missing(data[[var]]) & !is.na(data[[var]]))))
#'       if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]==-2)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==-1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==0)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==2)])/sum(data[['weight']][!is.missing(data[[var]])]),sum(data[['weight']][which(is.missing(data[[var]]) & !is.na(data[[var]]))])/sum(data[['weight']][!is.missing(data[[var]])])) }
#'       colors <- c(colors, "lightgrey")    }
#'     else {
#'       mat <- c(length(which(data[[var]]==-2))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==-1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==0))/length(which(!is.missing(data[[var]]))),  length(which(data[[var]]==1))/length(which(!is.missing(data[[var]]))),  length(which(data[[var]]==2))/length(which(!is.missing(data[[var]]))))
#'       if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]==-2)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==-1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==0)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==2)])/sum(data[['weight']][!is.missing(data[[var]])])) } }
#'     matrice <- c(matrice, mat) }
#'   matrice <- matrix(c(matrice), ncol=length(vars))
#'   if (is.na(labels)) { labels <- vars }
#'   if (accord) { values <- c("Pas du tout", "Pas vraiment d'accord", "Indifférent-e", "Assez", "Tout à fait d'accord")
#'   if (miss) { widths <- c(0.16,0.16,0.13,0.125,0.145,0.05) }
#'   else { widths <- c(0.18,0.185,0.15,0.14,0.2) } }
#'   else { values <- c("Baisser fortement", "légèrement", "Maintenir", "Augmenter légèrement", "fortement")
#'   if (miss) { widths <- c(0.153,0.14,0.14,0.15,0.083,0.05) }
#'   else { widths <- c(0.173,0.16,0.165,0.19,0.095) } }
#'   if (en) {values <- c("Strongly decrease", "Slightly decrease", "Maintain", "Slightly increase", "Strongly increase")
#'   if (accord) values <- c("Totally disagree", "Disagree", "Indifferent", "Agree", "Totally agree")
#'   if (miss) { widths <- c(0.16,0.15,0.14,0.13,0.12,0.06) }
#'   else { widths <- c(0.173,0.16,0.165,0.19,0.095) } }
#'   if (miss) {
#'     if (en) values <- c(values, "PNR")
#'     else values <- c(values, "NSP") }
#'   # if (accord) { values <- c("Pas du tout d'accord", "Pas vraiment d'accord", "Indifférent-e", "Assez d'accord", "Tout à fait d'accord") }
#'   # else { values <- c("Baisser fortement", "Baisser légèrement", "Maintenir au niveau actuel", "Augmenter légèrement", "Augmenter fortement") }
#'   # if (miss) { values <- c(values, "NSP (Ne sait pas, ne se prononce pas)")} # TODO: trouver widths pour ceux-là et les mettre
#'   before_par <- par()
#'   titre <- 0
#'   if (!is.na(title)) { titre <- 1.5 }
#'   par(mar=margin, oma=c(0,0,titre,0))
#'   frame()
#'   abline(v=seq(0,1,by=0.1), lty=3, col="grey")
#'   axis(1, at = seq(0,1,by=0.1))
#'   barplot(matrice, width=width, horiz=TRUE, add=TRUE, col=colors, names.arg = labels, cex.names = cex, border=NA, ylim=c(0,1), legend.text=values, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
#'   title(title, outer=TRUE)
#'   par(before_par)
#'   # legend("topright", fill=colors, legend=values, ncol=2)
#' }
#' oui_non <- function(vars, file, labels = vars, data = s, display_value = T, sort=T, colors=color(2), weights=T, margin_r=0, margin_l=NA, title="", en=FALSE, NSP=FALSE) { # 250 l
#'   margin_t <- 30
#'   if (title!="") { margin_t <- 80 }
#'   if (grepl("<br>", title)) { margin_t <- 130 }
#'   if (is.na(margin_l)) { margin_l <- 4.7*max(nchar(labels)/(1 + str_count(labels, '<br>'))) }
#'   oui <- non <- nsp <- c()
#'   for (var in vars) {
#'     if (weights) {
#'       oui <- c(oui, sum(data[['weight']][which(data[[var]]==T | data[[var]]=="Oui" | data[[var]]=="Pour" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="quotient")])/sum(data[['weight']][which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) )
#'       non <- c(non, sum(data[['weight']][which(data[[var]]==FALSE | data[[var]]=="Non" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="indiv")])/sum(data[['weight']][which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) )
#'       nsp <- c(nsp, sum(data[['weight']][which(data[[var]]=="NSP" | data[[var]]==-1)])/sum(data[['weight']][which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) ) #  | data[[var]]==-1 | data[[var]]=="NSP"
#'     }
#'     else {
#'       oui <- c(oui, length(which(data[[var]]==T | data[[var]]=="Oui" | data[[var]]=="Pour" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="quotient"))/length(which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
#'       non <- c(non, length(which(data[[var]]==FALSE | data[[var]]=="Non" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="indiv"))/length(which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
#'       nsp <- c(nsp, length(which(data[[var]]=="NSP" | data[[var]]==-1))/length(which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
#'     }
#'   }
#'   true_nsp <- round(100 * nsp*(oui+non))
#'   oui <- round(100 * oui)
#'   non <- round(100 * non)
#'   nsp <- round(100 * nsp)
#'   if (sort) order_as <- order(oui/(oui+non))
#'   else order_as <- 1:length(oui)
#'   y <- labels[order_as]
#'   non <- non[order_as]
#'   nsp <- nsp[order_as]
#'   true_nsp <- true_nsp[order_as]
#'   if (sort) oui <- sort(oui)
#'   o <- round(100 * oui / (oui + non))
#'   n <- round(100 * non / (oui + non))
#'
#'   if (en==T) {
#'     hover_oui <- paste('Yes<br>', oui, '% of answers<br>', o, '% of expressed answers')
#'     hover_non <- paste('No<br>', non, '% of answers<br>',n, '% of expressed answers')
#'     hover_nsp <- paste('PNR<br>', true_nsp, '% of answers')
#'     Text <- c("Yes", "No", "PNR")      }
#'   else if (en==FALSE) {
#'     hover_oui <- paste('Oui<br>', oui, '% des réponses<br>', o, '% des réponses exprimées')
#'     hover_non <- paste('Non<br>', non, '% des réponses<br>',n, '% des réponses exprimées')
#'     hover_nsp <- paste('NSP<br>', true_nsp, '% des réponses')
#'     Text <- c("Oui", "Non", "NSP") }
#'   else {
#'     hover_oui <- paste('Oui<br>', oui, '% des réponses<br>', o, '% des réponses exprimées')
#'     hover_non <- paste('Non<br>', non, '% des réponses<br>',n, '% des réponses exprimées')
#'     hover_nsp <- paste('NSP<br>', true_nsp, '% des réponses')
#'     Text <- en
#'     if (length(Text) ==2) Text <- c(Text, 'PNR')}
#'   if (display_value) {
#'     hover_oui <- paste(oui, '%')
#'     hover_non <- paste(non, '%')
#'     hover_nsp <- paste(true_nsp, '%')
#'   }
#'   if (!(NSP)) Text[3] <- ''
#'   print(oui)
#'   print(non)
#'   print(nsp)
#'   print(o)
#'   print(n)
#'   data <- data.frame(y, oui, non, nsp, o, n)
#'   data$y <- factor(data$y, levels = data[["y"]])
#'   y <- c(y, '')
#'   bars <- plot_ly(data, x = ~o, y = ~y, type = 'bar', orientation = 'h', text = hover_oui, textposition = 'auto', # last one displays values; colors were forestgreen and darkred
#'                   hoverinfo = 'text', marker = list(color = colors[1], line = list(color = 'white', width = 1))) %>%
#'     add_trace(x = ~n, text = hover_non, hoverinfo = 'text', marker = list(color = colors[2])) %>%
#'     add_trace(x = ~nsp, text = hover_nsp, hoverinfo = 'text', marker = list(color = 'lightgrey')) %>%
#'     layout(xaxis = list(title = "",
#'                         showgrid = FALSE,
#'                         showline = FALSE,
#'                         showticklabels = FALSE,
#'                         zeroline = FALSE,
#'                         domain = c(0.15, 1)),
#'            yaxis = list(title = "",
#'                         showgrid = FALSE,
#'                         showline = FALSE,
#'                         showticklabels = FALSE,
#'                         zeroline = FALSE),
#'            hovermode = 'closest',
#'            barmode = 'stack',
#'            title = title,
#'            titlefont = list(color='black'),
#'            font = list(color='black'),
#'            # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
#'            margin = list(l = margin_l, r = margin_r, t = margin_t, b = 0),
#'            showlegend = FALSE) %>%
#'     # labeling the y-axis
#'     add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
#'                     xanchor = 'right',
#'                     text = y,
#'                     font = list(family = 'Arial', size = 14, color = 'black'),
#'                     showarrow = FALSE, align = 'right') %>%
#'     # labeling the first Likert scale (on the top)
#'     add_annotations(xref = 'x', yref = 'paper',
#'                     x = c(10, 90, 110),
#'                     y = 1.1,
#'                     text = Text,
#'                     font = list(family = 'Arial', size = 15, color = 'black'),
#'                     showarrow = FALSE) # %>%
#'   # labeling the percentages of each bar (x_axis)
#'   # add_annotations(xref = 'x', yref = 'y',
#'   #                 x = o / 2, y = y,
#'   #                 text = paste(data[,"oui"], '%'),
#'   #                 font = list(family = 'Arial', size = 14, color = 'white'),
#'   #                 showarrow = FALSE) %>%
#'   # add_annotations(xref = 'x', yref = 'y',
#'   #                 x = o + n / 2, y = y,
#'   #                 text = paste(data[,"non"], '%'),
#'   #                 font = list(family = 'Arial', size = 14, color = 'white'),
#'   #                 showarrow = FALSE) %>%
#'   # add_annotations(xref = 'x', yref = 'y',
#'   #                 x = o + n + nsp / 2, y = y,
#'   #                 text = paste(data[,"nsp"], '%'),
#'   #                 font = list(family = 'Arial', size = 14, color = 'white'),
#'   #                 showarrow = FALSE) %>%
#'   # api_create(bars, filename=file, sharing="public")
#'   return(bars) # bugs most often than not
#' }
#' data5 <- function(vars, data=e, miss=T, weights=T, rev=FALSE) {
#'   matrice <- c()
#'   colors <-  c(rainbow(4, end=4/15), "forestgreen") # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
#'   for (var in vars) {
#'     if (miss) {
#'       if (is.null(annotation(data[[var]]))) {
#'         mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))),length(which(is.na(data[[var]])))/length(which(!is.missing(n(data[[var]]))))) # removed "n()"
#'         if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]),sum(data[['weight']][which(is.na(data[[var]]))])/sum(data[['weight']][!is.missing(n(data[[var]]))])) } }
#'       else {
#'         mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))),length(which(is.missing(data[[var]]) & !is.na(data[[var]])))/length(which(!is.missing(data[[var]])))) # removed "n()"
#'         if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]),sum(data[['weight']][which(is.missing(data[[var]]) & !is.na(data[[var]]))])/sum(data[['weight']][!is.missing(data[[var]])])) } }
#'       colors <- c(colors, "lightgrey")    }
#'     else {
#'       mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))),  length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))),  length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))))
#'       if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))])) } }
#'     matrice <- c(matrice, mat) }
#'   matrice <- matrix(c(matrice), ncol=length(vars))
#'   if (rev & !(miss)) return(matrice[5:1,])
#'   else if (rev & miss) return(matrice[c(5:1,6),])
#'   else return(matrice)
#'   # return(as.data.frame(matrice))
#' }
data1 <- function(vars, data=e, weights=T) {
  if (is.null(data[['weight']])) weights <- F # TODO? warning
  res <- c()
  for (var in vars) {
    if (weights) { res <- c(res, sum(data[['weight']][which(data[[var]]==TRUE)])/sum(data[['weight']][which(data[[var]]==TRUE | data[[var]]==FALSE)])) }
    else { res <- c(res, length(which(data[[var]]==T))/length(which(data[[var]]==T | data[[var]]==FALSE))) }
  }
  return( matrix(res, ncol=length(vars)) )
}
dataN <- function(var, data=e, miss=T, weights = T, return = "", fr=F, rev=FALSE, rev_legend = FALSE, levels = NULL) {
  missing_labels <- c("NSP", "PNR", "Non concerné·e", "Included", "Don't know", "PNR or other", "NSP ou autre", "PNR ou autre", "PNR/Non-voter") # TODO: allow for non-standard PNR in a more straightforward way than adding the argument "fr" and putting its value below
  if (is.character(fr)) missing_labels <- c(missing_labels, fr)
  weight_var <- if (sum(!is.na(data$weight_country)) == nrow(data) && length(unique(data$country)) == 1) "weight_country" else "weight"
  if (is.null(data[[weight_var]])) weights <- F # TODO? warning
  mat <- c()
  if (is.character(data[[var]]) | (is.numeric(data[[var]]) & !any(grepl("item", class(data[[var]])))) | is.logical(data[[var]])) v <- as.factor(data[[var]]) # before: no is.logical
  else v <- data[[var]]
  # if (setequal(levels(v), c(T, F))) levels <- c(T) # before: not this line
  # else if (is.null(annotation(v))) levels <- levels(v)
  # else {
  #   levels <- labels(v)@.Data
  #   levels <- levels[levels %in% as.character(v)] # new, removes empty levels
  # }
  if (missing(levels)) levels <- Levels(v, max_values = Inf, names = T) # was Levels_data(v)
  levels <- levels[!(levels %in% missing_labels)]
  if (rev_legend) levels <- rev(levels) # new (05/20)
  if (weights) N <- sum(data[[weight_var]][!is.pnr(v) & (!(v %in% missing_labels))]) # c("NSP", "Non concerné·e")
  else N <- length(which(!is.pnr(v) & (!(v %in% missing_labels)))) # c("NSP", "Non concerné·e")
  for (val in levels) { # before: no %in% nowhere below
    if (weights) mat <- c(mat, sum(data[[weight_var]][which(v==val)])/N)
    else mat <- c(mat, length(which(v==val))/N) }
  if (rev) mat <- rev(mat)
  if (miss) {
    if (is.null(annotation(v))) {
      if (weights) mat <- c(mat, sum(data[[weight_var]][which(is.na(v) | v %in% missing_labels)])/N) # c("NSP", "Non concerné·e")
      else mat <- c(mat, length(which(is.na(v) | v %in% missing_labels))/N) # c("NSP", "Non concerné·e")
    } else  {
      if (weights) mat <- c(mat, sum(data[[weight_var]][which(is.pnr(v) & !is.na(v))])/N) # was defined without " & (!(v %in% c("NSP", "Non concerné·e")))" here and line below
      else mat <- c(mat, length(which(is.pnr(v) & !is.na(v)))/N) } } # mais ça semble équivalent pck les NSP sont missing dans ces cas-là
  if (max(nchar(levels))==3 & 'Oui' %in% levels & 'Non' %in% levels) { if (which(levels=='Non') < which(levels=='Oui')) mat[2:1] <- mat[1:2]; levels[c(which(levels=='Oui'),which(levels=='Non'))] <- c('Non', 'Oui') }
  if ((return %in% c("levels", "legend")) & miss & fr==TRUE) return(c(levels, 'NSP'))
  else if ((return %in% c("levels", "legend")) & miss & (fr==FALSE)) return(c(levels, 'PNR'))
  else if ((return %in% c("levels", "legend")) & miss & is.character(fr)) return(c(levels, fr))
  else if ((return %in% c("levels", "legend")) & (!(miss))) return(levels)
  else if (return == "N") return(N)
  else return(matrix(mat, ncol=1))
}
dataKN <- function(vars, data=e, miss=T, weights = T, return = "", fr=F, rev=FALSE, rev_legend = FALSE) {
  if (!any(vars %in% names(data))) {
    warning("Variable not found")
    out <- c()
  } else {
    if (length(vars) == 1) out <- dataN(vars, data = data, miss = miss, weights = weights, return = return, fr = fr, rev = rev, rev_legend = rev_legend)
    else if (is.logical(data[[vars[1]]])) {
      mat <- data1(vars, data, weights)
      levels <- dataN(vars[1], data = data, return = "legend")
      out <- if (return == "legend") levels else mat }
    else {
      values <- setNames(lapply(vars, function(v) dataN(v, data = data, miss = miss, weights = weights, return = "legend", fr = fr, rev = rev)), vars)
      nb_values <- sapply(values, length)
      levels <- values[[which(nb_values == max(nb_values))[1]]]
      if (!all(sapply(values, function(var) all(var %in% levels)))) {
        warning("No variable contains all possible values, this may create bugs")
        for (v in vars) levels <- union(levels, values[[v]]) }
      # The above code (after else) is new (and here to manage cases with different sets of levels for different variables)
      res <- c()
      for (var in vars) res <- c(res, dataN(var = var, data = data, miss = miss, weights = weights, return = "", fr = fr, rev = rev, levels = levels))
      mat <- matrix(res, ncol=length(vars))
      if (rev_legend) levels <- rev(levels)
      out <- if (return == "legend") levels else mat
    }
  }
  return(out)
}
#' dataN2 <- function(var, df = list(c, e), miss=T, weights = T, fr=F, rev=FALSE, return = "") {
#'   if (return %in% c("levels", "legend")) return(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev, return = return))
#'   else return(cbind(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev), dataN(var, df[[2]], miss = miss, weights = weights, fr = fr, rev = rev))) }
#' dataN3 <- function(var, df = list(e2, e, c), miss=T, weights = T, fr=F, rev=FALSE, return = "") {
#'   if (return %in% c("levels", "legend")) return(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev, return = return))
#'   else return(cbind(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev), dataN(var, df[[2]], miss = miss, weights = weights, fr = fr, rev = rev), dataN(var, df[[3]], miss = miss, weights = weights, fr = fr, rev = rev))) }
dataNK <- function(var, df = list(e2, e, c), miss=T, weights = T, fr=F, rev=FALSE, return = "") {
  if (return %in% c("levels", "legend")) return(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev, return = return))
  else {
    values <- lapply(df, function(d) dataN(var, data = d, miss = miss, weights = weights, return = "legend", fr = fr, rev = rev))
    nb_values <- sapply(values, length)
    levels <- values[[which(nb_values == max(nb_values))[1]]]
    if (!all(sapply(values, function(var) all(var %in% levels)))) {
      warning("No variable contains all possible values, this may create bugs")
      for (d in 1:length(df)) levels <- union(levels, values[[d]]) }
    # The above code (after else) is new (and here to manage cases with different sets of levels for different variables)
    return(do.call(cbind, lapply(df, function (d) {dataN(var, d, levels = levels, miss = miss, weights = weights, fr = fr, rev = rev)}))) } }
#' data12 <- function(vars, df = list(e, e2), miss=T, weights = T, fr=F, rev=FALSE, return = "") {
#'   if (length(vars)==1) return(dataN2(var=vars, df=list(df[[2]], df[[1]]), miss=miss, weights=weights, fr=fr, rev=rev, return=return))
#'   else {
#'     init <- T
#'     for (var in vars) {
#'       if (init) {
#'         data <- dataN2(var=var, df=list(df[[2]], df[[1]]), miss=miss, weights=weights, fr=fr, rev=rev, return=return)
#'         init <- F
#'       } else {
#'         data <- cbind(data, dataN2(var=var, df=list(df[[2]], df[[1]]), miss=miss, weights=weights, fr=fr, rev=rev, return=return))
#'       }
#'     }
#'     return(data)
#'   } }
#' barres12 <- function(vars, df=list(e, e2), labels, legend=hover, comp = "V2", orig = NULL, miss=T, weights = T, fr=F, rev=T, color=c(), rev_color = FALSE, hover=legend, sort=TRUE, thin=T, return="", showLegend=T, export_xls = F) {
#'   if (missing(vars) & missing(legend) & missing(hover)) warning('hover or legend must be given')
#'   if (!missing(miss)) nsp <- miss
#'   data1 <- dataKN(vars, data=df[[1]], miss=miss, weights = weights, return = "", fr=fr, rev=rev)
#'   if (missing(legend) & missing(hover)) {
#'     if (is.logical(df[[1]][[vars[1]]])) hover <- legend <- labels # data1(var = vars[1], data=df, weights = weights) # before: uncommented and "else" next line
#'     else hover <- legend <- dataN(var = vars[1], data=df[[1]], miss=miss, weights = weights, return = "legend", fr=fr, rev_legend = rev) }
#'   agree <- order_agree(data = data1, miss = miss)
#'   if (is.logical(df[[1]][[vars[1]]])) agree <- rev(agree)
#'   if (return=="data") return(data12(vars[agree], df = df, miss=miss, weights = weights, fr=fr, rev=rev, return = ""))
#'   else if (return=="labels") return(labels12(labels[agree], en = !fr, comp = comp, orig = orig))
#'   else if (return=="legend") return(legend)
#'   else return(barres(data = data12(vars[agree], df = df, miss=miss, weights = weights, fr=fr, rev=rev, return = ""),
#'                      labels=labels12(labels[agree], en = !fr, comp = comp, orig = orig), legend=legend,
#'                      miss=miss, weights = weights, fr=fr, rev=rev, color=color, rev_color = rev_color, hover=hover, sort=F, thin=thin, showLegend=showLegend, export_xls = export_xls))
#' }
#'
#' labels12 <- function(labels, en=F, comp = "V2", orig = NULL) {
#'   new_labels <- c()
#'   lab2 <- ifelse(comp=="V2", ifelse(en, " Wave 2 (W2)", " Vague 2 (V2)"), comp)
#'   lab1 <- ifelse(missing(orig), ifelse(en, " (W1)", " (V1)"), orig)
#'   for (l in labels) {
#'     new_labels <- c(new_labels, lab2, paste(l, lab1, sep=""))
#'     lab2 <- paste("", lab2) }
#'   return(new_labels)
#' }
labelsN <- function(labels, levels, parentheses = T) {
  new_labels <- c()
  if (parentheses) {
    labs_other <- paste0("(", levels[1:(length(levels)-1)], ")")
    labs_main <- paste0("<b>", labels, "</b><br>(", levels[length(levels)], ")")
  } else {
    double_dot <- ifelse(max(length(labels))>1, ": ", "")
    labs_other <- levels[1:(length(levels)-1)]
    labs_main <- paste0("<b>", labels, double_dot, "</b><br>", levels[length(levels)])  }
  for (l in seq_along(labels)) new_labels <- c(new_labels, labs_other, labs_main[l])
  return(rev(new_labels)) # version var (lev1) / (lev2) / ...
  # return(sapply(labels, function(l) {return(paste(l, levels, sep=": "))})) # version var: lev1 / var: lev2 / ...
}
barresN <- function(vars, along = NULL, df=list(e), labels = NULL, legend=hover, miss=T, weights = T, fr=F, rev=T, color=c(), share_labels = NULL, margin_l = NULL, sort = F,
                    rev_color = FALSE, hover=legend, thin=T, return="", showLegend=T, export_xls = F, parentheses = F, nolabel = F, error_margin = F, alphabetical = F) {
  if (nolabel & length(labels)==1) labels <- ""
  if (is.data.frame(df)) df <- list(df)
  if (!missing(along)) {
    if (along == "country_name" & !alphabetical & exists("countries_names")) {
      levels <- c()
      for (l in countries_names) if (l %in% Levels(df[[1]][[along]])) levels <- c(levels, l)
    } else levels <- Levels(df[[1]][[along]])
    levels <- sub("^\\*", "", rev(levels))
  }
  if (!missing(along)) data <- lapply(seq_along(levels), function(l) return(df[[1]][df[[1]][[along]]==levels[l],]))
  else data <- df
  if (!missing(along) & missing(labels)) labels <- paste(along, levels, sep=": ")
  if (!missing(along) & length(labels) < length(df)*length(levels)*length(vars)) labels <- labelsN(labels, levels, parentheses = parentheses)
  if (missing(vars) & missing(legend) & missing(hover)) warning('hover or legend must be given')
  if (!missing(miss)) nsp <- miss
  data1 <- dataKN(vars, data=df[[1]], miss=miss, weights = weights, return = "", fr=fr, rev=rev)
  if (missing(legend) & missing(hover)) {
    if (is.logical(df[[1]][[vars[1]]])) { showLegend = F; legend <- "True"; hover <- labels; } # data1(var = vars[1], data=df, weights = weights) # before: uncommented and "else" next line
    else hover <- legend <- dataN(var = vars[1], data=df[[1]], miss=miss, weights = weights, return = "legend", fr=fr, rev_legend = rev) }
  agree <- order_agree(data = data1, miss = miss)
  if (is.logical(df[[1]][[vars[1]]])) agree <- rev(agree)
  if (return=="levels") {if (is.null(levels)) {return(labels)} else {return(levels)}}
  else if (return=="labels") return(labels) # labels12(labels[agree], en = !fr, comp = comp, orig = orig)
  else if (return=="legend") return(legend)
  else {
    plotted_data <- dataNK(vars[agree], df = data, miss=miss, weights = weights, fr=fr, rev=rev, return = "")
    if (return=="data") { return(plotted_data)
    } else {
      not_nan <- sapply(c(1:ncol(plotted_data)), function(j) any(!is.nan(plotted_data[,j])))
      plotted_data <- plotted_data[, not_nan, drop=FALSE]
      return(barres(data = plotted_data, labels=labels[not_nan], legend=legend, share_labels= share_labels, margin_l = margin_l, # labels12(labels[agree], en = !fr, comp = comp, orig = orig) # /!\ doesn't currently support multiple vars
                    miss=miss, weights = weights, fr=fr, rev=rev, color=color, rev_color = rev_color, hover=hover, sort=F, thin=thin, showLegend=showLegend, export_xls = export_xls, error_margin = error_margin))
    } }
}
color <- function(v, grey=FALSE, grey_replaces_last = T, rev_color = FALSE, theme='RdBu') { # TODO! whitout white
  if (is.matrix(v)) n <- nrow(v)
  else if (length(v) > 1) n <- length(v)
  else n <- v # cf. http://research.stowers.org/mcm/efg/R/Color/Chart/ColorChart.pdf
  if (grey & grey_replaces_last & n > 1) n <- n-1
  if (theme=='rainbow') {
    if (n == 1) cols <- c("#66B3B3") # "brown": #A52A2A Presentation Teal: #008096 (title) #1A8C8C (dark) #66B3B3 #99CCCC (light)
    else if (n == 2) cols <- c("#66B3B3", "#A52A2A") # c("lightgreen", "plum") = c("#90EE90", "#DDA0DD")
    else if (n == 3) cols <- color5[c(1,3,5)]
    else if (n == 4) cols <- c(rainbow(4, end=4/15)[1:3], "#228B22")
    else if (n == 5) cols <- c(rainbow(4, end=4/15)[1:3], "#00FF00", "#228B22") # the last two are: green, forestgreen
    else if (n == 6) cols <- rainbow(6)
    else if (n == 7) cols <- c("#000000", rainbow(7)[c(1:3,5:7)])
    else cols <- rainbow(n) # diverge_hcl green2red brewer.pal(n, Spectral/RdBu...)  https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
  } else if (theme=='default') {
    cols <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(n)
  } else {
    cols <- rev(brewer.pal(max(n, 3), theme))
    if (n == 1) cols <- cols[1]
    # if (n == 2) cols <- cols[c(1,3)]
    else if (n %% 2 == 0) cols <- rev(brewer.pal(n+2, theme))[c(1:(n/2),(n/2+2):(n+1))] }
  if (n > 10) cols <- colorRampPalette(cols)(n)
  if (rev_color) cols <- rev(cols)
  if (grey & n > 1) return(c(cols, "#D3D3D3")) # lightgrey
  else return(cols)
}
#' # accord5 <- c("Pas du tout d'accord", "Pas vraiment d'accord", "Indifférent-e", "Assez d'accord", "Tout à fait d'accord")
#' oui_non5 <- c("Non, pas du tout", "Non, pas vraiment", "Indifférent-e/NSP", "Oui, plutôt", "Oui, tout à fait")
#' yes_no5 <- c("Not at all", "Not really", "Indifferent/PNR", "Rather yes", "Yes, completely")
#' # agree5 <- c("Strongly disagree", "Disagree", "Indifferent", "Agree", "Strongly agree")
#' # evol5 <- c("Baisser fortement", "Baisser légèrement", "Maintenir au niveau", "Augmenter légèrement", "Augmenter fortement")
#' # evolve5 <- c("Strongly decrease", "Slightly decrease", "Maintain", "Slightly increase", "Strongly increase")
order_agree <- function(data, miss, rev = T, n = ncol(data)) {
  agree <- c()
  if (!missing(miss)) {
    if (miss) for (i in 1:n) agree <- c(agree, sum(data[floor(nrow(data)/2+1):max(1,(nrow(data)-1)),i]))
    else for (i in 1:n) agree <- c(agree, sum(data[ifelse(nrow(data)==1,1,ceiling(nrow(data)/2+1)):nrow(data),i]))
  } else {
    if (nrow(data)==5 | nrow(data)==6) { for (i in 1:n) { agree <- c(agree, data[4, i] + data[5, i]) } }
    else if (nrow(data)==7) { for (i in 1:n) { agree <- c(agree, data[6, i] + data[7, i]) } }
    else { for (i in 1:n) { agree <- c(agree, data[1, i]) } } }
  return(order(agree, decreasing = rev)) }
barres <- function(data, vars, file, title="", labels, color=c(), rev_color = FALSE, hover=legend, nsp=TRUE, sort=TRUE, legend=hover, showLegend=T,
                   margin_r=0, margin_l=NULL, share_labels = NULL, online=FALSE, export_xls = F, digits = 0, add_means = FALSE, show_legend_means = T, transform_mean = NULL,
                   display_values=T, thin=T, legend_x=NA, show_ticks=T, xrange=NA, save = FALSE, df=e, miss=T,
                   weights = T, fr=F, rev=T, grouped = F, error_margin = F, color_margin = '#00000033', N = NA, font = 'Arial') { # default: Arial (also: Times, Latin Modern Sans, Computer Modern) # OECD: Computer Modern
  if (missing(vars) & missing(legend) & missing(hover)) warning('hover or legend must be given')
  if (!missing(miss)) nsp <- miss
  labels <- rev(unname(labels))
  if (!missing(vars)) vars <- rev(vars)
  if (missing(data) & !missing(vars)) {
    data <- dataKN(vars, data=df, miss=miss, weights = weights, return = "", fr=fr, rev=rev)
    N <- dataN(vars[1], data=df, miss=miss, weights = weights, return = "N")
    if ((missing(legend) || is.null(legend)) & missing(hover)) {
      if (is.logical(df[[vars[1]]])) hover <- legend <- labels # data1(var = vars[1], data=df, weights = weights)
      else hover <- legend <- dataN(var = vars[1], data=df, miss=miss, weights = weights, return = "legend", fr=fr, rev_legend = rev) } }
  if (length(color)==0) color <- color(data, nsp, rev_color = rev_color)
  if (identical(legend, TRUE) & missing(error_margin)) error_margin <- T
  margin_t <- 0 + 25*(!(thin))
  if (title!="") { margin_t <- 100 }
  if (grepl("<br>", title)) { margin_t <- 150 }
  legendSize <- 15+2 # 10, 13
  legendY <- 1 #1.1  + 0.3*thin/(ncol(data)-1) # last term may be problematic
  legendX <- 0.2
  legendFont = font #'Open Sans'
  if (is.null(margin_l)) margin_l <- 0 # 4.7*max(nchar(labels)/(1 + str_count(labels, '<br>')))
  if (is.null(share_labels)) share_labels <- 0.01 + 0.49*(!(" " %in% labels)) # 0.14
  if (max(nchar(labels)) > 25) { legendSize <- 15 } # 9, 13
  # if (max(nchar(labels)) > 50) { legendSize <- 8 }
  # if (max(nchar(labels)) > 60) { legendSize <- 7 }
  if (max(nchar(labels)) > 50) { # 70
    legendSize <- 13 # 11
    # legendY = 1.2
    legendX= -0.2 # 1
    # if (ncol(data)>1) margin_t = 170
  }
  legendX <- .96 # overwrites the previous legendX that was defined with xanchor = 'left'
  if (!is.na(legend_x)) legendX <- legend_x
  if (!showLegend) { margin_t <- max(0, margin_t - 70) }
  if (ncol(data)==1) legendY <- 1 # 1.5 + 0.3*thin
  if (!is.null(add_means) && add_means && is.null(transform_mean)) transform_mean <- identity
  if (!is.null(add_means) && add_means) means <- sapply(vars, function(v) return(transform_mean(wtd.mean(df[[v]], weights = df[["weight"]]))))
  if (sort) {
    order <- order_agree(data = data, miss = miss, rev = rev, n = length(labels))
    labels <- labels[order]
    data <- matrix(data[, order], nrow=nrow(data))
    if (!is.null(add_means) && add_means) means <- means[order]
  }
  if (is.na(xrange)) xrange <- c(0, max(colSums(data))*1.099)
  if (nrow(data)==1 & (sort | !showLegend)) {  # new: add !showLegend to manage responsable_CC i.e. comparisons of a multiple answer question
    if (!sort) order <- 1:length(labels)
    hover <- hover[order]
    value <- c()
    for (i in 1:length(hover)) {
      hover[i] <- paste(hover[i], "<br>Choisi dans ", round(100*data[1, i]), "% des réponses", sep="")
      value[i] <- paste(ifelse(data[1, i] > 0.01, round(100*data[1, i]), round(100*data[1, i], 1)), '%', sep='')
      value[i] <- paste(round(100*data[1, i], digits = digits), '%', sep='') } # '%  '
    hovers <- matrix(hover, nrow=length(hover))
    values <- matrix(value, nrow=length(hover))
  }
  else {
    hovers <- values <- c()
    if (nsp) {
      for (i in 1:(length(hover)-1)) {
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]/(1+data[length(hover), j])), '% des réponses<br>', round(100*data[i, j]), '% des réponses exprimées') )
          values <- c(values, paste(round(100*data[i, j]/(1+data[length(hover), j]), digits = digits), '%', sep='')) # '%  '
        }
      }
      for (j in 1:length(labels)) {
        hovers <- c(hovers, paste(hover[length(hover)], '<br>', round(100*data[length(hover), j]/(1+data[length(hover), j])), '% des réponses<br>') )
        values <- c(values, paste(round(100*data[length(hover), j]/(1+data[length(hover), j]), digits = digits), '%', sep='')) # '%  '
      }
    }
    else {
      if (is.element(hover[length(hover)],c("PNR", "PNR or other", "NSP"))) hover <- hover[1:(length(hover)-1)]
      if (is.element(legend[length(legend)],c("PNR", "PNR or other", "NSP"))) legend <- legend[1:(length(legend)-1)]
      for (i in 1:length(hover)) {
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]), '% des réponses exprimées<br>') )
          values <- c(values, paste(round(100*data[i, j], digits = digits), '%', sep='')) # '%  '
        }
      }
    }
    hovers <- matrix(hovers, ncol=length(hover))
    values <- matrix(values, ncol=length(hover))
  }
  if (!(display_values)) values <- replace(values, T, '')

  bars <- plot_ly(x = data[1,], y = labels, type = 'bar', orientation = 'h', text = values[,1], textposition = 'auto',
                  error_x = list(visible = error_margin, array=qnorm(1-0.05/2)*sqrt(data[1,]*(1-data[1,])/(N-1)), color = color_margin), # sort=FALSE,
                  hoverinfo = hovers[,1], name=legend[1], marker = list(color = color[1], line = list(color = 'white'))) %>% # , width = 0

    plotly::layout(xaxis = list(title = "",
                                showgrid = show_ticks,
                                showline = FALSE,
                                showticklabels = show_ticks,
                                gridcolor = toRGB("gray70"), # + noir, + proche de 0
                                gridwidth = 1,
                                griddash = "dot",
                                autotick = FALSE,
                                ticks = "outside",
                                tick0 = 0,
                                dtick = 0.1,
                                ticklen = 5*show_ticks,
                                tickwidth = 1,
                                tickcolor = toRGB("gray70"),
                                zeroline = T,
                                range = xrange,
                                domain = c(share_labels, 1)
    ),
    yaxis = list(title = "",
                 showgrid = FALSE,
                 showline = FALSE,
                 showticklabels = FALSE,
                 categoryorder = "trace",
                 # automargin = T,
                 zeroline = FALSE),
    hovermode = 'closest',
    barmode = ifelse(grouped, 'group', 'stack'),
    title = list(text = title, font = list(color = 'black')),
    # title = title,
    # titlefont = list(color='black'),
    font = list(color='black', size=legendSize-1),
    # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
    margin = list(l = margin_l, r = margin_r, t = margin_t, b = 24, autoexpand = thin), # 21, autoexpand=FALSE removes useless margin at bottom but creates bug with legend
    # margin = list(b = 20, t = margin_t),
    legend = list(orientation='h', y=legendY, x=legendX, xanchor = 'right', yanchor = 'bottom', traceorder='normal', font=list(size=legendSize+2, color='black', family = font)), # family='Balto',  , family=legendFont
    # showlegend = (showLegend & !((("Yes" %in% legend) | ("Oui" %in% legend)) & (length(legend)<4)))) %>%
    showlegend = showLegend # (showLegend & !(setequal(legend, c('Yes', 'No', 'PNR')) | setequal(legend, c('Oui', 'Non', 'NSP')) | setequal(legend, c('Yes', 'No')) | setequal(legend, c('Oui', 'Non'))))
    ) %>%

    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = share_labels - 0.01, y = labels,
                    xanchor = 'right',
                    text = labels,
                    font = list(family = font, size = 14+2, color = 'black',
                                type = if (length(labels) == 2) 'bold' else '' # to avoid the last item being more black (for real bold, use 'Arial Black')
                                  ),
                    showarrow = FALSE, align = 'right') # %>%
  # Legend in the Yes/No case
  if (showLegend == FALSE) {
    if ((setequal(legend, c('Yes', 'No', 'PNR')) | setequal(legend, c('Oui', 'Non', 'NSP')))) {
      bars <- bars %>% add_annotations(xref = 'x', yref = 'paper',
                                       x = c(0.1, 0.9, 1.1),
                                       y = 1.5,
                                       text = legend,
                                       font = list(family = font, size = 16, color = 'black'),
                                       showarrow = FALSE) } # %>%
    if ((setequal(legend, c('Yes', 'No')) | setequal(legend, c('Oui', 'Non')))) {
      bars <- bars %>% add_annotations(xref = 'x', yref = 'paper',
                                       x = c(0.1, 0.9),
                                       y = 1.5,
                                       text = legend,
                                       font = list(family = font, size = 16, color = 'black'),
                                       showarrow = FALSE) } # %>%
  }

  # print(nrow(data))
  # print(hover)
  # print(nrow(hovers))
  # print(ncol(hovers))
  if (nrow(data)>1) { for (i in 2:nrow(data)) { # evaluate=TRUE,
    bars <- add_trace(bars, x = data[i,], name=legend[i], text = values[,i], hoverinfo = 'text', hovertext = hovers[,i], marker = list(color = color[i]),
                      error_x = list(visible = error_margin, array=qnorm(1-0.05/2)*sqrt(data[i,]*(1-data[i,])/(N-1)), color = color_margin)) # width thickness (in px)
  } } # /!\ When data and vars are not provided, N cannot be computed, but error_margin=T still returns a (zero) confidence interval
  if (!is.null(add_means) && add_means)  bars <- add_trace(bars, x = means, name = "mean", marker = list(color = 'black', size = 10, symbol = 'diamond'), showlegend = (!is.null(show_legend_means) && show_legend_means), type = 'scatter', mode = 'markers')
  if (online) { api_create(bars, filename=file, sharing="public") }
  if (!missing(file) & save) save_plotly(bars, filename = file) # new
  if (export_xls) {
    table <- as.data.frame(data, row.names = legend)
    names(table) <- labels
    return(table) }
  else return(bars)
}
#' # plot(1:3,1:3) # example
#' # dev.copy(png, filename="test.png") # save plot from R (not plotly)
#' # dev.off()
#' # orca(example, file = "image.png") # BEST METHOD, cf. below
fig_height <- function(nb_bars, large = F) return(ifelse(nb_bars == 1, 140, 220 + 30*(nb_bars - 2)) + 10*nb_bars*large) # 2 ~ 220, 3 ~ 250, 4 ~ 280, 5 ~ 325, 6 ~ 360, 7 ~ 380, TRUE ~ 400 # 2 ~ 200-240, 3 ~ 240-275, 4 ~ 270-340, 5 ~ 320-340, 6 ~ 400, 7 ~ 340-430,
save_plot <- function(plot=NULL, filename = deparse(substitute(plot)), folder = '../figures/', width = dev.size('px')[1], height = dev.size('px')[2], method='dev', trim = T, format = 'png') {
  if (any(class(plot) %in% c("data.frame", "array"))) {
    # file <- paste(folder, "xls/", filename, ".xlsx", sep='')
    file <- paste(sub("figures", "xlsx", folder), filename, ".xlsx", sep='') # xlsx
    write.xlsx(as.data.frame(plot), file, row.names = T, overwrite = T)
  } else {
    file <- paste0(folder, filename, ".", format)
    # print(file)
    if (grepl('dev', method)) {
      if (format == 'png') {
        dev.copy(png, filename = file, width = width, height = height) # save plot from R (not plotly)
        dev.off() }
      else if (format == 'svg') {
        dev.copy(svg, filename = file, width = width/100, height = height/100) # save plot from R (not plotly)
        dev.off() } # TODO choose width height with PDF
      else if (format == 'pdf') dev.print(pdf, file = file) # because dev.size('px')[1]/dev.size('in')[1] = 105 , width = width/105, height = height/105
    }
    else {
      server <- orca_serve() # doesn't work within a function because requires admin rights
      server$export(plot, file = file, width = width, height = height)
      server$close()
    }
    if (trim & format %in% c('png')) image_write(image_trim(image_read(file)), file) # , 'svg'
    if (trim & format == 'pdf') plot_crop(file) } # to crop pdf, see also code_oecd/crop_pdf.sh and run it in the desired folder
}
save_plotly <- function(plot, filename = deparse(substitute(plot)), folder = '../figures/', width = dev.size('px')[1], height = dev.size('px')[2], method='orca', format = 'pdf', trim = T) { # in case connection refused, turn off Windows Defender on private networks
  if (any(class(plot) == "data.frame")) {
    # file <- paste(folder, "xls/", filename, ".xlsx", sep='')
    file <- paste(sub("figures", "xlsx", folder), filename, ".xlsx", sep='')
    write.xlsx(plot, file, row.names = T, overwrite = T)
    print(file)
  } else {
    file <- paste(folder, filename, ".", format, sep='')
    # print(file)
    if (grepl('webshot', method)) { # four times faster: 2.5s (vs. 10s) but saves useless widgets and doesn't exactly respect the display
      saveWidget(plot, 'temp.html')
      webshot('temp.html', file, delay = 0.1, vwidth = width, vheight = height)
      file.remove('temp.html')}
    # else orca(plot, file = file, width = width, height = height, format = format) # bug with encoding in Windows
    else {
      server <- orca_serve() # doesn't work within a function because requires admin rights
      server$export(plot, file = file, width = width, height = height)
      server$close()
    }
    if (trim & format == 'png') image_write(image_trim(image_read(file)), file)
    if (trim & format == 'pdf') plot_crop(file) }
}
#' correlogram <- function(grep = NULL, vars = NULL, df = e) {
#'   if (missing(vars)) vars <- names(df)[grepl(grep, names(df)) & !grepl("_funding|_correct", names(df))]
#'   data <- df[,vars]
#'   names(data) <- vars
#'   corr <- cor(data, use="complete.obs")
#'   p.mat <- cor.mtest(data) # corrplot does not work when some packages are loaded before 'corrplot' => if it doesn't work, restart R and load only corrplot.
#'   corrplot(corr, method='color', p.mat = p.mat, sig.level = 0.01, diag=FALSE, tl.srt=35, tl.col='black', insig = 'blank', addCoef.col = 'black', addCoefasPercent = T, type='upper') #, order='hclust'
#' }
automatic_folder <- function(along = "country_name", data = e, several = "country_comparison") {
  if (along %in% c("country", "country_name", "wave")) {
    folder <- unique(data[[along]])
    if (length(folder) > 1) folder <- paste0('../figures/', several, '/')
    else folder <- paste0("../figures/", folder, "/")
    return(folder)
  } else warning("'folder' missing")
}
heatmap_plot <- function(data, type = "full", p.mat = NULL, proportion = T, percent = FALSE, colors = 'RdYlBu', nb_digits = NULL) { # type in full, upper, lower
  diag <- if(type=="full") T else F
  # color_lims <- if(proportion) c(0,1) else { if (min(data)>=2 & max(data)<= 2) c(-2,2) else c(min(0, data), max(data)) }
  color_lims <- if(proportion) c(0,1) else { if (min(data, na.rm=T)>=2 & max(data, na.rm=T)<= 2) c(-2,2) else c(min(0, data, na.rm=T), max(data, na.rm=T)) }
  if (missing(nb_digits) || is.null(nb_digits)) nb_digits <- if(proportion | percent) 0 else 1
  # color2 <- c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")
  # col <- colorRampPalette(color2)(200)
  # # if (proportion) col <- colorRampPalette(c(rep("#67001F", 10), col2))(200)
  par(xpd=TRUE)
  return(corrplot(data, method='color', col = if(colors %in% c('RdBu', 'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdYlBu')) COL2(colors) else COL1(colors), tl.cex = 1.3, na.label = "NA", number.cex = 1.3, mar = c(1,1,1.3,3), cl.pos = 'n', col.lim = color_lims, number.digits = nb_digits, p.mat = p.mat, sig.level = 0.01, diag=diag, tl.srt=35, tl.col='black', insig = 'blank', addCoef.col = 'black', addCoefasPercent = (proportion | percent), type=type, is.corr = F) ) #  cl.pos = 'n' removes the scale # cex # mar ...1.1
}
heatmap_table <- function(vars, labels = vars, data = e, along = "country_name", special = c(), conditions = c("", ">= 1", "/"), on_control = FALSE, alphabetical = T, export_xls = T, filename = "", sort = FALSE, folder = NULL, weights = T, remove_na = T, transpose = FALSE) {
  # The condition must work with the form: "data$var cond", e.g. "> 0", "%in% c('a', 'b')" work
  e <- data
  if (on_control) e <- e[e$treatment=="None",]
  if (missing(folder)) folder <- automatic_folder(along, data)
  if (along == "country_name" & !alphabetical & exists("countries_names")) {
    if (exists("countries_names_hm") & any(c('High-income','Middle-income') %in% special)) names <- countries_names_hm else names <- countries_names
    levels <- c()
    for (l in names) if (l %in% Levels(e[[along]])) levels <- c(levels, l)
  } else levels <- Levels(e[[along]], values = FALSE)
  nb_vars <- length(vars)
  if (length(conditions)==1) conditions <- rep(conditions[1], nb_vars)
  up_labels <- c(special, levels)
  if (any(c('non-OECD', 'Non-OECD', 'non-oecd') %in% special)) { # TODO manage all df
    if (length(levels) == 14) up_labels <- c(special[!special %in% c('non-OECD', 'Non-OECD', 'non-oecd')], levels)
    else if (levels[15]=="Brazil") up_labels <- c(special[!special %in% c('non-OECD', 'Non-OECD', 'non-oecd')], levels[1:14], "Non-OECD", levels[15:length(levels)])
    else warning("Unkown number of levels") }
  if (any(c('high-income', 'High-income', 'High income') %in% special)) {
    if (length(levels) == 12) up_labels <- c("High-income", levels)
    else if (levels[13]=="Brazil") up_labels <- c(special[!special %in% c('middle-income', 'Middle-income', 'Middle income')], levels[high_income[names(names)]], "Middle-income", levels[!high_income[names(names)]])
    else warning("Unkown number of levels") }
  if (any(c('middle-income', 'Middle-income', 'Middle income') %in% special) & length(levels) == 8)  up_labels <- c("Middle-income", levels)
  if (any(c("Eu", "Eu4", "EU4", "EU", "Europe") %in% special) & all(levels == countries_names)) up_labels <- c(levels[5], "Europe", levels[1:4])
  if (any(c("Eu", "Eu4", "EU4", "EU", "Europe") %in% special) & all(levels == countries_names[1:4])) up_labels <- c("Europe", levels[1:4])
  table <- array(NA, dim = c(nb_vars, length(c(special, levels))), dimnames = list(vars, up_labels))
  for (c in up_labels) {
    if (c %in% levels) { df_c <- e[e[[along]]==c,]
    } else if (c %in% c('World', 'world', 'total', 'all')) { df_c <- e
    } else if (c %in% c('OECD', 'oecd', 'EU')) { df_c <- e[which(oecd[e$country]),]
    } else if (c %in% c('non-OECD', 'Non-OECD', 'non-oecd')) { df_c <- e[which(!oecd[e$country]),]
    } else if (c %in% c('high-income', 'High-income', 'High income')) { df_c <- e[which(high_income[e$country]),]
    } else if (c %in% c('middle-income', 'Middle-income', 'Middle income')) { df_c <- e[which(!high_income[e$country]),]
    } else if (c %in% c('Europe', 'Europe4')) { df_c <- e[e$continent == "Europe",]
    } else if (c %in% countries) { df_c <- e[e$country == c,]
    } else if (c %in% c("Eu", "Eu4", "EU4", "EU")) { df_c <- e[e$continent == "Eu4",] 
    } else if (c %in% countries_names) { df_c <- e[e$country_name == c,] }
    for (v in 1:nb_vars) {
      if (vars[v] %in% c("gcs_support", "nr_support", "gcs_support_100")) { 
        temp <- df_c
        df_c <- df_c[df_c$wave != "US2",] }
      var_c <- df_c[[vars[v]]][!is.na(df_c[[vars[v]]])]
      if (conditions[v] == "median") {
        if (weights & length(var_c) > 0 & c %in% c(countries_EU, names(countries_EU))) table[v,c] <- eval(str2expression(paste("wtd.median(var_c, na.rm = T, weight = df_c$weight_country[!is.na(df_c[[vars[v]]])])")))
        if (weights & length(var_c) > 0 & !(c %in% c(countries_EU, names(countries_EU)))) table[v,c] <- eval(str2expression(paste("wtd.median(var_c, na.rm = T, weight = df_c$weight[!is.na(df_c[[vars[v]]])])")))
        if (!weights & length(var_c) > 0) table[v,c] <- eval(str2expression(paste("median(var_c, na.rm = T)")))
      } else {
        if (weights & length(var_c) > 0 & c %in% c(countries_EU, names(countries_EU))) table[v,c] <- eval(str2expression(paste("wtd.mean(var_c", conditions[v], ", na.rm = T, weights = df_c$weight_country[!is.na(df_c[[vars[v]]])])")))
        if (weights & length(var_c) > 0 & !(c %in% c(countries_EU, names(countries_EU)))) table[v,c] <- eval(str2expression(paste("wtd.mean(var_c", conditions[v], ", na.rm = T, weights = df_c$weight[!is.na(df_c[[vars[v]]])])")))
        if (!weights & length(var_c) > 0) table[v,c] <- eval(str2expression(paste("mean(var_c", conditions[v], ", na.rm = T)")))
      }
      if (vars[v] %in% c("gcs_support", "nr_support", "gcs_support_100")) df_c <- temp
    }
  }
  row.names(table) <- labels
  if (sort) table <- table[order(-table[,1]),]
  if (remove_na) table <- table[sapply(1:nrow(table), function(i) { !all(is.na(table[i,])) }), sapply(1:ncol(table), function(j) { !all(is.na(table[,j])) }), drop = FALSE]
  if (transpose) table <- t(table)
  if (export_xls) save_plot(table, filename = sub("figures", "xlsx", paste0(folder, filename)))
  return(table)
}
heatmap_wrapper <- function(vars, labels = vars, name = deparse(substitute(vars)), along = "country_name", labels_along = NULL, special = c(),
                            conditions = c("", ">= 1", "/"), data = e, width = NULL, height = NULL, alphabetical = T, on_control = FALSE,
                            export_xls = T, format = 'pdf', sort = FALSE, proportion = NULL, percent = FALSE, nb_digits = NULL, trim = T,
                            colors = 'RdYlBu', folder = NULL, weights = T) {
  # width: 1770 to see Ukraine (for 20 countries), 1460 to see longest label (for 20 countries), 800 for four countries.
  # alternative solution to see Ukraine/labels: reduce height (e.g. width=1000, height=240 for 5 rows). Font is larger but picture of lower quality / more pixelized.
  # Longest label: "Richest countries should pay even more to help vulnerable ones" (62 characters, variables_burden_sharing_few).
  # special can be c("World", "OECD")
  if (is.null(folder)) folder <- automatic_folder(along, data)
  if (is.null(width)) width <- ifelse(length(labels) <= 3, 1000, ifelse(length(labels) <= 8, 1550, 1770)) # TODO! more precise than <= 3 vs. > 3
  if (is.null(height)) height <- ifelse(length(labels) <= 3, 163, ifelse(length(labels) <= 8, 400, 600))

  for (cond in conditions) {
    filename <- paste(sub("variables_", "", name),
                      case_when(cond == "" ~ "mean",
                                cond == "median" ~ "median",
                                cond == "> 0" ~ "positive",
                                cond == ">= 1" ~ "positive",
                                cond == "< 0" ~ "negative",
                                cond == "<= -1" ~ "negative",
                                cond == ">= 0" ~ "non-negative",
                                cond == "<= 0" ~ "non-positive",
                                cond == "== 2" ~ "max",
                                cond == "== -2" ~ "min",
                                cond == "-" ~ "difference",
                                cond == "/" ~ "share", # uses >0 for binary data (detected as neg=0) 
                                cond == "//" ~ "share_strict", # to use when no one gave a negative answer though it was an option
                                TRUE ~ "unknown"), sep = "_")
    tryCatch({
      if (cond %in% c("/", "-", "//")) { 
        pos <- heatmap_table(vars = vars, labels = labels, data = data, along = along, special = special, conditions = ">= 1", on_control = on_control, alphabetical = alphabetical, sort = FALSE, weights = weights)
        neg <- heatmap_table(vars = vars, labels = labels, data = data, along = along, special = special, conditions = "<= -1", on_control = on_control, alphabetical = alphabetical, sort = FALSE, weights = weights)
        if (cond == "-") temp <- pos - neg else temp <- pos / (pos + neg)
        if (cond == "/") {
          binary_rows <- which(rowMeans(neg)==0)
          temp[binary_rows,] <- pos[binary_rows,]
          row.names(temp)[binary_rows] <- paste0(row.names(temp)[binary_rows], "*") }
        for (i in 1:length(vars)) if (is.logical(data[[vars[i]]])) temp[i, ] <- pos[i, ]
      } else {  temp <- heatmap_table(vars = vars, labels = labels, data = data, along = along, special = special, conditions = cond, on_control = on_control, alphabetical = alphabetical, sort = FALSE, weights = weights) }
      if (!missing(labels_along) & length(labels_along) == ncol(temp)) colnames(temp) <- labels_along
      if (sort) temp <- temp[order(-temp[,1]),, drop = FALSE]
      if (export_xls) save_plot(as.data.frame(temp), filename = sub("figures", "xlsx", paste0(folder, filename)))
      heatmap_plot(temp, proportion = ifelse(is.null(proportion), !cond %in% c("median", ""), proportion), percent = percent, nb_digits = nb_digits, colors = colors)
      save_plot(filename = paste0(folder, filename), width = width, height = height, format = format, trim = trim)
      print(paste0(filename, ": success"))
    }, error = function(cond) { print(paste0(filename, ": fail.")) } )
  }
}

#' ##### Other #####
#' CImedian <- function(vec) { # 95% confidence interval
#'   res <- tryCatch(unlist(ci.median(vec[!is.na(vec) & vec!=-1])), error=function(e) {print('NA')})
#'   return(paste(res[paste('ci.lower')], res[paste('ci.median')], res[paste('ci.upper')], length(which(!is.na(vec) & vec!=-1)))) }
#'
#' # from http://pcwww.liv.ac.uk/~william/R/crosstab.r http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html
Crosstab <- function (..., dec.places = NULL, type = NULL, style = "wide", row.vars = NULL, col.vars = NULL, percentages = TRUE,  addmargins = TRUE, subtotals=TRUE) {
  #Declare function used to convert frequency counts into relevant type of proportion or percentage
  mk.pcnt.tbl <- function(tbl, type) {
    a <- length(row.vars)
    b <- length(col.vars)
    mrgn <- switch(type, column.pct = c(row.vars[-a], col.vars),
                   row.pct = c(row.vars, col.vars[-b]),
                   joint.pct = c(row.vars[-a], col.vars[-b]),
                   total.pct = NULL)
    tbl <- prop.table(tbl, mrgn)
    if (percentages) {
      tbl <- tbl * 100
    }
    tbl
  }

  #Find no. of vars (all; row; col) for use in subsequent code
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars


  #Check to make sure all user-supplied arguments have valid values
  stopifnot(as.integer(dec.places) == dec.places, dec.places > -1)
  #type: see next section of code
  stopifnot(is.character(style))
  stopifnot(is.logical(percentages))
  stopifnot(is.logical(addmargins))
  stopifnot(is.logical(subtotals))
  stopifnot(n.vars>=1)

  #Convert supplied table type(s) into full text string (e.g. "f" becomes "frequency")
  #If invalid type supplied, failed match gives user automatic error message
  types <- NULL
  choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
  for (tp in type) types <- c(types, match.arg(tp, choices))
  type <- types

  #If no type supplied, default to 'frequency + total' for univariate tables and to
  #'frequency' for multi-dimenstional tables

  #For univariate table....
  if (n.vars == 1) {
    if (is.null(type)) {
      # default = freq count + total.pct
      type <- c("frequency", "total.pct")
      #row.vars <- 1
    } else {
      #and any requests for row / col / joint.pct must be changed into requests for 'total.pct'
      type <- ifelse(type == "frequency", "frequency", "total.pct")
    }
    #For multivariate tables...
  } else if (is.null(type)) {
    # default = frequency count
    type <- "frequency"
  }



  #Check for integrity of requested analysis and adjust values of function arguments as required

  if ((addmargins==FALSE) & (subtotals==FALSE)) {
    warning("WARNING: Request to suppress subtotals (subtotals=FALSE) ignored because no margins requested (addmargins=FALSE)")
    subtotals <- TRUE
  }

  if ((n.vars>1) & (length(type)>1) & (addmargins==TRUE)) {
    warning("WARNING: Only row totals added when more than one table type requested")
    #Code lower down selecting type of margin implements this...
  }

  if ((length(type)>1) & (subtotals==FALSE)) {
    warning("WARNING: Can only request supply one table type if requesting suppression of subtotals; suppression of subtotals not executed")
    subtotals <- TRUE
  }

  if ((length(type)==1) & (subtotals==FALSE)) {
    choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
    tp <- match.arg(type, choices)
    if (tp %in% c("row.pct","column.pct","joint.pct")) {
      warning("WARNING: subtotals can only be suppressed for tables of type 'frequency' or 'total.pct'")
      subtotals<- TRUE
    }
  }

  if ((n.vars > 2) & (n.col.vars>1) & (subtotals==FALSE))
    warning("WARNING: suppression of subtotals assumes only 1 col var; table flattened accordingly")


  if ( (subtotals==FALSE) & (n.vars>2) )  {
    #If subtotals not required AND total table vars > 2
    #Reassign all but last col.var as row vars
    #[because, for simplicity, Crosstabs assumes removal of subtotals uses tables with only ONE col var]
    #N.B. Subtotals only present in tables with > 2 cross-classified vars...
    if (length(col.vars)>1) {
      row.vars <- c(row.vars,col.vars[-length(col.vars)])
      col.vars <- col.vars[length(col.vars)]
      n.row.vars <- length(row.vars)
      n.col.vars <- 1
    }
  }

  #If dec.places not set by user, set to 2 unlesss only one table of type frequency requested,
  #in which case set to 0.  [Leaves user with possibility of having frequency tables with > 0 dp]
  if (is.null(dec.places)) {
    if ((length(type)==1) & (type[1]=="frequency")) {
      dec.places <- 0
    } else {
      dec.places <-2
    }
  }

  #Take the original input data, whatever form originally supplied in,
  #convert into table format using requested row and col vars, and save as 'tbl'

  args <- list(...)

  if (length(args) > 1) {
    if (!all(sapply(args, is.factor)))
      stop("If more than one argument is passed then all must be factors")
    tbl <- table(...)
  }
  else {
    if (is.factor(...)) {
      tbl <- table(...)
    }
    else if (is.table(...)) {
      tbl <- eval(...)
    }
    else if (is.data.frame(...)) {
      #tbl <- table(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        tbl <- table(...)
      }
      else {
        var.names <- c(row.vars,col.vars)
        A <- (...)
        tbl <- table(A[var.names])
        if(length(var.names==1)) names(dimnames(tbl)) <- var.names
        #[table() only autocompletes dimnames for multivariate Crosstabs of dataframes]
      }
    }
    else if (class(...) == "ftable") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- names(attr(tbl, "row.vars"))
        col.vars <- names(attr(tbl, "col.vars"))
      }
      tbl <- as.table(tbl)
    }
    else if (class(...) == "ctab") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- tbl$row.vars
        col.vars <- tbl$col.vars
      }
      for (opt in c("dec.places", "type", "style", "percentages",
                    "addmargins", "subtotals")) if (is.null(get(opt)))
                      assign(opt, eval(parse(text = paste("tbl$", opt,
                                                          sep = ""))))
      tbl <- tbl$table
    }
    else {
      stop("first argument must be either factors or a table object")
    }
  }

  #Convert supplied table style into full text string (e.g. "l" becomes "long")
  style <- match.arg(style, c("long", "wide"))

  #Extract row and col names to be used in creating 'tbl' from supplied input data
  nms <- names(dimnames(tbl))
  z <- length(nms)
  if (!is.null(row.vars) && !is.numeric(row.vars)) {
    row.vars <- order(match(nms, row.vars), na.last = NA)
  }
  if (!is.null(col.vars) && !is.numeric(col.vars)) {
    col.vars <- order(match(nms, col.vars), na.last = NA)
  }
  if (!is.null(row.vars) && is.null(col.vars)) {
    col.vars <- (1:z)[-row.vars]
  }
  if (!is.null(col.vars) && is.null(row.vars)) {
    row.vars <- (1:z)[-col.vars]
  }
  if (is.null(row.vars) && is.null(col.vars)) {
    col.vars <- z
    row.vars <- (1:z)[-col.vars]
  }

  #Take the original input data, converted into table format using supplied row and col vars (tbl)
  #and create a second version (Crosstab) which stores results as percentages if a percentage table type is requested.
  if (type[1] == "frequency")
    Crosstab <- tbl
  else
    Crosstab <- mk.pcnt.tbl(tbl, type[1])


  #If multiple table types requested, create and add these to
  if (length(type) > 1) {
    tbldat <- as.data.frame.table(Crosstab)
    z <- length(names(tbldat)) + 1
    tbldat[z] <- 1
    pcntlab <- type
    pcntlab[match("frequency", type)] <- "Count"
    pcntlab[match("row.pct", type)] <- "Row %"
    pcntlab[match("column.pct", type)] <- "Column %"
    pcntlab[match("joint.pct", type)] <- "Joint %"
    pcntlab[match("total.pct", type)] <- "Total %"
    for (i in 2:length(type)) {
      if (type[i] == "frequency")
        Crosstab <- tbl
      else Crosstab <- mk.pcnt.tbl(tbl, type[i])
      Crosstab <- as.data.frame.table(Crosstab)
      Crosstab[z] <- i
      tbldat <- rbind(tbldat, Crosstab)
    }
    tbldat[[z]] <- as.factor(tbldat[[z]])
    levels(tbldat[[z]]) <- pcntlab
    Crosstab <- xtabs(Freq ~ ., data = tbldat)
    names(dimnames(Crosstab))[z - 1] <- ""
  }


  #Add margins if required, adding only those margins appropriate to user request
  if (addmargins==TRUE) {

    vars <- c(row.vars,col.vars)

    if (length(type)==1) {
      if (type=="row.pct")
      { Crosstab <- addmargins(Crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
      }
      else
      { if (type=="column.pct")
      { Crosstab <- addmargins(Crosstab,margin=c(vars[n.row.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.row.vars]))
      }
        else
        { if (type=="joint.pct")
        { Crosstab <- addmargins(Crosstab,margin=c(vars[(n.row.vars)],vars[n.vars]))
        tbl <- addmargins(tbl,margin=c(vars[(n.row.vars)],vars[n.vars]))
        }
          else #must be total.pct OR frequency
          { Crosstab <- addmargins(Crosstab)
          tbl <- addmargins(tbl)
          }
        }
      }
    }

    #If more than one table type requested, only adding row totals makes any sense...
    if (length(type)>1) {
      Crosstab <- addmargins(Crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
    }

  }


  #If subtotals not required, and total vars > 2, create dataframe version of table, with relevent
  #subtotal rows / cols dropped [Subtotals only present in tables with > 2 cross-classified vars]
  t1 <- NULL
  if ( (subtotals==FALSE) & (n.vars>2) )  {

    #Create version of Crosstab in ftable format
    t1 <- Crosstab
    t1 <- ftable(t1,row.vars=row.vars,col.vars=col.vars)

    #Convert to a dataframe
    t1 <- as.data.frame(format(t1),stringsAsFactors=FALSE)

    #Remove backslashes from category names AND colnames
    t1 <- apply(t1[,],2, function(x) gsub("\"","",x))
    #Remove preceding and trailing spaces from category names to enable accurate capture of 'sum' rows/cols
    #[Use of grep might extrac category labels with 'sum' as part of a longer one or two word string...]
    t1 <- apply(t1,2,function(x) gsub("[[:space:]]*$","",gsub("^[[:space:]]*","",x)))

    #Reshape dataframe to that variable and category labels display as required
    #(a) Move col category names down one row; and move col variable name one column to right
    t1[2,(n.row.vars+1):ncol(t1)] <- t1[1,(n.row.vars+1):ncol(t1)]
    t1[1,] <- ""
    t1[1,(n.row.vars+2)] <- t1[2,(n.row.vars+1)]
    #(b) Drop the now redundant column separating the row.var labels from the table data + col.var labels
    t1 <- t1[,-(n.row.vars+1)]

    #In 'lab', assign category labels for each variable to all rows (to allow identification of sub-totals)
    lab <- t1[,1:n.row.vars]
    for (c in 1:n.row.vars) {
      for (r in 2:nrow(lab)) {
        if (lab[r,c]=="") lab[r,c] <- lab[r-1,c]
      }
    }

    lab <- (apply(lab[,1:n.row.vars],2,function(x) x=="Sum"))
    lab <- apply(lab,1,sum)
    #Filter out rows of dataframe containing subtotals

    t1 <- t1[((lab==0) | (lab==n.row.vars)),]

    #Move the 'Sum' label associated with last row to the first column; in the process
    #setting the final row labels associated with other row variables to ""
    t1[nrow(t1),1] <- "Sum"
    t1[nrow(t1),(2:n.row.vars)] <- ""

    #set row and column names to NULL
    rownames(t1) <- NULL
    colnames(t1) <- NULL

  }



  #Create output object 'result' [class: Crosstab]
  result <- NULL
  #(a) record of argument values used to produce tabular output
  result$row.vars <- row.vars
  result$col.vars <- col.vars
  result$dec.places <- dec.places
  result$type <- type
  result$style <- style
  result$percentages <- percentages
  result$addmargins <- addmargins
  result$subtotals <- subtotals

  #(b) tabular output [3 variants]
  result$table <- tbl  #Stores original cross-tab frequency counts without margins [class: table]
  result$Crosstab <- Crosstab #Stores cross-tab in table format using requested style(frequency/pct) and table margins (on/off)
  #[class: table]
  result$Crosstab.nosub <- t1  #Crosstab with subtotals suppressed [class: dataframe; or NULL if no subtotals suppressed]
  class(result) <- "Crosstab"

  #Return 'result' as output of function
  result

}

print.Crosstab <- function(x,dec.places=x$dec.places,subtotals=x$subtotals,...) {

  row.vars <- x$row.vars
  col.vars <- x$col.vars
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars

  if (length(x$type)>1) {
    z<-length(names(dimnames(x$Crosstab)))
    if (x$style=="long") {
      row.vars<-c(row.vars,z)
    } else {
      col.vars<-c(z,col.vars)
    }
  }

  if (n.vars==1) {
    if (length(x$type)==1) {
      tmp <- data.frame(round(x$Crosstab,x$dec.places))
      colnames(tmp)[2] <- ifelse(x$type=="frequency","Count","%")
      print(tmp,row.names=FALSE)
    } else {
      print(round(x$Crosstab,x$dec.places))
    }
  }


  #If table has only 2 dimensions, or subtotals required for >2 dimensional table,
  #print table using ftable() on x$Crosstab
  if ((n.vars == 2) | ((subtotals==TRUE) & (n.vars>2))) {

    tbl <- ftable(x$Crosstab,row.vars=row.vars,col.vars=col.vars)

    if (!all(as.integer(tbl)==as.numeric(tbl))) tbl <- round(tbl,dec.places)
    print(tbl,...)

  }

  #If subtotals NOT required AND > 2 dimensions, print table using write.table() on x$Crosstab.nosub
  if ((subtotals==FALSE) & (n.vars>2))  {

    t1 <- x$Crosstab.nosub

    #Convert numbers to required decimal places, right aligned
    width <- max( nchar(t1[1,]), nchar(t1[2,]), 7 )
    dec.places <- x$dec.places
    number.format <- paste("%",width,".",dec.places,"f",sep="")
    t1[3:nrow(t1),((n.row.vars+1):ncol(t1))] <- sprintf(number.format,as.numeric(t1[3:nrow(t1),((n.row.vars+1):ncol(t1))]))

    #Adjust column variable label to same width as numbers, left aligned, padding with trailing spaces as required
    col.var.format <- paste("%-",width,"s",sep="")
    t1[1,(n.row.vars+1):ncol(t1)] <- sprintf(col.var.format,t1[1,(n.row.vars+1):ncol(t1)])
    #Adjust column category labels to same width as numbers, right aligned, padding with preceding spaces as required
    col.cat.format <- paste("%",width,"s",sep="")
    t1[2,(n.row.vars+1):ncol(t1)] <- sprintf(col.cat.format,t1[2,(n.row.vars+1):ncol(t1)])

    #Adjust row labels so that each column is of fixed width, using trailing spaces as required
    for (i in 1:n.row.vars) {
      width <- max(nchar(t1[,i])) + 2
      row.lab.format <- paste("%-",width,"s",sep="")
      t1[,i] <- sprintf(row.lab.format,t1[,i])
    }

    write.table(t1,quote=FALSE,col.names=FALSE,row.names=FALSE)

  }

}

# inflate_for_miss <- function(v) return(c(v[1:(length(v)-1)]/(1-v[length(v)]), v[length(v)]))
#
# close <- function(x, y, prec = 0.0001) return(all(abs(x - y) < prec))
#
# cor.mtest <- function(mat, ...) {
#   mat <- as.matrix(mat)
#   n <- ncol(mat)
#   p.mat<- matrix(NA, n, n)
#   diag(p.mat) <- 0
#   for (i in 1:(n - 1)) {
#     for (j in (i + 1):n) {
#       tmp <- cor.test(mat[, i], mat[, j], ...)
#       p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
#     }
#   }
#   colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
#   p.mat
# }
# rquery.wordcloud <- function(x, type=c("text", "url", "file"), lang="english", excludeWords=NULL,
#                              textStemming=FALSE,  colorPalette="Dark2", min.freq=3, max.words=200) {
#   # http://www.sthda.com/english/wiki/word-cloud-generator-in-r-one-killer-function-to-do-everything-you-need
#   if(type[1]=="file") text <- readLines(x)
#   else if(type[1]=="url") text <- html_to_text(x)
#   else if(type[1]=="text") text <- x
#
#   # Load the text as a corpus
#   docs <- Corpus(VectorSource(text))
#   # Convert the text to lower case
#   docs <- tm_map(docs, content_transformer(tolower))
#   # Remove numbers
#   docs <- tm_map(docs, removeNumbers)
#   # Remove stopwords for the language
#   docs <- tm_map(docs, removeWords, stopwords(lang))
#   # Remove punctuations
#   docs <- tm_map(docs, removePunctuation)
#   # Eliminate extra white spaces
#   docs <- tm_map(docs, stripWhitespace)
#   # Remove your own stopwords
#   if(!is.null(excludeWords))
#     docs <- tm_map(docs, removeWords, excludeWords)
#   # Text stemming
#   if(textStemming) docs <- tm_map(docs, stemDocument)
#   # Create term-document matrix
#   tdm <- TermDocumentMatrix(docs)
#   m <- as.matrix(tdm)
#   v <- sort(rowSums(m),decreasing=TRUE)
#   d <- data.frame(word = names(v),freq=v)
#   # check the color palette name
#   if(!colorPalette %in% rownames(brewer.pal.info)) colors = colorPalette
#   else colors = brewer.pal(8, colorPalette)
#   # Plot the word cloud
#   set.seed(1234)
#   wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
#             random.order=FALSE, rot.per=0, #0.35,
#             use.r.layout=FALSE, colors=colors)
#
#   invisible(list(tdm=tdm, freqTable = d))
# }
#
plot_world_map <- function(var, condition = "", df = co2_pop, on_control = FALSE, save = T, continuous = FALSE, width = dev.size('px')[1], height = dev.size('px')[2], legend_x = .05, rev_color = FALSE,
                           breaks = NULL, labels = NULL, legend = NULL, limits = NULL, fill_na = FALSE, format = "png", trim = T, na_label = "NA", parties = NULL) {
  if (!is.null(parties)) {
    if ("Dem USA" %in% parties & !"USA" %in% parties) parties <- c(parties, "USA")
    df[[var]][!df$code %in% parties] <- NA
    na_label <- "Non Parties" }
  table <- heatmap_table(vars = var, data = df, along = "country_map", conditions = c(condition), on_control = on_control, remove_na = FALSE)
  # df_countries <- c(Country_Names[colnames(table)], "Wallis and Futuna", "Vatican", "Tobago", "Trinidad", "Sint Maarten", "Liechtenstein", "Saint Kitts", "Nevis", "Monaco", "Jersey", "Barbuda", "Antigua", "Saint Barthelemy", "Reunion", "Grenadines", "Virgin Islands", "Turks and Caicos Islands", "Saint Pierre and Miquelon", "Saint Helena", "Ascension Island", "Niue", "Palau", "Pitcairn Islands", "South Sandwich Islands")
  # df <- data.frame(country = df_countries, mean = c(as.vector(table), seq(-1.84, 1.94, 0.2), seq(0.06, 0.86, 0.2))) # For oecd_climate
  df_countries <- df$country_map
  df <- data.frame(country_map = df_countries, mean = as.vector(table))

  if (condition != "") {
    if (is.null(breaks)) breaks <- c(-Inf, .2, .35, .5, .65, .8, Inf)
    if (is.null(labels)) labels <- c("0-20%", "20-35%", "35-50%", "50-65%", "65-80%", "80-100%")
    if (is.null(legend)) legend <- paste("Share", condition)
    if (is.null(limits)) limits <- c(0, 1)
  } else {
    if (is.null(breaks)) breaks <- c(-Inf, -1.2, -.8, -.4, 0, .4, .8, 1.2, Inf) # c(-Inf, -1, -.5, -.25, 0, .25, .5, 1, Inf)
    if (is.null(labels)) labels <- c("< -1.2", "-1.2 - -0.8", "-0.8 - -0.4", "-0.4 - 0", "0 - 0.4", "0.4 - 0.8", "0.8 - 1.2", "> 1.2")
    if (is.null(legend)) legend <- "Mean"
    if (is.null(limits)) limits <- c(-2, 2)
  }
  if (continuous) df$mean <- pmax(pmin(df$mean, limits[2]), limits[1])

  world_map <- map_data(map = "world")
  world_map <- world_map[world_map$region != "Antarctica",] #
  world_map <- world_map[!world_map$region %in% c("Antarctica", "American Samoa", "Micronesia", "Guam", "Niue", "Pitcairn Islands", "Cook Islands", "Tonga", "Kiribati", "Marshall Islands", "French Polynesia", "Fiji", "Samoa", "Wallis and Futuna", "Vanuatu"),]
  # world_map$region <- iso.alpha(world_map$region)

  if ("Dem USA" %in% parties) {
    us_states <- map_data(map = "state")
    blue_states <- tolower(c("California", "Illinois", "New York", "New Jersey", "Washington", "Massachusetts", "Oregon", "Connecticut", "Delaware", "Rhode Island", "District of Columbia", "Vermont")) # , "Hawaii" and Alaska missing from the map
    non_blue_states <- setdiff(us_states$region, blue_states)
    us_states$region[us_states$region %in% blue_states] <- "USA" #"Dem USA"
    us_states$region[us_states$region %in% non_blue_states] <- "Non-Dem USA"
    world_map$region[world_map$subregion == "Alaska"] <- "Non-Dem USA"
    world_map <- world_map[world_map$region != "USA",] # | world_map$subregion == "Alaska",]
    world_map <- merge_maps(world_map, us_states) 
  }

  df_na <- data.frame(country_map = setdiff(world_map$region, df_countries), mean = if (fill_na) breaks[2] else NA)
  df <- merge(df, df_na, all = T)

  df$group <- cut(df$mean, breaks = breaks, labels = labels)

  if (!continuous) {
    (plot <- ggplot(df) + geom_map(aes(map_id = country_map, fill = fct_rev(group)), map = world_map) + coord_proj("+proj=robin", xlim = c(-135, 178.5), ylim = c(-56, 84)) + #geom_sf() + #devtools::install_github("eliocamp/ggalt@new-coord-proj") update ggplot2 xlim = c(162, 178.5) for mercator
       geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'grey', size = 0,  fill = NA) + expand_limits(x = world_map$long, y = world_map$lat) + theme_void() + theme(legend.position = c(legend_x, .29)) + # coord_fixed() +
       scale_fill_manual(name = legend, drop = FALSE, values = color(length(breaks)-1, rev_color = rev_color), labels = function(breaks) {breaks[is.na(breaks)] <- na_label; breaks})) #, na.value = "grey50" +proj=eck4 (equal area) +proj=wintri (compromise) +proj=robin (compromise, default) Without ggalt::coord_proj(), the default use is a sort of mercator
  } else {
    (plot <- ggplot(df) + geom_map(aes(map_id = country_map, fill = mean), map = world_map) + coord_proj("+proj=robin") + #geom_sf() + #devtools::install_github("eliocamp/ggalt@new-coord-proj")
       geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'grey', fill = NA) + expand_limits(x = world_map$long, y = world_map$lat) + theme_void() + coord_fixed() +
       scale_fill_manual(palette = "RdBu", direction = 1, limits = limits, na.value = "grey50")) #scale_fill_viridis_c(option = "plasma", trans = "sqrt"))
  }

  print(plot)
  if (save) for (f in format) save_plot(plot, filename = ifelse(continuous, paste0(var, "_cont"), var), folder = '../figures/maps/', width = width, height = height, format = f, trim = trim)
  # return(plot)
}

merge_maps <- function(map1, map2) {
  map1$group <- map1$group + max(map2$group)
  map1$order <- map1$order + max(map2$order)
  return(rbind(map1, map2))
}

dem_us <- non_dem_us <- co2_pop[co2_pop$code == "USA",]
dem_us$country_map <- "Dem USA"
non_dem_us$country_map <- "Non-Dem USA"
dem_us$gain_adj_over_gdp_2040 <- .05
df <- rbind(co2_pop[co2_pop$code != "USA",], dem_us, non_dem_us)
# df <- co2_pop[co2_pop$code != "USA",]
# for (s in unique(us_states$region)) {
#   temp <- co2_pop[co2_pop$code == "USA",]
#   temp$country_map <- s
#   df <- rbind(df, temp)
# }

# df_map <- data.frame(country_map = df$country_map, mean = df$gain_adj_over_gdp_2050) # mean = as.vector(table))
df$group <- cut(df$gain_adj_over_gdp_2040, breaks = c(-Inf, -1.2, -.8, -.4, 0, .4, .8, 1.2, Inf), labels = c("< -1.2", "-1.2 - -0.8", "-0.8 - -0.4", "-0.4 - 0", "0 - 0.4", "0.4 - 0.8", "0.8 - 1.2", "> 1.2"))

us_states <- map_data(map = "state")
blue_states <- tolower(c("California", "Illinois", "New York", "New Jersey", "Washington", "Massachusetts", "Oregon", "Connecticut", "Delaware", "Rhode Island", "District of Columbia", "Vermont")) # , "Hawaii" and Alaska missing from the map
non_blue_states <- setdiff(us_states$region, blue_states)
us_regions <- data.frame(list(state = unique(us_states$region)))
us_regions$region <- ifelse(us_regions$state %in% blue_states, "Dem USA", "Non-Dem USA")
us_states$region[us_states$region %in% blue_states] <- "Dem USA"
us_states$region[us_states$region %in% non_blue_states] <- "Non-Dem USA"
# us_states$group[us_states$region %in% blue_states] <- 1
# us_states$group[us_states$region %in% non_blue_states] <- 2
# non_blue_usa <- maps::map("state", regions = non_blue_states, boundary = FALSE, interior = TRUE, plot = FALSE, fill = TRUE)
# non_blue_usa <- fortify(non_blue_usa)
# non_blue_usa <- us_states %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>% st_transform(crs = 3310)
# non_blue_usa <- non_blue_usa %>% group_by(region)
# non_blue_usa$value <- 0.1
world_map <- map_data(map = "world")
world_map$region[world_map$subregion == "Alaska"] <- "Non-Dem USA"
world_map <- world_map[world_map$region != "USA",]
# row.names(world_map) <- paste0("w", row.names(world_map))
# world_map <- spChFIDs(world_map, row.names(world_map)) # Requires maptools:: or mappoly::merge_maps or rgdal::spRbind
# world_map <- world_map %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>% st_transform(crs = 3310)
world_map <- merge_maps(world_map, us_states)

us_states <- maps::map("state", plot = FALSE, exact = FALSE, fill = TRUE) %>% st_as_sf()
world_map <- map_data(map = "world")
world_map$region[world_map$subregion == "Alaska"] <- "Non-Dem USA"
world_map <- world_map[world_map$region != "USA",] # | world_map$subregion == "Alaska",]
world_map <- maps::map("world", plot = FALSE, exact = FALSE, fill = TRUE, regions = "(?!USA):*|USA:Alaska") #%>% st_as_sf()
names(world_map)[5] <- "ID"
world_map <- world_map[, c(1:5)]
world_map <- world_map %>% st_as_sf()

us_state <- maps::map("state", plot = FALSE, exact = FALSE, fill = TRUE)
IDs <- sapply(strsplit(us_state$names, ":"), function(x) x[1])
us_state <- map2SpatialPolygons(us_state, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
us_state <- merge(us_state, us_regions, by.x = "", )
names(us_states)
world_map <- raster::aggregate(world_map, "region")

states <- getData("GADM", country = "USA", level = 1) %>% st_as_sf() %>% st_transform(crs = 3310)

ggplot(non_blue_usa) +
  geom_sf(aes(fill = value)) +
  geom_sf_text(aes(label = region), check_overlap = T)+
  scale_fill_viridis_c()

megakotas <- states %>%
  left_join(y = rownames_to_column(co2_pop, var = "State"), by = c("NAME_1" = "State")) %>%
  mutate(State = fct_collapse(NAME_1, Megakotas = c("North Dakota", "South Dakota"))) %>%
  # group_by(State) %>%  summarise(Murder = sum(Murder)) %>%
  st_simplify(dTolerance = 1000)

world_map <- world_map %>% st_as_sf() %>% st_transform(crs = 3310) %>% st_simplify(dTolerance = 1000)

us_coord <- map_data("state")
blue_states_coord <- map("state", regions = blue_states, boundary = TRUE, interior = FALSE, plot = FALSE)[c("x", "y")] %>%  base::as.data.frame() %>%  sort_points(y = "y", x = "x") %>%  mutate(region = "Dem USA")
non_blue_states_coord <- map("state", regions = non_blue_states, boundary = TRUE, interior = FALSE, plot = FALSE)[c("x", "y")] %>%  as.data.frame() %>%  sort_points(y = "y", x = "x") %>%  mutate(region = "Non-Dem USA")
names(blue_states_coord) <- names(non_blue_states_coord) <- c("long", "lat", "region")
world_map <- world_map[world_map$region != "USA",]
world_map <- merge_maps(world_map, non_blue_states_coord)

world_map <- world_map %>%
  add_rownames("region") %>%
  mutate(region = replace(tolower(region), tolower(region) %in% c("north dakota", "south dakota"),
                          "megakotas")) %>%
  group_by(region) %>%
  mutate_all(sum)

df <- rbind(co2_pop, co2_pop[co2_pop$code]

ggplot(df) + geom_map(aes(map_id = country_map, fill = fct_rev(group)), map = world_map) + #coord_proj("+proj=robin", xlim = c(-135, 178.5), ylim = c(-56, 84)) + #geom_sf() + #devtools::install_github("eliocamp/ggalt@new-coord-proj") update ggplot2 xlim = c(162, 178.5) for mercator
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'grey', size = 0,  fill = NA) + expand_limits(x = world_map$long, y = world_map$lat) + theme_void() + theme(legend.position = c(0.05, .29)) + # coord_fixed() +
  # geom_map(data = as.data.frame(list(ID = "Non-Dem USA", group = "0.4 - 0.8")), map = us_states, color = 'green', size = 0) +
  # geom_map(data = df, map = world_map, color = 'green', size = 0) +
  scale_fill_manual(name = "mean", drop = FALSE, values = color(8), labels = paste("a", 1:8)) #, na.value = "grey50" +proj=eck4 (equal area) +proj=wintri (compromise) +proj=robin (compromise, default) Without ggalt::coord_proj(), the default use is a sort of mercator


ggplot(mapping = aes(map_id=country_map, fill = group)) +
  geom_map(data = df[!df$code %in% c("USA", "Dem USA", "Non-Dem USA"),],
           map = world_map, size = 0.15, color = "#ffffff") +
  geom_map(data = df[df$code %in% c("Non-Dem USA"),],
           map = non_blue_states_coord, size = 0.15, color = "#ffffff") + 
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_manual(name = "mean", drop = FALSE, values = color(8), labels = paste("a", 1:8)) +
  theme_void() + theme(legend.position = c(0.05, .29))

plot_world_map("gain_adj_over_gdp_2050", condition = "", df = df, save = FALSE, breaks = NULL, labels = NULL, legend = NULL, limits = NULL, parties = NULL)
 
us_states <- map_data("state")
states_data <- data.frame(region = ifelse(us_states$region %in% non_blue_states, "Non-Dem USA", "Dem USA"), long = c(us_states$x), lat = c(us_states$y))
states_sf <- st_as_sf(us_states, coords = c("long", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = states_sf, aes(fill = region, group = region), color = "black") +
  scale_fill_manual(values = c("A" = "lightblue", "B" = "white")) +
  coord_sf(crs = st_crs(4326)) +
  theme_void()

us_coord <- map_data("state") 
non_blue_coord <- map("state", regions = non_blue_states, boundary = TRUE, interior = FALSE, plot = FALSE)[c("x", "y")] %>%
  as.data.frame() %>%   sort_points(y = "y", x = "x") %>% mutate(region = "Non-Dem USA")
blue_coord <- map("state", regions = blue_states, boundary = TRUE, interior = FALSE, plot = FALSE)[c("x", "y")] %>%
  as.data.frame() %>%   sort_points(y = "y", x = "x") %>% mutate(region = "Dem USA")

us_regions <- data.frame(list(state = unique(us_states$region)))
# us_regions$region <- ifelse(us_regions$state %in% blue_states, "Dem USA", us_regions$state)
us_regions$value <- runif(49)
us_regions$area <- us_regions$region
us_regions$region <- us_regions$state
us_regions$region[grepl("Non-Dem USA", us_regions$region)] <- "Non-Dem USA"
# us_regions <- add_rownames(us_regions, "region")

ggplot(mapping = aes(map_id=region, fill = value)) +
  geom_map(data = us_regions[us_regions$region != "Non-Dem USA",],
           map = blue_coord, size = 0.15, color = "#ffffff") +
  geom_map(data =  us_regions[us_regions$region == "Non-Dem USA",],
           map = non_blue_coord, size = 0.15, color = "#ffffff") + 
  expand_limits(x = us_coord$long, y = us_coord$lat) +
  scale_fill_continuous(low = 'thistle2', high = 'darkred', guide = 'colorbar') +
  labs(x=NULL, y=NULL) 

us_coord <- map_data("state") 
megakotas_coord <- map("state", regions = non_blue_states, 
                       boundary = TRUE, interior = FALSE, plot = FALSE)[c("x", "y")] %>%
  as.data.frame() %>%
  sort_points(y = "y", x = "x") %>%
  mutate(region = "dakota")

us_regions$value <- runif(49)
us_regions$area <- us_regions$region
us_regions$region <- us_regions$state
us_regions$region[grepl("dakota", us_regions$region)] <- "dakota"
# us_regions <- add_rownames(us_regions, "region")

ggplot(mapping = aes(map_id=region, fill = value)) +
  geom_map(data = us_regions[us_regions$region != "dakota",],
           map = us_coord, size = 0.15, color = "#ffffff") +
  geom_map(data =  us_regions[us_regions$region == "dakota",],
           map = megakotas_coord, size = 0.15, color = "#ffffff") + 
  expand_limits(x = us_coord$long, y = us_coord$lat) +
  scale_fill_continuous(low = 'thistle2', high = 'darkred', guide = 'colorbar') +
  labs(x=NULL, y=NULL) 

##### Plot along #####
# gives a list of regressions with given covariates and the different values for the 'subsamples' variable and the 'outcomes'
# outcomes, covariates: string vectors / subsamples: variable name
# /!\ when logit_margin = T, we don't take weight into account (haven't found an R function that gives the marginal logit effects with weight)
regressions_list <- function(outcomes, covariates, subsamples = NULL, df = e, logit = c(FALSE), weight = 'weight', atmean = T, logit_margin = T, summary = FALSE) {
  # TODO! handle outcomes of type "future_richness" (so that they are understood as as.numeric(future_richness))
  # TODO! handle along of type "income" (so that they are understood as as.factor(income)) => this can be done using , names_levels = paste0("as.factor(income)", c("Q1", "Q2", "Q3", "Q4"))
  if (length(logit)==1) if (is.null(subsamples)) logit <- rep(logit, length(outcomes)) else logit <- logit <- rep(logit, length(outcomes)*max(1, length(Levels(df[[subsamples]]))))
  regs <- list()
  i <- 0
  if (!is.null(subsamples)) {
    if (subsamples %in% covariates) {
      warning("subsamples should not be in covariates")
      covariates <- covariates[covariates!=subsamples] }
    for (s in Levels(df[[subsamples]])) {
      regs <- c(regs, regressions_list(outcomes = outcomes, covariates = covariates, subsamples = NULL, df = df[df[[subsamples]]==s,], logit = logit[(i*length(outcomes)+1):((i+1)*length(outcomes))], weight = weight, atmean = atmean, logit_margin = logit_margin))
      i <- i + 1   }
  } else for (y in outcomes) {
    several_values <- c()
    for (c in seq_along(covariates)) several_values[c] <- length(unique(df[[covariates[c]]])) > 1
    covariates <- covariates[several_values]
    if ("double.item" %in% class(df[[y]])) dependent_var <- paste0("as.numeric(", y, ")") else dependent_var <- y
    formula <- as.formula(paste(dependent_var, " ~ ", paste(covariates, collapse = ' + ')))
    i <- i + 1
    if (logit[i]) {
      if (logit_margin) {
        reg <- logitmfx(formula, data = df, atmean = atmean) #$mfxest
        if (summary) reg <- reg$mfxest
      } else reg <- glm(formula, family = binomial(link='logit'), data = df, weights = weight)
    } else reg <- lm(formula, data = df, weights = weight)
    if (summary) reg <- summary(reg)
    regs <- c(regs, list(reg))
  }
  return(regs)
}

# Given a set of regressions with one common variable (along), gives the coefs and CI of the levels of that variable.
#   or, if subsamples == along & !missing(covariates), gives the coefs/CI of all covariates with one regression for each subsample
mean_ci_along_regressions <- function(regs, along, labels, df = e, origin = 'others_at_mean', logit = c(FALSE), logit_margin = T, confidence = 0.95, factor_along = FALSE, covariates = NULL,
                                      subsamples = NULL, names_levels = paste0(along, levels_along), levels_along = Levels(df[[along]]), weight = 'weight', print_regs = FALSE) { # to handle numeric variables: levels_along = ifelse(is.numeric(df[[along]]), c(), Levels(df[[along]]))
  # names_levels[1] should correspond to the control group (concatenation of along and the omitted level)
  # origin can be 0, 'intercept': the intercept, the 'control_mean': true (or predicted) mean of the control group, or 'others_at_mean': all variables at their mean except along (at 0 i.e. the control group)
  # TODO: logit, origin
  # TODO! handle continuous covariates; covariates/regressions with only one level; solve glitch of displaying some omitted values (e.g. Vote: Left)
  if (!is.null(subsamples) && subsamples == along && !is.null(covariates)) {
    mean_ci <- data.frame()
    for (v in covariates) {
      mean_ci_v <- mean_ci_along_regressions(regs = regs, along = v, labels = levels_along, subsamples = subsamples, df = df, covariates = NULL, origin = origin, logit = logit, logit_margin = logit_margin, confidence = confidence, factor_along = factor_along, weight = weight)
      mean_ci_v$along <- paste0(v, mean_ci_v$along)
      if (exists("labels_vars")) mean_ci_v$along <- labels_vars[mean_ci_v$along]
      names(mean_ci_v) <- c("along", "mean", "CI_low", "CI_high", "y")
      mean_ci <- rbind(mean_ci, mean_ci_v)
      mean_ci <- mean_ci[mean_ci$y != "<NA>",]
      mean_ci <- mean_ci[!is.na(mean_ci$mean),]
    }
  } else {
    if (length(names_levels) == 1 & grepl("(", names_levels, fixed = T)) {
      lev <- sub("(", "", sub(")", "", names_levels, fixed=T), fixed=T)
      levels_along <- c("FALSE", "TRUE")
      names_levels <- paste0(lev, levels_along)
    }
    if (factor_along) names_levels <- paste0("as.factor(", along, ")", levels_along)
    k <- length(names_levels)
    if (k < 2) warning("along must have several levels.")
    if (length(levels_along)!=length(names_levels)) warning("levels_along and names_levels must have same length.")
    mean_ci <- data.frame()
    i <- 0
    if (class(regs) != "list") regs <- list(regs)
    if (length(logit)==1) logit <- rep(logit, length(regs)) # TODO determine automatically whether logit through class(regs)
    for (r in regs) {
      if (print_regs) print(summary(r))
      i <- i+1
      label <- rep(labels[i], length(levels_along))
      if (!is.null(subsamples)) data_s <- df[df[[subsamples]] == Levels(df[[subsamples]])[i],] else data_s <- df
      if (!("weight" %in% names(data_s))) data_s$weight <- 1
      if (logit[i] & logit_margin) {
        regmxf <- r$mfxest
        reg <- r$fit }
      else reg <- r
      if (origin == 'intercept') origin_value <- reg$coefficients[["(Intercept)"]]
      else if (origin == 'others_at_mean') {
        if (names(reg$coefficients)[1] == "(Intercept)") origin_value <- reg$coefficients[["(Intercept)"]]
        else origin_value <- 0
        variables <- names(reg$model)[2:length(names(reg$model))]
        for (v in variables) {
          if (v %in% names(reg$xlevels) & v != along) for (j in reg$xlevels[[v]][2:length(reg$xlevels[[v]])]) {
            name_var <- sub("^as\\.factor\\((.*)\\)$", "\\1", "as.factor(income)")
            if (!name_var %in% names(data_s)) warning(paste(name_var, "not in names(df): its coefficient set to 0 to compute the origin value."))
            else origin_value <- origin_value + reg$coefficients[[paste0(v, j)]] * wtd.mean(data_s[[name_var]] == j, weights = data_s$weight, na.rm = T) }
          else if (v %in% names(reg$coefficients) & !(v %in% c(along, "(weights)"))) origin_value <- origin_value + reg$coefficients[[v]] * wtd.mean(data_s[[v]], weights = data_s$weight, na.rm = T)
        }
        if (logit[i]) origin_value <- 1/(1+exp(-origin_value)) # cf. https://www.princeton.edu/~otorres/LogitR101.pdf for an alternative coding
      } else if (origin == 'control_mean') {
        dependent_var <- if (logit[i] & logit_margin) reg$y else reg$model[[1]]
        origin_value <- wtd.mean(dependent_var[data_s[[along]] == levels_along[1]], weights = data_s$weight[data_s[[along]] == levels_along[1]])
      } else origin_value <- 0
      if (logit[i] & logit_margin) { # logit margins
        # if ("lm" %in% class(reg)) warning("Logit margins should be provided: logitmfx(..)")
        for (l in names_levels[2:k]) if (!l %in% row.names(regmxf)) {
          warning(paste("Covariate", l, "is absent from regression, replaced by NA."))
          regmxf <- rbind(regmxf, rep(NA, ncol(regmxf)))
          row.names(regmxf)[length(row.names(regmxf))] <- l }

        coefs <- origin_value + c(0, regmxf[names_levels[2:k],1])

        # SEs <- c(0, regmxf[names_levels[2:k],2])
        # z <- qnorm(1-(1-confidence)/2)
        # CI <- cbind(coefs - z*SEs, coefs + z*SEs) # CIs approximated using Standard Errors of logitmargin(...)$mfxest. Pb: no CI for omitted variable.

        n <- length(reg$fitted.values)
        t <- qt(1-(1-confidence)/2, n)
        sigma <- sqrt(wtd.mean((reg$y - reg$fitted.values)^2)) #, weights = data_s$weight)) TODO: handle weight for logit_margin (uncommenting this would only work when there is no missing value so that length(data_s$weight)==length(reg$y))
        SDs <- sapply(levels_along, function(i) return(sqrt(wtd.mean(((data_s[[along]] == i) - wtd.mean(data_s[[along]] == i, weights = data_s$weight, na.rm = T))^2, weights = data_s$weight, na.rm = T))))
        CI <- cbind(coefs - t*sqrt(1/n)*sigma/SDs, coefs + t*sqrt(1/n)*sigma/SDs) # CIs approximated as if the results were that of a linear regression. Stata uses a more appropriate method: the Delta method (which also have some issues: CIs can be outside [0; 1]), not easily implementable in R (despite msm::deltamethod)
      } else { # OLS
        if (logit[i]) warning("Are you sure you want the logit coefficients rather than the marginal effects? If not, set logit_margin = T.")
        #   mean_ci_origin <- binconf(x = sum(data_s[[weight]][data_s[[along]] == levels_along[1]], na.rm=T), n = sum(data_s[[weight]], na.rm=T), alpha = 1-confidence) # WRONG! This is the CI for the group mean, not the CI for the regression coefficient!
        #   CI_origin <- mean_ci_origin[2:3] - (origin_value == 0) * mean_ci_origin[1] # c(0, 0) # TODO! check that this confidence interval at the origin is correct (I doubt it)
        n <- length(reg$fitted.values)
        t <- qt(1-(1-confidence)/2, n)
        if (!("weight" %in% names(data_s))) data_s$weight <- 1
        if (!("weights" %in% names(reg))) reg$weights <- 1
        sigma <- sqrt(wtd.mean((reg$model[[1]] - reg$fitted.values)^2, weights = reg$weights))
        SD <- sqrt(wtd.mean(((data_s[[along]] == levels_along[1]) - wtd.mean(data_s[[along]] == levels_along[1], weights = data_s$weight, na.rm = T))^2, weights = data_s$weight, na.rm = T))
        CI_origin <- c(-1, 1)*t*sqrt(1/n)*sigma/SD # This computation is very close to confint(...), which we can't use for the omitted variable.
        coefs <- origin_value + c(0, reg$coefficients[names_levels[2:k]]) # what about emmeans(reg, ~ 1 | along) to compute e.g. CI_origin?
        # CI <- origin_value + rbind(CI_origin, confint(reg, names_levels[2:k], confidence)) # if simple OLS
        robust_SEs <- coeftest(reg, vcov = vcovHC(reg, "HC1")) # another way to get (non-robust) SEs is summary(reg)$coefficients[,2]
        # print(robust_SEs)
        # print(names_levels)
        for (l in names_levels[2:k]) if (!l %in% row.names(robust_SEs)) {
          warning(paste("Covariate", l, "is absent from regression, replaced by NA."))
          names(coefs) <- names_levels[1:k]
          robust_SEs <- rbind(robust_SEs, rep(NA, ncol(robust_SEs)))
          row.names(robust_SEs)[length(row.names(robust_SEs))] <- l }
        CI <- origin_value + rbind(CI_origin, cbind(robust_SEs[names_levels[2:k], 1] - t*robust_SEs[names_levels[2:k], 2], robust_SEs[names_levels[2:k], 1] + t*robust_SEs[names_levels[2:k], 2]))
      }
      mean_ci_reg <- data.frame(y = label, mean = coefs, CI_low = CI[,1], CI_high = CI[,2], along = levels_along)
      mean_ci <- rbind(mean_ci, mean_ci_reg)
    }
    row.names(mean_ci) <- NULL
  }
  return(mean_ci)
}


# Two cases: with covariates (coefficients or marginal effects are shown, depending on origin = 0 or not) or without covariates (i.e. unconditional means of subgroups)
# Four configurations: a. and b. one outcomes, two heterogeneities / c. and d. different outcomes, one heterogeneity (d. is invert_y_along = T)
# a. one outcome, y: subsamples (e.g. countries), along: heterogeneity; b. one outcome, y: covariates, along = subsamples: heterogeneity (e.g. countries)
# c. y: outcomes, along; d. y: heterogeneity, along: outcomes
# For numerical outcomes (i.e. not dummies), set conditions to rep("", length(outcomes))
mean_ci <- function(along, outcome_vars = outcomes, outcomes = paste0(outcome_vars, conditions), covariates = NULL, subsamples = NULL, conditions = c(" > 0"), invert_y_along = FALSE, df = e, labels = outcome_vars, factor_along = FALSE,
                    origin = 'others_at_mean', logit = c(FALSE), weight = 'weight', atmean = T, logit_margin = T, confidence = 0.95,
                    labels_along = levels_along, names_levels = paste0(along, levels_along), levels_along = Levels(df[[along]]), heterogeneity_condition = "", order_y = NULL, order_along = NULL, print_regs = print_regs) {
  z <- qnorm(1-(1-confidence)/2)
  if (all(labels_along == levels_along) & is.logical(df[[along]])) { labels_along <- paste0(along, ": ", levels_along) }
  names(labels_along) <- as.character(levels_along) # setNames(levels_along, levels_along)
  if (!is.null(covariates)) { # If conditional (regressions)
    if (!(along %in% c(covariates, subsamples))) print("ERROR: along must be in covariates")
    if (any(logit) & !logit_margin) print("Warning: Are you sure you want the logit coefficients rather than the marginal effects? If not, set logit_margin = T.")
    if (!is.null(subsamples) & (missing(labels) | identical(labels, outcome_vars))) labels <- Levels(df[[subsamples]])
    regs <- regressions_list(outcomes = outcomes, covariates = covariates, subsamples = subsamples, df = df, logit = logit, weight = weight, atmean = atmean, logit_margin = logit_margin, summary = FALSE)
    mean_ci <- mean_ci_along_regressions(regs = regs, along = along, labels = labels, df = df, origin = origin, logit = logit, logit_margin = logit_margin, confidence = confidence, subsamples = subsamples, covariates = covariates, names_levels = names_levels, levels_along = levels_along, factor_along = factor_along, weight = weight, print_regs = print_regs)
    mean_ci$along <- labels_along[as.character(mean_ci$along)]
  } else { # If unconditional (subgroup means)
    if (!is.null(subsamples)) { # Configuration a.
      if (length(outcomes) > 1) warning("There cannot be several outcomes with subsamples, only the first outcome will be used.")
      outcome <- outcomes[1]
      y_loop <- Levels(df[[subsamples]])
      if (missing(labels) | identical(labels, outcome_vars)) labels <- y_loop # TODO: replace by/use name_levels or levels_along
      if (is.character(y_loop)) y_loop <- paste0("'", y_loop, "'")
      cond <- paste0("[x$", subsamples, "==", y_loop, "]")
      configurations <- paste0("(x$", outcome, ")", cond, ", w = x[[weight]]", cond)
    } else { # Configuration c.
      y_loop <- outcomes
      configurations <- paste0("x$", y_loop, ", w = x[[weight]]")
    }
    i <- 0
    mean_ci <- data.frame()
    for (configuration in configurations) {
      i <- i + 1
      mean_ci_reg <- as.data.frame(sapply(split(df, eval(str2expression(paste("df[[along]]", heterogeneity_condition, sep = "")))),
                                          function(x) c(eval(str2expression(paste("wtd.mean(", configuration, ", na.rm=T)"))),
                                                        eval(str2expression(paste("sqrt(modi::weighted.var(", configuration,", na.rm=T))/sqrt(NROW(x))"))))))
      mean_ci_reg <- as.data.frame(t(apply(mean_ci_reg, 2, function(x) c(x[1],x[1]-z*x[2], x[1]+z*x[2])))) # /!\ This is a CI for a normal distribution, only an approximation in cases of binomial distributions
      mean_ci_reg <- tibble::rownames_to_column(mean_ci_reg, along)
      mean_ci_reg$y <- labels[i]
      names(mean_ci_reg) <- c("along", "mean", "CI_low", "CI_high", "y")
      mean_ci_reg$along <- labels_along[as.character(mean_ci_reg$along)]
      mean_ci <- rbind(mean_ci, mean_ci_reg)
    }
  }
  if (invert_y_along) names(mean_ci) <- c("y", "mean", "CI_low", "CI_high", "along")
  # if (invert_y_along) {
  #   names(mean_ci)[which(names(mean_ci) == "along")] <- "temp"
  #   names(mean_ci)[which(names(mean_ci) == "y")] <- "along"
  #   names(mean_ci)[which(names(mean_ci) == "temp")] <- "y"  }

  if (exists("countries_names")) {
    if (all(Levels(mean_ci$along)==sort(countries_names))) mean_ci$along <- factor(mean_ci$along, levels = countries_names)
    if (all(Levels(mean_ci$y)==sort(countries_names))) mean_ci$y <- factor(mean_ci$y, levels = rev(countries_names))
  }
  if (!is.null(order_y)) if (sort(Levels(mean_ci$y))==sort(order_y)) mean_ci$y <- factor(mean_ci$y, levels = order_y)
  if (!is.null(order_along)) if (sort(Levels(mean_ci$along))==sort(order_along)) mean_ci$along <- factor(mean_ci$along, levels = order_along)
  return(mean_ci)
}

plot_along <- function(along, mean_ci = NULL, vars = outcomes, outcomes = paste0(vars, conditions), covariates = NULL, subsamples = NULL, conditions = c(" > 0"), invert_y_along = FALSE, df = e, labels = vars, factor_along = FALSE,
                       origin = 'others_at_mean', logit = c(FALSE), atmean = T, logit_margin = T, labels_along = levels_along, names_levels = paste0(along, levels_along), levels_along = Levels(df[[along]]),  # condition = "> 0", #country_heterogeneity = FALSE, along_labels,
                       confidence = 0.95, weight = "weight", heterogeneity_condition = "", return_mean_ci = FALSE, print_name = FALSE, legend_top = FALSE, to_percent = FALSE, colors = NULL, color_RdBu = FALSE,
                       legend_x = '', legend_y = '', plot_origin_line = FALSE, name = NULL, folder = '../figures/country_comparison/', width = dev.size('px')[1], height = dev.size('px')[2], save = T, order_y = NULL, order_along = NULL) {
  # TODO multiple conditions, show legend for 20 countries (display UA!) even if there is less than 4 variables
  # TODO: automatic values when missing(legend_x), legend_y
  # TODO! make invert_y_along work for regressions/covariates
  if (exists("labels_vars") & missing(labels)) labels[vars %in% names(labels_vars)] <- labels_vars[vars[vars %in% names(labels_vars)]]
  if (missing(name) & !missing(vars) & !missing(along)) {
    if (any(grepl('["\']', deparse(substitute(vars))))) {
      name <- ifelse(invert_y_along, paste0(along, "_by_", vars[1]), paste0(vars[1], "_by_", along))
      warning("The filename is formed with the first variable name, given that the argument 'name' is missing.")
    } else name <- ifelse(invert_y_along, name <- paste0(along, "_by_", deparse(substitute(vars))), paste0(deparse(substitute(vars)), "_by_", along))
  } else if (missing(name) & !missing(mean_ci)) {
    potential_name <- deparse(substitute(mean_ci))
    if (missing(along)) along <- ""
    if (grepl('["\']', potential_name)) warning("(file)name should be provided, or mean_ci should have a name.")
    name <- ifelse(invert_y_along, paste0(along, "_by_", mean_ci), paste0(mean_ci, "_by_", along))
  } else if (missing(name)) name <- "temp"
  name <- sub("rev(", "", sub(")", "", sub("country_name", "country", name, fixed = T), fixed = T), fixed = T)
  if (print_name) print(name) # TODO: name with subsamples

  if (missing(folder) & deparse(substitute(df)) %in% tolower(countries)) folder <- paste0("../figures/", toupper(deparse(substitute(df))), "/")

  if (missing(mean_ci)) mean_ci <- mean_ci(along = along, outcome_vars = vars, outcomes = outcomes, covariates = covariates, subsamples = subsamples, conditions = conditions, invert_y_along = invert_y_along, df = df, labels = labels, factor_along = factor_along,
                                           origin = origin, logit = logit, weight = weight, atmean = atmean, logit_margin = logit_margin, confidence = confidence, order_y = order_y, order_along = order_along,
                                           names_levels = names_levels, labels_along = labels_along, levels_along = levels_along, heterogeneity_condition = heterogeneity_condition, print_regs = return_mean_ci)

  #  if (missing(mean_ci)) {
  #    mean_ci <- bind_rows((lapply(vars, heterogeneity_mean_CI, heterogeneity_group = along, df=df, weight = weight, along_labels = along_labels, country_heterogeneity = country_heterogeneity, heterogeneity_condition = heterogeneity_condition, condition = condition, confidence = confidence)))
  #    mean_ci$y <- factor(mean_ci$y, levels = vars, labels = labels) }
  #
  #  if (invert_y_along & country_heterogeneity == F) {
  #    names(mean_ci)[which(names(mean_ci) == "along")] <- "temp"
  #    names(mean_ci)[which(names(mean_ci) == "y")] <- "along"
  #    names(mean_ci)[which(names(mean_ci) == "temp")] <- "y" # or the les robust one-liner: names(mean_ci) <- c("variable", "mean", "CI_low", "CI_high", "along")
  #  } else if (country_heterogeneity) {
  #    names(mean_ci)[which(names(mean_ci) == "variable")] <- "policy" # TODO: generalize this by rewriting heterogeneity_mean_CI
  #    names(mean_ci)[which(names(mean_ci) == "country")] <- "y"
  # }

  if (plot_origin_line) {
    origins <- mean_ci$mean[mean_ci$along == levels_along[1]]
    names(origins) <- mean_ci$y[mean_ci$along == names_levels[1]]
  } else origins <- c()
  if (to_percent) mean_ci[,c("mean", "CI_low", "CI_high")] <- 100*mean_ci[,c("mean", "CI_low", "CI_high")]
  if (color_RdBu) colors <- sub("#F7F7F7", "#FFED6F", color(length(Levels(df[[along]])), rev_color = T)) # , grey_replaces_last = T, grey = T

  plot <- ggplot(mean_ci) + sapply(origins, function(xint) geom_vline(aes(xintercept = xint), linetype = "longdash", color = "grey")) + # For plot, we need mean_ci (cols: mean, CI_low,high, variable, along), legend_x, legend_y. For save, we need: name, folder, width, height.
    geom_pointrange( aes(x = mean, y = y, color = along, xmin = CI_low, xmax = CI_high), position = position_dodge(width = .5)) +
    labs(x = legend_x, y = legend_y, color="") + theme_minimal() + theme(legend.title = element_blank(), legend.position = ifelse(legend_top, "top", "right")) +
    {if (!missing(colors)) scale_color_manual(values = colors)} # + scale_color_manual(values = color(length(levels_along), theme='rainbow')) # can be theme = 'rainbow', 'RdBu', 'default' or any brewer theme, but the issue with RdBu/default is that the middle one is white for odd number of categories
  # scale_color_manual(labels = Levels(df[[along]]), values = color(length(Levels(df[[along]])), theme='rainbow'))# BUG when we specify labels: the legend does not correspond to the colors
  plot
  if (save) save_plotly(plot, filename = name, folder = folder, width = width, height = height, trim = T)
  if (return_mean_ci) return(mean_ci)
  else return(plot)
}


# # var_to_decompose and group_of_interest: you need to input only one variable as a character
# # controls and indices, can be a character vector
# # Factor variables from control need to be in controls_factor
# gelbach_decomposition <- function(var_to_decompose, group_of_interest, controls, controls_factor, indices, df=e, weights = "weight") {
#   # We restrict the df to the variables we'll use, since there can be some incompatibilities
#   # in using R dataframes in Stata
#   df <- df %>%
#     select(c(var_to_decompose, group_of_interest, controls, controls_factor, indices, weights))
#
#   # Rename var because problem with Stata for variables with names too long
#   indices_short <- c()
#   for (i in seq_along(indices)){
#     indices_short[i] <- paste("index_", i, sep = "")
#   }
#   df <- df %>%
#     rename_with(~ indices_short[which(indices == .x)], .cols = indices)
#   df <- df %>%
#     rename("var_to_decompose" = var_to_decompose)
#
#
#   # First, we prepare the options for the analysis
#   option_b1x2 <- ""
#   for (i in seq_along(indices)){
#     option_b1x2 <- paste(option_b1x2,"g", i, " = ", indices_short[i], " : ", sep = "")
#   }
#   option_b1x2 <- substr(option_b1x2, 1, nchar(option_b1x2)-3)
#   nbr_indices <- length(indices)
#
#   # We stock the different lines of codes for Stata into a vector
#   # Each element corresponds to a different line of code to run in Stata
#   # We will then collapse those commands altogether to run them w/ RStata
#   stata_cmd <- c()
#   stata_cmd[1] <- "
#   set more off
#   //ssc install b1x2, replace"
#   stata_cmd[2] <- paste("global indices", paste('"', paste(indices_short, collapse = " "), '"', sep =""), sep = " ")
#   stata_cmd[3] <- paste("global controls", paste('"', paste(controls, collapse = " "), '"', sep = ""), sep = " ")
#   stata_cmd[4] <- paste("global controls_factor", paste('"', paste(controls_factor, collapse = " "), '"', sep = ""), sep = " ")
#   stata_cmd[5] <- paste("global option_b1x2", paste('"', option_b1x2, '"', sep = ""), sep = " ")
#   stata_cmd[6] <- paste("global nbr_indices", paste(nbr_indices), sep = " ")
#   stata_cmd[7] <- paste("global nbr_plus_one_indices", paste(nbr_indices+1), sep = " ")
#   stata_cmd[8] <- paste("global var_to_decompose", paste("var_to_decompose"), sep = " ")
#   stata_cmd[9] <- paste("local var_to_decompose", paste("var_to_decompose"), sep = " ")
#   stata_cmd[10] <- paste("global group_of_interest", paste(group_of_interest), sep = " ")
#   stata_cmd[11] <- paste("local group_of_interest", paste(group_of_interest), sep = " ")
#   stata_cmd[12] <- paste("global local_weight [aw=", paste(weights), paste("]"), sep = "")
#   stata_cmd[13] <- "do gelbach_stata.do"
#
#   stata_cmd <- paste(stata_cmd, collapse = "\n")
#   # We input df, and obtain the data frame with the share explained by each indice
#   final <- stata(stata_cmd, data.in = df, data.out = T)
#
#   return(final)
# }

representativeness_table <- function(country_list, weighted = T, non_weighted = T, label_operator = union, all = FALSE, omit = c("Other", "Not 25-64", "Employment_18_64: Employed", "Employment_18_64: 65+", "PNR", "Urban: FALSE"),
                                     filename = NULL, folder = "../tables/sample_composition/", return_table = FALSE, threshold_skip = 0.01, weight_var = "weight", abbr = NULL) {
  rows <- c()
  pop <- sample <- sample_weighted <- labels <- list()
  for (i in seq_along(country_list)) {
    df <- d(country_list[i])
    k <- country_list[i]
    c <- sub("[0-9p]+", "", toupper(k))

    labels[[k]] <- "Sample size"
    pop[[k]] <- ""
    sample[[k]] <- sample_weighted[[k]] <- prettyNum(nrow(df), big.mark = ",")
    quota_variables <- if (all & paste0(c, "_all") %in% names(quotas)) quotas[[paste0(c, "_all")]] else quotas[[c]]
    for (q in quota_variables) {
      q_name <- ifelse(q %in% names(levels_quotas), q, paste0(c, "_", q))
      for (j in seq_along(levels_quotas[[q_name]])) {
        # print(paste(c, q_name, j))
        if (pop_freq[[c]][[q_name]][j] > threshold_skip) {
          labels[[k]] <- c(labels[[k]], paste0(capitalize(q), ": ", levels_quotas[[q_name]][j]))
          pop[[k]] <- c(pop[[k]], sprintf("%.2f", round(pop_freq[[c]][[q_name]][j], digits = 2)))
          sample[[k]] <- c(sample[[k]], sprintf("%.2f", round(mean(as.character(df[[q]]) == levels_quotas[[q_name]][j], na.rm = T), digits = 2)))
          if (weighted) sample_weighted[[k]] <- c(sample_weighted[[k]], sprintf("%.2f", round(wtd.mean(as.character(df[[q]]) == levels_quotas[[q_name]][j], na.rm = T, weights = df[[weight_var]]), digits = 2)))
        }
      }
    }
    names(pop[[k]]) <- names(sample[[k]]) <- labels[[k]]
    if (weighted) names(sample_weighted[[k]]) <- labels[[k]]
    rows <- label_operator(rows, labels[[k]])
  }

  variables <- unique(gsub("(.*):.*", "\\1", rows))
  order_rows <- c()
  for (v in variables) for (i in 1:length(rows)) order_rows <- c(order_rows, if (v == sub("(.*):.*", "\\1", rows[i])) i else NULL)
  rows <- rows[order_rows]
  table <- data.frame(row.names = rows)
  for (k in country_list) {
    table[[paste0(k, "_pop")]] <- pop[[k]][rows]
    if (non_weighted) table[[paste0(k, "_sample")]] <- sample[[k]][rows]
    if (weighted) table[[paste0(k, "_sample_weighted")]] <- sample_weighted[[k]][rows]
  }

  table <- table[!multi_grepl(omit, row.names(table)),]
  if (return_table) return(table)
  else export_representativeness_table(table = table, country_list = country_list, weighted = weighted, non_weighted = non_weighted, filename = if (all) paste0(filename, "_all") else filename, folder = folder, abbr = abbr)
}
# representativeness_table(c("US1"), return_table = T)
export_representativeness_table <- function(table, country_list, weighted = T, non_weighted = T, filename = NULL, folder = "../tables/sample_composition", abbr = NULL) {
  header <- c("", rep((1 + weighted + non_weighted), length(country_list)))
  names(header) <- c("", country_list)

  line_sep <- c()
  for (i in 2:nrow(table)) {
    previous <- substr(row.names(table)[i-1], 1, 5)
    current <- substr(row.names(table)[i], 1, 5)
    line_sep <- c(line_sep, if (previous == current) "" else "\\addlinespace")
  }

  row.names(table) <- gsub("_", "\\_", row.names(table), fixed = T)
  if (is.null(abbr)) abbr <- length(country_list) > 3
  latex_output <- kbl(table, "latex", caption = NULL, position = "b", escape = F, booktabs = T,
                      col.names = rep(c("Pop.", if (non_weighted) {if (abbr) "Sam." else ("Sample")}, if (weighted) {if (abbr) "\\makecell{Wght.\\\\sam.}" else "\\makecell{Weighted\\\\sample}"}), length(country_list)),
                      linesep = line_sep) %>% add_header_above(header)

  if (is.null(filename)) filename <- paste(country_list, collapse = "_")
  if (filename == "_all") filename <- paste0(paste(country_list, collapse = "_"), "_all")
  cat(paste(latex_output, collapse="\n"), file = paste0(folder, filename, ".tex"))
}

reweighted_estimate <- function(predicted_var = NULL, country = "EU", weights = FALSE, verbose = FALSE, omit = c()) { 
  # Regresses predicted_var on 'levels_quotas[[country]]' and predicts the value based on population frequencies in 'pop_freq'.
  # If is.null(predicted_var), returns instead the vector of population frequencies with which to multiply the vector of coefficients of the regression.
  e <- d(country)
  freq_pop <- pop_freq[[sub("[0-9p]+", "", country)]]
  for (i in names(freq_pop)) names(freq_pop[[i]]) <- levels_quotas[[i]]
  vars_pop <-  names(freq_pop)
  covariates <- vars_pop
  freq_vec <- c("(Intercept)" = 1)
  for (c in vars_pop) {
    c_name <- sub(paste0("^", sub("[0-9p]+", "", country), "_"), "", c)
    # print("character.item" %in% class(e[[c_name]]))
    # if (c %in% names(e) && !(is.factor(e[[c]]) | is.character(e[[c]])) & paste0(c, "_factor") %in% names(e)) new_c <- paste0(c, "_factor")
    # else if (c %in% names(e) && !(is.factor(e[[c]]) | is.character(e[[c]])) & paste0(sub("_.*", "", c), "_factor") %in% names(e)) new_c <- paste0(sub("_.*", "", c), "_factor")
    if ("character.item" %in% class(e[[c_name]])) new_c <- paste0("as.character(as.factor(", c_name, "))")
    else if (!(is.factor(e[[c_name]]) | is.character(e[[c_name]]) | is.logical(e[[c_name]]))) new_c <- paste0("as.character(", c_name, ")")
    else new_c <- c_name
    # print(c)
    covariates[covariates == c] <- new_c
    if (Levels(e[[c_name]])[1] %in% c("TRUE / FALSE / NA", "TRUE / FALSE")) freq_vec[paste0(new_c, "TRUE")] <- freq_pop[[c]][[1]]
    else for (i in intersect(Levels(e[[c_name]]), levels_quotas[[c]])) freq_vec[paste0(new_c, i)] <- freq_pop[[c]][[i]]
  }
  
  if (is.null(predicted_var)) return(freq_vec)
  else {
    formula_reg <- reg_formula(predicted_var, covariates[!multi_grepl(omit, covariates)])
    if (weights) reg <- lm(formula_reg, data = e, weights = e$weight)
    else reg <- lm(formula_reg, data = e)
    if (verbose) {
      print(paste("Share of observations in regression:", length(reg$fitted.values)/nrow(e)))
      print("Levels without a coefficient:")
      print(setdiff(names(freq_vec), names(reg$coefficients)))    
    }
    if (length(setdiff(names(reg$coefficients), names(freq_vec))) > 0) {
      warning(paste("Coefficients not taken into account:", paste(setdiff(names(reg$coefficients), names(freq_vec)), collapse = "; ")))
    }
    estimate <- sum(reg$coefficients[intersect(names(reg$coefficients), names(freq_vec))] * freq_vec[intersect(names(reg$coefficients), names(freq_vec))], na.rm = T)
    return(estimate)
  }
}
