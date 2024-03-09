atpol_files <- list.files(path = "/home/sapi/80gb",
                          pattern = "[0-9]{4}.LOC$",
                          recursive = TRUE,
                          full.names = TRUE)


atpol_files

for (j in seq_along(atpol_files)) {
  x <- atpolR::extract_data_from_old_atpol(atpol_files[[j]])
  if(!is.null(x)) {
    x |>
      dplyr::mutate(date = lubridate::year(date)) |>
      dplyr::arrange(date) |>
      write.csv(file = gstools::format_file_name(name = x[1,"species"][[1]], 
                                                 dir = "data/atpol",
                                                 extension = "csv"),
                row.names = FALSE)
  }
}

atpol_files


# old TURBOVEG ------------------------------------------------------------------------------------------

turboveg_path <- "/home/sapi/80gb/TURBOVEG"

list.dirs(path = turboveg_path)

# Species list from FLORA dir

old_species_list <- foreign::read.dbf(paste0(turboveg_path, "/FLORA/TVFLORA.DBF")) |>
  subset(SYNONYM == FALSE)
poland_species <- foreign::read.dbf("/home/sapi/.wine/drive_c/Turbowin/species/Poland/species.dbf") |>
  subset(SYNONYM == FALSE)

old_species_list |>
#  head() |>
  dplyr::left_join(poland_species, by = c("SPECIES_NR" = "OLDSPECNR")) |>
  dplyr::mutate(abx = stringr::str_replace(as.character(ABBREVIAT.x), pattern = "ssp.", replacement = "subsp."),
                aby = stringr::str_replace(as.character(ABBREVIAT.y), pattern = "ssp.", replacement = "subsp.")) |>
  dplyr::mutate(abx = stringr::str_replace(abx, pattern = "v. ", replacement = "var. "),
                aby = stringr::str_replace(aby, pattern = "v. ", replacement = "var. ")) |>
  subset(abx != aby) |>
  subset(select = c(abx, aby))

# seems old species list ~ new species list for Poland


list.files(path = paste0(turboveg_path, "/AGNEST1"))

# releve headers
foreign::read.dbf("/home/sapi/80gb/TURBOVEG/AGNEST1/TVHABITA.DBF") |>
  head()

# releve - species

foreign::read.dbf("/home/sapi/80gb/TURBOVEG/AGNEST1/TVABUND.DBF") |>
  subset(RELEVE_NR == 1840) |>
#  dplyr::left_join(poland_species, by = c("SPECIES_NR" = "OLDSPECNR"))
  dplyr::left_join(old_species_list, by = "SPECIES_NR")





  library(vegdata)

vegdata::tv.home()

vegdata::tv.refl("Poland")

vegdata::tax('Quercus robur')

vegdata::tv.db()

db <- "alte_buchern"
#nagłówki zdjęć 
# sites <- 
  tv.site(db) |>
    head()

species <- read.dbf("/home/sapi/80gb/TURBOVEG/FLORA/TVFLORA.DBF")

species <- species |>
  dplyr::distinct(SPECIES_NR, .keep_all = TRUE)

plots <- read.dbf("/home/sapi/80gb/TURBOVEG/AGNEST1/TVHABITA.DBF")

plots$
  
  details <- read.dbf("/home/sapi/80gb/TURBOVEG/AGNEST1/TVABUND.DBF")

read.dbf("/home/sapi/80gb/TURBOVEG/AGNEST1/REMARKS.DBF")

plots |>
  dplyr::left_join(details, by = "RELEVE_NR") |>
  dplyr::left_join(species, by = "SPECIES_NR")

plots$DATE
nchar(as.character(plots$DATE)) == 4
species |>
  nrow()

species |>
  dplyr::distinct(SPECIES_NR, .keep_all = TRUE)


stringi::stri_enc_detect(plots[1, "REMARKS"])

stringi::stri_encode(plots[1, "REMARKS"], from = "CP-852", to = "utf8")
iconv(plots[1, "REMARKS"], from = "CP852", to = "utf8")



# dbread ------------------------------------------------------------------------------------------------------------

db <- "AGNEST1"
tv_home <- "/home/sapi/.wine/drive_c/Turbowin"

function (db, tv_home, drop = TRUE, common.only = FALSE, verbose = TRUE, 
          replace.names, ...) 
{
  if (missing(tv_home)) 
    tv_home <- tv.home()
  site <- read.dbf(file.path(tv_home, "Data", db[1], "tvhabita.dbf"), 
                   as.is = TRUE)
  names(site) <- TCS.replace(names(site))
  if (!missing(replace.names)) {
    for (i in 1:nrow(replace.names)) names(site) <- sub(paste("^", 
                                                              replace.names[i, 1], "$", sep = ""), replace.names[i, 
                                                                                                                 2], names(site))
  }
  if (suppressWarnings(any(site[, sapply(site, is.numeric)] < 
                           -1e+05, na.rm = TRUE))) 
    message(paste("WARNING! Values less than -100,000. \n", 
                  "WARNING! tvhabita.dbf may be corrupt. \n", "WARNING! Please correct by im- / exporting e.g. with OpenOffice."))
  if (length(db) > 1) 
    for (i in 2:length(db)) {
      site.tmp <- read.dbf(file.path(tv_home, "Data", 
                                     db[i], "tvhabita.dbf"), as.is = TRUE)
      names(site.tmp) <- TCS.replace(names(site.tmp))
      if (!any(c("SURF_AREA", "AREA_MIN") %in% names(site.tmp))) 
        stop(db[i])
      if (any(site$PlotObservationID %in% site.tmp$PlotObservationID)) 
        stop("Found duplicate releve numbers in ", db[i], 
             " aborting!")
      cols1 <- names(site)
      cols2 <- names(site.tmp)
      if (common.only) {
        common <- intersect(cols1, cols2)
        site <- rbind(site[, common], site.tmp[, common])
      }
      else {
        All <- union(cols1, cols2)
        miss1 <- setdiff(All, cols1)
        miss2 <- setdiff(All, cols2)
        site[, c(as.character(miss1))] <- NA
        site.tmp[, c(as.character(miss2))] <- NA
        site <- rbind(site, site.tmp)
      }
    }
  for (i in names(site)) if (is.character(site[, i])) 
    site[, i] <- iconv(site[, i], getOption("tv.iconv"), 
                       "")
  if (!inherits(site$DATE, "Date")) {
    if (any(is.na(site$DATE))) 
      message(sum(is.na(site$DATE)), " releves without date. Not converted from factor to date format.")
    else {
      site$DATE <- gsub("/", "", site$DATE)
      index <- nchar(as.character(site$DATE)) == 4
      fun <- function(x) paste(x, "0101", sep = "")
      if (any(index)) 
        site$DATE[index] <- fun(site$DATE[index])
      index <- nchar(as.character(site$DATE)) == 6
      fun <- function(x) paste(x, "01", sep = "")
      site$DATE[index] <- fun(site$DATE[index])
      site$DATE <- as.Date(site$DATE, "%Y%m%d")
    }
  }
  n <- sum(site$SURF_AREA == 0 | is.na(site$SURF_AREA))
  if (n > 0) 
    message(paste(n, " releves without survey area"))
  site$SURF_AREA[site$SURF_AREA == 0] <- NA
  fun <- function(x) all(is.na(x))
  na <- apply(site, 2, fun)
  if (drop & any(na)) {
    if (verbose) {
      message("Some columns contain no data and are omitted.")
      print(names(site)[na], quote = FALSE)
    }
    site <- site[, !na]
  }
  fun.2 <- function(x) all(x == 0 | is.na(x))
  leer <- apply(site, 2, fun.2)
  if (drop & any(leer)) {
    if (verbose) {
      message("Some numeric columns contain only 0 values and are omitted.")
      print(names(site)[leer], quote = FALSE)
    }
    site <- site[, !leer]
  }
  fun.3 <- function(x) is.numeric(x) & any(x == 0, na.rm = TRUE)
  null <- logical()
  for (i in 1:length(site)) null[i] <- fun.3(site[, i])
  if (any(null) && getOption("warn") >= 0) {
    if (verbose) {
      message("Some numeric fields contain 0 values:")
      print(names(site)[null], quote = FALSE)
      message("Please check if these are really meant as 0 or if they are erroneously assigned because of DBase restrictions.")
      message(paste("If so, use something like:"))
      message("site$Column_name[site$Column_name==0] <- NA", 
              "\n")
    }
  }
  site <- site[order(site$PlotObservationID), ]
  if (file.access(file.path(tv_home, "Data", db[1], "tvwin.dbf")) == 
      0) 
    attr(site, "taxreflist") <- read.dbf(file.path(tv_home, 
                                                   "Data", db, "tvwin.dbf"), as.is = TRUE)$FLORA
  return(site)
}


# -------------------------------------------------------------------------------------------------------

file <- "/home/sapi/80gb/Gnomon/Dane/CAUCALIS_DAUCOIDES_BAZA.GNM"
con <- file(file, "rb")

readBin(con, "character", 256*2-1)

