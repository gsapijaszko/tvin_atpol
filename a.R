get_all_data <- function(st = "") {
#  print(st)
  a <- list()
  
  dl_pos <- stringr::str_locate(st, "Dl:")
  dl <- substr(st, dl_pos[[1]], nchar(st))
  sz_pos <- stringr::str_locate(dl, "Sz:")
  sz <- trimws(substr(dl, sz_pos[[1]]+3, nchar(dl)))
  dl <- trimws(substr(dl, 4, sz_pos[[1]]-1))
  
  dl <- strsplit(dl, split = "(\\s)+")[[1]] |>
    as.numeric()
  
  sz <- strsplit(sz, split = "(\\s)+")[[1]] |>
    as.numeric()
  
  a$lon <- dl[[1]] + dl[[2]]/60 + dl[[3]]/60^2
  a$lat <- sz[[1]] + sz[[2]]/60 + sz[[3]]/60^2
  
  sea <- c("ś", "ć", "ć", "Ĺ", "ĺ", "Ą", "ó", "×", "Ž", "ž", "Ć", "ť", "Ź", "í")
  rep <- c("Ś", "ą", "ć", "ę", "ł", "ń", "ó", "ś", "ź", "ż", "Ć", "Ł", "Ź", "Ż")
  names(rep) <- sea
  
  atpol_pos <- stringr::str_locate(st, "[A-Z]{2}[0-9]{2,}")
  
  record_type_no <- trimws(substr(st, 1, atpol_pos[[1]]-1))
  record_type_no <- strsplit(record_type_no, "(\\s)+")
  
  if(length(record_type_no[[1]]) == 2L) {
    a$record_type <- record_type_no[[1]][1]
    a$record_number <- as.numeric(record_type_no[[1]][2])  
  } else if(length(record_type_no[[1]]) == 1L) {
    if(grepl("^[A-Z][0-9]+", record_type_no[[1]][1]) &&  grepl("[0-9]$", record_type_no[[1]][1])) {
      a$record_type <- substr(record_type_no[[1]][1], 1, 1)
      a$record_number <- substr(record_type_no[[1]][1], 2, nchar(record_type_no[[1]][1])) |>
        as.numeric()
    }
  } else {
    a$record_type <- NA
    a$record_number <- NA
  }

  a$atpol_square <- substr(st, atpol_pos[[1]], atpol_pos[[2]])
  
  st <- trimws(substr(st, atpol_pos[[2]]+1, dl_pos[[1]]-1))

  desc_pos <- stringr::str_locate(st, pattern = "(\\d)+(\\s){2}[A-Z]")

  a$description <- trimws(substr(st, 1, desc_pos[[1]]-1)) |>
    stringr::str_replace_all(pattern = "(\\s){1,}", replacement = " ") |>
    stringr::str_replace_all(pattern = rep) |>
    stringr::str_replace(pattern = "(\\,)$", replacement = "")
  
  year_pos <- stringr::str_locate(st, "(\\d){1,}$")
  year <- substr(st, year_pos[[1]], year_pos[[2]])
  if(year == "0") {
    a$date <- as.POSIXct(NA, "%Y%m%d", origin = "1970-01-01 00:00:00 UTC")
  } else if(nchar(year) == 4L) {
    a$date <- as.POSIXct(paste0(year, "-01-02 00:00:00 UTC"), origin = "1970-01-01 00:00:00 UTC")
  }
  
  st <- trimws(substr(st, desc_pos[[1]], year_pos[[1]]-1))
  number_pos <- stringr::str_locate(st, "^(\\d){1,}")

  a$number_of_entries <- substr(st, number_pos[[1]], number_pos[[2]]) |>
    as.numeric()
  a$author_name <- trimws(substr(st, number_pos[[2]]+1, nchar(st))) |>
    stringr::str_replace_all(pattern = "(\\s){1,}", replacement = " ") |>
    stringr::str_replace_all(pattern = rep)

  return(a)
}

f <- "/home/sapi/80gb/Atpol/Wynikowe/0224.LOC"

header <- read.delim(file = f,
                     skip = 0,
                     nrows = 4,
                     blank.lines.skip = FALSE,
                     header = FALSE)
if(ncol(header) != 1L) {
  warning("More than one column in header!")
}

atpol_species_no <- sub(pattern = "Raport rekordow gatunku: ", replacement = "", header[1, 1]) |>
  as.numeric()
species_name <- header[2, 1] |>
  as.character()
no_of_unique_atpol_squares <- sub(pattern = "Liczba kwadratow: ", replacement = "", header[4,1]) |>
  as.numeric()

t <- read.delim(file = f,
                skip = 4,
                fileEncoding = "CP852",
                #           encoding = "UTF8",
                blank.lines.skip = FALSE,
                header = FALSE)

# t

b <- tibble::tibble(
  lon = numeric(),
  lat = numeric(),
  record_type = character(),
  record_number = numeric(),
  atpol_square = character(),
  description = character(),
  number_of_entries = numeric(),
  author_name = character(),
  date = as.POSIXct(NA, "%Y%m%d", origin = "1970-01-01 00:00:00 UTC")
)

if(no_of_unique_atpol_squares > 0L) {
  # for (i in rev(seq_len(nrow(t)))) {
  #   b <- get_all_data(st = t[i, ]) |>
  #     rbind(b)
  # }
  longlist <- lapply(seq_len(nrow(t)), function(i)  get_all_data(st = t[i, ]))
  b <- do.call(rbind, longlist)
  b <- b |> 
    tibble::as_tibble() |>
    dplyr::rowwise() |>
    dplyr::mutate(date = as.POSIXct(date, origin = "1970-01-01 00:00:00 UTC")) |>
    tidyr::unnest(cols = c(lon, lat, record_type, record_number, atpol_square, description, number_of_entries, author_name))
  
  b$species <- species_name
}

b |>
  dplyr::mutate(date = lubridate::year(date)) |>
#  dplyr::arrange(date) |>
  write.csv(file = gstools::format_file_name(species_name, extension = "csv"),
            row.names = FALSE)
  
Śś

t[21:30,]
# -----------------------------------------------------------------------------------------------------------------


library(vegdata)

vegdata::tv.home()

vegdata::tv.refl("Poland")

vegdata::tax('Quercus robur')

vegdata::tv.db()

db <- "AGNEST1"
#nagłówki zdjęć 
sites <- tv.site(db)

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



