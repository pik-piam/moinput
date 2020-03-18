#' @title readLPJmL inputs
#' @description Read LPJmL inputs into MAgPIE objects
#' @param subtype Switch between different inputs
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Marcos Alves, Kristine Karstens
#' @seealso
#' \code{\link{readLPJmL}}
#' @examples
#'
#' \dontrun{
#' readSource("LPJmL", subtype="LPJmL_inputs:rcp85:HadGEM2.temperature", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom lpjclass read.LPJ_input
#' @importFrom lucode path
 
readLPJmL_inputs <-
  function(subtype = "LPJmL_inputs:rcp85:HadGEM2.temperature") {
    if (grepl("\\.", subtype)) {
      subtype     <- strsplit(gsub(":", "/" , subtype), split = "\\.")
      folder      <- unlist(subtype)[1]
      subtype     <- unlist(subtype)[2]
      
    } else {
      natveg <-
        c(
          "temperature",
          "precipitation",
          "longwave_radiation",
          "shortwave_radiation",
          "soil",
          "wetdays",
          "co2"
        )
      
      if (subtype %in% natveg) {
        folder <- "LPJmL_inputs"
        
      } else{
        folder <- "LPJmL_inputs" # ask the the practical reason for this.
      }
      
      cat(paste0("Set input folder to default climate data set: ", folder))
    }
    
    
    
    if (exists(path(folder))) {
      files_list <- list.files(path(folder))
      files <-
        c(
          temperature           = files_list[grep("tas", files_list)],
          precipitation         = files_list[grep("pr", files_list)],
          longwave_radiation    = files_list[grep("lwnet", files_list)],
          shortwave_radiation   = files_list[grep("rsds", files_list)],
          soil                  = files_list[grep("soil", files_list)],
          wetdays               = files_list[grep("wet", files_list)],
          co2                   = files_list[grep("CO2", files_list)],
          grid                  = files_list[grep("grid", files_list)]
        )
    } else {
      stop(paste("Path", path(folder), "does not exist!"))
    }
    
    file_name <- toolSubtypeSelect(subtype, files)
    
    if (tmp <- file.exists(path(folder, "tmp.out"))) {
      tmp        <- readLines(path(folder, "tmp.out"))
      years      <- as.numeric(unlist(regmatches(tmp, gregexpr("\\d{4}", tmp))))
      start_year <- years[1]
      years      <- seq(years[1], years[2], 1)
      
    } else {
      # default
      start_year  <- 1901
      years      <- seq(start_year, 2017, 1)
    }
    
    if (subtype %in% c("temperature", "precipitation", "wetdays")) {
      x <- read.LPJ_input(
        file_name = path(folder, file_name),
        out_years = paste0("y", years),
        namesum = TRUE
      )
      
      x <- collapseNames(as.magpie(x))
      x <- x / 12
      getNames(x) <- subtype
      
    } else if (subtype %in% c("longwave_radiation", "shortwave_radiation")) {
      x <- read.LPJ_input(
        file_name = path(folder, file_name),
        out_years = paste0("y", years),
        namesum = TRUE
      )
      
      x <- collapseNames(as.magpie(x))
      x <- x / 365
      getNames(x) <- subtype
      
    } else if (subtype %in% c("co2")) {
      
      x  <- read.table(path(folder, file_name))
      id <- match(years, x[, 1])
      x  <- x[id,]
      x  <- collapseNames(as.magpie(x))
      getNames(x) <- subtype
      
    } else if (subtype %in% c("soil")) {
      
      grid = readRDS(path(folder, files["grid"]))
      cell_mapping <- toolGetMapping(
        name = "CountrytoCellMApping.csv", type = "cell")
      
      sk <- file(path(folder, file_name), "rb")
      y  <- readBin(sk, integer(), n = 67420, size = 1)
      close(sk)
      
      x  <-
        array(NA,
              dim = c(67420, length(years)),
              dimnames = list(1:67420, paste0("y", years)))
      for (i in paste0("y", years)) {
        x[, i] <- y
      }
      
      x <- cbind(grid, x)
      x <- merge(cell_mapping, x, by = c("lon", "lat"))
      x <- as.magpie(x[, -c(1, 2, 3, 5, 6)])
      x <- collapseNames(as.magpie(x))
      getNames(x) <- subtype
      
    } else {
      stop(paste0("subtype ", subtype, " is not existing"))
    }
    
    return(x)
    
  }
