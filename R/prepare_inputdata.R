#' Prepare input data for further steps.
#'
#' @param txtIO_pth Path for the txtInOut folder of the SWAT2012 project
#' @param mgt_pth Path for the folder holding the user management schedule and
#'   CNOP .csv files
#' @param ant_days Antecendent days considered for the development of the
#'   API (Antecedent Precipitation Index)
#'
#' @return Returns a list structure holding following components:
#' \itemize{
#'   \item \code{$MGT_CNOP}: A list of the user management schedules and
#'      respective CNOP tables
#'   \item \code{$Lookup}: A lookup table holding codes for management operations,
#'      fertilizers, tillage operations, crop types from SWAT, Linking table for
#'      weather stations to Subbasins, and simulation period from file.cio.
#'   \item \code{$Precipitation}: Precipitation data from the SWAT project
#'   \item\code{$Antecedent_Precip}: Calculated antecedent precipitation, derived
#'      from precipitation and temperature data of the SWAT project
#'   \item \code{$Temperature}: Normalized temperature index calculated from
#'      tempearture data of the SWAT project and statistics derived from them
#'  }
#'
#' @import dplyr
#' @importFrom data.table fread
#' @importFrom magrittr '%<>%'
#'
#'
#' @export


prepare_inputdata <- function(txtIO_pth, mgt_pth, ant_days = 5){

  # Initiate progress bar for writing MGT files ----------------------------
  print("Read and prepare input data:")
  prgr_bar <- txtProgressBar(min = 0, max = 100, initial = 0, style = 3)

  # Read weather inputs ----------------------------------------------------
  # Read the min/max tempeartures for the stations:
  if(file.exists(txtIO_pth%//%"Tmp1.Tmp")){
    file.rename(txtIO_pth%//%"Tmp1.Tmp",
                txtIO_pth%//%"tmp1.tmp") #Solves bug in Linux SWAT execution
  }
  temp_data <- read_weather(txtIO_pth%//%"tmp1.tmp", "XXX.X")
  setTxtProgressBar(prgr_bar, 20)

  # Read the precipitation data for the stations:
  precip_data <- read_weather(txtIO_pth%//%"pcp1.pcp", "XXX.XX")
  setTxtProgressBar(prgr_bar, 40)

  # Read management schedule file and all lookup tables --------------------
  # Management schedule files and lookup tables for curve numbers (CN) according
  # to operation and soil type
  mgt_cnop <- read_mgtcnop(mgt_pth)
  rm(mgt_pth)
  setTxtProgressBar(prgr_bar, 50)

  # List of lookup tables holding management, fertilizer, tillage, and crop types.
  # The subbasin files contain the information which weather stations SWAT
  # allocates to which subbasins. The output is a lookup table of station
  # allocations. This is also added to the lookup list.
  lookup <- read_lookuptables(txtIO_pth)
  setTxtProgressBar(prgr_bar, 60)

  # Edit weather inputs ----------------------------------------------------
  # Edit precipitation data
  # Assign the station precipitation data to the respective subbasins
  # and aggregate the precipitation data to daily accumulated values
  precip_data %<>% modify_weather(., lookup, "PCP") %>%
    limit_timespan(., lookup)
  setTxtProgressBar(prgr_bar, 65)

  # Edit temperature data
  # Assign the station temperature data to the respective subbasins
  temp_min <- temp_data[,c(1,seq(3, dim(temp_data)[2], 2))]
  temp_max <- temp_data[,c(1,seq(2, dim(temp_data)[2], 2))]
  temp_mean <- cbind(temp_data[,1],
                     (temp_data[,seq(2, dim(temp_data)[2], 2)] +
                        temp_data[,seq(3, dim(temp_data)[2], 2)])/2)
  rm(temp_data)
  setTxtProgressBar(prgr_bar, 70)

  temp_min %<>%  modify_weather(., lookup, "TMP") %>%
    limit_timespan(., lookup)
  temp_max %<>%  modify_weather(., lookup, "TMP") %>%
    limit_timespan(., lookup)
  temp_mean %<>% modify_weather(., lookup, "TMP")
  setTxtProgressBar(prgr_bar, 80)

  # Calculate normalized deviations to the monthly daily mean tempeartures
  # for each subbasin.
  temp_index <- compute_TIndex(temp_mean, lookup)
  temp_stat <- temp_index[["stat"]]
  temp_index <- temp_index[["index"]]
  temp_index %<>% limit_timespan(., lookup)
  rm(temp_mean)
  setTxtProgressBar(prgr_bar, 90)

  # Calculate antecedent water content
  api_data <- (select(precip_data, starts_with("SUB")) -
                compute_ET0Hargreaves(temp_min, temp_max, lookup)) %>%
    apply(., 2, compute_AMC, c(0.8,0.85,0.90, 0.95), ant_days) %>%
    lapply(., cbind, precip_data[,1:4]) %>%
    lapply(., select,c(YEAR,MON,DAY,JDN,A,B,C,D))

  rm(temp_min, temp_max)

  input_list <- list(mgt_cnop = mgt_cnop,
                     lookup = lookup,
                     precipitation = precip_data,
                     antecedent_precip = api_data,
                     temperature_index = temp_index,
                     temperature_stat  = temp_stat)

  # set and close progress bar
  setTxtProgressBar(prgr_bar, 100)
  close(prgr_bar)

  return(input_list)
}
