#'
#' This is an example of the get_rice_data function
#' 


get_rice_data <- function() { 

  library( tidyverse )
  library( lubridate )
  
  url <- "https://docs.google.com/spreadsheets/d/1Mk1YGH9LqjF7drJE-td1G_JkdADOU0eMlrP01WFBT8s/pub?gid=0&single=true&output=csv"
  
  read_csv( url, col_types = cols() ) |>                                   # The col_types argument suppresses output. 
    mutate( Date = mdy_hms(DateTime) ) |>
    mutate( Day = day( Date ) ) |>                                         # Have day of the week
    mutate( Weekday = wday( Date,
                            label = TRUE,
                            abbr = FALSE ) ) |>                            # Make weekdays as factor labels
    mutate( Month = droplevels( month( Date, 
                                       label = TRUE, 
                                       abbr = FALSE)) )  |>                # Make months as existing factor labels
    mutate( Air_Temperature = (AirTempF-32) * 5/9, 
            Wind_Speed = WindSpeed_mph * 1.6,
            Rain = Rain_in * 25.4 ) |>                                     # Make everything in SI
    select( -SurfaceWaterElev_m_levelNad83m, -Depth_ft, -PH_mv,
            -AirTempF, -WindSpeed_mph, -Rain_in,
            -SpCond_mScm, -BGAPC_CML, -BGAPC_rfu, -ODO_mgl,
            -DateTime, -RecordID) |>   # Dropping columns
    rename( Water_Depth = Depth_m,
            Chlorophyl = Chla_ugl,
            Barametric_Pressure = BP_HG,
            Water_Temperature = H2O_TempC,
            Salinity = Salinity_ppt,
            pH = PH, ODO = ODO_sat,
            Turbidity = Turbidity_ntu) |>                                  # Renaming Existing Columns & reorder
    select( Date, Month, Day, Weekday, 
            Air_Temperature, Rain, Wind_Speed, Wind_Direction = WindDir, 
            Humidity=RelHumidity, Barametric_Pressure, PAR,
            Water_Temperature, Water_Depth, everything()) -> rice_data
  
  return( rice_data )                                                      # Return the data
}


