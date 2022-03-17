#' Add a custom station as node
#' 
#' Adds a custom station as a node and connects all stations within
#' defined reach as transfers
#'
#' @param from  (lon, lat) coordinates of station to create
#' @param gtfs 	A set of gtfs_cp data returned from extract_gtfs_cp or, 
#' for more efficient queries, pre-processed with gtfs_cp_timetable.
#' @param maxDistance Connect all stations within this distance 
#' (meters) to the new created node
#' @return A set of gtfs_cp data
#' @export 
#'
addCustomTransfers <- function(from,
                             gtfs,
                             maxDistance = 500,
                             customStopName = "CFL:0:00") {
  gtfs_cp <- data.table::copy (gtfs)
  
  if (!"timetable" %in% names (gtfs_cp)) {
    gtfs_cp <- make_timetable (gtfs_cp)
  }
  
  # how far should we search for stations
  # to connect in delta lon,lat?
  d <- from
  d[1] <- d[1] + ifelse(d[1]>0, 1, -1) # equator
  suppressMessages(
    d <- geodist::geodist(from,
                          d)[1,1]
  )
  d <- 1/d*maxDistance
  
  # Find stations within distance
  subset(gtfs_cp$stops,
           gtfs_cp$stops$stop_lon >= from[1] - d &
           gtfs_cp$stops$stop_lon <= from[1] + d &
           gtfs_cp$stops$stop_lat >= from[2] - d &
           gtfs_cp$stops$stop_lat <= from[2] + d) -> possibleStops
  
  if(nrow(possibleStops) == 0) {
    stop("No stations found near from",
         "Try to increase maxDistance")
  }
  suppressMessages(
    d <- geodist::geodist(from,
                          possibleStops[,c("stop_lat", "stop_lon")])
  )
  d <- as.numeric(d)
  possibleStops$d <- d
  subset(possibleStops,
         possibleStops$d <= maxDistance) -> possibleStops
  #possibleStops$min_transfer_time <- ceiling(possibleStops$d/1.111111)
  possibleStops$min_transfer_time <- 0
  # Add from as stop
  possibleStops <- possibleStops[,c("stop_id",
                                    "min_transfer_time")]
  
  possibleStops$from_stop_id <- nrow(gtfs_cp$stop_ids)+1
  possibleStops$transfer_type <- 2
  possibleStops$to_stop_id <- sapply(possibleStops$stop_id,
                                     function(x) which(x == gtfs_cp$stop_ids))
  possibleStops <- possibleStops[,-1]
  
  gtfs_cp$stop_ids <- rbind(gtfs_cp$stop_ids,
                            data.frame(stop_ids = customStopName))
  
  gtfs_cp$transfers <- rbind(gtfs_cp$transfers,
                          possibleStops)
  
  gtfs_cp$stops <- rbind(gtfs_cp$stops,
                      data.frame(stop_id = customStopName,
                                 stop_name = customStopName,
                                 stop_lon = from[1],
                                 stop_lat = from[2]),
                      fill = T)
  return(gtfs_cp)
}

addCustomTimetable <- function(from,
                               gtfs,
                               maxDistance = 500,
                               start_time_limits = c(16 * 60 * 60, 
                                                     17 * 60 * 60),
                               departureDelta = 60,
                               customStopName = "CFL:0:00") {
  gtfs_cp <- data.table::copy (gtfs)
  
  if (!"timetable" %in% names (gtfs_cp)) {
    gtfs_cp <- make_timetable (gtfs_cp)
  }
  
  # how far should we search for stations
  # to connect in delta lon,lat?
  d <- from
  d[1] <- d[1] + ifelse(d[1]>0, 1, -1) # equator
  suppressMessages(
    d <- geodist::geodist(from,
                          d)[1,1]
  )
  d <- 1/d*maxDistance
  
  # Find stations within distance
  subset(gtfs_cp$stops,
         gtfs_cp$stops$stop_lon >= from[1] - d &
           gtfs_cp$stops$stop_lon <= from[1] + d &
           gtfs_cp$stops$stop_lat >= from[2] - d &
           gtfs_cp$stops$stop_lat <= from[2] + d) -> possibleStops
  
  if(nrow(possibleStops) == 0) {
    stop("No stations found near from",
         "Try to increase maxDistance")
  }
  suppressMessages(
    d <- geodist::geodist(from,
                          possibleStops[,c("stop_lat", "stop_lon")])
  )
  d <- as.numeric(d)
  possibleStops$d <- d
  subset(possibleStops,
         possibleStops$d <= maxDistance) -> possibleStops
  possibleStops$min_transfer_time <- ceiling(possibleStops$d/1.111111)

  # Expand departures
  departures <- data.table::data.table(departure_time = seq.int(start_time_limits[1],
                                                                start_time_limits[2], 
                                                                by = departureDelta),
                                       joinColumn = 0)
  
  possibleStops$joinColumn <- 0
  possibleStops <- possibleStops[departures, nomatch=0, on="joinColumn", allow.cartesian=TRUE]
  possibleStops$joinColumn <- NULL
  

  # Add from as stop
  possibleStops$arrival_time <- possibleStops$departure_time + possibleStops$min_transfer_time
  possibleStops <- possibleStops[possibleStops$arrival_time <= start_time_limits[2],]
  
  possibleStops <- possibleStops[,c("stop_id",
                                    "departure_time",
                                    "arrival_time")]
  
  possibleStops$trip_id <- 1
  possibleStops$departure_station <- nrow(gtfs_cp$stop_ids)+1
  
  possibleStops$arrival_station <- sapply(possibleStops$stop_id,
                                     function(x) which(x == gtfs_cp$stop_ids))
  possibleStops <- possibleStops[,-1]
  
  gtfs_cp$stop_ids <- rbind(gtfs_cp$stop_ids,
                            data.frame(stop_ids = customStopName))

  gtfs_cp$timetable <- rbind(gtfs_cp$timetable,
                             possibleStops)
  
  gtfs_cp$stops <- rbind(gtfs_cp$stops,
                         data.frame(stop_id = customStopName,
                                    stop_name = customStopName,
                                    stop_lon = from[1],
                                    stop_lat = from[2]),
                         fill = T)
  return(gtfs_cp)
}

