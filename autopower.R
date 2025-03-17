#' Writing a Function for Automobile Power Generation
#' @param V is vehicle speed (assuming no headwind) in m/s
#' @param m is vehicle mass in kg
#' @param A is the surfance of the car in m^2
#' @param g is acceleration due to gravity (9.8 m/s^2)
#' @param p_air is the density of air
#' @param Pb is power in watts
#' @param crolling is the coefficient of rolling resistance
#' @param cdrag is the coefficient of drag
#' @return Pb



# Updated auto_power function with speed conversion
auto_power <- function(V, m, A, g = 9.8, p_air = 1.2, crolling = 0.015, cdrag = 0.3, kmh_to_ms = TRUE) {
  # If speed is in km/h, convert it to m/s
  if (kmh_to_ms) {
    V <- V * 1000 / 3600  # Convert speed from km/h to m/s
  }
  
  # Ensure that input values for mass and area are valid
  V <- ifelse((V < 0), return("Speed cannot be negative"), V)
  m <- ifelse((m < 0), NA, m)
  A <- ifelse((A < 0), NA, A)
  
  # Calculate power in watts (Pb)
  Pb <- (0.5 * p_air * A * cdrag * V^3) + (crolling * m * g * V)
  return(Pb)
}


