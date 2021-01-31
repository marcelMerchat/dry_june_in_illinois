## Three-Body Solar System Problem: Sun, Jupiter, Earth
## Author: Marcel Merchat
## Date: January 5, 2021

#############################################################################

## Generate Simulated Data for Solar System Orbits

## Class Definitions

## (1) PointLocationVector Line-71
## (2) VelocityVector      Line-282
## (3) ForceVector         Line-463
## (4) Planet              Line-583
## (5) SolarSystem         Line-784
## (6) CenterOfMass        Line-1004
## (7) OrbitData           Line-1052
## (8) OrbitAnalysis       Line-1476
## (9) OrbitController     Line-1937

## Generate Simulated Data for Solar System Orbits               Line-2308
## The orbit data is saved as a csv file.

## Function-1: run_orbit_analyzer                                Line-2354

## Examples of running simulation:                               Line-2411
## Duplicate Final Simulation:                                   Line-2465

#############################################################################

## Data Processing                                               Line-2479

## Process raw simulated orbit data                              
## to make irradiation year records

## Class Definitions

## (10) DailyOrbitSummary                                        Line-2483
## (11) YearOrbitSummary                                         Line-2604

#############################################################################

## Combine Solar Irradiation with Weather and Lake-Level Data    Line-2697   
## This makes merged meteorological csv data file.

## Class Definition

## (12) DataProcessor                                            Line-2740

## Merge year records for weather, lake-levels, and solar data   Line-2852

##  Example of script command to declare 'DataProcessor' object  Line-2856
##  to process simulated raw solar data and merge with weather
##  and lake-level data. The final merged meteorological file
##  is generated.  

############################################################################
############################################################################

## Code Begins Here

#############################################################################
#############################################################################

library(R6)

## Class Definitions

##############################################################################

## Class 1: PointLocationVector

## Fairly unique to R, the arguments are searched to determine
## if rectangular (x , y, z) or polar (mag, theta, phi) coordinates
## were passed. In any case, the class member variables always reflect the
## the latest changes--both the rectangular and the equivalent polar
## variable set are always updated with any change and can be retrieved
## from the object with get_x(), get_y(), get_z(), get_mag(), get_angle(),
## get_phi().
## The original method only handled two dimensions of x and y.
## The z and phi dimensions were added to handle three dimensions.

## Perhaps get_angle should be renamed get_theta. An improved function
## would also handle errors such as passing both kinds
## of coordinates or incomplete coordinate sets.

PointLocationVector <- R6Class("PointLocationVector", 
    list(
       x = NA,
       y = NA,
       z = NA,
       mag = NA,
       theta = NA,
       phi = NA,
       pie = 3.1415926535897932385,
       two_pie = 2 * 3.1415926535897932385,
       ## Option-1: Enter rectangular coordinates x, y, z
       ## Option-2: Enter polar coordinates mag, theta, phi
       initialize = function(...,degrees=TRUE) {
         arg_list <- c(as.list(environment()), list(...))
         name_list <- names(arg_list)
         x_found <- sum(grepl("x",name_list))

         if(x_found > 0 ){
           x_param <- grep("x",name_list)
           x <- arg_list[[x_param]]
         }
         y_found <- sum(grepl("y",name_list))
         if(y_found > 0 ){
           y_param <- grep("y",name_list)
           y <- arg_list[[y_param]]
         }
         z_found <- sum(grepl("z",name_list))
         if(z_found > 0 ){
           z_param <- grep("z",name_list)
           z <- arg_list[[z_param]]
         }
         
         mag_found <- sum(grepl("mag",name_list))
         if(mag_found > 0 ){
           mag_param <- grep("mag",name_list)
           mag <- arg_list[[mag_param]]
         }
         theta_found <- sum(grepl("theta",name_list))
         if(theta_found > 0 ){
           theta_param <- grep("theta",name_list)
           theta <- arg_list[[theta_param]]
         }
         phi_found <- sum(grepl("phi",name_list))
         if(phi_found > 0 ){
           phi_param <- grep("phi",name_list)
           phi <- arg_list[[phi_param]]
         }
         if(x_found > 0 && y_found > 0){
            mag <- ((x^2)+(y^2))^0.5
            theta <- self$arctrig360(x,y,degrees)
            self$x <- x
            self$y <- y
            self$z <- z 
            self$mag <- ((x^2)+(y^2)+(z^2))^0.5
            self$theta <- self$arctrig360(x=x,y=y)
            if( (x + y) != 0) {
              self$phi <- 360 * asin(z / (  ((x^2)+(y^2) + z^2  + 0.0001 )^0.5  )) / self$two_pie 
            } else {
              self$phi <- 90 
            }
         } else if (mag_found > 0 && theta_found > 0) {
            x <- mag * cos(self$two_pie*theta/360) * cos(self$two_pie*phi/360)
            y <- mag * sin(self$two_pie*theta/360) * cos(self$two_pie*phi/360)
            z <- mag * sin(self$two_pie*phi/360)
            self$x <- x
            self$y <- y
            self$z <- z
            self$mag <- mag
            self$theta <- theta
            self$phi <- phi
         }
       },
       set_x = function(x) {
           self$x <- x
           if( (x + self$y) != 0) {
             self$phi <- 360 * asin(self$z / (  ((x^2)+(self$y^2)  + (self$z)^2 + 0.0001)^0.5  ) ) /self$two_pie
           } else {
             self$phi <- 90 
           }
           self$mag <- ((self$x^2)+(self$y^2)+(self$z^2))^0.5
           self$theta <- self$arctrig360(x=x,y=self$y)
           },
       set_y = function(y) {
           self$y <- y
           self$mag <- ((self$x^2)+(self$y^2)+(self$z^2))^0.5
           self$theta <- self$arctrig360(x=self$x,y=self$y)
           if( (self$x + y) != 0) {
             self$phi <- 360 * asin(self$z / (  ((self$x^2)+(y^2)  + (self$z)^2 +0.0001)^0.5  ) ) / self$two_pie
           } else {
             self$phi <- 90
           }
       },
       set_z = function(z) {
           self$z <- z
           self$mag <- ((self$x^2)+(self$y^2)+(z^2))^0.5
           self$theta <- self$arctrig360(x=self$x,y=self$y)
           if( (self$x + self$y) != 0) {
             self$phi <- 360 * asin(z / (  ((self$x^2)+(self$y^2) + z^2  + 0.0001)^0.5  ) ) / self$two_pie
           } else {
             self$phi <- 90
           }
       },
       get_x = function() {
           self$x
       },
       get_y = function() {
           self$y
       },
       get_z = function() {
           self$z
       },
       get_mag = function() {
           self$mag
       },
       get_angle = function() {
           self$theta
       },
       get_phi = function() {
           self$phi
       },
       distance = function(pt1, pt2){
         delta_x <- pt1$get_x() - pt2$get_x()
         delta_y <- pt1$get_y() - pt2$get_y()
         delta_z <- pt1$get_z() - pt2$get_z()
         (delta_x^2 + delta_y^2 + delta_z^2)^0.5
       },
       diff = function(pt1, pt2){
         delta_x <- pt1$get_x() - pt2$get_x()
         delta_y <- pt1$get_y() - pt2$get_y()
         delta_z <- pt1$get_z() - pt2$get_z()
         PointLocationVector$new(x=delta_x,y=delta_y,z=delta_z)
       },
       add = function(pt1, pt2){
         delta_x <- pt1$get_x() + pt2$get_x()
         delta_y <- pt1$get_y() + pt2$get_y()
         delta_z <- pt1$get_z() + pt2$get_z()
         PointLocationVector$new(x=delta_x,y=delta_y,z=delta_z)
       },
       sgn = function(x){
         x >= 0
       },
       arctrig360 = function(x,y, degrees = TRUE){
         sx <- self$sgn(x)
         sy <- self$sgn(y)
         if(x != 0 ){
           theta <- atan(y/x) +
             self$pie * ((!sx && sy) || (!sx && !sy)) +
             self$two_pie * (sx && !sy)
           if(degrees == TRUE){
             theta <- 360 * theta / self$two_pie
           }
         } else if (x == 0 && y > 0){
           theta <- self$pie/2
           if(degrees == TRUE){
             theta <- 90
           }
         } else if (x == 0 && y < 0){
           theta <- 3 * self$pie / 2
           if(degrees == TRUE){
             theta <- 270
           }
         } else {
           theta <- 0
         }
         theta
       },
       cos_x = function(x) {
         # Converting degrees to radian 
         #x <- x * (two_pie / 360); 
         x1 <- 1 
         # maps the sum along the series 
         cosx <- x1
         print(x1)
         ## holds the actual value cos[n] 
         ## cosval <- cos(x) 
         for (i in 1:5) {
           print(2*i)
           x1 <- -x1 
           denominator <- factorial(2 * i) 
           print(denominator)
           term <- x1 * x^(2*i) / denominator
           print(term)
           cosx <- cosx + term 
           print(cosx)
         }
         cosx
       }
    )
)

# myPoint1 <- PointLocationVector$new(x=1,y=1,z=1)
# myPoint2 <- PointLocationVector$new(mag=3^0.5,theta=45,phi=35.26476,degrees=TRUE)
# myPoint1$distance(pt1=myPoint1,pt2=myPoint2)
# [1] 5

## Class 2: VelocityVector

## similarly to the PointLocationVector class, both rectangular and polar
## coordinates are handled.

VelocityVector <- R6Class("VelocityVector", 
   list(
     vx = NA,
     vy = NA,
     vz = NA,
     mag = NA,
     theta = NA,
     phi = NA,
     pie = 3.1415926535897932385,
     two_pie = 2 * 3.1415926535897932385,
     ## Option-1: Enter rectangular velocity components vx, vy, vz
     ## Option-2: Enter polar coordinates mag, theta, phi
     initialize = function(...,degrees=TRUE) {
       arg_list <- c(as.list(environment()), list(...))
       name_list <- names(arg_list)
       
       x_found <- sum(grepl("vx",name_list))
       if(x_found > 0 ){
         x_param <- grep("vx",name_list)
         vx <- arg_list[[x_param]]
       }
       y_found <- sum(grepl("vy",name_list))
       if(y_found > 0 ){
         y_param <- grep("vy",name_list)
         vy <- arg_list[[y_param]]
       }
       z_found <- sum(grepl("vz",name_list))
       if(z_found > 0 ){
         z_param <- grep("vz",name_list)
         vz <- arg_list[[z_param]]
       }
       
       mag_found <- sum(grepl("mag",name_list))
       if(mag_found > 0 ){
         mag_param <- grep("mag",name_list)
         mag <- arg_list[[mag_param]]
       }
       theta_found <- sum(grepl("theta",name_list))
       if(theta_found > 0 ){
         theta_param <- grep("theta",name_list)
         theta <- arg_list[[theta_param]]
       }
       phi_found <- sum(grepl("phi",name_list))
       if(phi_found > 0 ){
         phi_param <- grep("phi",name_list)
         phi <- arg_list[[phi_param]]
       }
       if(x_found > 0 && y_found > 0){
          mag <- ((vx^2)+(vy^2))^0.5
          theta <- self$arctrig360(vx,vy,degrees)
          self$vx <- vx
          self$vy <- vy
          self$vz <- vz 
          self$mag <- ((vx^2)+(vy^2)+(self$vz^2))^0.5
          self$theta <- self$arctrig360(x=vx,y=vy)
          if( (vx + vy) != 0) {
             self$phi <- 360 * asin(vz / (  ((vx^2)+(vy^2)  + vz^2 + 0.0001)^0.5  ) ) / self$two_pie 
          }
        } else if (mag_found > 0 && theta_found > 0) {
          vx <- mag * cos(self$two_pie*theta/360) * cos(self$two_pie*phi/360)
          vy <- mag * sin(self$two_pie*theta/360) * cos(self$two_pie*phi/360)
          vz <- mag * sin(self$two_pie*phi/360)
          self$vx <- vx
          self$vy <- vy
          self$vz <- vz
          self$mag <- mag
          self$theta <- theta
          self$phi <- phi
        }
     },
     set_vx = function(vx) {
       self$vx <- vx
       if( (vx + self$vy) != 0) {
         self$phi <- 360 * asin(self$vz / (  ((vx^2)+(self$vy^2) + (self$vz)^2  + 0.0001)^0.5  ) ) / self$two_pie
       } else {
         self$phi <- 90
       }
       self$mag <- ((vx^2)+(self$vy^2)+(self$vz^2))^0.5
       self$theta <- self$arctrig360(x=vx,y=self$vy)
     },
     set_vy = function(vy) {
       self$vy <- vy
       self$mag <- ((self$vx^2)+(vy^2)+(self$vz^2))^0.5
       self$theta <- self$arctrig360(x=self$vx,y=vy)
       if( (self$vx + vy) != 0) {
         self$phi <- self$phi <- 360 * asin(self$vz / (  ((self$vx^2)+(vy^2)  + (self$vz)^2 + 0.0001)^0.5  ) ) /self$two_pie 
       } else {
         self$phi <- 90
       }
     },
     set_vz = function(vz) {
       self$z <- vz
       self$mag <- ((self$vx^2)+(self$vy^2)+(vz^2))^0.5
       self$theta <- self$arctrig360(x=self$vx,y=self$vy)
       if( (self$vx + self$vy) != 0) {
         self$phi <- 360 * asin(vz / (  ((self$vx^2)+(self$vy^2)  + vz^2  + 0.0001)^0.5  ) ) /self$two_pie 
       } else {
         self$phi <- 90
       }
     },
     get_vx = function() {
       self$vx
     },
     get_vy = function() {
       self$vy
     },
     get_vz = function() {
       self$vz
     },
     get_mag = function() {
       self$mag
     },
     get_angle = function() {
       self$theta
     },
     get_phi = function() {
       self$phi
     },
     magnitude = function(pt1, pt2){
       delta_x <- pt1$get_x() - pt2$get_x()
       delta_y <- pt1$get_y() - pt2$get_y()
       delta_z <- pt1$get_z() - pt2$get_z()
       (delta_x^2 + delta_y^2 + delta_z^2)^0.5
     },
     diff = function(pt1, pt2){
       delta_x <- pt1$get_x() - pt2$get_x()
       delta_y <- pt1$get_y() - pt2$get_y()
       delta_z <- pt1$get_z() - pt2$get_z()
       PointLocationVector$new(x=delta_x,y=delta_y,z=delta_z)
     },
     sgn = function(x){
       x >= 0
     },
     arctrig360 = function(x,y, degrees = TRUE){
       sx <- self$sgn(x)
       sy <- self$sgn(y)
       if(x != 0 ){
         theta <- atan(y/x) +
           self$pie * ((!sx && sy) || (!sx && !sy)) +
           self$two_pie * (sx && !sy)
         if(degrees == TRUE){
           theta <- 360 * theta / self$two_pie
         }
       } else if (x == 0 && y > 0){
         theta <- self$pie/2
         if(degrees == TRUE){
           theta <- 90
         }
       } else if (x == 0 && y < 0){
         theta <- 3 * self$pie / 2
         if(degrees == TRUE){
           theta <- 270
         }
       } else {
           theta <- 0
       }
       theta
     },
     cos_x = function(x) {
       # Convert degrees to radians 
       # x <- x * (two_pie / 360); 
       x1 <- 1 
       cosx <- x1
       ## holds the actual value cos[n] 
       for (i in 1:5) {
         x1 <- -x1 
         denominator <- factorial(2 * i) 
         term <- x1 * x^(2*i) / denominator
         # track the current sum along the series 
         cosx <- cosx + term
       }
       cosx
     }
   )
)

## (Class 3) Force Vector

## The tail point represents the center-of-mass of all the planets except
## one planet that is deleted. It is represented by the 'PointLocationVector'
## object that specifies the position of this 'deleted' center-of-mass
## with respect to the coordinate origin.
 
## The head represents the arrowhead end of the vector at the position of
## the planet that was deleted from the center-of-mass calculation. It is 
## represented by the 'PointLocationVector' object that specifies the 
## center-of-mass of the planet and its moons.

## The Force Vector or 'ForceVector' object is obtained by subtracting the tail
## PointLocationVector for the center-of-mass from the head PointLocationVector 
## at the planet. 

## Force = magnitude * (head - tail)

## The magnitude is calculated within the ForceVector class. 
## Head and tail are 'PointLocationVector' objects..
ForceVector <- R6Class("ForceVector",                  
   list(
     mag = NA,
     theta = NA,
     phi = NA,
     fx = NA,
     fy = NA,
     fz = NA,
     pie = 3.1415926535897932385,
     two_pie = 2 * 3.1415926535897932385,
     initialize = function(head, tail,
                           planet_mass , cm_mass, ..., degrees=TRUE) {
       
       yr <- 24*3600 * 365.256363
       arg_list <- c(as.list(environment()), list(...))
       pie <- 3.1415926535897932385
       two_pie <- 2 * pie
       name_list <- names(arg_list)
       
       tail_found <- sum(grepl("tail",name_list))
       if(tail_found > 0 ){
         tail_param <- grep("tail",name_list)
         tail <- arg_list[[tail_param]]
       }
       head_found <- sum(grepl("head",name_list))
       if(head_found > 0 ){
         head_param <- grep("head",name_list)
         head <- arg_list[[head_param]]
       }
       if(tail_found > 0 && head_found > 0){
         diff_vector <- head$diff(head,tail)
         d <- diff_vector$mag
         ## Universal Gravitational Constant 6.6743×10^(-11) m^3 kg second^2
         G <- 6.6743*10^(-11)
         mag <- G * planet_mass * cm_mass / (d^2)
         self$mag <- mag
         theta <- diff_vector$theta
         self$theta <- theta
         phi <- diff_vector$phi
         self$phi <- phi
         self$fx <- mag * cos(self$two_pie*theta/360) * cos(self$two_pie*phi/360)
         self$fy <- mag * sin(self$two_pie*theta/360) * cos(self$two_pie*phi/360)
         self$fz <- mag * sin(self$two_pie*phi/360)
       }
     },
     set_mag = function(mag) {
       self$mag <- mag
     },
     set_theta = function(theta) {
       self$theta <- theta
     },
     set_phi = function(phi) {
       self$phi <- phi
     },
     get_mag = function() {
       self$mag
     },
     get_theta = function() {
       self$theta
     },
     get_phi = function() {
       self$phi
     },
     distance = function(tail, head){
       delta_x <- head$get_x() - tail$get_x()
       delta_y <- head$get_y() - tail$get_y()
       delta_z <- head$get_z() - tail$get_z()
       (delta_x^2 + delta_y^2  + delta_z^2)^0.5
     },
     sgn = function(x){
       x >= 0
     },
     arctrig360 = function(x,y, degrees = TRUE){
       sx <- self$sgn(x)
       sy <- self$sgn(y)
       if(x != 0 ){
         theta <- atan(y/x) +
           self$pie * ((!sx && sy) || (!sx && !sy)) +
           self$two_pie * (sx && !sy)
         if(degrees == TRUE){
           theta <- 360 * theta / self$two_pie
         }
       } else if (x == 0 && y > 0){
         theta <- self$pie/2
         if(degrees == TRUE){
           theta <- 90
         }
       } else if (x == 0 && y < 0){
         theta <- 3 * self$pie / 2
         if(degrees == TRUE){
           theta <- 270
         }
       } else {
         theta <- 0
       }
       theta
     }
   )
)

## Class 4: Planet

Planet <- R6Class("Planet", 
    list(
      name = NA,
      position = NA,
      x = NA,
      y = NA, 
      z = NA, 
      distance = NA,
      theta = NA, 
      phi = 90, 
      px = NA,
      py = NA,
      pz = NA,
      v0 = NA,
      aphelion = NA,
      heading0 = NA,
      heading_inclination0 = 0,
      year =  365.256363 *60*60*24,
      mass = NA,
      sun_mass = 1.98847 * 10^30,
      # Jupiter system mass including the four largest moons
      jupiter_system_mass = 1.898576 * 10^27,
      radius = NA,
      pie = 3.1415926535897932385,
      two_pie = 2 * 3.1415926535897932385,
      initialize = function(name, year_length=365.256363, aphelion=NA,
                            planet_mass=5.9722 * 10^24, moon_mass=7.342 * 10^22,
                            location=NA, set_v0=FALSE,
                            heading0=192.94719, heading_inclin0=0, radius=6371) {
        self$name <- name
        ## Universal Gravitational Constant
        G <- 6.6743*10^(-11)
        mass <- planet_mass + moon_mass
        jup_distance0 <- 816.6*10^9
        pie <- 3.1415926535897932385
        two_pie <- 2 * pie
        
        if(exists("location", mode="environment")){
           x <- location$x
           y <- location$y
           z <- location$z
           self$distance <- ((x^2)+(y^2)+(z^2))^0.5
           self$theta <- self$arctrig360(x=x,y=y)
           if( (x + y) != 0) {
             self$phi <- 360 * asin(z / (  ((x^2)+(y^2)  + z^2  + 0.0001)^0.5  ) ) / self$two_pie 
           } else {
             self$phi <- 90 
           }
           d <- (x^2 + y^2 + z^2)^0.5
           aphelion <- d
        }  else { 
           location <- PointLocationVector$new(mag=aphelion*10^9, theta=45,phi=1.3)
           x <- location$x
           y <- location$y
           z <- ((x^2 + y^2)^0.5)*cos(two_pie*1.3/360)
           z <- location$z
           d <- aphelion*10^9
        }  
        self$position <- location
        self$x <- x
        self$y <- y
        self$z <- z
        self$distance <- d
        self$aphelion <- d
        self$theta <- location$theta
        self$phi <- location$phi
        self$radius <- radius * 1000
        ##r_sun <- d*mass/m_sun
        sun_distance0 <- -jup_distance0*self$jupiter_system_mass/self$sun_mass
        if(set_v0){
            if (d > (2 * (10^9)) ) {
               v0 <- ((G*self$sun_mass)/d)^0.5
            } else {
               ## Only true for the sun
               ## Initial sun velocity
               v0 <- ((G*self$jupiter_system_mass*sun_distance0)/(d^2))^0.5
            }
            self$v0 <- v0
            self$px <- mass * v0 * cos(self$two_pie*heading0/360) *
                                   cos(self$two_pie*heading_inclin0/360)
            self$py <- mass * v0 * sin(self$two_pie*heading0/360) *
                                   cos(self$two_pie*heading_inclin0/360)
            self$pz <- mass * v0 * sin(self$two_pie*heading_inclin0/360)
            #cat("Initial x-velocity = ",self$px/mass,"m/s, initial y-velocity = ",self$py/mass,"m/s\n")
        } 
        self$heading0 <- heading0
        self$heading_inclination0 <- heading_inclin0 
        self$mass <- mass
        self$year <- year_length * 3600 * 24
      },
      get_mass = function() {
         self$mass
      },
      set_x = function(x) {
         self$x <- x
         d <- ((x^2)+(self$y)^2+(self$z^2))^0.5
         self$distance <- d
         self$position <- PointLocationVector$new(x=x,y=self$y,z=self$z)
         self$theta <- self$position$theta
         self$phi <- self$position$phi
      },
      set_y = function(y) {
         self$y <- y
         d <- ((self$x^2)+(y^2)+(self$z^2))^0.5
         self$distance <- d
         self$position <- PointLocationVector$new(x=self$x,y=y,z=self$z)
         self$theta <- self$position$theta
         self$phi <- self$position$phi
      },
      set_z = function(z) {
        self$z <- z
        d <- ((self$x^2)+(self$y^2)+(z^2))^0.5
        self$distance <- d
        self$position <- PointLocationVector$new(x=self$x,y=self$y,z=z)
        self$theta <- self$position$theta
        self$phi <- self$position$phi
      },
      get_x = function() {
         self$x
      },
      get_y = function() {
         self$y
      },
      get_z = function() {
        self$z
      },
      set_px = function(p) {
         self$px <- p
      },
      set_py = function(p) {
         self$py <- p
      },
      set_pz = function(p) {
        self$pz <- p
      },
      get_px = function() {
         self$px
      },
      get_py = function() {
         self$py
      },
      get_pz = function() {
        self$pz
      },
      get_vx = function() {
        self$px/self$mass
      },
      get_vy = function() {
        self$py/self$mass
      },
      get_vz = function() {
        self$pz/self$mass
      },
      get_v0 = function() {
         self$v0
      },
      set_v0 = function(v,h=self$heading0,incln=self$heading_inclination0) {
        self$v0 <- v
        self$heading0 <- h
        self$heading_inclination0 <- incln
        self$px <- self$mass * v * cos(self$two_pie*h/360) * cos(self$two_pie*incln/360)
        self$py <- self$mass * v * sin(self$two_pie*h/360) * cos(self$two_pie*incln/360)
        self$pz <- self$mass * v * sin(self$two_pie*incln/360)
        #cat("Initial velocity = ",v,"m/s, initial_heading = ",self$heading0,"degrees\n")
      },
      get_heading0 = function() {
         self$heading0
      },
      sgn = function(x){
        x >= 0
      },
      arctrig360 = function(x,y, degrees = TRUE){
        sx <- self$sgn(x)
        sy <- self$sgn(y)
        if(x != 0 ){
          theta <- atan(y/x) +
            self$pie * ((!sx && sy) || (!sx && !sy)) +
            self$two_pie * (sx && !sy)
          if(degrees == TRUE){
            theta <- 360 * theta / self$two_pie
          }
        } else if (x == 0 && y > 0){
          theta <- self$pie/2
          if(degrees == TRUE){
            theta <- 90
          }
        } else if (x == 0 && y < 0){
          theta <- 3 * self$pie / 2
          if(degrees == TRUE){
            theta <- 270
          }
        } else {
          theta <- 0
        }
        theta
      }
   )
)

## Class 5: SolarSystem

SolarSystem <- R6Class("SolarSystem", 
  list(
    other_planets = NULL,
    sun = NULL,
    jupiter= NULL,
    earth = NULL,
    sun_mass = 1.98847 * 10^30,
    jupiter_year = 11.862*365.256363,
    # Jupiter system mass including the four largest moons
    jupiter_system_mass = 1.898576 * 10^27,
    initialize = function(planet="earth",v0_jupiter=12423, v0_earth=29298.59295,
                          calibrate=TRUE) {
      ## Jupiter and the sun are the two dominate features of the solar system.
      ## To simplify the equations, the plane of the solar system is defined
      ## by Jupiter's orbit and the inclination for earth's orbit and all others
      ## are given with respect to zero inclination for Jupiter's orbit plane.
      
      ## The x-axis is defined by the line through the initial positions
      ## of Jupiter and the sun. The x-y plane is defined by the plane formed 
      ## by the x-axis and the initial direction of Jupiter's velocity.
      ## The z-axis is defined by the line through the center of mass for the 
      ## initial positions of Jupiter and the sun and perpendicular
      ## to the x-y plane. 

      ## Initial Conditions
      
      ## The initial direction of the earth's velocity is set at aphelion
      ## above the x-y plane and parallel to it.  The earth's orbit inclined by
      ## about 1.3 degrees from plane of Jupiter's orbit so that aphelion the
      ## z coordinate of the earth is maximum. 
      G <- 6.6743*10^(-11)
      pie <- 3.1415926535897932385
      two_pie <- 2 * pie
      sun <- self$get_sun(calibrate)
      jupiter <- self$get_jupiter(v0=v0_jupiter)
      earth <- self$get_earth(v0=v0_earth)
      self$sun <- sun
      self$jupiter <- jupiter
      self$earth <- earth
      if(planet=="earth"){
        self$other_planets <- list(sun,jupiter) 
      } else {
        self$other_planets <- list(sun,earth) 
      }
      self$other_planets
    },
    get_sun = function(calibrate=TRUE) {
      ## Jupiter and the sun are the two dominate features of the solar system.
      G <- 6.6743*10^(-11)
      pie <- 3.1415926535897932385
      two_pie <- 2 * pie
      
      sun_radius <- 695700 # km
      sun_mass <- self$sun_mass
      jup_mass <- self$jupiter_system_mass
      jup <- self$get_jupiter() 
      
      ## Calculation of initial center of mass 
      jup_distance0 <- jup$distance  #793042000000
      sun_distance0 <- jup_distance0*jup_mass/sun_mass
      inclination <- 1.2986 # degrees

      jup_vx <- jup$px / jup_mass
      jup_vy <- jup$py / jup_mass
      jup_vz <- jup$pz / jup_mass
      
      sun_vx <- -jup_vx*jup_mass/sun_mass
      sun_vy <- -jup_vy*jup_mass/sun_mass
      sun_vz <- -jup_vz*jup_mass/sun_mass
      v_0 <- VelocityVector$new(vx= sun_vx, vy= sun_vy, vz= sun_vz)
      v0 <- v_0$mag
      ## planet1_angle <- 5.246395
      ## heading0 <- 104.75385
      theta <- jup$theta - 180
      heading0 <- v_0$theta
      sun_position0 <- PointLocationVector$new(mag=sun_distance0,
                                                 theta = theta , phi = -inclination)
      sun <- Planet$new(name="sun",year_length=self$jupiter_year,
                        planet_mass=sun_mass, moon_mass= 0,
                        location=sun_position0, set_v0=TRUE, heading0=heading0,
                        radius=sun_radius)
      #v0 <- ((G*jup_mass*sun_distance0)/(jup_distance0^2))^0.5
      # v_sun0 <- ((-G*m2*sun_distance0)/(r2^2))^0.5
      #((-G*m2*r1)/(r2^2))^0.5
      sun$set_v0(v0)
      sun
    },
    get_jupiter = function(v0=12413) {
      G <- 6.6743*10^(-11)
      sun_mass <- 1.98847 * 10^30
      jupiter_mass <- 1.8982*10^27
      
      # Jupiter's moons
      io <- 8.9*10^22
      europa <- 4.8*10^22
      callisto <- 8.9*10^22
      ganymede <- 15*10^22
      amalthea <- 208*10^16
      jup_moons <- io + europa + callisto + ganymede
      # jupiter_mass <- jupiter_mass  + jup_moons
      
      inclination <- 1.2986 # degrees
      ## Sidereal orbit period (days)	4,332.589	11.862
      # synodic period 398.88; orbit yr = 11.862
      #jup_year <- 11.862*365.256363 #    374342400
 
      jup_year_secs <- 24*3600 * self$jupiter_year

      #jup_distance0 <- 816.62*10^9
      #jup_position0 <- PointLocationVector$new(x=distance0,y=0,z=0)
      
      # Jupiter at Aphelion: "2017-02-17 07:17:00 GMT"
      # https://in-the-sky.org/news.php?id=20170217_12_100
      
      jup_ap_2017_secs <- as.numeric(as.POSIXct("2017-02-17 07:17:00 GMT",
                                                origin="1970-01-01",tz="GMT"))
      jup_ap_2017 <- as.POSIXct(x=jup_ap_2017_secs ,tz="GMT",origin="1970-01-01 00:00:00")
      jup_ap_2017 <- jup_ap_2017 + as.difftime((0)*jup_year_secs, unit="secs")
      jup_ap_1957 <- jup_ap_2017 + as.difftime((-5)*jup_year_secs, unit="secs")
      jup_ap_1957 <- as.POSIXct(x="1957-10-26 23:16:58 GMT",
                                tz="GMT",origin="1970-01-01 00:00:00")
      jup_ap_1957_secs <- as.numeric(jup_ap_1957,origin="1970-01-01",tz="GMT")
      #  -384396182
      
      ap2050secs <- 2540645455
      ap1957secs <- ap2050secs + (1957-2050)*31558118
      
      jup_ap_1957 <- "1957-10-26 23:16:58 GMT"
      earth_ap1957 <- as.POSIXct(x="1957-07-04 19:28:01 GMT",
                                 tz="GMT",origin="1970-01-01 00:00:00")
      earth_ap_1957_secs <- as.numeric(earth_ap1957,origin="1970-01-01",tz="GMT")
      #  [1]  -394259519
      
      diff <- difftime(jup_ap_1957,earth_ap1957)
      lag_angle <- 360 * (as.numeric(diff) / self$jupiter_year ) 
      # [1] -114.159 days
      
      # Lag of 9.559484 degrees or 115.0502 days from the sun's aphelion on
      # July 4, 1957 to Jupiter's aphelion on October 26, 1957. 
      
      # Given that the angle for Jupiter's aphelion is 14.75385 degrees,
      # its angle should lag 9.559484 degrees or 115.0502 days on July 4 
      # from 14.75385 degrees. Jupiter's angle on July 4 should be 
      # 14.75385 - 9.559484 or 5.194366 degrees. 
      
      # However, given the velocity lower near aphelion resulting in an angle
      # of 5.196012 degrees.
      
      distance0 <- 816.616*10^9## Calibration: lag is 0 degrees
      theta <- 194.75385 
      heading0 <- theta + 90
      radius <- 7192
      #v_0 <- VelocityVector$new(vx= 0, vy=  12413.9, vz= 0)
      jup_position0 <- PointLocationVector$new(mag=distance0,
                                               theta=theta, phi = inclination)
      jup <- Planet$new(name="jupiter",year_length=self$jupiter_year, 
                        planet_mass= jupiter_mass, moon_mass=jup_moons,
                        location=jup_position0, set_v0=TRUE, heading0=heading0,
                        radius=radius)  
      v_jup0 <- ((G*sun_mass)/distance0)^0.5 # 12753.08
      jup$distance <- distance0
      jup$set_v0(v0)
      jup$theta <- theta
      jup$heading0 <- heading0
      v_0 <- VelocityVector$new(mag = v0, theta = heading0,
                                phi = inclination)
      mass <- jupiter_mass + jup_moons
      jup$px <- v_0$vx  * mass
      jup$py <- v_0$vy  * mass
      jup$pz <- v_0$vz  * mass
      jup
    },
    get_earth = function(v0=29298.59295) {
      # Aphelion distance to CM for solar system
      ap2050secs <- 2540645455
      ap2050 <- as.POSIXct(x=ap2050secs,tz="GMT",origin="1970-01-01 00:00:00")
      ap1957secs <- ap2050secs + (1957-2050)*31558118
      ap1958secs <- ap2050secs + (1958-2050)*31558118
      ap2020secs <- ap2050secs + (2020-2050)*31558118
      as.POSIXct(x=ap1958secs,tz="GMT",origin="1970-01-01 00:00:00")
      ## "1957-07-04 19:28:01 GMT"
      ## "1958-07-05 01:36:39 GMT"
      ## The distance between the earth and the center of the solar system is
      ## known to vary from about 147.1-million to 152.1-million kilometers
      ## every year. Consider the additional variance of the distance between
      ## the earth and the sun caused by the small orbit of the sun around the
      ## center of the solar system of very roughly 0.8-million kilometers
      ## with a period roughly the same as Jupiter's orbit of 11.862 years
      ## to a first approximation for this model which neglects Saturn and the
      ## other planets excluded from the three-body model.
      #distance0 <- 152.1
      distance0 <- 152.147
      inclination <- 0
      #earth_distance_xy_plane <- 152.1*cos(6.28*earth_inclination/360)
      earth_mass <- 5.9722 * 10^24
      radius <- 6378 #km
      moon_mass <- 7.342 * 10^22  # [1] 31558118 seconds
      m3 <- earth_mass + moon_mass
      year <- 365.256363
      heading0 <- 12.94719
      # 282.94719 + 90
      earth_position0 <- PointLocationVector$new(mag=distance0*10^9,
                                                  theta=282.94719,
                                                  phi = inclination)
      earth <- Planet$new(name="earth",year_length=year,
                          planet_mass=earth_mass,moon_mass=moon_mass,
                          location=earth_position0,radius=radius,
                          set_v0=TRUE, heading0=heading0)
      earth$set_v0(v0)
      earth
    },
    get_cm = function(planet1,planet2){
      cm <- CenterOfMass$new(planet1,planet2)
      cm$position
    }
  )
)

## Class 6: CenterOfMass

## Only for two planets
CenterOfMass <- R6Class("CenterOfMass", 
  list(
    position = NULL,
    #PointLocationVector$new(),
    x = NA,
    y = NA,
    z = NA,
    mass = NA,
    initialize = function(planet1, planet2) {
      coord <- self$cm(planet1,planet2)
      x <- coord[1]
      y <- coord[2]
      z <- coord[3]
      self$x <- x
      self$y <- y
      self$z <- z
      self$position = PointLocationVector$new(x=x, y=y,z=z)
      m1 <- planet1$get_mass()
      m2 <- planet2$get_mass()
      m <- m1 + m2
      self$mass <- m
    },
    cm = function(planet1,planet2){
      m1 <- planet1$get_mass()
      m2 <- planet2$get_mass()
      x <- (m1*planet1$x + m2*planet2$x)/(m1+m2)
      y <- (m1*planet1$y + m2*planet2$y)/(m1+m2)
      z <- (m1*planet1$z + m2*planet2$z)/(m1+m2)
      c(x,y,z)
    },
    get_mass = function() {
      self$mass
    },
    get_x = function() {
      self$x
    },
    get_y = function() {
      self$y
    },
    get_z = function() {
      self$z
    }
  )
)

## Class 7: OrbitData 

OrbitData <- R6Class("OrbitData", 
    list(
      specification = NULL,
      aphelion = NA,
      period = NA,
      perihelion = NA,
      m_sun = 1.99*10^30,
      x0 = NA,
      y0 = NA,
      theta0 = NA,
      v0 = NA,
      start_time = 0,
      data = data.frame(),
      G = 6.6743*(10^-11),
      initialize = function(solar_system, planet, planet_name,v0,
                            sample_rate=5000,decimator=8,
                            revolutions = 1) {
        self$G <- 6.6743*(10^-11)
        self$data <- self$get_orbit_data(solar_system,
                                         planet,planet_name=planet_name,v0=v0,
                                         sample_rate=sample_rate,
                                         decimator=decimator,
                                         revolutions = revolutions)
      },
      force360 = function(f,theta,phi){
        two_pie <- 2*3.141592
        theta_rad <- two_pie * theta / 360
        phi_rad <- two_pie * phi / 360
        fx <- f * cos(theta_rad) * cos(phi_rad)
        fy <- f * sin(theta_rad) * cos(phi_rad)
        fz <- f * sin(phi_rad)
        c(fx,fy,fz)
      },
      get_orbit_data = function(solar_system, planet,planet_name,v0,
                                sample_rate=5000,decimator=8,
                                revolutions = 1){
        otherplanets <- solar_system$other_planets
        sun <- otherplanets[[1]]
        planet_yr <- planet$year
        planet_theta0 <- planet$theta
        planet_phi0 <- planet$phi
        planet_distance0 <- planet$distance
        planet_x0 <- planet$x
        planet_y0 <- planet$y
        planet_z0 <- planet$z
        planet_vx0 <- planet$get_vx()
        planet_vy0 <- planet$get_vy()
        planet_vz0 <- planet$get_vz()
        planet_heading0 <- planet$heading0
        planet_heading_incln0 <- planet$heading_inclination0
        
        if (planet$name == "jupiter"){
          sun_theta0 <- sun$theta
          sun_phi0 <- sun$phi
          sun_distance0 <- sun$distance
          sun_x0 <- sun$x
          sun_y0 <- sun$y
          sun_z0 <- sun$z
          sun_vx0 <- sun$get_vx()
          sun_vy0 <- sun$get_vy()
          sun_vz0 <- sun$get_vz()
          sun_heading0 <- sun$heading0
          sun_heading_incln0 <- sun$heading_inclination0
        }
        
        planet1 <- otherplanets[[2]]
        pt1 <- c(sun$x,sun$y,sun$z)
        pt2 <- c(planet1$x,planet1$y,planet1$z)
        pt3 <- c(planet$x,planet$y,planet$z)
        
        m1 <- sun$mass
        m2 <- planet1$mass
        m3 <- planet$mass
        sun_mass <- sun$mass
        m12 <- m1 + m2
        m13 <- m1 + m3
        m23 <- m2 + m3
        
        # center of mass
        cm12 <- self$get_cm(sun,planet1)
        cm13 <- self$get_cm(sun,planet)
        cm23 <- self$get_cm(planet1,planet)
        cm12x <- cm12$x
        cm12y <- cm12$y
        cm12z <- cm12$z
        cmx <- ((m1+m2)*cm12x + m3*planet$x)/(m1+m2+m3)
        cmy <- ((m1+m2)*cm12y + m3*planet$y)/(m1+m2+m3)
        cmz <- ((m1+m2)*cm12z + m3*planet$z)/(m1+m2+m3)
        cm <- PointLocationVector$new(x=cmx, y=cmy,z=cmz)
        # vector angles
        # The vector points from the tail point
        # represented by the first parameter
        # to the head by the second parameter.
        
        th1_23 <- cm23$diff(cm23,sun$position)$theta
        th2_13 <- cm13$diff(cm13,planet1$position)$theta
        th3_12 <- cm12$diff(cm12,planet$position)$theta
        
        d1_23 <- cm23$distance(cm23,sun$position)
        d2_13 <- cm13$distance(cm13,planet1$position)
        d3_12 <- cm12$distance(cm12,planet$position)
        
        period0 <- planet$year
        dt0 <- period0 / sample_rate
        
        ## as.POSIXct(x=ap1958secs,tz="GMT",origin="1970-01-01 00:00:00")
        ## [1] "1958-07-05 01:36:39 GMT"
        ## (earth_ap_1958_secs - jup_ap_1957_secs)/86400 = 251.097 days of lag
        lagsecs <-  (251.097 * 86400)
        lagintervals <-  floor(lagsecs/dt0)
        #iterations <- (revolutions+9) * 1.1 * sample_rate + lagintervals
        iterations <- (revolutions) * 1.1 * sample_rate + lagintervals
        fast <- 10 * 1.1 * sample_rate
        fast <- 0
        
        ttt <- Sys.time()
        
        for (i in 1:iterations) {
           ## Reset planet at aphelion time
           ## This conditional block is only used twice.
           if(i==1 ||  i==lagintervals){
             dt <- dt0/10
             
             j <- i + 1
             if (planet_name == "jupiter"){
               planet <- solar_system$jupiter
             }
             if (planet_name == "earth"){
               planet <- solar_system$earth
             }
             planet$theta <- planet_theta0
             planet$phi <- planet_phi0
             planet$y <- planet_y0
             planet$set_x(planet_x0)
             planet$set_z(planet_z0)
             planet$year  <- planet_yr
             planet$heading0 <- planet_heading0
             planet$heading_inclination0 <- planet_heading_incln0
             planet$set_v0(v0)
             vx_planet <- planet$px / m3
             vy_planet <- planet$py / m3
             vz_planet <- planet$pz / m3
             
             if (planet_name == "jupiter"){
               theta <- planet$theta - 180
               sun$theta <- sun_theta0
               sun$y <- sun_y0
               sun$set_x(sun_x0)
               sun$set_z(sun_z0)
               sun$heading0 <- sun_heading0
               sun$heading_inclination0 <- -sun_heading_incln0
               jup_vx <- planet$px / m3
               jup_vy <- planet$py / m3
               jup_vz <- planet$pz / m3
               # sun_vx <- -jup_vx*jup_mass/m1
               # sun_vy <- -jup_vy*jup_mass/m1
               # sun_vz <- -jup_vz*jup_mass/m1
               sunvx <- sun_vx0 
               sunvy <- sun_vy0 
               sunvz <- sun_vz0 
               sun_v0 <- VelocityVector$new(vx= sunvx, vy= sunvy, vz= sunvz)
               heading0 <- sun_v0$theta
               sun_position0 <- PointLocationVector$new(mag=sun_distance0,
                                                        theta = theta , phi = sun_phi0) #  -phi0
               sun <- Planet$new(name="sun",year_length=self$jupiter_year,
                                 planet_mass=sun$mass, moon_mass= 0,
                                 location=sun_position0, set_v0=TRUE, heading0=heading0,
                                 radius=sun$radius)
               sun$set_v0(sun_v0$mag)
             }
          ## Basic loop section:    
             pt3 <- c(planet$x,planet$y,planet$z)
             
             # center of mass
             cm13 <- self$get_cm(sun,planet)
             cm23 <- self$get_cm(planet1,planet)
             cm12 <- self$get_cm(sun,planet1)
             
             cm123x <- ((m1+m2)*cm12x + m3*planet$x)/(m1+m2+m3)
             cm123y <- ((m1+m2)*cm12y + m3*planet$y)/(m1+m2+m3)
             cm123z <- ((m1+m2)*cm12z + m3*planet$z)/(m1+m2+m3)
             cm123 <- PointLocationVector$new(x=cm123x, y=cm123y,z=cm123z)
             
           # initialize data vectors
             d_sun <- sun$distance
             d_planet1 <- planet1$distance
             d_planet1_sun <- sun$position$distance(planet1$position,sun$position)
             d_planet <- planet$distance
             d_planet_sun <- sun$position$distance(planet$position,sun$position)
             
             sun_angle <- sun$theta
             planet1_angle <- planet1$theta
             planet_angle <- planet$theta
             planet_phi <- c(planet$phi)
             
             sun_x_coord <- sun$get_x() 
             sun_y_coord <- sun$get_y()
             sun_z_coord <- sun$get_z()
             planet1_x_coord <- planet1$get_x() 
             planet1_y_coord <- planet1$get_y() 
             planet1_z_coord <- planet1$get_z() 
             planet_x_coord <- planet$get_x() 
             planet_y_coord <- planet$get_y() 
             planet_z_coord <- planet$get_z()
             sun_vx <- sun$get_px()/m1
             sun_vy <- sun$get_py()/m1
             sun_vz <- sun$get_pz()/m1
             planet1_vx <- planet1$get_px()/m2
             planet1_vy <- planet1$get_py()/m2 
             planet1_vz <- planet1$get_pz()/m2
             planet_vx <- planet$get_px()/m3 
             planet_vy <- planet$get_py()/m3 
             planet_vz <- planet$get_pz()/m3
             time <- c(0)
             t <- 0
             #  (revolutions+9) * 1.1 * sample_rate + lagintervals
           } else if ((i>lagintervals) && (i < lagintervals + fast)  ){
             ## This condition is only true for a short number of cycles
             ## at the beginning of the simulation where the sample rate is
             ## increased by a factor of 10 for about one orbit. 
             dt <- dt0/10
           } else {
             ## Reset sample rate for main simulation. 
             dt <- dt0
           }
       ##  Calculate gravitational force for the sun
           f1_23 <- ForceVector$new(head=cm23, tail=sun$position,
                                    planet_mass=m1, cm_mass=m23)
           moment <- self$get_new_momentum(sun, f1_23, dt)
           velocity <- self$get_new_velocity(moment, m1)
       ##  Update position vector for sun        
           pos <- self$get_new_position(sun, velocity, dt)
       ##  Reset momentum vector of Sun    
           sun$set_px(moment[1])
           sun$set_py(moment[2])
           sun$set_pz(moment[3])
           vx_sun <- moment[1]/m1
           vy_sun <- moment[2]/m1
           vz_sun <- moment[3]/m1
           sun$set_x(pos[1])
           sun$set_y(pos[2])
           sun$set_z(pos[3])
           x_sun <- pos[1]
           y_sun <- pos[2]
           z_sun <- pos[3]
           
       ##  Calculate gravitational force for second body
           f2_13 <- ForceVector$new(head=cm13, tail=planet1$position,
                                    planet_mass=m2, cm_mass=m13) 
           #f2_13mag <- G*m23*m1/(d1_23^2)
           moment <- self$get_new_momentum(planet1, f2_13, dt)
           velocity <- self$get_new_velocity(moment, planet1$get_mass())
       ##  Update position vector for second body, often this is Jupiter.  
           pos <- self$get_new_position(planet1, velocity, dt)
       ##  Reset momentum vector   
           planet1$set_px(moment[1])
           planet1$set_py(moment[2])
           planet1$set_pz(moment[3])
           vx_planet1 <- moment[1]/m2
           vy_planet1 <- moment[2]/m2
           vz_planet1 <- moment[3]/m2
           planet1$set_x(pos[1])
           planet1$set_y(pos[2])
           planet1$set_z(pos[3])
           x_planet1 <- pos[1]
           y_planet1 <- pos[2]
           z_planet1 <- pos[3]
       ##  Calculate gravitational force for third body (often earth)
           f3_12 <- ForceVector$new(head=cm12, tail=planet$position,
                                    planet_mass=m3, cm_mass=m12)
           moment <- self$get_new_momentum(planet, f3_12, dt)
           velocity <- self$get_new_velocity(moment, planet$get_mass())
       ##  Update position vector for third body, often this is earth.  
           pos <- self$get_new_position(planet, velocity, dt)
       ##  Reset momentum vector    
           planet$set_px(moment[1])
           planet$set_py(moment[2])
           planet$set_pz(moment[3])
           vx_planet <- moment[1]/m3
           vy_planet <- moment[2]/m3
           vz_planet <- moment[3]/m3
           planet$set_x(pos[1])
           planet$set_y(pos[2])
           planet$set_z(pos[3])
           x_planet <- pos[1]
           y_planet <- pos[2]
           z_planet <- pos[3]
           
           d_planet1_to_sun <- sun$position$distance(planet1$position,sun$position)
           d_planet_to_sun <- sun$position$distance(planet$position,sun$position)
           
           cm12 <- self$get_cm(sun,planet1)
           cm13 <- self$get_cm(sun,planet)
           cm23 <- self$get_cm(planet1,planet)
           
           cm123x <- ((m1+m2)*cm12x + m3*planet$x)/(m1+m2+m3)
           cm123y <- ((m1+m2)*cm12y + m3*planet$y)/(m1+m2+m3)
           cm123z <- ((m1+m2)*cm12z + m3*planet$z)/(m1+m2+m3)
           cm123 <- PointLocationVector$new(x=cm123x, y=cm123y,z=cm123z)
           # vector angles
           # The vector points from the tail point
           # represented by the first parameter
           # to the head by the second parameter.
           
          th1_23 <- cm23$diff(cm23,sun$position)$theta
          th2_13 <- cm13$diff(cm13,planet1$position)$theta
          th3_12 <- cm12$diff(cm12,planet$position)$theta

          d1_23 <- cm23$distance(cm23,sun$position)
          d2_13 <- cm13$distance(cm13,planet1$position)
          d3_12 <- cm12$distance(cm12,planet$position)
          t <- t + dt
          
          if( i==lagintervals ||
              (
                (i/decimator == floor((i)/decimator)) && i > lagintervals
              )
            )
          { 
            ## Add current loop results to cumulative vectors
            cmx <- c(cmx, cm123x)
            cmy <- c(cmy, cm123y)
            cmz <- c(cmz, cm123z)
            sun_x_coord <- c(sun_x_coord,x_sun)
            sun_y_coord <- c(sun_y_coord,y_sun)
            sun_z_coord <- c(sun_z_coord,z_sun)
            sun_vx <- c(sun_vx,vx_sun)
            sun_vy <- c(sun_vy,vy_sun)
            sun_vz <- c(sun_vz,vz_sun)
            planet1_x_coord <- c(planet1_x_coord,x_planet1)
            planet1_y_coord <- c(planet1_y_coord,y_planet1 )
            planet1_z_coord <- c(planet1_z_coord,z_planet1 )
            planet1_vx <- c(planet1_vx,vx_planet1)
            planet1_vy <- c(planet1_vy,vy_planet1)
            planet1_vz <- c(planet1_vz,vz_planet1)
            planet_x_coord <- c(planet_x_coord,x_planet)
            planet_y_coord <- c(planet_y_coord,y_planet)
            planet_z_coord <- c(planet_z_coord,z_planet)
            planet_vx <- c(planet_vx,vx_planet)
            planet_vy <- c(planet_vy,vy_planet)
            planet_vz <- c(planet_vz,vz_planet)
            
            time <- c(time,t)
            dt <- dt0
            #d_sun <- c(d_sun,d1_23)
            d_sun <- c(d_sun,sun$distance)
            d_planet1 <- c(d_planet1,planet1$distance)
            d_planet1_sun <- c(d_planet1_sun,d_planet1_to_sun)

            d_planet <- c(d_planet,planet$distance)
            d_planet_sun <- c(d_planet_sun,d_planet_to_sun)

            sun_angle <- c(sun_angle, sun$theta)
            planet1_angle <- c(planet1_angle, planet1$theta)
            planet_angle <- c(planet_angle, planet$theta)
            if(i/20000  ==  floor(i/20000)){
              currtime <- paste(as.POSIXct(x=Sys.time()))
              cat(i, "iterations of the loop completed at ",
                  currtime,".\n")
            }
          }
        }  # end for loop
        ttt2 <- Sys.time()
        difff <- difftime(ttt2, ttt, units = "secs")
        cat("Loop time for ",iterations," was ", difff,  " seconds.\n")
        data.frame(time,cmx,cmy,cmz,
                    d_sun, sun_x_coord, sun_y_coord, sun_z_coord,
                    sun_vx, sun_vy, sun_vz, sun_angle,
                    d_planet1, planet1_x_coord, planet1_y_coord, planet1_z_coord,
                    planet1_vx, planet1_vy, planet1_vz,
                    planet1_angle, d_planet1_sun,
                    d_planet, planet_x_coord, planet_y_coord, planet_z_coord,
                    planet_vx, planet_vy, planet_vz,  
                    planet_angle, d_planet_sun
        )
     },
     get_cm = function(planet1,planet2){
        cm <- CenterOfMass$new(planet1,planet2)
        cm$position
     },
     distance = function(pt1, pt2){
       delta_x <- pt[1] - pt2[1]
       delta_y <- pt[2] - pt2[2]
       delta_z <- pt[3] - pt2[3]
       (delta_x^2 + delta_y^2 + delta_z^2)^0.5
     },
     get_new_momentum = function(planet, force, dt) {
       px <- planet$get_px() + force$fx * dt
       py <- planet$get_py() + force$fy * dt
       pz <- planet$get_pz() + force$fz * dt
       c(px,py,pz)
     },
     get_new_velocity = function(moment, mass) {
       vx <- moment[1]/mass
       vy <- moment[2]/mass 
       vz <- moment[3]/mass
       c(vx,vy,vz)
     },
     get_new_position = function(planet, velocity, dt) {
       x <- planet$x + velocity[1] * dt
       y <- planet$y + velocity[2] * dt
       z <- planet$z + velocity[3] * dt
       c(x,y,z)
     },
     cos_x = function(x) {
         # Converting degrees to radian 
         x <- x * (two_pie / 360); 
         x1 <- 1 
         # maps the sum along the series 
         cosx <- x1          
         ## holds the actual value cos[n] 
         ## cosval <- cos(x) 
         for (i in 1:8) {
           denominator = factorial(2 * i) 
           x1 <- -x1 * x^(2*i) / denominator
           cosx <- cosx + x1 
         }
         cosx
     }
  )
)

## Class 8: OrbitAnalysis

OrbitAnalysis <- R6Class("OrbitAnalysis", 
  list(
    aphelion = NA,
    period = NA,
    perihelion = NA,
    v0 = NA,
    start_time = 0,
    orbit_data = data.frame(),
    orbit_plot = NULL,
    initialize = function(planet, df, theta0=0, optimized=FALSE) {
      self$orbit_data <- df
      if(optimized==TRUE) {
        self$period <- self$get_period(df$time, df$planet_angle, theta0, planet$year)
      }
    },
    get_period = function(time,angle,theta0, year_length){
      len <- length(angle)
      vectheta0 <- rep(theta0,len)
      vec360minustheta <- rep(360-theta0,len)
      vectheta0 <- rep(theta0,len)
      angle_normal <- sapply(angle, function(x){
        if(x < theta0){
          x + 360 - theta0
        } else {
          x -  theta0
        }
      })
      vec360 <- rep(360,len)
      select_angle <- angle_normal < vec360
      vecyr <- rep(year_length,len)
      select_time  <-         time <  vecyr 
      record_minus <- sum(time < vecyr)
      record_plus <- record_minus + 1
      t1 <- time[record_minus]
      t2 <- time[record_plus]
      p1 <- angle_normal[record_minus]
      p2 <- angle_normal[record_plus]
      m <- (t2 - t1) / (p2 - p1) 
      ## interpolation
      if(p1 < 360 && p2 < p1){
        p2 <- p2 + 360
      } 
      if(p1 < 180 && p2 > p1){
        p1 <- p1 + 360
        p2 <- p2 + 360
      } 
      t1 + m * (360 - p1)
    },
    get_aphelion_period = function(time,d_planet){
      len <- length(time)
      vecaphelion <- rep(d_planet[1],len-1)
      aphelion_year <- d_planet[2:len] < vecaphelion
      record_minus <- sum(aphelion_year) + 1
      record_plus <- record_minus + 1
      
      t1 <- time[record_minus]
      t2 <- time[record_plus]
      p1 <- d_planet[record_minus]
      p2 <- d_planet[record_plus]
      delta <- t2 - t1
      if(p2 <= p1){
         t360 <- t1 + delta/2
      } else {
         t360 <- t2 - delta/2
      }
      t360
    },
    get_aphelion_correction = function(df){
      ## 99-year average
      ap2050secs <- 2540645455
      earth_ap_1957_secs <- ap2050secs + (1957-2050)*31558118
      earth_ap_1958_secs <- ap2050secs + (1958-2050)*31558118
      earth_ap_1959_secs <- ap2050secs + (1959-2050)*31558118
      earth_ap_1960_secs <- ap2050secs + (1960-2050)*31558118
      # earth_ap1957 <- as.POSIXct(x="1957-07-04 03:04:44 GMT",
      #                            tz="GMT",origin="1970-01-01 00:00:00")
      rows <- dim(df)[1]
      timevec <- rep(earth_ap_1958_secs, rows)  #  -362701401
      adjusted_time_vec <- df$time + timevec
      df[,"time"] <- adjusted_time_vec
      self$orbit_data <- df
      
      dt <- df[2,c("time")] - df[1,c("time")]
      t <- earth_ap_1958_secs
      
      rows_logic <- rows - 2
      earth_aphelion_logic <- rep(FALSE, rows) 
      planet_ap_logic <- rep(FALSE, rows)
      earth1959 <- rep(FALSE, rows) 
      
      for(i in 1:rows_logic){
        t <- t + dt
        j <- i + 1
        k <- i + 2
        planet_ap_logic[j] <-  df$d_planet[i] < df$d_planet[j]  &&  df$d_planet[j] > df$d_planet[k] 
      }
      df[,"planet_ap_logic"] <- planet_ap_logic
      planet_aphelion <- subset(df, planet_ap_logic==TRUE , 
                                select = c("time",
                                           "d_sun","sun_angle","sun_vx", "sun_vy", "sun_vz",
                                           "d_planet1","planet1_angle",
                                           "planet1_vx", "planet1_vy", "planet1_vz",
                                           "d_planet","planet_angle",
                                           "planet_vx", "planet_vy", "planet_vz"))
      
      
      aphelion_time <- as.POSIXct(planet_aphelion[1,"time"],origin="1970-01-01",tz="GMT")
      aphelion_1959 <- as.POSIXct(earth_ap_1959_secs, origin="1970-01-01")
      aphelion_time_secs <- as.numeric(aphelion_time)
      aphelion_1959_secs <- as.numeric(aphelion_1959)
      aphelion_error_secs <- aphelion_1959_secs - aphelion_time_secs 
      
      # jan_01_1959 <- as.POSIXct("1959-01-01 00:00:00 GMT", origin="1970-01-01")
      # jan_01_1959_secs <- as.numeric(jan_01_1959)
      # jan_01_1959_error <- diff(aphelion_1959, jan_01_1959) 
      
      timevec <- rep(aphelion_error_secs, rows)  #  -362701401
      adjusted_time_vec <- df$time + timevec
      df[,"time"] <- adjusted_time_vec
      self$orbit_data <- df
      adjusted_time_vec 
    }, 
    get_plot = function(x1,y1,x2,y2){
      solar_system <- SolarSystem$new()
      otherplanetlist <- solar_system$other_planets
      sun <- otherplanetlist[[1]]
      
      par(mar=c(5.1,6,4.1,2.1))
      xlims <- c(-10^12,10^12) / 10^9
      ylims <- c(-10^12,10^12) / 10^9
      xlabs <- seq(-1000, 1000, by=500)
      ylabs <- seq(-1000, 1000, by=500)
      samples <- seq(2,length(x1),by=8)
      xs1 <- x1[samples]/10^9
      ys1 <- y1[samples]/10^9
      pltraw <- plot(xs1,ys1,lwd=2,pch=19,cex=0.7,
                     xlim=xlims, ylim=ylims, xlab=" ", ylab=" ",
                     main="Solar System with Jupiter and Earth",
                     col.main="#008800",
                     xaxt="n", yaxt="n"  ,
                     cex.main=1.5, cex.sub=1.5)
      axis(1, at=xlabs,labels=xlabs, col.axis="black", cex.lab=1.5)
      axis(2, at=xlabs,labels=xlabs, col.axis="black", las=2, cex.axis=1.2)
      mtext("millions of km", side=1, line=2.5, cex=1.4,col="#008800")
      mtext("millions of km", side=2, line=3, cex=1.4,col="#008800")

      abline(h=seq(-1000,1000,by=100),v=seq(-1000,1000,by=100), col = "#DDDDFF")
      abline(h=seq(-1000,1000,by=500),v=seq(-1000,1000,by=500), col = "#99AA99")
      abline(h=0,v=0, col = "#448844",lwd=1.3)
      text(620, 570, "Jupiter",
           adj = c(0,00), cex = 1.4, col = "#008800")
      text(150, 150, "Earth",
           adj = c(0,00), cex = 1.4, col = "#008800")
      text(840, 100, "Apogee",
           adj = c(0,0), cex = 1.2, col = "#226622")
      xs2 <- x2[samples]/10^9
      ys2 <- y2[samples]/10^9
      par(new=TRUE)
      plot(xs2,ys2,xlim=xlims, ylim=ylims,
           xlab=" ", ylab=" ",
           xaxt="n", yaxt="n" 
           
      )
      draw.circle(sun$x/10^9,0, radius=10*sun$radius/10^9)
      draw.circle(sun$x/10^9,0, radius=10)
    },
    get_earth_plot = function(x1,y1,x2,y2){
      solar_system <- SolarSystem$new()
      otherplanetlist <- solar_system$other_planets
      sun <- otherplanetlist[[1]]
      
      par(mar=c(5.1,6,4.1,2.1))
      xlims <- c(-1.6*10^11,1.6*10^11) / 10^9
      ylims <- c(-1.6*10^11,1.6*10^11) / 10^9
      xlabs <- seq(-1000, 1000, by=50)
      ylabs <- seq(-1000, 1000, by=50)
      samples <- seq(2,length(x1),by=32)
      xs1 <- x1[samples]/10^9
      ys1 <- y1[samples]/10^9
      pltraw <- plot(xs1,ys1,lwd=2,pch=19,cex=0.7,
                     xlim=xlims, ylim=ylims, xlab=" ", ylab=" ",
                     main="Earth's Orbit",
                     col.main="#008800",
                     xaxt="n", yaxt="n"  ,
                     cex.main=1.5, cex.sub=1.5)
      axis(1, at=xlabs,labels=xlabs, col.axis="black", cex.lab=1.5)
      axis(2, at=xlabs,labels=xlabs, col.axis="black", las=2, cex.axis=1.2)
      mtext("millions of km", side=1, line=2.5, cex=1.4,col="#008800")
      mtext("millions of km", side=2, line=3, cex=1.4,col="#008800")
      abline(h=seq(-1000,1000,by=10),v=seq(-1000,1000,by=10), col = "#DDDDFF")
      abline(h=seq(-1000,1000,by=50),v=seq(-1000,1000,by=50), col = "#99AA99")
      abline(h=0,v=0, col = "#448844",lwd=1.3)
      text(840, 100, "Apogee",
           adj = c(0,0), cex = 1.2, col = "#226622")
      xs2 <- x2[samples]/10^9
      ys2 <- y2[samples]/10^9
      draw.circle(sun$x/10^9,0, radius=sun$radius/10^9)
    },
    get_sun_plot = function(x1,y1,x2,y2){
      solar_system <- SolarSystem$new(planet="earth")
      otherplanetlist <- solar_system$other_planets
      sun <- otherplanetlist[[1]]
      
      par(mar=c(5.1,6,4.1,2.1))
      xlims <- c(-2*10^9,2*10^9) / 10^9
      ylims <- c(-2*10^9,2*10^9) / 10^9
      xlabs <- seq(-1000, 1000, by=2)
      ylabs <- seq(-1000, 1000, by=2)
      samples <- seq(2,length(x1),by=1024)
      xs1 <- x1[samples]/10^9
      ys1 <- y1[samples]/10^9
      pltraw <- plot(xs1/10,ys1/10,lwd=2,pch=8,cex=0.7,
                     xlim=xlims, ylim=ylims, xlab=" ", ylab=" ",
                     main="Path of Sun around Center of Mass",
                     col.main="#008800",
                     xaxt="n", yaxt="n"  ,
                     cex.main=1.5, cex.sub=1.5)
      axis(1, at=xlabs,labels=xlabs, col.axis="black", cex.lab=1.5)
      axis(2, at=xlabs,labels=xlabs, col.axis="black", las=2, cex.axis=1.2)
      mtext("millions of km", side=1, line=2.5, cex=1.4,col="#008800")
      mtext("millions of km", side=2, line=3, cex=1.4,col="#008800")
      abline(h=seq(-1000,1000,by=1),v=seq(-1000,1000,by=1), col = "#DDDDFF")
      abline(h=seq(-1000,1000,by=2),v=seq(-1000,1000,by=2), col = "#99AA99")
      abline(h=0,v=0, col = "#448844",lwd=1.3)

      text(4,3, "152.1-km To Earth",
          adj = c(0,00), cex = 1.4, col = "#008800")
      arrows(0, 0,6,6, col= 'darkgreen')
      text(840, 100, "Apogee",
           adj = c(0,0), cex = 1.2, col = "#226622")
      xs2 <- x2[samples]/10^9
      ys2 <- y2[samples]/10^9
      par(new=TRUE)
      plot(xs2,ys2,xlim=xlims, ylim=ylims,
           xlab=" ", ylab=" ",
           xaxt="n", yaxt="n" ,pch=3)
      draw.circle(sun$x/10^9,0, radius=sun$radius/(10^9),
                  col=adjustcolor("#ee9922", alpha=0.5) )
    },
    get_sun_diagram = function(x,y){
      solar_system <- SolarSystem$new(planet="earth")
      otherplanetlist <- solar_system$other_planets
      sun <- otherplanetlist[[1]]
      earth <- solar_system$earth
      print(earth$distance) 
      
      par(mar=c(5.1,6,4.1,2.1))
      xlims <- c(-4*10^9,4*10^9) / 10^9
      ylims <- c(-2*10^9,4*10^9) / 10^9
      samples <- seq(2,length(x),by=512)
      xs1 <- x[samples]/10^9
      ys1 <- y[samples]/10^9
      pltraw <- plot(xs1,ys1,lwd=2,pch=3,cex=0.8,
                     xlim=xlims, ylim=ylims, xlab=" ", ylab=" ",
                     main="Earth Distance to Sun",
                     col.main="#008800",
                     xaxt="n", yaxt="n",
                     #sub="3-Body Sun-Jupiter-Earth System",
                     cex.main=1.5, cex.sub=1.5)
      mtext("", side=1, line=2.5, cex=1.4,col="#008800")
      mtext("", side=2, line=3, cex=1.4,col="#008800")
      abline(h=seq(-1000,1000,by=1),v=seq(-1000,1000,by=1), col = "#DDDDFF")
      abline(h=seq(-1000,1000,by=2),v=seq(-1000,1000,by=2), col = "#99AA99")
      abline(h=0,v=0, col = "#448844",lwd=1.3)
      text(-2.4,2.95, "Earth to Center-of-Mass", srt=300,
           adj = c(0,0), cex = 1.1, col = "#008800")
      text(-1.62,2.65, "Earth-Sun Distance", srt=307,
           adj = c(0,0), cex = 1.1, col = "#008800")
      arrows(-2.5, 3.5,-0.05,0.05, col= 'darkgreen')
      arrows(-2.5, 3.5,sun$x/10^9,0, col= 'darkgreen')
      mtext("Three Body Sun-Jupiter-Earth System",
            side=3, line=0.2, cex=1.2,col="#008800")
      text(-2.3, 3.6, "Earth: Shown at 10X size at close distance",
            adj = c(0,0), cex = 1.1, col = "#008800")
      text(-1.9, 3.33, "Actual position is approximately ",
           adj = c(0,0), cex = 1.1, col = "#008800")
      text(-1.9, 3.1, "150 million km away.",
           adj = c(0,0), cex = 1.1, col = "#008800")
      
      text(-3.3, -1.2, "The orbits are roughly centered around the center of mass",
           adj = c(0,0), cex = 1.1, col = "#008800")
      text(-3.3, -1.45, "of the solar system, including the motion of the sun.",
           adj = c(0,0), cex = 1.1, col = "#008800")
      text(-2.85, 0.55, "Sun Orbit Path",
           adj = c(0,0), cex = 1.3, col = "#226622")
      arrows(-1.9, 0.45,-sun$x/10^9,-0.1, col= 'darkgreen')
      text(1.1, 0.6, "Sun",
           adj = c(0,0), cex = 1.3, col = "#226622")
      draw.circle(sun$x/10^9,0, radius=sun$radius/(10^9),col=adjustcolor("#ee9922", alpha=0.5) )
      draw.circle(-2.5,3.5, radius=earth$radius/(10^8),col = "#6699ee",density=0.4)
    },
    get_sun_distance = function(t1,y1){
      solar_system <- SolarSystem$new()
      otherplanetlist <- solar_system$other_planets
      sun <- otherplanetlist[[1]]
      df_distance <- result[[1]]
      rows <- dim(df_distance)[1]
      
      par(mar=c(5.1,6,4.1,2.1))
      xlims <- c(0,12)
      ylims <- c(146,154)
      xlabs <- seq(-20, 20, by=2)
      ylabs <- seq(-140, 160, by=2)
      
      samples <- seq(1,rows,by=16)
      len <- length(samples)
      divisor <- rep(10^9,len)
      divisor2 <- rep(31558119,len)
      xs1 <- t1[samples]/divisor2
      ys1 <- y1[samples]/divisor
      plot(xs1,ys1,lwd=2,pch=19,cex=0.7,
           xlim=xlims, ylim=ylims, xlab=" ", ylab=" ",
           main="Solar System with Jupiter and Earth",
           col.main="#008800",
           xaxt="n", yaxt="n"  ,
           cex.main=1.5, cex.sub=1.5)
      axis(1, at=xlabs,labels=xlabs, col.axis="black", cex.lab=1.5)
      axis(2, at=ylabs,labels=ylabs, col.axis="black", las=2, cex.axis=1.2)
      mtext("Time (yrs)", side=1, line=2.5, cex=1.4,col="#008800")
      mtext("millions of km", side=2, line=3, cex=1.4,col="#008800")
      mtext("Earth Distance to Sun", side=3, line=-0, cex=1.2,
            col="#008800")
      abline(h=seq(-10,10,by=1),v=seq(-20,20,by=1), col = "#DDDDFF")
      abline(h=seq(-146,154,by=2),v=seq(-20,20,by=1), col = "#99AA99")
    },
    get_orbit_distance_plot = function(){
      solar_system <- SolarSystem$new("earth")
      otherplanetlist <- solar_system$other_planets
      sun <- otherplanetlist[[1]]
      df_distance <- self$orbit_data
      t1 <- df_distance$time
      d <- df_distance$d_planet_sun / (10^9)
      rows <- dim(df_distance)[1]
     
      par(mar=c(5.1,6,4.1,2.1))
      xlims <- c(1960,2040)
      ylims <- c(146,154)
      xlabs <- seq(1900, 2100, by=10)
      ylabs <- seq(140, 160, by=1)
      
      samples <- seq(1,rows,by=16)
      len <- length(samples)
      divisor <- rep(1,len)
      divisor2 <- rep(31558119,len)
      xs1 <- 1959 + t1[samples]/divisor2
      ys1 <- d[samples]/divisor
      plot(xs1,ys1,lwd=2,pch=19,cex=0.7,
           xlim=xlims, ylim=ylims, xlab=" ", ylab=" ",
           main="Earth-Sun Distance",
           col.main="#008800",
           xaxt="n", yaxt="n",
           cex.main=1.5, cex.sub=1.5)
      axis(1, at=xlabs,labels=xlabs, col.axis="black", cex.lab=1.5)
      axis(2, at=ylabs,labels=ylabs, col.axis="black", las=1, cex.axis=1.2)
      mtext("Year", side=1, line=2.5, cex=1.4,col="#008800")
      mtext("Distance (millions of kilometers)", side=2, line=3.5, cex=1.3,col="#008800")
      abline(h=seq(0,1000,by=1),v=seq(1900,2100,by=10),col = "#99AA99" )
      abline(v=seq(1905,2095,by=10),col = "#DDDDFF" )
    },
    get_irradiation_plot = function(){
      solar_system <- SolarSystem$new("earth")
      otherplanetlist <- solar_system$other_planets
      pie <- 3.1415926535897932385
      two_pie <- 2 * pie
      sun <- otherplanetlist[[1]]
      df_distance <- self$orbit_data
      t1 <- df_distance$time
      y1 <- df_distance$d_planet_sun / (10^9)
      rows <- dim(df_distance)[1]
      d <- df_distance$d_planet_sun 
      
      #watt_m2 <- 1368
      watt_m2 <- 1361
      
      msed <- (1/mean(d^(-2)))^0.5
      # [1] 149642243647
      # mean inverse square is [1] 4.465721e-23
      # Reciprocal is [1] 2.23928e+22
      
      r <- msed
      # effective radius of the earth's orbit is 
      # msed / 10^9  # [1] 149.6422 million kilometers
      # Based on msed mean inverse square-root distance
      # [1] 3.829803e+26 384.6 yotta watts (3.846×10^26 watts)
      # https://en.wikipedia.org/wiki/Solar_irradiance
      
      # area <- 2 * two_pie * r^2
      # total <- area * watt_m2
      
      yw <- 3.829803e+26
      
      #irrad <- 1361*(msed/ys1)^2
      irrad_mean <- yw/(2*two_pie*msed^2)
      irrad <- yw/(2*two_pie*d^2)
      
      par(mar=c(5.1,6,4.1,2.1))
      
      xlims <- c(1950,2040)
      ylims <- c(1280,1440)
      xlabs <- seq(1900, 2100, by=10)
      ylabs <- seq(1240, 1480, by=40)
      
      samples <- seq(1,rows,by=16)
      len <- length(samples)
      divisor <- rep(1,len)
      divisor2 <- rep(31558119,len)
      xs1 <- 1970 + t1[samples]/divisor2
      irad <- irrad[samples]/divisor
      plot(xs1,irad,lwd=2,pch=19,cex=0.7,
           xlim=xlims, ylim=ylims, xlab=" ", ylab=" ",
           main="Solar Irradiation Power for Earth",
           col.main="#008800",
           xaxt="n", yaxt="n",
           cex.main=1.5, cex.sub=1.5)
      axis(1, at=xlabs,labels=xlabs, col.axis="black", cex.lab=1.5)
      axis(2, at=ylabs,labels=ylabs, col.axis="black", las=2, cex.axis=1.2)
      mtext("Year", side=1, line=2.5, cex=1.3,col="#008800")
      mtext("Power Density (Watts square meter)", side=2, line=3.5, cex=1.4,col="#008800")
      mtext("Three-body Solution with Jupiter", side=3, line=-0, cex=1.2,
            col="#008800")
      abline(h=seq(1000,1500,by=10),v=seq(1905,2095,by=10), col = "#DDDDFF")
      abline(h=seq(1240,1480,by=40),v=seq(1900,2100,by=10), col = "#99AA99")
    },
    get_cm_distance = function(t,y2){
      solar_system <- SolarSystem$new("earth")
      otherplanetlist <- solar_system$other_planets
      sun <- otherplanetlist[[1]]
      rows <- dim(df_distance)[1]
      par(mar=c(5.1,6,4.1,2.1))
      
      xlims <- c(0,12)
      ylims <- c(146,154)
      
      xlabs <- seq(-20, 20, by=2)
      ylabs <- seq(-140, 160, by=2)
      samples <- seq(1,rows,by=16)
      len <- length(samples)
      divisor <- rep(10^9,len)
      divisor2 <- rep(31558119,len)
      x <- t[samples]/divisor2
      y <- y2[samples]/divisor
      plot(x,y,lwd=2,pch=19,cex=0.7,
           xlim=xlims, ylim=ylims, xlab=" ", ylab=" ",
           main="Solar System with Jupiter and Earth",
           col.main="#008800",
           xaxt="n", yaxt="n"  ,
           cex.main=1.5, cex.sub=1.5)
      axis(1, at=xlabs,labels=xlabs, col.axis="black", cex.lab=1.5)
      axis(2, at=ylabs,labels=ylabs, col.axis="black", las=2, cex.axis=1.2)
      mtext("Time (yrs)", side=1, line=2.5, cex=1.4,col="#008800")
      mtext("millions of km", side=2, line=3, cex=1.4,col="#008800")
      mtext("Earth Distance to Center-of-Mass of Solar System", side=3, line=-0, cex=1.2,
            col="#008800")
      abline(h=seq(-145,155,by=1),v=seq(-20,20,by=1), col = "#DDDDFF")
      abline(h=seq(-146,154,by=2),v=seq(-20,20,by=2), col = "#99AA99")
      }
    )
)

## Class 9: Controller

OrbitController <- R6Class("OrbitController", 
    list(
      v0 = NA,
      aphelion = NA,
      data = NULL, 
      plot = NULL,
      period = NA,
      summary = NA,
      start_time = 0,
      jupiter_start = NULL,
      result = NA,
      initialize = function(planet0="earth", sample_rate=sample_rate,
                            decimator=8, optimized=FALSE,
                            loops = 8, v0_jupiter=12423, v0_earth=29298.5925,
                            revolutions = 1,calibrate=TRUE){
      period <- 365.256363 # period 398.88  11.862 yr
      if (optimized==TRUE){
          loops <- loops - 1
          if (loops < 1){
             cat("For optimization, at least two loops will be calculated.")
          }
      } else {
          cat("One loop will be calculated. Optimized is set to FALSE.")
      }
      # Sample_rate: Number of discrete sample points in one period
      solar_system <- SolarSystem$new(planet=planet0,
                                      v0_jupiter=v0_jupiter, v0=v0_earth,
                                      calibrate=calibrate)
      if (planet0 == "earth"){
          planet <- solar_system$earth
          planet1_name <- solar_system$jupiter$name
          v0 <- v0_earth
          theta0 <- planet$theta
          planet_yr <- planet$year
          planet1_yr <- solar_system$jupiter$year
          v0_planet1 <- solar_system$jupiter$v0
          theta1 <- solar_system$jupiter$theta
          heading0_planet1 <- solar_system$jupiter$heading0
          planet$set_v0(v0_earth)
      }
      if (planet0 == "jupiter"){
          planet <- solar_system$jupiter
          planet1_name <- solar_system$earth$name
          v0 <- v0_jupiter
          theta0 <- planet$theta
          planet_yr <- planet$year
          planet1_yr <- solar_system$earth$year
          v0_planet1 <- solar_system$earth$v0
          theta1 <- solar_system$earth$theta
          heading0_planet1 <- solar_system$earth$heading0
          planet$set_v0(v0_jupiter)
      }
      printv0 <- formatC(v0, digits = 4, format = "f")
      print1v0 <- formatC(v0_planet1, digits = 4, format = "f")
      cat("Initial velocity for ",planet0," = ",printv0,"m/s, initial_heading = ",
                                  planet$heading0,"degrees\n")
      cat("Initial velocity for ",planet1_name," = ",print1v0,"m/s, initial_heading = ",
            heading0_planet1,"degrees\n")
      v0_vec <- c(v0)
      ap0 <- planet$aphelion
      orbitdata <- OrbitData$new(solar_system, planet,
                                    planet_name= planet0, v0=v0,
                                    sample_rate=sample_rate,
                                    decimator=decimator,
                                    revolutions=revolutions)
      df <- orbitdata$data
      analysis <- OrbitAnalysis$new(planet, df=df, theta0, optimized=optimized)
      p <- analysis$period
      period_vec <- c(p)
      ap0_vec <- c(ap0)

      if (optimized==TRUE && loops > 0){
         for(i in 1:loops) {
            solar_system <- SolarSystem$new(planet=planet0,v0_jupiter=v0_jupiter,
                                            v0_earth=v0_earth,
                                            calibrate=calibrate)
            if (planet0 == "earth"){
              planet <- solar_system$earth
            }
            if (planet0 == "jupiter"){
              planet <- solar_system$jupiter
            }
            v0 <- self$get_new_v0(planet,v0,p)
            planet$set_v0(v0)
            printv0 <- formatC(v0, digits = 4, format = "f")
            cat("Initial velocity for ",planet0," = ",printv0,"m/s, initial_heading = ",
                planet$heading0,"degrees\n")
            result_temp <- OrbitData$new(solar_system, planet,
                                         planet_name= planet0, v0=v0,
                                         sample_rate=sample_rate,
                                         decimator=decimator,
                                         revolutions=revolutions)
            df <- result_temp$data
            analysis <- OrbitAnalysis$new(planet, df, theta0, optimized=optimized)
            p <- analysis$period
            v0_vec <- c(v0_vec,v0)
            period_vec <- c(period_vec,p)
            ap0_vec <- c(ap0_vec,ap0)

          } ## for loop
          self$period <- analysis$period
          self$aphelion <- ap0_vec
          self$period <- period_vec
        }  else  {
          analysis <- OrbitAnalysis$new(planet, df=df, theta0, optimized=optimized)
          self$plot <- analysis$orbit_plot
        } ## end optimized conditional block
        self$summary <- self$get_orbit_summary(analysis$orbit_data,
                                               planet_yr, theta0,theta1,
                                               planet1_yr)
        self$jupiter_start <- self$get_jupiter_start(analysis$orbit_data)
        self$result <- analysis
        self$v0 <- v0_vec
        self$data <- analysis$orbit_data
      },
      get_new_v0 = function(planet,v0,period0){
        planet_yr <- planet$year
        period_undershoot <- (planet_yr - period0)
        fraction <- period_undershoot / planet_yr
        period_factor <- 1 - fraction
        v0 / (period_factor^0.25)
      },
      get_new_aphelion = function(planet,period0){
        planet_yr <- planet$year
        period_undershoot <- (planet_yr - period0)
        fraction <- period_undershoot / planet_yr
        period_factor <- 1 - fraction
        planet$aphelion / (period_factor^0.02)
      },
      get_orbit_summary = function(df, planet_year, theta0,theta1,
                                   planet1_year){
        year_parameters <- c("year_length", "angular_speed")
        rows <- dim(df)[1]
        ## yr 31558119 seconds
        time <- df$time - df$time[1]
        yrvec <- rep(planet_year,rows)
        yrvec1 <- rep(planet1_year,rows)
        yearlogic <- (time < yrvec)
        yearlogic1 <- (time < yrvec1)
        record_minus <- sum(yearlogic )
        record_plus <- record_minus + 1
        trimmed_row_count <- record_plus + 1
        dftrimmed <- df[1:trimmed_row_count,]
        
        seconds_yr <-  dftrimmed$time[record_plus]
        degrees_year1 <-  dftrimmed$planet_angle[record_minus]
        degrees_year2 <-  dftrimmed$planet_angle[record_plus]
        if(degrees_year1 < 180){
          degrees_year1 <- degrees_year1 + 360
        } 
        angular_speed1 <- degrees_year1
        
        if(degrees_year2 < 180){
          degrees_year2 <- degrees_year2 + 360
        } 
        angular_speed1 <- degrees_year1 - theta0
        angular_speed2 <- degrees_year2 - theta0
        year_length <- seconds_yr
        planet_year_data <- data.frame(year_length,  angular_speed1, angular_speed2)
        summary_names <- c("Aphelion", "Perihelion")
        
     ## planet
        d_planet_sun <- df$d_planet_sun
        d_planet1_sun <- df$d_planet1_sun
        d_planet <- df$d_planet
        d_planet1 <- df$d_planet1
        
        len <- length(d_planet_sun)
        maxsys <- max(d_planet[50:len])/10^9
        minsys <- min(d_planet)/10^9
        
        minsun <- min(d_planet_sun)/10^9
        maxsun <- max(d_planet_sun)/10^9
        
        planet <- c(maxsys, minsys)
        planet_Sun <- c(maxsun, minsun)
        planet_data <- data.frame(summary_names, planet, planet_Sun)
        
        maxsys1 <- max(d_planet1[50:len])/10^9
        minsys1 <- min(d_planet1)/10^9
        minsun1 <- min(d_planet1_sun)/10^9
        maxsun1 <- max(d_planet1_sun)/10^9
        
        planet1 <- c(maxsys1, minsys1)
        planet1_sun <- c(maxsun1, minsun1)
        planet1_data <- data.frame(summary_names, planet1,planet1_sun)
        
        ###########################################################
        # planet_year <-   4332.589 *  3600 * 24
        # 365.256 days
        # 31558119 seconds

        year_parameters <- c("year_length", "angular_speed")
        record_minus1 <- sum(yearlogic1)
        if (record_minus1 < (rows - 3)) {
          record_plus1 <- record_minus1 + 1
          seconds_yr <-  df$time[record_plus1]
          degrees_year1 <-  df$planet1_angle[record_minus1] - theta1
          degrees_year2 <-  df$planet1_angle[record_plus1] - theta1
          if(degrees_year1 < 180){
            degrees_year1 <- degrees_year1 + 360
          } 
          angular_speed1 <- degrees_year1
          if(degrees_year2 < 180){
            degrees_year2 <- degrees_year2 + 360
          } 
          angular_speed1 <- degrees_year1
          angular_speed2 <- degrees_year2
          year_length <- seconds_yr
          planet1_year_data <- data.frame(year_length,  angular_speed1, angular_speed2)
          
          list(planet_data, planet1_data,planet_year_data, planet1_year_data)
          
        } else {
          list(planet_data, planet1_data,planet_year_data)
        }
       
      },
      get_jupiter_start = function(df){
        earth_ap_2019_secs <- as.numeric(as.POSIXct("2019-07-05 00:00:00 GMT", origin="1970-01-01",tz="GMT")   )
        ## 99-year average
        ap2050secs <- 2540645455
        earth_ap2050 <- as.POSIXct(x=ap2050secs,tz="GMT",origin="1970-01-01 00:00:00")
        earth_ap_2050_secs <- as.numeric(as.POSIXct(earth_ap2050, origin="1970-01-01 00:00:00",tz="GMT"))
        earth_ap_1957_secs <- ap2050secs + (1957-2050)*31558118
        earth_ap_1958_secs <- ap2050secs + (1958-2050)*31558118
        earth_ap_1959_secs <- ap2050secs + (1959-2050)*31558118
        earth_ap_1960_secs <- ap2050secs + (1960-2050)*31558118
        earth_ap_2020_secs <- ap2050secs + (2020-2050)*31558118
        earth_ap_2018_secs <- ap2050secs + (2018-2050)*31558118
        
        # earth_ap1957 <- as.POSIXct(x="1957-07-04 03:04:44 GMT",
        #                            tz="GMT",origin="1970-01-01 00:00:00")
        # Sidereal orbit period (days)	4,332.589	365.256	11.862
        
        jup_year <- 11.862*365.256363     #  374342400
        jup_year_secs <- 24*3600* jup_year
        jup_ap_2017_secs <- as.numeric(as.POSIXct("2017-02-17 07:17:00 GMT",
                                       origin="1970-01-01",tz="GMT"))
        # jup_ap_2017 <- as.POSIXct(x=jup_ap_2017_secs,
        #                           tz="GMT",origin="1970-01-01 00:00:00")
        
        jup_ap_1957_secs <- jup_ap_2017_secs + (-5)*jup_year_secs 
        jup_ap_1969_secs <- jup_ap_2017_secs + (-4)*jup_year_secs 
        jup_ap_1981_secs <- jup_ap_2017_secs + (-3)*jup_year_secs 
        jup_ap_1993_secs <- jup_ap_2017_secs + (-2)*jup_year_secs 
        jup_ap_2005_secs <- jup_ap_2017_secs + (-1)*jup_year_secs 
        jup_ap_2017_secs <- jup_ap_2017_secs + (0)*jup_year_secs 
        jup_ap_2028_secs <- jup_ap_2017_secs + (1)*jup_year_secs 
        jup_ap_2040_secs <- jup_ap_2017_secs + (2)*jup_year_secs 
        jup_ap_2052_secs <- jup_ap_2017_secs + (3)*jup_year_secs 

        rows <- dim(df)[1]
        dfanys <- df[10:rows,]
        rows2  <- dim(dfanys)[1]
        timevec <- rep(earth_ap_1958_secs, rows2)  #  -362701401
        adjusted_time_vec <- dfanys$time + timevec
        dfanys[,"time"] <- adjusted_time_vec
        
        dt <- dfanys[2,c("time")] - dfanys[1,c("time")]
        timerec <- dt * 69894 
        t <- earth_ap_1958_secs
        
        rows_logic <- rows2 - 2
        logic <- rep(FALSE, rows2) 
        jup_aphelion_logic <- rep(FALSE, rows2)
        earth_aphelion_logic <- rep(FALSE, rows2) 
        planet_ap_logic <- rep(FALSE, rows2)
        planet1_ap_logic <- rep(FALSE, rows2)
        planet_peri_logic <- rep(FALSE, rows2)
        planet1_peri_logic <- rep(FALSE, rows2)
        earth1959 <- rep(FALSE, rows2) 
        jup1969 <- rep(FALSE, rows2) 
        for(i in 1:rows_logic){
          t <- t + dt
          j <- i + 1
          k <- i + 2
          planet_ap_logic[j] <-  dfanys$d_planet[i] < dfanys$d_planet[j]  &&  dfanys$d_planet[j] > dfanys$d_planet[k] 
          planet1_ap_logic[j] <-  dfanys$d_planet1[i] < dfanys$d_planet1[j]  &&  dfanys$d_planet1[j] > dfanys$d_planet1[k] 
          planet_peri_logic[j] <-  dfanys$d_planet[i] > dfanys$d_planet[j]  &&  dfanys$d_planet[j] < dfanys$d_planet[k] 
          planet1_peri_logic[j] <-  dfanys$d_planet1[i] > dfanys$d_planet1[j]  &&  dfanys$d_planet1[j] < dfanys$d_planet1[k] 
          
          jup_aphelion_logic[j] <-  dfanys$time[i] < jup_ap_1957_secs &&  dfanys$time[j] > jup_ap_1957_secs
          jup1969[j] <-  dfanys$time[i] < jup_ap_1969_secs &&  dfanys$time[j] > jup_ap_1969_secs
                    # 5.211676
          earth_aphelion_logic[j] <-  dfanys$time[i] < earth_ap_1958_secs  &&  dfanys$time[j] > earth_ap_1958_secs
          earth1959[j] <-  dfanys$time[i] < earth_ap_1959_secs  &&  dfanys$time[j] > earth_ap_1959_secs
        }
        dfanys[,"planet_ap_logic"] <- planet_ap_logic
        dfanys[,"planet1_ap_logic"] <- planet1_ap_logic
        dfanys[,"planet_peri_logic"] <- planet_peri_logic
        dfanys[,"planet1_peri_logic"] <- planet1_peri_logic
        
        dfanys[,"earth_aphelion_logic"] <- earth_aphelion_logic
        dfanys[,"earth1959"] <- earth1959
        dfanys[,"jup_aphelion_logic"] <- jup_aphelion_logic
        dfanys[,"jup1969"] <- jup1969

        earth_aphelion <- subset(dfanys, earth_aphelion_logic==TRUE , 
                  select = c("time",
                             "d_sun","sun_angle","sun_vx", "sun_vy", "sun_vz",
                             "d_planet1","planet1_angle",
                             "planet1_vx", "planet1_vy", "planet1_vz",
                             "d_planet","planet_angle",
                             "planet_vx", "planet_vy", "planet_vz"))
        jup_aphelion <- subset(dfanys, jup_aphelion_logic==TRUE , 
                  select = c("time",
                             "d_sun","sun_angle","sun_vx", "sun_vy", "sun_vz",
                             "d_planet1","planet1_angle",
                             "planet1_vx", "planet1_vy", "planet1_vz",
                             "d_planet","planet_angle",
                             "planet_vx", "planet_vy", "planet_vz"))
        planet1_aphelion <- subset(dfanys, planet1_ap_logic==TRUE , 
                               select = c("time",
                                          "d_sun","sun_angle","sun_vx", "sun_vy", "sun_vz",
                                          "d_planet1","planet1_angle",
                                          "planet1_vx", "planet1_vy", "planet1_vz",
                                          "d_planet","planet_angle",
                                          "planet_vx", "planet_vy", "planet_vz"))
        planet_aphelion <- subset(dfanys, planet_ap_logic==TRUE , 
                                  select = c("time",
                                             "d_sun","sun_angle","sun_vx", "sun_vy", "sun_vz",
                                             "d_planet1","planet1_angle",
                                             "planet1_vx", "planet1_vy", "planet1_vz",
                                             "d_planet","planet_angle",
                                             "planet_vx", "planet_vy", "planet_vz"))
        
        planet1_perihelion <- subset(dfanys, planet1_peri_logic==TRUE , 
                                   select = c("time",
                                              "d_sun","sun_angle","sun_vx", "sun_vy", "sun_vz",
                                              "d_planet1","planet1_angle",
                                              "planet1_vx", "planet1_vy", "planet1_vz",
                                              "d_planet","planet_angle",
                                              "planet_vx", "planet_vy", "planet_vz"))
        planet_perihelion <- subset(dfanys, planet_peri_logic==TRUE , 
                                  select = c("time",
                                             "d_sun","sun_angle","sun_vx", "sun_vy", "sun_vz",
                                             "d_planet1","planet1_angle",
                                             "planet1_vx", "planet1_vy", "planet1_vz",
                                             "d_planet","planet_angle",
                                             "planet_vx", "planet_vy", "planet_vz"))
        
        earth_1959 <- subset(dfanys, earth1959==TRUE , 
                                 select = c("time",
                                            "d_sun","sun_angle","sun_vx", "sun_vy", "sun_vz",
                                            "d_planet1","planet1_angle",
                                            "planet1_vx", "planet1_vy", "planet1_vz",
                                            "d_planet","planet_angle",
                                            "planet_vx", "planet_vy", "planet_vz"))
        jup_1969 <- subset(dfanys, jup1969==TRUE , 
                               select = c("time",
                                          "d_sun","sun_angle","sun_vx", "sun_vy", "sun_vz",
                                          "d_planet1","planet1_angle",
                                          "planet1_vx", "planet1_vy", "planet1_vz",
                                          "d_planet","planet_angle",
                                          "planet_vx", "planet_vy", "planet_vz"))
        earth <- as.POSIXct(earth_aphelion[1,"time"],origin="1970-01-01",tz="GMT")
        jupiter <- as.POSIXct(jup_aphelion[1,"time"],origin="1970-01-01",tz="GMT")
        earth1959ap <- as.POSIXct(earth_1959[1,"time"],origin="1970-01-01",tz="GMT")
        jup1969ap <- as.POSIXct(jup_1969[1,"time"],origin="1970-01-01",tz="GMT")
        date_list <- list(earth,earth1959ap,jupiter, jup1969ap )
        list(earth_aphelion, jup_aphelion,planet_aphelion,planet_perihelion,
             planet1_aphelion,planet1_perihelion,date_list)
      }
   )
)

########################################################################

## Generate Simulated Data for Solar System Orbits

########################################################################

## Explanation

## The function named 'run_orbit_analyzer' run to generate a csv file
## of the simulation results. There are examples after the below.

## For this three-body solution of the sun, Jupiter, and earth, the sun has a
## unique role because its mass dominates the system. As a result, either
## Jupiter or earth can be selected as the planet and the other selected
## as 'planet1'. In a future solution of four or more planets, selections for 
## planet-2, Planet-3, and so on would also be made. To avoid calculating the 
## position, velocity, and direction of all of the planets simultaneously, 
## The initial condition are established at the aphelion of the orbit where the
## location is well established and the velocity vector points in a
## direction perpendicular to the position vector. The initial velocity
## is set so that the period of the orbit agrees with the well known year.

## To use this program, 'planet1' is set at a known approximate aphelion 
## location and time.  In the case of Jupiter as 'planet1',
## an aphelion on October 26, 1957 at 1957-10-26 23:16:58 GMT,
## corresponding to 384,396,182 seconds before 1970-01-01 00:00:00 is used
## by the program.

## The sun and 'planet1' are initially set so that their 
## common center of mass is at the origin of the coordinate system. 
## In a future solution of four or more planets, selections for 
## planet-2, Planet-3, and so on would also be made.  To complete the initial
## conditions, the earth is reset at the time of its 
## aphelion in July of 1958 which perturbs the center of mass of three body
## system by a small amount.

## As the number of sample points increases, the position of the earth
## after exactly one year appear to approach the initial location. It would
## actually require a very large number of sample points to study how accurate
## this might be and whether the orbit might have some instability. It appears 
## to require around 10,000 sample points per year to study how Jupiter 
## perturbs the orbit of the earth over the period from 1958 to 2030. 
## Uncertainty regarding the initial position and velocity of the sun and
## Jupiter further constrains how better accuracy with more sample points
## would improve this analysis.  

#############################################################################

## Function 1: run_orbit_analyzer

#############################################################################

## Examples of running the function named 'run_orbit_analyzer' to generate
## a csv file of simulation results.

## By setting optimized to True and loops to 2 or more, the simulation will
## operate in a repeating loop with a corrected initial velocity that 
## converges on an orbital period of the planet year. When the orbital period
## is considered close enough to one year, the simulation can be run with
## optimized=FALSE and loops=1. Setting revolutions=12 will simulate the earth
## orbit over one period of Jupiter's period which is just under twelve years.


## The parameters v0_earth and v0_jupiter accept the magnitude of the initial
## planet velocity in meters per second.

run_orbit_analyzer <- function(planet="earth",sample_rate=6000,
                               optimized=FALSE,loops=8,
                               v0_jupiter=12413.9, v0_earth=29298.5925,
                               revolutions= 1,calibrate=TRUE){
  setwd("C:/Users/merch/OneDrive/Documents/physics/earthScience/jupiter")
  
  pie <- 3.1415926535897932385
  two_pie <-  2 * 3.1415926535897932385
  solar_system <- SolarSystem$new(planet="earth",
                                  v0_jupiter=v0_jupiter, v0_earth=v0_earth,
                                  calibrate=calibrate)
  if(optimized == TRUE){
    analyzer <- OrbitController$new(planet0=planet, sample_rate=sample_rate,
                                    optimized = TRUE,loops=loops,
                                    v0_jupiter=v0_jupiter, v0_earth=v0_earth,
                                    revolutions=revolutions, calibrate=calibrate)
    df <- analyzer$data
    write.csv(df,
              "junk2testearth_sample_rate72000loops3jup12413r9e29310rev36.csv",
              row.names = FALSE)
    list(analyzer$data,analyzer$summary, analyzer$period, analyzer$v0,analyzer$jupiter_start)
  } else {
    analyzer <- OrbitController$new(planet0=planet, sample_rate=sample_rate,
                                    optimized = FALSE,
                                    v0_jupiter=v0_jupiter, v0_earth=v0_earth,
                                    revolutions =  revolutions,loops=loops,
                                    calibrate=calibrate)
    df <- analyzer$data
    write.csv(df,
              "junk2earth_sample_rate8000e29301jup12409rev72.csv",
              row.names = FALSE)
    analyzer$plot
    list(analyzer$data,analyzer$summary, analyzer$v0,
         analyzer$jupiter_start)
  }
}

#############################################################################

## Examples of Running Simulation:

#############################################################################

# As the final simulation is exceedingly long, start with Example-2 for an
# introduction. As you develop confidence in the operation and results,
# increase the sample rate and number of orbit revolutions before attempting
# the formal results. This will also permit an estimate of the computation time
# the final result. 

# Example-1: Check Program Setup

# result <- run_orbit_analyzer(planet="earth", sample_rate=3000,v0_earth=29317,
#                              optimized=TRUE, loops = 2, v0_jupiter=12409,
#                              revolutions=1)

# Example-2:

# Set optimized=TRUE for optimization loop to help estimate initial planet
# velocity in meters per second at aphelion orbit position. This might have a
# running time over 30-minutes.

# Optimize initial velocity: 
# run_orbit_analyzer <- function(planet="earth",sample_rate=6000,
#                                optimized=FALSE,loops=8,
#                                v0_jupiter=12413.9, v0_earth=29298.5925,
#                                revolutions= 1,calibrate=TRUE)

# Example-3:

# Short final result simulation without Optimization loop.

# result <- run_orbit_analyzer(planet="earth", sample_rate=1000,optimized=FALSE,
#                              v0_earth=29301, v0_jupiter=12409,revolutions=1,
#                              loops=1)

# Example-4:

# For this example, Jupiter is added last as the planet. Set optimized=TRUE
# for optimization loop to help estimate initial velocity of Jupiter
# in meters per second at aphelion orbit position.

# result <- run_orbit_analyzer(planet="jupiter", sample_rate=48000,
#                              v0_earth=29303,
#                              optimized=TRUE, loops = 2, v0_jupiter=12413,
#                              revolutions=2)

# head(result[[1]])
# result[[2]]
# result[[3]]
# result[[4]]

#############################################################################

# Duplicate Final Simulation

#############################################################################

## This might have a running time over 5-hours.

# result <- run_orbit_analyzer(planet="earth", sample_rate=8000,optimized=FALSE,
#                               v0_earth=29301, v0_jupiter=12409,revolutions=72)

# Setting revolutions=72 will simulate the earth and Jupiter
# orbits over the 72 year period from 1958 to 2030.

#############################################################################

## Data Processing

#############################################################################

DailyOrbitSummary <- R6Class("DailyOrbitSummary", 
    list(
        raw_orbit_file_name=NULL,
        daily_orbit_data_frame = NULL,
        decimator=8,
        optimized=FALSE,
        initialize = function(
           raw_orbit_file_name="solar_system_data.csv",
           daily_orbit_data_frame = data.frame(),
           decimator=decimator, optimized=FALSE){
          
              self$decimator <- decimator
              self$optimized <- optimized
              self$raw_orbit_file_name <- raw_orbit_file_name 
              self$daily_orbit_data_frame <-  
                self$generate_daily_irradiation_records(sample_rate=8000, 
                                                     decimator=8)
        },
        ## Generate data frame of daily summary solar irradiation records
        ## processing results from run_orbit_analyzer function
        ## If there were 8000 samples per year, the sample rate corresponds to
        ## roughly one sample every 66 minutes. Thus a daily file records 
        ## consists of an average of about 22 raw simulation records. For example,
        ## the distance of the earth to the sun for a daily record is the average
        ## of all samples over the date.
        
        generate_daily_irradiation_records = function(sample_rate=8000, 
                                                      decimator=8){
          browser()
          dfresult <- read.csv(self$raw_orbit_file_name,
                               stringsAsFactors=FALSE, row.names = NULL)
          pie <- 3.1415926535897932385
          two_pie <-  2 * 3.1415926535897932385
          ## First record is July 5, 1958 at earth aphelion
          totalrecords <- (length(dfresult$planet_x_coord))/decimator
          fast <- (10 * 1.1 * sample_rate)/decimator + 1
          # [1] 11001  (before decimation [1] 88000)
          orbit1 <- fast + 11.62*sample_rate/decimator + 1
          
          timelt <- as.POSIXlt(x=dfresult[,1],tz="GMT",origin="1958-07-05 01:36:39 GMT")
          ## dfresult$time[1] = 0 = -362,701,401 seconds
          year <- timelt$year + 1900
          month <- timelt$mon + 1
          monthday <- timelt$mday
          
          dfresult[,"Year"] <- year
          dfresult[,"Month"] <- month
          dfresult[,"Day"] <- monthday
          ## Reduce possible multiple daily records to day records
          agg1 <- aggregate(dfresult, by=list(year=dfresult$Year,month=dfresult$Month,
                                              day=dfresult$Day), FUN=mean)
          dates <- as.character(as.Date(floor(agg1$time/86400),origin="1958-07-05 01:36:39 GMT"))
          agg1$Date <- as.character(dates)
          agg1 <- agg1[agg1$year!=1958,]
          # head(agg1)
          daily_solar_data <- agg1[order(agg1$year,agg1$month,agg1$day),]
          daily_solar_data <- daily_solar_data[,c(-34,-35,-36)]
          d_planet_sun <- daily_solar_data[,"d_planet_sun"]
          
          ## Solar Output Power
          ## The instantaneous irradiation impinging upon the earth varies as
          ## the distance between the earth and the sun changes in its orbit
          ## about the center of the solar system. For the simulation, the solar
          ## output power was set so that the average irradiation for earth is
          ## 1361-Watts per square meter based on the effective mean-square
          ## distance of 149.642 million kilometers generated by the simulation
          ## for a 1-year orbit which varies slightly from the 1-AU definition
          ## of 149.59787 million kilometers.
          
          ## The solar output that corresponds to this is 382.98-yotta watts
          ## that corresponds to a former NASA measurement of 1367.6 Watts per
          ## square meter and a sun-earth distance of 1-AU or 149.59787 million
          ## kilometers.
          
          ## Popular Solar Power Figure:
          ## 3.846e+26 = 4 * 1367.6 * 3.14159 * 149.59787^2 * 10^18
          
          ## After correcting for recent NASA measurements of solar radiation
          ## and the simulated results:
            
          ## Average solar irradiation      = 1361 watts per square-meter
          ## Mean-square sun-earth distance = 149,642,243 kilometers
          
          ## Solar Power Figure for Simulation:
          ## 3.8298e+26 = 4 * 1361 * 3.14159 * 149.642243 * 10^18
          
          ## The solar output of around 384.6 yota-Watts is corrected
          ## to 382.983 yota-watts which is close to the estimate of 3.83e26 at
          ## https://www.omnicalculator.com/physics/luminosity
          
          ## Knowing the true solar output would require knowing the
          ## instantaneous distance to the sun at the time of a very accurate
          ## irradiation measurement. Since the distance to the sun is always
          ## changing in a cycle of many years depending on the position of the
          ## sun with respect to Jupiter and Saturn and the position of the
          ## earth within the earth-moon system, there is a small uncertainty
          ## regarding the sun-earth distance at particular times that would be
          ## needed to establish the true solar output power.       
          
          ## Average measured irradiation for earth
          watt_m2 <- 1361  # 1368
          ys1 <- d_planet_sun
          ## Effective average or mean-square sun-earth distance
          msed <- (1/mean(ys1^(-2)))^0.5  # 149642243647
          # mean inverse square is [1] 4.465721e-23
          # Reciprocal is [1] 2.23928e+22
          
          yw <- 3.829803e+26 ## 382.98-yotta watts ## 384.6
          irrad <- yw/(2*two_pie*d_planet_sun^2) 
          ## irrad_mean <- yw/(2*two_pie*msed^2) ## 1361 Watts/ square meter
          ## irrad_mean <- watt_m2*(msed/ys1)^2  
          
          ## AU definition ## 2 * 1367.7 * 2*3.14159 * 149.59787^2 * 10^12 *10^6
          ## 1367.7 https://ui.adsabs.harvard.edu/abs/1981SoPh...74..217W/abstract http://archives.math.utk.edu/ICTCM/VOL25/S048/paper.pdf
          ## 3.846093e+26
          ## 2 * 1367.6 * 2*3.14159 * 149.59787^2 * 10^12 *10^6 [1] 3.84609e+26
          
          daily_solar_data[,"irrad"] <- irrad
          daily_solar_data[,c(34,1:33,35)] 
        }
))

YearOrbitSummary <- R6Class("YearOrbitSummary", 
    list(
        raw_orbit_file_name=NULL,
        daily_orbit_summary = NULL,
        yearly_irradiation_data = NULL,
        year_orbit_file_name = NULL,
        first_year=1959,
        final_year=2018,
        decimator=8,
        optimized=FALSE,
        initialize = function(
          raw_orbit_file_name="solar_system_data.csv",
          daily_orbit_data_frame = data.frame(),
          year_orbit_file_name  = "junkYearly2018SolarsystemData.csv",
          first_year=1959, final_year=2018,
          decimator=decimator, optimized=FALSE){
            
            self$decimator <- decimator
            self$optimized <- optimized
            self$daily_orbit_summary <-
              DailyOrbitSummary$new(raw_orbit_file_name=raw_orbit_file_name,
                                     decimator=decimator, optimized=FALSE)
            self$yearly_irradiation_data <- 
                self$get_irradiation_year_data(daily_solar_system_data=
                              self$daily_orbit_summary$daily_orbit_data_frame,
                first_year= first_year, final_year= final_year,
                new_file_name=year_orbit_file_name)
            self$make_irradiation_year_record_file(year_orbit_data=
                                                self$yearly_irradiation_data,
                                              file_name=year_orbit_file_name)
          },
          ## Function-4: generate_yearly_irradiation_records
          ## Make solar irradiation year record file from
          ## summary data frame on DailyOrbitSummary object.
          make_irradiation_year_record_file = function(year_orbit_data,
                                               file_name="junkIrradData.csv"){
              write.csv(year_orbit_data, file_name, row.names = FALSE)
          },
          get_irradiation_year_data = function(daily_solar_system_data,
                       first_year=1959, final_year=2018,
                       new_file_name="junkYearOrbits") {
                yrvec <- first_year:final_year
                solar_df <- daily_solar_system_data
                YearAverageIrrad <- sapply(yrvec,function(x){
                   mean(solar_df[solar_df$year==x,"irrad"])
                })
                JanAverageIrrad <- sapply(yrvec,function(x){
                   mean(solar_df[solar_df$year==x & solar_df$month==1,"irrad"])
                })
                FebAverageIrrad <- sapply(yrvec,function(x){
                   mean(solar_df[solar_df$year==x & solar_df$month==2,"irrad"])
                })
                MarAverageIrrad <- sapply(yrvec,function(x){
                   mean(solar_df[solar_df$year==x & solar_df$month==3,"irrad"])
                })
                AprAverageIrrad <- sapply(yrvec,function(x){
                   mean(solar_df[solar_df$year==x & solar_df$month==4,"irrad"])
                })
                MayAverageIrrad <- sapply(yrvec,function(x){
                   mean(solar_df[solar_df$year==x & solar_df$month==5,"irrad"])
                })
                JunAverageIrrad <- sapply(yrvec,function(x){
                   mean(solar_df[solar_df$year==x & solar_df$month==6,"irrad"])
                })
                JulAverageIrrad <- sapply(yrvec,function(x){
                   mean(solar_df[solar_df$year==x & solar_df$month==7,"irrad"])
                })
                AugAverageIrrad <- sapply(yrvec,function(x){
                   mean(solar_df[solar_df$year==x & solar_df$month==8,"irrad"])
                })
                SepAverageIrrad <- sapply(yrvec,function(x){
                   mean(solar_df[solar_df$year==x & solar_df$month==9,"irrad"])
                })
                OctAverageIrrad <- sapply(yrvec,function(x){
                   mean(solar_df[solar_df$year==x & solar_df$month==10,"irrad"])
                })
                NovAverageIrrad <- sapply(yrvec,function(x){
                   mean(solar_df[solar_df$year==x & solar_df$month==11,"irrad"])
                })
                DecAverageIrrad <- sapply(yrvec,function(x){
                   mean(solar_df[solar_df$year==x & solar_df$month==12,"irrad"])
                })
                year <- yrvec
                data.frame(year,YearAverageIrrad,
                     JulAverageIrrad, AugAverageIrrad, SepAverageIrrad,
                     OctAverageIrrad, NovAverageIrrad, DecAverageIrrad,
                     JanAverageIrrad, FebAverageIrrad, MarAverageIrrad,
                     AprAverageIrrad, MayAverageIrrad, JunAverageIrrad)
            }
))

###########################################################################

## Combine solar irradiation with weather and lake-level data  

###########################################################################

## Process simulated raw solar data and merge with weather and lake level data.
## Make merged meterological csv data file.
## Option to make csv data file with yearly irradiation records 

## Declaring a DataProcessor object results in generating csv files.

## 1. A solar irradiation csv file of year records with these fourteen columns:

##    The year (1 column)
##    Average solar radiation for the year (1 column)
##    Average solar radiation for January, February, March, ect. (12 columns)

## 2. A merged yearly meteorological weather csv file of year records that
##    includes the fourteen columns of irradiation data with the existing
##    287 columns of weather and lake-level data plus the year column,
##    a total of three hundred columns.

## To declare the DataProcessor object, pass these parameters: 

## weather_file_name:    The file name for the existing weather and lake-levels 
##                       with 287 columns plus the year column.

## raw_orbit_file_name:  The file name for the simulated solar system orbit 
##                       data. The number of daily samples is controlled by
##                       the sample rate, which is 8000 per earth year for
##                       this study. 

## year_orbit_file_name: The file name to be given for a new csv file  
##                       of solar irradiation data with 12 month columns,
##                       a year total column, and a year column.

## merged_file_name:     The file name to be given for a new merged yearly
##                       meteorological weather csv file of year records
##                       that includes the fourteen columns of irradiation
##                       data plus the existing 287 columns and the year
##                       column, a total of three hundred columns.

##########################################################################

DataProcessor <- R6Class("DataProcessor", 
                         list(
                           weather_file_name = NULL,
                           raw_orbit_file_name=NULL,
                           year_orbit_file_name = NULL,
                           year_orbit_summary = NULL,
                           merged_file_name = NULL,
                           first_year=1959,
                           final_year=2018,
                           decimator=8,
                           optimized=FALSE,
                           initialize = function(first_year=1959, final_year=2018,
                                                 weather_file_name="weather_year_records.csv",
                                                 raw_orbit_file_name="solar_system_data.csv",
                                                 year_orbit_file_name  = "junkYearly2018SolarsystemData.csv",
                                                 merged_file_name = "junkYearlyChicagoMeteorlogicWeather.csv",
                                                 decimator=8,optimized=FALSE){
                             
                             self$weather_file_name <-weather_file_name
                             self$merged_file_name <- merged_file_name
                             self$decimator <- decimator
                             self$optimized <- optimized
                             self$year_orbit_summary <-
                               YearOrbitSummary$new(first_year=first_year,
                                                    final_year=final_year,
                                                    raw_orbit_file_name=raw_orbit_file_name,
                                                    year_orbit_file_name=year_orbit_file_name, 
                                                    decimator=decimator, optimized=FALSE)
                             daily_irradiation_data <- 
                               self$year_orbit_summary$daily_orbit_summary$daily_orbit_data_frame
                             yearly_irradiation_data <- 
                               self$year_orbit_summary$yearly_irradiation_data
                             self$make_merged_meteorological_file(weather_file_name=
                                                                    weather_file_name,
                                                                  file_name= merged_file_name,
                                                                  solar_data_frame=daily_irradiation_data)
                           },
                           ## Function-E2: Make merged meteorological data file named
                           ## yearlyChicagoMeteorologicalWeather.csv
                           ## combining weather and lake-level file with solar irradiation 
                           ## Make and save final merged data file for study.
                           
                           make_merged_meteorological_file  = function(weather_file_name,
                                                                       solar_data_frame,
                                                                       file_name="junkYearlyMeteorologicWeather.csv"){
                             browser()
                             year_weather_records <- read.csv(weather_file_name,
                                                              stringsAsFactors=FALSE, row.names = NULL)
                             yearlymeteorological <- year_weather_records
                             solar_df <- solar_data_frame
                             yrvec <- yearlymeteorological$Year
                             
                             YearAverageIrrad <- sapply(yrvec,function(x){
                               mean(solar_df[solar_df$year==x,"irrad"])
                             })
                             JanAverageIrrad <- sapply(yrvec,function(x){
                               mean(solar_df[solar_df$year==x & solar_df$month==1,"irrad"])
                             })
                             FebAverageIrrad <- sapply(yrvec,function(x){
                               mean(solar_df[solar_df$year==x & solar_df$month==2,"irrad"])
                             })
                             MarAverageIrrad <- sapply(yrvec,function(x){
                               mean(solar_df[solar_df$year==x & solar_df$month==3,"irrad"])
                             })
                             AprAverageIrrad <- sapply(yrvec,function(x){
                               mean(solar_df[solar_df$year==x & solar_df$month==4,"irrad"])
                             })
                             MayAverageIrrad <- sapply(yrvec,function(x){
                               mean(solar_df[solar_df$year==x & solar_df$month==5,"irrad"])
                             })
                             JunAverageIrrad <- sapply(yrvec,function(x){
                               mean(solar_df[solar_df$year==x & solar_df$month==6,"irrad"])
                             })
                             JulAverageIrrad <- sapply(yrvec,function(x){
                               mean(solar_df[solar_df$year==x & solar_df$month==7,"irrad"])
                             })
                             AugAverageIrrad <- sapply(yrvec,function(x){
                               mean(solar_df[solar_df$year==x & solar_df$month==8,"irrad"])
                             })
                             SepAverageIrrad <- sapply(yrvec,function(x){
                               mean(solar_df[solar_df$year==x & solar_df$month==9,"irrad"])
                             })
                             OctAverageIrrad <- sapply(yrvec,function(x){
                               mean(solar_df[solar_df$year==x & solar_df$month==10,"irrad"])
                             })
                             NovAverageIrrad <- sapply(yrvec,function(x){
                               mean(solar_df[solar_df$year==x & solar_df$month==11,"irrad"])
                             })
                             DecAverageIrrad <- sapply(yrvec,function(x){
                               mean(solar_df[solar_df$year==x & solar_df$month==12,"irrad"])
                             })
                             yearlymeteorological[,"YearAverageIrrad"] <- YearAverageIrrad
                             yearlymeteorological[,"JulAverageIrrad"] <- JulAverageIrrad
                             yearlymeteorological[,"AugAverageIrrad"] <- AugAverageIrrad
                             yearlymeteorological[,"SepAverageIrrad"] <- SepAverageIrrad
                             yearlymeteorological[,"OctAverageIrrad"] <- OctAverageIrrad
                             yearlymeteorological[,"NovAverageIrrad"] <- NovAverageIrrad
                             yearlymeteorological[,"DecAverageIrrad"] <- DecAverageIrrad
                             yearlymeteorological[,"JanAverageIrrad"] <- JanAverageIrrad
                             yearlymeteorological[,"FebAverageIrrad"] <- FebAverageIrrad
                             yearlymeteorological[,"MarAverageIrrad"] <- MarAverageIrrad
                             yearlymeteorological[,"AprAverageIrrad"] <- AprAverageIrrad
                             yearlymeteorological[,"MayAverageIrrad"] <- MayAverageIrrad
                             yearlymeteorological[,"JunAverageIrrad"] <- JunAverageIrrad
                             
                             write.csv(yearlymeteorological, file_name, row.names = FALSE)
                           }
                         ))


###########################################################################

## Merge all year records for weather, lake-levels, and solar data.

###########################################################################

## Example:

DataProcessor$new(first_year=1959, final_year=2018,
    weather_file_name     = "yearlyChicagoWeatherAndLakeLevels.csv",
    raw_orbit_file_name   = "earth_sample_rate8000e29301jup12409rev72.csv",
    year_orbit_file_name  = "junkYearly2018IrradiationData.csv",
    merged_file_name      = "junkYearlyChicagoMeteorlogicWeather.csv",
    optimized=FALSE)