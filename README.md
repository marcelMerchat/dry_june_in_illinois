# dry_june_in_illinois

The report describes a machine-learning prediction comparing these methods:

A. Ordinary-least-squares
B. Partial-least-squares (PLS)
C. Pre-processing with Principal-Components-Analysis followed by PLS
D. Ridge regression

Report published at https://rpubs.com/marcelMerchat/713373

#  Overview

The Farmer's Almanac advises about "A cold and wet June" and it might seem
foolhardy to improve upon a maxim like this one or try to predict anything
about weather, but this investigation nevertheless considers whether anything
might be gained if we apply machine-learning or artificial intelligence to
weather data records from O'Hare Airport at Chicago, Illinois covering the
years from 1960 to 2018. This analysis certainly shows the difficulties in
weather prediction, but perhaps it shows that the likelihood or probability
of a wet June appears be weakly correlated with snow in February and negative
weak correlation with March temperature and other variables over the previous
eleven months of a yearly weather record.

After exploring the relationship between June rainfall and weather data by
looking for clusters, we develop four algorithms that predict the amount of
rain for the month of June in Section-12. We compare the mean-squared error
for four models using cross-validation in in Section-13, and finally perform
final tests for which the bias, variance, and mean-square-error for each
method is presented in Section-14.

In trying to predict the amount of June rain, this report can only point to
a relatively weak correlation with weather in the preceding months. The data
indicates that the probability of a wet June is slightly correlated with heavy
snow in February and cold weather in March and April. This lowers the
probability of a dry June a little, but the difference between a dry June that
affects crop yields and a less damaging one can be affected by one storm in the
middle of the month. Another way of saying this is that the relatively large
variance in June rain level is more crucial when the predicted total rainfall
is small, particularly below 100-mm, compared to years with more rain. None of
the dry years in the final test have predictions above 100-mm for June rain
while the three wettest year in the training data have predictions above 100-mm
for June rain as shown in the exploratory plot of Figure-6.

The reproducible code for this project and report is shared at this address:

   https://github.com/marcelMerchat/dry_june_in_illinois

# Raw Data

The raw weather data is for Station USW00094846 at O’Hare Airport at
Chicago, Illinois covering the years from 1960 to 2018. The raw data was
processed to make a data frame of year records that also including the level
of Lakes Huron-Michigan and simulated solar irradiation levels. The raw data
for O’Hare Airport was automatically downloaded following the application
programming interface (API) at the National Centers for Environmental
Information (NCEI) for the United States Government.

https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries

The following query parameters were appended to the internet address to fetch
the data:

stations=USW00094846
startDate=1958-01-01, endDate=2019-07-01, format=csv

The raw lake-level data covers the period beginning with the 1958-1959 snow year
and ends with the 2018-2019 snow year where a snow year begins on July 1 and
ends the following calendar year on June 30. The download was saved as a file
named ChicagoData2018.csv.

## Numerical Data Fields

These fields were selected for analysis:

PRCP, Precipitation (tenths of mm)
SNOW, Snowfall (mm)
SNWD, Snow depth (mm)
TMAX, Maximum temperature in tenths of degrees (°C)
TMIN, Minimum temperature in tenths of degrees (°C)
AWND, Average daily wind speed (tenths of meters per second)
WDF2, Direction of fastest 2-minute wind (degrees)
WSF2, Fastest 2-minute wind speed (meters per second)
WSF5, Gust intensity as fastest 5-second wind speed (meters per second)

The raw data was processed to make a more uniform data table or frame where
units as tenths of millimeters or tenths of degrees are simply expressed as
millimeters or degrees respectively.

## Categorical or Incident Report Fields
These fields indicate whether not something occurred on a given day. A field
with two possible values such as a medical diagnosis where a disease is present
or not present is an example of a categorical variables. Another example is the
WT03 field which indicates if thunder was reported on a given day or not.

WT01, Fog, ice fog, or freezing fog (may include heavy fog)
WT03, Thunder
WT05, Hail
WT08, Smoke or haze
WT09, Blowing or drifting snow
WT11, High or damaging winds
WT13, Mist
WT16, Rain (may include freezing rain, drizzle, and freezing drizzle)
WT17, Freezing rain
WT18, Snow, snow pellets, snow grains, or ice crystals

## Other Meterological Data
Although we only use the above weather data from Station USW00094846 at O’Hare
Airport for our final predictions, we also explored if the level of Lakes
Huron-Michigan or solar irradiation levels were correlated with June rain,
but this other meteorological data was eliminated from model-building and
prediction as the airport weather data provides most of the predictive power.

## Lake Huron-Michigan Water level
The Army Corp of Engineering considers Lakes Huron and Michigan as a single body
of water with the same average water level. The monthly mean water level for
Station 9075014 at Harbor Beach, MI and Station 9087044 at Calumet Harbor in
Illinois was automatically downloaded from the NOAA government website with
query parameters to select the years from 1959 until 2019 as well as the IGLD
datum which specifies a water-level correction for the slow elevation change
caused by the ice loading during the ice age thousands of years ago.

The lake levels for the two stations were averaged together for the analysis to
solve the problem of a few missing day reports. The raw downloaded data was
saved using these file names:

9075014 Harbor Beach, MI: “huron_michigan_harbor_beach19592018.csv”
9087044 Calumet Harbor, IL: “huron_michigan_calumet_harbor19592018.csv.”

As only airport weather data was selected for the final models, the dimension
of the solution was reduced by elimination of lake level variables from the
model. However, the lake data is still part of the data frame of all variables.

## Simulated Solar Irradiation Data
Solar data was not selected for the final models despite having some low
correlation with June rain as the weather variables in the airport data have
higher correlation. Since it might be unwarranted to discard the perturbation of
the earth’s orbit and the solar irradiation levels for the earth caused by the
gravitational mass of Jupiter without understanding how insignificant this might
be, the solar data requires further study. The description of the simulated
solar irradiation data is included in the Appendix of the report at
paragraph 15.6.

The code for generating the raw solar data is contained in the file named
SunJupiterEarthSimulation.R. The raw data was processed to produce monthly and
year averages as described below under preparing year records.

## Handling Missing Data

The data quality for the airport and lake stations are very high. However, as
fields were left blank for data entry where zero is obvious such as reports of
blowing snow for July at Chicago and other binary categorical fields, there was
some need convert the blanks to zero for the analysis. Where data is missing for
binary categorical fields, blanks were replaced with 0.

Another type of obvious missing data is wind data for years before wind data
was recorded. In this case, missing data is indicated as NA which helps data
processing to identify missing data versus many other kinds of blank spaces
in files.

## Changes for Modified Data File:

There are separate columns for the year, month, and date as integers.

Temperatures are now indicated in degrees Centigrade. In the original data,
the temperature is multiplied by 10 to eliminate any decimals.
Rain precipitation is converted to millimeters where the raw PRCP field was
expressed in tenths of a millimeter without decimals. For example, 1.2-mm of
precipitation was originally recorded as 12 which may have been helpful for
data entry. In the new data file, 1.2-mm of rain is expressed as 1.2.

The data begins in 1959 when maximum and minimum temperature are first reported
over a full year. Before average wind speed data was reported beginning in 1984,
this data is indicated as NA in the new file. Since peak daily two-minute wind
speed and direction as well as peak 5-second wind gusts are first reported
for a full year in 1997, the missing data before 1997 for these fields are also
indicated by NA. The processed version of the daily raw weather data was saved
as this file:

chicagoWeatherMetricUnits.csv

The code that accomplishes this is in this file:

prepareWeatherAndLakeLevelYearRecords.R

This file also contains other data-processing code for making the file named
yearlyChicagoWeatherAndLakeLevel.csv that is described directly below for
preparing year records.

## Preparing Year Records

Year records consist of snow years beginning in July and ending in June. The
raw daily record data was processed to produce snow year records beginning with
the year designated as 1959 from July 1959 to June 1960 as the first year. For
each raw daily weather variable, a year record is constructed from twenty-two
variables including nineteen daily weather variables, and three lake-level
variables for high, mean, and low average monthly water levels. For each of the
twenty-two variables, the computed year record includes twelve monthly variables
plus a thirteenth variable as the average for the entire year. A year record
consists of 286 columns (13 X 22) plus one variable for the year. As a result,
a data frame with 287 columns was prepared and saved as a file named
yearlyChicagoWeatherAndLakeLevel.csv.

After generating the raw simulated irradiation data, the raw irradiation data
was processed to produce a year record data frame and file that corresponds to
the year records for weather and lake-levels. A year record for irradiation
includes twelve monthly averages plus a thirteenth variable for the year average
plus one variable for the year. The data frame of irradiation year records was
saved as a file named yearlyIrradiationData.csv. The code for generating the
raw irradiation data and year records is in the file named
SunJupiterEarthSimulation.R

The thirteen solar irradiation columns were added to the 287 columns of weather
and lake-levels and the combined file of 300 columns was saved as a file named
yearlyChicagoMeteorologicalWeather.csv. The code for this final merging of data
is at the end of the file named SunJupiterEarthSimulation.R.

## Simulated Solar Irradiation Data
The purpose of this study was to evaluate the small variability of solar
irradiation impinging upon the earth over a number of Jupiter orbits. A
three-body model of the solar system was formed consisting of the sun, Jupiter,
and the earth. This simplified model was selected because these three bodies
and the related moons account for well over 99% of the total mass the solar
system. An improved model requiring more orbit calculations would include Venus
and Saturn. The computer code that generated the simulation is in the file
SunJupiterEarthSimulation.R.

Although the solar output power is fairly constant, the instantaneous
irradiation impinging upon the earth varies as the distance between the earth
and the sun changes in its rather circular but elliptical orbit about the center
of the solar system. The distance between the earth and the center of the solar
system is known to vary from about 147.1-million to 152.1-million kilometers
every year. The investigation considers the larger variance of the distance
between the earth and the sun caused by the small orbit of the sun around the
center of the solar system of very roughly 0.8-million kilometers with a period
roughly the same as Jupiter’s orbit of 11.862 years to a first approximation
for this model which neglects Saturn and the other planets excluded from this
three-body model.

## Dominance of Jupiter among the Planets
If we add up the mass in a NASA-JPL table, Jupiter accounts for approximately
71.2% of the solar system mass outside the sun, but it is responsible for most
of the force applied to the inner solar system consisting mainly of the sun,
Mercury, Venus, and earth lumped together as a system because the Saturn,
Uranus, and Neptune gas giants are further away. As far as Jupiter and the
other gas giants are concerned, the inner rocky planets including Venus, earth,
and the sun are a combined system with a center-of-mass within 1000-km of the
center of the sun. After this approximation, Jupiter accounts for at least
91.4% of the force applied to the sun-venus-earth system; Saturn accounts for
about 8% because it is much further away, Uranus 0.3%. Although these error
terms are large, the calculations can still shed some light about the variation
of solar irradiation upon the earth.

## Sample Points
More sample points would be helpful. The sampling rate was increased to 80,000
samples per year for the first year of earth’s orbit to obtain a more accurate
preliminary year after which the final sampling rate was reset at 8000 samples
per year on July 5th, 1959. The longitudinal position of earth’s orbit advances
about 1 degree at the shift to the final lower sampling rate. Since the computer
could recalculate the forces, velocities, and positions about 2000 times per
minute, 72 years of data was generated in about five hours. The sample rate of
8000 is a compromise between accuracy and what was feasible.

## Initial conditions
Solving the orbit equations would permit one to start the planets simultaneously
with the required velocity and heading necessary to reach aphelion at the proper
times. Indeed, solving the equation would eliminate the need for this entire
project of generating simulated orbit data. The solution of the orbits of a
three-body planetary system has not been found as an equation or in what is
called closed form.

Simulated data collection began on July 5th, 1959 after setting the initial
conditions as follows. The planets were started at aphelion where the direction
of the planet velocity is approximately perpendicular to the position vector to
the center of mass of the solar system. Starting orbits at aphelion leads to a
new problem because the planets do not reach the aphelion of their orbit
simultaneously. For example, while the earth is at aphelion every year in early
July, the calendar date of aphelion for Jupiter varies because the period of
Jupiter’s orbit is 11.862 years. Since, this makes it impossible to set the
initial conditions for all of the planets simultaneously, a process was used to
set the planets at aphelion in a sequential manner. After starting the three
bodies at aphelion at the time of Jupiter’s aphelion in 1957, the earth is
arbitrarily moved and restarted at its own aphelion after a delay of 251 days
on July 5, 1958.

## The Sun and Jupiter
The biggest error in this three-body model of the sun, Jupiter, and earth is
likely the initial position of the sun opposite jupiter to start the simulation.
Consider that two stars of a binary star system pair orbit a common
center-of-mass with the same period. Similar to a binary star pair, we might be
tempted to guess that the sun would orbit the common center-of-mass of the
Jupiter-sun combination somewhat roughly every 11.86 years like Jupiter because
Jupiter contains 71% of the total planet masses and that the center of the solar
system is bounded within some region near the center of mass of the Jupiter-sun
combination subject to the effects of the other planets as known error terms for
this model. The position of the sun is probably not well known beyond this
approximation at this time in the year 2020. Adding Saturn to the model would
help reduce the error concerning the center-of-mass as the combined mass of
Jupiter and Saturn contains 90% of the total planet masses.

Lacking better information, the sun is placed at the aphelion its orbit
simultaneously with Jupiter on October 26, 1957. The position of the Sun is set
opposite Jupiter so that the center-of-mass of the Jupiter-Sun system is
positioned at origin of the coordinate system.

Time of aphelion for Sun-Jupiter System:
October 26, 1957 at 1957-10-26 23:16:58 GMT, corresponding to -384,396,182
seconds before 1970-01-01 00:00:00.

## Earth
After starting Jupiter on October 26, 1957, the position of earth is reset at
aphelion on July 5, 1958 after a delay of approximately 251.097 days at
-362,701,401 seconds.

Aphelion of the earth-moon system in the year 2050 was estimated as the 99-year
average of the earth’s aphelions from 2001 to 2099 using the data from Fred
Espenak at www.Astropixels.com based on JPL DE405. Using this data, the 99-year
mean is in the year 2050 at 2050-07-05 14:50:55 GMT, which corresponds to
2,540,645,455 seconds after 1970-01-01 00:00:00.

Aphelion2050 = 2,540,645,455 seconds;
Going back in time using a sideral year of approximately 365.256 days or
31,558,118 seconds, the corresponding time of Earth’s aphelion on July 5, 1958
at 01:36:39 GMT is as follows:
Aphelion1958 = Aphelion2050 + (1958-2050)*31558118
Aphelion1958 = -362,701,401 seconds

Adding the earth as a third body transforms the stationary center-of-mass of
the sun-Jupiter system at the origin of the coordinate system to a new
center-of-mass which orbits the origin with radius of 0.00045-million
kilometers. As the distance from the earth to the sun varies about 5-million
kilometers, this issue was small enough to proceed with the other analysis.
This problem could be eliminated by a solution of the equation for the
three-body model.

## Solar Irradiation Data:

The file named 'solar_system_data_earth_sample_rate8000v29301jupv12409rev72.csv'
holds the raw simulated orbit data for the three-body system of the sun,
earth, and Jupiter.

- The file name string 'earth_sample_rate8000' means the data was generated
with earth added as the final planet of the system at the time of
earth's aphelion in July 1958. The position and velocity of earth's orbit
was re-calculated 8000 times per earth-year to generate the simulated data.

- The string 'v29301' means the initial velocity of earth
was 29,301 meters per second at aphelion in July 1958.

- The string 'jupv12409' means the initial velocity or aphelion velocity of
Jupiter in was 12,409 meters per second in November of 1957.

- The string 'rev72' means the simulation was performed for 72 earth orbits or
revolutions about the sun.

The number of sample points was increased by a factor of 10 or from 8000 to
80,000 for the first revolution to study how a higher sample rate might refine
earth's orbit.  Since a Jupiter-year is nearly 11.9 times as much as a single
earth year, Jupiter's orbit is more highly sampled.   
