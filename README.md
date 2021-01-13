# dry_june_in_illinois

The reports describes a machine-learning prediction, comparing
ordinary-least-squares, partial-least-squares with
principal-components-analysis pre-processing, and ridge regression.
The raw weather data is from Station USW00094846 at O’Hare Airport at
Chicago, Illinois covering the years from 1960 to 2018. The raw data was
processed to make a data frame of year records that also including the level
of Lakes Huron-Michigan and simulated solar irradiation levels.

Solar Irradiation Data:

The file named 'solar_system_data_earth_sample_rate8000v29301jupv12409rev72.csv'
holds the raw simulated orbit data for the three-body system of the sun,
earth, and Jupiter.

The file name string 'earth_sample_rate8000' means the data was generated
with earth added as the final planet of the system at the time of
earth's aphelion in July 1958. The position and velocity of earth's orbit
was re-calculated 8000 times per earth-year to generate the simulated data.

The string 'v29301' means the initial velocity of earth
was 29,301 meters per second at aphelion in July 1958.

The string 'jupv12409' means the initial velocity or aphelion velocity of
Jupiter in was 12,409 meters per second in November of 1957.

The string 'rev72' means the simulation was performed for 72 earth orbits or
revolutions about the sun. The number of sample points was increased by
a factor of 10 or from 8000 to 80,000 for the first revolution to study how
a higher sample rate might refine earth's orbit.  Since a Jupiter-year is
nearly 11.9 times as much as a single earth year, Jupiter's orbit is more
highly sampled.   
