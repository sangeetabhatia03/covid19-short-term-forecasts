Overview

The objective of this project is to forecast for the daily number of
COVID-19 cases in countries with sustained local transmission. We will
produce forecasts for the week starting Monday every Monday at noon
(London time). The forecasts will be produced by three teams and a
centralised team will also produce ensemble forecasts.


Workflow

- A centralised team will make available cleaned data for all
  countries that have at least 100 cases of COVID-19, and at least
  10 cases in the last week. Note that this means the number of
  countries for which forecasts are produced could vary every
  week. The data will be available in the github repo by Mondays 1
  AM. In addition to the case numbers, we will also provide a serial
  interval distribution, which we strongly recommend is used by all teams.
- Each team produces forecasts of daily incidence, and estimates
  of effective reproduction number and makes them available in a
  standard format (see below).
- The centralised team (Pierre and Sangeeta) will feed these into
  visualisations. They will also use agreed upon model averaging methods to
  produce ensemble forecasts that also feed into visualisation.


Data Formats
 - Input Data: 
   
   In the 'Team.input' folder, a file will be released each week named data_'date'.rds'. The ddate corresponds to     the last Sunday included in the data.
   Reading the file (e.g. 'd <-readRDS('../data/data_2020-03-08.rds')') will create a list including:
   * The last date in the dataset 'date_week_finishing';
   * The thresholds used to select countries with active local transmission, i.e. currently at least 100 cases reported in last 4 weeks (Threshold_criterion_4weeks) and at least 10 cases reported in the last week        (Threshold_criterion_7days)'.
   * A dataframe (I_active_transmission) containing the dates and reported incidence for each country with        active trasnmssion.
   * A vector 'Country' listing the names of the countries with active transmission.
   * Two variables specifying the mean and standard deviation of the serial interval to be used (subject to       change follwing Neil's advice).
   
   A new input file (.rds) will be made available by monday morning of each week.
   
 - Output Data:
   
   This is to be submitted by each team by noon on the monday of each week.
   
   The file should be submitted directly to the 'Team.output' folder or emailed to both pierre and Sangeeta and have a standard form, i.e.  a list saved as .rds, named: 'team name_'Std_results_results_week_end_'date_week_finishing'.rds). The team name should 4 chatacters long. the list should contain:
   * The input data frame 'I_active_transmission' mentionned above.
   * The vector 'Country' mentionned above
   * A data frame named 'Rt_last' containing for each country (column) 10,000 samples of the posterior distribution of the estimated Rt on the last sunday of the dataset.
   * A list named 'Predictons' containing one data frame for each country (named after the country). Each data frame will have 7 columns containing 10,000 samples of the posterior distribution of the predicted incidence (for 7 days, each column named as the date associated with the prediction). 
   
   The folder already contains the ouputs for one model for weeks ending on the 8th and 15th of March.
  
