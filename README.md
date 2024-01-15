# TrafficAccidents

This project was conducted as part of the HarvardX Data Science Capstone Module. The primary goal is to perform a 
predictive analysis of traffic accident that leverages data for Germany provided by the local government 
Landesbetrieb Information und Technik Nordrhein-Westfalen. The objective is to construct a predictive model 
for the severity of accidents to generate insight into common types and causes of severe accidents.

Recognizing that the accident data alone lacked strong predictive power, additional data sources were integrated. 
Population information, geographic and historical weather data, and street information from OpenStretMap were 
retrieved and merged with the dataset, introducing additional variables for the predictive analysis.

To construct a comprehensive model, various modeling techniques, such as oversampling, linear modeling, 
random forest, and singular value decomposition, were combined. This approach aims to enhance the predictive capabilities and provide a more nuanced understanding of accident severity.

Road category, road surface, number of lanes, maximum speed, bidge locations and whether the road is lit is
retrieved from OpenStreetMap. It takes a lot of computing time to identify the roads where the accidents happened. 
Therefore, the information has been retrieved for each accident and pre-processed in script:

  osm_match.R

The resulting data is available under:
  road_all1.csv and
  road_all2.csv

loading of other data, data analysis and modeling are performed in script 
  TrafficAccidents.R

This script alco includes R markdown code to generate the R markdown file
  TrafficAccidents.Rmd
  
and the final report
  TrafficAccidents.pdf

  
