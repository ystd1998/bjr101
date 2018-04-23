Shiny App: Earthquake Distrubution
========================================================
author: Ystd1998
date: 4/18/2018
autosize: true

Latest Earthquakes Worldwide
========================================================

Data source:
<http://ds.iris.edu/seismon/eventlist/index.phtml>.

Can search the Worldwide earthquakes by using:
- Magnitude
- Depth

Earthquakes Data Summary
========================================================





```r
url <- "http://ds.iris.edu/seismon/eventlist/index.phtml"
data <- read_html(url) %>% 
    html_node("table") %>% 
    html_table(fill = TRUE) %>% 
    tbl_df()
names(data)
```

```
[1] "DATE and TIME (UTC)"                                            
[2] "LAT"                                                            
[3] "LON"                                                            
[4] "MAG"                                                            
[5] "DEPTHkm"                                                        
[6] "LOCATION   \n                           (Shows interactive map)"
[7] "IRIS ID  (Other info)"                                          
```

Shinny App UI Introduction
========================================================
- Show Control Panel: Hide or displace following options;
- Magnitude Selection Bar: 
  Select the Earthquake if its magnitude is in the range;
- Depth Selection Bar:
  Select the Earthquake if its depth is in the range;
- Show plate boundary: 
  Show or Hide Earthquake's plates distributions
- Show Data: 
  HShow or Hide the original raw data
  
Conclusion
========================================================  

- Show the global earth quake distribution on real time data;
- Selectvie display the earthquake based on magnitude and/or depth conditions;
- display the earth plate boundary plots;
- display or hide the original raw data
