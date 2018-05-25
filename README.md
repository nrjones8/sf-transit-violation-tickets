This repo contains data used in writing about how citations from SF MUNI fare inspectors relate to the weather on a given day - that topic is discussed in more detail here: http://nrjones8.me/more-muni-citations-on-rainy-days.html

All analysis code used in that post can be found in `ticket_analysis.R`.

## Datasets in this repo
* `data/raw/muni_average_weekday_boardings.csv` - Average weekday boardings, by month. Downloaded from https://www.sfmta.com/reports/muni-average-weekday-boardings
* `data/raw/order_1327523_noaa_2017_01_01_to_2018_04_23.csv` - Data about historical daily weather in San Francisco from the NOAA
* `data/raw/P000340_042418_Transit_Violation_Tickets_March_2017_through_March_2018.xlsx` - individual citation data, obtained from SFMTA via public records request. This is the original, unchanged data.
* `data/P000340_042418_Transit_Violation_Tickets_March_2017_through_March_2018.csv` - individual citation data, obtained from SFMTA. This is the CSV version of what was originally provided (the `.xlsx` file mentioned above).