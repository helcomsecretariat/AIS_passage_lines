# AIS passage lines statistics

The R script available in this repository will help to produce statistics based on Automatic Identification System (AIS) data.
The data used with this script is from the HELCOM AIS data server and the dataset is already harmonized.
The script was developped for the "Shipping Accidents in the Baltic Sea from 2014 to 2017" http://www.helcom.fi/Lists/Publications/Ship-accidents-2014-2017-report.pdf published by HELCOM - the Baltic Marine Environment Protection Commission - in November 2018.




### Prerequisites

- R version 3.4.3 and R Studio 1.0.153
- AIS data already harmonized (script: https://github.com/helcomsecretariat/AIS-data-processing-for-statistics-and-maps/ ). A sample is available in the repository.
- a shape file with the lines defined as polygons. The polygons are used to be able to have the exact time when the ships are crossing it. It is also easier for the computer to process it. They have a rather small width.
- a csv file with ship information (shiptypes). A sample is available in the repository.
- relevant paths to read the AIS data, shp file and csv file with ship information.

## Authors

Florent NICOLAS, HELCOM Secretariat: florent.nicolas@helcom.fi


## License

GNU General Public License v3.0

