# AIDA (Archive of Italian radiocarbon DAtes)

#### Alessio Palmisano, Andrew Bevan, Alex Kabelindde, Neil Roberts & Stephen Shennan 

The archive AIDA provides a collation of 4,010 radiocarbon dates from 947 archaeological sites in Italy  (mainland, including Sicily and Sardinia) spanning from 11,000 to 1500 uncal. yr. BP . These dates have been collected from existing online digital archives, and electronic and print original publications. 

![Fig1](https://user-images.githubusercontent.com/13691742/128021804-165e950c-4488-43a2-bd30-6dec327c9d19.jpg)

The plot below show the Summed Probability Distribution (SPD) generated by using all the calibrated radiocarbon dates (in grey, n=4,010) and those ones from short-lived organic samples (in green, n=1,057).

![spd](https://user-images.githubusercontent.com/13691742/128027134-9ce9b2c6-62c6-4e3d-994d-913143acf173.jpg)

There are several caveats related to the present dataset as the original radiocarbon dates had an inhomogeneous quality of associated information which reflect the diversity of standards in the original sources. For instance, 77% of the dates have information about the sample material (e.g., seed, bone), but only 26% have the material taxa (e.g., Triticum dicoccum, Ovis, etc.).  


## Main Dataset AIDA

The main dataset is to be found within /Palmisano_etal_data_and_code/csv/dates.csv. The csv-file is encoded in ‘UTF-8’. The geographic coordinates are stored as unprojected LatLon coordinate system, WGS84 datum. 


| Datafield | Description 
| :-----------   | :----------------------- | 
| DateID (numeric) | unique identifier for the radiocarbon sample | 
| LabID (character) | unique identifier for the lab’s radiocarbon sample |
|OthLabID (character)| unique alternative identifier for the lab’s radiocarbon sample |
|Problems (character)| problems related to the radiocarbon sample (e.g. missing Lab Id, duplicated Lab Ids, etc.)|
|CRA (numeric)| radiocarbon concentration expressed in years before present (BP)|
|Error (numeric)| Standard error of radiocarbon date in years|
|DC13 (numeric)| d13C values of radiocarbon sample|
|Material (character)| material of the radiocarbon sample|
|Species (character)| species of the radiocarbon sample|
|SiteID (numeric)| unique identifier of the site from which the radiocarbon sample has been collected|
|SiteName (character)| name of archaeological site|
|SiteContext (character)| original archaeological context from which the radiocarbon sample was collected|
|SiteType (character)| type of archaeological site|
|Country (character)| country from which the radiocarbon sample was collected|
|Longitude (numeric)| WGS84 eastings|
|Latitude (numeric)| WGS84 northings|
|LocQual (character)| scale defining the accuracy of the spatial coordinates of radiocarbon samples|
|Source (character)| source from which the radiocarbon samples have been collected|
|Comment (character)| Comments about the issues reported in the field "Problems"|

#### Location Quality Key

A – exact coordinates of radiocarbon sample (centroid of the archaeological site from which was collected)

B- the radiocarbon sample is within a 2km radius’ buffer of the coordinates collected

C -the radiocarbon sample is within a 5km radius’ buffer of the coordinates collected

D- the radiocarbon sample is within a 10 km radius’ buffer of the coordinates collected

E- the radiocarbon sample is within a 20 km radius’ buffer of the coordinates collected


## Major contributing datasets/databases

The literature used to compile the present dataset can be found in the file References.txt. However, NERD benefitted from the following existing digital online archives:

* [BANADORA. Banque Nationale de Données Radiocarbonne pour l'Europe et le Proche Orient, Centre de Datation par le Radiocarbonne.](http://www.arar.mom.fr/banadora/) Lyon: CNRS.  

* [CalPal - The Cologne Radiocarbon Calibration & Palaeoclimate Research Package.](https://uni-koeln.academia.edu/BernhardWeninger/CalPal) Developed by Weninger, B., Jöris, O., and Danzeglocke, U.

* Capuzzo, G., Boaretto, E. and Barceló, J.A., 2014. [EUBAR: A database of 14C measurements for the European Bronze Age. A Bayesian analysis of 14C-dated archaeological contexts from Northern Italy and Southern France.](https://www.cambridge.org/core/journals/radiocarbon/article/abs/eubar-a-database-of-14c-measurements-for-the-european-bronze-age-a-bayesian-analysis-of-14cdated-archaeological-contexts-from-northern-italy-and-southern-france/DF3C690F061B00012963E4ACED54BDD0) Radiocarbon, 56(2), pp.851-869.(http://www.telearchaeology.org/EUBAR/)

* Hinz, M., Furholt, M., Müller, J., Raetzel-Fabian, D., Rinne, C.,  Sjögren, K. G., and Wotzka,H. P., 2012. [RADON - Radiocarbon dates online 2012. Central European database of 14C dates for the Neolithic and Early Bronze Age. Journal of Neolithic Archaeology, 14, 1-4](http://radon.ufg.uni-kiel.de/) 

* [IRPA/KIK. Royal Institute for Cultural Heritage web based Radiocarbon database.](http://c14.kikirpa.be/) Van Strydonck, M. and De Roock, E., 2011. Royal Institute for Cultural Heritage web-based radiocarbon database. Radiocarbon, 53(2), pp.367-370. 

* [ORAU. Oxford Radiocarbon Accelerator Unit online database.](https://c14.arch.ox.ac.uk/database/db.php) Oxford: University of Oxford.


## Licence

The AIDA dataset is made available under the [Creative Common License CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/).

## How to cite
Palmisano, A., Bevan, A., Kabelindde,A., Roberts, N., & Shennan, S., 2021. "AIDA: Archive of Italian radiocarbon DAtes", version 1.0 (3rd August 2021): https://github.com/apalmisano82/AIDA

## Case studies using AIDA

* Palmisano, A., Bevan, A. and Shennan, S., 2017. [Comparing archaeological proxies for long-term population patterns: An example from central Italy.](https://www.sciencedirect.com/science/article/pii/S0305440317301474) Journal of Archaeological Science, 87, pp.59-72.

* Palmisano, A., Bevan, A., & Shennan, S., 2018. [Regional Demographic Trends and Settlement Patterns in Central Italy: Archaeological Sites and Radiocarbon Dates.](http://doi.org/10.5334/joad.43) Journal of Open Archaeology Data, 6(1), p.2. 

* Stoddart, S., Woodbridge, J., Palmisano, A., Mercuri, A.M., Mensing, S.A., Colombaroli, D., Sadori, L., Magri, D., Di Rita, F., Giardini, M., and Mariotti Lippi, M., 2019. [Tyrrhenian central Italy: Holocene population and landscape ecology.](https://journals.sagepub.com/doi/abs/10.1177/0959683619826696) The Holocene, 29(5), pp.761-775.

* Roberts, C.N., Woodbridge, J., Palmisano, A., Bevan, A., Fyfe, R., and Shennan, S., 2019. [Mediterranean landscape change during the Holocene: Synthesis, comparison and regional trends in population, land cover and climate.](https://journals.sagepub.com/doi/abs/10.1177/0959683619826697) The Holocene, 29(5), pp.923-937.

* Palmisano, A., Bevan, A., Kabelindde,A., Roberts, N., and Shennan, S., in press. [Long-Term Demographic Trends in Prehistoric Italy: climate impacts and regionalised socio-ecological trajectories.] Journal of World Prehistory, 34 (3). 

## Grants

The archive AIDA was collated thanks to the following grants:

* [Changing the Face of The Mediterranean: Land cover and population since the advent of farming.](https://www.plymouth.ac.uk/research/centre-for-research-in-environment-and-society-ceres/changing-the-face-of-the-mediterranean-land-cover-and-population-since-the-advent-of-farming) Leverhulme Trust, grant number:RPG-2015-031. Grant holders: Neil Roberts (PI), Andrew Bevan (Co-I), Ralph Fyfe (Co-I), & Stephen Shennan (Co-I). 

![logo](https://user-images.githubusercontent.com/13691742/128041312-cd2969ab-7be7-4660-87e7-0539a36c7b2a.png)
