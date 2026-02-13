# MaisLE_ringing
Repository with data, code and manuscript to reproduce results from "Title of manuscript" with link to preprint []()

## abstract

## content

### data

`/data`: This folder contains all necessary data to run the scripts
- `/data/shp`: shp-files needed for plotting (Borders of Germany and Federal States)  
- `AVONET1.csv`: AVONET dataset used for trait-groups  
- `dat_mod_decade.csv`: Number of individuals (*NInd_pd*) and individual density (*dens_1000hm2*) per *species*, net (*netNr*), site (*site_short*) and decade (*julian_day*)  
- `dat_mod2_decade.csv`: Number of species (*nSpec*) per net (*netNr*), site (*site_short*) and decade (*julian_day*)  
- `dat_net_decade.csv`: information per net (*netNr*), site (*site_short*) and decade (*julian_day*), including *year*, 
distance to field margin (*distance_m*), landscape covers at several scales, weed infestation in % (*prop_weed*), 
height of maize plants (*Hoehe_cm*) and position (*lon*, *lat*)  
- `dat_site.csv`: additional summarized site information including total *effort_hm2*, trapping duration (*dur_h*), 
number of nets (*Nnet*), height of nets (*Hnet_m*), length of nets (*Lnet_m*), area of nets (*Anet_m2*), 
number of days with trapping (*Ndays*), and *area_ha* of the maize field  
- `OSM_attributes.csv`: OSM attributes used to extract open landscape data   
- `spec_info.csv`: species info concerning traits groups derived from Kamp et al. 2020 and the German Red List  

### Code

`/R`: Contains all the `R` code to run the analysis.
- `setup.R`: runs the whole analysis. Model output and predictions are saved in `/output`    
- `net-based.Rmd`: code to run models and make posterior predictions for species and trait-group specific models  
- `net-based_NSpec.Rmd`: code to run models and make posterior predictions for species diversity models  
- `plots_tables.R`: code to produce plots and tables needed for manuscript     
- `utils.R`: helper functions  

`/Stan`: Contains Stan code for the models. Models are saved in `/saved models`  

### Manuscript

`index.qmd`: Manuscript source file.
`/graphs`: Contains figures used in the manuscript and supplement.
