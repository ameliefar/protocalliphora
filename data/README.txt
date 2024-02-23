Folder "data" encompasses all datasets to run the analyses and figures. There are three datasets. 
For each dataset, is provided a general description and the details for each associated variable

#nestling_condition.csv
A csv table gatehring the dataset for the analyses of nestling body condition and parasite load (pre-analyses).
csv table with 8 variables and 3263 observations

- indID: character; individual identity;
- broodID: character; nestbox where the individual was laid and fledged; coined from study site ("pir" or "tua") and nestbox number; 
- year: character; from 2004 to 2021; year corresponding to measurements and parasite load quantification;
- hatch_size: integer; number of hatchlings in the brood;
- par_load: integer; parasite load for the whole brood;
- relative_par_load: real; ratio of parasite load in the brood divided by number of hatchlings in the brood;
- tarsus: real; measurement of the tarsus, alternative method following ESF guidelines, usually when the nestling is 15 days old;
- mass: real; body mass of the nestling when it is about 15 days old


#recruit_color.csv
A csv table gathering the dataset for the analyses relating plumage colour as a first year breeder to parasite load as a nestling
csv table with 13 variables and 130 observations

- indID: character; individual identity;
- sex: character; two possible values: 
	- "M" for male
	- "F" for female
- broodID: character; nestbox where the individual was laid and fledged; coined from study site ("pir" or "tua") and nestbox number; 
- year: character; from 2004 to 2021; year when the individual was a nestling and parasite load was quantified;
- hatch_size: integer; number of hatchlings in the brood;
- par_load: integer; parasite load for the whole brood;
- relative_par_load: real; ratio of parasite load in the brood divided by number of hatchlings in the brood;
- spl_period: character; period of feather collection; two possible values:
	- "C" indicates feathers were collected when potential breeders start constructing nests, in early spring;
	- "F" indicates feathers were collected when breeders feeding (10 to 15 days old) nestlings, later in spring
- BB: real; color variable corresponding to mean brightness from the blue crown
- BUVC: real; color variable corresponding to UV-chroma from the blue crown
- YB: real; color variable corresponding to mean brightness from the yellow breast patch
- YUVC: real; color variable corresponding to UV-chroma from the yellow breast patch
- YC: real; color variable corresponding to yellow chroma from the yellow breast patch

#adult_color.csv
A csv table gathering the dataset for the analyses relating plumage colour of breeders to the parasite load of their brood the previous year
csv table with 14 variables and 551 observations

- indID: character; individual identity;
- sex: character; two possible values: 
	- "M" for male
	- "F" for female
- min_age: integer; minimum age of the bird (age in year 2 when sampling feathers); varies from 2 to 8
- broodID: character; nestbox from year 1 (when quantifying parasite load); coined from study site ("pir" or "tua") and nestbox number; 
- year: character; from 2004 to 2021; corresponds to "year 1" when quantifying parasite load
- hatch_size: integer; number of hatchlings in the brood (from year 1);
- par_load: integer; parasite load for the whole brood (from year 1);
- relative_par_load: real; ratio of parasite load in the brood divided by number of hatchlings in the brood;
- spl_period: character; period of feather collection; two possible values:
	- "C" indicates feathers were collected when potential breeders start constructing nests, in early spring;
	- "F" indicates feathers were collected when breeders feeding (10 to 15 days old) nestlings, later in spring
- BB: real; color variable corresponding to mean brightness from the blue crown
- BUVC: real; color variable corresponding to UV-chroma from the blue crown
- YB: real; color variable corresponding to mean brightness from the yellow breast patch
- YUVC: real; color variable corresponding to UV-chroma from the yellow breast patch
- YC: real; color variable corresponding to yellow chroma from the yellow breast patch
