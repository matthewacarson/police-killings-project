mypath <- 'C:\\Users\\madou\\OneDrive - UCLA IT Services\\1)_PS-Honors\\police_killings_github\\udp_expansion_matt\\data\\outputs\\databases\\'

all_cities_2018 <- read_csv(file = paste0(mypath, 'all_cities_2018.csv'))
zillow_database_2018 <- read_csv(file = paste0(mypath, 'zillow_database_2018.csv'))
LosAngeles_database_2018 <- read_csv(file = paste0(mypath, 'LosAngeles_database_2018.csv'))

pums <- read_csv(file = paste0(mypath, 'pums.csv'))


# This will check which columns that are present in the Los Angeles CSV are NOT present in the zillow_database_2018.csv
# 2/7/2024: This one looks the closest to having them all. This is the one I will use for now.
colnames(LosAngeles_database_2018)[!colnames(LosAngeles_database_2018) %in% colnames(zillow_database_2018)]

colnames(LosAngeles_database_2018)[!colnames(LosAngeles_database_2018) %in% colnames(all_cities_2018)]

colnames(LosAngeles_database_2018)[!colnames(LosAngeles_database_2018) %in% colnames(pums)]
