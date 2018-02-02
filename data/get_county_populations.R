countyfile = 'data/colorado_populations_components-change-county.csv'
if(!file.exists(countyfile)){
        url = 'https://storage.googleapis.com/co-publicdata/components-change-county.csv'
        download.file(url,countyfile)
}

# Read in county demographic data for Colorado 
county_populations <- read.csv(countyfile, stringsAsFactors = F) %>%
        select(County,Year,Population) %>% 
        filter(Year>=2014) %>% 
        filter(Year<=2017)  %>% 
        rename(County_ID=County) %>% 
        mutate(year=as.character(Year)) %>% 
        select(County_ID,year,Population)

# Read in Counties to match County names with IDs
county_IDs <- read.csv('data/colorado_county.csv', stringsAsFactors = F) %>% select(1:2)
colnames(county_IDs) <- c('County','County_ID')

# Join Counties with county_populations to add County Names
county_populations <- inner_join(county_IDs,county_populations,by='County_ID') 
county_populations$County <- str_replace_all(county_populations$County,' County','')

# Return county_populations
rm(countyfile, county_IDs)
county_populations <- county_populations %>% as_tibble()