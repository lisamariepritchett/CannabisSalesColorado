# get zipcodes
zip_file = 'data/zip_codes_states.csv'
if(!file.exists(zip_file)){
        url = 'http://notebook.gaslampmedia.com/wp-content/uploads/2013/08/zip_codes_states.csv'
}
co_zipcodes <- read.csv(zip_file, stringsAsFactors = F) %>% filter(state=='CO')

        
# download medical outlets
medfile = 'data/medcenters.xlsx'
if(!file.exists(medfile)){
        med_storesurl <- 'https://www.colorado.gov/pacific/sites/default/files/Centers%2001022018.xlsx'
        download.file(med_storesurl,medfile, mode = 'wb')
        }

# read data from excel
med_stores <- readWorksheetFromFile(medfile, sheet='Sheet1',
                                     startCol=1, endCol=6,
                                     startRow=2)
# some cleaning
med_stores <- med_stores %>% 
        rename(zip_code = ZIP., med.license=LICENSE..) %>%
        mutate(zip_code=as.integer(zip_code))  %>% 
        as_tibble()

# join with co_zipcodes to get county and geocodes lat, lng
med_stores <- left_join(med_stores,co_zipcodes, by="zip_code")



retailsurl = 'https://www.colorado.gov/pacific/sites/default/files/Stores%2001022018.xlsx'
retailfile = 'data/retailcenters.xlsx'
if(!file.exists(retailfile)){
        download.file(retailsurl,retailfile, mode = 'wb')}

retail_stores <- readWorksheetFromFile(retailfile, sheet='Sheet1',
                                        startCol=1, endCol=6,
                                        startRow=2)


retail_stores <- retail_stores %>% 
        rename(zip_code = ZIP.,retail.license=LICENSE..) %>%
        mutate(zip_code=as.integer(zip_code)) %>% 
        as_tibble()

# join with co_zipcodes to get county and geocodes lat, lng
retail_stores <- left_join(retail_stores,co_zipcodes, by="zip_code")


combined_stores <- full_join(med_stores,retail_stores) 
combined_stores <- left_join(combined_stores,co_zipcodes)



###########################
## Count the number of stores of each kind (med,rec,combined) in each County
n.med_county <- med_stores %>% group_by(County=county) %>% summarise(medical=n())
n.retail_county <- retail_stores %>% group_by(County=county) %>% summarise(recreational=n())
n.combined_county <- combined_stores %>% group_by(County=county) %>% summarise(Combined=n())

# Combine counts into dataframe and gather into long tidy form indexed by County and Kind
county_n.stores <- full_join(n.med_county,n.retail_county,by='County')
county_n.stores <- full_join(county_n.stores,n.combined_county,by='County')
county_n.stores <- county_n.stores %>% gather(key='kind', value='n.stores', -County)


# Calculate Porportion of stores in each county
county_n.stores <- county_n.stores %>% 
        group_by(County,kind) %>% 
        summarise(n.stores=sum(n.stores, na.rm=T)) %>% 
        ungroup() %>% 
        mutate(freq=n.stores/sum(n.stores)) %>% 
        filter(!is.na(County))


# Remove objects made in processing
rm(medfile,retailfile,retailsurl, n.med_county,n.retail_county,n.combined_county)
