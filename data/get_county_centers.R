zip_file = 'data/zip_codes_states.csv'
if(!file.exists(zip_file)){
        url = 'http://notebook.gaslampmedia.com/wp-content/uploads/2013/08/zip_codes_states.csv'
        download.file(url,zip_file)}

co_zipcodes <- read.csv(zip_file, stringsAsFactors = F) %>% filter(state=='CO')
county_centers <- co_zipcodes %>% 
        filter(state=='CO') %>% 
        select(latitude,longitude,county) %>% 
        group_by(county) %>% 
        na.omit() %>% 
        summarise_all('mean') %>%
        rename(County=county)

# Nudge Denver's coordinates
county_centers[county_centers$County=='Denver',2:3] = c(39.7,-105)

rm(zip_file)

