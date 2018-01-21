# Get Cannabis Data from excel files and preprocess into tidy dataframe

enter_envir <- ls()

# Create codes to paste into urls for data for each month
# Make string codes for 2014 to 2016
codes = c()
for (year in c('14', '15', '16','16')) {
        for (month in str_pad(1:12, 2, pad = '0')) {
                codes = c(codes,paste(month, year, sep = ''))
        }
}
# Add on what is available for 2017
codes = c(codes,paste(str_pad(1:10,2,pad='0'),'17',sep = ''))

#Loop through codes, downloading data for each month and saving in a dataframe
medical.sales = data.frame(County='')
recreational.sales = data.frame(County='')
medical.NR = data.frame(County='')
recreational.NR = data.frame(County='')
for (code in codes) {
        # paste together url from code
        myurl = paste('https://www.colorado.gov/pacific/sites/default/files/',
                      code,
                      '_MarijuanaSalesReport%20PUBLISH.xlsx',
                      sep = '')
        destfile = paste('salesdata/cannabissales_',code,'.xlsx',sep='')
        
        # download file if not yet downloaded
        if (!file.exists(destfile)){
                download.file(myurl,destfile=destfile, mode = 'wb')}
        
        # Read in Medical sales Data from file
        wb = loadWorkbook(destfile)
        med1 = readWorksheetFromFile(file = destfile,
                                     sheet = 'Sheet1',
                                     startRow = 6, endRow = 44, 
                                     startCol = 1, endCol = 2,
                                     colTypes = 'character')
        
        ## Read in Recreational sales Data from file
        rec1 =  readWorksheetFromFile(file = destfile,
                                      sheet = 'Sheet1',
                                      startRow = 6, endRow = 44, 
                                      startCol=4, endCol = 5,
                                      colTypes = 'character') 
        
        # shorten column name
        colnames(med1)[2] =  substr(colnames(med1)[2], 1,
                                    nchar(colnames(med1)[2])-8)
        colnames(rec1)[2] =  substr(colnames(rec1)[2], 1,
                                    nchar(colnames(rec1)[2])-8)
        
        # set aside NR rows
        medNR = med1[med1[2]=='NR', ] %>% filter(!is.na(County))
        recNR = rec1[rec1[2]=='NR', ] %>% filter(!is.na(County))
        
        
        # Replace NR with average of the NR Counties for each month
        medtotalNR = med1[med1$County=='Sum of NR Counties 4',][[2]]
        medtotalNR = gsub('[$,]','',medtotalNR) %>% as.numeric()
        medavgNR = medtotalNR / sum(medNR[,2]=='NR')
        medNR[,2] = gsub('NR',medavgNR[1],medNR[,2])
        
        # Replace NR with average of the NR Counties for each month
        rectotalNR = rec1[rec1$County=='Sum of NR Counties 4',][[2]]
        rectotalNR = gsub('[$,]','',rectotalNR) %>% as.numeric()
        recavgNR = rectotalNR / sum(recNR[,2]=='NR')
        recNR[,2] = gsub('NR',recavgNR[1], x=recNR[,2])
        
        
        # join to build dataframes
        medical.sales = full_join(medical.sales,med1,by='County')
        recreational.sales = full_join(recreational.sales, rec1,by='County')
        medical.NR = full_join(medical.NR,medNR, by='County')
        recreational.NR = full_join(recreational.NR,recNR, by='County')
}


##### 
## Get list of Colorado Counties
counties <- read.csv('colorado_county.csv', stringsAsFactors = F) %>% select(1)
colnames(counties) = 'County'
counties <- str_replace(counties$County,' County','')


##### 
## Gather each dataframe from wide to long making columns for month and and sales 
# for each type of sales
medical.sales <- medical.sales %>% 
        filter(County %in% counties) %>% 
        gather(key='month.year',value='medical.sales', -County)

recreational.sales <- recreational.sales %>% 
        filter(County %in% counties) %>% 
        gather(key='month.year',value='recreational.sales', -County)

medical.NR <- medical.NR %>% 
        gather(key='month.year',value='medical.NR',-County)

recreational.NR <- recreational.NR %>% 
        gather(key='month.year',value='recreational.NR',-County)

## Clean sales data, remove dollar signs and commas, coerce to numeric
medical.sales$medical.sales <- str_replace_all(
        medical.sales$medical.sales,'[$,NR]','') %>% 
        as.numeric()

recreational.sales$recreational.sales <- str_replace_all(
        recreational.sales$recreational.sales,'[$,NR ]','') %>% 
        as.numeric()

## Join 4 Types; medical and recreational sales, and then both NR into 
cannabis.sales <- full_join(medical.sales,recreational.sales, 
                            by = c("County", "month.year"))
cannabis.sales <- full_join(cannabis.sales,medical.NR, 
                            by = c("County", "month.year"))
cannabis.sales <- full_join(cannabis.sales,recreational.NR, 
                            by = c('County','month.year'))

cannabis.sales <- cannabis.sales %>%
        filter(County!='') %>% 
        mutate(medical.NR =as.numeric(medical.NR),
               recreational.NR=as.numeric(recreational.NR)) %>% 
        mutate(date = paste('01.',month.year,sep='') %>% 
                       as.Date('%d.%B.%Y'),
               year = format(date, '%Y'))

# Add combined sales and combined NR
cannabis.sales$Combined.sales=rowSums(select(
        cannabis.sales,medical.sales,recreational.sales), na.rm = T)
cannabis.sales$Combined.NR=rowSums(select(
        cannabis.sales,medical.NR,recreational.NR), na.rm = T)



#####
## Gather into even longer dataframe
## Make Sales Type a Factor Variable 
cannabis.sales <- cannabis.sales %>% 
        select(-month.year) %>% 
        gather(key = 'type', value = 'USD', -County, -year, -date)

# make dummy variables to help track sales type 
cannabis.sales$is.medical = str_detect(cannabis.sales$type,'medical')
cannabis.sales$is.Combined = str_detect(cannabis.sales$type,'Combined')
cannabis.sales$is.Estimate = str_detect(cannabis.sales$type,'NR')
cannabis.sales$kind = str_extract(cannabis.sales$type,'[aA-zZ]*')



# reorder columns
cannabis.sales <- cannabis.sales %>%
        as_tibble()


rm(list = setdiff(ls(),c('cannabis.sales',enter_envir)))


