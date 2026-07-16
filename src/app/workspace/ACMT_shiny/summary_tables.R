## Pull all summary tables ####
## create data pull summaries folder if it doesn't exist: 

# ACS summary tables ####
summary_table_function<-function(dataset_path, summary_path, missingness_path){
    #if(file.exists(dataset_path) == FALSE){
    #  write.csv(data.frame(MESSAGE=c('Measures not yet pulled', 'Go back to the corresponding tab in the ShinyApp and pull data')), summary_path)
    #}
        
      summary_vars<-c('year', 'radius')
      
    if(file.exists(dataset_path) == TRUE){
        tryCatch(
          #create the summary table and write to data_summary folder
          {write.csv(read.csv(dataset_path)%>%dplyr::select(id, any_of(summary_vars), everything())%>%
                        dplyr::select(-any_of('X'))%>%
                       mutate_if(is.numeric, round, digits=3)%>%table_summary(.), summary_path)
  },error=function(e){cat("ERROR :", conditionMessage(e), "\n")})#this will print any error messages
          #create the missingness table and write to data_summary folder
      tryCatch(
      {write.csv(read.csv(dataset_path)%>%dplyr::select(id, any_of(summary_vars), everything())%>%
                    dplyr::select(-any_of('X'))%>%
                   mutate_if(is.numeric, round, digits=3)%>%table_missingness(.),missingness_path)
      },error=function(e){cat("ERROR :", conditionMessage(e), "\n")})#this will print any error messages     
      
}
  }

## Run the summary table function for each dataset:
# ACS
summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv', 
                       summary_path='~/workspace/ACMT_shiny/data_pull_summaries/acs_summary.csv', 
                       missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/acs_missingness.csv')
# CDC - Places
summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv', 
                       summary_path='~/workspace/ACMT_shiny/data_pull_summaries/cdc_summary.csv', 
                       missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/cdc_missingness.csv')
# Walk
summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv', 
                       summary_path='~/workspace/ACMT_shiny/data_pull_summaries/walk_summary.csv', 
                       missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/walk_missingness.csv')
# Mrfei
summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv', 
                       summary_path='~/workspace/ACMT_shiny/data_pull_summaries/mrfei_summary.csv', 
                       missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/mrfei_missingness.csv')

# Parks
summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv', 
                       summary_path='~/workspace/ACMT_shiny/data_pull_summaries/parks_summary.csv', 
                       missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/parks_missingness.csv')

# Crimerisk
summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_crimerisk.csv', 
                       summary_path='~/workspace/ACMT_shiny/data_pull_summaries/crimerisk_summary.csv', 
                       missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/crimerisk_missingness.csv')

# Sidewalk
summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_sidewalk.csv', 
                       summary_path='~/workspace/ACMT_shiny/data_pull_summaries/sidewalk_summary.csv', 
                       missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/sidewalk_missingness.csv')

# RPP
summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv', 
                       summary_path='~/workspace/ACMT_shiny/data_pull_summaries/rpp_summary.csv', 
                       missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/rpp_missingness.csv')

# Gentrification
summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv', 
                       summary_path='~/workspace/ACMT_shiny/data_pull_summaries/gentrification_summary.csv', 
                       missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/gentrification_missingness.csv')

# NLCD
summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv', 
                       summary_path='~/workspace/ACMT_shiny/data_pull_summaries/nlcd_summary.csv', 
                       missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/nlcd_missingness.csv')

# County GEOID
if(file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_county.csv')==TRUE){
  tryCatch({write.csv(
  read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_county.csv')%>%group_by(county_geoid) %>%
    dplyr::summarise(GEOID_count=n()), 
  '~/workspace/ACMT_shiny/data_pull_summaries/county_geoid_summary.csv')
},error=function(e){cat("ERROR :", conditionMessage(e), "\n")})#this will print any error messages  
} 
if(file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_county.csv')==FALSE){
  write.csv(data.frame(MESSAGE=c('Measures not yet pulled', 'Go back to the ACS tab in the ShinyApp and complete step 4')),
                 '~/workspace/ACMT_shiny/data_pull_summaries/county_geoid_summary.csv')}
  
### Create pdf of summary tables
create_report_function<-function(){
summary_list<-c('~/workspace/ACMT_shiny/data_pull_summaries/acs_summary.csv', '~/workspace/ACMT_shiny/data_pull_summaries/acs_missingness.csv', # ACS
                '~/workspace/ACMT_shiny/data_pull_summaries/walk_summary.csv','~/workspace/ACMT_shiny/data_pull_summaries/walk_missingness.csv', #Walk Index
                '~/workspace/ACMT_shiny/data_pull_summaries/cdc_summary.csv', '~/workspace/ACMT_shiny/data_pull_summaries/cdc_missingness.csv',  #CDC
                '~/workspace/ACMT_shiny/data_pull_summaries/nlcd_summary.csv', '~/workspace/ACMT_shiny/data_pull_summaries/nlcd_missingness.csv', #NLCD
                '~/workspace/ACMT_shiny/data_pull_summaries/mrfei_summary.csv',  '~/workspace/ACMT_shiny/data_pull_summaries/mrfei_missingness.csv', #MRFEI
                '~/workspace/ACMT_shiny/data_pull_summaries/parks_summary.csv','~/workspace/ACMT_shiny/data_pull_summaries/parks_missingness.csv', #Parks
                '~/workspace/ACMT_shiny/data_pull_summaries/crimerisk_summary.csv',  '~/workspace/ACMT_shiny/data_pull_summaries/crimerisk_missingness.csv', #CrimeRisk
                '~/workspace/ACMT_shiny/data_pull_summaries/sidewalk_summary.csv',  '~/workspace/ACMT_shiny/data_pull_summaries/sidewalk_missingness.csv', #Sidewalk
                  '~/workspace/ACMT_shiny/data_pull_summaries/rpp_summary.csv', '~/workspace/ACMT_shiny/data_pull_summaries/rpp_missingness.csv', #RPP
                '~/workspace/ACMT_shiny/data_pull_summaries/gentrification_summary.csv',  '~/workspace/ACMT_shiny/data_pull_summaries/gentrification_missingness.csv', #gentrification
                '~/workspace/ACMT_shiny/data_pull_summaries/county_geoid_summary.csv')
summary_list_plots<-list()

for(i in 1:length(summary_list)){
  if(file.exists(summary_list[[i]])){
    
    
  summary_list_plots[[i]]<- tryCatch(
    ggplot() + 
      annotation_custom(tableGrob(read.csv(summary_list[[i]], row.names = F) %>% head(20)%>%dplyr::select(-X), 
                                                                   theme=ttheme_default(base_size=10), rows=NULL)) + 
    labs(title = paste0(sub(".*data_pull_summaries/", "", summary_list[[i]]), ': Measure values'))+theme_minimal(), error=function(e) 
      ggplot() + annotation_custom(tableGrob(data.frame(Status='data pull not complete'), rows=NULL)) + 
      labs(title = paste0(sub(".*data_pull_summaries/", "", summary_list[[i]]), ': Measure values'))+theme_minimal())
  }
  
  else{
  summary_list_plots[[i]]<-ggplot() + annotation_custom(tableGrob(data.frame(Status='data pull not complete'), rows=NULL)) + 
    labs(title = paste0(sub(".*data_pull_summaries/", "", summary_list[[i]]), ': Measure values'))+theme_minimal()
  
  }
}
pdf('~/workspace/ACMT_shiny/data_pull_summaries/data_summary.pdf')
print(marrangeGrob(summary_list_plots, nrow=1, ncol=1))
dev.off()
}

create_report_function()


  