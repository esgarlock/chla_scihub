#Analysis of Canadian Sci-Hub User Behaviour 
#Written by Emma Garlock, MSc MISt (Research Librarian, University of Ottawa)
#Last Updated: May 26th, 2025

library(tidyverse)
#Set themes for all the plots
theme_set(theme_bw()+theme(panel.grid.major = element_blank(),text = element_text(size = 20)))

#Upload Canada dataset and rename columns 
data=read.delim("test.txt",header = FALSE)#Canada data set, you can download the full version here https://zenodo.org/records/1158301
#change col names 
colnames(data)=c("Timestamp",
                 "DOI",
                 "IP identifier",
                 "User identifier",
                 "Country according to GeoIP",
                 "City according to GeoIP",
                 "Latitude",
                 "Longitude")

#just get some baseline info for the Canada group. 
unique_values=data%>%
  dplyr::select(2:5)%>%
  filter(`Country according to GeoIP`=="Canada")%>%
  summarise_each(funs(n_distinct(.)))
#There were 1928614 DOIs, 100110 IPs and 1962069 user IDs

#Get stats for each Canadian city. Since I'm just counting downloads, and I know that each line is its own download, I only need to count the number of times each city appears in the city column to see who is downloading the most.

cities=data%>%
  mutate(`City according to GeoIP`=ifelse(`City according to GeoIP`=="Montréal","Montreal",`City according to GeoIP`))%>%
  group_by(`City according to GeoIP`)%>%
  count()%>%
  ungroup()%>%
  mutate(n=as.numeric(n))%>% #Want to express as a percentage (I know the total is correct because its the same as the row numbers for data)
  mutate(total=(sum(n, na.rm = TRUE)))%>%
  mutate(percent=(n/total)*100)

#save a csv that I can use as a table in the ppt 

write.csv(cities,"chla_results/sci_hub_cities.csv",row.names=FALSE)

#Get a list of the top 5 cities (this will be used later)
top_cities=cities%>%
  mutate(`City according to GeoIP`=ifelse(`City according to GeoIP`=="Montréal","Montreal",`City according to GeoIP`))%>%
  top_n(n,n=5)%>%
  dplyr::select(`City according to GeoIP`)

cities_list=top_cities$`City according to GeoIP`#save as a list so we can filter later

#look at the dates people use Sci-Hub the Most 

#Get plot of date. 
dates_plot=data%>%
  dplyr::select(Timestamp)%>%
  mutate(date=str_sub(Timestamp,1,10))%>%
  group_by(date)%>%
  count()%>%
  mutate(date=as.Date(date,"%Y-%m-%d"))%>%#get dates formatted properly
  ggplot(aes(x=date,y=n))+
  geom_point(size=2,color="#332288")+
  xlab("Date")+
  ylab("Number of Downloads")

dates_plot

ggsave("chla_results/dates_2017_scatterplot.png",plot=dates_plot,width = 22, height = 20, units = "cm")

#Cumulative Usage by Datge 

csum_city=data%>%
  dplyr::select(Timestamp,`City according to GeoIP`)%>%
  mutate(date=str_sub(Timestamp,1,10))%>%
  filter(`City according to GeoIP` %in% cities_list)%>%
  group_by(`City according to GeoIP`,date)%>%
  count()%>%
  mutate(date=as.Date(date,"%Y-%m-%d"))%>%
  ungroup()%>%
  group_by(`City according to GeoIP`)%>%
  mutate(csum = cumsum(n))

#Make a plot 
dates_city_plot=ggplot(csum_city,aes(x=date,y=(csum/10000)))+
  geom_line(aes(color=`City according to GeoIP`),linewidth=1.5)+
  xlab("Date")+
  ylab("Cumulative Downloads (10K)")+
  scale_color_manual(values=c("#332288",'#117733',"#44AA99","#CC6677","#882255"))+
  guides(color=guide_legend(title="City",title.position="top"))+
  theme(legend.title = element_text(hjust=0.5),legend.position = "left")
dates_city_plot

ggsave("chla_results/csum_dates_2017_liineplot.png",plot=dates_city_plot,width = 22, height = 20, units = "cm")

#DOI stuff 
#Get the DOIS so I can import them into Zotero to get bibliographic data, only grab the top 200 though for speed reasons 
doi=data%>%
  group_by(DOI)%>%
  count()%>%
  ungroup()%>%
  mutate(n=as.numeric(n))%>%
  mutate(total=(sum(n, na.rm = TRUE)))%>%
  mutate(percent=(n/total)*100)%>%
  slice_max(percent,n=200)


doi_csv=doi%>%dplyr::select(DOI)#make into a format that I can save and upload to Zotero

write.csv(doi_csv,"doi_import.csv",row.names = FALSE)


#import Zotero metadata,the SciHubDOI information is available on GitHub 

#Upload data and lowercase and remove whitespaces fromDOI since I will be merging with that later 
zotero=read.csv("SciHubDOI.csv")%>%
  mutate(DOI=str_squish(str_to_lower(DOI)))

#Get a reduced dataset since I don't need all 80+ columns 
zotero_select=zotero%>%
  dplyr::select(Publication.Year,Title,Publication.Title)

#publication year of journal 

#Make a decade list for plotting, so I can set the factor levels 
decade_list=c("Pre-1950","1950-1959","1960-1969","1970-1979","1980-1989","1990-1999","2000-2009","2010-2019")

#See when the pirated papers were published (didn't make it to the final presentation)
pubyear=zotero_select%>%
  dplyr::select(Publication.Year)%>%
  mutate(decade = floor(Publication.Year/10)*10) %>%
  group_by(decade) %>% 
  count()%>%
  ungroup()%>%
  mutate(upper_range=decade+9)%>%
  mutate(labels=paste0(as.character(decade),"-",as.character(upper_range)))%>%
  mutate(labels=ifelse(decade<1950,"Pre-1950",labels))%>%
  group_by(labels)%>%
  summarise_all(sum)%>%
  dplyr::select(labels,n)%>%
  ggplot(aes(x=labels,y=n))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits = decade_list)

pubyear

#Journal Categories 
#The categories are based on the Clarivate Indexing categories. So if DOI's are found in journals not indexed, then they won't have a category. Also don't have this data availabel on github, because I don't think Clarivate would like that. You can download it from your own accounts though 
#I've used SCIE, AHCI, ESCI, and SSCI so I have branched out into some social sciences 
SCIE_Index=read.csv('SCIE_index.csv')
scie_issn=SCIE_Index%>%
  dplyr::select(ISSN,eISSN,Web.of.Science.Categories)%>%
  pivot_longer(1:2,names_to="type",values_to="issn")%>% #I combine the issn and eissn into one column, since idk if the issn found in zotero is for the physical or digital copy, so either way it'll match 
  mutate_all(na_if,"")%>%
  drop_na(issn)
AHCI_Index=read.csv('AHCI_index.csv')
AHCI_issn=AHCI_Index%>%
  dplyr::select(ISSN,eISSN,Web.of.Science.Categories)%>%
  pivot_longer(1:2,names_to="type",values_to="issn")%>%
  mutate_all(na_if,"")%>%
  drop_na(issn)
ESCI_Index=read.csv('ESCI_index.csv')
ESCI_issn=ESCI_Index%>%
  dplyr::select(ISSN,eISSN,Web.of.Science.Categories)%>%
  pivot_longer(1:2,names_to="type",values_to="issn")%>%
  mutate_all(na_if,"")%>%
  drop_na(issn)
SSCI_Index=read.csv('SSCI_index.csv')
SSCI_issn=SSCI_Index%>%
  dplyr::select(ISSN,eISSN,Web.of.Science.Categories)%>%
  pivot_longer(1:2,names_to="type",values_to="issn")%>%
  mutate_all(na_if,"")%>%
  drop_na(issn)

#Bind all of them together in one dataset
WoS_issn=rbind(scie_issn,AHCI_issn,ESCI_issn,SSCI_issn)%>%distinct()

#Get ISSN From Zotero 
ISSN=zotero%>%
  dplyr::select(Key, "issn"=ISSN)%>%
  separate_longer_delim(issn,delim=",")%>%
  mutate(issn=str_squish(issn))%>%
  mutate_all(na_if,"")%>%
  drop_na(issn)
  

#Join Dataset with the WoS categories, make sure I don't double count, and then summarize the most populat categories (papers might get "double counted" because they have more than one category applied)
top_categories=ISSN%>%
  left_join(WoS_issn,by="issn")%>%
  dplyr::select(1:3)%>%
  distinct()%>%
  separate_longer_delim(Web.of.Science.Categories,delim="|")%>%
  dplyr::select(-issn)%>%
  mutate(categories=str_squish(Web.of.Science.Categories))%>%
  dplyr::select(-Web.of.Science.Categories)%>%
  distinct()%>%
  group_by(categories)%>%
  count()

#write csv to use as table in the ppt 
write.csv(top_categories,"chla_results/top_categories.csv",row.names = FALSE)

#See DOI by City 

top_doi_list=zotero$DOI #make a list of relevant DOIS so that I don't do these analyses on the entire dataset, because that would take too long 
city_doi=data%>%
  dplyr::select(DOI,`City according to GeoIP`)%>%
  mutate(DOI=str_squish(str_to_lower(DOI)))%>%
  filter(DOI %in% top_doi_list)%>%
  mutate(`City according to GeoIP`=ifelse(`City according to GeoIP`=="Montréal","Montreal",`City according to GeoIP`))%>%
  group_by(DOI,`City according to GeoIP`)%>%
  count()%>%
  left_join(zotero,by="DOI")%>% #need to do this because I want the titles 
  dplyr::select(DOI,`City according to GeoIP`,'city_downloads'=n, Title, ISSN)%>%
  left_join(doi,by="DOI")#this gives me the total numbers and percentages 

#just clean the above up a little bit to make it more presentation friendly. 
city_doi_presentation_table=city_doi%>%
  ungroup()%>%
  dplyr::select(Title,`City according to GeoIP`,city_downloads,'total_downloads'=n)%>%
  filter(total_downloads>470)
 
  write.csv(city_doi_presentation_table,"chla_results/top_doi_city.csv")
