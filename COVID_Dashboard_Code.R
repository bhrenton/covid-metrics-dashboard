library(tidyverse)
library(usdata)
library(lubridate)
library(zoo)
library(readxl)
#State
hospitalizations<-read_csv("https://healthdata.gov/resource/g62h-syeh.csv?$limit=80000") %>% 
  arrange(date,state) %>% 
  filter(date>="2020-07-31") %>% 
  mutate(previous_day_hospitalizations=previous_day_admission_adult_covid_confirmed+previous_day_admission_pediatric_covid_confirmed) %>% 
  select(date,state,inpatient_beds,inpatient_beds_used,inpatient_beds_used_covid,total_staffed_adult_icu_beds,staffed_adult_icu_bed_occupancy,staffed_icu_adult_patients_confirmed_covid,previous_day_hospitalizations)
hospitalizations_national<-hospitalizations %>% 
  group_by(date) %>% 
  summarise(inpatient_beds=sum(inpatient_beds),inpatient_beds_used=sum(inpatient_beds_used),inpatient_beds_used_covid=sum(inpatient_beds_used_covid,na.rm=TRUE),total_staffed_adult_icu_beds=sum(total_staffed_adult_icu_beds),staffed_adult_icu_bed_occupancy=sum(staffed_adult_icu_bed_occupancy),staffed_icu_adult_patients_confirmed_covid=sum(staffed_icu_adult_patients_confirmed_covid),previous_day_hospitalizations=sum(previous_day_hospitalizations))%>% 
  mutate(state="US") 
hospitalizations_all<-rbind(hospitalizations,hospitalizations_national) %>% 
  mutate(State=abbr2state(state)) %>% 
  mutate(State=ifelse(state=="AS","American Samoa",ifelse(state=="VI","US Virgin Islands",ifelse(state=="PR","Puerto Rico",ifelse(state=="US","United States",State))))) %>% 
  rename(abbr=state) %>% 
  mutate(key=paste(date,State))
deaths<-read_csv("https://data.cdc.gov/resource/mpx5-t7tu.csv?$limit=60000") %>% 
  filter(group=="weekly") %>% 
  rename(date=data_period_end,weekly_covid_deaths=covid_deaths) %>%
  filter(!jurisdiction_residence%in%c("New York","New York City")) %>% 
  mutate(jurisdiction_residence=ifelse(jurisdiction_residence=="New York and New York City","New York",jurisdiction_residence)) %>% 
  mutate(key=paste(date,jurisdiction_residence)) %>% 
  select(key,weekly_covid_deaths,covid_pct_of_total,jurisdiction_residence,date) %>% 
  rename(deaths_date=date)
wastewater_state <- read_csv("https://data.cdc.gov/resource/2ew6-ywp6.csv?$limit=1000000") %>% 
  arrange(date_end,reporting_jurisdiction) %>% 
  filter(percentile<=100) %>% 
  mutate(reporting_jurisdiction=ifelse(reporting_jurisdiction=="New York City","New York",reporting_jurisdiction)) %>% 
  mutate(reporting_jurisdiction=ifelse(reporting_jurisdiction=="Houston","Texas",reporting_jurisdiction)) %>% 
  group_by(date_end,reporting_jurisdiction) %>% 
  summarise(cdc_wastewater_percentchange=mean(ptc_15d,na.rm=TRUE),cdc_wastewater_percentile=mean(percentile,na.rm=TRUE)) %>% 
  mutate(key=paste(date_end,reporting_jurisdiction)) %>% 
  ungroup() %>% 
  select(key,date_end,reporting_jurisdiction,cdc_wastewater_percentchange,cdc_wastewater_percentile)
wastewater_national <- read_csv("https://data.cdc.gov/resource/2ew6-ywp6.csv?$limit=1000000") %>% 
  arrange(date_end,reporting_jurisdiction) %>% 
  filter(percentile<=100) %>%
  group_by(date_end) %>% 
  summarise(cdc_wastewater_percentchange=mean(ptc_15d,na.rm=TRUE),cdc_wastewater_percentile=mean(percentile,na.rm=TRUE)) %>% 
  mutate(reporting_jurisdiction="United States") %>% 
  mutate(key=paste(date_end,reporting_jurisdiction)) %>% 
  ungroup() %>% 
  select(key,date_end,reporting_jurisdiction,cdc_wastewater_percentchange,cdc_wastewater_percentile)
wastewater_all<-rbind(wastewater_state,wastewater_national)
tests<-read_csv("https://data.cdc.gov/resource/gvsb-yw6g.csv?$limit=1000000") %>% 
  filter(level=="National") %>% 
  mutate(posted=as.Date(posted)) %>% 
  filter(posted==as.Date("2023-08-09")) %>% 
  rename(weekly_test_positivity=percent_pos,weekly_tests=number_tested) %>% 
  mutate(key=paste(mmwrweek_end,"United States")) %>% 
  select(key,weekly_test_positivity,weekly_tests)
case_data<-read_csv("https://data.cdc.gov/resource/pwn4-m3yp.csv?$limit=60000") %>% 
  mutate(date=as.Date(end_date)) %>% 
  mutate(state2=ifelse(state%in%c("NY","NYC"),"NY",state)) %>% 
  group_by(date,state2) %>% 
  summarise(new_case=sum(new_cases),new_death=sum(new_deaths)) %>% 
  rename(state=state2) %>% 
  arrange(date,state) %>% 
  group_by(date,state) %>% 
  summarise(new_cases=sum(new_case)) %>% 
  mutate(State=abbr2state(state)) %>% 
  filter(!is.na(State)) %>% 
  mutate(date=date+3) %>% 
  mutate(key=paste(date,State)) %>% 
  ungroup() %>% 
  select(key,new_cases)
case_data_us<-read_csv("https://data.cdc.gov/resource/pwn4-m3yp.csv?$limit=60000") %>% 
  mutate(date=as.Date(end_date)) %>% 
  mutate(state2=ifelse(state%in%c("NY","NYC"),"NY",state)) %>% 
  group_by(date,state2) %>% 
  summarise(new_case=sum(new_cases),new_death=sum(new_deaths)) %>% 
  rename(state=state2) %>% 
  arrange(date,state) %>% 
  group_by(date) %>% 
  summarise(new_cases=sum(new_case)) %>% 
  mutate(State="United States") %>% 
  mutate(date=date+3) %>% 
  mutate(key=paste(date,State)) %>% 
  ungroup() %>% 
  select(key,new_cases)
case_data_all<-rbind(case_data,case_data_us)
states_all<-full_join(hospitalizations_all,deaths,by="key") %>% 
  filter(!grepl("Region",key)) %>% 
  mutate(State=ifelse(is.na(State),jurisdiction_residence,State)) %>% 
  mutate(date=as.Date(date)) %>% 
  mutate(deaths_date=as.Date(deaths_date)) %>% 
  mutate(date=ifelse(is.na(date),deaths_date,date)) %>% 
  mutate(date=as.Date(date)) %>% 
  full_join(.,wastewater_all,by="key") %>% 
  left_join(.,tests,by="key") %>% 
  left_join(.,case_data_all,by="key") %>% 
  mutate(date=as.Date(date)) %>% 
  mutate(date=as.Date(date)) %>% 
  mutate(date=ifelse(is.na(date),date_end,date)) %>% 
  mutate(date=as.Date(date)) %>% 
  mutate(State=ifelse(is.na(State),reporting_jurisdiction,State))  %>% 
  arrange(date,State) %>% 
  mutate(inpatient_beds_available=inpatient_beds-inpatient_beds_used,icu_beds_available=total_staffed_adult_icu_beds-staffed_adult_icu_bed_occupancy) %>% 
  select(date,State,inpatient_beds,inpatient_beds_used,inpatient_beds_available,inpatient_beds_used_covid,total_staffed_adult_icu_beds,staffed_adult_icu_bed_occupancy,icu_beds_available,staffed_icu_adult_patients_confirmed_covid,previous_day_hospitalizations,weekly_covid_deaths,covid_pct_of_total,cdc_wastewater_percentchange,cdc_wastewater_percentile,weekly_test_positivity,weekly_tests,new_cases) %>% 
  group_by(State) %>% 
  mutate(new_daily_average_admissions=rollmeanr(previous_day_hospitalizations,k=7,fill=NA)) %>% 
  group_by(State) %>% 
  mutate(admissions_pctchange=(new_daily_average_admissions-(lag(new_daily_average_admissions,n=7)))/(lag(new_daily_average_admissions,n=7))*100) %>% 
  select(-previous_day_hospitalizations) %>% 
  rename(`Total Inpatient Beds`=inpatient_beds,`Inpatient Beds Used`=inpatient_beds_used,`Inpatient Beds Available`=inpatient_beds_available,`Inpatient Beds Used by COVID Patients`=inpatient_beds_used_covid,`Total Adult ICU Beds`=total_staffed_adult_icu_beds,`Adult ICU Beds Used`=staffed_adult_icu_bed_occupancy,`Adult ICU Beds Available`=icu_beds_available,`Adult ICU Beds Used by COVID Patients`=staffed_icu_adult_patients_confirmed_covid,`New COVID Admissions (7-day average)`=new_daily_average_admissions,`Weekly Change in COVID Hospital Admissions`=admissions_pctchange,`Weekly COVID Deaths`=weekly_covid_deaths,`Percent of Total Deaths that are COVID`=covid_pct_of_total,`Weekly Test Positivity`=weekly_test_positivity,`Weekly Tests`=weekly_tests,`Wastewater Percentile of Maximum`=cdc_wastewater_percentile,`Wastewater 15-day Percent Change`=cdc_wastewater_percentchange,`Weekly New Cases`=new_cases) 
wastewater_state_maxdate<- read_csv("https://data.cdc.gov/resource/2ew6-ywp6.csv?$limit=1000000") %>% 
  arrange(date_end,reporting_jurisdiction) %>% 
  ungroup() %>% 
  filter(date_end==max(date_end)) %>% 
  select(date_end) %>% 
  unique()
wastewater_state_maxdate<-as.Date(wastewater_state_maxdate$date_end)
states_all_tableau<-states_all %>% 
  mutate(weekday=wday(date)) %>% 
  filter(weekday==7|date==wastewater_state_maxdate)%>% 
  select(-weekday)
write.csv(states_all_tableau, "covid_dashboard_states_tableau.csv", row.names = F, na = "")


#Counties
hospitalizations_county<-read_csv("https://data.cdc.gov/resource/akn2-qxic.csv?$limit=60000") %>% 
  select(week_end_date,county,state,fips_code,total_adm_all_covid_confirmed,admissions_covid_confirmed,total_adm_all_covid_confirmed_per_100k) %>% 
  mutate(fips_code = str_pad(fips_code,5,pad = "0")) %>% 
  rename(weekly_admissions=total_adm_all_covid_confirmed,weekly_admissions_100k=total_adm_all_covid_confirmed_per_100k,admissions_pctchange=admissions_covid_confirmed) %>% 
  arrange(week_end_date,fips_code) %>% 
  mutate(county_state=paste(county,state,sep=", ")) %>% 
  mutate(key=paste(week_end_date,fips_code)) 
hospitalizations_county_old<-read_csv("https://data.cdc.gov/resource/3nnm-4jni.csv?$limit=250000") %>% 
  mutate(week_end_date=as.Date(date_updated)+2) %>% 
  arrange(week_end_date,county_fips) %>% 
  rename(fips_code=county_fips,weekly_admissions_100k=covid_hospital_admissions_per_100k) %>% 
  mutate(weekly_admissions=(weekly_admissions_100k/100000)*county_population) %>% 
  mutate(county_state=paste(county,state,sep=", ")) %>% 
  group_by(fips_code) %>% 
  mutate(admissions_pctchange=(weekly_admissions-(lag(weekly_admissions,n=1)))/(lag(weekly_admissions,n=1))*100) %>% 
  ungroup() %>% 
  mutate(key=paste(week_end_date,fips_code)) %>% 
  filter(week_end_date<="2023-05-05") %>% 
  select(week_end_date,county,state,fips_code,weekly_admissions,admissions_pctchange,weekly_admissions_100k,county_state,key)
hospitalizations_county_all<-rbind(hospitalizations_county,hospitalizations_county_old)
unassigned_counties<-data<-read_excel("wastewater_unassignedcounties.xlsx") %>% 
  select(key_plot_id,sup_fips)
wastewater_county <- read_csv("https://data.cdc.gov/resource/2ew6-ywp6.csv?$limit=1000000") %>% 
  mutate(county_fips = str_pad(county_fips,5,pad = "0")) %>% 
  arrange(date_end,county_fips) %>% 
  filter(percentile<=100) %>% 
  left_join(.,unassigned_counties,by="key_plot_id") %>% 
  mutate(county_fips=ifelse(is.na(county_fips),sup_fips,county_fips)) %>% 
  select(-sup_fips) %>% 
  group_by(date_end,county_fips) %>% 
  summarise(cdc_wastewater_percentchange=mean(ptc_15d,na.rm=TRUE),cdc_wastewater_percentile=mean(percentile,na.rm=TRUE)) %>% 
  mutate(key=paste(date_end,county_fips)) %>% 
  ungroup() %>% 
  rename(countyfips_wastewater=county_fips) %>% 
  select(key,date_end,countyfips_wastewater,cdc_wastewater_percentchange,cdc_wastewater_percentile)
countycases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
  select(-c("UID","iso2","iso3","code3","Admin2","Province_State","Country_Region","Lat","Long_","Combined_Key")) %>% 
  pivot_longer(cols = -c(1),
               names_to = "Date",
               values_to = "Cases") %>% 
  mutate(FIPS = str_pad(FIPS,5,pad = "0")) %>% 
  select(FIPS, Date, Cases) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  group_by(FIPS) %>% 
  mutate(newcases=Cases-lag(Cases)) %>% 
  select(Date,FIPS,newcases) %>% 
  mutate(new_cases=rollsumr(newcases,k=7,fill=NA)) %>% 
  select(Date,FIPS,new_cases) %>% 
  group_by(FIPS) %>% 
  complete(Date = seq.Date(min(Date), today(), by="day")) %>%
  mutate(weekday=wday(Date)) %>% 
  filter(weekday==7) %>% 
  select(-weekday) %>% 
  mutate(key=paste(Date,FIPS)) %>% 
  ungroup() %>% 
  select(Date,FIPS,key,new_cases)
counties_all<-full_join(countycases,hospitalizations_county_all,by="key") %>%
  full_join(.,wastewater_county,by="key") %>% 
  select(-week_end_date,-fips_code) %>% 
  rename(week_end_date=Date,fips_code=FIPS) %>% 
  group_by(fips_code) %>% 
  fill(county_state, .direction = "up") %>% 
  mutate(fips_code=ifelse(is.na(fips_code),countyfips_wastewater,fips_code)) %>% 
  mutate(week_end_date=as.Date(week_end_date)) %>% 
  mutate(date_end=as.Date(date_end)) %>% 
  mutate(week_end_date=ifelse(is.na(week_end_date),date_end,week_end_date)) %>% 
  mutate(week_end_date=as.Date(week_end_date)) %>% 
  filter(!is.na(fips_code)) %>% 
  rename(date=week_end_date) %>% 
  arrange(date,fips_code) %>% 
  group_by(fips_code) %>% 
  fill(county_state, .direction = "down") %>% 
  select(date,fips_code,county_state,weekly_admissions,weekly_admissions_100k,admissions_pctchange,cdc_wastewater_percentchange,cdc_wastewater_percentile,new_cases) %>% 
  rename(County=county_state,`New Weekly COVID-19 Hospital Admissions`=weekly_admissions,`New Weekly COVID-19 Hospital Admissions per 100,000`=weekly_admissions_100k,`Weekly Change in COVID-19 Hospital Admissions`=admissions_pctchange,`Wastewater Percentile of Maximum`=cdc_wastewater_percentile,`Wastewater 15-day Percent Change`=cdc_wastewater_percentchange,`Weekly New Cases`=new_cases) 
wastewater_county_maxdate<-wastewater_county %>%
  ungroup() %>% 
  filter(date_end==max(date_end)) %>% 
  select(date_end) %>% 
  unique()
wastewater_county_maxdate<-as.Date(wastewater_county_maxdate$date_end)
counties_all_tableau<-counties_all %>% 
  mutate(weekday=wday(date)) %>% 
  filter(weekday==7|date==wastewater_county_maxdate)%>% 
  select(-weekday) %>% 
  filter(!is.na(County)) 
write.csv(counties_all_tableau, "covid_dashboard_counties_tableau.csv", row.names = F, na = "")

#RESP-NET


resp_net <- read_csv("https://data.cdc.gov/resource/kvib-3txy.csv?$limit=1000000") %>% 
  mutate(week_ending_date=as.Date(week_ending_date)) %>% 
  #filter(week_ending_date>="2020-01-01") %>% 
  select(week_ending_date,age_group,sex,race_ethnicity,site,surveillance_network,weekly_rate) %>% 
  unique()  %>% 
  arrange(week_ending_date,age_group,sex,race_ethnicity,site,surveillance_network) %>% 
  mutate(weekly_rate=ifelse(is.na(weekly_rate),0,weekly_rate)) %>%
  unique() %>% 
  group_by(week_ending_date,age_group,sex,race_ethnicity,site,surveillance_network) %>% 
  filter(weekly_rate==max(weekly_rate))%>% 
  pivot_wider(names_from="surveillance_network",values_from="weekly_rate") %>% 
  arrange(week_ending_date)

write.csv(resp_net, "covid_dashboard_resp_illnesses.csv", row.names = F, na = "")
