library(tidyverse)
library(progress)
library(ggsci)
library(rtrim)


# lisa liigikoodid

ebi<-read_excel("src/Eesti linnud 20250211.xlsx") %>% filter(kat=="A"|ladnimi=="Columba livia") %>% rename(liik=`3+3`) %>% mutate(ord=1:nrow(.)) %>% select(liik,ladnimi,ord) %>%
  mutate(
    ladnimi=recode(ladnimi, "Buteo buteo vulpinus"="Buteo buteo")
  )
lnim <- read_csv("G:/analyys/20250722_bdart12/teatmik/lnim.csv") %>%
  filter(is.na(`Rakendatav liik`)) %>%
  select(Id,`Natura kood`,`EURING kood`,`Nimi eesti k`,`Nimi ladina k`) %>%
  rename(eelis_id=Id,ladnimi=`Nimi ladina k`, species_code=`Natura kood`, euring=`EURING kood`) %>%
  mutate(
    ladnimi=recode(ladnimi, "Cygnus columbianus bewickii"="Cygnus columbianus"),
    euring=gsub("^0","",euring),
    euring=as.integer(euring),
    euring==replace(euring, which(ladnimi=="Columba livia"), 6650)
  )

#ebi %>% left_join(lnim, by="ladnimi") %>%
#  filter(is.na(eelis_id)) %>%
#  view()

eestilinnud<-ebi %>% left_join(lnim, by="ladnimi")



# transektide trendid

load("data/loendus-20251015.RData")


# erista loendused, kus 1 korral (atlas) on loetud vaid ribadel 1&2

atlas1vaatlustearvribades<-vaatlus %>%
  filter(kordus==1 & skeem=="atlas") %>%
  left_join(loendus %>% select(kordus,skeem,transekt,obsrvr), by=c("kordus","skeem","transekt")) %>%
  filter(!is.na(riba)) %>%
  group_by(transekt,obsrvr,riba) %>%
  summarise(N=n()) %>%
  pivot_wider(names_from="riba", values_from="N") %>%
  arrange(-desc(`3`))
write_excel_csv2(atlas1vaatlustearvribades, "atlas1vaatlustearvribades.csv", na='')

# praegu ei välista


vwide<-vaatlus %>%
  filter(!(kordus==2 & transekt %in% c(180, 201, 209, 215, 223, 224, 229, 232)) ) %>%
  group_by(skeem,transekt,liik,kordus) %>%
  summarise(paare=sum(paare)) %>%
  pivot_wider(names_from="kordus", values_from="paare", values_fill=0)




data1 <- vaatlus %>%
  filter(!(kordus==2 & transekt %in% c(180, 201, 209, 215, 223, 224, 229, 232)) ) %>%
  group_by(skeem,transekt,aasta,liik,kordus) %>%
  summarise(paare=sum(paare)) %>%
  filter(!is.na(aasta)) #%>%
  #mutate(
  #  year=case_when(
  #    skeem=="soome" & kordus==1 ~ 2001,
  #    skeem=="atlas" & kordus==1 ~ 2008,
  #    skeem=="soome" & kordus==2 ~ 2022,
  #    skeem=="atlas" & kordus==2 ~ 2024,
  #  )
  #)


trstrata<-data1 %>% group_by(skeem,liik) %>%
  summarise(N=n()) %>%
  arrange(-desc(N)) %>%
  filter(N>5)

resultsuccess<-trstrata %>% mutate(success=FALSE)

n_iter<-nrow(trstrata)
pb <- progress_bar$new(format = "  töötlen [:bar] :percent eta: :eta", total = n_iter, clear = FALSE, width= 60)
for(i in 1:nrow(trstrata)) {
  
  skeemkood<-trstrata$skeem[i]
  sp<-trstrata$liik[i]
  euringcode<-eestilinnud %>% filter(liik==sp) %>% select(euring) %>% unlist() %>% unname()
  
  
  data2<-data1 %>% filter(liik==sp & skeem==skeemkood) %>% ungroup() %>%
    rename(site=transekt, count=paare, year=aasta) %>%
    select(site,year,count)
  
  yrange<-data1 %>% ungroup() %>% filter(skeem==skeemkood) %>% select(aasta) %>% unlist() %>% unname() %>% range()
  #sites<-vwide %>% ungroup() %>% filter(liik==sp & skeem==skeemkood) %>% select(transekt) %>%
  #  unlist() %>% unname()
  sites<-data1 %>% ungroup() %>% filter(skeem==skeemkood) %>% select(transekt) %>% distinct() %>% unlist() %>% unname()
  
  obs<-data1 %>% ungroup() %>% filter(skeem==skeemkood) %>% select(transekt,aasta) %>% distinct() %>% rename(site=transekt, year=aasta) %>% mutate(cens=1)
  
  data3<-expand_grid(site=sites, year=seq(yrange[1],yrange[2])) %>%
    left_join(obs, by=c("site","year")) %>%
    left_join(data2, by=c("site","year")) %>%
    mutate(
      count=replace(count, which(is.na(count)&!is.na(cens)),0)
    ) %>%
    select(-cens)
  
  
  m <- try( suppressMessages(trim(count~site+year, data=data3, model=2, overdisp=TRUE)) )
  if(!inherits(m, "try-error")) {
    
    resultsuccess$success[i]<-TRUE

    slope1<-rtrim::overall(m)$slope
    trendline1 <- rtrim::trendlines(rtrim::overall(m))
    
    slopedata1<-slope1 %>% mutate(liik=sp, euring=euringcode, skeem=skeemkood)
    trendlinedata1<-trendline1 %>% mutate(liik=sp, euring=euringcode, skeem=skeemkood) 
    
    if(i==1) {
      slopedata <- slopedata1
      trendlinedata <- trendlinedata1
    } else {
      slopedata <- slopedata %>% bind_rows(slopedata1)
      trendlinedata <- trendlinedata %>% bind_rows(trendlinedata1)
    }
    
  }
  
  pb$tick()
}






# PR0065

# kontrolli, kas PR0065 tulemus on olemas

onltoverview <- read_csv2("G:/analyys/20250312_pecbms/04_ONLT/Estonia_6/overview.csv") %>%
  mutate(
    # hallvaresed on seal liigi täpsusega, st ka EURING kood
    species_number=replace(species_number, which(species_number==15670), 15673)
  )





resultsuccessstrata <- resultsuccess %>% ungroup() %>% filter(success==TRUE) %>% select(liik) %>% distinct() %>%
  left_join(eestilinnud %>% select(liik,euring), by="liik") %>%
  left_join(
    onltoverview %>% select(species_number, success) %>% rename(euring=species_number),
    by="euring"
  ) %>%
  filter(!is.na(success))



n_iter<-nrow(resultsuccessstrata)
pb <- progress_bar$new(format = "  töötlen [:bar] :percent eta: :eta", total = n_iter, clear = FALSE, width= 60)
for(i in 1:nrow(resultsuccessstrata)) {
  
  # i<-8
  
  euringcode<-resultsuccessstrata$euring[i]
  sp<-resultsuccessstrata$liik[i]
  
  onltresultfile<-onltoverview %>% filter(species_number==euringcode) %>% select(ss_combinations) %>% unlist() %>% unname()
  onltresultfilename<-paste0("G:/analyys/20250312_pecbms/02_Inputs/",onltresultfile,".RData")
  load(onltresultfilename)
  
  
  resultcp<-tibble(
    cp=seq(1,nrow(result$time.totals)),
    time=result$time.totals$time
  )
  testcp<-resultcp %>%
    filter(time %in% c(2000) ) %>%
    select(cp) %>% unlist() %>% unname()
  slope1 <- rtrim::overall(result, changepoints=testcp)$slope
  trendline1 <- rtrim::trendlines(rtrim::overall(result, changepoints=testcp))
  testcp<-resultcp %>%
    filter(time %in% c(2004) ) %>%
    select(cp) %>% unlist() %>% unname()
  slope2 <- rtrim::overall(result, changepoints=testcp)$slope
  trendline2 <- rtrim::trendlines(rtrim::overall(result, changepoints=testcp))
  
  slopedata1<-slope1 %>% mutate(liik=sp, euring=euringcode, skeem="PR0065-2000") %>%
    bind_rows(
      slope2 %>% mutate(liik=sp, euring=euringcode, skeem="PR0065-2004")
    )
  trendlinedata1<-trendline1 %>% mutate(liik=sp, euring=euringcode, skeem="PR0065-2000") %>%
    bind_rows(
      trendline2 %>% mutate(liik=sp, euring=euringcode, skeem="PR0065-2004") 
    )
  
  
  #if(i==1) {
  #  slopedata <- slopedata1
  #  trendlinedata <- trendlinedata1
  #} else {
    slopedata <- slopedata %>% bind_rows(slopedata1)
    trendlinedata <- trendlinedata %>% bind_rows(trendlinedata1)
  #}
  
  pb$tick()
  
}


save(eestilinnud, slopedata, trendlinedata, file="data/dsresults-20251016.RData")








