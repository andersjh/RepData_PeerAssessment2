library(dplyr)
library(lubridate)
library(ggplot2)
library(R.utils)

if (!file.exists("StormData.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                "StormData.csv.bz2")
  bunzip2("StormData.csv.bz2")
}

#raw.data = read.table("StormData.csv", sep=",", header = T, stringsAsFactors = F)
base.data = raw.data %>%
    select(BGN_DATE,EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP) %>%
    filter(FATALITIES>0 | INJURIES>0 | PROPDMG>0, CROPDMG>0)

print(unique(base.data$EVTYPE))
print(unique(base.data$PROPDMGEXP))
print(unique(base.data$CROPDMGEXP))

base.data$PROPDMGEXP = toupper(base.data$PROPDMGEXP)
base.data$CROPDMGEXP = toupper(base.data$CROPDMGEXP)
base.data = base.data %>%
  mutate(prop.factor = ifelse(PROPDMGEXP == "B", 1, 
                         ifelse(PROPDMGEXP=="M", .001,
                            ifelse(PROPDMGEXP=="K", .000001,0)))) %>%
  mutate(crop.factor = ifelse(CROPDMGEXP == "B", 1, 
                          ifelse(CROPDMGEXP=="M", .001,
                            ifelse(CROPDMGEXP=="K", .000001,0)))) %>%
  mutate(total.cost = (PROPDMG * prop.factor) + (CROPDMG * crop.factor))

# organaize hurricanes
cur.type = grepl("hurricane", base.data$EVTYPE, ignore.case = T) 
base.data[cur.type,2] = "HURRICANE"
# organaize thunderstorm
cur.type = grepl("thunderstorm", base.data$EVTYPE, ignore.case = T) 
base.data[cur.type,2] = "THUNDERSTORM"
# organaize flooding
cur.type = grepl("flood", base.data$EVTYPE, ignore.case = T) 
base.data[cur.type,2] = "FLOOD"
# organaize flooding
cur.type = grepl("tornado", base.data$EVTYPE, ignore.case = T) 
base.data[cur.type,2] = "TORNADOE"# 
#organaize heat
cur.type = grepl("heat", base.data$EVTYPE, ignore.case = T) 
base.data[cur.type,2] = "HEAT"
#combine winters storms/blizzards
cur.type = grepl("blizz", base.data$EVTYPE, ignore.case = T) 
base.data[cur.type,2] = "WINTER STORM"
cur.type = grepl("winter storm", base.data$EVTYPE, ignore.case = T) 
base.data[cur.type,2] = "WINTER STORM"
# assuming tstm = tropical storm
cur.type = grepl("tstm win", base.data$EVTYPE, ignore.case = T) 
base.data[cur.type,2] = "TROPICAL STORM"

print(unique(base.data$EVTYPE))

sum.data = base.data %>%
  select(EVTYPE,FATALITIES,INJURIES,total.cost) %>%
  group_by(EVTYPE) %>%
  summarise(TOTAL_FATALITIES = sum(FATALITIES),
            TOTAL_INJURIES = sum(INJURIES),
            TOTAL_COST_BILLIONS = sum(total.cost))

sum.fatalities = sum.data %>% 
  select(EVTYPE, TOTAL_FATALITIES) %>%
  arrange(desc(TOTAL_FATALITIES), EVTYPE)

sum.injuries = sum.data %>% 
  select(EVTYPE, TOTAL_INJURIES) %>%
  arrange(desc(TOTAL_INJURIES), EVTYPE)

sum.cost = sum.data %>% 
  select(EVTYPE, TOTAL_COST_BILLIONS) %>%
  arrange(desc(TOTAL_COST_BILLIONS), EVTYPE)


