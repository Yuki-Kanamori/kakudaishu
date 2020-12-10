require(ggplot2)
require(dplyr)

dir_save = "/Users/Yuki/Dropbox/業務/拡大種/2020/output"


# 各県の漁獲量の年トレンド --------------------------------------------------
# 魚種名がうまく引っ掛けられない時はunique()で魚種名リストを出してコピペする
unique(ao$species)
unique(iwate$species)
unique(miya$魚種コード)
unique(fuk$species)
unique(iba$species)

pref = NULL

# 無い年がある！！
ao_a = ao %>% filter(species == "アカガレイ")
summary(ao_a)
%>% group_by(年) %>% summarise(total = sum(catch))

iwa_a = iwate %>% filter(species == "アカガレイ")
summary(iwa_a)
iwa_a = iwa_a %>% group_by(year) %>% summarize(total = sum(catch)) %>% mutate(pref = "岩手")
pref = rbind(pref, iwa_a)

miya_a = miya %>% filter(魚種コード == "アカガレイ") %>% dplyr::rename(catch = "日別水揚量(キロ)")
summary(miya_a)
miya_a = miya_a %>% group_by(year) %>% summarize(total = sum(catch)) %>% mutate(pref = "宮城")
pref = rbind(pref, miya_a)

fuk_a = fuk %>% filter(species == "アカガレイ")
summary(fuk_a)
fuk_a = fuk_a %>% group_by(年) %>% summarize(total = catch) %>% mutate(pref = "福島")
pref = rbind(pref, fuk_a)

iba_a = iba %>% filter(species == "アカガレイ")
summary(iba_a)
iba_a = iba_a %>% group_by(year) %>% summarize(total = sum(catch)) %>% mutate(pref = "茨城")
pref = rbind(pref, iba_a)





# 漁法別の漁獲量の年トレンド -------------------------------------------------


