require(ggplot2)
require(dplyr)

dir_save = "/Users/Yuki/Dropbox/業務/拡大種/2020/output"


# 各県の漁獲量の年トレンド --------------------------------------------------
# 
# 魚種名がうまく引っ掛けられない時はunique()で魚種名リストを出してコピペする
# 総じて日本語を扱うのが苦手なので，適宜英語表記に変えながら処理（group_byは特に上手くいかなくなることが多い）
# 

unique(ao$species)
unique(iwate$species)
unique(miya$魚種コード)
unique(fuk$species)
unique(iba$species)

pref = NULL

ao_a = ao %>% filter(species == "アカガレイ") %>% dplyr::rename(year = 年)
summary(ao_a)
ao_a = ao_a %>% group_by(year) %>% summarise(total = sum(catch)) %>% mutate(pref = "青森")
pref = rbind(pref, ao_a)

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
fuk_a = fuk_a %>% na.omit() %>% dplyr::rename(year = 年)
# group_byでなぜか年別計算ができん->plyrに切り替え
# fuk_a = fuk_a %>% group_by(year) %>% summarize(total = catch) %>% mutate(pref = "福島")
require(plyr)
fuk_a = ddply(fuk_a, .(year), summarize, total = sum(catch))
fuk_a = fuk_a %>% mutate(pref = "福島")

head(pref, 3); head(fuk_a, 3)
pref = rbind(pref, fuk_a)

iba_a = iba %>% filter(species == "アカガレイ")
summary(iba_a)
iba_a = iba_a %>% group_by(year) %>% summarize(total = sum(catch)) %>% mutate(pref = "茨城")
pref = rbind(pref, iba_a)

summary(pref)

levels(pref$pref) 
pref$pref = factor(pref$pref, levels = c("青森", "岩手", "宮城", "福島", "茨城"))
levels(pref$pref)

g = ggplot(pref, aes(x = year, y = total))
p = geom_point(size = 5)
l = geom_line(size = 1)
lab = labs(x = "年級", y = "雌親魚量（トン）")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
oya = g+l+p+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2017, by = 2), expand = c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 6000))

fig14 = grid.arrange(ko, oya, ncol = 1)
ggsave(file = "fig14.png", plot = fig14, units = "in", width = 11.69, height = 8.27)



# 漁法別の漁獲量の年トレンド -------------------------------------------------


