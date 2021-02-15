require(ggplot2)
require(dplyr)

dir_save = "/Users/Yuki/Dropbox/業務/拡大種/2020/output"


# 必要な魚種の抽出 ------------------------------------------------------
# 
# 魚種名がうまく引っ掛けられない時はunique()で魚種名リストを出してコピペする
# 総じて日本語を扱うのが苦手なので，適宜英語表記に変更（group_byは特に上手くいかなくなることが多い）
# あまりにもダメな時はRをrestart

unique(ao$species)
unique(iwate$species)
unique(miya$魚種コード)
unique(fuk$species)
unique(iba$species)

ao_a = ao %>% filter(species == "アカガレイ") %>% dplyr::rename(year = 年, method = 漁業種名)
iwa_a = iwate %>% filter(species == "アカガレイ") %>% dplyr::rename(method = 漁業種名)
miya_a = miya %>% filter(魚種コード == "アカガレイ") %>% dplyr::rename(catch = "日別水揚量(キロ)", method = 漁業種コード)
fuk_a = fuk %>% filter(species == "アカガレイ") %>% dplyr::rename(year = 年, method = 漁業種名)
iba_a = iba %>% filter(species == "アカガレイ") %>% dplyr::rename(method = 漁法)


# 各県の漁獲量の年トレンド --------------------------------------------------
pref = NULL

summary(ao_a)
ao1 = ao_a %>% dplyr::group_by(year) %>% dplyr::summarise(total = sum(catch)) %>% mutate(pref = "青森")
pref = rbind(pref, ao1)

summary(iwa_a)
iwa1 = iwa_a %>% dplyr::group_by(year) %>% dplyr::summarize(total = sum(catch)) %>% mutate(pref = "岩手")
pref = rbind(pref, iwa1)

summary(miya_a)
miya1 = miya_a %>% dplyr::group_by(year) %>% dplyr::summarize(total = sum(catch)) %>% mutate(pref = "宮城")
pref = rbind(pref, miya1)

summary(fuk_a)
fuk_a = fuk_a %>% na.omit()
# group_byでなぜか年別計算ができん->plyrに切り替え
# fuk_a = fuk_a %>% group_by(year) %>% summarize(total = catch) %>% mutate(pref = "福島")
require(plyr)
fuk1 = ddply(fuk_a, .(year), summarize, total = sum(catch))
fuk1 = fuk1 %>% mutate(pref = "福島")
head(pref, 3); head(fuk1, 3)
pref = rbind(pref, fuk1)

summary(iba_a)
iba1 = ddply(iba_a, .(year), summarize, total = sum(catch))
iba1 = iba1 %>% mutate(pref = "茨城")
pref = rbind(pref, iba1)

summary(pref)

levels(pref$pref) 
pref$pref = factor(pref$pref, levels = c("青森", "岩手", "宮城", "福島", "茨城"))
levels(pref$pref)
setwd(dir_save)
write.csv(pref, "pref_akagarei.csv")

g = ggplot(pref, aes(x = year, y = total/1000))
p = geom_point(size = 5)
l = geom_line(size = 1)
lab = labs(x = "年", y = "漁獲量（トン）")
f = facet_wrap(~ pref, ncol = 2, scales = "free")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
fig_pref = g+l+p+f+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1990, 2019, by = 2), expand = c(0, 0.5))
#+scale_y_continuous(expand = c(0,0),limits = c(0, 520))

setwd(dir_save)
ggsave(file = "fig_pref.png", plot = fig_pref, units = "in", width = 11.69, height = 8.27)



# 漁法別の漁獲量の年トレンド -------------------------------------------------
ao_a2 = ao_a %>% filter(catch > 0)
iwa_a2 = iwa_a %>% filter(catch > 0)
miya_a2 = miya_a %>% filter(catch > 0)
fuk_a2 = fuk_a %>% filter(catch > 0)
iba_a2 = iba_a %>% filter(catch > 0)

unique(ao_a2$method)
unique(iwa_a2$method)
unique(miya_a2$method)
unique(fuk_a2$method)
unique(iba_a2$method)

tag_a = data_frame(method = unique(ao_a2$method), method2 = c("沖底", "小底", "その他",　"その他", "刺網"))
tag_i = data_frame(method = unique(iwa_a2$method), method2 = c("沖底", "その他", "その他", "刺網", "その他", "その他", "その他", "その他", "その他"))
tag_m = data_frame(method = unique(miya_a2$method), method2 = c("刺網", "沖底", "その他", "小底", "その他", "小底", "その他", "その他", "その他", "小底", "その他", "その他", "その他"))
tag_f = data_frame(method = unique(fuk_a2$method), method2 = c("沖底", "小底", "刺網", "その他"))
tag_ib = data_frame(method = unique(iba_a2$method), method2 = c("沖底", "小底", "小底", "その他", "その他", "その他", "その他", "その他", "その他", "その他"))

ao_a2 = right_join(ao_a2, tag_a, by = "method") %>% select(year, method2, catch)
iwa_a2 = right_join(iwa_a2, tag_i, by = "method") %>% select(year, method2, catch)
miya_a2 = right_join(miya_a2, tag_m, by = "method") %>% select(year, method2, catch)
fuk_a2 = right_join(fuk_a2, tag_f, by = "method") %>% select(year, method2, catch)
iba_a2 = right_join(iba_a2, tag_ib, by = "method") %>% select(year, method2, catch)

pre = c("ao_a2", "iwa_a2", "miya_a2", "fuk_a2", "iba_a2")
method = NULL
for(i in 1:length(pre)){
  data = get(pre[i]) 
  data = ddply(data, .(year, method2), summarize, total = sum(catch))
  method = rbind(method, data)
}
method2 = ddply(method, .(year, method2), summarize, total = sum(total))
summary(method2)

levels(method2$method2)
method2$method2 = factor(method2$method2, levels = c("沖底", "小底", "刺網", "その他"))

check = ddply(method2, .(year), summarize, total = sum(total))
summary(check)

g = ggplot(method2, aes(x = year, y = total/1000, fill = method2))
b = geom_bar(stat = "identity", width = 0.5, colour = "black")
lab = labs(x = "年", y = "漁獲量 (トン)", fill = "漁業種")
col_catch = c("grey50", "white", "grey0", "grey90")
c = scale_fill_manual(values = col_catch)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.5), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.5), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(1.5)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
fig_method = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0), breaks=seq(1990, 2019, by = 3))+scale_y_continuous(expand = c(0,0),limits = c(0, 920))

setwd(dir_save)
ggsave(file = "fig_method.png", plot = fig_method, units = "in", width = 11.69, height = 8.27)




# check ---------------------------------------------------------
c = NULL
for(i in 1:length(pre)){
  data = get(pre[i]) %>% mutate(pref = paste0(pre[i])) 
  data = ddply(data, .(pref, method2), summarize, total = sum(catch))
  c = rbind(c, data)
}
