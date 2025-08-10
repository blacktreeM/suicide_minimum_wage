library(readxl); library(dplyr); library(tidyr); library(pdftools)
library(stringr); library(zoo); library(gridExtra); library(htmlTable)
if(F){
  rm(list=ls())
  setwd("C:/Users/masan/Documents/suicide")
  #https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00450013&kikan=00450&tstat=000001025143&cycle=8&result_page=1&tclass1val=0
  #性・都道府県別自殺死亡数・死亡率（人口10万対）の年次比較　－昭和25・30・35・40・45・50・55・60・平成２・７・12～15年－（自殺死亡率）
  # original csv file is corrupt and prefecture names not recognizable, converted to excel on google sheet
  # s26 is counts, s27 is rate
  data = read_excel("s26.xlsx"); head(data) # heisai 12-15, 2000-2003
  data = na.omit(data[c(5, 8:nrow(data)), c(1, 12:15)]); head(data)
  colnames(data) = c('pref', 'y2000', 'y2001', 'y2002', 'y2003')
  data$pref = gsub('\\s+', '', data$pref); unique(data$pref)
  data = data %>% mutate(across(.cols = -c(pref), .fns = ~as.numeric(gsub(' ', '', .)))) %>% 
    pivot_longer(cols = contains('y'), names_to = 'year', values_to = 'suicide') %>% 
    mutate(year = as.numeric(gsub('y', '', year))) %>% filter(pref!="不詳"); head(data); table(data$year)
  data = data[-c(1:(48*4)), ] #remove overall
  men = data[1:(48*4),]; head(men); tail(men)
  women = data[-c(1:(48*4)),]; head(women); tail(women)
  men$gender = 'male'
  women$gender = 'female'
  Data = rbind(men, women); aggregate(suicide~gender, data = Data, mean); table(Data$year)
  #
  data = read_excel("s27.xlsx"); head(data) # heisai 12-15, 2000-2003
  data = na.omit(data[c(5, 8:nrow(data)), c(1, 12:15)]); head(data)
  colnames(data) = c('pref', 'y2000', 'y2001', 'y2002', 'y2003')
  data$pref = gsub('\\s+', '', data$pref); unique(data$pref)
  data = data %>% mutate(across(.cols = -c(pref), .fns = as.numeric)) %>% 
    pivot_longer(cols = contains('y'), names_to = 'year', values_to = 'rate') %>% 
    mutate(year = as.numeric(gsub('y', '', year))); head(data); table(data$year)
  data = data[-c(1:(48*4)), ] #remove overall
  men = data[1:(48*4),]; head(men); tail(men)
  women = data[-c(1:(48*4)),]; head(women); tail(women)
  men$gender = 'male'
  women$gender = 'female'
  data = rbind(men, women); aggregate(rate~gender, data = data, mean); table(data$year)
  data0003 = merge(data, Data, by = c('pref', 'year', 'gender'))
  # https://www.cspss.jp/pdf/%E5%90%84%E9%83%BD%E9%81%93%E5%BA%9C%E7%9C%8C%E3%81%AB%E3%81%8A%E3%81%91%E3%82%8B%E8%87%AA%E6%AE%BA%E3%81%AE%E6%A6%82%E8%A6%81%EF%BC%88%E5%B9%B3%E6%88%9016%E5%B9%B4%EF%BD%9E%E5%B9%B3%E6%88%9018%E5%B9%B4%EF%BC%89.pdf
  # heisei 16-18, 2004-2006 
  file = '各都道府県における自殺の概要（平成16年～平成18年）.pdf'
  pdf_text = pdf_text(file)
  lines = str_split(pdf_text[1], "\n")[[1]]
  lines = lines[lines != ""]
  lines = gsub("\\s+", " ", lines)
  lines = gsub(",", "", lines)
  df = read.table(text = pdf_text[1:3], fill = TRUE)
  delete = paste(c('総数', '計', '男', 'ページ', '都道府県'), collapse='|')
  df = df %>% filter(!if_any(.cols = everything(), .fns = ~ grepl(delete, .)))
  df16 = df[seq(1, nrow(df), 3), c(3, 5)]
  df18 = df[seq(3, nrow(df), 3), c(3, 5)]
  df17 = df[seq(2, nrow(df), 3),]; tail(df17)
  bottom = df17[nrow(df17),c(3, 5)]
  (pref = df17[1:47,1])
  pref = c(pref, '全国')
  df17 = df17[1:47,c(4, 6)]
  colnames(bottom) = colnames(df17)
  df17 = rbind(df17, bottom); tail(df17)
  df = cbind(pref, df16, df17, df18); head(df)
  colnames(df) = c('pref', 'men2004', 'women2004', 'men2005', 'women2005', 'men2006', 'women2006'); head(df)
  df = df %>% mutate(across(.cols = -c(pref), .fns = ~gsub(',', '', .)),
                     across(.cols = -c(pref), .fns = as.numeric)) %>% 
    pivot_longer(cols = contains('200'), names_to = 'year', values_to = 'suicide'); head(data); table(df$year)
  df = df %>% mutate(gender = case_when(
    grepl("women", year) ~ "female", grepl("men", year) ~ "male"), 
    year = as.numeric(gsub("^(men|women)", "", year))) ; head(df)
  data0406 = df
  ########################################################33
  # 2007-2008 pdf files https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/hukushi_kaigo/seikatsuhogo/jisatsu/jisatsu_kiso_h19-20.html
  folder = '/H1920-SOKUHOU01/'
  location = paste0(getwd(), folder)
  (files = list.files(location)); length(files)
  mk = data.frame(matrix(nrow=0, ncol=4))
  colnames(mk) = c('pref', 'year', 'suicide', 'gender')
  for (file in files){
    h = read.table(text = pdf_text(paste0(location, file)), fill = TRUE)
    pref = h[1,1]; print(pref)
    y2007 = h[4:5, 2]
    y2008 = h[4:5, 3]
    d = data.frame(pref = pref, y2007 = y2007, y2008 = y2008, gender = c('male', 'female'))
    d = d %>% pivot_longer(cols = c(y2007, y2008), names_to = 'year', values_to = 'suicide') %>% 
      mutate(year = as.numeric(gsub('y', '', year)), suicide = as.numeric(gsub(',', '', suicide)))
    mk = rbind(mk, d)
  }
  summary(mk); table(mk$pref); table(mk$year)
  data0708 = mk
  # for 2014, 対前年比columns added 4, 6, 8, so use this as base
  (files = list.files(getwd(), pattern = "県・自殺日・住居地")); length(files)
  (files = setdiff(files, "2014-0-2014CDR-14-00021_A5表(県・自殺日・住居地).xls"))
  df = read_xls("2014-0-2014CDR-14-00021_A5表(県・自殺日・住居地).xls", sheet = 2) # sheet 2 is men
  (df = df[7:nrow(df), c(2, 3, 5)])
  colnames(df) = c('pref', 'suicide', 'rate')
  df$year = 2014; head(df)
  for (i in 1:15){
    year = substring(files[i], 1, 4); print(year)
    df1 = read_xls(files[i], sheet = 2)
    df1 = df1[7:nrow(df1), 2:4]
    colnames(df1) = c('pref', 'suicide', 'rate')
    df1$year = year
    df = rbind(df, df1)
  }
  table(df$year)
  men = df %>% filter(pref != '不明' & pref != '都道府県名') %>% mutate(gender = 'male')
  ## women
  df = read_xls("2014-0-2014CDR-14-00021_A5表(県・自殺日・住居地).xls", sheet = 3) # sheet 3 women
  (df = df[7:nrow(df), c(2, 3, 5)])
  colnames(df) = c('pref', 'suicide', 'rate')
  df$year = 2014; head(df)
  for (i in 1:15){
    year = substring(files[i], 1, 4); print(year)
    df1 = read_xls(files[i], sheet = 3)
    df1 = df1[7:nrow(df1), 2:4]
    colnames(df1) = c('pref', 'suicide', 'rate')
    df1$year = year
    df = rbind(df, df1); print(i)
  }
  table(df$year)
  women = df %>% filter(pref != '不明' & pref != '都道府県名') %>% mutate(gender = 'female')
  data0924 = rbind(men, women)
  data0924 = data0924 %>% mutate(across(.cols = c(suicide, rate, year), .fns = as.numeric))
  #
  data = data0003 %>% bind_rows(data0406) %>% bind_rows(data0708) %>% bind_rows(data0924); summary(data); table(data$year)
  unique(data$pref)
  data = data %>% mutate(pref = gsub('県', '', pref), pref = gsub('東京都', '東京', pref), pref = gsub('府', '', pref))
  length(unique(data$pref))
  save(data, file = 'suicide2000.RDa')
}
####################
if(F){
  setwd("C:/Users/masan/Documents/suicide")
load('suicide2000.RDa'); table(data$year); aggregate(suicide~year, data=data, mean); head(data)
# 2007-2008
(data0708 = data %>% filter(year %in% 2007:2008) %>% group_by(year, gender) %>% 
  summarize(suicide = sum(suicide)) %>% ungroup() %>% mutate(pref = '全国'))
suicide = data %>% bind_rows(data0708); aggregate(suicide~year, data=data, mean)
aggregate(rate~year, data=suicide, mean)
### covariates
x = 'suicide2000.xlsx'
data = read_excel(x, sheet = 1)
excel_sheets(x)
(year = substring(data[2,2], 1, 4))
var = as.character(data[5, seq(4, ncol(data), 2)]); var # only even numbers columns, starts at 4
data = data[6:nrow(data), c(1, 2, seq(4, ncol(data), 2))]
colnames(data) = c('fips', 'pref', var)
data$year = year
colnames(data)
for (i in 2:25){
  df = read_excel(x, sheet = i)
  year = substring(df[2,2], 1, 4)
  var = as.character(df[5, seq(4, ncol(df), 2)]) 
  df = df[6:nrow(df), c(1, 2, seq(4, ncol(df), 2))]
  colnames(df) = c('fips', 'pref', var)
  df$year = year
  data = bind_rows(data, df); print(i)
}
table(data$year); unique(data$pref)
data = data %>% filter(!(pref %in% c("調査又は集計していないもの", "データが得られないもの", 
                                     "数値が秘匿されているもの")) & !is.na(pref))
(oldnames = colnames(data))
colnames(data) = c('fips', 'pref', 'popMen', 'popWomen', 'year', 'mw', 'age15', 'age65',
                   'patient', 'welfare', 'unmarried', 'alone', 'income1', 'college',
                   'unempMen', 'unempWomen', 'income2', 'income3', 'cpi')
numerical = setdiff(colnames(data), 'pref')
data[, numerical] = lapply(data[, numerical], function(x) as.numeric(gsub(',', '', x))); summary(data)
setdiff(unique(data$pref), unique(suicide$pref))
X = data %>% mutate(pref = gsub('県', '', pref), pref = gsub('東京都', '東京', pref), pref = gsub('府', '', pref))
data = suicide %>% left_join(X, by = c('year', 'pref')) %>% 
  mutate(sr = ifelse(gender=='male', 100000*suicide/popMen, 100000*suicide/popWomen)); summary(data)
data %>% filter(pref == '青森' & gender=='male') %>% select(year, suicide, rate, sr) %>% arrange(year)
data$Pref = 'Japan'
data$Pref[data$pref == '北海道'] = 'Hokkaido'
data$Pref[data$pref == '青森'] = 'Aomori'
data$Pref[data$pref == '岩手'] = 'Iwate'
data$Pref[data$pref == '宮城'] = 'Miyagi'
data$Pref[data$pref == '秋田'] = 'Akita'
data$Pref[data$pref == '山形'] = 'Yamagata'
data$Pref[data$pref == '福島'] = 'Fukushima'
data$Pref[data$pref == '茨城'] = 'Ibaraki'
data$Pref[data$pref == '栃木'] = 'Tochigi'
data$Pref[data$pref == '群馬'] = 'Gunma'
data$Pref[data$pref == '埼玉'] = 'Saitama'
data$Pref[data$pref == '千葉'] = 'Chiba'
data$Pref[data$pref == '東京'] = 'Tokyo'
data$Pref[data$pref == '神奈川'] = 'Kanagawa'
data$Pref[data$pref == '新潟'] = 'Niigata'
data$Pref[data$pref == '富山'] = 'Toyama'
data$Pref[data$pref == '石川'] = 'Ishikawa'
data$Pref[data$pref == '福井'] = 'Fukui'
data$Pref[data$pref == '山梨'] = 'Yamanashi'
data$Pref[data$pref == '長野'] = 'Nagano'
data$Pref[data$pref == '岐阜'] = 'Gifu'
data$Pref[data$pref == '静岡'] = 'Shizuoka'
data$Pref[data$pref == '愛知'] = 'Aichi'
data$Pref[data$pref == '三重'] = 'Mie'
data$Pref[data$pref == '滋賀'] = 'Shiga'
data$Pref[data$pref == '京都'] = 'Kyoto'
data$Pref[data$pref == '大阪'] = 'Osaka'
data$Pref[data$pref == '兵庫'] = 'Hyogo'
data$Pref[data$pref == '奈良'] = 'Nara'
data$Pref[data$pref == '和歌山'] = 'Wakayama'
data$Pref[data$pref == '鳥取'] = 'Tottori'
data$Pref[data$pref == '島根'] = 'Shimane'
data$Pref[data$pref == '岡山'] = 'Okayama'
data$Pref[data$pref == '広島'] = 'Hiroshima'
data$Pref[data$pref == '山口'] = 'Yamaguchi'
data$Pref[data$pref == '徳島'] = 'Tokushima'
data$Pref[data$pref == '香川'] = 'Kagawa'
data$Pref[data$pref == '愛媛'] = 'Ehime'
data$Pref[data$pref == '高知'] = 'Kochi'
data$Pref[data$pref == '福岡'] = 'Fukuoka'
data$Pref[data$pref == '佐賀'] = 'Saga'
data$Pref[data$pref == '長崎'] = 'Nagasaki'
data$Pref[data$pref == '熊本'] = 'Kumamoto'
data$Pref[data$pref == '大分'] = 'Oita'
data$Pref[data$pref == '宮崎'] = 'Miyazaki'
data$Pref[data$pref == '鹿児島'] = 'Kagoshima'
data$Pref[data$pref == '沖縄'] = 'Okinawa'
save(data, file = 'suicide2000X.RDa')
}
####################
# synthetic control
library(Synth); library(ggplot2); library(SCtools)
setwd("C:/Users/masan/Documents/suicide")
load('suicide2000X.RDa'); table(data$year); aggregate(suicide~year, data=data, mean); head(data)
#japan = data %>% filter(pref == '全国') %>% select(year, gender, sr) %>% arrange(gender, year); japan
(japan = data %>% filter(pref %in% c('全国', '福島', '宮城', '岩手')) %>% 
  group_by(year, gender) %>% arrange(year) %>% 
  mutate(suicide = sum(suicide), popMen = sum(popMen), popWomen = sum(popWomen),
         sr = ifelse(gender=='male', 100000*suicide/popMen, 100000*suicide/popWomen),
         id = row_number(), Pref = 'Japan') %>% filter(id==1) %>% ungroup())
japan_men = japan %>% filter(gender=='male')
japan_women = japan %>% filter(gender=='female')
data %>% filter(pref == '福島' & gender=='female') %>% select(year, suicide, sr)%>% arrange(year)
data %>% filter(pref == '宮城' & gender=='female') %>% select(year, suicide, sr)%>% arrange(year)
data %>% filter(pref == '岩手' & gender=='female') %>% select(year, suicide, sr)%>% arrange(year)
treatY = data %>% filter(pref %in% c('福島', '宮城', '岩手')) %>% group_by(year, gender) %>% 
  mutate(suicide = sum(suicide), popMen = sum(popMen), popWomen = sum(popWomen),
         sr = ifelse(gender=='male', 100000*suicide/popMen, 100000*suicide/popWomen),
         id = row_number(), Pref = 'Iwate/Fukushima/Miyagi', fips = 48000) %>% filter(id==1) %>% ungroup() %>% 
  select(fips, Pref, year, sr, gender); head(treatY)
x = c("age15", "age65", 'college', "patient", "welfare", 'alone', "unempMen", "unempWomen", 'income3', 'mw', 'cpi') 
treatX = data %>% filter(pref %in% c('福島', '宮城', '岩手')) %>% group_by(year, gender) %>% 
  summarize(across(.cols = x, .fns = mean)) %>% 
  ungroup(); treatX %>% filter(gender=='male') %>% head(., 11)
treat = merge(treatY, treatX, by = c('year', 'gender')); head(treat); summary(treat)
data = data %>% bind_rows(treat)
data %>% filter(Pref=='treat')
colnames(data)
data %>% filter(Pref == 'Tokyo' & gender=='male' & year< 2011) %>% 
  select(-pref, -gender, -rate, -popMen, -popWomen, -fips, -Pref, -suicide, -income1) %>% arrange(year)
data %>% filter(Pref == 'Fukushima' & gender=='male') %>% select('year', x)%>% arrange(year)
max(aggregate(sr~year, data = subset(data, gender=='male'), max)[2])
max(aggregate(sr~year, data = subset(data, gender=='female'), max)[2])
# men
synth_men = dataprep(foo = subset(data, gender=='male'),
                     predictors = x,
                     predictors.op = "mean",
                     #special.predictors = list(list('sr', 2000, 'mean'), list('sr', 2005, 'mean'), list('sr', 2010, 'mean')),
                     time.predictors.prior = 2000:2010,
                     dependent = 'sr',
                     unit.variable = "fips",
                     unit.names.variable = "Pref",
                     time.variable = "year",
                     treatment.identifier = 'Iwate/Fukushima/Miyagi',
                     controls.identifier = setdiff(unique(data$Pref), 
                                                   c('Iwate', 'Miyagi', 'Fukushima', "Japan", 'Iwate/Fukushima/Miyagi')),
                     time.optimize.ssr = 2000:2010,
                     time.plot = 2000:2024)
men = synth(data.prep.obj = synth_men, Sigf.ipop=7, verbose=T)
# women
synth_women = dataprep(foo = subset(data, gender=='female'),
                     predictors = x,
                     predictors.op = "mean",
                     #special.predictors = list(list('sr', 2000, 'mean'), list('sr', 2005, 'mean'), list('sr', 2010, 'mean')),
                     time.predictors.prior = 2000:2010,
                     dependent = 'sr',
                     unit.variable = "fips",
                     unit.names.variable = "Pref",
                     time.variable = "year",
                     treatment.identifier = 'Iwate/Fukushima/Miyagi',
                     controls.identifier = setdiff(unique(data$Pref), 
                                                   c('Iwate', 'Miyagi', 'Fukushima', "Japan", 'Iwate/Fukushima/Miyagi')),
                     time.optimize.ssr = 2000:2010,
                     time.plot = 2000:2024)
women = synth(data.prep.obj = synth_women, Sigf.ipop=7)
spec = list(geom_vline(xintercept = 2011, color = 'grey'),
              xlab(''), theme_classic(), theme(plot.title = element_text(hjust = 0.5)),
              scale_color_manual(values = c('black', 'darkgrey', 'darkgrey'), 
                                 breaks = c('Iwate/Fukushima/Miyagi', 'Synthetic control', 'Japan')),
              theme(legend.position = 'bottom'),
              guides(color=guide_legend(title="")))
(g1 = ggplot()+ ggtitle('(a) Male suicide rate')+
    geom_line(data = subset(japan_men), linetype = 'dotted',
              aes(x=2000:2024, y = sr, color='Japan'), linewidth = 1)+
    geom_line(aes(x=2000:2024, y = synth_men$Y0plot%*%men$solution.w, color='Synthetic control'),
              linewidth = 1) + spec +
    geom_line(aes(x=2000:2024, y = synth_men$Y1plot, color='Iwate/Fukushima/Miyagi'), linewidth = 1)+
    scale_y_continuous(name = "", limits = c(0, 50), expand=c(0,0)))
(g2 = ggplot()+ ggtitle('(b) Female suicide rate')+
    geom_line(data = subset(japan_women, gender == 'female'), linetype = 'dotted',
              aes(x=2000:2024, y = sr, color='Japan'), linewidth = 1)+
    geom_line(aes(x=2000:2024, y = synth_women$Y0plot%*%women$solution.w, color='Synthetic control'),
              linewidth = 1) + spec +
    geom_line(aes(x=2000:2024, y = synth_women$Y1plot, color='Iwate/Fukushima/Miyagi'), linewidth = 1)+
    scale_y_continuous(name = "", limits = c(0, 20), expand=c(0,0)))
png("synth_plot.png", width = 600, height = 800)
marrangeGrob(list(g1, g2), ncol = 1, nrow =2, top=NULL); dev.off()
###############
(gaps_men = synth_men$Y1plot - (synth_men$Y0plot%*%men$solution.w))
(gaps_women = synth_women$Y1plot - (synth_women$Y0plot%*%women$solution.w))
###### average gap for women
(post_effect = gaps_women[-c(1:11)])  
mean(post_effect)
mean(synth_women$Y1plot[1:11])# pre-period mean
mean(synth_women$Y1plot[-c(1:11)]) #post mean
#####
(gap1 = ggplot()+ labs(title = '(a) Male suicide rate') + xlab('') +
  geom_vline(xintercept = 2011, color = 'grey') +
  geom_hline(yintercept = 0) + theme_classic()+
    geom_line(aes(x=2000:2024, y = gaps_men), linewidth = 1)+
    scale_y_continuous(name = "", limits = c(-6, 6), expand=c(0,0)) +
    theme(plot.title = element_text(hjust = 0.5)))
(gap2 = ggplot()+ ggtitle('(b) Female suicide rate') + xlab('')+ 
  geom_vline(xintercept = 2011, color = 'grey') +
  geom_hline(yintercept = 0) + theme_classic()+
  geom_line(aes(x=2000:2024, y = gaps_women), linewidth = 1)+
  scale_y_continuous(name = "", limits = c(-6, 6), expand=c(0,0)) +
    theme(plot.title = element_text(hjust = 0.5)))
png("synth_gap_plot.png", width = 600, height = 800)
marrangeGrob(list(gap1, gap2), ncol = 1, nrow =2, top=NULL); dev.off()
#################
(group_men = synth.tab(dataprep.res = synth_men, synth.res = men)$tab.w)
(group_women = synth.tab(dataprep.res = synth_women, synth.res = women)$tab.w)
(group_men = group_men %>% filter(w.weights>0) %>% arrange(desc(w.weights)))
(group_women = group_women %>% filter(w.weights>0) %>% arrange(desc(w.weights)))
(group_men = group_men %>% mutate(x = paste0(unit.names, ' (', w.weights, ')')) %>% select(x) %>% mutate(id = 1:nrow(group_men)))
(group_women = group_women %>% mutate(y = paste0(unit.names, ' (', w.weights, ')')) %>% select(y)%>% mutate(id = 1:nrow(group_women)))
(controls = group_men %>% full_join(group_women) %>% select(-id))
colnames(controls) = c('Male suicide rate', 'Female suicide rate')
writeLines(htmlTable(controls, rnames = F), "Table_synthetic.html")
###############
(men_x = synth.tab(dataprep.res = synth_men, synth.res = men)$tab.pred)
(women_x = synth.tab(dataprep.res = synth_women, synth.res = women)$tab.pred)
(x = cbind(men_x[,-3], women_x[, -1]))
(x = round(x, 1))
var_name = c('Percent young (age < 15)', 'Percent old (age > 65)',  'Percent college graduate',
             "New admissions in psychiatric hospitals", 'Number of households receiving public assistance',
             "Percent living alone", 
             'Male unemployed rate', 'Female unemployed rate', 'Income per capita (in 1,000 yen)',
             'Minimum wage (2010)',  'Cost of living index')
(summary_table = as.data.frame(cbind(var_name, x)))
colnames(summary_table) = c('Variables', 'Iwate/Fukushima/Miyagi', 'Synthetic control (men)',
                            'Synthetic control (women)', 'Sample mean')
writeLines(htmlTable(summary_table, rnames = F), "Table_predictors.html")
##########################
#MEN = generate.placebos(synth_men, men, strategy = "multicore")
WOMEN = generate.placebos(synth_women, women, strategy = "multicore")
figureRatio = function(df){
  mk <- mspe.test(df); print(mk$p.val)
  test <- data.frame(mk$test)
  test$treat <- "control units"
  test[nrow(test), 3] <- as.character(test[nrow(test), 2])
  ggplot(data = test, aes(x = MSPE.ratios, 
                          y = reorder(unit, MSPE.ratios, max), colour = factor(treat), 
                          shape = factor(treat))) + 
    geom_point(size = 4) + 
    scale_color_manual(values = c("gray80", "black"), guide = "none") + 
    scale_shape_manual(values = c(17, 19), guide = "none") + # 16 is black circle 19 is bigger circle
    labs(y = '', x = "Post/Pre MSPE ratio", title = '') +
    theme(panel.grid.major = element_line(colour = "gray80") ,
          panel.grid.minor = element_line(colour = "gray90"), 
          panel.background = element_blank(), axis.line.x = element_line(colour = "black"), 
          axis.line.y = ggplot2::element_line(colour = "black"), 
          axis.text.y = ggplot2::element_text(colour = "black"), 
          axis.text.x = ggplot2::element_text(colour = "black"))
}
(fig = figureRatio(WOMEN))
mk
ggsave(fig, file='placebo.png', height=8, width=6)
