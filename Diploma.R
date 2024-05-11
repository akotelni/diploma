####данные#####
library(readxl)
data1 <- read_excel("C:/Users/Anastasia/Desktop/Ecoplatform data.xlsx")
#data2 конвертации
data2 <- read_excel("C:/Users/Anastasia/Desktop/Ecoplatfrom conv.xlsx")
data3 <- read_excel("C:/Users/Anastasia/Desktop/Ecoplatform accounts.xlsx")
#благотворительность 0

#объединим данные
data1$телефон
data3$телефон
data4 <- data1
data4$телефон <- data4$телефон + 7*10^10
library(dplyr)
#объединим (data4) и data3
data5 <- left_join(data4, data3, by = "телефон")
x <- data5$телефон
y <- x[duplicated(x)]
table(y)
data_unique <- filter(data5, !data5$телефон %in%  unique(y))
dim(data_unique)
str(data5)
#переименнуем переменные
data6 <- subset(data_unique, select = -c(1:18, 48, 70, 71, 73:79, 84:85, 87:95))
names(data6) <- c("Help", "SelfBenefit", "Welfare", "Opinions", "EnvImp", 
                  "Motivate", "JointEfforts", "Curiosity", "WarmGlow",
                  "EnvCon", "FutureGen", "Bonuses",
                  "Competition", "Ranking", "Fashion", "Crowd", "Status", 
                  "SortingBef", "RecyclingBef", "EnvIntAf", 
                  "EcoActAf", "ConfRecycl", "Telling", 
                  "Encouraging", "PosAttitude", "EcoHabits", 
                  "Control", "NonPart", "Associations", 
                  "EnvKnow1", "EnvKnow2",
                  "EnvKnow3", "FewBonuses",
                  "Prices", "Distance", "Fullness", "Promotions", "Other",
                  "InfAcq", "InfSN", "InfStreet", "InfMedia",
                  "InfOther", "Gender", "Age", "City", "Education", "Income", "Family", 
                  "Children", "Account", "BonusesT", "Balance", "Plastic", "Aluminum", 
                  "Clothing")
#заменим na на 0 
data6$FewBonuses[is.na(data6$FewBonuses)] <-  0
data6$Prices[is.na(data6$Prices)] <-  0
data6$Distance[is.na(data6$Distance)] <-  0
data6$Fullness[is.na(data6$Fullness)] <-  0
data6$Promotions[is.na(data6$Promotions)] <-  0
data6$InfAcq[is.na(data6$InfAcq)] <-  0
data6$InfSN[is.na(data6$InfSN)] <-  0
data6$InfStreet[is.na(data6$InfStreet)] <-  0
data6$InfMedia[is.na(data6$InfMedia)] <-  0
#удалим контрольные переменные
data7 <- subset(data6, Control == 2)
data8 <- subset(data7, select = -Control)
#удалим переменную account
data9 <- subset(data8, select = -Account)
#сделаем переменную eco habits бинарной
data9$EcoHabits[data9$EcoHabits == 1] <- 0
data9$EcoHabits[data9$EcoHabits == 2] <- 1
data9$EcoHabits[data9$EcoHabits == 3] <- 1
data9$EcoHabits[data9$EcoHabits == 4] <- 1
data9$EcoHabits[data9$EcoHabits == 5] <- 1
#создадим переменную Environmental Knowledge
#объединим ответы по переменным EnvKnow1, EnvKnow2, EnvKnow3
#1 - если правильно ответил на вопрос, 0 - инче
#в EnvKnow1 1 - правтильный ответ
data9$EnvKnow1[data9$EnvKnow1 == 1] <- 1
data9$EnvKnow1[data9$EnvKnow1 == 2] <- 0
data9$EnvKnow1[data9$EnvKnow1 == 3] <- 0
data9$EnvKnow1[data9$EnvKnow1 == 4] <- 0
#в EnvKnow2 3 - правтильный ответ
data9$EnvKnow2[data9$EnvKnow2 == 1] <- 0
data9$EnvKnow2[data9$EnvKnow2 == 2] <- 0
data9$EnvKnow2[data9$EnvKnow2 == 3] <- 1
data9$EnvKnow2[data9$EnvKnow2 == 4] <- 0
#в EnvKnow3 3 - правтильный ответ
data9$EnvKnow3[data9$EnvKnow3 == 1] <- 0
data9$EnvKnow3[data9$EnvKnow3 == 2] <- 0
data9$EnvKnow3[data9$EnvKnow3 == 3] <- 1
data9$EnvKnow3[data9$EnvKnow3 == 4] <- 0
#объединим 3 вопроса в одну переменную EnvKnow
data9$EnvKnow <-  data9$EnvKnow1 + data9$EnvKnow2 + data9$EnvKnow3
data10 <- subset(data9, select = -c(EnvKnow1, EnvKnow2, EnvKnow3))
#сделаем переменную Gender бинарной
data10$Gender[data10$Gender == 1] <- "женский"
data10$Gender[data10$Gender == 2] <- "мужской"
data10$Gender[data10$Gender == "женский"] <- 1
data10$Gender[data10$Gender == "мужской"] <- 0
#сделаем переменную City бинарной
data10$City[data10$City == 1] <- 1
data10$City[data10$City == 2] <- 0
#1 - город-миллионник
#0 - город с населением < 1 000 000 чел
#переименнуем значения в переменной Education
#data10$Education[data10$Education == 1] <- "начальное"
#data10$Education[data10$Education == 2] <- "среднее общее"
#data10$Education[data10$Education == 3] <- "среднее полное"
#data10$Education[data10$Education == 4] <- "среднее профессиональное"
#data10$Education[data10$Education == 5] <- "неоконченное высшее"
#data10$Education[data10$Education == 6] <- "высшее" 
#переименнуем значения в переменной Children
data10$Children[data10$Children == 1] <- 0
data10$Children[data10$Children == 2] <- 1
data10$Children[data10$Children == 3] <- 2
data10$Children[data10$Children == 4] <- 3
#переименнуем значения в переменной Family
#data10$Family[data10$Family == 1] <- "один"
#data10$Family[data10$Family == 2] <- "отношения"
#data10$Family[data10$Family == 3] <- "брак"
#сделаем долю потраченных бонусов от общей суммы бонусов
data10$BonusesExp <-  data10$BonusesT - data10$Balance
data10$BonusesExp <-  data10$BonusesExp/data10$BonusesT
data11 <- subset(data10, select = -c(Balance, BonusesT))
#данные для облаков слов: 1 - причины неучастия знакомых, 2 - ассоциации с брендом
data_words <- subset(data11, select = c(NonPart, Associations))
data12 <- subset(data11, select = -c(NonPart, Associations, Other, InfOther))
#сдлеаем переменную Plastic
data13 <- as.data.frame(data12)
#Plastic - зависимая переменная, уберем переменную аллюминий 
#оставим
data14 <- data13
dim(data14)
#уберем выбросы
data15 <- filter(data14, data14$Plastic < 1000, data14$Plastic > 0)
data16 <- filter(data15, data15$Age < 95, data15$Age > 1)
length(data16$Aluminum[data16$Aluminum >= 1000])
data16$Aluminum[data16$Aluminum >= 1000] <- NA
data16$Aluminum[is.na(data16$Aluminum)] <-  0
data16$Clothing[data16$Clothing > 10] <- NA
data16$Clothing[is.na(data16$Clothing)] <-  0
#уберем help, welfare, SelfBenefit and Opinions, так как они ни на что не влияют
#удалим PosAttitude, тк это повтор со Status
#и JointEfforts, тк это странная переменная
data17 <- subset(data16, select = -c(Help, Welfare, Opinions, SelfBenefit, 
                                     PosAttitude, JointEfforts))
Eco_data <- data17
#View(Eco_data)
dim(Eco_data)
names(Eco_data)
str(Eco_data)
#выборка: 1048 ответов
#без дубликатов, с номером телефона и контрольным вопросом
#итоговое число наблюдений: 753
#41 переменная
summary(Eco_data$Plastic)
length(Eco_data$Clothing[Eco_data$Clothing >0])
length(Eco_data$Aluminum[Eco_data$Aluminum >0])

####Описательные статистики####

#корреляционная матрица
library(corrplot)
library(dplyr)
names(Eco_data)
data_corr <- subset(Eco_data, 
                    select = -c(FewBonuses, Prices, Distance, Fullness, 
                                Promotions, InfAcq, InfSN, InfStreet, 
                                InfMedia))
data_corr1 <- mutate_all(data_corr, as.numeric)
summary(data_corr1)
corrplot(cor(data_corr1) , order="hclust", tl.cex = 0.6)
Eco_data_corr <- mutate_all(Eco_data, as.numeric)
#полная матрица
corrplot(cor(Eco_data_corr) , order="hclust", tl.cex = 0.6)

names(Eco_data)
library(Hmisc)
data_corr_small <- subset(Eco_data, 
                    select = c(Curiosity, WarmGlow,
                  EnvCon, FutureGen, Bonuses,
                  Competition, Ranking, Fashion, Crowd, Status))
data_corr_small1 <- mutate_all(data_corr_small, as.numeric)
#Telling, Encouraging
corr_5 <- rcorr(as.matrix(data_corr_small))
M <- corr_5$r
p_mat <- corr_5$P
corrplot(M, type = "lower", 
         p.mat = p_mat, sig.level = 0.01)

library(dplyr)
library(ggstatsplot)
library(gapminder)
library(dplyr)
library(paletteer)
library(ggplot2)
names(data_corr_small1) <- c("Любопытство", "Теплое свечение", "Экология", 
                             "Будущие поколения", "Бонусы", "Соревнование", 
                             "Рейтинг", "Мода", "Мнение толпы", "Статус")
ggcorrmat(
  data  = data_corr_small1, 
  title  = "Корреляционная матрица", ,
  colors = c("#E69F00", "white", "#009E73")) 
?ggcorrmat
####Графики каналы привлечения####
Eco_data <- mutate_all(Eco_data, as.numeric)
data_inf <- data.frame(name = c("Рассказали знакомые","Социальные сети","Увидел фандомат на улице","СМИ"),
  value = c(sum(Eco_data$InfAcq), sum(Eco_data$InfSN), sum(Eco_data$InfStreet), 
            sum(Eco_data$InfMedia)))

ggplot(data_inf, aes(reorder(name, value), value)) + geom_col() + 
  theme(axis.text.x = element_text(size = 2)) + labs(x = "", y = "") + 
  theme_bw() + geom_text(aes(label = round(value, 1), vjust = -0.2)) +
  geom_col(fill = "#53cf75") 

####Графики привычки####
Eco_data <- mutate_all(Eco_data, as.numeric)
Eco_data_children_Yes <- filter(Eco_data, Eco_data$EcoHabits == 1)
Eco_data_children_No <- filter(Eco_data, Eco_data$EcoHabits == 0, Eco_data$Children > 0)
Eco_data_children_No_No <- filter(Eco_data, Eco_data$EcoHabits == 0, Eco_data$Children == 0)
data_habits <- data.frame(name = c("Нет детей","Не прививаю экопривычки детям", "Прививаю экопривычки"), value = c(nrow(Eco_data_children_No_No), 
                                 nrow(Eco_data_children_No), nrow(Eco_data_children_Yes)))

ggplot(data_habits, aes(reorder(name, value), value)) + geom_col() + 
  theme(axis.text = element_text(size = 36)) + labs(x = "", y = "") + 
  theme_bw() + geom_text(aes(label = round(value, 1), vjust = -0.2)) +
  geom_col(fill = "#53cf75") 

####Графики недостатки####
library(dplyr)
Eco_data <- mutate_all(Eco_data, as.numeric)
data_cons <- data.frame(name = c("Мало бонусов за сдачу","Высокая стоимость акций",
                                 "Фандоматы далеко расположены",
                                 "Фандоматы переполнены","Нет интересных акций"),
                       value = c(sum(Eco_data$FewBonuses), sum(Eco_data$Prices), 
                                 sum(Eco_data$Distance), 
                                 sum(Eco_data$Fullness),
                                 sum(Eco_data$Promotions)))

ggplot(data_cons, aes(reorder(name, value), value)) + geom_col() + 
  theme(axis.text = element_text(size = 30)) + labs(x = "", y = "") + 
  theme_bw() + geom_text(aes(label = round(value, 1), vjust = -0.2)) +
  geom_col(fill = "#53cf75") + ggtitle("Недостатки программы")

####Графики социально-демографические####
summary(Eco_data)
names(Eco_data)
library(gtsummary)
library(modelsummary)

datafs <- Eco_data %>% dplyr::select(Gender, City, Education, Family, Children, Income)
datafs <- datafs %>% mutate(datafs, City = as.character(City))
datafs <- datafs %>% mutate(datafs, Children = as.character(Children))
str(datafs)

datafs$Gender[datafs$Gender == 0] <- "Мужской"
datafs$Gender[datafs$Gender == 1] <- "Женский"

datafs$City[datafs$City == 0] <- "Город с населением < 1 мил. чел."
datafs$City[datafs$City == 1] <- "Город-миллионник"

datafs$Education[datafs$Education == 1] <- "Начальное"
datafs$Education[datafs$Education == 2] <- "Среднее общее"
datafs$Education[datafs$Education == 3] <- "Среднее полное"
datafs$Education[datafs$Education == 4] <- "Среднее профессиональное"
datafs$Education[datafs$Education == 5] <- "Неоконченное высшее"
datafs$Education[datafs$Education == 6] <- "Высшее"

datafs$Family[datafs$Family == 1] <- "Женат/замужем"
datafs$Family[datafs$Family == 2] <- "Не состою в отношениях"
datafs$Family[datafs$Family == 3] <- "Состою в отношениях"

datafs$Children[datafs$Children == 0] <- "Нет детей"
datafs$Children[datafs$Children == 3] <- "3 и более"

datafs$Income[datafs$Income == 1] <- "(1) Денег с трудом хватает даже на еду"
datafs$Income[datafs$Income == 2] <- "(2) Хватает на еду, но покупка одежды затруднительна"
datafs$Income[datafs$Income == 3] <- "(3) Денег хватает на еду и одежду, но покупка товаров длительного пользования затруднительна"
datafs$Income[datafs$Income == 4] <- "(4) Могу позволить себе товары длительного пользования, но покупка машины затруднительна"
datafs$Income[datafs$Income == 5] <- "(5) Могу позволить себе практически все, кроме покупки квартиры и дачи"
datafs$Income[datafs$Income == 6] <- "(6) Могу позволить себе все, включая покупку квартиры и дачи"
datafs$Income[datafs$Income == 7] <- "(7) Затрудняюсь ответить"

datasummary_skim(datafs, type = "categorical")

#графики по социально-демографическим характеристикам
library(magrittr)
Eco_data_plot <- as.data.frame(Eco_data) 
ggplot(Eco_data_plot, aes(x = Age)) + geom_bar(fill = "#53cf75")  +
  theme(axis.line = element_line(), axis.title=element_text(size=8), panel.background = element_blank()) + 
  labs(x = "Возраст", y = "Количество пользователей") 
Eco_data_plot$Family[Eco_data_plot$Family == 1] <- "Женат/замужем"
Eco_data_plot$Family[Eco_data_plot$Family == 2] <- "Не состою в отношениях"
Eco_data_plot$Family[Eco_data_plot$Family == 3] <- "Состою в отношениях"
ggplot(Eco_data_plot, aes(x = Family)) + geom_bar(fill = "#53cf75") + 
  theme(axis.line = element_line(), panel.background = element_blank()) + 
  labs(x = "", y = "Количество пользователей") +
  ggtitle("Семейное положение") 

Eco_data_plot$Gender[Eco_data_plot$Gender == 0] <- "Мужской"
Eco_data_plot$Gender[Eco_data_plot$Gender == 1] <- "Женский"
ggplot(Eco_data_plot, aes(x = Gender)) + geom_bar(fill = "#53cf75") +
  theme(axis.line = element_line(), panel.background = element_blank()) +
  labs(x = "", y = "Количество пользователей") +
  ggtitle("Пол") 

Eco_data_plot$City[Eco_data_plot$City == 0] <- "Город с населением < 1 мил. чел."
Eco_data_plot$City[Eco_data_plot$City == 1] <- "Город-миллионник"
ggplot(Eco_data_plot, aes(x = City)) + geom_bar(fill = "#53cf75") +
  theme(axis.line = element_line(), panel.background = element_blank()) +
  labs(x = "", y = "Количество пользователей") +
  ggtitle("Место жительства") 

Eco_data_plot$Education[Eco_data_plot$Education == 1] <- "Начальное"
Eco_data_plot$Education[Eco_data_plot$Education == 2] <- "Среднее общее"
Eco_data_plot$Education[Eco_data_plot$Education == 3] <- "Среднее полное"
Eco_data_plot$Education[Eco_data_plot$Education == 4] <- "Среднее профессиональное"
Eco_data_plot$Education[Eco_data_plot$Education == 5] <- "Неоконченное высшее"
Eco_data_plot$Education[Eco_data_plot$Education == 6] <- "Высшее"
ggplot(Eco_data_plot, aes(x = Education)) + geom_bar(fill = "#53cf75") +
  theme(axis.line = element_line(), panel.background = element_blank()) +
  labs(x = "", y = "Количество пользователей") +
  ggtitle("Уровень образования") 

ggplot(Eco_data_plot, aes(x = Income)) + geom_bar(fill = "#53cf75") +
  theme(axis.line = element_line(), panel.background = element_blank()) +
  labs(x = "", y = "Количество пользователей") +
  ggtitle("Уровень доходов") 

Eco_data_plot$Children[Eco_data_plot$Children == 0] <- "Нет детей"
Eco_data_plot$Children[Eco_data_plot$Children == 3] <- "3 и более"
ggplot(Eco_data_plot, aes(x = Children)) + geom_bar(fill = "#53cf75") +
  theme(axis.line = element_line(), panel.background = element_blank()) +
  labs(x = "", y = "Количество пользователей") +
  ggtitle("Количество детей")

####шкала Лайкерта#####
#создадим данные для шкалы Лайкерта
data_likert <- lapply(Eco_data, factor, ordered = TRUE)
str(data_likert)
data_likert1 <- as.data.frame(data_likert) 
library(dplyr)
data_likert_motivations <- data_likert1 %>% 
  dplyr::select(WarmGlow, FutureGen, EnvCon, Curiosity, 
                Bonuses, Competition, Ranking,
                Status, Crowd, Fashion)

names(data_likert_motivations) <- c("Теплое свечение", "Будущие поколения", 
                                    "Экология", "Любопытство", 
                                    "Бонусы", "Соревнование", 
                                    "Рейтинг", "Статус", "Мнение толпы",
                                    "Мода")
#Лайкерт плот по мотивациям
library(likert)
likert_motivations <- likert(data_likert_motivations) 
dev
plot(likert_motivations) + ggtitle("Мотивации") +  
  theme(plot.title = element_text (hjust = 0.5 )) +
  scale_fill_brewer(palette="Greens")
plot(likert_motivations, type = "heat", low.color = "white", cex.axis = 3,
     high.color = "#00a550", text.color = "black", text.size = 4.5, wrap = 50) + ggtitle("Мотивации")
likert.density.plot(likert_motivations, facet = FALSE)
likert.density.plot(likert_motivations) 

#Лайкерт плот для других
#вера
data_belief <- data_likert1 %>% dplyr::select(EnvImp, Motivate, 
                                              ConfRecycl) 
names(data_belief) <- c("Экология", "Мотивация", "Уверенность")
likert_belief <- likert(data_belief)
plot(likert_belief) + ggtitle("Вера индивида в свои действия") + 
  theme(plot.title = element_text (hjust = 0.5 )) +  scale_fill_brewer(palette="Greens")
#изменения
data_change <- data_likert1 %>% dplyr::select(SortingBef, RecyclingBef,
                                              EnvIntAf, EcoActAf)
names(data_change) <- c("Сортировка до",
                        "Переработка до", 
                        "Экоинтерес после", 
                        "Экоактивизм после")
likert_change <- likert(data_change)
plot(likert_change) + ggtitle("Изменение в поведении") + 
  theme(plot.title = element_text (hjust = 0.5 )) +  scale_fill_brewer(palette="Greens")
#взаимоотношения со знакомыми
data_people <- data_likert1 %>% dplyr::select(Telling, Encouraging)
names(data_people) <- c("Рассказ", "Призыв")
likert_people <- likert(data_people)
plot(likert_people) + ggtitle("Взаимодействие со знакомыми") + 
  theme(plot.title = element_text (hjust = 0.5 )) +  scale_fill_brewer(palette="Greens")

####Облака слов####
#Load the Plasticckages 
library(readtext) 
library(tm) 
library(wordcloud) 
library(tmap) 
library(wordcloud2)
library(RColorBrewer)
data_word1 <- subset(data_words, select = 1)
data_word2 <- subset(data_words, select = 2)

#чего не хвататет знакомым для участия
docs1 <-  Corpus(VectorSource(data_word1)) 
inspect(docs1)
docs1 <- tm_map(docs1, content_transformer(tolower))
docs1 <- tm_map(docs1, removePunctuation)
docs1 <- tm_map(docs1, PlainTextDocument)
docs1 <- tm_map(docs1, removeWords, stopwords('russian'))
docs1 <- tm_map(docs1,removeWords, c("это", "мало", "нужно", "сдать",
                                     "сдавать", "просто", "сбором", "домом",
                                     "бутылки", "сдачи", "рядом",
                                     "бутылок", "приема", "пунктов",
                                     "места", "мест", "автоматов",
                                     "окружающей", "переработку", "приема",
                                     "заниматья", "хранить", "не",
                                     "хватает", "и", "но", "дома", "этим", 
                                     "делать", "действительно", "сортировать",
                                     "заниматься", "собирать", "поблизости", 
                                     "сделать", "мусора", "среде", "экологии",
                                     "городе", "сдают", "приёма", "вторсырье",
                                     "сортировку", "сбора", "банки", 
                                     "фандомата", "мусор"))
docs1 <- tm_map(docs1, stemDocument)

dtm <- TermDocumentMatrix(docs1) %>% as.matrix()
v <- sort(rowSums(dtm),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud2(d)
ggplot(data = d, 
       aes(label = word, size = freq)
       

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, 
        col ="#77dd77", main ="Самые распространенные слова", 
        ylab = "Частота", cex.names = 0.7)

#ассоциации с брендом
docs2 <-  Corpus(VectorSource(data_word2)) 
inspect(docs2)
docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, removePunctuation)
docs2 <- tm_map(docs2, PlainTextDocument)
docs2 <- tm_map(docs2, removeWords, stopwords('russian'))
docs2 <- tm_map(docs2,removeWords, c("окружающей", "экологии", "окружающая",
                                     "среды", "для", "среде", "об", "не",
                                     "экологическая", "экологичность",
                                     "эко", "природе", "природы"))
docs2 <- tm_map(docs2, stemDocument)
                
dtm2 <- TermDocumentMatrix(docs2) %>% as.matrix()
v2 <- sort(rowSums(dtm2),decreasing = TRUE)
d2 <- data.frame(word = names(v2),freq=v2)
wordcloud2(d2)

barplot(d2[1:10,]$freq, las = 2, names.arg = d2[1:10,]$word, 
        col ="#77dd77", main ="Самые распространенные слова", 
        ylab = "Частота", cex.names = 0.7)

####Множественная регрессия для бутылок####
library(MASS)
library(car)
str(Eco_data)
Eco_data2 <- dplyr::select(Eco_data, -Education, -Family) 
Eco_data3 <- mutate_all(Eco_data2, as.numeric)
Eco_data4 <- mutate(Eco_data3, Eco_data$Education, Eco_data$Family)
names(Eco_data4)[names(Eco_data4)=="Eco_data$Education"] <- "Education"
names(Eco_data4)[names(Eco_data4)=="Eco_data$Family"] <- "Family"
names(Eco_data4)

mod1 <- lm(Plastic ~ ., Eco_data4)
summary(mod1)

#тест на мультиколлинеарность
vif(mod1)
#мультиколлинеарности нет

#посмотрим на остатки с графиком QQ-plot
plot(mod1, which = 2)

boxCox(mod1)
#логарифмировать не нужно

#тест Рамсея
library(lmtest)
resettest(mod1)
#H0: ничего не пропущено
#p-value < 0.05 => что-то пропущено

#подбор спецификации с помощью crPlots
crPlots(mod1)

#добавим логарифм
mod2 <- update(mod1, .~. + log(BonusesExp) - BonusesExp, data = Eco_data4)
summary(mod2)
resettest(mod2)
mod3 <- stepAIC(mod2)
summary(mod3)
#уберем незначимые переменные
mod4 <- update(mod3, .~. -FutureGen -Telling -Education, data = Eco_data4)
summary(mod4)

#добавим логарифм к бутылкам
mod_log <- lm(log(Plastic + 1) ~ ., data = Eco_data4)
summary(mod_log)
vif(mod_log)
#мультиколлинеарности нет
#тест Рамсея
resettest(mod_log)
#что0то пропущено
#посмотрим спецификацию
crPlots(mod_log)
#добавим логарифм
mod_log1 <- update(mod_log, .~. +log(BonusesExp))
summary(mod_log1)
resettest(mod_log1)
#уберем незначимые переменные
mod_log2 <- stepAIC(mod_log1)
summary(mod_log2)

#робастные стандартыне ошибки
library(sandwich)
mod_log4 <- vcovHC(mod_log2, type = "HC0")
coeftest(mod_log1, mod_log4)

summary(mod_log4)

library(stargazer)
stargazer(mod4, mod_log2, title = "Множественные регрессии",
          column.labels = c("Без логарифма", "С логарифмом"),
          type = "html", omit.stat = c("adj.rsq"), out = "ecoregr.html")

library(stargazer)
stargazer(mod_log, mod_log2, title = "Множественные регрессии",
          column.labels = c("Все", "Значимые"),
          type = "html", omit.stat = c("adj.rsq"), out = "ecolog.html")

library(stargazer)
stargazer(mod_log2, title = "Множественная регрессия для бутылок",
          type = "html", omit.stat = c("adj.rsq"), out = "ecoregr.html")

####Множественная регрессия для алюминия####
library(MASS)
library(car)
Eco_data2 <- dplyr::select(Eco_data, -Education, -Family) 
Eco_data3 <- mutate_all(Eco_data2, as.numeric)
Eco_data4 <- mutate(Eco_data3, Eco_data$Education, Eco_data$Family)
names(Eco_data4)[names(Eco_data4)=="Eco_data$Education"] <- "Education"
names(Eco_data4)[names(Eco_data4)=="Eco_data$Family"] <- "Family"
names(Eco_data4)

summary(Eco_data4$Aluminum)
sum(Eco_data4$Aluminum > 0)
Eco_al <- filter(Eco_data4, Eco_data4$Aluminum > 0)

mod_a1 <- lm(Aluminum ~ ., Eco_al)
summary(mod_a1)

#тест на мультиколлинеарность
vif(mod_a1)
#мультиколлинеарности нет

#посмотрим на остатки с графиком QQ-plot
plot(mod_a1, which = 2)

boxCox(mod_a1)
#логарифмировать

#добавим логарифм 
mod_log_al <- lm(log(Aluminum + 1) ~ ., data = Eco_al)
summary(mod_log_al)
vif(mod_log_al)
#мультиколлинеарности нет
#тест Рамсея
resettest(mod_log_al)
#что0то пропущено
#посмотрим спецификацию
crPlots(mod_log_al)
#добавим логарифм
mod_log_al1 <- update(mod_log_al, .~. +log(BonusesExp))
summary(mod_log_al1)
resettest(mod_log_al1)
#уберем незначимые переменные
mod_log_al2 <- stepAIC(mod_log_al1)
summary(mod_log_al2)
#уберем незначимые переменные
mod_log_al3 <- update(mod_log_al2, .~. -Curiosity -EnvCon -Ranking
                   -Education -Status -Motivate, data = Eco_al)
summary(mod_log_al3)

#робастные стандартыне ошибки
library(sandwich)
mod_log_al4 <- vcovHC(mod_log_al3, type = "HC0")
coeftest(mod_log_al3, mod_log_al4)

library(stargazer)
stargazer(mod_log_al3, title = "Множественная регрессия для алюминия",
          type = "html", omit.stat = c("adj.rsq"), out = "ecoal.html")


####Логистическая регрессия для одежды####
Eco_data5 <- mutate(Eco_data4, Clothing = as.factor(Clothing))
mod_glm_cl <- glm(Clothing ~., family = binomial, data = Eco_data5, x = TRUE)
summary(mod_glm_cl)
#уберем незначимые переменные
mod_glm_cl2 <- stepAIC(mod_glm_cl)
summary(mod_glm_cl2)

mod_glm_cl3 <- update(mod_glm_cl2, .~.  -Distance -InfMedia, data = Eco_data5)
summary(mod_glm_cl3)

library(pscl)
pR2(mod_glm_cl3)['McFadden']

library(stargazer)
stargazer(mod_glm_cl3, title = "Логистическая регрессия для одежды",
          type = "html", omit.stat = c("adj.rsq"), out = "clglm.html")

library(stargazer)
stargazer(mod_log3, mod_glm_cl3, title = "Регрессии",
          type = "html", omit.stat = c("adj.rsq"), out = "tworegres.html")

#####Кластеризация PCA по мотивациям####
library(FactoMineR)
library(corrplot)
library(factoextra)
library(dplyr)
names(Eco_data)
data_PCA_motivation1 <- mutate_all(Eco_data, as.numeric)
dev.off()
mod_PCA <- PCA(data_PCA_motivation1)
dim(data_PCA_motivation1)
data_PCA <- subset(data_PCA_motivation1, 
                   select = -c(InfStreet, Gender, FewBonuses, 
                               Clothing, Aluminum, Distance,
                               Plastic, InfSN, RecyclingBef, 
                               City, Income, InfAcq, Promotions,
                               Prices, InfMedia, BonusesExp, Fullness,
                               Education, Age, Family, Children, EcoHabits,
                               EnvKnow))
mod_PCA2 <- PCA(data_PCA) 
dim(data_PCA)

#матрица нагрузок первых двух компонент
corrplot(mod_PCA2$var$coord[,1:2], cl.align.text = "l")

#график каменистой осыпи
fviz_eig(mod_PCA2, addlabels = TRUE, ylim = c(0, 50)) 
#берем три кластера
mod_clust3 <- HCPC(mod_PCA2, nb.clust = 3)
fviz_cluster(mod_clust3, palette = "jco", 
             geom = "point")
clusters3 <- mod_clust3$data.clust
table(clusters3$clust)

mod_clust3$data.clust #номера кластеров
mod_clust3$desc.var #интерпретация через средние координаты
mod_clust3$desc.axes #интерпретация через главные компоненты
mod_clust3$desc.ind$para #типичные представители кластера


#попробуем взять 4
mod_clust4 <- HCPC(mod_PCA2, nb.clust = 4)
fviz_cluster(mod_clust4, palette = c("#c7e9c0", "#74c476", "#248b45", "#00441b"), 
             geom = "point", title = "Кластеризация") + theme_classic() 
clusters4 <- mod_clust4$data.clust
table(clusters4$clust)


mod_clust4$data.clust$clust #номера кластеров
mod_clust4$desc.var #интерпретация через средние координаты
mod_clust4$desc.axes #интерпретация через главные компоненты
mod_clust4$desc.ind$para #типичные представители кластера

#посчитаем средние
data_clust_0 <- mutate(Eco_data, mod_clust4$data.clust$clust)
data_clust <- mutate_all(data_clust_0, as.numeric)
names(data_clust)[names(data_clust)=="mod_clust4$data.clust$clust"] <- "clust"
library(dplyr)

names(data_clust)
clusters <- data_clust %>% group_by(clust) %>%
  summarise(mean_EnvImp = mean(EnvImp), 
            mean_Motivate = mean(Motivate), 
            mean_Curiosity = mean(Curiosity),
            mean_WarmGlow = mean(WarmGlow),
            mean_EnvCon = mean(EnvCon),
            mean_FutureGen = mean(FutureGen), 
            mean_Bonuses = mean(Bonuses), 
            mean_Competition = mean(Competition), 
            mean_Ranking = mean(Ranking), 
            mean_Fashion = mean(Fashion),
            mean_Crowd = mean(Crowd),
            mean_Status = mean(Status),
            mean_SortingBef = mean(SortingBef),
            mean_RecyclingBef = mean(RecyclingBef),
            mean_EnvIntAf = mean(EnvIntAf),
            mean_EcoActAf = mean(EcoActAf),
            mean_ConfRecycl = mean(ConfRecycl),
            mean_Telling = mean(Telling),
            mean_Encouraging = mean(Encouraging),
            mean_EcoHabits = mean(EcoHabits),
            mean_FewBonuses = mean(FewBonuses),
            mean_Prices = mean(Prices),
            mean_Distance = mean(Distance),
            mean_Fullness = mean(Fullness),
            mean_Promotions = mean(Promotions),
            mean_InfAcq = mean(InfAcq),
            mean_InfSN = mean(InfSN),
            mean_InfStreet = mean(InfStreet),
            mean_InfMedia = mean(InfMedia),
            mean_Gender = mean(Gender),
            mean_Age = mean(Age),
            mean_City = mean(City),
            mean_Education = mean(Education),
            mean_Income = mean(Income),
            mean_Family = mean(Family),
            mean_Children = mean(Children),
            mean_Plastic = mean(Plastic),
            mean_Aluminum = mean(Aluminum),
            mean_Clothing = mean(Clothing),
            mean_EnvKnow = mean(EnvKnow),
            mean_BonusesExp = mean(BonusesExp),
            .groups = 'drop')

#сделаем таблицу
clusters_all <- data_clust %>% group_by(clust) %>%summarise_all(mean)
means_table <- t(clusters_all)
nrow(means_table)
means_table1 <- means_table[2:42,]
means_table2 <- round(means_table1, digits = 2) 
colnames(means_table2) <- c("Кластер 1", "Кластер 2", "Кластер 3", "Кластер 4")
library(gt)
library(dplyr)
Variable <- data.frame(colnames(clusters_all))
Variable <- Variable[-1,]
means_table3 <- data.frame(Variable, means_table2)
means_table3 %>% gt() %>%
  tab_header(
    title = "Средние значения по кластерам")


library(dplyr)
#распределение по пластику
#plt <- ggbetweenstats(
  #data = data_clust,
  #x = as.factor(clust),
  #y = Plastic,
  #title   = "Plastic")
                                                                                                                                                                                                                                                                     axis.text.y = element_text(size = 15), axis.title = element_text(size = 20), legend.text = element_text(size = 15))
#пластик
ggplot(data_clust, aes(as.factor(clust), Plastic)) + geom_violin(aes(fill = as.factor(clust))) + 
  geom_point(position = position_jitter(seed = 1, width = 0.3)) + 
  scale_fill_brewer(palette = "Greens") + 
  labs(x = "Кластер", y = "Количество", fill = "Кластер") + theme_bw()

#алюминий
ggplot(data_clust, aes(as.factor(clust), Aluminum)) + geom_violin(aes(fill = as.factor(clust))) + 
  geom_point(position = position_jitter(seed = 1, width = 0.3)) + 
  scale_fill_brewer(palette = "Greens") + 
  labs(x = "Кластер", y = "Количество сданных алюминиевых банок", fill = "Кластер") + theme_bw() +
  ggtitle("Сдача алюминиевых банок по кластерам")

#одежда
ggplot(data_clust, aes(as.factor(clust), log(Clothing))) + geom_violin(aes(fill = as.factor(clust))) + 
  geom_point(position = position_jitter(seed = 1, width = 0.3)) + 
  scale_fill_brewer(palette = "Greens") + 
  labs(x = "Кластер", y = "Килограммы сданной одежды", fill = "Кластер") + theme_bw() +
  ggtitle("Сдача одежды по кластерам")

plt <- ggbetweenstats(
  data = data_clust,
  x = as.factor(clust),
  y = Plastic
)

####Радары####
library(fmsb)
names(clusters)

data_radar <- dplyr::select(clusters, c(mean_Curiosity, mean_WarmGlow, 
                                 mean_EnvCon, mean_FutureGen, mean_Bonuses, 
                                 mean_Competition, 
                                 mean_Ranking, mean_Fashion, mean_Crowd,
                                 mean_Status))

data_radar <- rbind(rep(5,3), rep(1,3), data_radar)
names(data_radar) <- c("Любопытство", "Теплое свечение", "Экология", 
                       "Будущие поколения", "Бонусы",
                     "Соревнование", "Рейтинг", "Мода", "Мнение знакомых", "Статус")

library(hrbrthemes)
#кластер 1
par(mfrow = c(1,1))
R_cluster1 <- data_radar[1:3,]
radarchart(R_cluster1, axistype=1, pfcol = scales::alpha("#c7e9c0", 0.5), 
           pcol = scales::alpha("#c7e9c0", 0.5),
           plwd=4 , cglcol="grey", cglty=1, 
           axislabcol="grey", caxislabels=seq(1,5,1), cglwd=0.6, vlcex=0.55, 
           title = "Кластер 1")
R_cluster1 %>% slice(3) %>% t() %>% as.data.frame() %>% add_rownames() %>% 
  arrange(V1) %>% mutate(rowname=factor(rowname, rowname)) %>%
  ggplot( aes(x=rowname, y=V1)) +
  geom_segment( aes(x=rowname ,xend=rowname, y=1, yend=V1), color="#c7e9c0") +
  geom_point(size=5, color="#c7e9c0") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text( size=48 ),
    legend.position="motive"
  ) +
  ylim(1,5) +
  ylab("") +
  xlab("") 

#кластер 2
R_cluster2 <- data_radar[c(1, 2, 4),]
radarchart(R_cluster2, axistype=1, pfcol = scales::alpha("#74c476", 0.5), 
           pcol = scales::alpha("#74c476", 0.5), plwd=4 ,   cglcol="grey", cglty=1, 
           axislabcol="grey", caxislabels=seq(1,5,1), cglwd=0.6, vlcex=0.55, 
           title = "Кластер 2")

R_cluster2 %>% slice(3) %>% t() %>% as.data.frame() %>% add_rownames() %>% 
  arrange(V1) %>% mutate(rowname=factor(rowname, rowname)) %>%
  ggplot( aes(x=rowname, y=V1)) +
  geom_segment( aes(x=rowname ,xend=rowname, y=1, yend=V1), color="#74c476") +
  geom_point(size=5, color="#74c476") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text( size=48 ),
    legend.position="motive") +
  ylim(1,5) +
  ylab("") +
  xlab("") 

#кластер 3
R_cluster3 <- data_radar[c(1, 2, 5),]
radarchart(R_cluster3, axistype=1, pfcol = scales::alpha("#248b45", 0.5), 
           pcol = scales::alpha("#248b45", 0.5), plwd=4 ,   cglcol="grey", cglty=1, 
           axislabcol="grey", caxislabels=seq(1,5,1), cglwd=0.6, vlcex=0.55, 
           title = "Кластер 3")

R_cluster3 %>% slice(3) %>% t() %>% as.data.frame() %>% add_rownames() %>% 
  arrange(V1) %>% mutate(rowname=factor(rowname, rowname)) %>%
  ggplot( aes(x=rowname, y=V1)) +
  geom_segment( aes(x=rowname ,xend=rowname, y=1, yend=V1), color="#248b45") +
  geom_point(size=5, color="#248b45") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text( size=48 ),
    legend.position="motive"
  ) +
  ylim(1,5) +
  ylab("") +
  xlab("") 

#кластер 4
R_cluster4 <- data_radar[c(1, 2, 6),]
radarchart(R_cluster4, axistype=1,  pfcol = scales::alpha("#00441b", 0.5), 
           pcol = scales::alpha( "#00441b", 0.5), plwd=4 ,   cglcol="grey", cglty=1, 
           axislabcol="grey", caxislabels=seq(1,5,1), cglwd=0.6, vlcex=0.55,
           title = "Кластер 4")

R_cluster4 %>% slice(3) %>% t() %>% as.data.frame() %>% add_rownames() %>% 
  arrange(V1) %>% mutate(rowname=factor(rowname, rowname)) %>%
  ggplot( aes(x=rowname, y=V1)) +
  geom_segment( aes(x=rowname ,xend=rowname, y=1, yend=V1), color="#00441b") +
  geom_point(size=5, color="#00441b") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text( size=48 ),
    legend.position="motive"
  ) +
  ylim(1,5) +
  ylab("") +
  xlab("") 


####Факторный ананиз####
#корреляционная матрица
library(corrplot)
library(dplyr)
names(Eco_data)
data_corrf <- Eco_data %>% 
  dplyr::select(Status, Fashion, Crowd, Curiosity, 
                Bonuses, Competition, Ranking,
                EnvKnow, 
                ConfRecycl, Motivate, EnvImp,
                FutureGen, WarmGlow, EnvCon)
names(data_corrf) <- c("Статус", "Мода", "Мнение знакомых",
                       "Любопытство", "Бонусы", "Соревнование", "Рейтинг",
                       "Знания экологии", "Вера в переработку",
                       "Мотивация других", "Вера в изменение",
                       "Будущие поколения", "Теплое свечение", "Экология")
data_corrf1 <- mutate_all(data_corrf, as.numeric)
corrplot(cor(data_corrf1) , order="hclust")
corrplot(cor(data_corrf1) , order="hclust", number.cex = 0.3,
         tl.cex = 0.6, cl.cex = 0.6,
         addrect = 8)
?corrplot
#corrplot(cor(data_corrf1)) 
#4 фактора и 3 одиночки
  
#факторный анализ
data_factor <- data.frame(Social = rowMeans(data_corrf1[,1:3]),
                          Gamble = rowMeans(data_corrf1[,6:7]),
                          Belief = rowMeans(data_corrf1[,10:11]),
                          Altruism  = rowMeans(data_corrf1[,12:14]))
Curiosity <- data_corrf1$Curiosity
EKnowledge <- data_corrf1$EnvKnow
Bonuses <- data_corrf1$Bonuses
ConfRecycl <- data_corrf1$ConfRecycl

data_factor_full <- data.frame(data_factor, Bonuses, Curiosity, EKnowledge,
                               ConfRecycl)
corrplot(cor(data_factor_full), order="hclust", addrect = 3)

str(Eco_data)
Eco_dataf <- mutate_all(Eco_data, as.numeric)
Eco_dataf1 <- scale(Eco_dataf)
#создадим факторы
Eco_fmodel <- "
        Altruism  =~ FutureGen + WarmGlow + EnvCon
        Gamble  =~ Competition + Ranking
        Social  =~ Status + Fashion + Crowd
        Belief  =~ Motivate + EnvImp"
#подтвеждающий факторный анализ
Facmod <- lavaan::cfa(Eco_fmodel, data = Eco_dataf1,
                      ordered = colnames(Eco_dataf1))
summary(Facmod)

library(lavaanPlot)
lavaanPlot(model = Facmod, node_options = list(shape = "box", 
                                                fontname = "Helvetica"), 
           edge_options = list(color = "green"), coefs = TRUE, stand = FALSE) 

library(semPlot)
semPaths(Facmod, what = "est", edge.label.cex = 0.7,
         edge.color = 1, esize = 1, sizeMan = 4.5, asize = 2.5,
         intercepts = FALSE, rotation = 4, thresholdColor = "red",
         mar = c(1, 5, 1.5, 5), fade = FALSE, nCharNodes = 4)

summary(Facmod, fit.measures=TRUE)
print(Facmod$loadings, cutoff = 0.3)
#Хи-квадрат значим - да
#Comparative Fit Index в окрестности 1 - да
#RMSEA близкое к нулю - не очень (0.076)


#разведывательный факторный анализ
library(psych)
mod_Fa <- fa(data_corr1, nfactors = 4, rotate = "none", fm = "ml")
print(mod_Fa$loadings, cutoff = 0.3)
mod2_Fa <- fa(data_corr1, nfactors = 4, rotate = "varimax", fm = "ml")
print(mod2_Fa$loadings, cutoff = 0.3)
#в целом похоже

mod3_Fa <- fa(data_corrf, nfactors = 4, rotate = "varimax", fm = "ml")
print(mod3_Fa$loadings, cutoff = 0.3)

library(gplots)
library(RColorBrewer)
heatmap.2(head(mod3_Fa$loadings), col=brewer.pal(9, "Greens"), 
          trace="none", key=FALSE, dend="none", Colv=FALSE, 
          cexCol = 1.2, main="Factor loadings for brand adjectives") 
heatmap.2(head(mod2_Fa$scores))

?heatmap.2
mod2_Fa$loadings

#посчитаем альфа кронбаха
#альтруизм
library(ltm)
Eco_dataf_Alt <- Eco_dataf %>% dplyr::select(FutureGen, WarmGlow, EnvCon)
cronbach.alpha(Eco_dataf_Alt)
#0.862

#экономический
#Eco_dataf_Ec <- Eco_dataf %>% dplyr::select(SelfBenefit, Bonuses)
#cronbach.alpha(Eco_dataf_Ec)
#0.418
#оставим в экономических мотивах только бонусы

#азарт
Eco_dataf_Gamb <- Eco_dataf %>% dplyr::select(Competition, Ranking)
cronbach.alpha(Eco_dataf_Gamb)
#0.748

#социальные
Eco_dataf_Soc <- Eco_dataf %>% dplyr::select(Status, Fashion, Crowd)
cronbach.alpha(Eco_dataf_Soc)
#0.731

#вера
Eco_dataf_Bel <- Eco_dataf %>% dplyr::select(Motivate, EnvImp)
cronbach.alpha(Eco_dataf_Bel)
#0.651


#сравним 2 модели
#модель просто
Eco_fmodel_simple <- "
        Altruism  =~ FutureGen + WarmGlow + EnvCon
        Gamble  =~ Competition + Ranking
        Social  =~ Status + Fashion + Crowd
        Bonuses ~~ 1*Bonuses
        Curiosity ~~ 1*Curiosity"
#подтвеждающий факторный анализ
Facmod_simple <- lavaan::cfa(Eco_fmodel_simple, data = Eco_dataf1,
                      ordered = colnames(Eco_dataf1))

#модель values
Eco_fmodel_values <- "
        Altruism  =~ FutureGen + WarmGlow + EnvCon
        Gamble  =~ Competition + Ranking
        Social  =~ Status + Fashion + Crowd
        Bonuses ~~ 1*Bonuses
        Curiosity ~~ 1*Curiosity
Values =~ Altruism + Gamble + Social + Bonuses + Curiosity"
#подтвеждающий факторный анализ
Facmod_values <- lavaan::cfa(Eco_fmodel_values, data = Eco_dataf1,
                      ordered = colnames(Eco_dataf1))

#прямая модель
Eco_fmodel_straight <- "
Values =~ FutureGen + WarmGlow + EnvCon + Competition + Ranking + Status + Fashion + Crowd + Bonuses + Curiosity"
#подтвеждающий факторный анализ
Facmod_straight <- lavaan::cfa(Eco_fmodel_straight, data = Eco_dataf1,
                             ordered = colnames(Eco_dataf1))

library(semTools)
summary(semTools::compareFit(Facmod_simple, Facmod_values, Facmod_straight))


####SEM####
str(Eco_data)
Eco_dataf2 <- Eco_dataf

Eco_dataf2$logPlastic <- log(Eco_dataf2$Plastic)
Eco_dataf3 <- subset(Eco_dataf2, select = -c(Plastic))
SEM_mod <- "
#построим структурные уравнения
        #мотивации
        Altruism  =~ FutureGen + WarmGlow + EnvCon
        Belief  =~ Motivate + EnvImp
        Gamble  =~ Competition + Ranking
        Social  =~ Status + Fashion + Crowd
#ConfRecycl ~ Altruism + Belief
#Gamble ~ Bonuses + Curiosity + Social 
#Curiosity ~ Social + Gamble + Altruism + Bonuses + ConfRecycl 
logPlastic ~ Altruism + Gamble + Social + Bonuses + Curiosity + ConfRecycl + 
Belief + EnvKnow + Age + Education + Family + Gender + City + Income"
SEM_plot <- lavaan::sem(SEM_mod, data = Eco_dataf3)
summary(SEM_plot, standardized=TRUE, fit.measures=TRUE)
summary(SEM_plot, rsquare = TRUE)
#Gamble на 1%
#Social, ConfRecycl на 10%

SEM_mod2 <- "
#построим структурные уравнения
        #мотивации
        Altruism  =~ FutureGen + WarmGlow + EnvCon
        Belief  =~ Motivate + EnvImp
        Gamble  =~ Competition + Ranking
        Social  =~ Status + Fashion + Crowd
ConfRecycl ~ Altruism + Belief
Gamble ~ Bonuses + Curiosity + Social 
Curiosity ~ Social + Gamble + Altruism + Bonuses + ConfRecycl 
logPlastic ~ Altruism + Gamble + Social + Curiosity + 
Age + Education"
SEM_plot2 <- lavaan::sem(SEM_mod2, data = Eco_dataf3)
summary(SEM_plot2, standardized=TRUE, fit.measures=TRUE)
summary(SEM_plot2, rsquare = TRUE)

#визуализация
semPaths(SEM_plot, "est", colFactor = 0, edge.label.cex = 1)
semPaths(SEM_plot2, "est", colFactor = 0, edge.label.cex = 1)

modindices(SEM_plot, sort=TRUE)
#ConfRecycl ~ Altruism + Belief
#Curiosity  ~ Social + Gamble

####Случайный лес для мотиваций####
library(rpart)
library(rpart.plot)
library(randomForestExplainer)
library(viridis)
#данные по мотивациям в data_forest
data_forest <- Eco_data %>% 
  dplyr::select(Plastic, WarmGlow, FutureGen, EnvCon, Curiosity, 
                Bonuses, Competition, Ranking,
                Status, Crowd, Fashion)
names(data_forest) <- c("Пластик", "Теплое.свечение", "Будущие.поколения",
                        "Экология", "Любопытство", "Бонусы", "Соревнование",
                        "Рейтинг", "Статус", "Мнение.знакомых", "Мода")

#построим дерево
Plastic_forest <- rpart(Пластик ~., data_forest)
summary(Plastic_forest)
Plastic_forest
rpart.plot(Plastic_forest, digits = 2, 
           box.palette = viridis::viridis(10, option = "D", begin = 0, end = 0.85), 
           shadow.col = NULL, col = "grey99", type = 4, extra = 1, tweak = 1.6)


library(randomForest)
library(randomForestExplainer)
forest <- randomForest(Пластик ~., data_forest, localImp = TRUE)
colnames(data_forest)
min_depth_frame <- min_depth_distribution(forest)
plot_min_depth_distribution(min_depth_frame, main = "Мотивы, ранжированные по степени важности") + scale_fill_viridis(discrete = TRUE) 

#для женщин
data_forest_gender <- Eco_data %>% 
  dplyr::select(Plastic, Gender, WarmGlow, FutureGen, EnvCon, Curiosity, 
                Bonuses, Competition, Ranking,
                Status, Crowd, Fashion)
data_forest_female <- filter(data_forest_gender, data_forest_gender$Gender == 1)

forest_female <- randomForest(Plastic ~., data_forest_female, localImp = TRUE)
min_depth_frame_female <- min_depth_distribution(forest_female)
plot_min_depth_distribution(min_depth_frame_female,
                            main = "Ранжированные мотивации для женщин") + 
  scale_fill_viridis(discrete = TRUE) 


#для мужчин
data_forest_gender <- Eco_data %>% 
  dplyr::select(Plastic, Gender, WarmGlow, FutureGen, EnvCon, Curiosity, 
                Bonuses, Competition, Ranking,
                Status, Crowd, Fashion)
data_forest_male <- filter(data_forest_gender, data_forest_gender$Gender == 0)

forest_male <- randomForest(Plastic ~., data_forest_male, localImp = TRUE)
min_depth_frame_male <- min_depth_distribution(forest_male)
plot_min_depth_distribution(min_depth_frame_male,
                            main = "Ранжированные мотивации для мужчин") + 
  scale_fill_viridis(discrete = TRUE) 

####SOM####
library(MASS)
library(kohonen)
library(RColorBrewer)
library(viridis)
library(AER)
library(mlbench)
library(dplyr)

#стандартизируем данные
Eco_SOM <- subset(Eco_data, select = c(Curiosity, WarmGlow,
                               EnvCon, FutureGen, 
                               Competition, Ranking, Fashion, Crowd, 
                               Status, Bonuses))
names(Eco_SOM) <- c("Любопытство", "Теплое Свечение", "Экология", "Будущие поколения",
                     "Соревнование", "Рейтинг", "Мода", "Мнение знакомых",
                     "Статус", "Бонусы")
Eco_SOM1 <- mutate_all(Eco_SOM, as.numeric)
Eco_SOM2 <- as.matrix(Eco_SOM1)

set.seed(1024)
dim(Eco_SOM2)
mod_map <- somgrid(xdim = 27, ydim = 27, topo = "hexagonal")
#построим пустую карту
#обучим модель
mod_som <- som(Eco_SOM2, grid = mod_map, keep.data = TRUE)

#сколько наблюдений попало в каждую ячейку?
plot(mod_som, type = "counts")
plot(mod_som, type = "mapping")

#эталонные наблюдения для кластеров
plot(mod_som, type = "code")

#извлечем эталоны
data_code <- getCodes(mod_som)

#другая палитра
palette_new <- function(n) {
  viridis(n)
}
#viridis(n), magma(n), inferno(n), plasma(n)
#интерпретация
par(mfrow = c(1,1))
for(i in 1:10) {
  plot(mod_som, type = "property", property = data_code[,i], 
       main = colnames(data_code)[i], palette = palette_new,
       shape = "straight")
}
par(mfrow = c(1,1))

#сделаем иерархическую кластеризацию
model_clust <- cutree(hclust(dist(data_code)), 4)

#выведем график
# Определяем палитру цветов
palette_new_2 <- function(n) {
  viridis(n)
}
plot(mod_som, type="mapping", bgcol = palette_new_2(4)[model_clust], 
     main = "Clusters", lwd = 0, shape = "straight") 
add.cluster.boundaries(mod_som, model_clust)
