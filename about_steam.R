library(readr)
steam_200k <- read_csv("~/repository2/steam-200k.csv", col_names = FALSE)

data = steam_200k 
colnames(data) <- c("userID", "game","status", "hours", "X") 
play = subset(data, status =="play") 
purchase = subset(data, status =="purchase") 
purchase[3] = NULL 
purchase[4] = NULL 
purchase[3] = NULL 
пп
 
Купленные игры в стим:
  length(unique(purchase$game))
5153 разных игр
length(unique(purchase$userID))
12393 пользователей

sort(table(purchase$game),decreasing=TRUE)[1:15]

По времени в игре стим:
  play[5] = NULL 
length(unique(play$game))
3598 разных игр
length(unique(play$userID))
11350 пользователей



# play = subset(play, hours > 6) не имеет смыслв из-за горизонтального нормирования

library("tibble")
sort(table(purchase$game),decreasing=TRUE)[1:15]

mean_time = as.data.frame(sort(by(play$hours, play$game, mean),decreasing=TRUE)[1:20])
mean_time = rownames_to_column(mean_time, var = "game")
colnames(mean_time) <- c("game","mean_of_hours_played") 



ggplot(mean_time, aes(x = reorder(game, -mean_of_hours_played), y=mean_of_hours_played, fill = game)) + geom_bar(stat = "identity") +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + xlab("") + ylab("mean time played (hours) ")



sum_time = as.data.frame(sort(by(play$hours, play$game, sum),decreasing=TRUE)[1:100])
sum_time = rownames_to_column(sum_time, var = "game")
colnames(sum_time) <- c("game","sum_of_hours_played") 

ggplot(sum_time, aes(x = reorder(game, -sum_of_hours_played), y=sum_of_hours_played, fill = game)) + geom_bar(stat = "identity") +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + xlab("") + ylab("sum time played (hours) ")


library(ggplot2)


STEAM = subset(play, play$game == "Dota 2" | play$game == "Counter-Strike Global Offensive"| play$game == "Team Fortress 2"| play$game == "Counter-Strike"| play$game == "Sid Meier's Civilization V"| play$game == "Counter-Strike Source"| play$game == "The Elder Scrolls V Skyrim"| play$game == "Garry's Mod"| play$game == "Call of Duty Modern Warfare 2 - Multiplayer"| play$game == "Left 4 Dead 2"| play$game == "Football Manager 2013"| play$game == "Football Manager 2012"| play$game == "Football Manager 2014"
               | play$game == "Terraria"| play$game == "Warframe"| play$game == "Football Manager 2015"| play$game == "Arma 3"| play$game == "Grand Theft Auto V"| play$game == "Borderlands 2"| play$game == "Empire Total War"| play$game == "Total War ROME II - Emperor Edition"| play$game == "Mount & Blade Warband"| play$game == "Call of Duty Modern Warfare 3 - Multiplayer"| play$game == "DayZ"| play$game == "PAYDAY 2"| play$game == "Call of Duty Black Ops - Multiplayer"| play$game == "Total War SHOGUN 2"
               | play$game == "Rust"| play$game == "Unturned"| play$game == "Fallout New Vegas"| play$game == "War Thunder"| play$game == ""| play$game == "Call of Duty Black Ops II - Multiplayer"| play$game == "ARK Survival Evolved"| play$game == "Football Manager 2010"| play$game == "Clicker Heroes"| play$game == "Football Manager 2011"| play$game == "Fallout 4"| play$game == "Path of Exile"| play$game == "Napoleon Total War"| play$game == "Europa Universalis IV"
               | play$game == "Arma 2 Operation Arrowhead"| play$game == "APB Reloaded"| play$game == "Crusader Kings II"| play$game == "Portal 2"| play$game == "Robocraft"| play$game == "Left 4 Dead"| play$game == "Call of Duty Modern Warfare 2"| play$game == "Killing Floor"| play$game == "Rocket League"| play$game == "Counter-Strike Condition Zero"| play$game == "XCOM Enemy Unknown"| play$game == "Football Manager 2009"| play$game == "H1Z1"| play$game == "Euro Truck Simulator 2"
               | play$game == "Torchlight II"| play$game == "The Binding of Isaac Rebirth"| play$game == "Age of Empires II HD Edition"| play$game == "FINAL FANTASY XIV A Realm Reborn"| play$game == "Battlefield Bad Company 2"| play$game == "DARK SOULS II"| play$game == "PlanetSide 2"| play$game == "AdVenture Capitalist"| play$game == "The Binding of Isaac"| play$game == "Call of Duty Modern Warfare 3"| play$game == "Kerbal Space Program"| play$game == "Rising Storm/Red Orchestra 2 Multiplayer"| play$game == "Starbound"| play$game == "Far Cry 3"
               | play$game == "Warhammer 40,000 Dawn of War II Retribution"| play$game == "The Witcher 3 Wild Hunt"| play$game == "Saints Row The Third"| play$game == "Tom Clancy's Ghost Recon Phantoms - NA"| play$game == "METAL GEAR SOLID V THE PHANTOM PAIN"| play$game == "Day of Defeat Source"| play$game == "Stronghold Kingdoms"| play$game == "Dark Souls Prepare to Die Edition"| play$game == "Borderlands"| play$game == "Trove"| play$game == "DC Universe Online"| play$game == "Grand Theft Auto IV"| play$game == "Space Engineers"| play$game == "Baldur's Gate II Enhanced Edition"
               | play$game == "Sid Meier's Civilization IV Beyond the Sword"| play$game == "Might & Magic Heroes VI"| play$game == "7 Days to Die"| play$game == "Half-Life 2"| play$game == "Call of Duty Ghosts - Multiplayer"| play$game == "Chivalry Medieval Warfare"| play$game == "Dungeon Defenders"| play$game == "Call of Duty Black Ops"| play$game == "BioShock Infinite"| play$game == "Half-Life 2 Deathmatch"| play$game == "Deus Ex Human Revolution"| play$game == "Star Trek Online"| play$game == "Don't Starve"| play$game == "Warhammer 40,000 Dawn of War II"
               | play$game == "Neverwinter"| play$game == "Medieval II Total War"| play$game == "Just Cause 2"| play$game == "Assassin's Creed IV Black Flag")

library(reshape)
library(Matrix)

# matrix = cast(STEAM, userID ~ game)


matrix <- read_delim("~/steam/STEAM1.csv", ";", escape_double = FALSE, locale = locale(grouping_mark = "."), trim_ws = TRUE)
matrix[,1] = NULL



matrix = data.matrix(matrix)

sqmatrix = sqrt(matrix) #уменьшить темп роста оценки 

numNAs <- apply(sqmatrix, 1, function(z) sum(is.na(z)))
sqmatrix = sqmatrix[numNAs < 94,] # оценено больше 5 игр
sum(rowSums(!is.na(sqmatrix)) < 94)

#горизонтальное нормировапние
for (i in (1:dim(sqmatrix)[1])) {
sqmatrix[i,] = cut(sqmatrix[i,], breaks = unique(quantile(sqmatrix[i,], c(.0, .20, .40, .60, 0.80, 1), na.rm = TRUE)),include.lowest = TRUE) }
max(sqmatrix, na.rm = TRUE)








steam_matrix <- as(sqmatrix, "realRatingMatrix")

similarity_users10 <- similarity(steam_matrix[1:10, ], method = "cosine", which = "users")

#вертикальное нормировапние
sqmatrix1 = sqmatrix
for (i in (1:dim(sqmatrix)[2])) {
  sqmatrix1[,i] = cut(sqmatrix[,i], breaks = unique(quantile(sqmatrix[,i], c(.0, .20, .40, .60, 0.80, 1), na.rm = TRUE)),include.lowest = TRUE) }


steam_matrix_v <- as(sqmatrix1, "realRatingMatrix")





average_ratings_per_user <- rowMeans(ratings_steam)
ggplot()+geom_histogram(aes(x=average_ratings_per_user)) +
  ggtitle("Распределение средних оценок пользователей")




set.seed(100)
steam.test.ind = sample(seq_len(nrow(steam_matrix_v)), size = nrow(steam_matrix_v)*0.2)
steam.test = steam_matrix_v[steam.test.ind,]
steam.main = steam_matrix_v[-steam.test.ind,]

recc_model <- Recommender(data = steam.test, method = "UBCF", parameter = list(method = "cosine"))
steam_predicted <- predict(object = recc_model, newdata = steam.test, n = 6)
str(steam_predicted)

model_details <- getModel(recc_model)
model_details$description
model_details$sim[1:5, 1:5]

recc_user_1 <- steam_predicted@items[[1]]
recc_user_1
games_user_1 <- steam_predicted@itemLabels[recc_user_1]
games_user_1



ЗАХАР!!!
  Я вижу работу системы так
user = (rep(NA,99))
Я обожаю биошок(11), бордерлендс оба(12 и 13), дарк соулс 2 (29)
неплохо отношусь к фаллауту 4 (41) и метал гиру (64)
терпеть не могу ассассинс крид (8)

оценки от 1 до 5, где 5 - обожаю, 1 - лучше бы никогда не играл
Таким образом, вектор моих оценок будет таким:

user[c(11,12,13,29)] = 5
user[c(41,64)] = 4
user[c(8)] = 1



Рекомендация для меня:
user = matrix(user,nrow = 1,ncol = 99)
  user_matrix <- as(user, "realRatingMatrix")

recommends <- predict(object = recc_model, newdata = user_matrix, n = 6)


recc <- recommends@items[[1]]
recc
games_user <- steam_predicted@itemLabels[recc]
games_user




