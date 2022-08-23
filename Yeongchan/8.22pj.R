# library
library(ggridges)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(reshape)

lifeExp_B <- read_excel('./project_data_1.xlsx', sheet = '기대수명')
old_B<- read_excel('./project_data_1.xlsx', sheet = '부양비_노령화지수')
movement_B<- read_excel('./project_data_1.xlsx', sheet = '순이동률')
brith_B<- read_excel('./project_data_1.xlsx', sheet = '합계출산율')
infant_B<- read_excel('./project_data_1.xlsx', sheet = '영아사망률')

names(lifeExp_B)
lifeExp_B_1 <- lifeExp_B[,c(1,47)]
lifeExp_B_1
names(old_B)
names(old_B[,c(1,129)])
old_B_1 <- old_B[,c(1,129)]
old_B_1
names(movement_B)
movement_B_1 <- movement_B[,c(1,17)]
names(brith_B)
brith_B_1 <- brith_B[,c(1,17)]
names(infant_B)
infant_B_1 <- infant_B[,c(1,17)]


lifeExp_B_DF <- data.frame(lifeExp_B_1)
old_B_DF <- data.frame(old_B_1)
movement_B_DF<- data.frame(movement_B_1)
brith_B_DF<- data.frame(brith_B_1)
infant_B_DF<- data.frame(infant_B_1)

colnames(lifeExp_B_DF) <- c("시점", '기대수명')
lifeExp_B_DF_1 <- lifeExp_B_DF[-1,]
lifeExp_B_DF_1
old_B_DF
colnames(old_B_DF)<- c("시점", '노령화지수')
old_B_DF_1 <- old_B_DF[c(-(1:2)),]
old_B_DF_1

movement_B_DF
colnames(movement_B_DF) <- c("시점", '순이동률')
movement_B_DF_1 <- movement_B_DF
brith_B_DF
colnames(brith_B_DF) <- c("시점", '합계출산율')
brith_B_DF_1 <- brith_B_DF

infant_B_DF
colnames(infant_B_DF) <- c("시점", '영아사망률')
infant_B_DF_1 <- infant_B_DF


lifeExp_B_DF_1 # 2
old_B_DF_1 # 3
movement_B_DF_1 # 1
brith_B_DF_1 # 1
infant_B_DF_1 # 1

rownames(lifeExp_B_DF_1) <- NULL
lifeExp_B_DF_1
rownames(old_B_DF_1) <- NULL
old_B_DF_1

old_B_DF_1[2]
five_DF <- cbind(lifeExp_B_DF_1, old_B_DF_1[2], movement_B_DF_1[2],
      brith_B_DF_1[2], infant_B_DF_1[2])
five_DF

five_DF_1<- melt(five_DF, id.vars = c('시점'))
five_DF_1
class(five_DF_1[3])
seq(1955, 2100, 5)
data

str(five_DF_1)
five_DF_1[,3] <- as.numeric(five_DF_1[,3])

# Load dataset from github
data <- five_DF_M
str(data)
# Plot
data %>%
  mutate(variable = fct_reorder(variable, value)) %>%
  ggplot( aes(y=variable, x=value,  fill=variable)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Assigned Probability (%)")


data <- read.table("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv", header=TRUE, sep=",")
data <- data %>% 
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0)) %>%
  filter(text %in% c("Almost Certainly","Very Good Chance","We Believe","Likely","About Even", "Little Chance", "Chances Are Slight", "Almost No Chance"))

str(data)

min.max.scale<-function (x) {
  return ((x-min(x))/(max(x)-min(x)))
}
five_DF
five_DF_1 # 시점, variable, value
five_DF_m <- five_DF
five_DF_m
five_DF_m$기대수명<-min.max.scale(five_DF$기대수명)
five_DF_m$노령화지수<-min.max.scale(five_DF$노령화지수)
five_DF_m$순이동률<-min.max.scale(five_DF$순이동률)
five_DF_m$합계출산율<-min.max.scale(five_DF$합계출산율)
five_DF_m$영아사망률<-min.max.scale(five_DF$영아사망률)

five_DF_m

five_DF_2[]

str(five_DF)

five_DF[,2] <- as.numeric(five_DF[,2])
five_DF[,3] <- as.numeric(five_DF[,3])
five_DF[,4] <- as.numeric(five_DF[,4])
five_DF[,5] <- as.numeric(five_DF[,5])
five_DF[,6] <- as.numeric(five_DF[,6])

five_DF[,2]

five_DF_M <- melt(five_DF_m, id.vars = c('시점'))
five_DF_M
