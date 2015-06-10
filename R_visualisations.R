#STAT 430 : Homework 5 ----
#### Avinash B 
pckgs = c("dplyr","treemap","streamgraph","ggplot2","babynames")
pkgs_loaded = lapply(pckgs, require, character.only=T)

data(Titanic)
d <- Titanic
d <- as.data.frame(d)


# Excercise 1 =====

# A) By Sex ####
by.sex<-as.data.frame(d %>% 
  group_by(Sex,Survived) %>%
  summarise(k = sum(Freq)) %>%
  mutate(survival.pct = k/sum(k),total = sum(k)) %>%
  filter(Survived == "Yes"))
 
treemap(by.sex,
        index=c("Sex"),
        vSize="total",
        vColor="survival.pct",
        type="value", title = "Fig 1 : Survival/population based on Sex",
        range = c(0,1))


# B) By Age ####
by.age<-as.data.frame(d %>% 
                        group_by(Age,Survived) %>%
                        summarise(k = sum(Freq)) %>%
                        mutate(survival.pct = k/sum(k), total = sum(k)) %>%
                        filter(Survived == "Yes"))
treemap(by.age,
        index=c("Age"),
        vSize="total",
        vColor="survival.pct",
        type="value", title = "Fig 2 : Survival/population based on Age", 
        range = c(0,1))

# C ) By Sex and Class #####

by.sex.class<-as.data.frame(d %>% 
                        group_by(Sex,Class,Survived) %>%
                        summarise(k = sum(Freq)) %>%
                        mutate(survival.pct = k/sum(k),total = sum(k))%>%
                        filter(Survived == "Yes"))
treemap(by.sex.class,
        index=c("Sex","Class"),
        vSize="total",
        vColor="survival.pct",
        type="value", title = "Fig 3 : Survival/population based on Sex and Class",
        range = c(0,1),
        palette=rainbow(10, s = 1, v = 1, start = 0, end = 1/6)
)


# D) By Age, Sex and Class #####

by.age.sex.class<-as.data.frame(d %>% 
                              group_by(Age,Sex,Class,Survived) %>%
                              summarise(k = sum(Freq)) %>%
                              mutate(survival.pct = k/sum(k),total = sum(k))%>%
                              filter(Survived == "Yes"))
                              
                              
treemap(by.age.sex.class,
        index=c("Age","Sex","Class"),
        vSize="total",
        vColor="survival.pct",
        type="value", title = "Fig 4 : Survival/population based on Age,Sex and Class",
        range = c(0,1),
        palette=rainbow(n = 10, s = 1, v = 1, start = 0,end = 1/6)
)


# Excercie 2 : Baby Names Graph Streams =====
# the following code snipets are a modification of the examples in class

# 1) Top Names ####

babynames %>% 
  group_by(year, sex) %>%
   top_n(1,n) -> top.names
top.names

streamgraph(top.names, "name", "n", "year") %>%
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = "year", tick_interval = 10, tick_format = "%Y")

# 2) Top name Female #####

babynames %>%
  filter(sex=="F", 
         name %in% top.names$name) -> top.names.female

streamgraph(top.names.female, "name", "n", "year") %>%
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = "year", tick_interval = 10, tick_format = "%Y")

# 3) Top name Male #####

babynames %>%
  filter(sex=="M", 
         name %in% top.names$name) -> top.names.male

streamgraph(top.names.male, "name", "n", "year") %>%
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = "year", tick_interval = 10, tick_format = "%Y")

# --------