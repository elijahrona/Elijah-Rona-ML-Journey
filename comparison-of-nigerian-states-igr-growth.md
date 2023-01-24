## knit: markdowntemplates::to_jupyter

Title: "Comparison of Nigerian States IGR Growth" output:
rmarkdown::github_document ---
`{r, echo = FALSE} knitr::opts_chunk$set(   fig.path = "README_figs_States_IGR/README-" )`

# Comparison of Nigerian States' IGR Growth

I am using R to create a simple dashboard with the aim of comparing
states in Nigeria based on their IGR performance.

## Methodology

I collected the data from the [National Bureau of
Statistics](https://www.nigerianstat.gov.ng/) (NBS) website and analyzed
it using R.

## Loading the Libraries

``` {r}
library(tidyverse)
library(geodata)
library(RColorBrewer)
library(patchwork)
library(ggpubr)
library(readxl)
library(sf)
library(readxl)
```

## Loading the Dataset

``` {r}
aa <- read_excel("C:/Users/Octopus/Downloads/2019-2021 IGR  OF STATES.xlsx")
```

``` {r}
head(aa)
```

## Making a Few Changes

I want to remove the row with "Total," as I do not need it. Also, I want
to create extra columns in the dataset that will help with the colors
and other features of the charts.

``` {r}
#Create a column with the percentage difference between 2021 and 2019 for each state
aa["Percent_Change"] <- round((((aa["2021_TOTAL"]-aa["2019_TOTAL"])/aa["2019_TOTAL"])*100),2)

#Delete the row with "Total"
aa <- aa[-38,]

#Change "FCT" to its full name
aa[37,1] <- "Federal Capital Territory"

#Create two columns with colors
aa <- aa %>%
  mutate(cond = case_when(
    Percent_Change > 0 ~ '#45a15f',
    Percent_Change < 0 ~ '#F8766D',
    TRUE ~ '#F8766D'))

aa <- aa %>%
  mutate(cond1 = case_when(
    Percent_Change > 0 ~ -0.1,
    Percent_Change < 0 ~ -0.1,
  ))
```

## Visualization

### 2019

``` {r}
ggplot(data = aa, aes(x = State, y =`2019_TOTAL`)) +
  geom_bar(stat = "identity", aes(fill = "#45a15f")) +
  scale_fill_identity() +
  geom_text(aes(label = round(`2019_TOTAL`,0), hjust = cond1), size = 2.5) +
  coord_flip() + 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        axis.text = element_text(color="black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_x_discrete(limits = rev(levels(as.factor(aa$State)))) +
  ylim(0,750000000000)+
  labs(title = "2019 IGR of States",
       caption = "Data Source: National Bureau of Statistics \n Plot Created By: @Elijah_Rona")
```

### 2021

``` {r}
ggplot(data = aa, aes(x = State, y =`2021_TOTAL`)) +
  geom_bar(stat = "identity", aes(fill = "#45a15f")) +
  scale_fill_identity() +
  geom_text(aes(label = round(`2021_TOTAL`,0), hjust = cond1), size = 2.5) +
  coord_flip() + 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        axis.text = element_text(color="black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_x_discrete(limits = rev(levels(as.factor(aa$State)))) +
  ylim(0,900000000000)+
  labs(title = "2021 IGR of States",
       caption = "Data Source: National Bureau of Statistics \n Plot Created By: @Elijah_Rona")
```

### Compare 2019 and 2021

``` {r}
p1 <- ggplot(data = aa, aes(x = State, y =Percent_Change)) +
  geom_bar(stat = "identity", aes(fill = cond)) +
  scale_fill_identity() +
  geom_text(aes(label = Percent_Change, hjust = cond1), size = 2.5) +
  coord_flip() + 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        axis.text = element_text(color="black")) +
  scale_x_discrete(limits = rev(levels(as.factor(aa$State)))) +
  labs(title = "Increase Perentage",
       caption = "Data Source: National Bureau of Statistics \n Plot Created By: @Elijah_Rona")+
  xlab("") + 
  ylab("") +
  ylim(-30,150)
p1
```

### Choropleth Maps

``` {r}
states <- geodata::gadm("Nigeria",
                        level = 1,
                        path = tempdir()) |> 
sf::st_as_sf()
head(states)
```

``` {r}
states <- states %>% 
  rename(State = NAME_1)

df = merge(x=states,y=aa,by="State", all = FALSE)
```

``` {r}
df <- df %>%
  mutate(cond2 = case_when(
    `2019_TOTAL` < 15000000000 ~ "₦6-15  Billion",
    `2019_TOTAL` < 30000000000 ~ "₦15-30 Billion",
    `2019_TOTAL` < 60000000000 ~ "₦30-60 Billion",
    `2019_TOTAL` < 120000000000 ~ "₦60-120 Billion",
    `2019_TOTAL` < 240000000000 ~ "₦120-240 Billion",
    `2019_TOTAL` > 240000000000 ~ "More than ₦240 Billion"))

df$cond2 <- factor(df$cond2, levels = c('₦6-15  Billion',
                                        '₦15-30 Billion',
                                        '₦30-60 Billion',
                                        '₦60-120 Billion',
                                        '₦120-240 Billion',
                                        'More than ₦240 Billion'))

df <- df %>%
  mutate(cond3 = case_when(
    `2021_TOTAL` < 15000000000 ~ "₦6-15  Billion",
    `2021_TOTAL` < 30000000000 ~ "₦15-30 Billion",
    `2021_TOTAL` < 60000000000 ~ "₦30-60 Billion",
    `2021_TOTAL` < 120000000000 ~ "₦60-120 Billion",
    `2021_TOTAL` < 240000000000 ~ "₦120-240 Billion",
    `2021_TOTAL` > 240000000000 ~ "More than ₦240 Billion"))

df$cond3 <- factor(df$cond3, levels = c('₦6-15  Billion',
                                        '₦15-30 Billion',
                                        '₦30-60 Billion',
                                        '₦60-120 Billion',
                                        '₦120-240 Billion',
                                        'More than ₦240 Billion'))

head(df)
```

``` {r}
p2 <- ggplot(df) +
  geom_sf(aes(fill = cond2),
          color = "black",
          linetype = 1,
          linewidth = 0.8)+
  labs(title = "2019 IGR of States") +
  theme_void() +
  scale_fill_manual(values=c("#cbf5d6", "#71bd87",
                                      "#45a15f", "#22783a",
                                      "#0d5c23", "#023610"),
                                      name="")
p2
```

``` {r}
p3 <- ggplot(df) +
  geom_sf(aes(fill = cond3),
          color = "black",
          linetype = 1,
          linewidth = 0.8)+
  labs(title = "2021 IGR of States") +
  theme_void() +
  scale_fill_manual(values=c("#cbf5d6", "#71bd87",
                                      "#45a15f", "#22783a",
                                      "#0d5c23", "#023610"),
                                      name="")
p3
```

``` {r}
p4 <- ggarrange(p2, p3, ncol=2, common.legend = TRUE, legend="bottom")
p4
```

### Combining Charts to Make a Dashboard

``` {r}
p5 <- annotate_figure(p4,
                left = text_grob("**Total IGR Including Tax and Others**", color = "black", rot = 90),
                right = text_grob("**Data for 2022 not available yet**", color = "black", rot = 90))
p5
```

``` {r}
ggarrange(p5, p1, nrow=2, heights = c(1, 2))
```

## Conclusion

Using data to monitor the performance of states is better than using the
words (i.e., ordinary opinions) of people. There are so many insights
that we can draw from the available data, but what is certain is that
some states people praise truly need to work harder to increase their
IGR performance.

Check out my activities on [Twitter](https://twitter.com/elijah_rona)
and [LinkedIn](https://www.linkedin.com/in/elijah-rona/).
