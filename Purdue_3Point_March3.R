#upload necessary libraries
library(cbbdata)
library(cbbplotR)
library(gt)
library(gtExtras)
library(ggplot2)
library(dplyr)
library(glue)

#Pull player logs from cbbdata
logs = cbd_torvik_player_game()

#Filter for Purdue players
purdue = logs %>% filter(team=="Purdue")%>% mutate(newdate = as.Date(paste0(year,"-03-03"))) %>% filter(date<newdate)

#Create summary filtering by year and shot attempts
threesummary = purdue %>% filter(year>=2019) %>% 
  group_by(year,player) %>% filter(sum(three_a)/n()>=3) %>% 
  summarise(makes = sum(three_m),attempts = sum(three_a),tpp = round(makes/attempts,3))  

#Clean Summary
threesummary = threesummary %>% mutate(threes = paste0(makes,"-",attempts)) %>% 
  select(player,year,threes,tpp) %>% ungroup() %>% arrange(desc(tpp))

#Create Graphic
threes = threesummary

#Create Header
title_header <- glue(
  "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;'>
       <span style='font-weight: bold; font-size: 28px; line-height: 0.6;'>Purdue 3-Point Shooting</span><br>
       <span style='font-size: 16px; font-weight: normal; line-height: 0.3;'>Players with 3.0+ TPA/Game - Since 2019<br>Games through March 3</span>
     </div>
     <div>
       <img src='https://i.imgur.com/BR348zG.png' style='height: 40px; width: auto; vertical-align: right;'>
     </div>
   </div>"
)

#Graphic
gms = threes %>% select(player,year,threes,tpp) %>% 
  gt() |> gt::fmt_markdown() %>% 
  tab_header(title = html(title_header))  %>% 
  gt_theme_538() %>% 
  cols_align(
    align = "left",
    columns = c(player)) |>
  cols_align(
    align = "center",
    columns = c(year,threes,tpp)) |>
  tab_spanner(
    label = "3-Pointers",
    columns = c(threes,tpp)
  ) %>% 
  fmt_percent(
    columns = (tpp),
    decimals = 1
  ) %>% 
  cols_width(
    threes ~ px(110),
    tpp ~ px(110),
    year ~ px(80),
    player ~ px(200))%>%
  cols_label(
    player="Player",
    year="Season",
    threes = "Shots",
    tpp = "Percentage"
  )|> tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = tpp)) %>% 
  gt_highlight_rows(
    columns = gt::everything(),
    rows = c(1,2,4,13),
    fill = "#e2d5bd") |>tab_source_note(md('Analysis by @cobrastats <br>
  Data via cbbdata'))|>tab_style(
    style = cell_text(size = px(8)),
    locations = cells_source_notes()
  )%>%
  gtsave("/Users/connorbradley/Desktop/basketball data/purdue3ssince2019.png",expand = 25)

