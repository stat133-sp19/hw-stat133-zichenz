# title: Make Shot Charts Script
# description: Codes to produce different charts
# input: klay, curry, kevin, andre, draymond
# output: pdf of each player's shot chart, pdf and png of gsw's short chart


library(ggplot2)
library(jpeg)
library(grid)

#4.1) Shot charts of each player
court_file <- "../images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc")
)

#andre's chart
andre_shot_chart <- ggplot(data = andre) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()
pdf('../images/andre-iguodala-shot-chart.pdf', width = 6.5, height = 5)
andre_shot_chart
dev.off()

#draymond's chart
draymond_shot_chart <- ggplot(data = draymond) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()
pdf('../images/draymond-green-shot-chart.pdf', width = 6.5, height = 5)
draymond_shot_chart
dev.off()

#kevin's chart
kevin_shot_chart <- ggplot(data = kevin) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()
pdf('../images/kevin-durant-shot-chart.pdf', width = 6.5, height = 5)
kevin_shot_chart
dev.off()

#klay's chart
klay_shot_chart <- ggplot(data = klay) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()
pdf('../images/klay-thompson-shot-chart.pdf', width = 6.5, height = 5)
klay_shot_chart
dev.off()

#stephen's chart
curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()
pdf('../images/stephen-curry-shot-chart.pdf', width = 6.5, height = 5)
curry_shot_chart
dev.off()

#4.2) Facetted Shot Chart
facetted_shot_chart <- ggplot(data = combined) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Charts: GSW (2016 season)') +
  theme_minimal()+
  facet_wrap(~ name)
facetted_shot_chart

#save pdf
pdf('../images/gsw-shot-charts.pdf', width=8,height=7)
facetted_shot_chart
dev.off()
#save png
png('../images/gsw-shot-charts.png', width=8,height=7, units="in", res = 72)
facetted_shot_chart
dev.off()
