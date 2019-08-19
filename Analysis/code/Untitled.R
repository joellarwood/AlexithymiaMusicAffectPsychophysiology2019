# To save plots 

# ggsave(filename = "valence.png",
# plot = valenceplot, 
# bg = "transparent")

transparent <-   theme(panel.background = element_rect(fill = "transparent"),
                       plot.background = element_rect(fill = "transparent", color = NA),
                       legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                       legend.box.background = element_rect(fill = "transparent"),
                       axis.line = element_line(color = "White"),
                       axis.text = element_text(color = "White"),
                       axis.title = element_text(color = "White"))

valenceplot <- interactions::interact_plot(valencemodel,
                            pred = TAS,
                            modx = affectcat, 
                            interval = TRUE)  + 
  ylab("Rated Valence") + 
  xlab("TAS") + 
  ylim(1,5) + 
  theme_classic() +
  transparent 

ggsave(filename = "ValencePlot.png",
       plot = valenceplot,
       bg = "transparent")

arousalplot <- interactions::interact_plot(arousalmodel,
                                           pred = TAS,
                                           modx = affectcat,
                                           interval = TRUE)  + 
  ylab("Rated Arousal") + 
  xlab("TAS") + 
  ylim (1,5) + 
  theme_classic() +
  transparent 

ggsave(filename = "ArousalPlot.png",
       plot = arousalplot,
       bg = "transparent")

valenceliking <- interactions::interact_plot(valencemodelliking,
                                             pred = likekey.keys,
                                             modx = affectcat,
                                             interval = TRUE)  + 
  ylab("Rated Valence") + 
  xlab("Song Liking") + 
  theme_classic() +
  transparent 

ggsave(filename = "ValenceLikingPlot.png",
       plot = valenceliking,
       bg = "transparent")

arousalliking <- interactions::interact_plot(arousalmodelliking,
                                             pred = likekey.keys,
                                             modx = affectcat,
                                             interval = TRUE)  + 
  ylab("Rated Arousal") + 
  xlab("Song Liking") + 
  theme_classic() +
  transparent 

ggsave(filename = "ArousalLikingPlot.png",
       plot = arousalliking,
       bg = "transparent")