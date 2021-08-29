# Analysis of character average emotions

character_emotions_df <- f_emotions %>% 
  left_join(d_utterance, by = c("utterance_id", "scene_id", "episode_id")) %>% 
  # who speaks each line?
  left_join(f_utterance, by = c("utterance_id", "scene_id", "episode_id")) %>% 
  left_join(d_characters, by = c("speaker_character_id" = "character_id")) %>% 
  # from which episodes/scenes?
  left_join(d_scene, by = c("scene_id", "episode_id")) %>% 
  # count words in each utterance
  mutate(n_verbatim = sapply(strsplit(text, " "), length)) %>% 
  # count of words by character by episode for each emotion
  filter(main_character) %>% 
  group_by(character_name, season, episode, season_episode, emotion) %>% 
  summarise(n_verbatim = sum(n_verbatim)) %>% 
  # calculate %
  mutate(prop = n_verbatim / sum(n_verbatim),
         character_name = word(character_name, 1)) %>% 
  ungroup()

emotion_col_palette <- f_emotions %>% 
  distinct(emotion) %>% 
  mutate(col = c('red', 'grey15', 'yellow', 'purple', 'blue', 'orange', 'green'))
emotion_col_palette <- setNames(emotion_col_palette$col, emotion_col_palette$emotion)

season_episode_id <- character_emotions_df %>% 
  distinct(season, episode, season_episode) %>% 
  arrange(season, episode) %>% 
  mutate(season_episode_row_n = 1:n())

p <- character_emotions_df %>% 
  # most observed emotion by character by episode
  group_by(character_name, season, episode, season_episode) %>% 
  filter(prop == max(prop)) %>% 
  ungroup() %>% 
  # x axis
  mutate(season_episode = factor(season_episode, levels = season_episode_id$season_episode)) %>% 
  ggplot() +
  geom_vline(xintercept = c(25, 49, 74)) +
  geom_point(aes(x = season_episode, y = character_name, size = prop, colour = emotion, alpha = prop)) +
  scale_colour_manual(values = emotion_col_palette) +
  scale_alpha(guide = FALSE) +
  scale_size(range = c(1, 10), guide = FALSE) +
  scale_x_discrete("") +
  scale_y_discrete("") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)); p

p_bar <- character_emotions_df %>% 
  # most observed emotion by character by episode
  group_by(character_name, season, episode, season_episode) %>% 
  filter(prop == max(prop)) %>% 
  ungroup() %>% 
  mutate(season_episode = factor(season_episode, levels = season_episode_id$season_episode),
         emotion = factor(emotion, levels = c('Joyful', 'Peaceful', 'Powerful', 'Mad', 'Sad', 'Scared', 'Neutral'))) %>% 
  ggplot() +
  geom_vline(xintercept = c(24.5, 48.5, 73.5), colour = "white") +
  geom_col(aes(x = season_episode, y = prop, group = character_name, fill = emotion), 
           position = "dodge") +
  facet_grid(character_name~., switch = "y") +
  scale_fill_manual(values = emotion_col_palette) +
  scale_x_discrete("") +
  scale_y_continuous("", breaks = NULL) +
  theme_classic(base_family = "Gabriel") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(colour = 'white'),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(colour = 'white'),
        strip.background = element_rect(fill = "black", colour = NA),
        strip.text = element_text(colour = 'white', face = 'bold')
        ); p_bar


for(this_season in unique(character_emotions_df$season)){
  
  p_alluvial <- character_emotions_df %>% 
    filter(season == this_season & episode <= 10) %>% 
    distinct(character_name, season_episode, .keep_all = TRUE) %>% 
    select(character_name, season_episode, emotion) %>% 
    pivot_wider(id_cols = character_name, names_from = season_episode, values_from = emotion) %>%
    replace(is.na(.), "Neutral") %>% 
    gather(season_episode, emotion, 2:ncol(.)) %>% 
    mutate(season_episode = factor(season_episode, levels = season_episode_id$season_episode),
           emotion = factor(emotion, levels = c('Joyful', 'Peaceful', 'Powerful', 'Mad', 'Sad', 'Scared', 'Neutral'))) %>% 
    ggplot(aes(x = season_episode, stratum = emotion, alluvium = character_name,
               fill = emotion, label = emotion)) +
    scale_fill_manual(values = emotion_col_palette) +
    geom_stratum() +
    geom_flow(stat = "alluvium", lode.guidance = "frontback",
              color = "darkgray") +
    theme(legend.position = "bottom"); p_alluvial
  
}
