# This script processes and cleans the utterance data set

main_character_friends <- c("Monica Geller", "Ross Geller", "Chandler Bing", "Joey Tribbiani", "Rachel Green", "Phoebe Buffay")

d_characters <- friends %>%
  distinct(speaker) %>% 
  drop_na(speaker) %>% 
  bind_rows(friends_entities %>% unnest(entities) %>% rename(speaker = entities)) %>% 
  distinct(speaker) %>% 
  mutate(character_id = 1:n(),
         main_character = speaker %in% main_character_friends) %>% 
  rename(character_name = speaker) %>% 
  select(character_id, character_name, main_character)

d_episode <- friends_info %>% 
  mutate(episode_id = 1:n(),
         season_episode = glue("S{season}E{episode}")) %>% 
  select(episode_id, everything())

d_scene <- friends %>%
  distinct(season, episode, scene) %>% 
  mutate(scene_id = 1:n(),
         season_episode_scene = glue("S{season}E{episode}_{scene}")) %>% 
  left_join(d_episode, by = c("season", "episode")) %>% 
  select(scene_id, everything())

d_utterance <- friends %>% 
  distinct(season, episode, scene, utterance, text) %>% 
  mutate(utterance_id = 1:n()) %>% 
  left_join(d_episode, by = c("season", "episode")) %>% 
  left_join(select(d_scene, season, episode, scene, scene_id), by = c("season", "episode", "scene")) %>% 
  select(utterance_id, utterance_within_scene = utterance, text, scene_id, episode_id)

f_emotions <- friends_emotions %>% 
  left_join(select(d_episode, season, episode, episode_id), by = c("season", "episode")) %>% 
  left_join(select(d_scene, season, episode, scene, scene_id), by = c("season", "episode", "scene")) %>% 
  left_join(select(d_utterance, utterance_within_scene, scene_id, episode_id, utterance_id), by = c("utterance" = "utterance_within_scene",
                                                                                                    "episode_id", "scene_id")) %>% 
  select(utterance_id, scene_id, episode_id, emotion)
  
f_utterance <- friends %>% 
  mutate(utterance_id = 1:n()) %>% 
  left_join(select(d_scene, scene_id, episode_id, season, episode, scene), by = c("season", "episode", "scene")) %>% 
  left_join(select(d_characters, character_id, character_name), by = c("speaker" = "character_name")) %>% 
  select(utterance_id, scene_id, episode_id, speaker_character_id = character_id)

f_entities <- friends_entities %>% 
  unnest(entities) %>% 
  left_join(select(d_episode, season, episode, episode_id), by = c("season", "episode")) %>% 
  left_join(select(d_scene, season, episode, scene, scene_id), by = c("season", "episode", "scene")) %>% 
  left_join(select(d_utterance, utterance_within_scene, scene_id, episode_id, utterance_id), by = c("utterance" = "utterance_within_scene",
                                                                                                    "episode_id", "scene_id")) %>% 
  select(utterance_id, scene_id, episode_id, entities) %>% 
  left_join(select(d_characters, character_name, character_id), by = c("entities" = "character_name")) %>% 
  select(utterance_id, scene_id, episode_id, entity_character_id = character_id)
