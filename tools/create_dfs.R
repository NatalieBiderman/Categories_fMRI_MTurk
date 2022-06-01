# This code creates data frames for category learning fMRI experiment. 
# The code was written by Natalie Biderman, March 2022


# ==============================================================================
# INSRET PARAMETERS 
# ==============================================================================

# general
n_designs = 4

# category learning 
n_novel_trials = 270; # should be divided by 10 (number of possible pairings)
n_exemplars_per_cat = 15;
n_cats = n_novel_trials*2/n_exemplars_per_cat; # 36
n_old_trials = n_novel_trials/3; # 90, should be divided by number of blocks 
n_total_stims = n_novel_trials * 2; # two new stims in each trial
n_practice_trials = 5;
n_decisions_trials = n_novel_trials + n_old_trials; # we want new trials within a block to be divided by 10 (the number of trial types)
n_blocks = 6;
old_stims_within_a_block = 1; # 1 if we want to limit the old items to be used from the same block
value_groups = c(20, 40, 60, 80); # make sure number of categories per cond divides equally by value groups
value_sd = 20;
n_first_novel_trials = 5; # the minimum of new trials in the beginning of a block
timing_category_learning = data.frame(sec = c(1000, 1500, 2000, 3000, 3500), reps = c(24, 14, 13, 4, 5)) # isi and iti timings, taken from an exponential distribution, aevraged to 1700 ms.

# size judgement
size_groups = c("S","M","L","XL")
n_size_cats = 20;
n_size_cats_per_group = n_size_cats/length(size_groups);
n_size_exemplars_per_cat = 9;
n_total_size_stims = n_size_cats * n_size_exemplars_per_cat
n_size_decisions_trials = n_total_size_stims/2;
n_size_blocks = 2;
timing_size = data.frame(sec = c(1000, 1500, 2000, 3000, 3500), reps = c(18, 12, 8, 4, 3)) # isi and iti timings, taken from an exponential distribution, aevraged to 1700 ms.

# RL task
n_rl_trials = 80; # should be divided by 10
n_rl_blocks = 2;
n_rl_trials_per_block = n_rl_trials/n_rl_blocks;
rl_value_groups_hard = c(40, 60); 
rl_value_groups_easy = c(20, 80); 
rl_value_sd = 20;
rl_percent_mean = 0.4; # percent of mean value group instances (if the value group is 60, then 0.4 of trials will include $60, 0.3 = $40, and 0.3 = $20)
# timing_RL = data.frame(sec = c(1000, 1500, 2000, 3000, 3500), reps = c(16, 10, 8, 3, 3)) # isi and iti timings, taken from an exponential distribution, averaged to 1700 ms.
timing_RL = data.frame(sec = c(1000, 1500, 2000, 3000, 3500), reps = c(8, 5, 4, 2, 1)) # isi and iti timings, taken from an exponential distribution, averaged to 1700 ms.


# ==============================================================================
# CREATE CATEGORY LEARNING DATA DRAME
# ==============================================================================

library("dplyr")

for (n in 1:n_designs){
  
  # create all possible combinations of value groups, and repeat them to create all novel trials.
  possible_pair_types <- as.data.frame(t(combn(value_groups, 2))) %>%
    rename(left_category_value = "V1", right_category_value = "V2") %>%
    bind_rows(data.frame(left_category_value = value_groups, right_category_value = value_groups)) %>%
    mutate(delta_value = right_category_value - left_category_value)
  
  # create the pair types we want to duplicate while considering a somewhat uniform distribution of delta value 
  repeated_pair_types <- bind_rows(possible_pair_types[possible_pair_types$delta_value==60,],
                                   possible_pair_types[possible_pair_types$delta_value==40,],
                                   possible_pair_types[possible_pair_types$left_category_value==40 & possible_pair_types$right_category_value==60,]) %>%
    slice(rep(1:n(), 2))
  possible_pair_types <- bind_rows(possible_pair_types, repeated_pair_types) 
                                   
  novel_trials <- possible_pair_types %>%
    slice(rep(1:n(), n_novel_trials/nrow(possible_pair_types))) %>%
    mutate(block = sort(rep(1:n_blocks, n_novel_trials/n_blocks)),
           high_cat_on_left = NaN)
  
  # add a vector for high category value on the left
  high_cat_on_left <- sample(rep(c(0,1), nrow(subset(novel_trials, delta_value>0))/2),replace=FALSE)
  for (i in 1:nrow(novel_trials)){
    if (novel_trials[i, "left_category_value"] != novel_trials[i, "right_category_value"] ){
      novel_trials[i, "high_cat_on_left"] = high_cat_on_left[1] # assign the first index
      high_cat_on_left = high_cat_on_left[-1] # remove the first index
    }
  }
  
  # create temp df
  all_trials_tmp <- data.frame(trial_num = NaN, 
                           block = sort(rep(1:n_blocks, n_decisions_trials/n_blocks)),
                           is_practice = 0,
                           old_trial = NaN,
                           pair_type = NaN,
                           left_category_value = NaN,
                           right_category_value = NaN,
                           delta_category_value = NaN,
                           high_cat_on_left = NaN,
                           iti = NaN,
                           isi = NaN)
  
  all_trials <- c()
  
  # run over blocks to (1) assign the position of old items, (2) assign left and 
  # right values for novel items, and (3) assign high category on left 
  
  for (b in 1:n_blocks){
    
    curr_block = all_trials_tmp %>% subset(block==b)
    
    # get novel trial for the current block
    curr_novel_trials = novel_trials %>% subset(block==b)
    curr_novel_trials = curr_novel_trials[sample(1:nrow(curr_novel_trials)), ]
    
    # ----------------------------------------------------------------------------
    # assign position of old item 
    # ----------------------------------------------------------------------------
    
    # create old trial bank
    is_old_trial = c(rep(1, n_old_trials/n_blocks), rep(0, n_novel_trials/n_blocks - n_first_novel_trials))
    
    # in each block, we run over trials to assign the position of old trials 
    for (t in 1:nrow(curr_block)){
      
      # first five trials in every block are new trials.
      if (t < n_first_novel_trials+1){
        curr_block[t,"old_trial"] = 0
      } else {
        
        # count how many old and new trials were presented until the current trial
        n_new_trials_until_t <- nrow(curr_block[1:t,] %>% subset(old_trial==0))
        n_old_trials_until_t <- nrow(curr_block[1:t,] %>% subset(old_trial==1))
        
        # make sure there at least two new trials that weren't used before to create an old trial
        if ((n_old_trials_until_t+1)*2 > n_new_trials_until_t){
          # if there aren't enough trials, we select a random novel index
          trial_index <- sample(which(is_old_trial==0),1)
        } else {
          # if there are enough trials we sample randomly any of the options 
          trial_index = sample(which(is_old_trial %in% c(0,1)),1)
        }
        
        # add old_trial assignment to df and remove the index from old+trial index
        curr_block[t, "old_trial"] = is_old_trial[trial_index]
        is_old_trial = is_old_trial[-trial_index]
      }
        
        # ------------------------------------------------------------------------
        # assign left and right values for novel trials 
        # ------------------------------------------------------------------------
        
        if (curr_block[t, "old_trial"]==0){
          curr_block[t, "left_category_value"] = curr_novel_trials[1,"left_category_value"]
          curr_block[t, "right_category_value"] = curr_novel_trials[1,"right_category_value"]
          curr_block[t, "pair_type"] = paste0(as.character(curr_block[t, "left_category_value"]), "-", as.character(curr_block[t, "right_category_value"]))
        }
      
        # ------------------------------------------------------------------------
        # assign high category on left 
        # ------------------------------------------------------------------------
        
        if (curr_block[t, "old_trial"]==0){
          curr_block[t, "high_cat_on_left"] = curr_novel_trials[1,"high_cat_on_left"]
          curr_novel_trials = curr_novel_trials[-1,] # remove the first index from the df
        }
        
        # switch position of left and right categories if default assignment doesn't match high_cat_on_left 
        if (!is.nan(curr_block[t, "high_cat_on_left"]) & curr_block[t, "old_trial"] == 0){
          if ((curr_block[t, "high_cat_on_left"] == 0 & curr_block[t, "left_category_value"] > curr_block[t, "right_category_value"]) |
              (curr_block[t, "high_cat_on_left"] == 1 & curr_block[t, "left_category_value"] < curr_block[t, "right_category_value"])){
            left = curr_block[t, "left_category_value"]; right = curr_block[t, "right_category_value"]
            curr_block[t, "left_category_value"] = right
            curr_block[t, "right_category_value"] = left
          }
        }
      
      curr_block[t, "delta_category_value"] = curr_block[t, "left_category_value"] - curr_block[t, "right_category_value"]
    } # t
    all_trials <- rbind(all_trials,curr_block) 
  } # b
  
  # ------------------------------------------------------------------------------
  # add timing variables for each block (run)
  # ------------------------------------------------------------------------------
  
  # we need to add the functions for computing the timing. 
  
  for (b in 1:n_blocks){
    
    isi = sample(rep(timing_category_learning$sec, timing_category_learning$reps), replace = FALSE)
    iti = sample(rep(timing_category_learning$sec, timing_category_learning$reps), replace = FALSE)
    
    all_trials[all_trials["block"]==b, "isi"] = isi
    all_trials[all_trials["block"]==b, "iti"] = iti
    
  }
  
  # ------------------------------------------------------------------------------
  # assign categories and items for novel trials 
  # ------------------------------------------------------------------------------
  
  # load stimuli csv
  all_stims <- read.csv("../Category_learning/Task/Stimuli/Stimuli_list/category_learning_stimuli.csv")
  
  # remove test items from stimuli set and keep only the first 15 exemplars 
  all_stims = subset(all_stims, is_test_item==0) %>%
    arrange(category)
  
  # assign categories to value groups according to n_designs 
  cats_order = cbind(value_groups, value_groups[c(2,3,4,1)], value_groups[c(3,4,1,2)], value_groups[c(4,1,2,3)]) # we rearrange the order of value groups 
  all_stims = all_stims %>%
    mutate(category_value = rep(as.numeric(cats_order[n,]), each=nrow(all_stims)/length(value_groups)),
           exemplar_value = NaN)
  
  # assign exemplar value
  all_cats = unique(all_stims["category"])
  row.names(all_cats) <- NULL
  for (c in 1:n_cats){
    # we randomize the order of sds from the mean
    exemp_values = sample(rep(c(-value_sd, 0, value_sd), n_exemplars_per_cat/3), replace = FALSE) 
    all_stims[all_stims$category==all_cats$category[c], "exemplar_value"] = 
      all_stims[all_stims$category==all_cats$category[c], "category_value"] + exemp_values
  }
  
  # for every new trial, randomly choose a stimulus according to the category value 
  # we also make sure not to pair up two stims from the same category in trials with similar category values
  experimental_stims = all_stims
  for (t in 1:nrow(all_trials)){
    if (all_trials[t, 'old_trial'] == 0){
      
      # assign left and right stims
      left_stim = sample_frac(subset(experimental_stims, category_value == all_trials[t,"left_category_value"]))[1,]
      right_stim = sample_frac(subset(experimental_stims, (category_value == all_trials[t,"right_category_value"]) & (category != left_stim$category)))[1,]
      
      # add parameters to data frame
      all_trials[t, "left_category"] = left_stim$category
      all_trials[t, "right_category"] = right_stim$category
      all_trials[t, "left_exemplar"] = left_stim$exemplar
      all_trials[t, "right_exemplar"] = right_stim$exemplar
      all_trials[t, "left_exemplar_value"] = left_stim$exemplar_value
      all_trials[t, "right_exemplar_value"] = right_stim$exemplar_value
      all_trials[t, "left_stim_index"] = left_stim$index
      all_trials[t, "right_stim_index"] = right_stim$index
      all_trials[t, "left_img"] = left_stim$img
      all_trials[t, "right_img"] = right_stim$img
      all_trials[t, "left_img_path"] = paste0("Stimuli/Experimental_stims/",left_stim$category,"/",left_stim$img)
      all_trials[t, "right_img_path"] = paste0("Stimuli/Experimental_stims/",right_stim$category,"/",right_stim$img)
      
    } else {
      
      all_trials[t, "left_category"] = NaN
      all_trials[t, "right_category"] = NaN
      all_trials[t, "left_exemplar"] = NaN
      all_trials[t, "right_exemplar"] = NaN
      all_trials[t, "left_exemplar_value"] = NaN
      all_trials[t, "right_exemplar_value"] = NaN
      all_trials[t, "left_stim_index"] = NaN
      all_trials[t, "right_stim_index"] = NaN
      all_trials[t, "left_img"] = NaN
      all_trials[t, "right_img"] = NaN
      all_trials[t, "left_img_path"] = NaN
      all_trials[t, "right_img_path"] = NaN
      
    }
    # remove the stims from our stimuli bank so they won't repeat again
    experimental_stims = subset(experimental_stims, !index %in% c(left_stim$index, right_stim$index))
  }
  
  # ------------------------------------------------------------------------------
  # add practice trials
  # ------------------------------------------------------------------------------
  
  # load practice stims 
  practice_stims <- read.csv("../Category_learning/Task/Stimuli/Stimuli_list/practice_category_learning_stimuli.csv")
  
  # manually assign category groups and values for practice stims
  practice_stims <- mutate(practice_stims,
                           category_value = c(20,20,20,40,40,60,60,80,80,80),
                           exemplar_value = c(0,20,40,40,20,40,60,80,100,100))
  
  # create decisions
  left_index = c(0, 4, 6, 3, 9);
  right_index = c(7, 1, 8, 5, 2);
  pair_type = c("20-80", "20-40", "60-80", "40-80","20-60");
  high_cat_on_left = c(0,1,0,0,1)
  isi = c(1500, 3000, 2000, 1000, 2500);
  iti = c(1000, 1500, 3000, 2000, 2500);
  
  practice_trials = data.frame()
  for (t in 1:length(left_index)){
    left_stim = subset(practice_stims, index==left_index[t])
    right_stim = subset(practice_stims, index==right_index[t])
    practice_trials[t, "trial_num"] = NaN;
    practice_trials[t, "block"] = 0;
    practice_trials[t, "is_practice"] = 1;
    practice_trials[t, "old_trial"] = 0;
    practice_trials[t, "pair_type"] = pair_type[t];
    practice_trials[t, "left_category_value"] = left_stim$category_value;
    practice_trials[t, "right_category_value"] = right_stim$category_value;
    practice_trials[t, "delta_category_value"] = left_stim$category_value - right_stim$category_value;
    practice_trials[t, "high_cat_on_left"] = high_cat_on_left[t];
    practice_trials[t, "iti"] = iti[t];
    practice_trials[t, "isi"] = isi[t];
    practice_trials[t, "left_category"] = left_stim$category
    practice_trials[t, "right_category"] = right_stim$category
    practice_trials[t, "left_exemplar"] = left_stim$exemplar
    practice_trials[t, "right_exemplar"] = right_stim$exemplar
    practice_trials[t, "left_exemplar_value"] = left_stim$exemplar_value
    practice_trials[t, "right_exemplar_value"] = right_stim$exemplar_value
    practice_trials[t, "left_stim_index"] = left_stim$index
    practice_trials[t, "right_stim_index"] = right_stim$index
    practice_trials[t, "left_img"] = left_stim$img
    practice_trials[t, "right_img"] = right_stim$img
    practice_trials[t, "left_img_path"] = paste0("Stimuli/Practice_stims/",left_stim$category,"/",left_stim$img)
    practice_trials[t, "right_img_path"] = paste0("Stimuli/Practice_stims/",right_stim$category,"/",right_stim$img)
  }
  
  # combine practice with regular trials 
  all_trials = rbind(practice_trials, all_trials)
  
  # add trial number 
  all_trials[,"trial_num"] = 0:(nrow(all_trials)-1)
  
  # save data frame
  readr::write_csv(all_trials,file(sprintf("../Category_learning/Task/Design_matrix/category_learning_trials_v%d.csv",n),encoding="UTF-8"))
  
}

# ==============================================================================
# CREATE CATEGORY MEMORY DATA FRAME
# ==============================================================================

all_stims <- read.csv("../Category_learning/Task/Stimuli/Stimuli_list/category_learning_stimuli.csv")

for (n in 1:n_designs){
  
  # find all categories 
  categories <- unique(all_stims$category) 
  
  # randomize order of categories and save within a data frame
  category_memory <- data.frame(trial_num = 0:(length(categories)-1), 
                                category = sample(categories))
  
  # load the relevant csv with info about values 
  category_learning_trials <- read.csv(paste0("../Category_learning/Task/Design_matrix/category_learning_trials_v",n,".csv")) %>% subset(old_trial == 0 & is_practice==0)
  left_cats <- category_learning_trials[,c("left_category", "left_category_value")]
  left_cats <- left_cats[!duplicated(left_cats),]
  
  # add value info
  for (i in 1:nrow(category_memory)){
    category_memory[i,"category_group_value"] = left_cats$left_category_value[left_cats$left_category == category_memory$category[i]]
  }
  
  # save 
  readr::write_csv(category_memory,file(sprintf("../static/Category_learning/Design_matrix/category_memory_trials_v%d.csv",n),encoding="UTF-8"))
}

# ==============================================================================
# CREATE SIZE DATA FRAME
# ==============================================================================

for (n in 1:n_designs){
  
  # load size stims 
  size_stims <- read.csv("../Size_judgement/Task/Stimuli/Stimuli_list/size_stimuli.csv")
  
  # remove test items 
  #size_stims = subset(size_stims, is_test_item==0)
  
  # for each category keep n_size_exemplars_per_cat per category
  curr_size_stims <- c()
  all_size_cats <- unique(size_stims$category)
  for (c in 1:n_size_cats){
    curr_cat <- size_stims %>% subset(category == all_size_cats[c]) %>% sample_frac()
    curr_cat <- curr_cat[1:n_size_exemplars_per_cat,]
    curr_size_stims <- rbind(curr_size_stims, curr_cat)
  }
  
  # ------------------------------------------------------------------------------
  # create trials
  # ------------------------------------------------------------------------------
  
  # create trial types (all combinations of size groups)
  possible_pair_types <- as.data.frame(t(combn(size_groups, 2))) %>%
    rename(left_size_group = "V1", right_size_group = "V2") %>%
    bind_rows(data.frame(left_size_group = size_groups, right_size_group = size_groups)) %>%
    mutate(pair_type = paste0(left_size_group,"-",right_size_group),
           left_size_num = case_when(left_size_group == "S" ~ 1,
                                     left_size_group == "M" ~ 2,
                                     left_size_group == "L" ~ 3,
                                     left_size_group == "XL" ~ 4),
           right_size_num = case_when(right_size_group == "S" ~ 1,
                                      right_size_group == "M" ~ 2,
                                      right_size_group == "L" ~ 3,
                                      right_size_group == "XL" ~ 4),
           delta_size = right_size_num - left_size_num)
  
  # create the pair types we want to duplicate while considering a somewhat uniform distribution of delta size 
  repeated_pair_types <- bind_rows(possible_pair_types[possible_pair_types$delta_size==3,],
                                   possible_pair_types[possible_pair_types$delta_size==2,],
                                   possible_pair_types[possible_pair_types$left_size_group=="M" & possible_pair_types$right_size_group=="L",]) %>%
    slice(rep(1:n(), 2))
  possible_pair_types <- bind_rows(possible_pair_types, repeated_pair_types) 
  
  pair_types <- possible_pair_types %>%
    slice(rep(1:n(), n_size_decisions_trials/nrow(possible_pair_types))) 
  
  size_trials <- data.frame(
    trial_num = NaN,
    block = sort(rep(1:n_size_blocks, n_size_decisions_trials/n_size_blocks)),
    pair_type = pair_types$pair_type,
    iti = NaN,
    isi = NaN,
    high_size_on_left = NaN,
    left_size_group = pair_types$left_size_group,
    right_size_group = pair_types$right_size_group,
    left_size_num = pair_types$left_size_num,
    right_size_num = pair_types$right_size_num)
  
  # add a vector for high size on the left
  high_size_on_left <- sample(rep(c(0,1), nrow(subset(size_trials, left_size_group!=right_size_group))/2),replace=FALSE)
  for (i in 1:nrow(size_trials)){
    if (size_trials[i, "left_size_group"] != size_trials[i, "right_size_group"] ){
      size_trials[i, "high_size_on_left"] = high_size_on_left[1] # assign the first index
      high_size_on_left = high_size_on_left[-1] # remove the first index
    }
  }
  
  # switch position of left and right size groups if default assignment doesn't match high_size_on_left 
  for (t in 1:nrow(size_trials)){
    if ((size_trials[t, "high_size_on_left"] == 0 & size_trials[t, "left_size_num"] > size_trials[t, "right_size_num"]) |
        (size_trials[t, "high_size_on_left"] == 1 & size_trials[t, "left_size_num"] < size_trials[t, "right_size_num"])){
      left_n = size_trials[t, "left_size_num"]; right_n = size_trials[t, "right_size_num"]
      left_s = size_trials[t, "left_size_group"]; right_s = size_trials[t, "right_size_group"]
      size_trials[t, "left_size_num"] = right_n
      size_trials[t, "right_size_num"] = left_n
      size_trials[t, "left_size_group"] = right_s
      size_trials[t, "right_size_group"] = left_s
    }
    size_trials[t, "delta_size"] = size_trials[t, "left_size_num"] - size_trials[t, "right_size_num"]
  }
  
  # ------------------------------------------------------------------------------
  # assign stims to trials
  # ------------------------------------------------------------------------------
  
  stims_bank = curr_size_stims
  for (t in 1:nrow(size_trials)){
    
    # we randomly select a stimulus according to its size group and then remove it from from stims bank
    left_stim = sample_frac(subset(stims_bank, size == size_trials[t,"left_size_group"]))[1,]
    # for the right stim, we make sure it isn't from the same category as the left stim
    right_stim = sample_frac(subset(stims_bank, size == size_trials[t,"right_size_group"] & category != left_stim$category))[1,]
    stims_bank = subset(stims_bank, !index %in% c(left_stim$index, right_stim$index))
    
    # assign the stims parameters 
    size_trials[t, "left_category"] = left_stim$category
    size_trials[t, "right_category"] = right_stim$category
    size_trials[t, "left_exemplar"] = left_stim$exemplar
    size_trials[t, "right_exemplar"] = right_stim$exemplar
    size_trials[t, "left_index"] = left_stim$index
    size_trials[t, "right_index"] = right_stim$index
    size_trials[t, "left_img"] = left_stim$img
    size_trials[t, "right_img"] = right_stim$img
    size_trials[t, "left_img_path"] = paste0("Stimuli/Size_stims/", left_stim$size, "/", left_stim$category, "/", left_stim$img)
    size_trials[t, "right_img_path"] = paste0("Stimuli/Size_stims/", right_stim$size, "/", right_stim$category, "/", right_stim$img)
    
  }
  
  # ------------------------------------------------------------------------------
  # add timing variables for each block (run) and shuffle order of trials within a block
  # ------------------------------------------------------------------------------
  
  # we need to add the functions for computing the timing. 
  
  for (b in 1:n_size_blocks){
    
    # shuffle order of trials 
    size_trials[size_trials$block==b,] = sample_frac(size_trials[size_trials$block==b,])
    
    # compute isi and iti
    isi = sample(rep(timing_size$sec, timing_size$reps), replace = FALSE)
    iti = sample(rep(timing_size$sec, timing_size$reps), replace = FALSE)
    
    size_trials[size_trials["block"]==b, "isi"] = isi
    size_trials[size_trials["block"]==b, "iti"] = iti
    
  }
  
  # add trial number 
  size_trials[,"trial_num"] = 0:(nrow(size_trials)-1)
  
  # save data frame
  write.csv(size_trials,sprintf("../static/Size_judgement/Design_matrix/size_trials_v%d.csv",n), row.names = FALSE, fileEncoding = "UTF-8")
  
}


# ==============================================================================
# CREATE RL DATA DRAME
# ==============================================================================

for (n in 1:n_designs){

  create_rl_trials <- function(rl_value_groups, condition){
    
    # create two value distributions for stims for each condition
    rl_value_groups = sort(rl_value_groups)
    low_dist = sample(c(rep(rl_value_groups[1], (n_rl_trials/2)*rl_percent_mean),
                        rep(rl_value_groups[1]+rl_value_sd, (n_rl_trials/2)*(1-rl_percent_mean)/2),
                        rep(rl_value_groups[1]-rl_value_sd, (n_rl_trials/2)*(1-rl_percent_mean)/2)))
    high_dist = sample(c(rep(rl_value_groups[2], (n_rl_trials/2)*rl_percent_mean),
                         rep(rl_value_groups[2]+rl_value_sd, (n_rl_trials/2)*(1-rl_percent_mean)/2),
                         rep(rl_value_groups[2]-rl_value_sd, (n_rl_trials/2)*(1-rl_percent_mean)/2)))
    
    # add block info 
    blocks = sort(rep(1:n_rl_blocks, (n_rl_trials/2)/n_rl_blocks));
    
    # assign stims according to n_design, so as to counterbalance the stims across participants
    if (n %% n_designs == 0){if (condition=="easy") {high_stim = "A"; low_stim = "B"} else {high_stim = "C"; low_stim = "D"}}
    if (n %% n_designs == 1){if (condition=="easy") {high_stim = "B"; low_stim = "A"} else {high_stim = "D"; low_stim = "C"}}
    if (n %% n_designs == 2){if (condition=="easy") {high_stim = "C"; low_stim = "D"} else {high_stim = "A"; low_stim = "B"}}
    if (n %% n_designs == 3){if (condition=="easy") {high_stim = "D"; low_stim = "C"} else {high_stim = "B"; low_stim = "A"}}
    
    # create high and low stims
    high_stims <- data.frame(img = high_stim, 
                             group_value = rl_value_groups[2],
                             item_value = high_dist)
    low_stims <- data.frame(img = low_stim, 
                            group_value = rl_value_groups[1],
                            item_value = low_dist)
    
    # randomize high item on left
    high_group_on_left = c()
    for (b in 1:n_rl_blocks){
      curr_high_group_on_left = sample(rep(c(0,1), (n_rl_trials/2)/n_rl_blocks))
      high_group_on_left = c(high_group_on_left,curr_high_group_on_left)
    }

    # create timing variables
    iti = c(); isi = c();
    for (b in 1:n_rl_blocks){
      curr_isi = sample(rep(timing_RL$sec, timing_RL$reps), replace = FALSE)
      curr_iti = sample(rep(timing_RL$sec, timing_RL$reps), replace = FALSE)
      iti = c(iti,curr_iti)
      isi = c(isi,curr_isi)
    }
    
    # create data frame
    curr_rl_trials <- data.frame(trial_num =  NaN,
                                 is_practice = 0,
                                 block = blocks,
                                 condition = condition,
                                 iti = iti,
                                 isi = isi)
    
    for (t in 1:(n_rl_trials/2)){
      if (high_group_on_left[t] == 1){
        curr_left_stim = high_stims[t,]; curr_right_stim = low_stims[t,];
      } else {
        curr_left_stim = low_stims[t,]; curr_right_stim = high_stims[t,];
      }
      curr_rl_trials[t, "high_group_on_left"] = high_group_on_left[t];
      curr_rl_trials[t, "left_group_value"] = curr_left_stim$group_value;
      curr_rl_trials[t, "right_group_value"] = curr_right_stim$group_value;
      curr_rl_trials[t, "left_item_value"] = curr_left_stim$item_value;
      curr_rl_trials[t, "right_item_value"] = curr_right_stim$item_value;
      curr_rl_trials[t, "delta_item_value"] = curr_left_stim$item_value - curr_right_stim$item_value;
      curr_rl_trials[t, "left_img"] = curr_left_stim$img;
      curr_rl_trials[t, "right_img"] = curr_right_stim$img;
      curr_rl_trials[t, "left_img_path"] = paste0("Stimuli/Fractals_stims/",curr_left_stim$img,".jpg");
      curr_rl_trials[t, "right_img_path"] = paste0("Stimuli/Fractals_stims/",curr_right_stim$img,".jpg");
    }
    
    return(curr_rl_trials)
  } 
  
  easy_trials <- create_rl_trials(rl_value_groups_easy, "easy")
  hard_trials <- create_rl_trials(rl_value_groups_hard, "hard")
  
  # concatenate all trial types
  rl_trials_tmp <- rbind(easy_trials, hard_trials) 
  
  # shuffle order of trials within a block
  
  rl_trials <- c()
  for (b in 1:n_rl_blocks){
    curr_block <- subset(rl_trials_tmp, block==b)
    curr_block <- curr_block[sample(nrow(curr_block), replace = FALSE),]
    rl_trials <- rbind(rl_trials, curr_block)
  }
  
  rl_trials[,"trial_num"] <- 0:(nrow(rl_trials)-1)

  # save data frame
  write.csv(rl_trials,sprintf("../static/Simple_RL/Design_matrix/RL_trials_v%d.csv",n), row.names = FALSE, fileEncoding = "UTF-8")
  
}
  
