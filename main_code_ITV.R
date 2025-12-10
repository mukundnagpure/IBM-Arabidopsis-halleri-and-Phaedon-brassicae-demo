run_simulation <- function(
  grid_size = 10,
  Np = 500,                   # Number of plants
  Sg = 0.5,                   # Share of glabrous plants
  Kc = 10,                    # Carrying capacity per grid cell for plants
  plant_lifespan = 2,
  fmax = 60,                  # Maximum flowers per plant
  c = 0.2,                    # Cost factor for hairy plant
  Nb = 50,                    # Number of beetles
  r_values = c(3,0),          # Possible relocation values
  r_probs = c(1,0),           # Probabilities for r values in population
  bmax = 10,                  # Maximum beetles per plant
  max_eggs = 100,             # Maximum number of eggs per beetle initially
  mutation_rate = 0.01,       # Mutation rate for plant reproduction
  d = 0.3,                    # Damage by one larvae (in no. of flowers)
  plant_mortality = 0.1,      # Random chance for plants to die
  t = 0.5,                    # Preference threshold
  t_sd = 0,
  beetle_mortality = 0.05,    # Beetles die due to natural causes
  egg_mortality_factor = 0.9, # Additional egg mortality factor
  years = 50,                 # Simulation years
  beetle_gen = 2,             # Generations of beetles per year
  boldness = 0.5              # Probability that beetles move to neighboring cells
) {
  
  # Derived parameters
  f_hairy <- fmax * (1 - c)   # Maximum flowers for hairy plants
  
  #Function to generate ti values based on distribution type, comment and uncomment
  generate_ti <- function(n) {
    # For normal distribution:
    return(pmax(0, pmin(1, rnorm(n, mean = t, sd = t_sd))))
    
    # For bimodal distribution:
    #return(sample(c(t, (1-t)), n, replace = TRUE, prob = c(0.5, 0.5)))
  }
  
  # Create grid structure
  cell_ids <- 1:(grid_size^2)
  
  grid_data <- data.frame(
    cell_id = cell_ids,
    row = ((cell_ids - 1) %/% grid_size) + 1,
    col = ((cell_ids - 1) %% grid_size) + 1
  )
  
  # Set up neighbor list
  # Define possible relative positions of the 8 neighbors (excluding (0, 0))
  neighbor_positions <- subset(
    expand.grid(row_offset = -1:1, col_offset = -1:1), 
    !(row_offset == 0 & col_offset == 0)
  )
  
  # Initialize a list to store neighbors for each cell
  neighbors_list <- list()
  
  # Loop through each cell in the grid
  for (i in 1:nrow(grid_data)) {
    row <- grid_data$row[i]
    col <- grid_data$col[i]
    
    # Initialize a vector to store neighbors of the current cell
    neighbor_ids <- integer(8)
    
    # For each neighbor position, calculate the neighbor's coordinates
    for (j in 1:nrow(neighbor_positions)) {
      new_row <- ((row + neighbor_positions$row_offset[j] - 1) %% grid_size) + 1
      new_col <- ((col + neighbor_positions$col_offset[j] - 1) %% grid_size) + 1
      
      # Get the neighbor's cell ID
      neighbor_ids[j] <- grid_data$cell_id[grid_data$row == new_row & grid_data$col == new_col]
    }
    
    # Store the neighbor IDs for the current cell
    neighbors_list[[i]] <- neighbor_ids
  }
  
  # Initial plant distribution
  # Generate plant data
  genotypes <- sample(
    c("HH", "Hh", "hh"),
    Np,
    replace = TRUE,
    prob = c((1 - sqrt(Sg))^2, 2 * sqrt(Sg) * (1 - sqrt(Sg)), Sg)
  )
  
  plants_df <- data.frame(
    plant_id = seq_len(Np),
    genotype = genotypes,
    age = sample(1:plant_lifespan, Np, replace = TRUE),
    cell_id = integer(Np),
    larvae_count = integer(Np),  # Initialize larvae_count when creating plants_df
    flowers = integer(Np)        # Initialize flowers when creating plants_df
  )
  
  
  # Initialize a count vector for plants in each cell
  plant_counts <- integer(grid_size^2)
  vacant_per_cell <- rep(Kc, grid_size^2) - plant_counts
  
  # Create list of vacant positions
  vacant_positions <- rep(1:grid_size^2, times=vacant_per_cell)
  
  # Distribute plants across the grid
  for (k in seq_len(Np)) {
    # Randomly assign the plant to one of the vacant cells
    cell_id <- sample(vacant_positions, 1)
    plants_df$cell_id[k] <- cell_id
    
    # Update plant count for the assigned cell
    plant_counts[cell_id] <- plant_counts[cell_id] + 1
    # Update vacant_per_cell for the assigned cell
    vacant_per_cell[cell_id] <- vacant_per_cell[cell_id] - 1
    
    # Remove only one instance of cell_id from vacant_positions
    remove_index <- match(cell_id, vacant_positions)  # Find the first occurrence
    vacant_positions <- vacant_positions[-remove_index]
  }
  
  
  # Initialize beetle population and distribute randomly
  # Generate beetle data
  beetle_df <- data.frame(
    beetle_id = seq_len(Nb),
    relocations = sample(r_values, Nb, replace = TRUE, prob = r_probs),
    eggs = sample(c(max_eggs, max_eggs * 0.6), Nb, replace = TRUE, prob = c(Sg, (1-Sg))),
    boldness = rep(boldness, Nb), # Probability that beetles move to neighboring cells during search
    ti = generate_ti(Nb),  # Individual threshold parameter
    cell_id = integer(Nb),
    plant_id = integer(Nb)  # Added column to track which plant each beetle is on
  )
  
  # Initialize beetle counts for each cell
  beetle_counts_per_cell <- integer(grid_size^2)
  
  # Distribute beetles randomly
  valid_beetle_indices <- integer(0)
  
  # Distribute beetles randomly to cells that have plants
  for (b in seq_len(Nb)) {
    # Get cells that have at least one plant
    valid_cells <- cell_ids[plant_counts > 0]
    
    if (length(valid_cells) > 0) {
      # Randomly assign beetle to a cell with plants
      beetle_df$cell_id[b] <- sample(valid_cells, 1)
      beetle_df$plant_id[b] <- 0  # Not yet assigned to a plant
    } else {
      
    }
  }
  
  # Remove beetles that couldn't be placed
  beetle_df <- beetle_df[beetle_df$cell_id > 0, ]
  
  
  # Initialize analysis_df for summary statistics
  analysis_df <- data.frame() 
  
  # Main simulation loop
  for (year in 1:years) {
    
    # Record initial plant counts BEFORE reproduction
    initial_total_plants <- nrow(plants_df)
    hairy_indices <- plants_df$genotype == "HH" | plants_df$genotype == "Hh"
    glabrous_indices <- plants_df$genotype == "hh"
    initial_hairy_plants <- sum(hairy_indices)
    initial_glabrous_plants <- sum(glabrous_indices)
    initial_prop_hairy <- ifelse(initial_total_plants > 0, initial_hairy_plants / initial_total_plants, 0)
    initial_prop_glabrous <- ifelse(initial_total_plants > 0, 1 - initial_prop_hairy, 0)
    
    # Initialize generation-specific beetle counts
    gen_beetles <- rep(0, beetle_gen)  # Initialize vector for all generations
    
    # Plants produce flowers based on genotype
    plants_df$flowers <- ifelse(
      plants_df$genotype == "hh", 
      fmax,  # Glabrous plants
      f_hairy  # Hairy plants
    )
    
    # Record flower counts before beetle damage
    pre_damage_avg_flowers_total <- ifelse(nrow(plants_df) > 0, mean(plants_df$flowers), 0)
    pre_damage_avg_flowers_hairy <- ifelse(sum(hairy_indices) > 0, mean(plants_df$flowers[hairy_indices]), NA)
    pre_damage_avg_flowers_glabrous <- ifelse(sum(glabrous_indices) > 0, mean(plants_df$flowers[glabrous_indices]), NA)
    
    
    # Beetle interaction loop
    total_larvae <- 0
    
    # Skip beetle processing if no beetles
    if (Nb == 0 || nrow(beetle_df) == 0) {
      # Set all generation beetle counts to 0
      gen_beetles <- rep(0, beetle_gen)
    } else { 
      
      for (gen in 1:beetle_gen) {
        # First check if there are any plants left
        if (nrow(plants_df) == 0) {
          # No plants left - skip all remaining beetle processing for all generations
          # Empty beetle dataframe since there are no plants for them to survive on
          beetle_df <- data.frame(
            beetle_id = integer(0),
            relocations = integer(0),
            eggs = numeric(0),
            boldness = numeric(0),
            ti = numeric(0),
            cell_id = integer(0),
            plant_id = integer(0)
          )
          break  # Exit the generation loop
        }
        
        # Reset larvae count for new generation - explicitly set for all plants
        plants_df$larvae_count <- 0
        
        # Apply beetle mortality before plant selection
        if (nrow(beetle_df) > 0) {
          # Each beetle survives with probability (1 - beetle_mortality)
          survivors <- rbinom(nrow(beetle_df), size = 1, prob = 1 - beetle_mortality) == 1
          # If we have any survivors
          if(any(survivors)){
            # Keep only surviving beetles
            beetle_df <- beetle_df[survivors, ]
            
            ## All beetles search for a plant and lay eggs
            remaining_beetles <- 1:nrow(beetle_df)
            living_beetles <- rep(TRUE, nrow(beetle_df))
            
            while (length(remaining_beetles) > 0) {
              # Pick the next beetle to process (random)
              b <- remaining_beetles[sample(length(remaining_beetles), 1)]
              
              beetle <- beetle_df[b, ]
              current_cell <- beetle$cell_id
              relocations_left <- beetle$relocations
              
              # Draw actual eggs from normal distribution with sd of 20% of the mean
              eggs_to_lay <- round(max(0, rnorm(1, mean = beetle$eggs, sd = 0.2 * beetle$eggs)))
              
              found_plant <- FALSE
              
              while (!found_plant && relocations_left >= 0) {
                # Use individual beetle boldness
                if (runif(1) < beetle$boldness) {
                  neighbors <- neighbors_list[[current_cell]]
                  current_cell <- sample(neighbors, 1)
                  beetle_df$cell_id[b] <- current_cell
                }
                
                # Get indices of all plants in current cell
                plant_indices <- which(plants_df$cell_id == current_cell)
                
                if (length(plant_indices) > 0) {
                  # Find plants that have less than bmax beetles
                  viable_plant_indices <- c()
                  
                  for (p in plant_indices) {
                    # Count beetles specifically on this plant
                    beetles_on_plant <- sum(beetle_df$plant_id == plants_df$plant_id[p] & living_beetles)
                    # Only count beetles that are still alive in this generation
                    
                    if (beetles_on_plant < bmax) {
                      viable_plant_indices <- c(viable_plant_indices, p)
                    }
                  }
                  
                  # Check if there are any viable plants in this cell
                  if (length(viable_plant_indices) > 0) {
                    # Randomly select a viable plant from the cell
                    chosen_index <- sample(viable_plant_indices, 1)
                    chosen_plant <- plants_df[chosen_index, ]
                    
                    # Update the beetle's plant assignment
                    beetle_df$plant_id[b] <- plants_df$plant_id[chosen_index]
                    
                    # Use initial_prop_glabrous instead of recalculating
                    current_Sg <- initial_prop_glabrous
                    
                    # Check if below threshold or found glabrous plant or out of relocations
                    if (current_Sg <  beetle$ti || chosen_plant$genotype == "hh" || relocations_left == 0) { #current_Sg <=  beetle$ti
                      # Either below threshold (no preference) or found glabrous or out of relocations
                      lambda_eggs <- eggs_to_lay * (1 - egg_mortality_factor)
                      surviving_eggs <- rpois(1, lambda_eggs)
                      plants_df$larvae_count[chosen_index] <- plants_df$larvae_count[chosen_index] + surviving_eggs
                      found_plant <- TRUE
                    } else {
                      # Found hairy, still has relocations, and above threshold
                      relocations_left <- relocations_left - 1
                    }
                  } else {
                    # No viable plants in cell, reduce relocation
                    relocations_left <- relocations_left - 1
                  }
                } else {
                  # No plants in cell, reduce relocation
                  relocations_left <- relocations_left - 1
                }
              }
              
              # If beetle didn't find a plant, mark it as dead and clear its plant assignment
              if (!found_plant) {
                living_beetles[b] <- FALSE
                beetle_df$plant_id[b] <- 0  # Clear plant assignment for beetles that didn't find a plant
              }
              
              # Remove this beetle from the remaining list
              remaining_beetles <- remaining_beetles[remaining_beetles != b]
            }
            
            # Keep only living beetles for next generation calculations
            beetle_df <- beetle_df[living_beetles, ]
            
            # Record beetle count for this generation before interactions
            gen_beetles[gen] <- nrow(beetle_df) 
            
            ## Calculate mortality based on density dependence
            plants_with_larvae <- which(plants_df$larvae_count > 0)
            surviving_larvae_counts <- integer(length(plants_with_larvae))
            
            for (i in seq_along(plants_with_larvae)) {
              plant_idx <- plants_with_larvae[i]
              
              # Calculate current carrying capacity based on available flowers
              # Each larva consumes 'd' flowers, so capacity = flowers/d
              plant_larvae_capacity <- plants_df$flowers[plant_idx] / d
              
              # Calculate survival probability (density-dependent)
              larvae_counts <- plants_df$larvae_count[plant_idx]
              larvae_survival_prob <- ifelse(
                larvae_counts > 0,
                pmax(0, pmin(plant_larvae_capacity / larvae_counts, 1)),
                0
              )
              
              # Apply survival probability to get surviving larvae
              surviving_larvae_counts[i] <- rbinom(1, larvae_counts, larvae_survival_prob)
            }
            
            ## Surviving larvae feed and damage plants
            for (i in seq_along(plants_with_larvae)) {
              plant_idx <- plants_with_larvae[i]
              plants_df$larvae_count[plant_idx] <- surviving_larvae_counts[i]
              # Surviving larvae feed and reduce flowers (preventing negative values)
              plants_df$flowers[plant_idx] <- max(0, plants_df$flowers[plant_idx] - surviving_larvae_counts[i] * d)
            }
            
            ## Surviving larvae become new beetles
            next_gen_beetles <- data.frame(
              beetle_id = integer(0),
              relocations = integer(0),
              eggs = numeric(0),
              boldness = numeric(0),
              ti = numeric(0),
              cell_id = integer(0),
              plant_id = integer(0)
            )
            
            # Process each plant with surviving larvae
            for (i in seq_along(plants_with_larvae)) {
              plant_idx <- plants_with_larvae[i]
              surviving_larvae <- surviving_larvae_counts[i]
              
              if (surviving_larvae > 0) {
                # Set egg capacity based on parent's host plant type and adjusted by population size
                new_max_eggs <- ifelse(
                  plants_df$genotype[plant_idx] == "hh",
                  max_eggs,
                  max_eggs * 0.6
                )
                
                
                # Add new beetles to next generation
                if (surviving_larvae > 0) {
                  new_beetles <- data.frame(
                    beetle_id = seq(nrow(next_gen_beetles) + 1, length.out = surviving_larvae),
                    relocations = sample(r_values, surviving_larvae, replace = TRUE, prob = r_probs),
                    eggs = rep(new_max_eggs, surviving_larvae),
                    boldness = boldness,
                    ti = generate_ti(surviving_larvae),
                    cell_id = rep(plants_df$cell_id[plant_idx], surviving_larvae),
                    plant_id = rep(0, surviving_larvae)
                  )
                  
                  next_gen_beetles <- rbind(next_gen_beetles, new_beetles)
                }
              }
            }
            if (nrow(next_gen_beetles) > (nrow(plants_df) * bmax)) {
              # If too many beetles, randomly select only up to the limit
              next_gen_beetles <- next_gen_beetles[sample(1:nrow(next_gen_beetles), nrow(plants_df) * bmax), ]
            }
            # Update beetle population for next generation
            if (nrow(next_gen_beetles) > 0) {
              beetle_df <- next_gen_beetles
            } else {
              # If no new beetles, empty the beetle population
              beetle_df <- data.frame(
                beetle_id = integer(0),
                relocations = integer(0),
                eggs = numeric(0),
                boldness = numeric(0),
                ti = numeric(0),
                cell_id = integer(0),
                plant_id = integer(0)
              )
            }
          }
          else {
            # Handle case where all beetles died
            beetle_df <- data.frame(
              beetle_id = integer(0),
              relocations = integer(0),
              eggs = numeric(0),
              boldness = numeric(0),
              ti = numeric(0),
              cell_id = integer(0),
              plant_id = integer(0)
            )
          }
        }
        # After larvae feeding, record the total larvae count for this generation
        total_larvae <- total_larvae + sum(plants_df$larvae_count)
      }
    }
    # Record flower counts AFTER beetle damage but BEFORE plant reproduction
    post_damage_avg_flowers_total <- ifelse(nrow(plants_df) > 0, mean(plants_df$flowers), 0)
    post_damage_avg_flowers_hairy <- ifelse(sum(hairy_indices) > 0, mean(plants_df$flowers[hairy_indices]), NA)
    post_damage_avg_flowers_glabrous <- ifelse(sum(glabrous_indices) > 0, mean(plants_df$flowers[glabrous_indices]), NA)
    
    
    # PLANT REPRODUCTION PHASE
    # Only plants with flowers can reproduce
    reproducing_plants <- plants_df[plants_df$flowers > 0, ]
    
    if (nrow(reproducing_plants) > 0) {
      # Calculate carrying capacity constraints
      total_potential_seeds <- sum(reproducing_plants$flowers)
      total_available_space <- sum(Kc * grid_size^2 - nrow(plants_df))
      
      # Probability of seed survival (density-dependent)
      seed_survival_prob <- max(0, min(total_available_space / total_potential_seeds, 1))
      
      # Calculate current number of plants in each cell once
      plant_counts_current <- table(factor(plants_df$cell_id, levels=1:(grid_size^2)))
      # Find cells with space available
      available_cells <- which(plant_counts_current < Kc)
      
      # Initialize new plants dataframe
      new_plants <- data.frame(
        plant_id = integer(0),
        genotype = character(0),
        age = integer(0),
        cell_id = integer(0),
        larvae_count = integer(0),
        flowers = integer(0)
      )
      
      # Generate seeds based on flowers and survival probability
      for (i in 1:nrow(reproducing_plants)) {
        parent1 <- reproducing_plants[i, ]
        
        # Calculate number of successful seeds
        successful_seeds <- rbinom(1, floor(parent1$flowers), seed_survival_prob)
        
        # Only process seeds that will survive
        if (successful_seeds > 0) {
          # Select random mate from reproducing plants EXCEPT parent1
          mate_candidates <- reproducing_plants[reproducing_plants$plant_id != parent1$plant_id, ]
          
          # Make sure there are other plants available for mating
          if (nrow(mate_candidates) > 0) {
            parent2 <- mate_candidates[sample(nrow(mate_candidates), 1), ]
            
            # Determine gametes from each parent
            # For parent1
            if (parent1$genotype == "HH") {
              gamete1 <- "H"
            } else if (parent1$genotype == "hh") {
              gamete1 <- "h"
            } else {  # Heterozygous "Hh"
              gamete1 <- sample(c("H", "h"), 1)
            }
            
            # For parent2
            if (parent2$genotype == "HH") {
              gamete2 <- "H"
            } else if (parent2$genotype == "hh") {
              gamete2 <- "h"
            } else {  # Heterozygous "Hh"
              gamete2 <- sample(c("H", "h"), 1)
            }
            
            # Apply possible mutation to gametes
            if (runif(1) < mutation_rate) {
              gamete1 <- ifelse(gamete1 == "H", "h", "H")
            }
            if (runif(1) < mutation_rate) {
              gamete2 <- ifelse(gamete2 == "H", "h", "H")
            }
            
            # Determine offspring genotype
            if (gamete1 == "H" && gamete2 == "H") {
              offspring_genotype <- "HH"
            } else if (gamete1 == "h" && gamete2 == "h") {
              offspring_genotype <- "hh"
            } else {
              offspring_genotype <- "Hh"
            }
            
            # Find a vacant cell for the new plant
            parent_cell <- parent1$cell_id
            
            # First decide if seed is local or long-distance disperser
            is_local <- runif(1) < 0.6  # 60% of seeds are local dispersers
            
            chosen_cell <- NULL
            
            # Simplified local dispersal code
            if (is_local) {
              if (parent_cell %in% available_cells) {
                chosen_cell <- parent_cell
              } else {
                is_local <- FALSE
              }
            }
            
            if (!is_local) {
              # Get all neighbor cells
              neighbor_cells <- neighbors_list[[parent_cell]]
              
              # Filter to only include neighbors with available space
              available_neighbors <- neighbor_cells[neighbor_cells %in% available_cells]
              
              if (length(available_neighbors) > 0) {
                # Only select from neighbors with space
                chosen_cell <- sample(available_neighbors, 1)
              } else {
                # Skip this seed if no neighbors have space
                next
              }
            }
            
            # Create new plant
            new_plant <- data.frame(
              plant_id = max(plants_df$plant_id) + nrow(new_plants) + 1,
              genotype = offspring_genotype,
              age = 0,  
              cell_id = chosen_cell,
              larvae_count = 0,
              flowers = 0  
            )
            
            new_plants <- rbind(new_plants, new_plant)
          }
        }
      }
      
      # Age existing plants
      plants_df$age <- plants_df$age + 1
      
      # Age-based mortality
      age_viable_plants <- plants_df[plants_df$age <= plant_lifespan, ]
      
      # Random mortality only to plants that haven't already died of old age
      random_survival <- runif(nrow(age_viable_plants)) > plant_mortality
      surviving_plants <- age_viable_plants[random_survival, ]
      
      # Combine surviving plants with new plants
      if (nrow(surviving_plants) > 0 || nrow(new_plants) > 0) {
        plants_df <- rbind(surviving_plants, new_plants)
      } else {
        # Handle the case where both are empty
        plants_df <- data.frame(
          plant_id = integer(0),
          genotype = character(0),
          age = integer(0),
          cell_id = integer(0),
          larvae_count = integer(0),
          flowers = integer(0)
        )
      }
      
      # Ensure row names are sequential
      if (nrow(plants_df) > 0) {
        rownames(plants_df) <- 1:nrow(plants_df)
      }
    } else {
      # If no plants can reproduce, just age the existing plants
      plants_df$age <- plants_df$age + 1
      plants_df <- plants_df[plants_df$age <= plant_lifespan, ]
    }
    
    # Calculate end-of-year plant counts for consistent timing
    end_year_total_plants <- nrow(plants_df)
    end_year_hairy_indices <- plants_df$genotype == "HH" | plants_df$genotype == "Hh"
    end_year_glabrous_indices <- plants_df$genotype == "hh"
    end_year_hairy_plants <- sum(end_year_hairy_indices)
    end_year_glabrous_plants <- sum(end_year_glabrous_indices)
    end_year_prop_hairy <- ifelse(end_year_total_plants > 0, end_year_hairy_plants / end_year_total_plants, 0)
    end_year_prop_glabrous <- ifelse(end_year_total_plants > 0, end_year_glabrous_plants / end_year_total_plants, 0)
    
    # Calculate total beetles across all generations
    total_beetles_all_gens <- sum(gen_beetles)
    
    # Create the updated analysis record with consistent timing (all end of year)
    year_stats <- data.frame(
      year = year,
      # Plant counts at the END of the year
      total_plants = end_year_total_plants,
      hairy_plants = end_year_hairy_plants,
      glabrous_plants = end_year_glabrous_plants,
      prop_hairy = end_year_prop_hairy,
      prop_glabrous = end_year_prop_glabrous,
      
      # Initial plant counts
      initial_total_plants = initial_total_plants,
      initial_hairy_plants = initial_hairy_plants,
      initial_glabrous_plants = initial_glabrous_plants,
      
      total_beetles_all_gens = total_beetles_all_gens,
      
      # Total larvae count across all generations
      total_larvae = total_larvae,
      
      # Flower counts BEFORE beetle damage
      avg_flowers_initial_total = pre_damage_avg_flowers_total,
      avg_flowers_initial_hairy = pre_damage_avg_flowers_hairy,
      avg_flowers_initial_glabrous = pre_damage_avg_flowers_glabrous,
      
      # Flower counts AFTER beetle damage 
      avg_flowers_total = post_damage_avg_flowers_total,
      avg_flowers_hairy = post_damage_avg_flowers_hairy,
      avg_flowers_glabrous = post_damage_avg_flowers_glabrous,
      
      # Damage percentage
      pct_damage_total = 100 * (1 - post_damage_avg_flowers_total / pre_damage_avg_flowers_total),
      pct_damage_hairy = ifelse(
        !is.na(pre_damage_avg_flowers_hairy) & pre_damage_avg_flowers_hairy > 0,
        100 * (1 - post_damage_avg_flowers_hairy / pre_damage_avg_flowers_hairy),NA),
      pct_damage_glabrous = ifelse(
        !is.na(pre_damage_avg_flowers_glabrous) & pre_damage_avg_flowers_glabrous > 0,
        100 * (1 - post_damage_avg_flowers_glabrous / pre_damage_avg_flowers_glabrous),NA),
      
      # Average larvae per plant
      avg_larvae_per_plant = ifelse(initial_total_plants > 0, total_larvae / initial_total_plants, 0)
    )
    
    # Append to the analysis dataframe
    analysis_df <- rbind(analysis_df, year_stats)

        #Check if plants go extinct
    
    if (nrow(plants_df) == 0) {
      break  # Exit the main year loop
    }
  }
  

  return(analysis_df)
}


