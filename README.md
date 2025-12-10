# IBM-Arabidopsis-halleri-and-Phaedon-brassicae
This project presents a spatially explicit individual-based model (IBM) designed to investigate how variation in herbivore foraging behaviour influences the maintenance of trichome polymorphism in *Arabidopsis halleri subsp. gemmifera*.

Originally developed as part of a research module, the model has since been refined and expanded into a full research report. The core simulation framework has been restructured into modular functions, enabling flexible application across a range of ecological scenarios. This functional design allows the model to be executed efficiently on high-performance computing (HPC) systems, supporting large-scale experimentation with up to 200 replicates per scenario.

By integrating herbivore behavioural variation with plant trait polymorphism, the model provides a tool to explore the ecological and evolutionary dynamics that sustain genetic diversity in natural populations. The results contribute to a deeper understanding of plant-herbivore interactions and the mechanisms underlying trait persistence in changing environments.


## Biological background

* *A. halleri* occurs as two morphs:

  * **Hairy** plants with trichomes (defended but costly: fewer flowers).
  * **Glabrous** plants without trichomes (undefended but higher flower production).
* The specialist leaf beetle **Phaedon brassicae** (here simplified as "beetles") prefers to feed and oviposit on glabrous plants, especially when they are common.
* This creates **negative frequency-dependent selection**:
  common morphs suffer more herbivory, rare morphs are temporarily favoured.

The model asks: **How does variation in individual beetle preferences (ti) change the balance between hairy and glabrous plants?**

---

## Model overview
The main simulation is implemented in `main_code_ITV.R` as the function:

```r
results <- run_simulation(...)
```
### Entities

* **Grid cells**

  * 10 x 10 toroidal grid (`grid_size = 10`).
  * Each cell can hold up to `Kc = 10` plants.
* **Plants**

  * Genotype: `"HH"`, `"Hh"` (hairy) or `"hh"` (glabrous).
  * Age: 0-2 years (`plant_lifespan = 2`).
  * State: number of flowers, larvae on the plant, grid cell location.
* **Beetles**

  * Movement: limited number of relocations (`r_values`, `r_probs`).
  * Boldness: probability to move to a neighbouring cell (`boldness`).
  * Egg capacity: max eggs per female (`max_eggs`, reduced on hairy hosts).
  * Individual **preference threshold `ti`**: how choosy the beetle is about feeding only on glabrous plants.
  * Location: grid cell and (if found) host plant.

### Spatial & temporal scale

* Space: 100 cells (1 m^2 patches conceptually), torus (wrapping edges).
* Time: 1 step = **1 year**, with **2 beetle generations per year** (`beetle_gen = 2`).
* Default run length: `years = 50`.

---

## Annual cycle (one simulation year)

For each year, the simulation runs the following steps:

1. **Plant flower production**

   * Glabrous plants produce `fmax` flowers.
   * Hairy plants pay a cost `c` and produce `f_hairy = fmax x (1 - c)` flowers.

2. **Beetle generations (repeated `beetle_gen` times)**

   * Apply background beetle mortality (`beetle_mortality`).
   * **Movement & host search**

     * Beetles move between neighbouring cells with probability `boldness`.
     * Within a cell they try to land on plants that have < `bmax` beetles.
   * **Choosiness rule (preference threshold `ti`)**

     * Compute current proportion of glabrous plants (S_g) at the *start* of the year.
     * If (S_g $\geq$ ti): beetle prefers **glabrous only**, rejecting hairy plants while it still has relocations left.
     * If (S_g < ti): beetle accepts **any** plant with free beetle slots.
     * On the final relocation, a "desperation rule" forces the beetle to accept any available plant.
   * **Oviposition**

     * Beetle lays a stochastic number of eggs (around its egg capacity).
     * Eggs suffer high mortality (Poisson thinning via `egg_mortality_factor`).
   * **Density-dependent larval survival & feeding**

     * Each plant has a larval "carrying capacity" = flowers / `d`.
     * Larval survival probability declines with larval crowding.
     * Surviving larvae reduce the plant's flowers by `d` per larva.
   * **New beetles**

     * Surviving larvae become next-generation adults.
     * Their egg capacity depends on whether they developed on a glabrous or hairy plant.
     * If total beetles exceed `bmax x number_of_plants`, the population is capped.

3. **Plant reproduction & seed dispersal**

   * Only plants with >0 flowers can reproduce.
   * Each flower is a potential seed; survival to establishment is density-dependent based on remaining free plant slots on the grid.
   * Each successful seed:

     * Picks a random mate among other flowering plants.
     * Inherits alleles via simple Mendelian rules (hairy dominant); each gamete can mutate with probability `mutation_rate`.
   * **Dispersal**

     * With probability 0.6, the seed tries to establish in the parent's cell (if space).
     * Otherwise, or if the parent cell is full, it disperses to neighbour cells with available space.
     * Seeds failing to find a free cell are lost.

4. **Plant mortality**

   * Age increases by one year.
   * Plants older than `plant_lifespan` die.
   * Surviving plants experience additional random mortality (`plant_mortality`).

5. **Data collection**

   * At the **end of each year**, the model records:

     * Total plants, number and proportion of hairy vs. glabrous plants.
     * Total beetles across both generations.
     * Total larvae.
     * Mean flowers (before and after damage) and percentage damage.
     * Average larvae per plant.

---

## Key parameters (selected)

You can change these via the arguments to `run_simulation()`:

* `grid_size`: side length of the square grid (default 10).
* `Np`, `Nb`: initial numbers of plants and beetles.
* `Sg`: initial share of glabrous plants.
* `Kc`: plant carrying capacity per cell.
* `fmax`, `c`: max flowers per plant and cost of being hairy.
* `bmax`: max beetles per plant.
* `d`: flower damage per larva.
* `plant_lifespan`, `plant_mortality`, `beetle_mortality`.
* `max_eggs`, `egg_mortality_factor`.
* `t`, `t_sd`: mean and standard deviation of the beetle preference threshold distribution.
* `beetle_gen`, `years`.
* `boldness`: probability of moving to a neighbouring cell.

The function `generate_ti()` inside `main_code_ITV.R` controls how **individual thresholds `ti` are drawn**.
You can switch between a normal distribution (current default) and a bimodal distribution by commenting/uncommenting the relevant lines.

---

## Output

`run_simulation()` returns a data frame (`analysis_df`) with one row per year.
Key columns include:

* `year`
* `total_plants`, `hairy_plants`, `glabrous_plants`
* `prop_hairy`, `prop_glabrous`
* `total_beetles_all_gens`
* `total_larvae`
* `avg_flowers_initial_total`, `avg_flowers_initial_hairy`, `avg_flowers_initial_glabrous`
* `avg_flowers_total`, `avg_flowers_hairy`, `avg_flowers_glabrous`
* `pct_damage_total`, `pct_damage_hairy`, `pct_damage_glabrous`
* `avg_larvae_per_plant`

These can be used directly for plotting time series of plant morph frequencies, herbivore abundance, and damage.

---

## How to run

From R:

```r
# In the repo root
source("main_code_ITV.R")

# Run a single simulation with default parameters
results <- run_simulation()

```

## Running multiple scenarios

The version of the model included in this repository is intended for single-replicate demonstrations.
In the full research workflow, we extend this model to run many parameter scenarios, each with multiple replicates, to explore how ecological conditions affect long-term morph dynamics.

### How scenario experiments are handled

1. Scenario generation
A separate script (not included here) creates grids of parameter combinations (e.g., different beetle preference distributions, defence costs, initial morph frequencies).
Each combination defines one scenario.

2. Replicate execution
A small R wrapper (not included) runs run_simulation() for one scenario-replicate pair and saves the resulting analysis_df to a results folder.
Each replicate is independent, making the workflow highly parallelisable.

2. HPC batch submission
Hundreds of replicates are executed on an HPC cluster using simple SLURM/bash scripts that:

- load R on the compute node

- run the wrapper script

- save outputs for later aggregation

These scripts are omitted because they are tied to unpublished work and contain machine-specific paths and configuration.

4. Post-processing
After all runs finish, analysis scripts combine replicate outputs and produce summaries and plots for the manuscript.
These result-specific tools are also not included.

### Why these files are not provided

The scenario-generation scripts, HPC job files, and analysis pipeline contain unpublished experiment designs and results and are therefore not shared here.
The simplified public repository focuses on:

- the core model ("main_code_ITV.R")

- an example run

- basic plotting

This is sufficient to demonstrate the modelling approach and code structure without compromising ongoing publication work.

## License
This project is released under the MIT License. See the LICENSE file for details.
