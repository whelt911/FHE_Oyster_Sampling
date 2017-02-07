# Fish Habitat Enhancement Project -- Oyster Sampling
Oysters are sampled annually by quadrat sampling.

Currently the dataset contains 2 monitoring events:
<li>
<ol>Spring 2016 - to quantify overwinter mortality</ol>
<ol>Fall 2016 - annual monitoring</ol>
</li>

## Purpose

Determine that oysters in the seeded treatment across 4 replicates are not distinctively different from
each other, which would possibly result in unequal treatment effects.

### oyster.data.import.and.cleanup.R
Imports and cleans the necessary data as .txt files and saves them as R objects for use by other scripts
<ul>oyster.data</ul>
<ul>quadrat.data</ul>

### fall_2016_oyster.density.R
Determines the density of oysters across 4 replicates

### oyster.size.distribution.R
Determines average length of oysters across 4 replicates

### percent.survival.R
Determines what percentage of oysters have survived since seeding.
This method is not entirely robust, since it is derived from the proportion of live oysters to recently dead 
(oysters that have died but still maintain intact boxes), but it has been used in previous studies to 
deduce survival.

### reef.height.R
Determines the average height and variance of the reef based on quadrat sampling.

<br>
<br>
## In Progress

The exploratory analysis has been completed.  Prior to publishing, robust statistical analysis must be
performed.
