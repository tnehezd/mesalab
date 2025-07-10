# Visualizaton Tools

This module contains functions for generating heatmaps, Hertzsprung–Russell (HR) diagrams,
and color–magnitude diagrams (CMDs) from MESA simulation outputs. It is typically used
after the data processing phase to visualize key stages of stellar evolution, especially
around the instability strip and blue loop phases.

Among its core features is a heatmap generator that visualizes the number of times
stellar models cross the instability strip (IS) across a grid of initial masses and metallicities.
Additionally, it can produce summary CSV files showing the durations of important evolutionary phases,
such as the blue loop and IS instability phases, providing insights into the temporal behavior of models.

```{toctree}
visualization
```
