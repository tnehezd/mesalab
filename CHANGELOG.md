## [v2.1.0] – 2025-10-22

### Changed
- **Instability Strip filtering logic modularized**  
  The IS membership check is now fully decoupled from the analysis pipeline, enabling precise mocking and targeted validation. This improves testability and semantic clarity across blue loop phases.

- **Crossing count computation clarified**  
  `crossing_count` now explicitly reflects the number of distinct IS entry events, based on `False → True` transitions. This replaces implicit assumptions tied to contiguous IS segments.

### Added
- **New summary metrics**  
  The output dictionary now includes `min_log_L`, `min_log_Teff`,and `min_log_R,` values extracted from the blue loop phase, enabling more complete physical characterization of the loop.

- **Unit tests for duration metrics**  
  Introduced dedicated tests for `safe_duration()` and `compute_true_instability_duration()` covering edge cases, open-ended segments, and multiple IS crossings. These ensure numerical robustness and reproducibility.

- **Mockable IS logic for test isolation**  
  The `is_in_instability_strip()` function is now patchable in all analysis tests, allowing controlled injection of IS membership and precise verification of downstream metrics.

### Fixed
- Corrected `blue_loop_detail_df` filtering to ensure only valid IS points are retained, even under mock conditions.
- Resolved test failures caused by mismatched mock argument signatures and ambiguous IS segment expectations.
- Prevented false positives in crossing count assertions by enforcing entry–exit separation in mock logic.



## [v2.0.0] – 2025-10-21

### Changed
- **Detail file naming convention updated**  
  Removed Y-tag from output filenames as it had some hidden bugs 🐞. File discovery logic adjusted to match the new naming scheme.

- **Instability duration logic revised**  
  `calculated_instability_duration` now reflects the actual cumulative time spent inside the Instability Strip, based on entry–exit intervals. This replaces the previous first-to-last crossing approximation, that did not reflect the real duration of the instabilit phase.

### Added
- **Robust duration computation**  
  Introduced `safe_duration()` to ensure all age-based calculations are numerically valid and time-ordered, preventing NaN propagation and negative durations.

### Fixed
- Prevented instability duration miscalculations caused by missing or unordered IS entry/exit timestamps.
- Ensured fallback consistency when blue loop detail data is unavailable or incomplete when `force_reanalysis = False`.
