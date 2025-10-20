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
