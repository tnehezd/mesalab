# mesalab/plotting/plot_config.py

DEFAULT_PLOT_CONFIG = {
    "figure": {
        "figsize": (8, 5),
        "dpi": 200,
        "facecolor": "white",
        "edgecolor": "white",
    },
    "axes": {
        "title_size": 14,
        "label_size": 12,
        "grid": True,
        "grid_style": "--",
        "grid_alpha": 0.6,
    },
    "scatter": {
        "s": 10,
        "alpha": 1.0,
        "cmap": "viridis",
    },
    "colorbar": {
        "size": "3%",
        "pad": 0.05,
        "label_size": 11,
    }
}