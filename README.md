# About `mesalab` pipeline

The Python package `mesalab` is designed for **processing and analyzing stellar evolution simulations performed with**  [MESA](https://docs.mesastar.org/en/latest/) (Modules for Experiments in Stellar Astrophysics). It is developed to efficiently handle **large grids of simulations**, such as those where stellar mass (M) and metallicity (Z) are systematically varied.

The primary goal of this pipeline is to take your MESA outputs and automatically:
    1.  Analyze each simulation within your grid.
    2.  Identify if the star enters the *blue loop* phase and crosses the instability strip.
    3.  Based on these findings, prepare [GYRE](https://gyre.readthedocs.io/) input files and run the corresponding pulsation simulations.

----

## Installation

### Install from pip

To install `mesalab` with pip:

```console
$ pip install seismolab
```
----

### Building from source

You can also install the current development version of `mesalab` with cloning the GitHub repository and install the code with pip:

```console
$ git clone https://github.com/konkolyseismolab/mesalab
$ cd mesalab
$ pip install .
```

---- 

## Usage

To get started, you'll need to prepare a configuration file (e.g., `config.yaml`) that specifies your MESA input directories, output locations, and analysis preferences.

You can run `mesalab` by providing your configuration file:

```console
$ mesalab --config myconfig.yaml
```

For more detailed information on configuration options, command-line arguments, and advanced usage, please consult the official `mesalab` documentation on [Read the Docs](https://mesalab.readthedocs.io/en/latest/index.html).


----

## Contributing

If you're interested in improving `mesalab`, feel free to **fork the repository**, make your changes, and **submit a pull request**. You can also open an [issue on GitHub](https://github.com/konkolyseismolab/mesalab/issues) if you encounter bugs or have feature suggestions.


----

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.