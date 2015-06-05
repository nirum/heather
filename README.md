# Heather

:partly_sunny: A haskell command-line app for fetching the weather forecast!

## Installation
Must have `ghc` installed. Running the installation script `./install.sh` (on Unix systems) will place the compiled executable in `/usr/local/bin`

## Usage
To get the current weather (for zipcode 94305)

```bash
$ heather current
> 58.94
```

Or specify a custom zipcode:
```bash
$ heather current 27708
> 64.94
```

Or get the entire temperature forecast for the next three days:
```bash
$ heather forecast
> 58.95 56.95 53.11 51.87 51.75 56.33 61.42 64.25 63.44 59.7 55.14 53.07 52.26 55.86 60.44 63.06 62.45 59.22 56.03 56.51 56.27 57.17 61.03 63.71 63.69 59.86 55.96 55.41 54.73 55.13 62.33 66.28 65.31 60.68 55.29 52.67 51.9 54.26 61.14 64.05 
```

We can pass this stream to other apps, like [Spark](https://github.com/holman/spark):
```bash
$ heather forecast | spark
```
results in:
![heather + sparklines](https://cloud.githubusercontent.com/assets/904854/7998174/940792b6-0aed-11e5-82f9-5098cc7f25fa.png)

See `heather -h` for more info.

## Contact
Niru Maheswaranathan (`nirum@stanford.edu`)
