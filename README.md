# dstack.ai

## Installation

The **dstack** package and **command line tool** must be installed with either **pip** or **Conda**:

```bash
pip install dstack
```
or
```bash
conda install -c dstack.ai dstack
```
Note, if you use **pip**, it is highly recommended to use **virtualenv** to manage local environment.
Of course you need to install R package too:
```R
install.packages("dstack")
```

## Configuration

Before you can use **dstack package** in your code, you must run the **dstack command line** tool configure a **dstack profile** where you specify your [dstack.ai](https://dstack.ai) username and token.

Configuring **dstack profiles** separately from your code, allows you to make the code safe and not include plain secret tokens.

Configuring a **dstack profile** can be done by the following command:

```bash
dstack config --token <TOKEN> --user <USER>
```
or simply
```bash
dstack config
```
In this case, the **dstack profile** name will be `default`. You can change it by including `--profile <PROFILE NAME>` in your command. This allows you to configure multiple profiles and refer to them from your code by their names.

By default, the configuration profile is stored locally, i.e. in your working directory: `<WORKING_DIRECTORY>/.dstack/config.yaml`

See [CLI Reference](https://docs.dstack.ai/cli-reference) to more information about command line tools or type `dstack config --help`.

## Publishing simple plots

Once the **dstack profile** is configured, you can publish plots from your R code. Let's consider how to publish simple [ggplot2](https://ggplot2.tidyverse.org/) plot: 


```r
library(ggplot2)
library(dstack)

df <- data.frame(x = c(1, 2, 3, 4), y = c(1, 4, 9, 16))
image <- ggplot(data = df, aes(x = x, y = y)) + geom_line()

push_frame("simple", image, "My first plot")
```

## Publishing interactive plots

In some cases, you want to have plots that are interactive and that can change when the user change its parameters. Suppose you want to publish a line plot that depends on the value of the parameter `Coefficient` (slope).
```r
library(ggplot2)
library(dstack)

line_plot <- function(a) { 
    x <- c(0:20)
    y <- sapply(x, function(x) { return(a * x) })
    df <- data.frame(x = x, y = y)
    plot <- ggplot(data = df, aes(x = x, y = y)) + 
        geom_line() + xlim(0, 20) + ylim(0, 20)
    return(plot)
}

coeff <- c(0.5, 1.0, 1.5, 2.0)
frame <- create_frame(stack = "line_plot")
for(c in coeff) {  
    frame <- commit(frame, line_plot(c), 
        paste0("Line plot with the coefficient of ", c), list(Coefficient = a))
}

push(frame)
```

## Documentation

For more details on the API and code samples, check out the [docs](https://docs.dstack.ai).
