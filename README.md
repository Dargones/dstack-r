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
dstack config add --token <TOKEN> --user <USER>
```
or simply
```bash
dstack config add
```
In this case, the **dstack profile** name will be `default`. You can change it by including 
profile name in your command `dstack config add <PROFILE_NAME>`. This allows you to configure multiple profiles and refer 
to them from your code by their names.

By default, the configuration profile is in your home directory: `$HOME/.dstack/config.yaml`

---

**NOTE**

Before CLI version 0.4.2 config was stored in a working directory. Please, do not forget to move the
local config into your home directory.

---

See [documentation](https://docs.dstack.ai/) for more information about command line tools or type `dstack --help`.

You can also configure **dstack** by using R console:
```r
dstack::configure(user = "<USER>", token = "<TOKEN>", persist = "global")
``` 

## How to install dstack server locally
From CLI version 0.4 it is possible to use a local version of [dstack](https://github.com/dstackai/dstack) 
server.
 
To start it, use the following command:
```bash
dstack server start
```
This command installs the latest version (if it's not installed) of the server and starts it. If environment variable `JAVA_HOME` is set
and version of JDK is compatible with the server, that version will be used. In the case if 
installer can't find `JAVA_HOME` or JDK version is incompatible with current server version
it will download a compatible version by itself. To update server use `dstack server update`. 

Follow instructions provided by the server in the terminal.

Use `dstack server --help` for more information.

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
