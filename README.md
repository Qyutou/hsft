
# HSFT - Haskell system fetch tool.

Simple and fast system fetch tool written in haskell. Also it can show
output of the shell commands, it can be easily configured to show
anything.

![HSFT example](examples/hsft-example-1.png) ![HSFT
example](examples/hsft-example-2.png)

![HSFT example](examples/hsft-example-3.png)

## Installation

It can be easily build manualy using cabal.

``` bash
$ git clone https://github.com/Qyutou/hsft
$ cd hsft
$ cabal install
```

To run the application, directory `.cabal/bin` must be in the $PATH, or
application can be installed to specified directory:

``` bash
$ cabal install --installdir=$HOME/bin
```

## Uninstall

To uninstall simply delete created binary file (by default
`~/.cabal/bin/hsft`)

``` bash
$ rm ~/.cabal/bin/hsft
```

## Configuration

The configuration file is `app/Config.hs`.

After changing configuration file you need to recompile the application:

``` bash
$ cabal install --overwrite-policy=always
```

The most important variable there is a `config`, which defines what
information to show and in which order. The values in this variable must
be separated with spaces. All possible values are stored in the
`fetchField` variable, where you can see the value and the command which
it will execute. Value `"line"` adds a separator horizontal line.

``` haskell
config :: Data.Text.Text
config = "os kernel wm line terminal shell uptime"
```

In the `app/Config.hs` can be found an example of adding a field, which
based on the output from shell script.

Another variable important for customization is `colors`. It must have
all fields with colors for every element: borderColor, titleColor,
separatorColor and infoColor. To use default color (foreground color),
the color field should be empty. All other colors can be specified like
`"red"`, `"green"`, `"brigthBlue"`, etc.

``` haskell
colors :: DataTypes.Colors
colors = DataTypes.Colors
  { borderColor = "blue"
  , titleColor = "yellow"
  , separatorColor = ""
  , infoColor = "white" }
```
