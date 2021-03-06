
# HSFT - Haskell system fetch tool.

Simple and fast system fetch tool written in haskell. Also it can show
output of the shell commands, it can be easily configured to show
anything.

![HSFT example](examples/img-1.png) ![HSFT example](examples/img-2.png)

![HSFT example](examples/img-3.png) ![HSFT example](examples/img-4.png)
![HSFT example](examples/img-5.png)

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

## Configuration

The configuration file is `app/Config.hs`. Explanations of every field
can be found in that file.

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

## TODO

-   [ ] Create better documentation.
-   [ ] Add stack support.
-   [ ] Add more fetch functions.
-   [ ] Improve speed of fetching disk free space.
