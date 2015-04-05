![Screenshot](http://i.imgur.com/l804Oah.png)

My panel is written in Haskell using Webkit and GTK+ for the window and the display, with XMonad handling the layout properly.

The panel displays things of interest to myself:

* Disk usage.
* Battery life.
* CPU temperature.
* Load.
* Volume.
* Real (non-cache) memory usage.
* My currently clocked in hours of work (of total 8:00).
* How many keys I've pressed today.
* The current time in my time zone (CET/CEST) and the time in PST and EST time zones.

## Setup

Much like other people's Emacs configurations, my XMonad configuration is rather hard to setup. It uses GTK+ and Webkit which are both hard to install and finicky on some systems. It also uses a custom i3status config (included), and my own script for getting memory usage (`mem-use.sh`) and it also reads from `~/Log/dita.log` (which uses [Dita](https://github.com/chrisdone/dita)) for typing information.

I provide no help or support in getting it to run, but there's no reason for me to keep the repo hidden either.
