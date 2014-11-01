osdkeys
=====

Show keys pressed with an on-screen display. Very useful for
screencasts. Works with XMonad. Linux only.

![Screenshot](http://i.imgur.com/tsGXach.png)

This program uses the `xinput` program to get a stream of key presses
and uses the `libnotify` library to display them on-screen.

Currently supported display notations are: Emacs

## Install

It can be installed from Hackage:

    $ cabal install osdkeys

## Usage

Usage is simple:

    $ osdkeys
    osdkeys: Arguments: DEVICE-ID [<max-keys-on-screen>]

    Use `xinput list' to get device ID.

Example:

    $ osdkeys 9

Will display keys from the device identified by `9` from `xinput
list`.

## Why not screenkey?

That has unpredictable key grabbing, doesn't support XMonad and is
unmaintained.
