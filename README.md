# hu.dwim.bluez

## What

A Common Lisp [FFI](https://en.wikipedia.org/wiki/Foreign_function_interface)
for [Bluez](http://www.bluez.org/) (aka libbluetooth, a Bluetooth stack for Linux).

## Who

Written by [attila@lendvai.name](mailto:attila@lendvai.name).

## Where

The primary communication channel is the facilities on
[the project's GitHub page](https://github.com/hu-dwim/hu.dwim.bluez).

## How

The project uses [CFFI/C2FFI](https://github.com/cffi/cffi) to
automatically generate the CFFI definitions from the C header files.

An example of how to use it can be found
[in this abandoned project](https://github.com/attila-lendvai/bluetooth-mqtt-gateway).

## Status

The entire C side of the Bluez API is available through CFFI.

It works well enough to play around with Bluez even from the Lisp REPL.

The lispy helpers in `fancy.lisp` may need some more love, but it should be a mundane task. Patches are welcome!

For now it's little-endian only, but it's trivial to fix.
