# hu.dwim.bluez
A Common Lisp [FFI](https://en.wikipedia.org/wiki/Foreign_function_interface)
for [Bluez](http://www.bluez.org/) (aka libbluetooth), which is a Linux Bluetooth
stack.

## Status

It works well enough to play around with Bluez, even interactively from the REPL, and it covers the entire C side of the Bluez API (yay for [CFFI/C2FFI](https://github.com/cffi/cffi/tree/master/src/c2ffi)!).

The lispy helpers in `fancy.lisp` may need some further extensions, but it should be mundane, not involving much extra complexity compared to what is already there.

### Limitations

For now it's little-endian only, but it's trivial to fix.

## Bug reports, contributions
Written by [attila@lendvai.name](mailto:attila@lendvai.name). The primary
communication channel is the facilities on
[the project's GitHub page](https://github.com/attila-lendvai/hu.dwim.bluez).
