# async-process

A Common Lisp library for creating and managing asynchronous processes with PTY support.

## Platform Support

- **Linux**: Full support via C library using PTY
- **BSD**: Full support via C library using PTY
- **macOS**: Full support via C library using PTY
- **Windows**: Full support via pure Lisp CFFI implementation (no C compilation required)

## Installation

### Unix-like Systems (Linux, FreeBSD, macOS)
using GNU make, `gmake` on Freebsd and macOS.

```bash
git clone https://github.com/soppelmann/async-process.git
cd async-process
make
sudo make install
```

The library installs to `/usr/local` by default. To install elsewhere:

```bash
make install PREFIX=/usr
make install PREFIX=$HOME/.local
```

#### Configuration options
Build as a static library as follows

```bash
make
./configure --enable-static
make all
sudo make install
```

### Windows

On Windows, no C compilation is required. The library uses a pure Lisp implementation via CFFI:

```bash
git clone https://github.com/soppelmann/async-process.git
cd async-process
```

Then simply load the library in your Lisp environment:

```lisp
(ql:quickload :async-process)
```

The ASDF system will automatically load the Windows-specific implementation (`src/async-process_windows.lisp`) when on Windows platforms.

## Usage

```
CL-USER> (ql:quickload :async-process)
To load "async-process":
  Load 1 ASDF system:
    async-process
; Loading "async-process"
..................................................
[package async-process].
(:ASYNC-PROCESS)
CL-USER> (in-package async-process)
#<PACKAGE "ASYNC-PROCESS">
ASYNC-PROCESS> (create-process "python")
#.(SB-SYS:INT-SAP #X7FFFEC002830)
ASYNC-PROCESS> (defparameter p *)
#.(SB-SYS:INT-SAP #X7FFFEC002830)
ASYNC-PROCESS> (process-receive-output p)
"Python 2.7.13 (default, Nov 24 2017, 17:33:09) 
[GCC 6.3.0 20170516] on linux2
Type \"help\", \"copyright\", \"credits\" or \"license\" for more information.
>>> "
ASYNC-PROCESS> (process-send-input p "1+1
")
; No value
ASYNC-PROCESS> (process-receive-output p)
"1+1
2
>>> "
```

## LICENSE
MIT
