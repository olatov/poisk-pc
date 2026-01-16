# Poisk Emulator

An emulator for the [Poisk](https://en.wikipedia.org/wiki/Poisk_%28computer%29) — an Intel 8088-based IBM PC compatible computer designed and manufactured by PO Electronmash (Kyiv, Ukraine) in the late 1980s to early 1990s.

![Version](https://img.shields.io/badge/version-0.9-blue)
![License](https://img.shields.io/badge/license-MIT-green)

## Screenshots

<img width="640" height="400" alt="1" src="https://github.com/user-attachments/assets/fbf3117a-852f-4302-b7b4-0d28ed7e9675" />
<img width="640" height="400" alt="2" src="https://github.com/user-attachments/assets/b66d8181-eeb6-457a-b1a1-a4345f78a732" />
<img width="640" height="400" alt="3" src="https://github.com/user-attachments/assets/f3381f94-f572-4146-8765-b3cab900b382" />
<img width="640" height="400" alt="4" src="https://github.com/user-attachments/assets/24dac001-80a0-4064-b407-73251044c49f" />
<img width="640" height="400" alt="5" src="https://github.com/user-attachments/assets/874816d5-d114-49fe-9947-b14cf666bce7" />
<img width="640" height="400" alt="6" src="https://github.com/user-attachments/assets/d52cd525-20c4-4999-a60f-6fc8f5b098f5" />

## Features

- **Accurate CPU emulation** — Built on [pas8088](https://github.com/olatov/pas8088), an Intel 8088 emulator written in Free Pascal
- **Graphics & Audio** — Powered by [ray4laz](https://github.com/GuvaCode/ray4laz) (RayLib for Lazarus/Free Pascal)
- **Hardware support**:
  - CGA-compatible video controller (with limitations determined by Poisk's hardware)
  - Timer (PIT 8253)
  - Keyboard controller
  - Floppy disk controller (B504)
  - XT-IDE hard disk controller
  - Cassette tape interface
  - PC Speaker audio
- **Display effects** — Scanlines, grayscale mode, 4:3 aspect ratio correction
- **Cross-platform** — Linux, Windows, and macOS support

## Quick Start

### Download Pre-built Binaries

Download the latest release from the releases page and extract the archive.

### Running the Emulator

Run the binary for your operating system with a BIOS ROM file:

```bash
# Linux
./poisk-linux-x86_64 --bios-rom=rom/demo.rom

# macOS
./poisk-darwin-universal --bios-rom=rom/demo.rom

# Windows
poisk-windows-x86_64.exe --bios-rom=rom/demo.rom
```

If a window appears with "POISK Demo ROM" and color bars, the emulator is working correctly.

For a full system, you'll need a complete BIOS ROM (not included):

```bash
./poisk-linux-x86_64 --bios-rom=rom/bios-1991.rom
```

### macOS Security Note

On macOS, you may need to remove the quarantine attribute:

```bash
xattr -c poisk-darwin-universal
```

## Command-Line Options

```
--bios-rom=<file>      Path to BIOS ROM file (required)
--fdc-rom=<file>       Path to FDC (Floppy Disk Controller) ROM
--xtide-rom=<file>     Path to XT-IDE ROM
--cartridge=<file>     Load cartridge ROM at C000:0000
--floppy-a=<file>      Mount floppy disk image in drive A:
--floppy-b=<file>      Mount floppy disk image in drive B:
--hdd=<file>           Mount hard disk image
--cassette=<file>      Load cassette tape image
--ram=<size>           RAM size in KB (default: 128)
--turbo                Enable turbo mode at startup
--fullscreen           Start in fullscreen mode
```

## Keyboard Shortcuts

All shortcuts require holding **F11** key:

### System Control
- **F11 + Esc** — Exit emulator
- **F11 + F1** — Reset computer
- **F11 + P** — Pause/unpause emulation
- **F11 + T** — Toggle turbo mode

### Cassette Tape
- **F11 + F5** — Play/stop tape
- **F11 + F6** — Rewind tape (10 seconds back)
- **F11 + F7** — Fast-forward tape (10 seconds forward)
- **F11 + F8** — Show current tape position

### Display
- **F11 + F** — Toggle fullscreen
- **F11 + A** — Toggle 4:3 aspect ratio correction
- **F11 + G** — Toggle grayscale mode
- **F11 + S** — Toggle scanlines effect
- **F11 + Y** — Toggle texture filtering (bilinear/point)

### Audio
- **F11 + M** — Mute/unmute sound
- **F11 + 9** — Decrease volume
- **F11 + 0** — Increase volume

## Building from Source

### Prerequisites

- [Free Pascal Compiler](https://www.freepascal.org/) (FPC) 3.2.0 or later
- [Lazarus IDE](https://www.lazarus-ide.org/) (optional, for GUI development)
- Git

### Dependencies

The project depends on:
- [pas8088](https://github.com/olatov/pas8088) — Intel 8088 CPU emulator
- [ray4laz](https://github.com/GuvaCode/ray4laz) — RayLib bindings for Free Pascal

### Build Steps

#### Using Docker (Linux hosts only)

```bash
./build.sh
```

The compiled binary will be in the `out/` directory.

**Note:** The Docker build script is currently configured for Linux x86_64 targets only.

#### Manual Build (Linux)

1. Clone the repository:
```bash
git clone https://github.com/yourusername/poisk-pc.git
cd poisk-pc
```

2. Clone dependencies:
```bash
cd deps
git clone https://github.com/olatov/pas8088.git
git clone https://github.com/GuvaCode/ray4laz.git
cd ray4laz
git checkout 5.6.2
cd ../..
```

3. Compile for Linux x86_64:
```bash
cd src
fpc -O2 -CX -XX -Xs poisk.lpr \
    -Fudeps/pas8088/src/pas8088 \
    -Fudeps/ray4laz/source \
    -Fldeps/ray4laz/libs/x86_64-linux
```

**Note:** For Windows or macOS builds, adjust the library path (`-Fl` option) to point to the appropriate platform directory in `deps/ray4laz/libs/`.

#### Using Lazarus IDE

1. Open `src/poisk.lpi` in Lazarus IDE
2. Set project options to include the dependency paths
3. Run → Build (Shift+F9)

## Project Structure

```
├── src/              # Source code
│   ├── poisk.lpr     # Main program
│   ├── memory.pas    # Memory management
│   ├── videocontroller.pas
│   ├── keyboard.pas
│   ├── timer.pas
│   └── ...
├── deps/             # Dependencies (pas8088, ray4laz)
├── release/          # Pre-built binaries
└── build.sh          # Docker build script
```

## Technical Details

### System Specifications

- **CPU**: Intel 8088 (turbo mode available)
- **RAM**: Configurable (default 128 KB)
- **Video**: CGA-compatible (640×200 mono, 320×200 4-color)
- **Storage**: Floppy disks (360KB ; 720KB), XT-IDE hard disks
- **Audio**: PC Speaker with PWM

## Tips

- If you hear a click from the speaker when pressing keys, press **F4** to toggle the BIOS keyboard click feature
- For extra performance, enable turbo mode with **F11 + T**
- Use scanlines and 4:3 aspect ratio for an authentic retro experience

## Contributing

Contributions are welcome! Please feel free to submit pull requests or open issues for bugs and feature requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- [pas8088](https://github.com/olatov/pas8088) — Intel 8088 CPU emulator by Oleg Latov
- [ray4laz](https://github.com/GuvaCode/ray4laz) — RayLib bindings for Free Pascal by GuvaCode
- [RayLib](https://www.raylib.com/) — Graphics library by raysan5

## Links

- [Poisk Computer (Wikipedia)](https://en.wikipedia.org/wiki/Poisk_%28computer%29)
- [Free Pascal](https://www.freepascal.org/)
- [Lazarus IDE](https://www.lazarus-ide.org/)

---

Made with ❤️ using Free Pascal and RayLib
