# nes
[![Build Status](https://travis-ci.org/lukas9393/nes.svg?branch=master)](https://travis-ci.org/lukas9393/nes)

This is an NES emulator written in rust.
```sh
$ nes PATH
```
Donkey Kong (World) (Rev A).nes           |  Super Mario Bros. (World).nes
:-------------------------:|:-------------------------:
![Donkey Kong (World) (Rev A).nes](https://github.com/lukas9393/nes/blob/master/assets/donkey_kong.gif?raw=true) | ![Super Mario Bros. (World).nes](https://github.com/lukas9393/nes/blob/master/assets/mario.gif?raw=true)

## Controls

```sh
 Button  |   Key
___________________
|   A    |    D   |
|   B    |    F   |
| Start  |  Enter |
| Select | R-Shift|
|   Up   |   Up   |
|  Down  |  Down  |
|  Left  |  Left  |
|  Right |  Right |
-------------------

ESC: close window
```

## TODOs

* Audio Controller
* Save function
* Mappers
    * UxROM
    * BxROM
    * MMC1
    * MMC3
    * ...
* Rewind function
* Second Controller
* Scalable
