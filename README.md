# VDB (vs code graphical debugger configurator for fpm)

`vdb` is an auxiliary program of `fortran-lang/fpm` to configurate vs code graphical debugger for fpm.

## Getting started

### Dependencies

- Git
- VS Code
- gdb
- [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
- [GDB Debugger - Beyond](https://marketplace.visualstudio.com/items?itemName=coolchyni.beyond-debug)

### Build with fortran-lang/fpm

Fortran Package Manager (fpm) is a package manager and build system for Fortran.<br>
You can build `vdb` using provided `fpm.toml`:

```sh
fpm install --profile release
```

### Configurate `luanch.json` in VS Code with `vdb`

Pass the program to be debugged to `vdb` for processing:

```sh
fpm [options] <test_name> --runner vdb
fpm [options] <test_name> --runner vdb -- ARGS
vdb clean  # Remove and backup JSON::launch.json
```

![start.gif](https://i.loli.net/2021/11/14/8mSWJc5iBC7KA3I.gif)

## Links

- [json-fortran](https://github.com/jacobwilliams/json-fortran)
- [test-drive](https://github.com/fortran-lang/test-drive)
- [stdlib](https://github.com/fortran-lang/stdlib)
- [fpm](https://github.com/fortran-lang/fpm)
