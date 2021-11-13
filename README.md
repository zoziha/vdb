# VDB (vs code graphical debugger configurator for fpm)

`vdb` is an auxiliary program of `fortran-lang/fpm` to configurate vs code graphical debugger for fpm.

## Get started

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
fpm build
fpm test
fpm install 
```

### Configurate `luanch.json` in VS Code with `vdb`

Pass the program to be debugged to `vdb` for processing:

```sh
fpm test <test_name> --runner vdb
fpm test <test_name> --runner vdb -- ARGS
vdb clean
vdb clean --all
vdb --list
```

## Links

- [json-fortran](https://github.com/jacobwilliams/json-fortran)
- [test-drive](https://github.com/fortran-lang/test-drive)
- [fpm](https://github.com/fortran-lang/fpm)