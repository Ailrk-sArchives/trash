# Nix

## Pacakge management
Handling dependencies is a bigger deal than it seems, softwares depend each other, solving dependencies is part of programming itself.

Dependency is a generic problem, it appears everywhere. A python package may depends on some other python packages, pip provides service to solve the dependeny for you. a window manager may depends on some text based confg files, you maintain it by putting the config file in a specific location in the file system... If your window manager depends on the python pacakge, what should you do? You may reolve the path of the script in some fixed location, or you may just put the code in the same directory... There is no standard way to manage this situaion.

We can easily manage module dependencies within the pacakge we write, but it becomes much harder to coordinate one package with others. Each package depends on different packages, and with the same dependency, the version can be different. Not to mention many libraries interface with code compiled in other languages, so you need to call some shell to create a binary first then link things together. If you links to global shared libary the version and path can also be annoying: you installed libgmp.so.3, it works nicely for everything except one specific tool you need. There is only one link name for libgmp, what do you do?

What if we have a universal language to describe dependencies in general? A langauge wrap on top of everything and form a giant DAG?

## Idea

In a package manager like pacman, each package comes with a build script written by nice ppl. Installing is based on running shell scripts, and the installed binary is actually a side effect of running the script.

In contrast, Nix is purely functional package manager. you write derivation instead, which is a declarative descrption of the dependency themselves. The final derivation is the output of a giant pure functions. Pure functions are referential transparent, so same dependency will eventually result in the same build.

## Where are things stored

Each package is uniquely identified. Two versions of the same package are considered two completely different packages. Each package is given a unique hash value as it's identity.

In traditional linux distro libraries are usually stored at /lib, /usr/local/lib, or /usr/lib, binaries are stored in /bin or /usr/bin, everything goes to different location. A build script needs to take care of where certain thing goes. In nix, everything are installed in /nix/store, and each derivation has unique name tagged with its hash value. All usage of a package are refered by softlink. Doing so you get single source of truth.

## What can you do with it that other package manager can't ?

- You can easily switch different version of a package. If your nix use llvm8 by default but you want llvm9, you only need to override the version in the nix-expression. Both llvm8 and llvm9 will be in /nix/store, and they are treated as completely different entities.

- You can easily create virtual environmnet without duplicate packages. It's nice to have a separate sandbox environment for a specific project. For each project you can write a project specific derivation, and nix can create an appropriate environment by setting enviroment variables link to the right packages in /nix/store. Because all packages are in the same store, if you have two projects both need, say llvm9, the dependency will not be duplicated. This is not the case in python virtualenv or npm, you need to reinstall everything for every project.

- You can deploy pacakge easier. The final build (the derivation at least) is the output of a pure function, so no matter where you run the nix expression you always get the same derivation. If the source is stable you should always get the same build.

## Drawback

- It's more complicate to use. Too many moving parts.

- It stores multiple verion of a package, and old packages will accumulate and waste lots of space.


## Concepts

- Derivation is a dictionary with descrptions of the build. Nix expression is the langauge helps you to build derivations.

- There are many tools to gerante nix expression, if your requirement is common there won't be much code to write. For example, if you are using cabal for haskell and wants to manage dependencies by nix, you can use the tool cabal2nix to generate the nix expression for you.

- Your nix expression will based on some other's nix expressions upstream. Someone maintains derivation for each package supported in nix server, and nix expr you need to write is just some modification and configuarion at the end of the pipeline.


## Tools to use

Three commons are enough.

- nix-channel: Nix expression are pure functions in the sense the derivation the output are pure. This doesn't imply the final build is always the same. To ensure that, we need to make sure the same descrption always refer to the sames package themselves. Nix maintains different channels, each channel has a pool of pacakges locked in a particular version. If you are using channel versoin 20.03, you llvm will always be llvm8.  This guarantee the same derivation goes to the same package.

- nix-shell: Create virtualenv like development environment. Same as the build, the enviroment is reproducible.

- nix-env: Command that feels like pacman, you can install, uninstall. and others. This command works on symlinks rather than packages themslves. If you delete someting iwth nix-env, the binary may still be in /nix/store, but it will not be visible anywhere. To actually get rid of it you need nix-collect-garbage.

## Misc
The main property of nix is to write nix expression once and build everywhere. This gives it large potential for doing different fun things. Nix expression is flexible enough to be a univesal language to describe dependencies in general.

- Manage all your dotfiles with nix. Install `home-manager`, write the systme config in `~/.config/nixpkgs/home.nix`, each time call `home-manager switch` will run the `home.nix` script and update system config.

- Manage haskell dependency with nix. If you have cabal2nix,  `cabal2nix . > default.nix` to generate the derivation, `cabal2ix --shell . > shell.nix` create the shell enviroment.

