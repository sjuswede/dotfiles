# dotfiles
Stowed dotfiles for various software running under my normal Linux install.

## Requirements

Ensure you have the following installed on your system

### Git

```
dnf install git
```

### Stow

```
dnf install stow
```

Install into $HOME using

```
$ git clone git@github.com:sjuswede/dotfiles.git
```

Renaming the dotfile directory to .dotfile might be handy.

```
mv ~/dotfiles ~/.dotfiles
```

The structure is one directory per program. This allows for stow'ing only select programs as required.
