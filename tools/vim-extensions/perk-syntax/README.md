# Perk Vim Syntax Plugin

This directory provides a minimal Vim plugin for Perk syntax highlighting and filetype detection.

## Installation
Copy (or symlink) the contents into your Vim runtime path, e.g.:

```
mkdir -p ~/.vim/pack/perk/start/perk
cp -R ./* ~/.vim/pack/perk/start/perk/
```

For Neovim:
```
mkdir -p ~/.local/share/nvim/site/pack/perk/start/perk
cp -R ./* ~/.local/share/nvim/site/pack/perk/start/perk/
```

Or use a plugin manager (lazy.nvim, vim-plug, etc.) pointing to `tools/vim-extensions/perk`.

## Features
- Highlights Perk keywords, types, numbers, strings (with escapes), comments and TODOs.
- Highlights function names after `fun` and variable names declared via `let`.
- Marks embedded C delimiters `BEGIN_C` / `END_C`.

## Extending
You can add more highlighting by editing `syntax/perk.vim`:
- Add new keywords to the `perkKeyword` group.
- Add additional types to `perkType`.
- Create new regions for multi-line constructs.

## Filetype Detection
`ftdetect/perk.vim` sets the filetype for any `*.perk` file automatically.

## Troubleshooting
If highlighting does not appear:
1. Run `:echo &filetype` to verify it reports `perk`.
2. Ensure the plugin directories are correctly placed in your runtime path.
3. Check for conflicting plugins defining `perk`.
