" Filetype detection for Perk
augroup perk_filetype
  autocmd!
  autocmd BufNewFile,BufRead *.perk setfiletype perk
augroup END
