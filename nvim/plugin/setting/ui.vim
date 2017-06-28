"" vim-airline
" Always show the status
set laststatus=2
" Don't show the mode below the statusline
set noshowmode
let g:airline_powerline_fonts = 1                         " Use powerline symbols
" Tablinify the tabbar
let g:airline#extensions#tabline#enabled = 1
 " Don't complain about C-style comments with funny indents
let g:airline#extensions#whitespace#mixed_indent_algo = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
