"" vim-easy-align
xmap ga <Plug>(EasyAlign)|         " Start interactive EasyAlign in visual mode (e.g. ipga)

nmap ga <Plug>(EasyAlign)|         " Start interactive EasyAlign for a motion/text object (e.g. gaip)

"" clever-f.vim
let g:clever_f_smart_case = 1

function! s:check_back_space() abort "{{{
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction"}}}

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-y>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ deoplete#manual_complete()


" Make Enter select completion without adding a new line.
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR>"

set completeopt-=preview
