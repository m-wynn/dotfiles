" Search forward for word under cursor
nnoremap // *

" Search backward for word under cursor
nnoremap ?? #

" Get rid of search highlights by pressing <Esc>
nnoremap <Esc> :noh<Return><Esc>

" Always search forward with `n` and backward with `N`.
" The bindings apply in Normal, Visual, Select, and Operator-pending modes.
noremap <expr> n (v:searchforward ? 'n' : 'N')
noremap <expr> N (v:searchforward ? 'N' : 'n')

noremap ; :
