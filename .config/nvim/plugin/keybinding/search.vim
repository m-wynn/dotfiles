" Search forward for word under cursor
nnoremap // *
" Search forward for text containing the current selection's equivalence.
vnoremap <leader>/ :VisualEquivalenceForward<CR>

" Search backward for word under cursor
nnoremap ?? #
" Search backward for text containing the current selection's equivalence.
vnoremap <leader>? :VisualEquivalenceBackward<CR>

" Get rid of search highlights by pressing <Esc>
nnoremap <Esc> :noh<Return><Esc>

" Always search forward with `n` and backward with `N`.
" The bindings apply in Normal, Visual, Select, and Operator-pending modes.
noremap <expr> n (v:searchforward ? 'n' : 'N')
noremap <expr> N (v:searchforward ? 'N' : 'n')

