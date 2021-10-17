"" vim-fugitive
augroup delete_fugitive_buffers
  autocmd BufReadPost fugitive://* set bufhidden=delete
  "" Delete old git-object buffers when traversing the dag
augroup ENDuto
set autoread

nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gd :Gdiff<CR>
nmap <Leader>gs :Gdiff HEAD~1<CR>
nmap <Leader>gb :Gblame<CR>

" Change vim skeleton file
let g:skeleton_template_dir = expand('~/.config/nvim/templates')

let g:sandwich#recipes = deepcopy(g:sandwich#default_recipes)

"" Neoterm
let g:neoterm_autoscroll=1
let g:neoterm_default_mod='belowright'
let g:neoterm_size=16
tnoremap <Esc><Esc> <C-\><C-n>
