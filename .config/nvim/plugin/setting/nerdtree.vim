"" nerdtree
function! NERDTreeRefresh()
  if &filetype ==# 'nerdtree'
    silent exe substitute(mapcheck('R'), '<CR>', '', '')
  endif
endfunction

augroup nerdtree
  autocmd BufEnter * call NERDTreeRefresh()
augroup end

map <C-n> :NERDTreeToggle<CR>
let g:NERDTreeUpdateOnCursorHold = 0

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | exe 'NERDTree' | endif

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
