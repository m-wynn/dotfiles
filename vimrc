set nocompatible
filetype plugin on
filetype indent on
syntax on

call plug#begin('~/.vim/plugged')
    Plug 'joonty/vdebug', { 'for': 'php' }
    Plug 'StanAngeloff/php.vim'
    Plug 'joonty/vim-phpqa'
    Plug 'shawncplus/phpcomplete.vim'
    Plug 'ervandew/supertab'
    Plug 'scrooloose/nerdtree'
    Plug 'tpope/vim-fugitive'
    Plug 'kien/ctrlp.vim'
    Plug 'vim-latex/vim-latex'
    Plug 'ap/vim-css-color'
    Plug de'longw/nginx.vim'
call plug#end()


"" phpqa tools
let g:phpqa_messdetector_ruleset = "~/.vim/rulesets.xml"
let g:phpqa_codesniffer_args = "--standard=Zend"
let g:phpqa_codesniffer_autorun = 0
let g:phpqa_messdetector_autorun = 0


"" File tree
"autocmd vimenter * NERDTree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

let g:ctrlp_map = '<c-p>'

"" Put temps somewhere else
set dir=~/.vim/swaps
set backupdir=~/.vim/backups
set undodir=~/.vim/undo
set undofile

" set show matching parenthesis
set showmatch
" Autoindentation.
set autoindent
" Syntax always on, please.
set backspace=eol,start,indent

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases 
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

set number
set mouse=a
" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

colorscheme elflord

"" LaTeX stuff
let g:Tex_DefaultTargetFormat = 'pdf'
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'

"" php.vim overrides
function! PhpSyntaxOverride()
  hi! def link phpDocTags  phpDefine
  hi! def link phpDocParam phpType
endfunction

augroup phpSyntaxOverride
  autocmd!
  autocmd FileType php call PhpSyntaxOverride()
augroup END