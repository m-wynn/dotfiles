let http_proxy=$http_proxy
if !filereadable(expand('~/.config/nvim/autoload/plug.vim'))
  if exists($http_proxy)
    silent !curl --insecure -x $http_proxy -fLo ~/.config/nvim/autoload/plug.vim --create-dirs http://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  else
    silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  endif
endif

if exists($http_proxy)
  let g:plug_url_fmrmat='http://github.com/%s.git'
endif

call plug#begin('~/.vim/plugged')
" Tools
Plug 'AndrewRadev/linediff.vim'                    " Diff two visual selections
Plug 'w0rp/ale'                                    " Linting
if !has('mac')
  Plug 'cazador481/fakeclip.neovim'                " Use X and tmux clipboard
endif
Plug 'ctrlpvim/ctrlp.vim'                          " Fuzzy File Finder
Plug 'Konfekt/FastFold'                            " Speed up folds
Plug 'noahfrederick/vim-skeleton'                  " Provides skeleton file
Plug 'ntpeters/vim-better-whitespace'              " Easily strip whitespace
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'tmhedberg/SimpylFold', {'for': 'python'}     " Python folding
Plug 'tpope/vim-abolish'                           " Smarter find-replacement
Plug 'tpope/vim-eunuch'                            " Handy UNIX commands
Plug 'tpope/vim-fugitive'                          " Git plugin
Plug 'tpope/vim-repeat'                            " Use . to repeat plugin stuff
Plug 'tpope/vim-sleuth'                            " Figure out tabs
Plug 'tpope/vim-unimpaired'                        " Add lots of handy mappings
Plug 'vimoutliner/vimoutliner'                     " Outlines
Plug 'Xuyuanp/nerdtree-git-plugin', { 'on': 'NERDTreeToggle' }

" Completion
Plug 'fszymanski/deoplete-emoji'
Plug 'sebastianmarkow/deoplete-rust', {'for': 'rust'}
Plug 'Shougo/context_filetype.vim'                 " Add context filetype
Plug 'Shougo/deoplete.nvim'                        " Completion
Plug 'Shougo/echodoc.vim'                          " Print documentation
Plug 'Shougo/neoinclude.vim'                       " Completion framework
Plug 'Shougo/neopairs.vim'                         " Autoclose after completion
Plug 'wellle/tmux-complete.vim'                    " Completion from tmux panes
Plug 'zchee/deoplete-jedi', {'for': 'python'}      " Python completion

" Editing
Plug 'AndrewRadev/splitjoin.vim'                   " Splitting and joining
Plug 'Chiel92/vim-autoformat'                      " Automagically format
Plug 'honza/vim-snippets'                          " Snippits Stuff
Plug 'junegunn/vim-easy-align'                     " Align things more easily
Plug 'lervag/vimtex', {'for': 'tex'}               " Latex Plugin
Plug 'machakann/vim-sandwich'                      " Change surrounding chars
Plug 'michaeljsmith/vim-indent-object'             " Indents as text objects
Plug 'pearofducks/ansible-vim', { 'do': './UltiSnips/generate.py' }
Plug 'raymond-w-ko/vim-lua-indent', {'for': 'lua'} " Better lua indents
Plug 'rhysd/clever-f.vim'                          " Better f and t
Plug 'rhysd/vim-grammarous'                        " Grammar checking
Plug 'sheerun/vim-polyglot'                        " Support for many languages
Plug 'jparise/vim-graphql'
Plug 'SirVer/ultisnips'                            " Snippits
Plug 'tpope/vim-commentary'                        " Comment things with gc
Plug 'tpope/vim-ragtag'                            " More tag mappings
Plug 'wellle/targets.vim'                          " More text objects

" UI
Plug 'ap/vim-css-color'                            " Colors your hex colors
Plug 'vim-airline/vim-airline'                     " Tabline/status bar for vim
Plug 'vim-airline/vim-airline-themes'              " Themes for Airline
Plug 'majutsushi/tagbar'                           " Display tags in a window
Plug 'w0ng/vim-hybrid'                             " Colors!
Plug 'https://gitlab.com/lafrenierejm/vim-equivalence.git'

call plug#end()

""""""""
"   General   "
"     Vim     "
""""""""

" Load plugins and indentation for specific filetypes
filetype plugin indent on

" Syntax highlighting
syntax on

" title in the titlebar.  A bit weird
set titlestring=VIM:\ %-5.10t\ %a%r%m titlelen=15
set title

" Put temps somewhere else
silent !mkdir -p ~/.local/share/nvim/swaps/
silent !mkdir -p ~/.local/share/nvim/backups/
silent !mkdir -p ~/.local/share/nvim/undo/

set directory=~/.local/share/nvim/swaps//
set backupdir=~/.local/share/nvim/backups//
set undodir=~/.local/share/nvim/undo//
set undofile

" Colors!
let g:hybrid_custom_term_colors = 1
set background=dark
colorscheme hybrid
let g:airline_theme = 'hybridline'
" Use this if you have a colorscheme that breaks terminal transparency
hi Normal ctermbg=none
" Use truecolor
set termguicolors

" Show matching parenthesis
set showmatch

" Autoindentation.
set autoindent

" tabs
set tabstop=4 softtabstop=0 expandtab shiftwidth=4

" Allow backspacing over line breaks, start of insert action, and autoindentation
set backspace=eol,start,indent

" Ignore case when lowercase is used in the search
set ignorecase smartcase
" Highlight search results
set hlsearch
" Move to matched string while typing the search pattern
set incsearch
" Wrap search around end-of-file
set wrapscan

" Completion for commands
set wildmenu

" Show line numbers
set number
augroup line_numbers
  autocmd InsertEnter * :set norelativenumber
  autocmd InsertLeave * :set relativenumber
augroup END

function! NumberToggle()
  if(&relativenumber == 1)
    set norelativenumber
  else
    set relativenumber
  endif
endfunc

nnoremap <C-n> :call NumberToggle()<cr>

" Make the mouse useful
set mouse=a

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

" Allow switching buffers without saving
set hidden

" Word wrapping is fine, just don't insert newlines, please.
set wrap linebreak list
set textwidth=0
set wrapmargin=0

" Split more naturally
set splitbelow
set splitright

" Make Enter select completion without adding a new line.
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR>"

" Don't ring the terminal bell
set visualbell t_fb=

" Incsub stuff
set inccommand=nosplit

" Remember cursor position on buffer leave
augroup cursorRemember
  au BufLeave * let b:winview = winsaveview()
  au BufEnter * if(exists('b:winview')) | call winrestview(b:winview) | endif
augroup end

" Automagically format on save
augroup saveFormat
  au BufWrite * :Autoformat
augroup end

" F12 resyncs syntax
noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>

" Change cursor shape
set guicursor=n-v-c:block-Cursor/lCursor-blinkon0,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor

" Change vim skeleton file
let g:skeleton_template_dir = "~/.config/nvim/templates"

let g:sandwich#recipes = deepcopy(g:sandwich#default_recipes)
