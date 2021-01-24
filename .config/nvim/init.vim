if !filereadable(expand('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

call plug#begin('~/.local/share/nvim/plugged')

" Tools
Plug 'airblade/vim-rooter'                         " Chdir to nearest .git
Plug 'jreybert/vimagit'                            " Git management buffer
Plug 'lotabout/skim', { 'dir': '~/.skim', 'do': './install' }
Plug 'lotabout/skim.vim'
Plug 'noahfrederick/vim-skeleton'                  " Provides skeleton file
Plug 'tmhedberg/SimpylFold', {'for': 'python'}     " Python folding
Plug 'deathlyfrantic/vim-fubitive', { 'branch': 'fix-bitbucket-cloud-urls' }
Plug 'tpope/vim-abolish'                           " Smarter find-replacement
Plug 'tpope/vim-eunuch'                            " Handy UNIX commands
Plug 'tpope/vim-fugitive'                          " Git plugin
Plug 'tpope/vim-repeat'                            " Use . to repeat plugin stuff
Plug 'tpope/vim-rhubarb'                           " Github plugin for fugitive
Plug 'tpope/vim-sleuth'                            " Figure out tabs
Plug 'tpope/vim-unimpaired'                        " Add lots of handy mappings
Plug 'dense-analysis/ale'                          " Linting
Plug 'kassio/neoterm'
Plug 'dense-analysis/ale'                          " Linting
Plug 'vim-vdebug/vdebug'

" NerdTree
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'     " NerdTree syntax highlighting
Plug 'Xuyuanp/nerdtree-git-plugin', { 'on': 'NERDTreeToggle' }

" Completion
Plug 'fszymanski/deoplete-emoji'
Plug 'Shougo/context_filetype.vim'                 " Add context filetype
Plug 'Shougo/deoplete.nvim'                        " Completion
Plug 'Shougo/echodoc.vim'                          " Print documentation
Plug 'Shougo/neoinclude.vim'                       " Completion framework
Plug 'Shougo/neopairs.vim'                         " Autoclose after completion
Plug 'wellle/tmux-complete.vim'                    " Completion from tmux panes

" Editing
Plug 'AndrewRadev/splitjoin.vim'                   " Splitting and joining
Plug 'junegunn/vim-easy-align'                     " Align things more easily
Plug 'tpope/vim-surround'
Plug 'machakann/vim-sandwich'                      " Change surrounding chars
Plug 'michaeljsmith/vim-indent-object'             " Indents as text objects
Plug 'rhysd/clever-f.vim'                          " Better f and t
Plug 'sheerun/vim-polyglot'                        " Support for many languages
Plug 'tpope/vim-commentary'                        " Comment things with gc
Plug 'tpope/vim-ragtag'                            " More tag mappings
Plug 'wellle/targets.vim'                          " More text objects
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }

" Snippets
Plug 'SirVer/ultisnips'
Plug 'epilande/vim-react-snippets'
Plug 'honza/vim-snippets'                          " Snippits Stuff

" UI
Plug 'ap/vim-css-color'                            " Colors your hex colors
Plug 'vim-airline/vim-airline'                     " Tabline/status bar for vim
Plug 'vim-airline/vim-airline-themes'              " Themes for Airline
Plug 'majutsushi/tagbar'                           " Display tags in a window
Plug 'morhetz/gruvbox'                             " Colors
Plug 'ryanoasis/vim-devicons'

call plug#end()

"""""""""""""""
"   General   "
"     Vim     "
"""""""""""""""

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
let g:enable_bold_font = 1
let g:enable_italic_font = 1
colorscheme gruvbox
let g:airline_theme = 'gruvbox'
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

let mapleader = "\<Space>"

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

nnoremap <C-m> :call NumberToggle()<cr>

" Make the mouse useful
set mouse=a

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

" Allow switching buffers without saving
set hidden

" wrap words
set wrap linebreak list
set textwidth=0
set wrapmargin=0

" Split more naturally
set splitbelow
set splitright

" Don't ring the terminal bell
set visualbell t_fb=

" Incsub stuff
set inccommand=nosplit

" F12 resyncs syntax
noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>

" Change cursor shape
set guicursor=n-v-c:block-Cursor/lCursor-blinkon0,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor

set nomodeline

" Jump to start and end of line using the home row keys
map H ^
map L $
let g:UltiSnipsExpandTrigger="<tab>"

" command! -bang -nargs=* Rg
"   \ call fzf#vim#rg_interactive(
"   \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
"   \   <bang>0 ? fzf#vim#with_preview({'options': '--delimiter : --nth 4..'}, 'up:60%')
"   \           : fzf#vim#with_preview({'options': '--delimiter : --nth 4..'}, 'right:50%:hidden', '?'),
"   \   <bang>0)

vnoremap p "_dP
