"" Lose the annoyances of vi
set nocompatible

"" Load plugins for specific filetypes
filetype plugin on

"" Load indentation rules for specific filetypes
filetype indent on

"" Syntax highlighting
syntax on

"" title in the titlebar.  A bit weird
set titlestring=VIM:\ %-5.10t\ %a%r%m titlelen=15
set title

"" tabs
set tabstop=4 softtabstop=0 noexpandtab shiftwidth=4

"" Put temps somewhere else
set dir=~/.vim/swaps
set backupdir=~/.vim/backups
set undodir=~/.vim/undo
set undofile

" show matching parenthesis
set showmatch

" Autoindentation.
set autoindent

" Allow backspacing over line breaks, start of insert action, and autoindentation
set backspace=eol,start,indent

" Ignore case when searching
set ignorecase
" When searching try to be smart about cases 
set smartcase
" Highlight search results
set hlsearch
" Makes search act like search in modern browsers
set incsearch

" Show line numbers
set number

set mouse=a

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

colorscheme elflord
hi Normal ctermbg=none		

"" Plugins using Vim Plug https://github.com/junegunn/vim-plug

call plug#begin('~/.vim/plugged')
	Plug 'ap/vim-css-color'						" Sets the background to your color. #ff0000 < that is white on red
	Plug 'bling/vim-airline'					" Informative tabline/status bar for vim
	Plug 'chase/vim-ansible-yaml'				" Syntax highlighting for ansible yaml files.  It knows if you're in an ansible folder.
	Plug 'edkolev/tmuxline.vim'					" Generate tmux powerline things that match vim.
	Plug 'ervandew/supertab'					" Make the tab key do tab completion.  Or any other key, for that matter.  Customizable
	Plug 'joonty/vdebug', { 'for': 'php' }		" Interfaces with debuggers.  Needs some configuration soon
	Plug 'joonty/vim-phpqa'						" PHP code checking stuff.	It's messdetector is frustrating, but other features prevent errors
	Plug 'kien/ctrlp.vim'						" Fuzzy file finder that I'll never remember to use
	Plug 'scrooloose/nerdtree'					" An only slightly confusing file-browser in a tree
	Plug '/scrooloose/syntastic'				" All the syntax checking ever
	Plug 'shawncplus/phpcomplete.vim'			" Lots of completions and ctag-jumping stuff for PHP.  Pretty cool, check readme for ctags
	Plug 'StanAngeloff/php.vim'					" Newer PHP syntax highlighting that's a pain to actually get working, I think
	Plug 'tpope/vim-fugitive'					" Git plugin for like, :Gstatus
	Plug 'vim-latex/vim-latex'					" Such a powerful thing for LaTeX
call plug#end()

"" bling/vim-airline
set laststatus=2								" Always show the status
set noshowmode									" Don't show the mode below the statusline, we're taking care of that in vim-airline
let g:airline_powerline_fonts = 1				" Use powerline symbols
let g:airline#extensions#tabline#enabled = 1	" Tablinify the tabbar


"" ervandew/supertab --  changes to let us use tab again
let g:SuperTabMappingForward = '<c-space>'
let g:SuperTabMappingBackward = '<s-c-space>'

"" joonty/vim-phpqa -- Define the ruleset (which you have to pick yourself), autorun, and more
let g:phpqa_messdetector_ruleset = "~/.vim/rulesets.xml"
let g:phpqa_codesniffer_args = "--standard=Zend"
let g:phpqa_codesniffer_autorun = 0
let g:phpqa_messdetector_autorun = 0

"" kien/ctrlp.vim -- Map it to ctrl-p
let g:ctrlp_map = '<c-p>'

"" scrooloose/nerdtree -- Start nerdtree on startup
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif



"" StanAngeloff/php.vim -- Overrides that should be removed or expanded later
function! PhpSyntaxOverride()
  hi! def link phpDocTags  phpDefine
  hi! def link phpDocParam phpType
endfunction

augroup phpSyntaxOverride
  autocmd!
  autocmd FileType php call PhpSyntaxOverride()
augroup END


"" vim-latex/vim-latex -- LaTex stuff
let g:Tex_DefaultTargetFormat = 'pdf'
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
let g:Imap_UsePlaceHolders = 0		"Set this if you ever EVER are going to use '()'
let g:Imap_FreezeImap=1


