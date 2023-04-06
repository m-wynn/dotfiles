vim.o.titlestring = 'VIM: %-5.10t %a%r%m'
vim.o.titlelen = 15
vim.o.title = true

os.execute('mkdir -p ~/.local/share/nvim/swaps/')
os.execute('mkdir -p ~/.local/share/nvim/backups/')
os.execute('mkdir -p ~/.local/share/nvim/undo/')

vim.o.directory= vim.env.HOME .. '/.local/share/nvim/swaps//'
vim.o.backupdir= vim.env.HOME ..'/.local/share/nvim/backups//'
vim.o.undodir= vim.env.HOME ..'/.local/share/nvim/undo//'
vim.bo.undofile=true

vim.o.shiftwidth=2
vim.o.tabstop=2

vim.cmd('filetype plugin indent on')
vim.cmd('syntax on')

vim.o.titlestring = 'VIM: %-5.10t %a%r%m'
vim.o.title = true


vim.g.enable_bold_font = true
vim.g.enable_italic_font = true
vim.g.enable_underline_font = true
vim.g.enable_strikethrough_font = true
vim.g.enable_reverse_font = true
vim.g.enable_inverse_font = true
vim.g.mapleader = ' '

vim.o.termguicolors = true

vim.o.showmatch = true
vim.o.autoindent = true
vim.o.expandtab = true
vim.o.smartindent = true
vim.o.smarttab = true
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.softtabstop = 0

vim.o.backspace = 'indent,eol,start'

vim.o.ignorecase = true
vim.o.incsearch = true
vim.o.smartcase = true
vim.o.hlsearch = true
vim.o.wrapscan = true
vim.o.inccommand = 'nosplit'

vim.o.wildmenu = true
vim.o.wildmode = 'longest:full'

vim.o.mouse = 'a'
vim.o.hidden = true
vim.o.wrap = true
vim.o.linebreak = true
vim.o.list = true
vim.o.textwidth = 0
vim.o.wrapmargin = 0
vim.o.splitbelow = true
vim.o.splitright = true

vim.o.visualbell = true
vim.o.guicursor='n-v-c:block-Cursor/lCursor-blinkon0,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor'
vim.o.modeline = false

vim.o.number = true
