vim.o.titlestring = 'VIM: %-5.10t %a%r%m'
vim.o.titlelen = 15
vim.o.title = true

os.execute("mkdir -p ~/.local/share/nvim/swaps/")
os.execute("mkdir -p ~/.local/share/nvim/backups/")
os.execute("mkdir -p ~/.local/share/nvim/undo/")

vim.o.directory= vim.env.HOME .. '/.local/share/nvim/swaps//'
vim.o.backupdir= vim.env.HOME ..'/.local/share/nvim/backups//'
vim.o.undodir= vim.env.HOME ..'/.local/share/nvim/undo//'
vim.o.undofile=true

vim.o.shiftwidth=2
vim.o.tabstop=2
