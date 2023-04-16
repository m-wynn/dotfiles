-- I find myself opening files in the same folder a lot
vim.keymap.set("n", "<C-e>", function()
  return string.format(":e ./%s/", vim.fn.expand("%:h"))
end, { expr = true })

vim.keymap.set("c", "<C-e>", function()
  return string.format("| e ./%s/", vim.fn.expand("%:h"))
end, { expr = true })

-- -- When pasting in visual mode, don't overwrite the buffer
vim.keymap.set("v", "p", '"_dP', { desc = "paste" })
