-- I find myself opening files in the same folder a lot
vim.keymap.set("n", "<C-e>", function()
  return string.format(":e %s/", vim.fn.expand("%:h"))
end, { expr = true })

vim.keymap.set("c", "<C-e>", function()
  return string.format("| e %s/", vim.fn.expand("%:h"))
end, { expr = true })

-- -- When pasting in visual mode, don't overwrite the buffer
vim.keymap.set("v", "p", '"_dP', { desc = "paste" })

vim.keymap.set("n", "<leader>fT", function()
  Snacks.terminal(nil, { cwd = vim.fn.expand("%:h") })
end, { desc = "Terminal (file directory)" })

vim.keymap.set("n", "<leader>tfi", function()
  Snacks.terminal("terraform init", { cwd = vim.fn.expand("%:h") })
end, { desc = "Terraform Init" })

vim.keymap.set("n", "<leader>tfp", function()
  Snacks.terminal("terraform plan", { cwd = vim.fn.expand("%:h") })
end, { desc = "Terraform Plan" })

vim.keymap.set("n", "<leader>tfc", function()
  Snacks.terminal("rm -rf ./.terraform", { cwd = vim.fn.expand("%:h") })
end, { desc = "Terraform Clean" })

vim.keymap.set("n", "<leader>tfl", function()
  Snacks.terminal("aws sso login")
end, { desc = "AWS SSO Login" })
