local null_ls = require("null-ls")
local checkov_source = {
  method = null_ls.methods.DIAGNOSTICS,
  filetypes = { "terraform" },
  generator = null_ls.generator({
    command = 'checkov', -- string or function
    args = {'-o', 'json', '-f', '$FILENAME'},
    format="json",
    check_exit_code = function(code)
      return true
    end,
    on_output = function(params)
      local diagnostics = {}
      for _, d in ipairs(params.output.results.failed_checks) do
        table.insert(diagnostics, {
          row = d.file_line_range[1],
          code = d.bc_check_id,
          message = d.check_name,
        })
      end
      return diagnostics
    end
  })
}

null_ls.register(checkov_source)
