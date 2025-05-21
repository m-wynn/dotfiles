local severities = {
  error = vim.diagnostic.severity.ERROR,
  warning = vim.diagnostic.severity.WARN,
  refactor = vim.diagnostic.severity.INFO,
  convention = vim.diagnostic.severity.HINT,
}
vim.filetype.add({
  pattern = {
    [".*/.github/workflows/.*%.yml"] = "yaml.ghaction",
    [".*/.github/workflows/.*%.yaml"] = "yaml.ghaction",
  },
})

require("lint").linters.checkov = {
  name = "checkov",
  cmd = "checkov",
  stdin = false,
  args = {
    "-o",
    "json",
    "-f",
  },
  stream = "stdout",
  ignore_exitcode = true,
  parser = function(output)
    if output == "" then
      return {}
    end
    local decoded = vim.fn.json_decode(output)
    local diagnostics = {}
    local source = decoded["check_type"]

    -- if they are no results, don't do anything.
    if decoded["results"] == nil then
      return {}
    end

    if decoded["results"]["failed_checks"] then
      for _, check in ipairs(decoded["results"]["failed_checks"]) do
        local id = check["check_id"]
        local name = check["check_name"]
        local range = check["file_line_range"]
        local guideline = check["guideline"]

        local sv = severities.warning
        table.insert(diagnostics, {
          lnum = range[1],
          col = 0,
          end_lnum = range[2],
          end_col = 0,
          severity = sv,
          message = id .. ": " .. name .. "\n" .. guideline,
          source = "checkov: " .. source,
        })
      end
    end
    return diagnostics
  end,
}

return {
  {
    "mfussenegger/nvim-lint",
    opts = {
      linters_by_ft = {
        terraform = { "checkov", "terraform_validate" },
        tf = { "checkov", "terraform_validate" },
        ghaction = { "actionlint" },
        yaml = { "yamllint" },
      },
    },
  },
}
