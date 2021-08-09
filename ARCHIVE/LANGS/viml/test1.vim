echo "Hi"

" Using lua to script nvim
" Each :lua has it's own scope, you can't share a
" variable in two :lua clauses
lua << EOF
local from_nvim = vim.api.nvim_eval('1 + 1')

-- does comment works? yes
print("from nvim: " .. from_nvim)

print("inspect" .. vim.inspect(vim.api.nvim_eval('[1, 2, 3]')))

print("inspect dicts" .. vim.inspect(vim.api.nvim_eval('{"foo": "bar", "baz": "qux"}')))
print(vim.api.nvim_eval('v:true'))
print(vim.api.nvim_eval('v:null'))

local tbl = { 1, 2, 3 }
for k, v in ipairs(tbl) do
  print(k)
end
print(tbl)

-- jut open the window and close it immediately
vim.api.nvim_command('new')
vim.api.nvim_command('wincmd j')
vim.api.nvim_command('set nonumber')
vim.api.nvim_command('q')

local result = vim.api.nvim_exec(
[[
let mytext = 'hello'
function! MyFunction(text)
  echo a:text
endfunction

call MyFunction(mytext)
]], true)

print(result)

-- nvim 4.4 is essentially unusable. All the newest progress is in neovim 5 (not
-- surprising to be honest.)


EOF

