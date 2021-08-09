let foo = "bar"
echo foo

" the difference from using set is you can use the value in an
" expression.
let &textwidth = &textwidth + 100
set textwidth?

" set local number to 1
let &g:number = 1

let name = "name"

function! EchoHi()
  " Using the global foo variable.
  echo "Hi, " . g:foo
endfunction

nnoremap <hi> :cal EchoHi()<cr>
nnoremap <leader>hh :echo "Hello, " . name<cr>

function! SuperTab()
  " viml provides some builtin functions
  let l:part = strpart(getline('.'), col('.') - 2, 1)
  if (l:part =~ '^\W\?$')
    return "\<Tab>"
  else
    return "\<C-n>"
  endif
endfunction

let g:global_var = "global variable"
let s:local_variabl = "local variable"
function! Local()
  let l:foo = "local variable to function"
endfunction

" viml prefix not only correspoinding to global and local scoping.
" You also have window scope, buffer scope, tab scope etc.

let w:foo = "window foo"
let b:state = "buffer state"
let t:state = "tab state"

" some operations
