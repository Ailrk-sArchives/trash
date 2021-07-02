-- lua curring has some problem comes from ambiguious syntax
fs = (function() return function() return 1 end end)()()

triple = ((function(a)
    return function(b) return function(c) return {a, b, c} end end
end)(1)(2)(3))

make_pair = function(a) return function(b) return {a, b} end end

function s(x) return function(y) return function(z) return (x(z))(y(z)) end end end

s(make_pair)(function(x) return x + 10 end)(10)

x = (s(make_pair)(function(x) return x + 10 end)(10))

y = (s(make_pair)(function(x) return x + 10 end)(10))

function print_all(...)
    local arg = table.pack(...)
    print('in print_all' .. #arg)
    for i, v in ipairs(arg) do
        print(i)
        print(v)
    end
end

print_all(1, 2, 3)
