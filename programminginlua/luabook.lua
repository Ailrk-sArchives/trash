-- Part I

start = {
    main = function()
        io.open('a')
        local a = "one string"
        local b = string.gsub(a, "one", "another")
        print(#a)
        print(a .. b)
        print(a .. 2)
        local page = [[
            <html>
                <head>
                    <title></title>
                </head>
                <body>
                    <a href="http://www.lua.org">Lua</a>
                </body>
            </html>
        ]]
        local mml = [=[
            <![CDATA[
                hello world
            ]]>
        ]=]
    end
}

Ch2EightQueen = {
    main = function ()
        local N = 8
        local function isplaceok (a, n, c)
            for i = 1, n - 1 do
                if (a[i] == c) or
                   (a[i] - 1 == c - n) or
                   (a[i] + 1 == c + n) then
                   return false
                end
            end
            return true
        end

        local function printsolution (a)
            for i=1, N do
                for j=1, N do
                    io.write(a[i] == j and "X" or ".", " ")
                end
                io.write("\n")
            end
            io.write("\n")
        end

        local function addqueen (a, n)
            if n > N then
                printsolution(a)
                os.exit()
            else
                for c=1, N do
                    if isplaceok(a, n, c) then
                        a[n] = c
                        addqueen(a, n + 1)
                    end
                end
            end
        end
        addqueen({}, 1)
    end
}

-- ch 4
Ch4String = {
    main = function()
        print(tonumber("0x3.1p-3"))
        print(tonumber("10001", 2))
        print(tostring(12))
        print(string.byte("abc", 1, 2))
    end,

    defsomefunc = function()
        local function insert(src, n, word)
            return string.sub(src, 1, n) .. word .. string.sub(src, n, -1)
        end

        local function remove(src, i, j)
            return string.sub(src, 1, i) .. string.sub(src, j, -1)
        end
        local function ispali(src)
            return string.reverse(src) == src
        end

        insert("long string", 5, "-o-")
        remove("good evening dwag", 5, 14)
        ispali("step on no pets")
        ispali("dog")
    end
}

-- Ch 5 data structures with table.
Ch5Table = {
    define = function ()
        local seasons = {"Spring", "Summer", "Autumn", "Winter"} -- table as array
        print(seasons)
        local point = { x = 0, y = 0 }                           -- table as record
        print(point)

        local polyline = {  -- table constructor mixed.
            color="blue",
            thickness=2,
            npoints=4,
            {x=0, y=0},
            {x=-10, y=0},
            {x=10, y=0},
            {x=1, y=0},
        }

        local polyline2 = {  -- for special characters as key.
            ["+"]="add",
            ["-"]="minus",
            ["*"]="multiply",
            ["/"]="divide",
        }
        print(polyline[2].x == -10)
        print(polyline2["-"] == "minus")
    end,

    traversal = function()
        -- as list
        local list1 = {}
        for i = 1, 10 do
            list1[i] = io.read()
        end

        local printlist = (function (list)
            for i=1, #list do
                print(list[i])
            end
        end)
        printlist(list1)

        local t = {10, print, x=12, k="hi"}
        for k, v in pairs(t) do
            print(k, v)
        end

    end,

    safenav = function()
        local E = {}
        local company = { director = { address = { zipcode = 1}} }
        local zip1 = (((company or E).director or E).address or E).zipcode
        local zip2 = (((company1 or E).director or E).address or E).zipcode
        print(zip1 == 1)
        print(zip2 == nil)
    end,

    exercise = function()
        -- recursive reference
        local a = {}
        a.a = a
        a.a.a.a.a.a.a.a.a.a.a.a = 100  -- now a.a is 100

        local escapeseq = {
            ["\n"] = "line break",
            ["\r"] = "return",
        }

        local function do_polynomial(t, x)
            local acc = 0
            for i, coeff in pairs(t) do
                acc = acc + coeff * x ^ (i - 1)
            end
            return acc
        end

        local function isseq(t)
            local flag = true
            for i=1, #t do
                if t[i] == nil then
                    print("spot")
                    flag = false
                end
            end
            return flag
        end

        local function insert_list(dest, src, idx)
            for i=1, #src do
                table.insert(dest, i+idx-1, src[i])
            end
        end

        local function tconcat(t, sep)
            local result = ""
            for k, v in pairs(t) do
                result = result .. v
                if k < #t then result = result .. sep end
            end
            return result
        end

        do_polynomial({1,3,5,7}, 10)
        isseq({1,2,3})
        isseq({1,2,nil,3})
        insert_list({1,2,3,4}, {10, 20, 30}, 2)
    end
}

Ch6Functions {
    func_features = function()

        local s, e = string.find("hello Lua user", "Lua")

        local function foo()
            return "a", "b", "c"
        end

        a, b, c = foo()  -- unpack tuple
        foot = {foo()}   -- unpack in table.
    end,

    varadic = function()
        local function add(...)
            local s = 0
            for _, v in ipairs{...} do  -- sugar.
                s = s + v
            end
            return s
        end
        add(1, 2, 3, 4, 5)

        local function printf(fmt, ...)
            io.write(string.format(fmt, ...))
        end
        printf("%d", 1)

        local function pack(...)
            return table.pack(...)
        end

        local t = {1, 2, 3}

        print(t == pack(table.unpack{t}))

    end,

    tailcall = function()
        -- f call g at last line. we know f has nothing else to
        -- do when calling g, so its stack is nolong needed.
        local function g(x) return x end
        local function f(x) x = x + 1; return g(x) end
        local function foo(n) if n > 0 then return foo(n - 1) else return n end end
    end,

    exercise = function()

        local printlist = function(list, idx)
            for i=(idx or 1), #list do
                print(list[i])
            end
        end

        local skipfirst = function(list)
            printlist(list, 2)
        end
        print(skipfirst({1,2,3}))

        -- random
        local randint = function(from, to)
            return math.ceil(math.random() * (from - to) + to)
        end

        local shuffle =function(list)
            local randomness = 500
            for _=1, randomness do
                local randidx = randint(1, #list)
                local picked = list[randidx]
                table.remove(list, randidx)
                table.insert(list, 1, picked)
            end
            return list
        end

        for i=1, 100 do
            print(table.concat(shuffle{1,2,3,4,5,6,7,8,9}, " "))
        end

    end
}

Ch7IO {
    simpleIO = function()

        local simple = function()
            io.input("eightqueen.lua")
            local f = io.read("a")
            f = string.gsub(f, "queen", "king")
            io.write(f)
            print(f)
        end

        local bylines = function()
            for _ = 1, math.huge do
                local line = io.read("L")
                if line == nil then break end
                io.write(line)
            end
        end

        local sortfile = function ()
            local lines = {}
            for line in io.lines() do
                lines[#lines + 1] = line
            end
            table.sort(lines)

            for _, l in pairs(lines) do
                io.write(l, "\n")
            end
        end

        local block = function()
            while true do
                local block = io.read(2^3) -- read 8 bytes.
                if not block then break end
                io.write("write: \n")
                io.write(block)
            end
        end
    end
}

Ch8Gaps = {

    scope = function()
        local i = 10
        x = "a"
        if i > 20 then
            local x = "b"
            print(x)
        else
            print(x)
        end
    end,

    doblock = function()
        local x1, x2
        local a = 10
        do
            local a2 = 2 * a
            local d = a2 ^ 2
            x1 = a2 + d
            x2 = a2 - d
        end
        print (x1, x2)
    end,

    returnlimit = function()  -- return must be next to a end keyword.
        local function foo(x)
            if x > 10 then
                do return 10 end  -- use do block to return in the middle
            end
            return x
        end
    end,

    mazegame = function()
        local function info(name)
            print("::::: You are in " .. name .. ":::::")
            print("go east or south?")
            io.write("e/s: ")
        end

        goto room1
        ::room1:: do
            info("room 1")
            local move = io.read()
            if move == "s" then goto room3
            elseif move == "e" then goto  room2
            else
                print("invalid move")
                goto room1
            end
        end

        ::room2:: do
            info("room 2")
            local move = io.read()
            if move == "s" then goto room3
            elseif move == "e" then goto  room1
            else
                print("invalid move")
                goto room2
            end
        end

        ::room3:: do
            info("room 3")
            local move = io.read()
            if move == "s" then goto room1
            elseif move == "e" then goto  room4
            else
                print("invalid move")
                goto room2
            end
        end

        ::room4:: do
            print("You win")
        end
    end

}


-- Part II
