#define LUA_MLIB
#include <lua.hpp>
#include <math.h>

extern "C" {
    static int cswap (lua_State *L) {
        double arg1 = luaL_checknumber(L, 1);
        double arg2 = luaL_checknumber(L, 2);

        lua_pushnumber(L, arg2);
        lua_pushnumber(L, arg1);
        return 2;
    }

    static int msin (lua_State *L) {
        double arg = luaL_checknumber(L, 1);
        lua_pushnumber(L, sin(arg));
        return 1;
    }

    static const struct luaL_Reg mylib[] = {
        {"cswap", cswap},
        {"msin", msin},
        {NULL, NULL}
    };

    LUA_MLIB int luaopen_mylib(lua_State *L) {
        luaL_newlib(L, mylib);
        return 1;
    }
}
