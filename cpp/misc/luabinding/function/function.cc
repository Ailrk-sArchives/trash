#include <lua.hpp>

extern "C" {
    double f(lua_State *L, double x, double y) {
        double z;
        lua_getglobal(L, "f");
        lua_pushnumber(L, x);
        lua_pushnumber(L, y);

        if (lua_pcall(L, 2, 1, 0) != 0)
            luaL_error(L, "error running function f: %s", lua_tostring(L, -1));

        if (!lua_isnumber(L, -1))
            luaL_error(L, "function f must return a num");
        z = lua_tonumber(L, -1);
        lua_pop(L, 1);
        return z;
    }

    int main(void) {
        lua_State *L = luaL_newstate();
        luaL_dofile(L, "./function.lua");
        luaL_openlibs(L);
        lua_getglobal(L, "f");

        double z = f(L, 1, 0.2);
        printf("%f", z);

        return 0;
    }

}

