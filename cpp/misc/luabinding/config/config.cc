#include <lua.hpp>
#include <cstring>
#define MAX_COLOR 255

typedef struct ColorRGB {
    unsigned char red, green, blue;
} ColorRGB;

struct ColorTable {
    char const *name;
    unsigned char red, green, blue;
} colortable[] = {
    {"WHITE", MAX_COLOR, MAX_COLOR, MAX_COLOR},
    {"RED", MAX_COLOR, 0, 0},
    {"GREEN", 0, MAX_COLOR, 0},
    {"BLUE", 0, 0, MAX_COLOR},
    {"BLACK", 0, 0, 0},
    {"NULL", 0, 0, 0}
};

// load some tables.
extern "C" {

    int getfield(lua_State *L, const char *key) {
        int result;
        lua_pushstring(L, key);
        lua_gettable(L, -2);
        if (!lua_isnumber(L, -1)) luaL_error(L, "invalid component");
        result = (int)lua_tonumber(L, -1);
        lua_pop(L, 1);
        return result;
    }

    // assume table is at the top.
    void setfield (lua_State *L, const char *key, int value) {
        lua_pushstring(L, key);
        lua_pushnumber(L, (double)value/MAX_COLOR); lua_settable(L, -3); }

    void setcolor(lua_State *L, struct ColorTable *ct) {
        lua_newtable(L);
        setfield(L, "r", ct->red);
        setfield(L, "g", ct->green);
        setfield(L, "b", ct->red);
        lua_setglobal(L, ct->name);
    }

    int main(void) {
        lua_State *L = luaL_newstate();
        luaL_dofile(L, "./config.lua");
        lua_getglobal(L, "background");

        ColorRGB color = {0, 0, 0};

        if (lua_isstring(L, -1)) {
            const char *name = lua_tostring(L, -1);
            int i = 0;
            while (colortable[i].name != NULL &&
                    strcmp(name, colortable[i].name) != 0)
                i++;

            if (colortable[i].name == NULL)
                luaL_error(L, "invalid color name %s", name);
            else {
                color.red = colortable[i].red;
                color.green = colortable[i].green;
                color.blue = colortable[i].blue;
            }
        } else if (lua_istable(L, -1)) {
            color.red = getfield(L, "r");
            color.green = getfield(L, "g");
            color.blue = getfield(L, "b");
        } else
            luaL_error(L, "invalid value for background");

        printf("RGB: %d, %d, %d", color.red, color.green, color.blue);
        return 0;
    }
}
