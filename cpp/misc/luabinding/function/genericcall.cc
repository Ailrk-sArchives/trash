#include <lua.hpp>
#include <stdarg.h>

extern "C" {

    void call_va(lua_State *L, const char *func, const char *sig, ...) {
        va_list vl;
        int narg, nres;

        va_start(vl, sig);
        lua_getglobal(L, func);

        narg = 0;
        while (*sig) {
            switch (*sig++) {
                case 'd':
                    lua_pushnumber(L, va_arg(vl, double));
                    break;
                case 'i':
                    lua_pushnumber(L, va_arg(vl, int));
                    break;
                case 's':
                    lua_pushstring(L, va_arg(vl, char *));
                    break;
                case '>':
                    goto endwhile;
                default:
                    luaL_error(L, "invalid option %s", *(sig - 1));
            }
            narg ++;
            luaL_checkstack(L, 1, "too many argumetns");
        }
        endwhile:
        nres = -nres;
        while (*sig) {

        }

        va_end(vl);
    }
}
