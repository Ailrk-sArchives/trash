import importlib
import sys


def lazy(name):
    try:
        return sys.modules[name]

    except KeyError:
        spec = importlib.util.find_spec(name)
        module = importlib.util.module_from_spec(spec)
        loader = importlib.util.LazyLoader(spec.loader)

        loader.exec_module(module)
        return module


expensive = lazy('expensive')

if __name__ == "__main__":
    print(expensive.response)


