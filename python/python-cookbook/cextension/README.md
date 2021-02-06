#### python setup.py
disutils is not only for packaging python packages. It can also be used for compiling c extensions directly.  `python setup.py bulid` and `python setup.py build_ext --inplace` can be used to control this process.

### basics of c extensions
`PyArg_ParseTuple` is primiarily used to check the input type. `Py_BuileValue` is used to create a python type within C extension.

After declared functions that is exported in extensions, c extensions typically has a `PyMethodDef` table of list all the informations about those methods. `PyMODINIT_FUNC` is used to created the module for python to use. The work flow is very similar to lua extensions.

It's just an API, and I'm still have no idea about how it works internally. Gonna check it out later.

PS: A quirk, the library used should be a shared library. When compiled with gcc it needs `-c` and `-fPIC` flags to make the library position independent to object file, then use `gcc -shared` to create the shared library. Besides that, during the linking stage the linker need to find the compiled shared library. If you didn't put the .so file in the standard path in `LD_LIBRARY_PATH`, gcc will not find it automatically. It is kinda annoying for a practise example, but it is just how it works.

Why `LD_LIBRARY_PATH` doesn't include the current path? I guess there is something to do with the intergrity, like if there is a name collision between your current package and the system standard one, which library will you link to. It could be wrong.


