import bpy
from bpy.props import (StringProperty,
                       BoolProperty,
                       IntProperty,
                       FloatProperty,
                       FloatVectorProperty,
                       EnumProperty,
                       PointerProperty,
                       )

from bpy.types import (Panel,
                       Menu,
                       Operator,
                       PropertyGroup)
bl_info = {
    "name": "Addon template",
    "blender": (2, 80, 0),
    "version": (0, 0, 1),
    "location": "3D View > Tools",
    "category": "Development"
}


class MyProperties(PropertyGroup):
    my_bool = BoolProperty(
        name="Enable or Disable",
        description="A bool property",
        default=False
    )

    my_int = IntProperty(
        name="Int Value",
        description="A integer property",
        default=32,
        min=10,
        max=100
    )

    my_float = FloatProperty(
        name="Float Value",
        description="A float property",
        default=32.1,
        min=0.0,
        max=0.1
    )

    my_float_vector = FloatVectorProperty(
        name="Float Vector",
        description="A float vec property",
        default=(0.0, 0.0, 0.0),
        min=0.0,
        max=0.1
    )

    my_string = StringProperty(
        name="String input",
        description="some strings",
        default="",
        maxlen=1024,
    )

    my_enum = EnumProperty(
        name="DropDown",
        description="Apply data to attributes",
        items=[("OP1", "Option 1", ""),
               ("OP2", "Option 2", ""),
               ("OP3", "Option 3", ""),
               ]
    )


class Hello(Operator):
    bl_label = "Print Values Operator"
    bl_idname = "wm.hello"

    def execute(self, context: bpy.types.Context):
        scence = context.scene
        mytool = scence.my_tool

        print("Hello")
        print("Bool: ", mytool.my_bool)
        print("Int: ", mytool.my_int)
        print("Float: ", mytool.my_float)
        print("String: ", mytool.my_string)
        print("Enum: ", mytool.my_enum)

        return {"FINISHED"}


class CustomMenu(bpy.types.Menu):
    bl_label = "Select"
    bl_idname = "OBJECT_CustomMenu"

    def draw(self, context: bpy.types.Context):
        layout = self.layout

        layout.operator("object.select_all",
                        text="Select/Unselect All").action = "TOGGLE"
        layout.operator("object.select_all", text="Inverse").action = "INVERT"
        layout.operator("object.select_random", text="Random")


class CustomPanel(bpy.types.Panel):
    bl_label = "My Panel"
    bl_idname = "OBJECT_CustomPanel"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"
    bl_category = "Tools"
    bl_context = "objectmode"

    @classmethod
    def poll(cls, context: bpy.types.Context):
        return context.object is not None

    def draw(self, context: bpy.types.Context):
        layout = self.layout
        scence = context.scene
        mytool = scence.my_tool

        layout.prop(mytool, "my_bool")
        layout.prop(mytool, "my_enum", text="")
        layout.prop(mytool, "my_int")
        layout.prop(mytool, "my_float")
        layout.prop(mytool, "my_string")
        layout.prop(mytool, "my_float_vector", text="")
        layout.menu(CustomMenu.bl_idname, text="Presets", icon="SCENE")
        layout.separator()


classes = (
    MyProperties,
    Hello,
    CustomMenu,
    CustomPanel
)


def register():
    from bpy.utils import register_class
    for cls in classes:
        register_class(cls)


def unregister():
    from bpy.utils import unregister_class
    for cls in reversed(classes):
        unregister_class(cls)
    del bpy.types.Scene.my_tool


if __name__ == "__main__":
    register()
