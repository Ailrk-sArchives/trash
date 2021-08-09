import bpy

bl_info = {
    "name": "Cursor Array",
    "blender": (2, 80, 0),
    "category": "Object",
}


class ObjectCursorArray(bpy.types.Operator):
    bl_idname = "object.cursor_array"
    bl_label = "Cursor Array"
    bl_options = {"REGISTER", "UNDO"}

    total = bpy.props.IntProperty(name="Step", default=2, min=1, max=100)

    def execute(self, context: bpy.types.Context):
        scene = context.scene
        cursor_pos = scene.cursor.location
        obj = context.active_object

        for i in range(self.total):
            obj_new = obj.copy()
            scene.collection.objects.link(obj_new)
            factor = i / self.total
            obj_new.location = (obj.location * factor) + \
                (cursor_pos * (1.0 - factor))

        return {"FINISHED"}


def menu_draw(self, context):
    self.layout.operator(ObjectCursorArray.bl_idname)


addon_keymaps = []


def register():
    bpy.utils.register_class(ObjectCursorArray)
    bpy.types.VIEW3D_MT_object.append(menu_draw)

    wm = bpy.context.window_manager

    kc = wm.keyconfigs.addon

    if kc:
        km = wm.keyconfigs.addon.keymaps.new(
            name="Object Mode", space_type="EMPTY")
        kmi = km.keymap_items.new(
            ObjectCursorArray.bl_idname, "T", "PRESS", ctrl=True, shift=True)
        kmi.properties.total = 4
        addon_keymaps.append((km, kmi))


def unregister():
    for km, kmi in addon_keymaps:
        km.keymap_items.remove(kmi)
    addon_keymaps.clear()

    bpy.utils.unregister_class(ObjectCursorArray)
    bpy.types.VIEW3D_MT_object.remove(menu_draw)


if __name__ == "__main__":
    register()
