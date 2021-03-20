#include <iostream>

// virtual table

struct Vector2VTable;
struct Vector3VTable;

struct vector2 {
  Vector2VTable *vtable;
  float x;
  float y;
};

struct vector3 {
  Vector3VTable *vtable;
  float x;
  float y;
  float z;
};

// virtual member functions
float vector2_dot_virtual(const vector2 *self, const vector2 &other) {
  return self->x + other.x + self->y + other.y;
}

float vector3_dot_virtual(const vector3 *self, const vector3 &other) {
  return self->x + other.x + self->y + other.y + self->z + other.z;
}

vector3 vector3_cross_virtual(const vector3 *self, const vector3 &other) {
  // fake cross product.
  return {0, 0, 0};
}

struct Vector2VTable {
  float (*dot)(const vector2 *self, const vector2 &other);
};

struct Vector3VTable {
  float (*dot)(const vector3 *self, const vector3 &other);
  vector3 (*cross)(const vector3 *self, const vector3 &other);
};

void vector2_constructor_base__(vector2 *vec) {
  vec->vtable = new Vector2VTable();
  vec->vtable->dot = &vector2_dot_virtual;
}

vector2 vector2_default_constructor() {
  vector2 tmp__{};
  vector2_constructor_base__(&tmp__);
  return tmp__;
}
vector2 vector2_copy_constructor(const vector2 &other) {
  vector2 tmp__ = vector2_default_constructor();
  tmp__.x = other.x;
  tmp__.y = other.y;
  return tmp__;
}
vector2 vector2_move_constructor(vector2 &&other) {
  vector2 tmp__ = vector2_default_constructor();
  tmp__.x = other.x;
  tmp__.y = other.y;
  return tmp__;
}
void vector2_destructor(vector2 *self) { delete self->vtable; }

void vector3_constructor_base__(vector3 *vec) {
  vec->vtable = new Vector3VTable();
  vec->vtable->dot = &vector3_dot_virtual;
  vec->vtable->cross = &vector3_cross_virtual;
}

vector3 vector3_default_constructor() {
  vector3 tmp__{};
  vector3_constructor_base__(&tmp__);
  return tmp__;
}

vector3 vector3_copy_constructor(const vector3 &other) {
  vector3 tmp__ = vector3_default_constructor();
  tmp__.x = other.x;
  tmp__.y = other.y;
  tmp__.z = other.z;
  return tmp__;
}
vector3 vector3_move_constructor(vector3 &&other) {
  vector3 tmp__ = vector3_default_constructor();
  tmp__.x = other.x;
  tmp__.y = other.y;
  tmp__.z = other.z;
  return tmp__;
}
void vector3_destructor(vector3 *self) { delete self->vtable; }

int main(void) {

  vector3 v3_1 = vector3_default_constructor();
  v3_1.x = 2;
  v3_1.y = 2;
  v3_1.z = 2;
  float result_1 = (*v3_1.vtable->dot)(&v3_1, v3_1);
  std::cout << "vector 3 dot product: " << result_1 << std::endl;

  vector3 v3_2 = vector3_copy_constructor(v3_1);

  vector2 v2_1 = vector2_copy_constructor(*reinterpret_cast<vector2 *>(&v3_2));

  vector3_destructor(&v3_1);
  vector3_destructor(&v3_2);
  vector2_destructor(&v2_1);
  return 0;
}
