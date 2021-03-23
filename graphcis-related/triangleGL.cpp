#define GLFW_INCLUDE_NONE
#include <GL/gl.h>
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <array>
#include <stdio.h>

struct Vertex {
  float x, y;
  float r, g, b;
};

static Vertex vertices[3] = {{-0.6f, -0.4f, 1.f, 0.f, 0.f},
                             {0.6f, -0.4f, 0.f, 1.f, 0.f},
                             {0.f, 0.6f, 0.f, 0.f, 1.f}};

// shader code as string directly.
static const char *vertex_shader_text =
    "#version 110\n"
    "uniform mat4 MVP;\n"
    "attribute vec3 vCol;\n"
    "attribute vec2 vPos;\n"
    "varying vec3 color;\n"
    "void main()\n"
    "{\n"
    "    gl_Position = MVP * vec4(vPos, 0.0, 1.0);\n"
    "    color = vCol;\n"
    "}\n";

static const char *fragment_shader_text =
    "#version 110\n"
    "varying vec3 color;\n"
    "void main()\n"
    "{\n"
    "    gl_FragColor = vec4(color, 1.0);\n"
    "}\n";

// setting up an error callback.
// it's passed to glfw, and in case anything failed, this
// cb will get called.
void error_callback(int error, const char *desc) {
  fprintf(stderr, "Error %d, %s\n", error, desc);
}

// get called when the window is closed.
void window_close_callback(GLFWwindow *w) {
  fprintf(stderr, "Window is closed");
}

// receiving the input
static void key_callback(GLFWwindow *window, int key, int scancode, int action,
                         int mod) {
  if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {
    glfwSetWindowShouldClose(window, GLFW_TRUE);
  }
}

int main(void) {
  GLFWwindow *window;

  // init
  if (!glfwInit())
    return -1;

  // setting an error callback.

  // create a windowed mode window with opengl context
  // normally widow creation won't fail, but it's possible that the context
  // creation failed due to driver support.
  if (!(window = glfwCreateWindow(640, 480, "Hello World", nullptr, nullptr))) {
    glfwTerminate();
    return -1;
  }

  // set minial version hint
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 2);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 0);

  // make the window's context the current context.
  glfwMakeContextCurrent(window);

  // register the error callback.
  glfwSetErrorCallback(error_callback);
  glfwSetWindowCloseCallback(window, window_close_callback);
  glfwSetKeyCallback(window, key_callback);

  glewExperimental = GL_TRUE;
  glewInit();

  // get version info
  glEnable(GL_DEPTH_TEST); // enable depth testing.
  glDepthFunc(GL_LEFT);    // depth-testing interpretes small value as closer.

  // loop til it's closed
  // this function just check the windows closing flag.
  while (!glfwWindowShouldClose(window)) {
    // renders stuffs
    int width, height;
    glfwGetFramebufferSize(window, &width, &height);
    glViewport(0, 0, width, height);

    glClear(GL_COLOR_BUFFER_BIT);
    glfwSwapBuffers(window);
    glfwPollEvents();
  }

  glfwTerminate();

  return 0;
}
