#include <stdio.h>
#include <stdlib.h>
#include <uv.h>

// we create a customized event loop, does nothing,
// and quit.

int main(void) {
  // create an event loop first
  uv_loop_t *loop = (uv_loop_t *)malloc(sizeof(uv_loop_t));
  uv_loop_init(loop); // initalize the loop.

  printf("Now quiting \n");
  uv_run(loop, UV_RUN_DEFAULT); // uv_run run on a given loop.

  uv_loop_close(loop); // deinitalize the loop.
  free(loop);

  return 0;
}
