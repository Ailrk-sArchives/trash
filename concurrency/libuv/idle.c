#include <stdio.h>
#include <uv.h>

// An event is essentailly a token notify you somehting happened.
// e.g. An event for socket read complete.
//
// Watcher is the component wathcing for what event happend, and
// make correspoinding actions.
//
// Each watcher will have a callback associated with it. When an event
// happend, the callback will be called.
//
// Define watcher as two tuple (e, f).

int64_t counter = 0;

void wait_for_a_while(uv_idle_t *handle) {
  counter++;
  if (counter >= 10e6)
    uv_idle_stop(handle);
}

int main(void) {

  uv_idle_t idler;
  uv_idle_init(uv_default_loop(), &idler);
  uv_idle_start(&idler, wait_for_a_while); // start watcher, register callback

  printf("Idling...\n");

  // idle event is fired immediately, so the callback will be called
  // directly.
  uv_run(uv_default_loop(), UV_RUN_DEFAULT);

  uv_loop_close(uv_default_loop());

  return 0;
}
