#include <ao/ao.h>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <math.h>
#include <thread>
#include <time.h>
#include <unistd.h>
#include <xcb/xcb.h>
#include <xcb/xcb_event.h>
#include <atomic>

// X c bindings, low level api for x window server.
// xcb is an alternative of xlib,

// Client and server model of X window system.
// Two parts:
// 1. Client decide what to do
// 2. X server actually draw on the screen and read user inputs.
// Note it's a bit different from a normal client server.
// Server handles drawing, so if you want to render a window
// on a remote machine, the remote machine need to run a server.

// X11 use x message protocol. It's orignally a TCP/IP net based protocol,
// but you also optimization for local client. Like using shared
// memory and unix domain socket.

// GUI programming and asynchronous nature.
// GUI program usually use asynchronous model. (event driven programming.)
// The program mostly sit aside, waiting for events event by the X server,
// and acts on these events.
//
// For an asynchronous model, a very important part to be aware is not to make
// your callback too big. If it takes up too much time, it will block the
// event queue and cause your UI hangs.

// Some basic notion of xcb
// 1. X connection
//    represents connection with given x server. It hides queue of
//    messages coming from the server, and queue of pending requests that
//    the client intends to send to the server.
// 2. Request and reply are not locked, the cost of round trip time is
// suppressed.
// 4. Graphics context
// 5. Events

ao_device *device;
ao_sample_format format;
int default_driver;
char *buffer;
int buf_size;
int sample;
int i;

void beep(float freq = 440.0) {
  /* -- Play some stuff -- */
  buf_size = format.bits / 8 * format.channels * format.rate;
  buffer = static_cast<char *>(calloc(buf_size, sizeof(char)));

  for (i = 0; i < format.rate; i++) {
    sample =
        (int)(0.75 * 32768.0 * sin(2 * M_PI * freq * ((float)i / format.rate)));

    /* Put the same stuff in left and right channel */
    buffer[4 * i] = buffer[4 * i + 2] = sample & 0xff;
    buffer[4 * i + 1] = buffer[4 * i + 3] = (sample >> 8) & 0xff;
  }
  ao_play(device, buffer, buf_size);
}

void report_screen(xcb_screen_t *screen) {
  std::cout << "==========GUI START============" << std::endl;
  std::cout << "screen: " << screen->root << std::endl;
  std::cout << "width: " << screen->width_in_pixels << std::endl;
  std::cout << "height: " << screen->height_in_pixels << std::endl;
  std::cout << "black pix: " << screen->black_pixel << std::endl;
  std::cout << "white pix: " << screen->white_pixel << std::endl;
  std::cout << "===============================" << std::endl;
}

int main(void) {

  /* -- Initialize -- */
  ao_initialize();

  /* -- Setup for default driver -- */
  default_driver = ao_default_driver_id();

  memset(&format, 0, sizeof(format));
  format.bits = 16;
  format.channels = 2;
  format.rate = 44100;
  format.byte_format = AO_FMT_LITTLE;

  /* -- Open driver -- */
  device = ao_open_live(default_driver, &format, NULL);
  if (device == NULL) {
    fprintf(stderr, "Error opening device.\n");
    exit(-1);
  }

  // end init libao

  int screen_num;

  // to do anythign you need a x server connection
  xcb_connection_t *conn = xcb_connect(NULL, &screen_num);

  if (xcb_connection_has_error(conn)) {
    std::cout << "error on connection" << std::endl;
    exit(1);
  }

  // get setup of this connection
  const xcb_setup_t *setup = xcb_get_setup(conn);

  // iterator of all screens.
  xcb_screen_iterator_t iter = xcb_setup_roots_iterator(setup);

  xcb_screen_t *screen = iter.data;
  report_screen(screen);

  // create window
  // config the window to take events.
  unsigned int value_mask = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK;
  unsigned int value_list[]{screen->white_pixel, XCB_EVENT_MASK_BUTTON_PRESS |
                                                     XCB_EVENT_MASK_KEY_PRESS |
                                                     XCB_EVENT_MASK_EXPOSURE};

  uint32_t window_id = xcb_generate_id(conn);

  xcb_create_window(conn, screen->root_depth, window_id, screen->root, 0, 0,
                    100, 100, 1, XCB_WINDOW_CLASS_INPUT_OUTPUT,
                    screen->root_visual, value_mask, &value_list);
  xcb_map_window(conn, window_id);

  xcb_flush(conn);

  // generic event takes all type of event.
  xcb_generic_event_t *evt;
  std::thread note;
  std::atomic<int> new_notehit;

  while ((evt = xcb_wait_for_event(conn))) {
    switch (evt->response_type) {
    case XCB_KEY_PRESS: {
      std::cout << "key is pressed"
                << (int)((xcb_key_press_event_t *)(evt))->detail << std::endl;

      switch ((int)((xcb_key_press_event_t *)(evt))->detail) {
      case 38: // F

        note = std::thread([]() { beep(349.23); });
        note.detach();
        break;
      case 39: // G
        std::thread([]() { beep(392); }).detach();
        break;
      case 40: // A
        std::thread([]() { beep(440); }).detach();
        break;
      case 41: // B
        std::thread([]() { beep(493.88); }).detach();
        break;
      case 42: // C
        std::thread([]() { beep(523.25); }).detach();
        break;
      case 43: // D
        std::thread([]() { beep(587.33); }).detach();
        break;
      case 44: // E
        std::thread([]() { beep(659.25); }).detach();
        break;
      case 45: // F
        std::thread([]() { beep(698.46); }).detach();
        break;

      default:
        std::thread([]() { beep(); }).detach();
      }

    } break;
    case XCB_BUTTON_PRESS:
      std::cout << "button pressed"
                << (int)((xcb_button_press_event_t *)(evt))->detail
                << std::endl;
      break;
    default:
      break;
    }
    free(evt);
  }

  xcb_disconnect(conn);

  /* -- Close and shutdown -- */
  ao_close(device);

  ao_shutdown();

  return 0;
}
