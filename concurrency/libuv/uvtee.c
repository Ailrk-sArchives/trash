#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <uv.h>

// uv_write_t + the buffer.
// we cast a normal uv_write_t to this type to associate buffer with it.
typedef struct {
  uv_write_t req;
  uv_buf_t buf;
} write_req_t;

uv_loop_t *loop;

// we have three pipes at hands.
uv_pipe_t stdin_pipe;
uv_pipe_t stdout_pipe;
uv_pipe_t file_pipe;

// initialize buffer.
void alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
  char *mem = (char *)malloc(suggested_size);
  *buf = uv_buf_init(mem, suggested_size);
}

// free uv_write_t and the buffer associated with it.
void free_write_req(uv_write_t *req) {
  write_req_t *wr = (write_req_t *)req;
  free(wr->buf.base);
  free(wr);
}

// when write finished just close resources.
void on_stdout_write(uv_write_t *req, int status) { free_write_req(req); }
void on_file_write(uv_write_t *req, int status) { free_write_req(req); }

// copy the data from buf to dest.
void write_data(uv_stream_t *dest, ssize_t size, uv_buf_t buf, uv_write_cb cb) {
  write_req_t *req = (write_req_t *)malloc(sizeof(write_req_t));
  char *mem = (char *)malloc(size);
  req->buf = uv_buf_init(mem, size);
  memcpy(req->buf.base, buf.base, size);

  uv_write((uv_write_t *)req, (uv_stream_t *)dest, &req->buf, 1, cb);
}

void read_stdin(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
  if (nread < 0) {
    if (nread == UV_EOF) {
      // end of file
      uv_close((uv_handle_t *)&stdin_pipe, NULL);
      uv_close((uv_handle_t *)&stdout_pipe, NULL);
      uv_close((uv_handle_t *)&file_pipe, NULL);
    }
  } else if (nread > 0) {
    /* write_data((uv_stream_t *)&stdout_pipe, nread, *buf, on_stdout_write); */
    write_data((uv_stream_t *)&stdout_pipe, nread, *buf, on_stdout_write);
    write_data((uv_stream_t *)&file_pipe, nread, *buf, on_file_write);
  }

  if (buf->base) {
    free(buf->base);
  }
}

int main(int argc, char *argv[]) {
  loop = uv_default_loop();

  uv_pipe_init(loop, &stdin_pipe, 0);
  uv_pipe_open(&stdin_pipe, 0);

  uv_pipe_init(loop, &stdout_pipe, 0);
  uv_pipe_open(&stdout_pipe, 1);

  uv_fs_t file_req;
  int fd = uv_fs_open(loop, &file_req, argv[1], O_CREAT | O_RDWR, 0644, NULL);

  uv_pipe_init(loop, &file_pipe, 0);
  uv_pipe_open(&file_pipe, 0);

  uv_read_start((uv_stream_t *)&stdin_pipe, alloc_buffer, read_stdin);

  uv_run(loop, UV_RUN_DEFAULT);
  return 0;
}
