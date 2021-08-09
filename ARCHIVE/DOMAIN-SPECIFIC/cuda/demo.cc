#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
// CPU can faster than GPU in sequential code
// GPU can faster than CPU in parallel code

/*  CUDA terminology
    Host: the CPU
    Device: a coprocessor to CPU (GPU)
        has its own DRAM (device memory)
        run many threads in parallel
    Kernal Code: data-parallel portions of an application run on device
 */

// Serial part in rost C code compiled by host compiler (gcc)
// Highly parallel part in device SPMD kernal C code compiled by NVIDIA compiler
#define __global__ // called from cpu, execute on device
#define __device__ // cannot be called from cpu
#define __host__   // called and exe on cpu
#define __const__  // declare on constant memeory
#define __shared__ // in shared memory
#define __syncthreads()
#define __threadfence()

typedef struct {
  int x;
  int y;
  int z;
} ThreadId;

typedef struct {
  int x;
  int y;
  int z;
} BlockId;

typedef struct {
  int x;
  int y;
  int z;
} GridDim;

typedef struct {
  int x;
  int y;
  int z;
} BlockDim;

typedef struct {
  int x;
  int y;
  int z;
} Dim3;

Dim3 dim3(int, int, int);

ThreadId threadIdx;
BlockId blockIdx;
GridDim gridDim;
BlockDim blockDim;

typedef enum {
  cudaMemcpyHostToDevice,
  cudaMemcpyDeviceToHost,
  cudaMemcpyDeviceToDevice,
} CudaDirection;

typedef enum {
  cudaSuccess = 0,
  cudaErrorInvalidValue,
  cudaErrorMemoryAllocation,
  cudaErrorInitializationError,
  cudaErrorCudartUnloading,
  cudaErrorSyncronizatrrionError = 25,
} CudaError_t;

CudaError_t cudaMalloc(void **d_ptr, size_t);
CudaError_t cudaMemcpy(void *dst, void *src, size_t, CudaDirection);
CudaError_t cudaFree(void *);
CudaError_t cudaMemSet(void *d_ptr, int value, size_t n);
CudaError_t cudaConfigCall(size_t gridDim, size_t blockDim);
CudaError_t cudaGetLastError();
CudaError_t cudaDeviceSynchorize();
void atomicAdd(void *, int);
int atomicCAS(int *, int, int);
int atomicExch(int *, int);
/*  Array of Threads
    A CUDA kernal is executed by an array of threads
    SPMD (single program multiple data)
    thread ID are used to
        decide what data to work on
        make control decision
 */

// Kernal function for worker on single thread.
__global__ void foo(int a, int b) {}

/* ------------- */
//  Sample HOST code
//  device copy the memory from host.
/* ------------- */
void sample_host_code() {
  int *a = 0, *da = 0, num_bytes = 200;
  a = (int *)malloc(num_bytes);

  cudaMalloc((void **)&da, num_bytes);
  cudaMemcpy(da, a, num_bytes, cudaMemcpyHostToDevice);

  // do something here.

  cudaMemcpy(a, da, num_bytes, cudaMemcpyDeviceToHost);

  cudaFree(da);
  free(a);
}
#define CUDA_CHECK(err)                                                        \
  if (err != cudaSuccess) {                                                    \
    printf("cuda memory allocation error");                                    \
    printf("Error%d; %s: %d", err, __FILE__, __LINE__);                        \
    printf(cudaGetErrorString(err));                                           \
    cudaDeviceReset();                                                         \
    exit(-1);                                                                  \
  }

/* ------------- */
//   __global__ add
/* ------------- */

__global__ void add(int a, int b, int *c) { // runs on device
  *c = a + b;
}
void parallel_add() {
  int c;
  int *dc;
  cudaMalloc((void **)&dc, sizeof(int));
  add<<<1, 1>>>(2, 7, d_c);
  cudaMemcpy(&c, dc, sizeof(int), cudaMemcpyDeviceToHost);
  cudaFree(dc);
  printf("2 + 7 = %d", c);
}

/* ------------- */
//   __global__ vector addition
/* ------------- */
#define N 1024
#define INIT(a, n)                                                             \
  for (int i_ = 0; i_ < n; ++i) {                                              \
    a[i] = rand() % N;                                                         \
  }

#define INT_VEC_ALLOC(a, n)                                                    \
  a = (int *)malloc(sizeof(int) * n) if (a == NULL) {                          \
    printf("memory allocation failed");                                        \
    exit(-1);                                                                  \
  }

__global__ void vec_add(int *a, int *b, int *c, int n) { // runs on device
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < n)
    c[i] = a[i] + b[i];
}

void parallel_vec_square() {
  int *a, *b; // also use b as output.
  int *da, *db;

  // calculate kernal config
  int nThreadPerBlock = 256;
  int nBlocks = N / nThreadPerBlock;
  if (N % nThreadPerBlock)
    nBlocks++;

  INT_VEC_ALLOC(a, N)
  INT_VEC_ALLOC(b, N)

  cudaMalloc((void **)da, N * sizeof(int));
  cudaMalloc((void **)db, N * sizeof(int));
  INIT(a, N)
  CUDA_CHECK(cudaMemcpy(da, a, N * sizeof(int), cudaMemcpyHostToDevice));
  CUDA_CHECK(cudaMemcpy(db, da, N * sizeof(int), cudaMemcpyDeviceToDevice));

  vec_add<<<nBlocks, nThreadPerBlock>>>(da, db, db, N);

  CUDA_CHECK(cudaMemcpy(b, db, N * sizeof(int), cudaMemcpyDeviceToHost));

  CUDA_CHECK(cudaGetLastError());
  CUDA_CHECK(cudaDeviceSynchorize());

  for (int i = 0; i < N; ++i)
    assert(a[i] == b[i] * b[i]);

  free(a);
  free(b);
  cudaFree(da);
  cudaFree(db);
}

/* ------------- */
// GPU design:
//  massively threaded. sustains 1000s of threads per app
// Software: grid and blocks
// Hardware:
//  SM: streaming multiprocessor
//  SP: streaming processor

// kernalFunc<<<gridSize, blockSize>>>()
// we can have as many blocks as we want
// each block can have maximum 1024 threads.

// CHOSING LUANCH CONFIGURATION
// KerFunc<<<gridSize, blockSize>>>(),
// blocks = (N-1) / (nthreads_per_block + 1)
// Goal: Threads per block should be large
// Blcok should be >= # of SM

/* ------------- */
// HIGHER DIMENSIONAL GRIDS / BLOCKS
// process 100 x 70 picture
/* ------------- */
#define WIDTH 100
#define HEIGHT 70
#define TILEWIDTH = 32
#define TILEHEIGHT = 32
__global__ void pickernal(float *dpin, float *dpout, int w, int h) {
  int y = blockIdx.y * blockDim.y + threadIdx.y;
  int x = blockIdx.x * blockDim.x + threadIdx.x;

  if ((y < h) && (x < w))
    dpout[y * w + x] = sin(dpin[y * w + x]) * 0.4;
}
// parallel kernal pic
void parallel_kernal_pic() {
  dim3 blocksize(WIDTH, HEIGHT);
  int nblk_x = (WIDTH - 1) / (TILEWIDTH - 1);
  int nblk_y = (HEIGHT - 1) / (TILEHEIGHT - 1);
  dim3 gridsize(nblk_x, nblk_y);

  float *x, *dx; // image
  pickernal<<<gridsize, blocksize>>>(x, dx, WIDTH, HEIGHT);
  cudaDeviceSynchorize(); // force printf in device ot flush here.
  printf("that's all");
  // ... memcpy stuffs.
}

/* ------------- */
// cuda limits.
// within a block maximum # of thread per block = 1024
// maximum dimension of a block 1024 / 1024/ 64
/* ------------- */

/* ------------- */
// Thread Cooperation
// Thread in same block can cooperate.
// communicate via shared memory, atomic operation and barrier synchronization.
// thread # used to assing works.
// threads can execute in any order.
/* ------------- */

#undef INIT
#undef INT_VEC_ALLOC
#undef N

/* ------------- */
//  PERFORMANCE and MEMORY
/* ------------- */

/* ------------- */
// Compute to Global Memory Access
// goal increase CGMA. More computation less IO.
/* ------------- */

/* ------------- */
//  Types of memory
// Programmable
//      register, shared memory, local memory, constant memory, global memory
// Non-programmable
//      L1, L2 Cache
/* ------------- */
// -----------------------
//         GRID
// -----------------------
//  BLOCK0,0 |  BLOCK0,1
// --------- | -----------
// SharedMem |  SharedMem
//  t, t, t..|  t, t, t..
//  r  r  r  |  r  r  r
// -----------------------
//       Global Memory
// -----------------------
//       Constant Memory
// -----------------------

// CUDA variables
// int x;               register | onchip mem | thread scope thread lifetime |
// very fast int array[10];       localmem | offchipmem | thread scope thread
// lifetime | slow
// __shared__ int x;    sharedmem|  onchip    | block scope block lifetime   |
// fast
// __device__ int x;    global   |  offchip   | grid+host scope,app lifetime |
// slow
// __const__ int x;     const    |  offchip   | grid+host scope,app lifetime |
// fast

// automatic variable:
// __local__ int a[10];
// automatic scalar var in register, automatic arrays resides in local memory by
// default

// -----------------------
//      Example of memory access
// -----------------------
#define N 1024
#define GRID_SZ = 4
#define BLOCK_SZ = N / GRID_SZ
__device__ float glb;    // in global mem
__const__ float C = 1.2; // in constant mem

__global__ void foo1(float *array, int len) {}
__device__ void helper() {}

__global__ void foo2(float *array, int len) {
  int i = threadIdx.x;    // in register
  float a1[100];          // often in local mem
  __shared__ float sh;    // block scope shared mem
  __shared__ float a2[N]; // same as sh.
  float *p_glb = &glb;    // pointer is in register. point to mem in global mem.
  const float *p_C = &C;
}
#undef N

// -----------------------
//      Memory tiling
//      utilize shared memory
//      reduce global memory access
// -----------------------
#define TILE_WIDTH = 32
__global__ void matrix_mul(float *m, float *n, float *p, int width) {
  __shared__ float ms[TILEWIDTH][TILEWIDTH];
  __shared__ float ns[TILEWIDTH][TILEWIDTH];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;
  int y = by * TILEWIDTH + ty;
  int x = bx * TILEWIDTH + tx;

  // identify row and column of p element to work on.
  float value = 0;

  // loop over m and n tiles requried to compute p element
  int num_phases = width / TILEWIDTH;
  for (int ph = 0; ph < TILEWIDTH; ph++) {
    ms[ty][tx] = m[y * width + (ph * TILEWIDTH + tx)];
    ns[ty][tx] = n[(ty + ph * TILEWIDTH) * width + x];

    __syncthreads(); // thread sync within the same block.
    for (int k = 0; k < TILEWIDTH; k++) {
      value += ms[ty][k] * ns[k][tx];
    }
    __syncthreads();
  }
  p[y * width + x] = value;
}

#undef TILEWIDTH

// -----------------------
//      Avoid data race
// -----------------------
__global__ void foo() {
  __shared__ int array[100];
  int i = threadIdx.x;
  int temp;
  array[i] = i;
  __syncthreads(); // data is written first before any reads it.
  if (i > 1 && i < 100)
    temp = array[i - 1]; // ensure reading original data.
  __syncthreads();
  if (i > 1 && i < 100)
    array[i] = temp; // if other reads / writes are done after that.
  __syncthreads();
  // ...
}

// -----------------------
//      Atomic operation
//      Slower. because of serilize dexecution of threads.
//      No specific order.
//      Only certain operations are support.
//      Only int support for most operations.
// -----------------------

#define NUMTHREADS 10000
#define BLOCK_WIDTH 63
__global__ void increment_atomic(int *x) { atomicAdd(x, 1); }

void parallel_inc() {
  int *hx, *dx;
  cudaMalloc((void **)&dx, sizeof(int));
  cudaMemSet(dx, 0, sizeof(int));
  increment_atomic<<<NUMTHREADS / BLOCK_WIDTH, BLOCK_WIDTH>>>(dx);
  cudaMemcpy((void **)&hx, dx, sizeof(dx), cudaMemcpyDeviceToHost);
  cudaDeviceSynchorize();
  printf("x = %d\n", hx);
  free(hx);
  cudaFree(dx);
}
#undef NUMTHREADS
#undef BLOCK_WIDTH

// -----------------------
//      Critical section / lock
// -----------------------
struct Lock {
  int *mutex;
  Lock() {
    cudaMalloc((void **)&mutex, sizeof(int));
    cudaMemSet(mutex, 0, sizeof(int));
  }
  ~Lock() { cudaFree(mutex); }
  __device__ void lock() {
    while (atomicCAS(mutex, 0, 1) != 0)
      ;
    __threadfence();
  }
  __device__ void unlock() {
    atomicExch(mutex, 0);
    __threadfence();
  }
};

// -----------------------
//      Thread scheduling.
// -----------------------
// Transparent scalability:
//      GPU responsible for assigning thread blocks to SMs.
//      threads in the same block might cooperate.
//

#define Q9
__device__ void brighten(int *pixel) {
  *pixel *= 2;
  if (*pixel > 255)
    *pixel = 255;
}
__global__ void process(int *image, int width, int height) {
  int y = blockIdx.y * blockDim.y + threadIdx.y;
  int x = blockIdx.x * blockDim.x + threadIdx.x;
  if ((y < height) && (x < width))
    brighten(&image[y * width + x]);
}
int main(void) {
  // read an image along with its width and height - don't parallelize this part
  int *image;
  int *image_d;
  int width, height;
  const int TILE_WIDTH = 32;
  readImage("name.bmp", image, &width, &height);
  cudaMalloc(&image_d, sizeof(int) * width * height);
  cudaMemcpy(image_d, image, sizeof(int) * width * height,
             cudaMemcpyHostToDevice);
  dim3 blocksize(TILE_WIDTH, TILE_WIDTH);
  int nblk_x = (width - 1) / (TILE_WIDTH - 1);
  int nblk_y = (height - 1) / (TILE_WIDTH - 1);
  dim3 gridsize(nblk_x, nblk_y);
  // process the image - you should run this part on the GPU
  process<<<gridsize, blocksize>>>(image, width, height);
  cudaMemcpy(image, image_d, sizeof(int) * width * height,
             cudaMemcpyDeviceToHost);

  // save results - don't parallelize this part
  saveImage(image);
  free(image), cudaFree(image_d);
  return 0;
}
#undef Q9

#define Q10
__shared__ float min = 100000.0f; // assume max value in f is < 100,000
__device__ float partial_min[32];

__device__ float f(float x,
                   float y) { // Function is visualized in the plot above
  return x * x * (4 - 2 * x * x + 0.3f * (x * x * x * x)) + x * y +
         y * y * (-4 + 0.4f * y * y);
}
__global__ float getmin(float xa, float xb, float ya, float yb, int n) {
  float hx = (xb - xa) / n; // assume xb > xa
  float hy = (yb - ya) / n; // assume yb > ya

  // calculate the index.
  int yi = blockIdx.y * blockDim.y + threadIdx.y;
  int xi = blockIdx.x * blockDim.x + threadIdx.x;
  // calculate the coordinate
  yi = yi * hy;
  xi = xi * hx;

  if (min > f(xi, yi) && (yi >= ya && yi <= yb) && (xi >= xa && xi <= xb))
    min = f(xi, yi);
  __syncthreads();
  partial_min[xi] = min;
  __syncthreads();
  return min;
}

int main() {
  getmin<<<32, 32>>>(-10, 10, -10, 10, 1024);
  float *partial_min_main;
  cudaMemcpy(partial_min_main, partial_min, sizeof(float) * 32,
             cudaMemcpyDeviceToHost);

  // final step host collect data from blocks
  float min = 100000.0f;
  for (int i = 0; i < 32; i++) {
    if (min > partial_min[i])
      min = partial_min[i];
  }
  free(partial_min);
  cudaFree(partial_min_main);
  printf("The min value is %.2f\n", min); // OUTPUT: The min value is -11.96
  return 0;
}
#undef Q10

#define Q11
#include <stdio.h>
#define WIDTH 5 // Size of matrices

int main() {
  // allocate space for M and N on the heap and initialize them
  int *M = malloc(WIDTH * WIDTH * sizeof(int));
  int *N = malloc(WIDTH * WIDTH * sizeof(int));
  read(M);
  read(N);
  // compute MxN and find min – note that we don’t need to save the full matrix
  // resulting from MxN

  MPI_Init(NULL, NULL);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
  MPI_Comm_size(MPI_COMM_WORLD, &comm_sz);

  int my_rank, comm_sz, my_n, source;
  if ()
    if (my_rank != 0) { // send my partial result to process 0
      MPI_Send(&my_sum, 1, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD);
    } else { // process 0 combines the partial results

      printf("Hi from process %dof%d!\n", my_rank, comm_sz); // Print my message
      for (int q = 1; q < comm_sz; q++) { // Print others’ messages
        MPI_Recv(msg, LEN, MPI_CHAR, q, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        printf("%s\n", msg);
      }
      total_sum = my_sum;
      for (source = 1; source < comm_sz; source++) {
        MPI_Recv(&my_sum, 1, MPI_DOUBLE, source, 0, MPI_COMM_WORLD,
                 MPI_STATUS_IGNORE);
        total_sum += my_sum;
      }
    }
  if (my_rank == 0)
    printf("%.15e\n", total_sum);

  int min = -1;
  for (int r = 0; r < WIDTH; r++)
    for (int c = 0; c < WIDTH; c++) {
      // compute value of MxN at (r,c)
      int value = 0;
      for (int k = 0; k < WIDTH; k++)
        value += M[r + WIDTH * k] * N[k + WIDTH * c];
      // find min
      if (min < value)
        min = value;
    }
  // print results
  printf("%d", min);
  return 0;
}
#undef Q11

#include <stdio.h>
#define WIDTH 5 // Size of matrices

int main() {
  // allocate space for M and N on the heap and initialize them
  int *M = malloc(WIDTH * WIDTH * sizeof(int));
  int *N = malloc(WIDTH * WIDTH * sizeof(int));
  read(M);
  read(N);
  MPI_Init(NULL, NULL);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
  MPI_Comm_size(MPI_COMM_WORLD, &comm_sz);
  int my_rank, comm_sz, my_n, source;

  if (my_rank == 0) {
    // scatter rows of first matrix to different processes
    MPI_Scatter(a, WIDTH * WIDTH / sizeof(int), MPI_INT, aa, N * N / size,
                MPI_INT, 0, MPI_COMM_WORLD);

  } else {
    MPI_Recv(&my_sum, 1, MPI_INT, source, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  }

  if (my_rank == 0) {
  }
  MPI_Finalize();
  return 0

         // compute MxN and find min – note that we don’t need to save the full
         // matrix resulting from MxN
         int min = -1;
  for (int r = 0; r < WIDTH; r++)
    for (int c = 0; c < WIDTH; c++) {
      // compute value of MxN at (r,c)
      int value = 0;
      for (int k = 0; k < WIDTH; k++)
        value += M[r + WIDTH * k] * N[k + WIDTH * c];
      // find min
      if (min < value)
        min = value;
    }
  // print results
  printf("%d", min);
  return 0;
}
