import http from 'http';

export function echoServerPrimitive() {
  http.createServer((req, res) => {
    if (req.method === 'POST' && req.url === '/echo') {
      let body: Array<Uint8Array> = [];
      req.on('data', chunk => {
        body.push(chunk);
      }).on('end', () => {
        res.end(Buffer.concat(body).toString());
      });
    } else {
      res.statusCode = 404;
      res.end();
    }
  }).listen(8080);
}


export function echoServer() {
  http.createServer((req, res) => {
    // handle error by registering error event
    req.on('error', err => {
      console.error(err);
      res.statusCode = 400;
      res.end();
    });
    if (req.method === 'POST' && req.url === '/echo') {
      // because req is ReadableStream and res is WritableStream
      // you can just pipe from one to the other.
      req.pipe(res);
    } else {
      res.statusCode = 404;
      res.end();
    }
  })
}
