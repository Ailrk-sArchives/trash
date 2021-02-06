// simulate context manager

interface Drop {
  drop: () => void,
}

class Client implements Drop {
  http: XMLHttpRequest;
  url: string;
  connection: Promise<string>;

  public Client(url: string) {
    this.url = url;
    this.http = new XMLHttpRequest();
    this.connection = this.connect();
  }

  private connect() {
    this.http.open("GET", this.url);
    this.http.send();
    return Promise.resolve(this.http.responseText)
  }

  public drop() {
    console.log("dropping");
  }
}

function raii<T extends Drop, U>(res: T) {
  return function (fn: (res: T) => U)  {
    const pResource = Promise.resolve(res);
    return pResource.then(fn)
      .finally(async () => {
        const resource = await pResource;
        return resource.drop();
      })
  }
}

@raii(new Client('http://www.google.com'))
function connect(res: Promise<string>) {
  res.then(response => console.log(response))
}
