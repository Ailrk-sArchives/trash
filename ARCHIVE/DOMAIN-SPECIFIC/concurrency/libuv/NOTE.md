# libuv

#### Intro
Normally when we call a system call for performing IO, like reading a file or receiving from a socket, the process will just wait there and doing nothing. In this case, IO blocks the process.

But it's not necessary to block. We can spawn another thread to do these IO tasks. Once the task is spawned, we can go ahead to do other things. When IO is finished, we then come back to fetch the result. In this case, IO performed asynchronously -- it doesn't block the main thread. (actually we don't need to spawn thread ourself, the idea is someone that's not us does the job eventually. This point is not elaborated at all in most nodejs docs, which is super weird. How can a single threaded program performing IO while doing other thing?)

But how do we perform the task? How do we get notified when the task is finished?

A scheme to achieve this is to use an event loop.

#### Motivation
Imagine we want to read a file from the disk. The file is large and takes 1 minute to read. Ideally we want to say: somebody read the file, when it's done, notify me. And then we go ahead to do other things (presumably use the result, the point is now we have the control over the result).

In this scenario, we expect to get the result from someone else who were performing the IO task and then choose what's next with the result.

For an event loop it is generally the same, but other then just informing us after someone finished the task, we provide a callback ahead of time -- our plan on how to use resource. Once the task is done, the "somebody" will not only inform us it's done, it will also tells us what we planned to do with the task.

So now the story becomes: I want to read the file, thread you can do it for me. After you're done, I'm going to do use the file to do xxx. Thread1 says ok, and after it's done it will reply to us: Hey main thread I'm done, this is the result, and you said you're gonna use the result to do xxx. And we go: oh yes, I'll do that right away.

Some terms: The somebody is a watcher, it watches on one event, and notify us with the callback we provided. When an event occurs, the watcher will hand us over a tuple of callback and it's argument so we can call.  e.g (callback, (arg1)). To perform the task just do something like `(funcall callback arg)`

#### Scheduling

We know we have this watcher thing to watch for some events to happen and gives us the callable tuple (a task). But who performs the event? We can say each time we want to perform some event, we create a new watcher. Once a watcher is triggered by an event, it will need to do something to let the main thread to get the task. Hypothetically it can just set a flag, the main thread check the flag periodically, if it's set, grab the task and call it.

But what happens when there are multiple watchers? Different watchers may finish in different order and get queued up waiting for the main thread to execute. The thing is the order for task to arrive doesn't matter, we can put all tasks in a queue and let the main thread perform them sequentially.

The queue is used for communication between watchers and the main thread, but no lock required because order doesn't matter.

#### Why register callback instead of decide what to do later?

We already saw in libuv we register a callback to handle the result. But why not just receive the result and do whatever afterwards?

We can think the watcher takes the continuation of the rest of our main thread. It not really because while it's waiting for task done the main thread is free to whatever. But once its' done it will invoke the continuation pass in earlier, our main thread will continue by the callback setup earlier.

One noticeable difference from normal continuation is that now multiple watchers takes the continuation at the same time. They run task on another thread (not necessarily but they can), and don't invoke the continuation right ahead. Instead they put the continuation on the queue and let the main thread jump into the continuation by polling over the queue.
