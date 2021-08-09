from collections import deque


class ActorScheduler:
    def __init__(self):
        self.actors = {}
        self._msg_queue = deque()

    def new_actor(self, name, actor):
        """ get new actor to schduler  """
        self._msg_queue.append((actor, None))
        self._actors[name] = actor

    def send(self, name, msg):
        """ send msg to actor """
        actor = self._actors.get(name)
        if actor:
            self._msg_queue.append((actor, msg))

    def run(self):
        while self._msg_queue:
            actor, msg = self._msg_queue.popleft()
            try:
                actor.send(msg)
            except StopIteration:
                pass
