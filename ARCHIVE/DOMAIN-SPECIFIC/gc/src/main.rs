mod mark_sweep;

// == GC ==
// The goal of an ideal garbage collector is to reclaim the space used by every object that will no longer be used by the program.

// === Big Idea ===
// - liveness is a global property, but free is a local action. This is why manual memory management i so hard.
// - Well designed programs are built from components that are highly cohesive and loosely coupled.
// - GC decouple the problem of memory manage instead of scatter it througout the code.
// - We care about the reachability of an object. If an object is not reachable, the gc will reclaim it's space.

// === GC algorithm criterions ===
// - safty: You don't want to reclaim live objects.
// - throughput: Overall time spent on gc should be as low as possible. That is gc happens as less frequent as possible.
// - completeness: Garbages need to evenutally be collected. Not necessarily instant because in some cases defer the collection of some garbages imporve the gc performances (young old generations).
// - pause time: of course shorter the better.

// === Some terms ===
// - mutator: application program. mutate the object graph
// - collector: collect unreachable nodes.
// - allocator: The C++ sorts.

// === Fundamental GC schemes ===
// 1. Mark and sweep
// 2. Copying collection
// 3. mark compact
// 4. reference counting

fn main() {
    println!("Hello, world!");
}
