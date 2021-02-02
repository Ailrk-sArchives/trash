/*
 * Deadlock:
 *  1. deadlock: several threads compete for a finite num of resources
 *     at same time
 *  2. deadlock prevention: algorithms check availability to avoid deadlock.
 *  3. deadlock detection: find instances of deadlock when thread stop making
 *     progress.
 *  4. starvation: thread waiting indefinitely for some resources.
 *     (different from wait for each other)
 *
 * Necessary condition for deadlock:
 *  1. Mutual exclusion
 *  2. Hold and wait
 *  3. No preemption
 *  4. circular wait. threads {t1 ... tn} where t1 -> ti ... tn -> t1
 *
 * Detect deadlock
 *  1. Scane resource allocation graph and then break the cycles.

 */


