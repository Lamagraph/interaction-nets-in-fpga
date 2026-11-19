.. _memory:

======
Memory
======

How the reduction core talks to the memory that holds the net, and why. Exact
types and signal timing are in the Haddock of ``Core.Memory``; where this page
and the code disagree, the code wins. Sections up to *Contract with the
allocator* describe what exists; *Planned design* describes what is decided but
not built.

Memory interface
================

The core must work with any memory without knowing how long it takes to answer.
So the core and the memory meet at one request/response interface with a
handshake, and the core infers nothing from elapsed cycles. The same interface is
meant for every level of a memory hierarchy, so levels can be stacked or replaced
without changing their neighbours. Everything that touches the net uses it: the
reduction core and the UART dump.

Rules (version 1):

1. A request is accepted in a cycle where it is present and ``ready`` is true. A
   client holds its request unchanged until it is accepted.
2. Every accepted request produces exactly one response, in request order.
3. At most one transaction is in flight. ``ready`` is false while a response is
   being delivered and during reset; requests presented then are ignored.
4. A response comes no earlier than the cycle after acceptance, writes included.
   The client must always consume it: there is no response-side backpressure.
5. A write acknowledgement means that a subsequent read observes the write.
6. Reset cancels the outstanding transaction. It does not restore memory
   contents, so restarting a reduction from the initial allocator state requires
   reloading the initial net.

The core uses ``Maybe Node`` as the cell. An empty cell is **data** (a read
returns "empty"), not the absence of a response; otherwise a client would wait
forever after reading a freed address.

Version 1 allows one request in flight. The core used to read one cell and write
another in the same cycle; that now takes two transactions, so reductions cost
more cycles. Widening the interface (a request carrying both a read and a write)
is possible if measurements show this matters, at the price of a more complex
cache.

Core behaviour
==============

A memory access is a pair of steps: issue the request and hold it until it is
accepted, then wait for the response, ignoring any response of another kind. The
core never proceeds on timing alone.

For one active pair the order is fixed:

1. read both active nodes;
2. clear both cells with acknowledged writes;
3. reduce;
4. for each external neighbour, read it and write it back with the recorded
   changes applied;
5. write the newly allocated nodes.

Clearing precedes every other write because the allocator can reuse the
addresses of the pair that was just consumed. Reduction is where addresses are
allocated, and it happens after the clears are acknowledged, so a reused address
is never overwritten by a stale clear.

Completion is a separate, terminal state entered when the allocator has no active
pair left. It raises ``done`` only after the last write is acknowledged, and stays
raised. Updating the root address during a reduction is not completion:
neighbours and new nodes may still need writing.

Ownership of the memory port
============================

``Core.Core.logicBoard`` selects the memory backend and connects it to the core.
The core keeps its logical address type; conversion to a block-RAM index is local
to this composition and is not part of the reduction algorithm.

The core owns the memory port until ``done``; the dump owns it afterwards. The two
never run together, so no arbiter is needed. A second client (for example an
allocator that keeps its free list in memory) would require one.

The dump uses the same interface, not a second raw RAM port. It reads cells in
ascending address order, skips empty cells, and holds each populated cell on its
output until the consumer reports it as transmitted. The UART reports that only
when the final byte of the node has been accepted, not when its byte counter
reaches the last index. A dump is one pass per reset and does not wrap.

Contract with the allocator
===========================

The allocator is developed separately. The memory side gives it:

- one linear address space of cells ``0 … N−1``, accessed the same way wherever
  a cell physically lives;
- data correctness: a read returns the last value written.

The allocator must not know about caches, DDR3, lines or slots.

``MemoryManager`` is currently a placeholder inside the core state, and it does
not scale: it keeps one bit per cell in registers and finds a free cell by
scanning. It works while the cell count is small, and it must be replaced before
the address space grows to external-memory size.

Possible extensions, to be agreed with the allocator's author: a hint to the
cache that an address was just allocated and need not be fetched, and allocator
access to memory as a second client.

Planned design
==============

Not implemented. Two levels are assumed: external DDR3 holds the net, block RAM
is a first-level cache. The interface above is the boundary between levels.

Granularity
-----------

Between the core and the first memory the unit is one cell. Between the cache and
DDR3 the unit is a line, several cells transferred together because DDR3 works in
bursts.

Cache
-----

Version 1 is direct-mapped, write-back, write-allocate and blocking, with one
request in flight; line size and slot count are parameters.

- A **line** is several consecutive cells, at least one DDR3 burst. A **slot**
  holds one line in block RAM. The **index** (derived from the address) selects
  the slot; the **tag** (high address bits) says which line is there; the
  **offset** selects the cell. **valid** means the slot holds something;
  **dirty** means it was modified and external memory is stale.
- Splitting an address is free (wiring). For a 20-bit address, 4 cells per line
  and 256 slots: tag bits 19–10, index 9–2, offset 1–0. Two addresses with the
  same index evict each other; associativity can be added if measurements show
  many conflicts.
- **Read:** tag match with valid is a hit and answers at once. Otherwise a miss:
  a valid and dirty slot is first written down (write-back), then the wanted line
  is read (fill). The core waits.
- **Write:** on a hit write the slot and set dirty; nothing goes down. On a miss
  fill first, then write.
- **flush** writes all dirty lines down (before dumping the result); **reset**
  clears valid. Valid bits live in registers so reset takes one cycle; block RAM
  is never cleared, a slot is overwritten on a miss.
