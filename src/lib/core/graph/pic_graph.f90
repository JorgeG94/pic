! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Graph traversal and shortest-path algorithms over CSR adjacency.
module pic_graph
   !! Shortest-path and traversal algorithms on a weighted directed graph
   !! stored as a `csr_t` from `pic_csr`.
   !!
   !! A graph on `n` vertices is a square `n` by `n` `csr_t`: row `u` holds
   !! one entry per outgoing edge of `u`, the column index is the head vertex
   !! and the value is the edge weight. Vertices are numbered `1:n`. An
   !! undirected graph is the symmetric case, stored with both directions.
   !!
   !! ### Reproducibility guarantee
   !!
   !! This is the property that makes the module worth having. `pic_heap`
   !! guarantees that entries with equal keys pop in FIFO order, so
   !! equal-distance vertices settle in a deterministic order rather than in
   !! whatever order a heap happens to rebalance into. On top of that,
   !! `dijkstra` relaxes each vertex's outgoing edges in ascending column
   !! order (CSR rows are sorted), and accepts a relaxation only on a strict
   !! improvement. Together those make the **entire output a pure function of
   !! the input**: not only `dist`, which is forced by the mathematics, but
   !! also `prev` and therefore the reconstructed path when several shortest
   !! paths tie. The same graph gives the same tie-broken path on every
   !! compiler and every run. `test_pic_graph` pins this with a graph that has
   !! two equal-cost shortest paths and asserts which one wins.
   !!
   !! Concretely, when two shortest paths to a vertex tie, the predecessor
   !! that is *discovered first in settle order* wins, and settle order among
   !! equal distances is the order in which those vertices were pushed.
   !!
   !! ### Sentinels
   !!
   !! * `GRAPH_INFINITY` (`huge(1.0_dp)`) is the distance of a vertex not
   !!   reachable from the source.
   !! * `GRAPH_NO_PREDECESSOR` (0) is the `prev` entry of the source and of
   !!   every unreachable vertex; 0 is not a valid vertex index.
   !! * `GRAPH_UNREACHABLE` (-1) is the `bfs` hop count of an unreachable
   !!   vertex.
   !!
   !! ### Negative weights are rejected
   !!
   !! Dijkstra and A* are simply invalid on negative edge weights. Rather than
   !! quietly returning wrong distances, `dijkstra`, `a_star` and `path_cost`
   !! report `ERROR_VALIDATION` when any stored weight is negative. Use a
   !! Bellman-Ford style algorithm for graphs with negative edges.
   !!
   !! ### The priority queue and its key
   !!
   !! `heap_t` is keyed by `integer(int64)` while distances here are
   !! `real(dp)`, so distances are mapped to keys by `distance_key`, an
   !! order-preserving and injective encoding of non-negative doubles into
   !! `integer(int64)` (see its own documentation). Because it is injective,
   !! two vertices tie in the heap exactly when their distances are equal as
   !! reals, which is what makes the FIFO tie-break above meaningful.
   !!
   !! The queue is used with the **lazy deletion** pattern: an improved
   !! distance is pushed as a new entry rather than decreasing an existing
   !! key, and stale pops are skipped by testing whether the vertex has
   !! already settled. `heap_t` deliberately offers no `decrease_key`, and
   !! this is its intended usage.
   !!
   !! ### Usage
   !!
   !!```fortran
   !! type(csr_t) :: g
   !! type(error_t) :: err
   !! real(dp), allocatable :: dist(:)
   !! integer(default_int), allocatable :: prev(:)
   !!
   !! call dijkstra(g, 1_default_int, dist, prev, err)
   !!```
   use pic_types, only: default_int, dp, int32, int64
   use pic_error, only: error_t, ERROR_VALIDATION
   use pic_csr, only: csr_t
   use pic_heap, only: heap_t
   implicit none

   private

   public :: dijkstra
   public :: a_star
   public :: bfs
   public :: connected_components
   public :: path_cost
   public :: GRAPH_INFINITY, GRAPH_NO_PREDECESSOR, GRAPH_UNREACHABLE

   real(dp), parameter :: GRAPH_INFINITY = huge(1.0_dp)
      !! Distance reported for a vertex unreachable from the source
   integer(default_int), parameter :: GRAPH_NO_PREDECESSOR = 0
      !! `prev` entry meaning "no predecessor"; 0 is never a valid vertex
   integer(default_int), parameter :: GRAPH_UNREACHABLE = -1
      !! `bfs` hop count of a vertex unreachable from the source

   integer(default_int), parameter :: MANTISSA_BITS = digits(1.0_dp) - 1
      !! Explicit-mantissa bit count of `real(dp)`, 52 for IEEE binary64
   integer(int64), parameter :: MANTISSA_SCALE = 2_int64**MANTISSA_BITS
      !! `2**MANTISSA_BITS`, the weight of one unit in the exponent field
   integer(int64), parameter :: EXPONENT_BIAS = int(maxexponent(1.0_dp), int64) - 2_int64
      !! Bias turning `exponent(x)` into the stored exponent field, 1022 for binary64

contains

   pure function distance_key(x) result(key)
      !! Map a non-negative `real(dp)` onto an `integer(int64)` heap key,
      !! preserving order exactly.
      !!
      !! For `x > 0` this reproduces the IEEE-754 bit pattern of `x` read as a
      !! 64-bit integer, which is monotonically increasing in `x`; `x <= 0`
      !! maps to 0. It is computed from `exponent` and `fraction` rather than
      !! with `transfer`, deliberately: `transfer` between real and integer
      !! kinds behaves inconsistently on nvfortran and LFortran, while
      !! `exponent`/`fraction` are plain intrinsics everywhere. The arithmetic
      !! is exact - `fraction(x)*2 - 1` is exact by Sterbenz and scaling it by
      !! `2**MANTISSA_BITS` lands on an integer - so the mapping is injective
      !! and equal distances produce equal keys. That is what lets the heap's
      !! FIFO tie-breaking decide ties between genuinely equal distances.
      !!
      !! The largest key it can produce is `(2*maxexponent - 2)*2**52 +
      !! 2**52 - 1`, comfortably below `huge(0_int64)`, so no encoding of a
      !! finite `real(dp)` overflows.
      real(dp), intent(in) :: x
         !! Distance to encode; must be non-negative and finite
      integer(int64) :: key
         !! Order-preserving encoding of `x`

      if (x <= 0.0_dp) then
         key = 0_int64
         return
      end if

      key = (int(exponent(x), int64) + EXPONENT_BIAS)*MANTISSA_SCALE &
            + int((fraction(x)*2.0_dp - 1.0_dp)*real(MANTISSA_SCALE, dp), int64)
   end function distance_key

   subroutine dijkstra(graph, source, dist, prev, err)
      !! Single-source shortest paths with non-negative edge weights.
      !!
      !! `dist(v)` is the shortest distance from `source` to `v`, or
      !! `GRAPH_INFINITY` when `v` is unreachable. `prev(v)` is the
      !! predecessor of `v` on that shortest path, or `GRAPH_NO_PREDECESSOR`
      !! for the source and for unreachable vertices. Both outputs are
      !! allocated to the number of vertices.
      !!
      !! `prev` is deterministic: see the module documentation on
      !! reproducibility. Reports `ERROR_VALIDATION` for a malformed or
      !! non-square adjacency matrix, an out-of-range `source`, or any
      !! negative edge weight; in that case both outputs come back zero-size.
      type(csr_t), intent(in) :: graph
         !! Square weighted adjacency matrix of the graph
      integer(default_int), intent(in) :: source
         !! Source vertex, in `1:n`
      real(dp), allocatable, intent(out) :: dist(:)
         !! Shortest distance to each vertex
      integer(default_int), allocatable, intent(out) :: prev(:)
         !! Predecessor of each vertex on its shortest path
      type(error_t), intent(out), optional :: err

      type(heap_t) :: queue
      logical, allocatable :: settled(:)
      integer(default_int) :: n, u, v, k
      integer(int64) :: key
      integer(int32) :: payload
      real(dp) :: dist_u, candidate

      if (.not. check_graph(graph, source, err)) then
         allocate (dist(0))
         allocate (prev(0))
         return
      end if
      if (.not. check_weights(graph, err)) then
         allocate (dist(0))
         allocate (prev(0))
         return
      end if

      n = graph%n_rows()
      allocate (dist(n))
      allocate (prev(n))
      allocate (settled(n))
      dist = GRAPH_INFINITY
      prev = GRAPH_NO_PREDECESSOR
      settled = .false.

      dist(source) = 0.0_dp
      call queue%init(n)
      call queue%push(distance_key(0.0_dp), int(source, int32))

      do while (.not. queue%is_empty())
         call queue%pop(key, payload)
         u = int(payload, default_int)
         if (settled(u)) cycle
         settled(u) = .true.
         dist_u = dist(u)
         do k = graph%row_ptr(u), graph%row_ptr(u + 1) - 1
            v = graph%col_idx(k)
            candidate = dist_u + graph%values(k)
            if (candidate < dist(v)) then
               dist(v) = candidate
               prev(v) = u
               call queue%push(distance_key(candidate), int(v, int32))
            end if
         end do
      end do

      call queue%destroy()
   end subroutine dijkstra

   subroutine a_star(graph, source, target, heuristic, path, err)
      !! A* search for one shortest path from `source` to `target`.
      !!
      !! `heuristic(v)` is a **precomputed array** of lower bounds on the
      !! remaining distance from `v` to `target`. An array rather than a
      !! procedure argument is a deliberate portability choice: passing
      !! procedures through an abstract interface is the more general design,
      !! but procedure dummies are exactly the corner of the language where
      !! the compilers PIC targets differ most, and a realistic heuristic
      !! (straight-line distance to the target from stored coordinates) is
      !! computed once per search anyway, so generality buys little here.
      !!
      !! The search settles each vertex once and stops as soon as `target`
      !! settles, so the heuristic must be **consistent** (monotone):
      !! `heuristic(u) - heuristic(v) <= w(u, v)` for every edge. Consistency
      !! implies admissibility, and with it A* returns a path of exactly the
      !! same total cost as `dijkstra`. With `heuristic = 0` - trivially
      !! consistent - it degenerates to Dijkstra exactly, expanding vertices
      !! in the same order and returning the same tie-broken path. Both are
      !! asserted in the test suite.
      !!
      !! `path` is allocated to the vertices from `source` to `target`
      !! inclusive, so `path(1) == source` and `path(size(path)) == target`; a
      !! search with `source == target` yields the single-element path
      !! `[source]`. An unreachable target is **not** an error: `path` comes
      !! back zero-size. Reports `ERROR_VALIDATION` for a malformed graph, an
      !! out-of-range `source` or `target`, a `heuristic` whose length is not
      !! the number of vertices or which holds a negative entry, or a negative
      !! edge weight.
      type(csr_t), intent(in) :: graph
         !! Square weighted adjacency matrix of the graph
      integer(default_int), intent(in) :: source
         !! Start vertex, in `1:n`
      integer(default_int), intent(in) :: target
         !! Goal vertex, in `1:n`
      real(dp), intent(in) :: heuristic(:)
         !! Admissible lower bound on the distance from each vertex to `target`
      integer(default_int), allocatable, intent(out) :: path(:)
         !! Vertices from `source` to `target`, or zero-size if unreachable
      type(error_t), intent(out), optional :: err

      type(heap_t) :: queue
      logical, allocatable :: settled(:)
      real(dp), allocatable :: cost_so_far(:)
      integer(default_int), allocatable :: prev(:)
      integer(default_int) :: n, u, v, k
      integer(int64) :: key
      integer(int32) :: payload
      real(dp) :: cost_u, candidate
      logical :: reached

      allocate (path(0))
      if (.not. check_graph(graph, source, err)) return
      if (.not. check_weights(graph, err)) return

      n = graph%n_rows()
      if (target < 1 .or. target > n) then
         call fail(err, "pic_graph: a_star target vertex is outside 1:n")
         return
      end if
      if (int(size(heuristic), default_int) /= n) then
         call fail(err, "pic_graph: a_star heuristic must have one entry per vertex")
         return
      end if
      do k = 1, n
         if (heuristic(k) < 0.0_dp) then
            call fail(err, "pic_graph: a_star heuristic must be non-negative")
            return
         end if
      end do

      allocate (cost_so_far(n))
      allocate (prev(n))
      allocate (settled(n))
      cost_so_far = GRAPH_INFINITY
      prev = GRAPH_NO_PREDECESSOR
      settled = .false.

      cost_so_far(source) = 0.0_dp
      reached = .false.
      call queue%init(n)
      call queue%push(distance_key(heuristic(source)), int(source, int32))

      do while (.not. queue%is_empty())
         call queue%pop(key, payload)
         u = int(payload, default_int)
         if (settled(u)) cycle
         settled(u) = .true.
         if (u == target) then
            reached = .true.
            exit
         end if
         cost_u = cost_so_far(u)
         do k = graph%row_ptr(u), graph%row_ptr(u + 1) - 1
            v = graph%col_idx(k)
            candidate = cost_u + graph%values(k)
            if (candidate < cost_so_far(v)) then
               cost_so_far(v) = candidate
               prev(v) = u
               call queue%push(distance_key(candidate + heuristic(v)), int(v, int32))
            end if
         end do
      end do

      call queue%destroy()
      if (reached) then
         deallocate (path)
         call trace_back(prev, source, target, path)
      end if
   end subroutine a_star

   pure subroutine trace_back(prev, source, target, path)
      !! Walk `prev` from `target` back to `source` and return it forwards.
      integer(default_int), intent(in) :: prev(:)
         !! Predecessor array, with `GRAPH_NO_PREDECESSOR` at the source
      integer(default_int), intent(in) :: source
         !! First vertex of the path
      integer(default_int), intent(in) :: target
         !! Last vertex of the path
      integer(default_int), allocatable, intent(out) :: path(:)
         !! `source` first, `target` last

      integer(default_int) :: n_hops, v, k

      n_hops = 1
      v = target
      do while (v /= source)
         v = prev(v)
         n_hops = n_hops + 1
      end do

      allocate (path(n_hops))
      v = target
      do k = n_hops, 1, -1
         path(k) = v
         v = prev(v)
      end do
   end subroutine trace_back

   subroutine path_cost(graph, path, cost, err)
      !! Total weight of walking `path` through `graph`.
      !!
      !! A zero-size path has cost `GRAPH_INFINITY` (there is no such walk); a
      !! single-vertex path has cost zero. Reports `ERROR_VALIDATION` when the
      !! graph is malformed or has negative weights, when a path entry is not
      !! a valid vertex, or when two consecutive entries are not joined by a
      !! stored edge.
      type(csr_t), intent(in) :: graph
         !! Square weighted adjacency matrix of the graph
      integer(default_int), intent(in) :: path(:)
         !! Vertices to walk, in order
      real(dp), intent(out) :: cost
         !! Sum of the weights of the traversed edges
      type(error_t), intent(out), optional :: err

      integer(default_int) :: n, i, k, u, v
      logical :: found

      cost = GRAPH_INFINITY
      if (int(size(path), default_int) == 0) return
      if (.not. check_graph(graph, path(1), err)) return
      if (.not. check_weights(graph, err)) return

      n = graph%n_rows()
      do i = 1, int(size(path), default_int)
         if (path(i) < 1 .or. path(i) > n) then
            call fail(err, "pic_graph: path_cost got a vertex outside 1:n")
            return
         end if
      end do

      cost = 0.0_dp
      do i = 1, int(size(path), default_int) - 1
         u = path(i)
         v = path(i + 1)
         found = .false.
         do k = graph%row_ptr(u), graph%row_ptr(u + 1) - 1
            if (graph%col_idx(k) == v) then
               cost = cost + graph%values(k)
               found = .true.
               exit
            end if
         end do
         if (.not. found) then
            call fail(err, "pic_graph: path_cost got consecutive vertices with no edge between them")
            cost = GRAPH_INFINITY
            return
         end if
      end do
   end subroutine path_cost

   subroutine bfs(graph, source, level, prev, err)
      !! Breadth-first search, ignoring edge weights.
      !!
      !! `level(v)` is the least number of edges from `source` to `v`, zero at
      !! the source and `GRAPH_UNREACHABLE` for a vertex that cannot be
      !! reached. `prev(v)` is the predecessor on that shortest hop path, or
      !! `GRAPH_NO_PREDECESSOR`. Because CSR rows are sorted ascending and the
      !! queue is FIFO, `prev` is deterministic here too: among equal-hop
      !! predecessors the one reached first in that order wins.
      !!
      !! Edge weights are not read at all, so negative weights are accepted.
      !! Reports `ERROR_VALIDATION` for a malformed or non-square graph or an
      !! out-of-range `source`.
      type(csr_t), intent(in) :: graph
         !! Square adjacency matrix of the graph; values are ignored
      integer(default_int), intent(in) :: source
         !! Source vertex, in `1:n`
      integer(default_int), allocatable, intent(out) :: level(:)
         !! Hop count to each vertex
      integer(default_int), allocatable, intent(out) :: prev(:)
         !! Predecessor of each vertex on its shortest hop path
      type(error_t), intent(out), optional :: err

      integer(default_int), allocatable :: queue(:)
      integer(default_int) :: n, head, tail, u, v, k

      if (.not. check_graph(graph, source, err)) then
         allocate (level(0))
         allocate (prev(0))
         return
      end if

      n = graph%n_rows()
      allocate (level(n))
      allocate (prev(n))
      allocate (queue(n))
      level = GRAPH_UNREACHABLE
      prev = GRAPH_NO_PREDECESSOR

      level(source) = 0
      queue(1) = source
      head = 1
      tail = 1
      do while (head <= tail)
         u = queue(head)
         head = head + 1
         do k = graph%row_ptr(u), graph%row_ptr(u + 1) - 1
            v = graph%col_idx(k)
            if (level(v) /= GRAPH_UNREACHABLE) cycle
            level(v) = level(u) + 1
            prev(v) = u
            tail = tail + 1
            queue(tail) = v
         end do
      end do
   end subroutine bfs

   subroutine connected_components(graph, component, n_components, err)
      !! Label the weakly connected components of the graph.
      !!
      !! Edge direction is ignored: the graph and its transpose are explored
      !! together, so `u` and `v` land in the same component whenever either
      !! `u -> v` or `v -> u` is stored. `component(v)` is a label in
      !! `1:n_components`, assigned in increasing order of the smallest vertex
      !! in each component, which makes the labelling deterministic. Reports
      !! `ERROR_VALIDATION` for a malformed or non-square graph.
      type(csr_t), intent(in) :: graph
         !! Square adjacency matrix of the graph; values are ignored
      integer(default_int), allocatable, intent(out) :: component(:)
         !! Component label of each vertex
      integer(default_int), intent(out) :: n_components
         !! Number of weakly connected components found
      type(error_t), intent(out), optional :: err

      type(csr_t) :: incoming
      integer(default_int), allocatable :: queue(:)
      integer(default_int) :: n, seed, head, tail, u

      n_components = 0
      if (.not. check_graph(graph, 1_default_int, err)) then
         allocate (component(0))
         return
      end if

      call graph%transpose(incoming, err)

      n = graph%n_rows()
      allocate (component(n))
      allocate (queue(n))
      component = 0

      do seed = 1, n
         if (component(seed) /= 0) cycle
         n_components = n_components + 1
         component(seed) = n_components
         queue(1) = seed
         head = 1
         tail = 1
         do while (head <= tail)
            u = queue(head)
            head = head + 1
            call enqueue_neighbours(graph, u, n_components, component, queue, tail)
            call enqueue_neighbours(incoming, u, n_components, component, queue, tail)
         end do
      end do

      call incoming%destroy()
   end subroutine connected_components

   pure subroutine enqueue_neighbours(adjacency, u, label, component, queue, tail)
      !! Label and enqueue every still-unlabelled neighbour of `u`.
      type(csr_t), intent(in) :: adjacency
         !! Adjacency to read row `u` from
      integer(default_int), intent(in) :: u
         !! Vertex whose neighbours are being visited
      integer(default_int), intent(in) :: label
         !! Component label to assign
      integer(default_int), intent(inout) :: component(:)
         !! Component labels, 0 where still unlabelled
      integer(default_int), intent(inout) :: queue(:)
         !! Visit queue
      integer(default_int), intent(inout) :: tail
         !! Index of the last used slot of `queue`

      integer(default_int) :: k, v

      do k = adjacency%row_ptr(u), adjacency%row_ptr(u + 1) - 1
         v = adjacency%col_idx(k)
         if (component(v) /= 0) cycle
         component(v) = label
         tail = tail + 1
         queue(tail) = v
      end do
   end subroutine enqueue_neighbours

   function check_graph(graph, source, err) result(ok)
      !! Validate the structural preconditions shared by every algorithm here.
      !!
      !! The adjacency must be a structurally sound `csr_t`, square, non-empty,
      !! small enough for a vertex index to fit the heap's `int32` payload,
      !! and `source` must be a real vertex.
      type(csr_t), intent(in) :: graph
         !! Adjacency matrix to check
      integer(default_int), intent(in) :: source
         !! Vertex index that must lie in `1:n`
      type(error_t), intent(out), optional :: err
      logical :: ok
         !! `.true.` when the graph and vertex are usable

      integer(default_int) :: n

      ok = .false.
      if (.not. graph%is_valid(err)) return

      n = graph%n_rows()
      if (n /= graph%n_cols()) then
         call fail(err, "pic_graph: the adjacency matrix of a graph must be square")
         return
      end if
      if (n < 1) then
         call fail(err, "pic_graph: a graph must have at least one vertex")
         return
      end if
      if (int(n, int64) > int(huge(0_int32), int64)) then
         call fail(err, "pic_graph: vertex count exceeds the int32 payload of the priority queue")
         return
      end if
      if (source < 1 .or. source > n) then
         call fail(err, "pic_graph: vertex index is outside 1:n")
         return
      end if

      ok = .true.
   end function check_graph

   function check_weights(graph, err) result(ok)
      !! Reject negative edge weights, on which Dijkstra and A* are invalid.
      type(csr_t), intent(in) :: graph
         !! Adjacency matrix whose stored values are the edge weights
      type(error_t), intent(out), optional :: err
      logical :: ok
         !! `.true.` when every stored weight is non-negative

      integer(default_int) :: k

      ok = .false.
      do k = 1, graph%nnz()
         if (graph%values(k) < 0.0_dp) then
            call fail(err, "pic_graph: negative edge weights are not supported by Dijkstra or A*")
            return
         end if
      end do
      ok = .true.
   end function check_weights

   pure subroutine fail(err, message)
      !! Set `err` to `ERROR_VALIDATION` with `message`, when `err` is present.
      type(error_t), intent(out), optional :: err
      character(len=*), intent(in) :: message
         !! Human-readable description of the violation

      if (present(err)) call err%set(ERROR_VALIDATION, message)
   end subroutine fail

end module pic_graph
