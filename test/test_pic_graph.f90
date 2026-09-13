module test_pic_graph
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, dp
   use pic_error, only: error_t, ERROR_VALIDATION
   use pic_csr, only: csr_t
   use pic_graph, only: dijkstra, a_star, bfs, connected_components, path_cost, &
                        GRAPH_INFINITY, GRAPH_NO_PREDECESSOR, GRAPH_UNREACHABLE
   implicit none
   private

   public :: collect_pic_graph_tests

   real(dp), parameter :: TOL = 1.0e-12_dp

   ! the classic six-vertex undirected test graph
   integer(default_int), parameter :: EDGE_HEAD(9) = [1, 1, 1, 2, 2, 3, 3, 4, 5]
   integer(default_int), parameter :: EDGE_TAIL(9) = [2, 3, 6, 3, 4, 4, 6, 5, 6]
   real(dp), parameter :: EDGE_WEIGHT(9) = [7.0_dp, 9.0_dp, 14.0_dp, 10.0_dp, 15.0_dp, &
                                            11.0_dp, 2.0_dp, 6.0_dp, 9.0_dp]

contains

   subroutine collect_pic_graph_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("dijkstra_known_distances", test_dijkstra_known_distances), &
                  new_unittest("dijkstra_unreachable", test_dijkstra_unreachable), &
                  new_unittest("dijkstra_single_vertex", test_dijkstra_single_vertex), &
                  new_unittest("dijkstra_fractional_weights", test_dijkstra_fractional_weights), &
                  new_unittest("deterministic_tie_break", test_deterministic_tie_break), &
                  new_unittest("negative_weight_rejected", test_negative_weight_rejected), &
                  new_unittest("dijkstra_bad_graph", test_dijkstra_bad_graph), &
                  new_unittest("a_star_matches_dijkstra", test_a_star_matches_dijkstra), &
                  new_unittest("a_star_zero_heuristic", test_a_star_zero_heuristic), &
                  new_unittest("a_star_source_is_target", test_a_star_source_is_target), &
                  new_unittest("a_star_unreachable_target", test_a_star_unreachable_target), &
                  new_unittest("a_star_errors", test_a_star_errors), &
                  new_unittest("path_cost_behaviour", test_path_cost_behaviour), &
                  new_unittest("bfs_hops", test_bfs_hops), &
                  new_unittest("bfs_errors", test_bfs_errors), &
                  new_unittest("connected_components_labels", test_connected_components_labels) &
                  ]
   end subroutine collect_pic_graph_tests

   subroutine build_reference_graph(g, err)
      !! Undirected six-vertex graph; both directions are stored.
      type(csr_t), intent(inout) :: g
      type(error_t), intent(out) :: err

      call g%build_from_coo(6_default_int, 6_default_int, &
                            [EDGE_HEAD, EDGE_TAIL], [EDGE_TAIL, EDGE_HEAD], &
                            [EDGE_WEIGHT, EDGE_WEIGHT], err)
   end subroutine build_reference_graph

   subroutine build_tie_graph(g, err)
      !! Two equal-cost shortest paths 1-2-4 and 1-3-4, both of cost 2.
      type(csr_t), intent(inout) :: g
      type(error_t), intent(out) :: err

      call g%build_from_coo(4_default_int, 4_default_int, &
                            [1_default_int, 1_default_int, 2_default_int, 3_default_int], &
                            [2_default_int, 3_default_int, 4_default_int, 4_default_int], &
                            [1.0_dp, 1.0_dp, 1.0_dp, 1.0_dp], err)
   end subroutine build_tie_graph

   subroutine trace(prev, source, target, path)
      !! Independent path reconstruction, used to cross-check a_star.
      integer(default_int), intent(in) :: prev(:), source, target
      integer(default_int), allocatable, intent(out) :: path(:)
      integer(default_int) :: n, v, k

      n = 1
      v = target
      do while (v /= source)
         v = prev(v)
         n = n + 1
      end do
      allocate (path(n))
      v = target
      do k = n, 1, -1
         path(k) = v
         v = prev(v)
      end do
   end subroutine trace

   subroutine test_dijkstra_known_distances(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      real(dp), allocatable :: dist(:)
      integer(default_int), allocatable :: prev(:)
      real(dp) :: expected(6)

      call build_reference_graph(g, err)
      call dijkstra(g, 1_default_int, dist, prev, err)
      call check(error,.not. err%has_error(), "dijkstra must succeed")
      if (allocated(error)) return

      expected = [0.0_dp, 7.0_dp, 9.0_dp, 20.0_dp, 20.0_dp, 11.0_dp]
      call check(error, size(dist) == 6, "one distance per vertex")
      if (allocated(error)) return
      call check(error, maxval(abs(dist - expected)) < TOL, "hand-computed shortest distances")
      if (allocated(error)) return

      call check(error, prev(1) == GRAPH_NO_PREDECESSOR, "the source has no predecessor")
      if (allocated(error)) return
      call check(error, prev(4) == 3, "vertex 4 is reached through 3")
      if (allocated(error)) return
      call check(error, prev(5) == 6, "vertex 5 is reached through 6")
      if (allocated(error)) return
      call check(error, prev(6) == 3, "vertex 6 is reached through 3")
      if (allocated(error)) return

      ! starting elsewhere must still be self-consistent
      call dijkstra(g, 5_default_int, dist, prev, err)
      call check(error, abs(dist(5)) < TOL, "distance to the source is zero")
      if (allocated(error)) return
      call check(error, abs(dist(1) - 20.0_dp) < TOL, "the graph is symmetric")
      if (allocated(error)) return
   end subroutine test_dijkstra_known_distances

   subroutine test_dijkstra_unreachable(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      real(dp), allocatable :: dist(:)
      integer(default_int), allocatable :: prev(:)

      ! 1 -> 2, vertex 3 isolated
      call g%build_from_coo(3_default_int, 3_default_int, &
                            [1_default_int], [2_default_int], [4.0_dp], err)
      call dijkstra(g, 1_default_int, dist, prev, err)
      call check(error,.not. err%has_error(), "dijkstra on a disconnected graph is fine")
      if (allocated(error)) return
      call check(error, abs(dist(2) - 4.0_dp) < TOL, "reachable vertex")
      if (allocated(error)) return
      call check(error, dist(3) >= GRAPH_INFINITY, "unreachable vertex gets the infinity sentinel")
      if (allocated(error)) return
      call check(error, prev(3) == GRAPH_NO_PREDECESSOR, "unreachable vertex has no predecessor")
      if (allocated(error)) return

      ! walking the wrong way down a one-way edge reaches nothing
      call dijkstra(g, 2_default_int, dist, prev, err)
      call check(error, dist(1) >= GRAPH_INFINITY, "edges are directed")
      if (allocated(error)) return
   end subroutine test_dijkstra_unreachable

   subroutine test_dijkstra_single_vertex(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      real(dp), allocatable :: dist(:)
      integer(default_int), allocatable :: prev(:), path(:)
      integer(default_int) :: no_index(0)
      real(dp) :: no_value(0)

      call g%build_from_coo(1_default_int, 1_default_int, no_index, no_index, no_value, err)
      call dijkstra(g, 1_default_int, dist, prev, err)
      call check(error,.not. err%has_error(), "single-vertex dijkstra")
      if (allocated(error)) return
      call check(error, size(dist) == 1 .and. abs(dist(1)) < TOL, "the only distance is zero")
      if (allocated(error)) return
      call check(error, prev(1) == GRAPH_NO_PREDECESSOR, "no predecessor")
      if (allocated(error)) return

      call a_star(g, 1_default_int, 1_default_int, [0.0_dp], path, err)
      call check(error,.not. err%has_error(), "single-vertex a_star")
      if (allocated(error)) return
      call check(error, size(path) == 1 .and. path(1) == 1, "the path is just the vertex")
      if (allocated(error)) return

      ! a graph with no vertices at all is rejected
      call g%build_from_coo(0_default_int, 0_default_int, no_index, no_index, no_value, err)
      call dijkstra(g, 1_default_int, dist, prev, err)
      call check(error, err%has_error(), "an empty graph is rejected")
      if (allocated(error)) return
      call check(error, size(dist) == 0, "a rejected dijkstra returns zero-size arrays")
      if (allocated(error)) return
   end subroutine test_dijkstra_single_vertex

   subroutine test_dijkstra_fractional_weights(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      real(dp), allocatable :: dist(:)
      integer(default_int), allocatable :: prev(:)

      ! a chain 1->2->3->4 of small, non-representable weights alongside a
      ! direct 1->4 shortcut that must lose by a hair; this exercises the
      ! real-to-key encoding over several orders of magnitude
      call g%build_from_coo(4_default_int, 4_default_int, &
                            [1_default_int, 2_default_int, 3_default_int, 1_default_int], &
                            [2_default_int, 3_default_int, 4_default_int, 4_default_int], &
                            [0.1_dp, 1.0e-8_dp, 1.0e6_dp, 1.0e6_dp + 1.0_dp], err)
      call dijkstra(g, 1_default_int, dist, prev, err)
      call check(error,.not. err%has_error(), "fractional dijkstra")
      if (allocated(error)) return
      call check(error, abs(dist(2) - 0.1_dp) < TOL, "first hop")
      if (allocated(error)) return
      call check(error, abs(dist(3) - (0.1_dp + 1.0e-8_dp)) < TOL, "tiny weight is not lost")
      if (allocated(error)) return
      call check(error, abs(dist(4) - (0.1_dp + 1.0e-8_dp + 1.0e6_dp)) < 1.0e-6_dp, "chain beats the shortcut")
      if (allocated(error)) return
      call check(error, prev(4) == 3, "the winning route is through the chain")
      if (allocated(error)) return
   end subroutine test_dijkstra_fractional_weights

   subroutine test_deterministic_tie_break(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      real(dp), allocatable :: dist(:)
      integer(default_int), allocatable :: prev(:), path(:)

      call build_tie_graph(g, err)
      call dijkstra(g, 1_default_int, dist, prev, err)
      call check(error,.not. err%has_error(), "tie graph dijkstra")
      if (allocated(error)) return
      call check(error, maxval(abs(dist - [0.0_dp, 1.0_dp, 1.0_dp, 2.0_dp])) < TOL, "both routes cost 2")
      if (allocated(error)) return

      ! vertices 2 and 3 tie at distance 1; the heap pops them FIFO, so 2
      ! settles first and claims vertex 4. this must not vary by compiler.
      call check(error, prev(4) == 2, "the deterministic tie winner is vertex 2")
      if (allocated(error)) return
      call check(error, prev(2) == 1 .and. prev(3) == 1, "both intermediates come from the source")
      if (allocated(error)) return

      call trace(prev, 1_default_int, 4_default_int, path)
      call check(error, size(path) == 3, "the tie-broken path has three vertices")
      if (allocated(error)) return
      call check(error, all(path == [1, 2, 4]), "the tie-broken path is 1-2-4")
      if (allocated(error)) return

      deallocate (path)
      call a_star(g, 1_default_int, 4_default_int, [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp], path, err)
      call check(error, all(path == [1, 2, 4]), "a_star breaks the tie the same way")
      if (allocated(error)) return
   end subroutine test_deterministic_tie_break

   subroutine test_negative_weight_rejected(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      real(dp), allocatable :: dist(:)
      integer(default_int), allocatable :: prev(:), level(:), path(:)
      real(dp) :: cost

      call g%build_from_coo(3_default_int, 3_default_int, &
                            [1_default_int, 2_default_int], [2_default_int, 3_default_int], &
                            [1.0_dp, -5.0_dp], err)
      call check(error,.not. err%has_error(), "a negative weight is storable")
      if (allocated(error)) return

      call dijkstra(g, 1_default_int, dist, prev, err)
      call check(error, err%has_error(), "dijkstra must refuse negative weights")
      if (allocated(error)) return
      call check(error, err%get_code() == ERROR_VALIDATION, "negative weights are a validation error")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "negative") > 0, "the message says negative")
      if (allocated(error)) return
      call check(error, size(dist) == 0 .and. size(prev) == 0, "no results are produced")
      if (allocated(error)) return

      call a_star(g, 1_default_int, 3_default_int, [0.0_dp, 0.0_dp, 0.0_dp], path, err)
      call check(error, err%has_error(), "a_star must refuse negative weights")
      if (allocated(error)) return

      call path_cost(g, [1_default_int, 2_default_int], cost, err)
      call check(error, err%has_error(), "path_cost must refuse negative weights")
      if (allocated(error)) return

      ! bfs never reads the weights, so it is happy
      call bfs(g, 1_default_int, level, prev, err)
      call check(error,.not. err%has_error(), "bfs ignores weights entirely")
      if (allocated(error)) return
      call check(error, level(3) == 2, "bfs still counts hops")
      if (allocated(error)) return
   end subroutine test_negative_weight_rejected

   subroutine test_dijkstra_bad_graph(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      real(dp), allocatable :: dist(:)
      integer(default_int), allocatable :: prev(:)

      ! not square
      call g%build_from_coo(2_default_int, 3_default_int, &
                            [1_default_int], [2_default_int], [1.0_dp], err)
      call dijkstra(g, 1_default_int, dist, prev, err)
      call check(error, err%has_error(), "a non-square adjacency matrix is rejected")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "square") > 0, "the message says square")
      if (allocated(error)) return

      call build_reference_graph(g, err)
      call dijkstra(g, 0_default_int, dist, prev, err)
      call check(error, err%has_error(), "source 0 is rejected")
      if (allocated(error)) return

      call dijkstra(g, 7_default_int, dist, prev, err)
      call check(error, err%has_error(), "source past n is rejected")
      if (allocated(error)) return

      ! structurally broken adjacency
      g%col_idx(1) = 99
      call dijkstra(g, 1_default_int, dist, prev, err)
      call check(error, err%has_error(), "a malformed adjacency matrix is rejected")
      if (allocated(error)) return

      ! and the same call without an error argument must not crash
      call dijkstra(g, 1_default_int, dist, prev)
      call check(error, size(dist) == 0, "a rejected call without err yields nothing")
      if (allocated(error)) return
   end subroutine test_dijkstra_bad_graph

   subroutine test_a_star_matches_dijkstra(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      real(dp), allocatable :: dist(:)
      integer(default_int), allocatable :: prev(:), path(:)
      real(dp) :: cost
      real(dp), parameter :: HEURISTIC(6) = [10.0_dp, 10.0_dp, 5.0_dp, 3.0_dp, 0.0_dp, 4.0_dp]

      call build_reference_graph(g, err)
      call dijkstra(g, 1_default_int, dist, prev, err)

      call a_star(g, 1_default_int, 5_default_int, HEURISTIC, path, err)
      call check(error,.not. err%has_error(), "a_star must succeed")
      if (allocated(error)) return
      call check(error, path(1) == 1, "the path starts at the source")
      if (allocated(error)) return
      call check(error, path(size(path)) == 5, "the path ends at the target")
      if (allocated(error)) return

      call path_cost(g, path, cost, err)
      call check(error,.not. err%has_error(), "path_cost must succeed")
      if (allocated(error)) return
      call check(error, abs(cost - dist(5)) < TOL, "an admissible heuristic finds a true shortest path")
      if (allocated(error)) return
      call check(error, abs(cost - 20.0_dp) < TOL, "and that cost is the hand-computed 20")
      if (allocated(error)) return
   end subroutine test_a_star_matches_dijkstra

   subroutine test_a_star_zero_heuristic(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      real(dp), allocatable :: dist(:)
      integer(default_int), allocatable :: prev(:), path(:), reference(:)
      real(dp) :: zero(6)

      call build_reference_graph(g, err)
      call dijkstra(g, 1_default_int, dist, prev, err)
      call trace(prev, 1_default_int, 5_default_int, reference)

      zero = 0.0_dp
      call a_star(g, 1_default_int, 5_default_int, zero, path, err)
      call check(error,.not. err%has_error(), "zero-heuristic a_star must succeed")
      if (allocated(error)) return
      call check(error, size(path) == size(reference), "same path length as dijkstra")
      if (allocated(error)) return
      call check(error, all(path == reference), "a zero heuristic degenerates to dijkstra exactly")
      if (allocated(error)) return
      call check(error, all(path == [1, 3, 6, 5]), "and that path is 1-3-6-5")
      if (allocated(error)) return
   end subroutine test_a_star_zero_heuristic

   subroutine test_a_star_source_is_target(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      integer(default_int), allocatable :: path(:)
      real(dp) :: cost
      real(dp), parameter :: HEURISTIC(6) = [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp]

      call build_reference_graph(g, err)
      call a_star(g, 3_default_int, 3_default_int, HEURISTIC, path, err)
      call check(error,.not. err%has_error(), "source == target must succeed")
      if (allocated(error)) return
      call check(error, size(path) == 1, "the path is a single vertex")
      if (allocated(error)) return
      call check(error, path(1) == 3, "and that vertex is the source")
      if (allocated(error)) return

      call path_cost(g, path, cost, err)
      call check(error, abs(cost) < TOL, "a single-vertex path costs nothing")
      if (allocated(error)) return
   end subroutine test_a_star_source_is_target

   subroutine test_a_star_unreachable_target(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      integer(default_int), allocatable :: path(:)

      call g%build_from_coo(3_default_int, 3_default_int, &
                            [1_default_int], [2_default_int], [1.0_dp], err)
      call a_star(g, 1_default_int, 3_default_int, [0.0_dp, 0.0_dp, 0.0_dp], path, err)
      call check(error,.not. err%has_error(), "an unreachable target is not an error")
      if (allocated(error)) return
      call check(error, size(path) == 0, "an unreachable target yields an empty path")
      if (allocated(error)) return
   end subroutine test_a_star_unreachable_target

   subroutine test_a_star_errors(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      integer(default_int), allocatable :: path(:)
      real(dp) :: zero(6), bad(6)

      call build_reference_graph(g, err)
      zero = 0.0_dp

      call a_star(g, 0_default_int, 5_default_int, zero, path, err)
      call check(error, err%has_error(), "an out-of-range source is rejected")
      if (allocated(error)) return
      call check(error, size(path) == 0, "and the path is empty")
      if (allocated(error)) return

      call a_star(g, 1_default_int, 0_default_int, zero, path, err)
      call check(error, err%has_error(), "target 0 is rejected")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "target") > 0, "the message names the target")
      if (allocated(error)) return

      call a_star(g, 1_default_int, 9_default_int, zero, path, err)
      call check(error, err%has_error(), "a target past n is rejected")
      if (allocated(error)) return

      call a_star(g, 1_default_int, 5_default_int, [0.0_dp, 0.0_dp], path, err)
      call check(error, err%has_error(), "a short heuristic is rejected")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "heuristic") > 0, "the message names the heuristic")
      if (allocated(error)) return

      bad = 0.0_dp
      bad(4) = -1.0_dp
      call a_star(g, 1_default_int, 5_default_int, bad, path, err)
      call check(error, err%has_error(), "a negative heuristic is rejected")
      if (allocated(error)) return
      call check(error, err%get_code() == ERROR_VALIDATION, "validation error code")
      if (allocated(error)) return

      call a_star(g, 1_default_int, 9_default_int, zero, path)
      call check(error, size(path) == 0, "a rejected a_star without err yields an empty path")
      if (allocated(error)) return
   end subroutine test_a_star_errors

   subroutine test_path_cost_behaviour(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      real(dp) :: cost
      integer(default_int) :: empty_path(0)

      call build_reference_graph(g, err)

      call path_cost(g, [1_default_int, 3_default_int, 6_default_int, 5_default_int], cost, err)
      call check(error,.not. err%has_error(), "walking a real path succeeds")
      if (allocated(error)) return
      call check(error, abs(cost - 20.0_dp) < TOL, "9 + 2 + 9 = 20")
      if (allocated(error)) return

      call path_cost(g, empty_path, cost, err)
      call check(error, cost >= GRAPH_INFINITY, "an empty path costs infinity")
      if (allocated(error)) return
      call check(error,.not. err%has_error(), "and is not an error")
      if (allocated(error)) return

      call path_cost(g, [1_default_int, 5_default_int], cost, err)
      call check(error, err%has_error(), "a missing edge is rejected")
      if (allocated(error)) return
      call check(error, cost >= GRAPH_INFINITY, "and the cost is reset to infinity")
      if (allocated(error)) return

      call path_cost(g, [1_default_int, 99_default_int], cost, err)
      call check(error, err%has_error(), "an out-of-range vertex is rejected")
      if (allocated(error)) return

      call path_cost(g, [0_default_int, 1_default_int], cost, err)
      call check(error, err%has_error(), "a leading out-of-range vertex is rejected")
      if (allocated(error)) return

      call path_cost(g, [1_default_int, 2_default_int], cost)
      call check(error, abs(cost - 7.0_dp) < TOL, "path_cost without err")
      if (allocated(error)) return
   end subroutine test_path_cost_behaviour

   subroutine test_bfs_hops(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      integer(default_int), allocatable :: level(:), prev(:)

      call build_reference_graph(g, err)
      call bfs(g, 1_default_int, level, prev, err)
      call check(error,.not. err%has_error(), "bfs must succeed")
      if (allocated(error)) return
      call check(error, level(1) == 0, "the source is at level zero")
      if (allocated(error)) return
      call check(error, all(level == [0, 1, 1, 2, 2, 1]), "hand-counted hop levels")
      if (allocated(error)) return
      call check(error, prev(4) == 2, "vertex 4 is first reached from vertex 2")
      if (allocated(error)) return
      call check(error, prev(1) == GRAPH_NO_PREDECESSOR, "the source has no predecessor")
      if (allocated(error)) return

      ! disconnected vertex keeps the unreachable sentinel
      call g%build_from_coo(3_default_int, 3_default_int, &
                            [1_default_int], [2_default_int], [1.0_dp], err)
      call bfs(g, 1_default_int, level, prev, err)
      call check(error, level(3) == GRAPH_UNREACHABLE, "unreachable vertices are flagged")
      if (allocated(error)) return
      call check(error, level(2) == 1, "reachable vertices are not")
      if (allocated(error)) return
   end subroutine test_bfs_hops

   subroutine test_bfs_errors(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      integer(default_int), allocatable :: level(:), prev(:)

      call build_reference_graph(g, err)
      call bfs(g, 12_default_int, level, prev, err)
      call check(error, err%has_error(), "an out-of-range source is rejected")
      if (allocated(error)) return
      call check(error, size(level) == 0 .and. size(prev) == 0, "no results are produced")
      if (allocated(error)) return
   end subroutine test_bfs_errors

   subroutine test_connected_components_labels(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: g
      type(error_t) :: err
      integer(default_int), allocatable :: component(:)
      integer(default_int) :: n_components

      ! 1 -> 2 and 5 -> 4 (stored one way only), vertex 3 isolated
      call g%build_from_coo(5_default_int, 5_default_int, &
                            [1_default_int, 5_default_int], [2_default_int, 4_default_int], &
                            [1.0_dp, 1.0_dp], err)
      call connected_components(g, component, n_components, err)
      call check(error,.not. err%has_error(), "connected_components must succeed")
      if (allocated(error)) return
      call check(error, n_components == 3, "three weakly connected components")
      if (allocated(error)) return
      call check(error, all(component == [1, 1, 2, 3, 3]), "labels follow the smallest vertex")
      if (allocated(error)) return

      ! the fully connected reference graph is a single component
      call build_reference_graph(g, err)
      call connected_components(g, component, n_components, err)
      call check(error, n_components == 1, "the reference graph is connected")
      if (allocated(error)) return
      call check(error, all(component == 1), "every vertex carries label 1")
      if (allocated(error)) return

      ! a non-square adjacency is rejected
      call g%build_from_coo(2_default_int, 3_default_int, &
                            [1_default_int], [2_default_int], [1.0_dp], err)
      call connected_components(g, component, n_components, err)
      call check(error, err%has_error(), "a non-square adjacency matrix is rejected")
      if (allocated(error)) return
      call check(error, n_components == 0 .and. size(component) == 0, "no labels are produced")
      if (allocated(error)) return
   end subroutine test_connected_components_labels

end module test_pic_graph
