module delaunay
    !! Provides a minimal, in-house implementation of the subset of the
    !! GEOMPACK library used by FPLOT to construct 2D Delaunay
    !! triangulations.
    !!
    !! Original FORTRAN77 version by Barry Joe.  FORTRAN90 version by
    !! John Burkardt.  Reference: Barry Joe, GEOMPACK - a software
    !! package for the generation of meshes using geometric algorithms,
    !! Advances in Engineering Software, Volume 13, pages 325-331, 1991.
    use iso_fortran_env, only : int32, real64
    implicit none
    private
    public :: r8tris2

contains
! ------------------------------------------------------------------------------
    subroutine r8tris2(node_num, node_xy, triangle_num, triangle_node, &
            triangle_neighbor)
        !! Constructs a Delaunay triangulation of 2D vertices.
        !!
        !! The routine constructs the Delaunay triangulation of a set of
        !! 2D vertices using an incremental approach and diagonal edge
        !! swaps.  Vertices are first sorted in lexicographically
        !! increasing (X,Y) order, and then are inserted one at a time
        !! from outside the convex hull.
        integer(int32), intent(in) :: node_num
            !! The number of vertices.
        real(real64), intent(inout), dimension(2, node_num) :: node_xy
            !! The coordinates of the vertices.  On output, the vertices
            !! have been sorted into dictionary order.
        integer(int32), intent(out) :: triangle_num
            !! The number of triangles in the triangulation;
            !! TRIANGLE_NUM is equal to 2*NODE_NUM - NB - 2, where NB is
            !! the number of boundary vertices.
        integer(int32), intent(out), dimension(3, 2 * node_num) :: &
            triangle_node
            !! The nodes that make up each triangle.  The elements are
            !! indices of the vertex array.  The vertices of the
            !! triangles are in counter clockwise order.
        integer(int32), intent(out), dimension(3, 2 * node_num) :: &
            triangle_neighbor
            !! The triangle neighbor list.  Positive elements are
            !! indices of TRIANGLE_NODE; negative elements are used for
            !! links of a counter clockwise linked list of boundary
            !! edges; LINK = -(3*I + J-1) where I, J = triangle, edge
            !! index; TRIANGLE_NEIGHBOR(J,I) refers to the neighbor
            !! along the edge from vertex J to J+1 (mod 3).

        ! Local Variables
        real(real64) :: cmax, tol
        integer(int32) :: e, i, j, k, l, ledg, lr, ltri, m, m1, m2, &
            n, redg, rtri, t, top
        integer(int32), dimension(node_num) :: indx, stack

        ! Initialization
        tol = 100.0d0 * epsilon(tol)

        ! Sort the vertices by increasing (x,y)
        call r82vec_sort_heap_index_a(node_num, node_xy, indx)
        call r82vec_permute(node_num, node_xy, indx)

        ! Make sure that the data points are "reasonably" distinct
        m1 = 1
        do i = 2, node_num
            m = m1
            m1 = i
            k = 0
            do j = 1, 2
                cmax = max(abs(node_xy(j, m)), abs(node_xy(j, m1)))
                if (tol * (cmax + 1.0d0) < &
                        abs(node_xy(j, m) - node_xy(j, m1))) then
                    k = j
                    exit
                end if
            end do
            if (k == 0) then
                error stop &
                    "R8TRIS2 - Fatal error!  Two points are too close together."
            end if
        end do

        ! Starting from points M1 and M2, search for a third point M that
        ! makes a "healthy" triangle (M1,M2,M)
        m1 = 1
        m2 = 2
        j = 3
        do
            if (node_num < j) then
                error stop &
                    "R8TRIS2 - Fatal error!  Could not find a non-collinear point."
            end if

            m = j
            lr = lrline(node_xy(1, m), node_xy(2, m), node_xy(1, m1), &
                node_xy(2, m1), node_xy(1, m2), node_xy(2, m2), 0.0d0)
            if (lr /= 0) exit
            j = j + 1
        end do

        ! Set up the triangle information for (M1,M2,M), and for any
        ! other triangles created because points were collinear with M1,
        ! M2
        triangle_num = j - 2

        if (lr == -1) then
            triangle_node(1, 1) = m1
            triangle_node(2, 1) = m2
            triangle_node(3, 1) = m
            triangle_neighbor(3, 1) = -3

            do i = 2, triangle_num
                m1 = m2
                m2 = i + 1
                triangle_node(1, i) = m1
                triangle_node(2, i) = m2
                triangle_node(3, i) = m
                triangle_neighbor(1, i - 1) = -3 * i
                triangle_neighbor(2, i - 1) = i
                triangle_neighbor(3, i) = i - 1
            end do

            triangle_neighbor(1, triangle_num) = -3 * triangle_num - 1
            triangle_neighbor(2, triangle_num) = -5
            ledg = 2
            ltri = triangle_num
        else
            triangle_node(1, 1) = m2
            triangle_node(2, 1) = m1
            triangle_node(3, 1) = m
            triangle_neighbor(1, 1) = -4

            do i = 2, triangle_num
                m1 = m2
                m2 = i + 1
                triangle_node(1, i) = m2
                triangle_node(2, i) = m1
                triangle_node(3, i) = m
                triangle_neighbor(3, i - 1) = i
                triangle_neighbor(1, i) = -3 * i - 3
                triangle_neighbor(2, i) = i - 1
            end do

            triangle_neighbor(3, triangle_num) = -3 * triangle_num
            triangle_neighbor(2, 1) = -3 * triangle_num - 2
            ledg = 2
            ltri = 1
        end if

        ! Insert the vertices one at a time from outside the convex
        ! hull, determine visible boundary edges, and apply diagonal
        ! edge swaps until the Delaunay triangulation of the vertices
        ! (so far) is obtained
        top = 0

        do i = j + 1, node_num
            m = i
            m1 = triangle_node(ledg, ltri)

            if (ledg <= 2) then
                m2 = triangle_node(ledg + 1, ltri)
            else
                m2 = triangle_node(1, ltri)
            end if

            lr = lrline(node_xy(1, m), node_xy(2, m), node_xy(1, m1), &
                node_xy(2, m1), node_xy(1, m2), node_xy(2, m2), 0.0d0)

            if (0 < lr) then
                rtri = ltri
                redg = ledg
                ltri = 0
            else
                l = -triangle_neighbor(ledg, ltri)
                rtri = l / 3
                redg = mod(l, 3) + 1
            end if

            call vbedg(node_xy(1, m), node_xy(2, m), node_num, node_xy, &
                triangle_num, triangle_node, triangle_neighbor, ltri, &
                ledg, rtri, redg)

            n = triangle_num + 1
            l = -triangle_neighbor(ledg, ltri)

            do
                t = l / 3
                e = mod(l, 3) + 1
                l = -triangle_neighbor(e, t)
                m2 = triangle_node(e, t)

                if (e <= 2) then
                    m1 = triangle_node(e + 1, t)
                else
                    m1 = triangle_node(1, t)
                end if

                triangle_num = triangle_num + 1
                triangle_neighbor(e, t) = triangle_num
                triangle_node(1, triangle_num) = m1
                triangle_node(2, triangle_num) = m2
                triangle_node(3, triangle_num) = m
                triangle_neighbor(1, triangle_num) = t
                triangle_neighbor(2, triangle_num) = triangle_num - 1
                triangle_neighbor(3, triangle_num) = triangle_num + 1
                top = top + 1

                if (node_num < top) then
                    error stop "R8TRIS2 - Fatal error!  Stack overflow."
                end if

                stack(top) = triangle_num

                if (t == rtri .and. e == redg) exit
            end do

            triangle_neighbor(ledg, ltri) = -3 * n - 1
            triangle_neighbor(2, n) = -3 * triangle_num - 2
            triangle_neighbor(3, triangle_num) = -l
            ltri = n
            ledg = 2

            call swapec(m, top, ltri, ledg, node_num, node_xy, &
                triangle_num, triangle_node, triangle_neighbor, stack)
        end do

        ! Now account for the sorting that was done
        do i = 1, 3
            do j = 1, triangle_num
                triangle_node(i, j) = indx(triangle_node(i, j))
            end do
        end do

        call perm_inverse(node_num, indx)
        call r82vec_permute(node_num, node_xy, indx)
    end subroutine

! ------------------------------------------------------------------------------
    pure subroutine r82vec_sort_heap_index_a(n, a, indx)
        !! Computes an indexed heap ascending sort of an R82VEC.
        !!
        !! The sorting is not actually carried out.  Rather an index
        !! array is created which defines the sorting.
        integer(int32), intent(in) :: n
            !! The number of entries in the array.
        real(real64), intent(in), dimension(2, n) :: a
            !! The array to be index-sorted.
        integer(int32), intent(out), dimension(n) :: indx
            !! The sort index.  The I-th element of the sorted array is
            !! A(1:2,INDX(I)).

        ! Local Variables
        real(real64), dimension(2) :: aval
        integer(int32) :: i, indxt, ir, j, l

        if (n < 1) return

        do i = 1, n
            indx(i) = i
        end do

        if (n == 1) return

        l = n / 2 + 1
        ir = n

        do
            if (1 < l) then
                l = l - 1
                indxt = indx(l)
                aval = a(:, indxt)
            else
                indxt = indx(ir)
                aval = a(:, indxt)
                indx(ir) = indx(1)
                ir = ir - 1
                if (ir == 1) then
                    indx(1) = indxt
                    exit
                end if
            end if

            i = l
            j = l + l

            do while (j <= ir)
                if (j < ir) then
                    if (a(1, indx(j)) < a(1, indx(j + 1)) .or. &
                            (a(1, indx(j)) == a(1, indx(j + 1)) .and. &
                            a(2, indx(j)) < a(2, indx(j + 1)))) then
                        j = j + 1
                    end if
                end if

                if (aval(1) < a(1, indx(j)) .or. &
                        (aval(1) == a(1, indx(j)) .and. &
                        aval(2) < a(2, indx(j)))) then
                    indx(i) = indx(j)
                    i = j
                    j = j + j
                else
                    j = ir + 1
                end if
            end do

            indx(i) = indxt
        end do
    end subroutine

! ------------------------------------------------------------------------------
    subroutine r82vec_permute(n, a, p)
        !! Permutes an R82VEC in place, according to a given permutation.
        integer(int32), intent(in) :: n
            !! The number of objects.
        real(real64), intent(inout), dimension(2, n) :: a
            !! The array to be permuted.
        integer(int32), intent(inout), dimension(n) :: p
            !! The permutation.  P(I) = J means that the I-th element of
            !! the output array should be the J-th element of the input
            !! array.

        ! Local Variables
        real(real64), dimension(2) :: a_temp
        integer(int32) :: ierror, iget, iput, istart

        call perm_check(n, p, ierror)
        if (ierror /= 0) then
            error stop &
                "R82VEC_PERMUTE - Fatal error!  P does not represent a permutation."
        end if

        do istart = 1, n
            if (p(istart) < 0) then
                cycle
            else if (p(istart) == istart) then
                p(istart) = -p(istart)
                cycle
            else
                a_temp = a(:, istart)
                iget = istart

                do
                    iput = iget
                    iget = p(iget)
                    p(iput) = -p(iput)

                    if (iget < 1 .or. n < iget) then
                        error stop &
                            "R82VEC_PERMUTE - Fatal error!  Invalid permutation entry."
                    end if

                    if (iget == istart) then
                        a(:, iput) = a_temp
                        exit
                    end if

                    a(:, iput) = a(:, iget)
                end do
            end if
        end do

        p = -p
    end subroutine

! ------------------------------------------------------------------------------
    pure subroutine perm_check(n, p, ierror)
        !! Checks that a vector represents a permutation.
        integer(int32), intent(in) :: n
            !! The number of entries.
        integer(int32), intent(in), dimension(n) :: p
            !! The array to check.
        integer(int32), intent(out) :: ierror
            !! 0 if the array represents a permutation; otherwise, the
            !! smallest missing value.

        ! Local Variables
        integer(int32) :: ifind, iseek

        ierror = 0
        do iseek = 1, n
            ierror = iseek
            do ifind = 1, n
                if (p(ifind) == iseek) then
                    ierror = 0
                    exit
                end if
            end do
            if (ierror /= 0) return
        end do
    end subroutine

! ------------------------------------------------------------------------------
    subroutine perm_inverse(n, p)
        !! Inverts a permutation in-place.
        integer(int32), intent(in) :: n
            !! The number of objects being permuted.
        integer(int32), intent(inout), dimension(n) :: p
            !! The permutation, in standard index form.  On output, P
            !! describes the inverse permutation.

        ! Local Variables
        integer(int32) :: i, i0, i1, i2, ierror, is

        if (n <= 0) then
            error stop "PERM_INVERSE - Fatal error!  N <= 0."
        end if

        call perm_check(n, p, ierror)
        if (ierror /= 0) then
            error stop &
                "PERM_INVERSE - Fatal error!  P does not represent a permutation."
        end if

        is = 1
        do i = 1, n
            i1 = p(i)
            do while (i < i1)
                i2 = p(i1)
                p(i1) = -i2
                i1 = i2
            end do
            is = -sign(1, p(i))
            p(i) = sign(p(i), is)
        end do

        do i = 1, n
            i1 = -p(i)
            if (0 <= i1) then
                i0 = i
                do
                    i2 = p(i1)
                    p(i1) = i0
                    if (i2 < 0) exit
                    i0 = i1
                    i1 = i2
                end do
            end if
        end do
    end subroutine

! ------------------------------------------------------------------------------
    pure function lrline(xu, yu, xv1, yv1, xv2, yv2, dv) result(rst)
        !! Determines if a point is left of, right of, or on a directed
        !! line.
        !!
        !! The directed line is parallel to, and at a signed distance DV
        !! from a directed base line from (XV1,YV1) to (XV2,YV2).
        real(real64), intent(in) :: xu
            !! The x-coordinate of the point whose position relative to
            !! the directed line is to be determined.
        real(real64), intent(in) :: yu
            !! The y-coordinate of the point whose position relative to
            !! the directed line is to be determined.
        real(real64), intent(in) :: xv1
            !! The x-coordinate of the first point defining the base
            !! line.
        real(real64), intent(in) :: yv1
            !! The y-coordinate of the first point defining the base
            !! line.
        real(real64), intent(in) :: xv2
            !! The x-coordinate of the second point defining the base
            !! line.
        real(real64), intent(in) :: yv2
            !! The y-coordinate of the second point defining the base
            !! line.
        real(real64), intent(in) :: dv
            !! The signed distance of the directed line from the
            !! directed base line.  DV is positive for a line to the
            !! left of the base line.
        integer(int32) :: rst
            !! +1 if the point is to the right of the directed line; 0 if
            !! the point is on the directed line; -1 if the point is to
            !! the left of the directed line.

        ! Local Variables
        real(real64) :: dx, dxu, dy, dyu, t, tol, tolabs

        tol = 100.0d0 * epsilon(tol)

        dx = xv2 - xv1
        dy = yv2 - yv1
        dxu = xu - xv1
        dyu = yu - yv1

        tolabs = tol * max(abs(dx), abs(dy), abs(dxu), abs(dyu), abs(dv))

        t = dy * dxu - dx * dyu + dv * sqrt(dx * dx + dy * dy)

        if (tolabs < t) then
            rst = 1
        else if (-tolabs <= t) then
            rst = 0
        else
            rst = -1
        end if
    end function

! ------------------------------------------------------------------------------
    pure function diaedg(x0, y0, x1, y1, x2, y2, x3, y3) result(rst)
        !! Chooses a diagonal edge.
        !!
        !! The routine determines whether 0--2 or 1--3 is the diagonal
        !! edge that should be chosen, based on the circumcircle
        !! criterion, where (X0,Y0), (X1,Y1), (X2,Y2), (X3,Y3) are the
        !! vertices of a simple quadrilateral in counterclockwise order.
        real(real64), intent(in) :: x0, y0, x1, y1, x2, y2, x3, y3
            !! The coordinates of the vertices of the quadrilateral.
        integer(int32) :: rst
            !! +1 if diagonal edge 02 is chosen; -1 if diagonal edge 13
            !! is chosen; 0 if the four vertices are cocircular.

        ! Local Variables
        real(real64) :: ca, cb, dx10, dx12, dx30, dx32, dy10, dy12, dy30, &
            dy32, s, tol, tola, tolb

        tol = 100.0d0 * epsilon(tol)

        dx10 = x1 - x0
        dy10 = y1 - y0
        dx12 = x1 - x2
        dy12 = y1 - y2
        dx30 = x3 - x0
        dy30 = y3 - y0
        dx32 = x3 - x2
        dy32 = y3 - y2

        tola = tol * max(abs(dx10), abs(dy10), abs(dx30), abs(dy30))
        tolb = tol * max(abs(dx12), abs(dy12), abs(dx32), abs(dy32))

        ca = dx10 * dx30 + dy10 * dy30
        cb = dx12 * dx32 + dy12 * dy32

        if (tola < ca .and. tolb < cb) then
            rst = -1
        else if (ca < -tola .and. cb < -tolb) then
            rst = 1
        else
            tola = max(tola, tolb)
            s = (dx10 * dy30 - dx30 * dy10) * cb + &
                (dx32 * dy12 - dx12 * dy32) * ca
            if (tola < s) then
                rst = -1
            else if (s < -tola) then
                rst = 1
            else
                rst = 0
            end if
        end if
    end function

! ------------------------------------------------------------------------------
    pure function i4_modp(i, j) result(rst)
        !! Returns the nonnegative remainder of I4 division.
        integer(int32), intent(in) :: i
            !! The number to be divided.
        integer(int32), intent(in) :: j
            !! The number that divides I.
        integer(int32) :: rst
            !! The nonnegative remainder when I is divided by J.

        rst = mod(i, j)
        if (rst < 0) rst = rst + abs(j)
    end function

! ------------------------------------------------------------------------------
    pure function i4_wrap(ival, ilo, ihi) result(rst)
        !! Forces an I4 to lie between given limits by wrapping.
        integer(int32), intent(in) :: ival
            !! The value to wrap.
        integer(int32), intent(in) :: ilo
            !! The lower bound.
        integer(int32), intent(in) :: ihi
            !! The upper bound.
        integer(int32) :: rst
            !! A "wrapped" version of IVAL.

        ! Local Variables
        integer(int32) :: jhi, jlo, wide

        jlo = min(ilo, ihi)
        jhi = max(ilo, ihi)
        wide = jhi - jlo + 1

        if (wide == 1) then
            rst = jlo
        else
            rst = jlo + i4_modp(ival - jlo, wide)
        end if
    end function

! ------------------------------------------------------------------------------
    pure subroutine vbedg(x, y, node_num, node_xy, triangle_num, &
            triangle_node, triangle_neighbor, ltri, ledg, rtri, redg)
        !! Determines which boundary edges are visible to a point.
        !!
        !! The point (X,Y) is assumed to be outside the convex hull of
        !! the region covered by the 2D triangulation.
        real(real64), intent(in) :: x
            !! The x-coordinate of a point outside the convex hull of
            !! the current triangulation.
        real(real64), intent(in) :: y
            !! The y-coordinate of a point outside the convex hull of
            !! the current triangulation.
        integer(int32), intent(in) :: node_num
            !! The number of points.
        real(real64), intent(in), dimension(2, node_num) :: node_xy
            !! The coordinates of the vertices.
        integer(int32), intent(in) :: triangle_num
            !! The number of triangles.
        integer(int32), intent(in), dimension(3, triangle_num) :: &
            triangle_node
            !! The triangle incidence list.
        integer(int32), intent(in), dimension(3, triangle_num) :: &
            triangle_neighbor
            !! The triangle neighbor list; negative values are used for
            !! links of a counterclockwise linked list of boundary
            !! edges; LINK = -(3*I + J-1) where I, J = triangle, edge
            !! index.
        integer(int32), intent(inout) :: ltri
            !! If LTRI /= 0 then this value is assumed to be already
            !! computed and is not changed, else it is updated.  On
            !! output, LTRI is the index of the boundary triangle to the
            !! left of the leftmost boundary triangle visible from
            !! (X,Y).
        integer(int32), intent(inout) :: ledg
            !! The boundary edge of triangle LTRI to the left of the
            !! leftmost boundary edge visible from (X,Y).  1 <= LEDG <=
            !! 3.
        integer(int32), intent(inout) :: rtri
            !! On input, the index of the boundary triangle to begin the
            !! search at.  On output, the index of the rightmost
            !! boundary triangle visible from (X,Y).
        integer(int32), intent(inout) :: redg
            !! The edge of triangle RTRI that is visible from (X,Y).
            !! 1 <= REDG <= 3.

        ! Local Variables
        integer(int32) :: a, b, e, l, lr, t
        logical :: ldone

        ! Find the rightmost visible boundary edge using links, then
        ! possibly the leftmost visible boundary edge using triangle
        ! neighbor information
        if (ltri == 0) then
            ldone = .false.
            ltri = rtri
            ledg = redg
        else
            ldone = .true.
        end if

        do
            l = -triangle_neighbor(redg, rtri)
            t = l / 3
            e = mod(l, 3) + 1
            a = triangle_node(e, t)

            if (e <= 2) then
                b = triangle_node(e + 1, t)
            else
                b = triangle_node(1, t)
            end if

            lr = lrline(x, y, node_xy(1, a), node_xy(2, a), &
                node_xy(1, b), node_xy(2, b), 0.0d0)

            if (lr <= 0) exit

            rtri = t
            redg = e
        end do

        if (ldone) return

        t = ltri
        e = ledg

        do
            b = triangle_node(e, t)
            e = i4_wrap(e - 1, 1, 3)

            do while (0 < triangle_neighbor(e, t))
                t = triangle_neighbor(e, t)
                if (triangle_node(1, t) == b) then
                    e = 3
                else if (triangle_node(2, t) == b) then
                    e = 1
                else
                    e = 2
                end if
            end do

            a = triangle_node(e, t)

            lr = lrline(x, y, node_xy(1, a), node_xy(2, a), &
                node_xy(1, b), node_xy(2, b), 0.0d0)

            if (lr <= 0) exit
        end do

        ltri = t
        ledg = e
    end subroutine

! ------------------------------------------------------------------------------
    pure subroutine swapec(i, top, btri, bedg, node_num, node_xy, &
            triangle_num, triangle_node, triangle_neighbor, stack)
        !! Swaps diagonal edges until all triangles are Delaunay.
        !!
        !! The routine swaps diagonal edges in a 2D triangulation, based
        !! on the empty circumcircle criterion, until all triangles are
        !! Delaunay, given that I is the index of the new vertex added
        !! to the triangulation.
        integer(int32), intent(in) :: i
            !! The index of the new vertex.
        integer(int32), intent(inout) :: top
            !! The index of the top of the stack.  On output, TOP is
            !! zero.
        integer(int32), intent(inout) :: btri
            !! On input, if positive, the triangle index of a boundary
            !! edge whose updated index must be recorded.  On output,
            !! this may be updated because of swaps.
        integer(int32), intent(inout) :: bedg
            !! On input, if positive, the edge index of a boundary edge
            !! whose updated index must be recorded.  On output, this
            !! may be updated because of swaps.
        integer(int32), intent(in) :: node_num
            !! The number of points.
        real(real64), intent(in), dimension(2, node_num) :: node_xy
            !! The coordinates of the points.
        integer(int32), intent(in) :: triangle_num
            !! The number of triangles.
        integer(int32), intent(inout), dimension(3, triangle_num) :: &
            triangle_node
            !! The triangle incidence list.  May be updated on output
            !! because of swaps.
        integer(int32), intent(inout), dimension(3, triangle_num) :: &
            triangle_neighbor
            !! The triangle neighbor list; negative values are used for
            !! links of the counter-clockwise linked list of boundary
            !! edges.  May be updated on output because of swaps.
        integer(int32), intent(inout), dimension(node_num) :: stack
            !! On input, entries 1 through TOP contain the indices of
            !! initial triangles (involving vertex I) put in the stack;
            !! the edges opposite I should be in the interior; entries
            !! TOP+1 through NODE_NUM are used as a stack.

        ! Local Variables
        integer(int32) :: a, b, c, e, ee, em1, ep1, f, fm1, fp1, l, r, s, &
            swap, t, tt, u
        real(real64) :: x, y

        x = node_xy(1, i)
        y = node_xy(2, i)

        do
            if (top <= 0) exit

            t = stack(top)
            top = top - 1

            if (triangle_node(1, t) == i) then
                e = 2
                b = triangle_node(3, t)
            else if (triangle_node(2, t) == i) then
                e = 3
                b = triangle_node(1, t)
            else
                e = 1
                b = triangle_node(2, t)
            end if

            a = triangle_node(e, t)
            u = triangle_neighbor(e, t)

            if (triangle_neighbor(1, u) == t) then
                f = 1
                c = triangle_node(3, u)
            else if (triangle_neighbor(2, u) == t) then
                f = 2
                c = triangle_node(1, u)
            else
                f = 3
                c = triangle_node(2, u)
            end if

            swap = diaedg(x, y, node_xy(1, a), node_xy(2, a), &
                node_xy(1, c), node_xy(2, c), node_xy(1, b), &
                node_xy(2, b))

            if (swap == 1) then
                em1 = i4_wrap(e - 1, 1, 3)
                ep1 = i4_wrap(e + 1, 1, 3)
                fm1 = i4_wrap(f - 1, 1, 3)
                fp1 = i4_wrap(f + 1, 1, 3)

                triangle_node(ep1, t) = c
                triangle_node(fp1, u) = i
                r = triangle_neighbor(ep1, t)
                s = triangle_neighbor(fp1, u)
                triangle_neighbor(ep1, t) = u
                triangle_neighbor(fp1, u) = t
                triangle_neighbor(e, t) = s
                triangle_neighbor(f, u) = r

                if (0 < triangle_neighbor(fm1, u)) then
                    top = top + 1
                    stack(top) = u
                end if

                if (0 < s) then
                    if (triangle_neighbor(1, s) == u) then
                        triangle_neighbor(1, s) = t
                    else if (triangle_neighbor(2, s) == u) then
                        triangle_neighbor(2, s) = t
                    else
                        triangle_neighbor(3, s) = t
                    end if

                    top = top + 1
                    if (node_num < top) then
                        error stop "SWAPEC - Fatal error!  Stack overflow."
                    end if
                    stack(top) = t
                else
                    if (u == btri .and. fp1 == bedg) then
                        btri = t
                        bedg = e
                    end if

                    l = -(3 * t + e - 1)
                    tt = t
                    ee = em1

                    do while (0 < triangle_neighbor(ee, tt))
                        tt = triangle_neighbor(ee, tt)
                        if (triangle_node(1, tt) == a) then
                            ee = 3
                        else if (triangle_node(2, tt) == a) then
                            ee = 1
                        else
                            ee = 2
                        end if
                    end do

                    triangle_neighbor(ee, tt) = l
                end if

                if (0 < r) then
                    if (triangle_neighbor(1, r) == t) then
                        triangle_neighbor(1, r) = u
                    else if (triangle_neighbor(2, r) == t) then
                        triangle_neighbor(2, r) = u
                    else
                        triangle_neighbor(3, r) = u
                    end if
                else
                    if (t == btri .and. ep1 == bedg) then
                        btri = u
                        bedg = f
                    end if

                    l = -(3 * u + f - 1)
                    tt = u
                    ee = fm1

                    do while (0 < triangle_neighbor(ee, tt))
                        tt = triangle_neighbor(ee, tt)
                        if (triangle_node(1, tt) == b) then
                            ee = 3
                        else if (triangle_node(2, tt) == b) then
                            ee = 1
                        else
                            ee = 2
                        end if
                    end do

                    triangle_neighbor(ee, tt) = l
                end if
            end if
        end do
    end subroutine

! ------------------------------------------------------------------------------
end module
