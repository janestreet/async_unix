## 111.13.00

- Added `Scheduler.yield_every`, which returns a function that calls
  `Scheduler.yield` every n-th call.

    This is useful in circumstances where you don't have strict control
    over where a deferred is examined, as in a `Deferred.List.iter`.

## 111.11.00

- Added `val Scheduler.yield : unit -> unit Deferred.t`, which becomes
  determined after the current cycle finishes.
- Improved the behavior of the scheduler's thread pool when
  `Thread.create` raises.

  With this improvement, when the thread pool is unable to create a
  thread, it presses on with the threads it has rather than raise.
  Subsequent attempts to add work to the thread pool will cause the
  pool to attempt to create the thread, as long as enough time has
  passed since the most recent thread-creation failure.

  Before this change, the thread pool wouldn't handle a
  `Thread.create` exception, and the exception would get raised to
  whatever code happened to be calling the `Thread_pool` function that
  tried to create a thread, e.g. `Thread_pool.add_work`.  This caused
  `In_thread.run` to unexpectedly raise, and in turn
  `In_thread.syscall` to unexpectedly raise, leading to:

  ```
  "Fd.syscall_in_thread bug -- should be impossible"
  ```

  Also, changed `should be impossible` text to `please report`, since
  there may be other lurking rare exceptions that `In_thread.syscall`
  can raise, and we'd like to hear about them.

  We rely on thread-pool-stuck detection to report problems where the
  inability to create threads causes the inability of the thread pool
  to make progress.  A tweak was needed to make that work -- now the
  thread-pool-stuck warning is based on whether the thread pool has
  unstarted work, rather than on whether the thread pool has an
  "available thread".  The latter would no longer work, since it is
  now possible for the thread pool to have unstarted work and to
  appear to have an available thread, i.e. `num_threads <
  max_num_threads`.

## 111.08.00

- Added `Sys.when_file_changes : string -> Time.t Pipe.Reader.t`.
- Added `Time.now ()` to some error messages.

## 111.06.00

- In the `Busy_pollers.t` record, made the `kernel_scheduler` field be
  `sexp_opaque`.

  Did this so that one doesn't get two copies of the kernel scheduler
  in sexps of the scheduler, which already has its own
  `kernel_scheduler` field.

## 111.03.00

- Improved `Socket.accept` to abort and return `` `Socket_closed`` when
  the file descriptor underlying the socket is closed.
- Added to `Socket.bind` a `?reuseaddr:bool` argument, preserving the
  default as `true`.
- Added `Fd.close_started`, which becomes determined when `close` is
  called on an `Fd.t`.

## 109.60.00

- Fixed a bug in detection of the thread pool being stuck that could
  overstate the amount of time the pool was stuck.

    It had been incorrectly reporting the duration of the thread pool
    being stuck if the pool had no work in it and then got enough jobs
    to be stuck.  In that situation, the duration included the time span
    before the pool got stuck.  If the pool had been idle long enough,
    this could even spuriously abort the program.

## 109.58.00

- Improved fairness of the async scheduler with respect to external
  threads, including I/O done in external threads.

  The change is to add a thread-safe queue of "external actions" that
  is checked after each job.

  Previously, when a job given to `In_thread.run` finished,
  `In_thread.run` would take the async lock, fill the result ivar and
  run a cycle.  The problem is that in some situations, due to poor OS
  scheduling, the helper thread never had a chance to grab the lock.
  Now, `In_thread.run` tries to take the lock:

  - if it can it does as before
  - if it can't it enqueues a thunk in the external actions queue and
    wakes up the scheduler

  With this change, the helper thread doing an `In_thread.run` will
  always quickly finish once the work is done, and the async scheduler
  will fill in the result ivar as soon as the current job finishes.
- Fixed `Epoll_file_descr_watcher.invariant` to deal with the timerfd,
  which has the edge-triggered flag set.
- Added `Writer.write_gen`, a generic functor for blitting directly to
  a writer's buffer.

## 109.55.00

- Fixed `Scheduler.is_running` to not initialize the scheduler.
- Added `Writer.make_write`, which is a general function for blitting
  directly to a writer's buffer.
- Added `Writer.transfer'`, which is like `Writer.transfer` but allows
  async actions in the callback.

  This was requested in pull request #1.
- Added `Writer.write_iobuf`, which blits the contents of an iobuf
  directly to a writer's buffer.

## 109.53.00

- Changed the scheduler to calibrate `Time_stamp_counter` every
  second.
- Improved error messages in the scheduler when `epoll` functions
  raise.
- Made `Scheduler.reset_in_forked_process` close the `epoll` file
  descriptor.

## 109.52.00

- Fixed a bug in `Unix.mkstemp`, which had a race because it used
  `Fd.create` in a thread.

  This bug affected `Writer.with_file_atomic`, `save`, `save_lines`,
  and `save_sexp`, and could cause corruption of the async scheduler
  data structures.
- Changed async to never do `set_nonblock` on `std{in,out,err}`, which
  allows `Core` I/O to use the standard file descriptors
  simultaneously with async.

  Before this change, the Core I/O libraries could (and sometimes did)
  fail due to `Sys_blocked_io`.
- Changed `Pipe.iter_without_pushback` to never call `f` after
  `Pipe.close_read` has been called.

  The new behavior is like `Pipe.iter`.

  Changed the implementation of `Pipe.fold_gen` and `Pipe.transfer_gen`
  to be analogous to `Pipe.iter_without_pushback`, and in particular to
  process as many elements as possible before calling `values_available`.
- Added `?expand_macros:bool` argument to `Reader.load_sexp*`
  functions, to support the new `Sexplib` macros.
- Added an optional argument to `Process.run` to accept nonzero exits
  as successful runs.

## 109.47.00

- Added `Socket.Address.Inet.to_host_and_port`.
- Changed `Fd_by_descr` so that it actually calls `Bounded_int_table.invariant`.

## 109.45.00

- Added `Fd.every_ready_to` `Fd.interruptible_every_ready_to` which
  register a callback to be called every time the fd becomes ready.

  These can significantly reduce allocation.
- Renamed `Fd.ready_to_interruptible` as `Fd.interruptible_ready_to`.
- Changed `Fd.ready_fold` to use `Fd.interruptible_ready_to`, to
  improve its performance.

## 109.42.00

- Added `Reader.drain`.

  ```ocaml
  val drain : t -> unit Deferred.t
  ```

- Added `Writer.with_close`.

  ```ocaml
  val with_close : t -> f:(unit -> 'a Deferred.t) -> 'a Deferred.t
  ```

## 109.41.00

- Changed `Reader` to treat more `errno` responses to a `read()` system call as `EOF` rather than raise.

  The following are now treated as `EOF`.

  ```
  ENETDOWN
  ENETRESET
  ENETUNREACH
  ETIMEDOUT
  ```

- Improved the error message that async prints when the thread pool is stuck, including the time of the error.

## 109.40.00

- Added value `Socket.Type.unix_dgram`, which represents a UNIX domain
  datagram socket.
- Added UDP socket functionality: `Socket.Opt.mcast_{loop,ttl}` and
  `Socket.mcast_{join,leave}`.
- Improved `Fd.ready_fold` to accept `?stop:(unit Deferred.t)` rather
  than `?stop:('a -> bool)`.

## 109.39.00

- Added "thread-local" storage, `Scheduler.{find,with}_local`, for
 `LWT` emulation.

  ```ocaml
  (** [with_local key value ~f] runs [f] right now with the binding [key ` value].  All
      calls to [find_local key] in [f] and computations started from [f] will return
      [value]. *)
  val with_local : 'a Univ_map.Key.t -> 'a option -> f:(unit -> 'b) -> 'b

  (** [find_local key] returns the value associated to [key] in the current execution
      context. *)
  val find_local : 'a Univ_map.Key.t -> 'a option
  ```

## 109.38.00

- Added `Reader.of_pipe` and `Writer.of_pipe`, for converting from `string Pipe.t`'s.

  These can be used to add arbitrary transformations (e.g. encryption, compression)
  to code that expects plain file- or socket-based readers and writers.

## 109.36.00

- Added `Process.run_lines`, which runs a process and returns stdout as a list of strings.

## 109.35.00

- Made some configuration possible via additional optional arguments
  to `go_main`.

  ```ocaml
  ?max_num_open_file_descrs:int
  ?max_num_threads:int
  ```
- Made some aspects of the async scheduler configurable via functions
  in `Scheduler`.

  ```ocaml
  val set_check_invariants                  : bool        -> unit
  val set_detect_invalid_access_from_thread : bool        -> unit
  val set_max_inter_cycle_timeout           : Time.Span.t -> unit
  val set_record_backtraces                 : bool        -> unit
  ```
- Added a dynamic check in `Pipe` that a consumer is used with the
  correct pipe.

  Specifically, check that a consumer supplied to a read operation on
  a `Pipe.Reader.t` was previously created by `add_consumer` with that
  same reader.
- Renamed `Pipe.fold` as `fold_without_pushback` and added `Pipe.fold`
  with an analogous type to `Pipe.iter`.
- Fixed a bug in `Pipe.merge`, which did not always close the
  resulting pipe when the merge was finished.

  This had prevented medusa regtests from working correctly.
- In `Writer`, changed the default `buffer_age_limit` for files to
  ``Unlimited`.

  THis was done for the same reason that we treat files specially in
  flush on close -- slowness will likely be resolved eventually with a
  file, unlike with a socket.

## 109.34.00

- Changed the scheduler to detect when the thread pool is stuck, i.e. when all threads are blocked and haven't made progress.

  Added default handlers for when the thread pool is stuck, and the
  ability for the user to configure their own handlers.

- Changed low-level I/O to not use nonblocking I/O for char devices.

  This fixes a problem due to `epoll` not working with `/dev/null`.  For
  example:

  ```ocaml
  let () =
    Reader.read_line (Lazy.force Reader.stdin)
    >>> fun _ ->
    shutdown 0
  ;;
  let () = never_returns (Scheduler.go ())
  ```

  had failed when run like:

  ```
  + ./z.exe </dev/null
  ("bug in async scheduler"
   ((Unix.Unix_error "Operation not permitted" epoll_ctl "")
  ...
  ```

- Made `Unix.openfile` use `fstat` to determine the kind of file for a file descriptor rather than assuming it's a regular file.

- Improved the `ASYNC_CONFIG` option `detect_invalid_access_from_thread` by having it include a backtrace in the error message.

## 109.31.00

- Renamed `Reader.read_one_chuck_at_a_time_until_eof` as `Reader.read_one_chunk_at_a_time`,
  and added a new case to the `handle_chunk` callback.

  The name change is to reflect that one can stop early, before EOF.

  The new `handle_chunk` case allows one to specify the number of bytes
  that were read in the `Stop` case.

  Also, changed `read_one_chunk_at_a_time` to use `do_read` instead of
  just locking the file without unlocking it.  This allows subsequent
  read operations to read from the file.

## 109.30.00

- Changed aync's scheduler to use `epoll` rather than `select` by default.

  This is based on a dynamic test to see whether `timerfd_create` works.

- Added support for "busy polling".

  This runs a thread that busy loops running user-supplied polling
  functions.  The busy-loop thread is distinct from async's scheduler
  thread, but it acquires the async lock so that the user-supplied
  function can do ordinary async operations, e.g. fill an ivar.

  Busy polling is useful for a situation like a shared-memory ringbuffer
  being used for IPC.  One can poll the ringbuffer with a busy poller,
  and then when data is detected, fill some ivar that causes async code
  to handle the data.

- Added `Async.Fd.clear_nonblocking`.

  This clears the nonblocking bit on the file descriptor underlying the
  fd, and causes async to treat the fd as though it doesn't support
  nonblocking I/O.

  This is useful for applications that want to share a file descriptor
  between async and non-async code and want to avoid `Sys_blocked_io`
  being seen by the non-async code.

## 109.27.00

- Fixed a performance problem in the scheduler due to repeated calls
  of `Timing_wheel.min_elt`.

  `Timing_wheel.min_elt` is an important part of async, since the
  scheduler calls it once per cycle to know when to timeout for
  `epoll` or `select`.  This causes a problem if `min_elt` is slow and
  called repeatedly, which happens in an application where the next
  clock event is a second out, and yet there are lots of cycles per
  second.

  `Timing_wheel.min_elt` now caches the minimum element, which
  eliminates the problem.
- Fixed async's clock to work on 32-bit machines.

  With the change to `Timing_wheel` in 109.22, async no longer worked
  on 32-bit machines, due to the clock overflowing.  This is because
  it is initialized to `Time.epoch`, and can only handle 6 days.

  The fix now in place is to start the clock at `Time.now ()` rather
  than `Time.epoch`.
- Added many functions to `Async.Sys` so that it looks more like
  `Core.Sys`.
- Changed `Reader.read_one_chunk_at_a_time_until_eof` to not destroy
  the reader buffer.

  Destroying the buffer failed if user code held on to the buffer.

## 109.24.00

- Changed `Reader.close` so that it frees the reader buffer using `Bigstring.unsafe_destroy`.

  This is an improvement over the previous situation, in which the
  buffer wasn't freed until its finalizer fired.

- Fixed a bug in `Reader.read_bin_prot`.

  It was missing a try-with that could cause it to raise without
  cleaning up the reader.

## 109.21.00

- Added `Unix.remove`.

## 109.20.00

- Set `close-on-exec` for both ends of the pipe used to wake up the scheduler.

## 109.19.00

- Reworked a number of `Reader` functions to improve performance by
  avoiding deferreds.

  This is a followup to the `Reader` improvements in 109.14, and
  eliminates some last vestiges of performance degradation that had
  been introduced in 109.04.
- Added function `Reader.lseek : t -> int64 -> mode:[< `Set | `End] ->
  int64 Deferred.t`.

  `lseek t offset ~mode` clears `t`'s buffer and calls `Unix.lseek` on
  `t`'s file descriptor.
- Added function `Writer.bytes_received : t -> int`.
- Added function `Unix.mkfifo : ?perm:file_perm -> string -> unit
  Deferred.t`, which was mistakenly missing.

  This is a simple wrapper around `Core.Unix.mkfifo`.

## 109.18.00

- added `Async.Unix.fcntl_{get,set}fl`.

  Made `Reader` and `Writer` detect if they are passed a file
  descriptor with incorrect permissions (`O_WRONLY` for `Reader`,
  `O_RDONLY` for `Writer`).

## 109.15.00

- The `epoll`-based scheduler now supports sub-millisecond timeouts,
  using `Linux_ext.Timerfd`.

  Async still uses the `select`-based scheduler by default.  We plan
  to switch the default to `epoll` in a few weeks, once we have done
  more testing.
- Eliminated module `Work_group`, which was for limiting the number of
  threads used by jobs.

  This was a little-used module that significantly complicated the
  implementation of the Async thread pool.

  One should consider using a `Throttle` instead.

  Along the way, fixed a bug in Async helper threads in which the
  finalizer could fire too early, causing an unhandled exception.  The
  fix involves relaxing the requirements on when
  `Thread_pool.finished_with_helper_thread` functions can be called,
  allowing it to be called while the helper thread still has work, but
  so long as no future work will be added.

## 109.14.00

- Fixed major performance degradation (since 109.04) in `Reader.read*`
  functions.
- Added function `Rpc.Implementation.map_inv`.

  ```ocaml
  val map_inv : 'a t -> f:('b -> 'a) -> 'b t
  ```
- Add functions `Reader.file_lines` and `Writer.save_lines`.

  These deal with files as lists of their lines.

  ```ocaml
  val Reader.file_lines : string -> string list Deferred.t
  val Writer.save_lines : string -> string list -> unit Deferred.t
  ```
- Added a `?wakeup_scheduler:bool` optional argument to functions in
  the `Thread_safe` module.

  The default is `true`, which continues the behavior that has been in place since 109.09.
  However, once can use `~wakeup_scheduler:false` to reduce CPU use, in return for increased
  latency (because the scheduler won't run a cycle immediately).

## 109.13.00

- Added `Writer.write_line`, which is `Writer.write` plus a newline at
  the end.
- Added `?close_on_exec:bool` argument to `{Reader,Writer}.open_file`
  and `Async.Unix.open_file`.

  Made the default `close_on_exec:true` for `Reader` and `Writer`.
- Added a `compare` function to `Socket.Address.Inet`.

## 109.12.00

- Fixed a bug in `Fd.syscall_in_thread`.

  The bug could cause:

  ```ocaml
  Fd.syscall_in_thread bug -- should be impossible
  ```

  The bug was that `syscall_in_thread` raised rather than returning `Error`.
- Changed `Tcp.connect` and `Tcp.with_connect` to also supply the connected socket.

  Supplying the connected socket makes it easy to call `Socket`
  functions, e.g.  to find out information about the connection with
  `Socket.get{peer,sock}name`.  This also gives information about the IP
  address *after* DNS, which wouldn't otherwise be available.

  One could reconstruct the socket by extracting the fd from the
  writer, and then calling `Socket.of_fd` with the correct
  `Socket.Type`.  But that is both error prone and not discoverable.
- Added `Writer.schedule_bigsubstring`, which parallels
  `Writer.schedule_bigstring`.

## 109.11.00

- Added a check to fail if `Scheduler.go` is called more than once.

## 109.10.00

- Added `Shutdown.do_not_finish_shutdown_before`.  This allows one to
  add `unit Deferred.t`'s that will delay the `shutdown` from
  finishing.  The implementation is more efficient than using
  `at_shutdown`.

## 109.09.00

- Added module `Thread_safe_pipe`, for streaming data outside async into async.
  This a more efficient and feature-ful way to send a sequence of values
  from outside async into async than `Thread_safe.pipe`, which has been
  eliminated.
- Changed functions in `Thread_safe` to always wake up the scheduler.
  Changed `Thread_safe.run_in_async{,_exn}` to not run a cycle, and
  instead rely on the scheduler to run the cycle.

## 109.08.00

- Added module `Async.Process`
  This is a new module for creating and dealing with child processes.
- For `Writer.save`, replaced the `temp_prefix` argument with `temp_file`.
- Added `Ivar.invariant` function.
- Added value `Scheduler.fold_fields`
  This lets one fold over the fields in the scheduler, eliminates an
  annoying place in catalog browser that reached into the internals of
  async to compute the sizes of the scheduler fields

## 109.07.00

- Changed the async scheduler so that if there are no upcoming events,
  it times out in 50ms rather than waiting forever.
- Improved `Reader.read_one_chunk_at_a_time_until_eof`:
  - the callback need not consume everything
  - add `\`Eof_with_unconsumed_data` as a possible result
  - grow internal buffer of the reader when needed
- Added `Shutdown.exit`, removed `Shutdown.shutdown_and_raise`.
- Added `Scheduler.force_current_cycle_to_end`.

