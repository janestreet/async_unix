open! Core
open! Import
module Writer_id = Unique_id.Int63 ()
module Reader_id = Unique_id.Int63 ()

module rec Writer : sig
  module Flush_result : sig
    type t =
      | Error
      | Consumer_left
      | Force_closed
      | Flushed of Time_ns.t
  end

  module Line_ending : sig
    type t =
      | Dos
      | Unix
  end

  module Check_buffer_age' : sig
    type 'a per_time_source =
      { active_checks : 'a t Bag.t
      ; closed : unit Ivar.t
      }

    and 'a t =
      { writer : 'a
      ; maximum_age : Time_ns.Span.t
      ; mutable bytes_received_at_now_minus_maximum_age : Int63.t
      ; bytes_received_queue : Int63.t Queue.t
      ; times_received_queue : Time_ns.t Queue.t
      ; mutable bytes_seen : Int63.t
      ; mutable too_old : unit Ivar.t
      ; for_this_time_source : 'a per_time_source
      }
  end

  module Open_flags : sig
    type t = Core_unix.Open_flags.t
  end

  type open_flags =
    [ `Already_closed
    | `Ok of Open_flags.t
    | `Error of exn
    ]

  module Destroy_or_keep : sig
    type t =
      | Destroy
      | Keep
  end

  module Scheduled : sig
    type t = (Bigstring.t Core_unix.IOVec.t * Destroy_or_keep.t) Deque.t
  end

  module Stop_reason : sig
    type t =
      | Error
      | Closed
      | Consumer_left
  end

  type t =
    { id : Writer_id.t
    ; mutable fd : Fd.t
    ; monitor : Monitor.t
    ; inner_monitor : Monitor.t
    ; mutable background_writer_state :
        [ `Running | `Not_running | `Stopped_permanently of Stop_reason.t ]
    ; background_writer_stopped : unit Ivar.t
    ; syscall : [ `Per_cycle | `Periodic of Time_float.Span.t ]
    ; mutable bytes_received : Int63.t
    ; mutable bytes_written : Int63.t
    ; scheduled : Scheduled.t
    ; mutable scheduled_bytes : int
    ; mutable buf : Bigstring.t
    ; mutable scheduled_back : int
    ; mutable back : int
    ; time_source : Time_source.t
    ; flushes : (Flush_result.t Ivar.t * Int63.t) Queue.t
    ; mutable close_state : [ `Open | `Closed_and_flushing | `Closed ]
    ; close_finished : unit Ivar.t
    ; close_started : unit Ivar.t
    ; producers_to_flush_at_close : (unit -> unit Deferred.t) Bag.t
    ; mutable flush_at_shutdown_elt : t Bag.Elt.t option
    ; mutable check_buffer_age : t Check_buffer_age'.t Bag.Elt.t option Lazy.t
    ; consumer_left : unit Ivar.t
    ; mutable raise_when_consumer_leaves : bool
    ; open_flags : open_flags
    ; line_ending : Line_ending.t
    ; mutable backing_out_channel : Backing_out_channel.t option
    }
end =
  Writer

and Reader : sig
  module State : sig
    type t =
      [ `Not_in_use
      | `In_use
      | `Closed
      ]
  end

  module Open_flags : sig
    type t = Core_unix.Open_flags.t
  end

  type open_flags =
    [ `Already_closed
    | `Ok of Open_flags.t
    | `Error of exn
    ]

  type t =
    { fd : Fd.t
    ; id : Reader_id.t
    ; mutable bytes_read : Int63.t
    ; mutable buf : Bigstring.t
    ; mutable close_may_destroy_buf : [ `Yes | `Not_now | `Not_ever ]
    ; mutable pos : int
    ; mutable available : int
    ; mutable state : State.t
    ; close_finished : unit Ivar.t
    ; mutable last_read_time : Time.t
    ; open_flags : open_flags
    }
end =
  Reader

and Fd : sig
  module Kind : sig
    type t =
      | Char
      | Fifo
      | File
      | Socket of [ `Unconnected | `Bound | `Passive | `Active | `Unknown ]
  end

  module State : sig
    type t =
      | Close_requested of Execution_context.t * (unit -> unit)
      | Closed
      | Open of unit Ivar.t
  end

  type ready_to_result =
    [ `Ready
    | `Bad_fd
    | `Closed
    | `Interrupted
    | `Unsupported
    ]

  module Watching : sig
    type t =
      | Not_watching
      | Watch_once of ready_to_result Ivar.t
      | Watch_repeatedly of
          { job : Job.t
          ; finished_ivar : [ `Bad_fd | `Closed | `Interrupted | `Unsupported ] Ivar.t
          ; pending : unit -> bool
          }
      | Stop_requested
  end

  module Nonblock_status : sig
    type t =
      | Blocking
      | Nonblocking
      | Unknown
  end

  type t =
    { file_descr : File_descr.t
    ; mutable info : Info.t
    ; mutable kind : Kind.t
    ; mutable can_set_nonblock : bool
    ; mutable nonblock_status : Nonblock_status.t
    ; mutable state : State.t
    ; watching : Watching.t Read_write_pair.Mutable.t
    ; mutable watching_has_changed : bool
    ; mutable num_active_syscalls : int
    ; close_finished : unit Ivar.t
    }
end =
  Fd

and Interruptor : sig
  type phase =
    | Sleeping
    | Awake
    | Interrupted

  type t =
    { read_fd : Fd.t Capsule.Initial.Data.t
    ; write_fd : File_descr.t
    ; phase : phase Atomic.t
    ; clearbuffer : Bytes.t
    }
end =
  Interruptor

and By_descr : sig @@ portable
  type 'a t
  type 'a repr = 'a Option_array.t

  external to_repr : 'a t -> 'a repr = "%identity"
  external of_repr : 'a repr -> 'a t = "%identity"
end =
  By_descr

and Scheduler : sig
  module File_descr_watcher : sig
    module type S = sig
      include File_descr_watcher_intf.S

      val watcher : t
    end

    type t = (module S)
  end

  type start_type =
    | Not_started
    | Called_go of Source_code_position.t
    | Called_block_on_async of Source_code_position.t
    | Called_external_run of
        { active : bool ref
        ; call_pos : Source_code_position.t
        }

  module External_fd_event : sig
    type t =
      { file_descr : File_descr.t
      ; read_or_write : Read_write_pair.Key.t
      ; event_type : [ `Ready | `Bad_fd ]
      }
  end

  module Thread_pool_stuck_status : sig
    type t =
      | No_unstarted_work
      | Stuck of
          { stuck_since : Time_ns.t
          ; num_work_completed : int
          }
  end

  module Extra_event_source_result : sig
    type t =
      | Quiescent
      | Active
  end

  type t =
    { mutex : Nano_mutex.t
    ; mutable start_type : start_type
    ; fds_whose_watching_has_changed : Fd.t Stack.t
    ; file_descr_watcher : File_descr_watcher.t
    ; busy_pollers : Busy_poller.packed Uniform_array.t
    ; mutable num_busy_pollers : int
    ; mutable extra_event_sources : (unit -> Extra_event_source_result.t) Uniform_array.t
    ; mutable time_spent_waiting_for_io : Time_stamp_counter.Span.t
    ; fd_by_descr : Fd.t By_descr.t
    ; external_fd_by_descr : bool Read_write_pair.t By_descr.t
    ; mutable external_fd_events : External_fd_event.t list
    ; mutable timerfd : Linux_ext.Timerfd.t option
    ; mutable timerfd_set_at : Time_ns.t
    ; mutable scheduler_thread_id : int
    ; interruptor : Interruptor.t
    ; signal_manager : Signal_manager.t
    ; thread_pool : Thread_pool.t
    ; mutable handle_thread_pool_stuck : Thread_pool.t -> stuck_for:Time_ns.Span.t -> unit
    ; mutable thread_pool_stuck : Thread_pool_stuck_status.t
    ; dns_lookup_throttle : unit Throttle.t
    ; mutable next_tsc_calibration : Time_stamp_counter.t
    ; kernel_scheduler : Kernel_scheduler.t
    ; mutable have_lock_do_cycle : (unit -> unit) option
    ; mutable max_inter_cycle_timeout : Max_inter_cycle_timeout.t
    ; mutable min_inter_cycle_timeout : Min_inter_cycle_timeout.t
    ; initialized_at : Backtrace.t
    ; uring : Io_uring_raw.t option
    }

  and the_one_and_only =
    | Not_ready_to_initialize of unit
    | Ready_to_initialize of (unit -> t)
    | Initialized of t
end =
  Scheduler
