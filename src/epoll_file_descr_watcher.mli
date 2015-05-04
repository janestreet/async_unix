open Core.Std

include File_descr_watcher_intf.S

(** [create ~num_file_descrs timerfd] creates a new file-descr-watcher that is able to
    watch file descriptors in {[ [0, num_file_descrs) ]}, and is also watching
    [timerfd]. *)
val create : num_file_descrs:int -> Linux_ext.Timerfd.t -> t
