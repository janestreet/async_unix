include File_descr_watcher_intf.S

(** [create ~num_file_descrs] creates a new file-descr-watcher that is able to watch
    file descriptors in {[ [0, num_file_descrs) ]}. *)
val create : num_file_descrs:int -> t

