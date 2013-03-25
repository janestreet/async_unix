open Core.Std

include File_descr_watcher_intf.S

(** Returns the underlying epoll value. *)
val epoll : t -> Linux_ext.Epoll.t
