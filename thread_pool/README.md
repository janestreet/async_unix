---
title: "Thread_pool"
uuid: 6030caa1-1213-364f-c27c-cefc797cb6ef
---

A thread pool is a set of OCaml threads used to do work, where each
piece of work is simply a thunk.

The `Async_unix` library uses `Thread_pool` to make blocking system
calls.
