(ns hyperlith.impl.lane-context)

(defrecord LaneCtx
    [exec dbs attr-cache attr-name-cache attr-byte-scratch
     html-dst-buf zstd-src-buf])
