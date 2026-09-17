(ns hyperlith.impl.lane-context)

(defrecord LaneCtx
    [exec dbs attr-cache attr-name-cache attr-byte-scratch
     zstd-src-buf zstd-dst-buf])
