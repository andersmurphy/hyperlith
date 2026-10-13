set -o errexit

cd sqlite-amalgamation

for target in aarch64-linux-gnu \
                  aarch64-macos-none \
                  x86_64-linux-gnu \
                  x86_64-macos-none \
                  x86_64-windows-gnu
do
    if [[ "$target" == *-windows-* ]]; then
        dl=''
        extension="dll"
        symbolic=''
    else
        dl='-ldl'
        extension="so"
        symbolic=''
        if [[ "$target" == *-linux-* ]]; then
            symbolic='-Wl,-Bsymbolic'
        fi
    fi
    echo "##### Building $target ####"
    zig cc -shared -Os -I. -fPIC -DSQLITE_DQS=0 \
        -DSQLITE_THREADSAFE=2 \
        -DSQLITE_DEFAULT_MEMSTATUS=0 \
        -DSQLITE_DEFAULT_WAL_SYNCHRONOUS=1 \
        -DSQLITE_LIKE_DOESNT_MATCH_BLOBS \
        -DSQLITE_MAX_EXPR_DEPTH=0 \
        -DSQLITE_OMIT_DEPRECATED \
        -DSQLITE_OMIT_PROGRESS_CALLBACK \
        -DSQLITE_OMIT_SHARED_CACHE \
        -DSQLITE_USE_ALLOCA \
        -DSQLITE_STRICT_SUBTYPE=1 \
        -DSQLITE_OMIT_AUTOINIT \
        -DSQLITE_DISABLE_PAGECACHE_OVERFLOW_STATS \
        -DSQLITE_ENABLE_STAT4 \
        -DSQLITE_ENABLE_RTREE \
        -DSQLITE_ENABLE_FTS5 \
        -DSQLITE_MAX_MMAP_SIZE=1099511627776 \
        sqlite3.c -lpthread $dl -lm $symbolic -o sqlite3.so -target $target
    cp -v sqlite3.so ../resources/sqlite3_$target.$extension
done

ls -l ../resources
