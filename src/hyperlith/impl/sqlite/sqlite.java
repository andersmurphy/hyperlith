package hyperlith.impl.sqlite;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.lang.foreign.*;
import java.lang.invoke.MethodHandle;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Locale;

public final class Sqlite {
    private Sqlite() {}

    private static final Linker LINKER = Linker.nativeLinker();
    private static final SymbolLookup LOOKUP = loadBundledLibrary();

    private static final MemoryLayout PTR = ValueLayout.ADDRESS;
    private static final MemoryLayout INT = ValueLayout.JAVA_INT;
    private static final MemoryLayout LONG = ValueLayout.JAVA_LONG;
    private static final MemoryLayout DOUBLE = ValueLayout.JAVA_DOUBLE;

    private static String resourceName() {
        String os = System.getProperty("os.name").toLowerCase(Locale.ROOT);
        String osKey = os.contains("win") ? "windows"
            : os.contains("nux") ? "linux"
            : os.contains("mac") ? "macos"
            : "unknown";
        String key = System.getProperty("os.arch") + "-" + osKey;
        return switch (key) {
        case "aarch64-linux" -> "sqlite3_aarch64-linux-gnu.so";
        case "aarch64-macos" -> "sqlite3_aarch64-macos-none.so";
        case "x86-linux", "x86_64-linux", "amd64-linux" -> "sqlite3_x86_64-linux-gnu.so";
        case "x86-macos", "x86_64-macos", "amd64-macos" -> "sqlite3_x86_64-macos-none.so";
        case "x86-windows", "x86_64-windows", "amd64-windows" -> "sqlite3_x86_64-windows-gnu.dll";
        default -> throw new UnsupportedOperationException("No bundled sqlite for " + key);
        };
    }

    private static SymbolLookup loadBundledLibrary() {
        String res = resourceName();
        try (InputStream in = Sqlite.class.getClassLoader().getResourceAsStream(res)) {
            if (in == null) throw new IllegalStateException("Missing resource: " + res);
            Path tmp = Files.createTempFile("sqlite4clj_", "_" + res);
            try {
                Files.copy(in, tmp, StandardCopyOption.REPLACE_EXISTING);
                return SymbolLookup.libraryLookup(tmp, Arena.global());
            } finally {
                Files.deleteIfExists(tmp);
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static MemorySegment find(String name) {
        return LOOKUP.find(name).orElseThrow();
    }

    private static MethodHandle fn(String name, MemoryLayout ret, MemoryLayout... args) {
        return LINKER.downcallHandle(find(name), FunctionDescriptor.of(ret, args));
    }

    private static MethodHandle voidFn(String name, MemoryLayout... args) {
        return LINKER.downcallHandle(find(name), FunctionDescriptor.ofVoid(args));
    }

    private static final MethodHandle INITIALIZE = fn("sqlite3_initialize", INT);
    static {
        try {
            int rc = (int) INITIALIZE.invokeExact();
            if (rc != 0) throw new IllegalStateException("sqlite3_initialize returned " + rc);
        } catch (RuntimeException | Error e) {
            throw e;
        } catch (Throwable t) {
            throw new IllegalStateException(t);
        }
    }

    private static final MethodHandle FREE = voidFn("sqlite3_free", PTR);
    public static void free(MemorySegment p) throws Throwable {
        FREE.invokeExact(p);
    }

    private static final MethodHandle ERRMSG = fn("sqlite3_errmsg", PTR, PTR);
    public static MemorySegment errmsg(MemorySegment db) throws Throwable {
        return (MemorySegment) ERRMSG.invokeExact(db);
    }
    
    private static final MethodHandle ERRSTR = fn("sqlite3_errstr", PTR, INT);
    public static MemorySegment errstr(int code) throws Throwable {
        return (MemorySegment) ERRSTR.invokeExact(code);
    }    
    
    private static final MethodHandle OPEN_V2 = fn("sqlite3_open_v2", INT, PTR, PTR, INT, PTR);
    public static int openV2(MemorySegment filename, MemorySegment ppDb, int flags, MemorySegment vfs) throws Throwable {
        return (int) OPEN_V2.invokeExact(filename, ppDb, flags, vfs);
    }
    
    private static final MethodHandle CLOSE = fn("sqlite3_close", INT, PTR);
    public static int close(MemorySegment db) throws Throwable {
        return (int) CLOSE.invokeExact(db);
    }

    private static final MethodHandle PREPARE_V3 = fn("sqlite3_prepare_v3", INT, PTR, PTR, INT, INT, PTR, PTR);
    public static int prepareV3(MemorySegment db, MemorySegment sql, int nByte, int flags,
                                MemorySegment ppStmt, MemorySegment pzTail) throws Throwable {
        return (int) PREPARE_V3.invokeExact(db, sql, nByte, flags, ppStmt, pzTail);
    }

    private static final MethodHandle RESET = fn("sqlite3_reset", INT, PTR);
    public static int reset(MemorySegment stmt) throws Throwable {
        return (int) RESET.invokeExact(stmt);
    }

    private static final MethodHandle CLEAR_BINDINGS = fn("sqlite3_clear_bindings", INT, PTR);
    public static int clearBindings(MemorySegment stmt) throws Throwable {
        return (int) CLEAR_BINDINGS.invokeExact(stmt);
    }

    private static final MethodHandle STEP = fn("sqlite3_step", INT, PTR);
    public static int step(MemorySegment stmt) throws Throwable {
        return (int) STEP.invokeExact(stmt);
    }
    
    private static final MethodHandle FINALIZE       = fn("sqlite3_finalize", INT, PTR);
    public static int finalizeStmt(MemorySegment stmt) throws Throwable {
        return (int) FINALIZE.invokeExact(stmt);
    }

    private static final MethodHandle BIND_INT64 = fn("sqlite3_bind_int64", INT, PTR, INT, LONG);
    public static int bindInt64(MemorySegment stmt, int i, long v) throws Throwable {
        return (int) BIND_INT64.invokeExact(stmt, i, v);
    }

    private static final MethodHandle BIND_DOUBLE = fn("sqlite3_bind_double", INT, PTR, INT, DOUBLE);
    public static int bindDouble(MemorySegment stmt, int i, double v) throws Throwable {
        return (int) BIND_DOUBLE.invokeExact(stmt, i, v);
    }

    private static final MethodHandle BIND_NULL = fn("sqlite3_bind_null", INT, PTR, INT);
    public static int bindNull(MemorySegment stmt, int i) throws Throwable {
        return (int) BIND_NULL.invokeExact(stmt, i);
    }

    private static final MethodHandle BIND_TEXT = fn("sqlite3_bind_text", INT, PTR, INT, PTR, INT, PTR);
    public static int bindText(MemorySegment stmt, int i, MemorySegment text, int n,
                               MemorySegment destructor) throws Throwable {
        return (int) BIND_TEXT.invokeExact(stmt, i, text, n, destructor);
    }

    private static final MethodHandle BIND_BLOB = fn("sqlite3_bind_blob", INT, PTR, INT, PTR, INT, PTR);
    public static int bindBlob(MemorySegment stmt, int i, MemorySegment blob, int n,
                               MemorySegment destructor) throws Throwable {
        return (int) BIND_BLOB.invokeExact(stmt, i, blob, n, destructor);
    }

    private static final MethodHandle COLUMN_COUNT = fn("sqlite3_column_count", INT, PTR);
    public static int columnCount(MemorySegment stmt) throws Throwable {
        return (int) COLUMN_COUNT.invokeExact(stmt);
    }

    private static final MethodHandle COLUMN_DOUBLE = fn("sqlite3_column_double", DOUBLE, PTR, INT);
    public static double columnDouble(MemorySegment stmt, int i) throws Throwable {
        return (double) COLUMN_DOUBLE.invokeExact(stmt, i);
    }

    private static final MethodHandle COLUMN_INT64 = fn("sqlite3_column_int64", LONG, PTR, INT);
    public static long columnInt64(MemorySegment stmt, int i) throws Throwable {
        return (long) COLUMN_INT64.invokeExact(stmt, i);
    }

    private static final MethodHandle COLUMN_TEXT = fn("sqlite3_column_text", PTR, PTR, INT);
    public static MemorySegment columnText(MemorySegment stmt, int i) throws Throwable {
        return (MemorySegment) COLUMN_TEXT.invokeExact(stmt, i);
    }

    private static final MethodHandle COLUMN_BYTES = fn("sqlite3_column_bytes", INT, PTR, INT);
    public static int columnBytes(MemorySegment stmt, int i) throws Throwable {
        return (int) COLUMN_BYTES.invokeExact(stmt, i);
    }

    private static final MethodHandle COLUMN_BLOB = fn("sqlite3_column_blob", PTR, PTR, INT);
    public static MemorySegment columnBlob(MemorySegment stmt, int i) throws Throwable {
        return (MemorySegment) COLUMN_BLOB.invokeExact(stmt, i);
    }

    private static final MethodHandle COLUMN_TYPE = fn("sqlite3_column_type", INT, PTR, INT);
    public static int columnType(MemorySegment stmt, int i) throws Throwable {
        return (int) COLUMN_TYPE.invokeExact(stmt, i);
    }
}
