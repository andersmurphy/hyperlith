package hyperlith.impl;

import java.io.FilterWriter;
import java.io.Writer;
import java.io.IOException;

public class SSENewlineFilterWriter extends FilterWriter {
    private static final char[] prefix = "\ndata: elements ".toCharArray();
    private static final int prefixLength= prefix.length;

    public SSENewlineFilterWriter(Writer out) {
        super(out);
    }

    @Override
    public void write(char[] cbuf, int off, int len) throws IOException {
        int end = off + len;
        int runStart = off;

        for (int i = off; i < end; i++) {
            if (cbuf[i] == '\n') {
                out.write(cbuf, runStart, i - runStart);
                out.write(prefix, 0, prefixLength);
                runStart = i + 1;
            }
        }
        out.write(cbuf, runStart, end - runStart);
    }
}
