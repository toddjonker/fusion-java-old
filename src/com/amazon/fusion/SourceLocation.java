// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.TextSpan;
import com.amazon.ion.util.Spans;

final class SourceLocation
{
    /** Zero-based */
    final long myLine;
    /** Zero-based */
    final long myColumn;


    /**
     * Returns an instance that represents the current span of the reader.
     * This currently only supports Ion text sources, and only captures the
     * start position.
     */
    static SourceLocation currentLocation(IonReader source)
    {
        TextSpan ts = Spans.currentSpan(TextSpan.class, source);
        if (ts != null)
        {
            // Convert from one-based to zero-based.
            return new SourceLocation(ts.getStartLine() - 1,
                                      ts.getStartColumn() - 1);
        }
        return null;
    }


    /**
     * @param line zero-based
     * @param column zero-based
     */
    SourceLocation(long line, long column)
    {
        myLine = line;
        myColumn = column;
    }
}
