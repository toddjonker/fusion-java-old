// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.TextSpan;
import com.amazon.ion.util.Spans;

final class SourceLocation
{
    final SourceName myName;
    /** Zero-based */
    final long myLine;
    /** Zero-based */
    final long myColumn;


    /**
     * Returns an instance that represents the current span of the reader.
     * This currently only supports Ion text sources, and only captures the
     * start position.
     */
    static SourceLocation currentLocation(IonReader source, SourceName name)
    {
        TextSpan ts = Spans.currentSpan(TextSpan.class, source);
        if (ts != null)
        {
            // Convert from one-based to zero-based.
            return new SourceLocation(name,
                                      ts.getStartLine() - 1,
                                      ts.getStartColumn() - 1);
        }
        return null;
    }


    /**
     * @param line zero-based
     * @param column zero-based
     */
    private SourceLocation(SourceName name, long line, long column)
    {
        myName = name;
        myLine = line;
        myColumn = column;
    }


    void display(StringBuilder out)
    {
        FusionUtils.writeFriendlyIndex(out, myLine);
        out.append(" line, ");
        FusionUtils.writeFriendlyIndex(out, myColumn);
        out.append(" column");

        if (myName != null)
        {
            out.append(" of ");
            out.append(myName.display());
        }
    }
}
