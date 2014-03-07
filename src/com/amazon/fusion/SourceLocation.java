// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.TextSpan;
import com.amazon.ion.util.Spans;
import java.io.IOException;


class SourceLocation
{
    /** May be null. */
    final SourceName myName;


    /**
     * @param name may be null
     */
    private SourceLocation(SourceName name)
    {
        myName = name;
    }


    /**
     * Gets the zero-based line number.
     * @return -1 if the line is unknown.
     */
    long getLine()
    {
        return -1;
    }

    /**
     * Gets the zero-based column number.
     * @return -1 if the column is unknown.
     */
    long getColumn()
    {
        return -1;
    }


    private static final class Shorts
        extends SourceLocation
    {
        private final short myLine;
        private final short myColumn;

        private Shorts(SourceName name, short line, short column)
        {
            super(name);
            myLine = line;
            myColumn = column;
        }

        @Override
        long getLine()
        {
            return myLine;
        }

        @Override
        long getColumn()
        {
            return myColumn;
        }
    }


    private static final class Ints
        extends SourceLocation
    {
        private final int myLine;
        private final int myColumn;

        private Ints(SourceName name, int line, int column)
        {
            super(name);
            myLine = line;
            myColumn = column;
        }

        @Override
        long getLine()
        {
            return myLine;
        }

        @Override
        long getColumn()
        {
            return myColumn;
        }
    }


    private static final class Longs
        extends SourceLocation
    {
        private final long myLine;
        private final long myColumn;

        private Longs(SourceName name, long line, long column)
        {
            super(name);
            myLine = line;
            myColumn = column;
        }

        @Override
        long getLine()
        {
            return myLine;
        }

        @Override
        long getColumn()
        {
            return myColumn;
        }
    }


    /**
     * Returns an instance that represents the current span of the reader.
     * This currently only supports Ion text sources, and only captures the
     * start position.
     *
     * @param source may be null.
     * @param name may be null.
     *
     * @return null if no location could be determined.
     */
    static SourceLocation forCurrentSpan(IonReader source, SourceName name)
    {
        TextSpan ts = Spans.currentSpan(TextSpan.class, source);
        if (ts != null)
        {
            // Convert from one-based to zero-based.
            long line   = ts.getStartLine  () - 1;
            long column = ts.getStartColumn() - 1;

            if (line <= Short.MAX_VALUE && column <= Short.MAX_VALUE)
            {
                return new Shorts(name, (short) line, (short) column);
            }

            if (line <= Integer.MAX_VALUE && column <= Integer.MAX_VALUE)
            {
                return new Ints(name, (int) line, (int) column);
            }

            return new Longs(name, line, column);
        }

        if (name != null)
        {
            // TODO Can this allocation be eliminated?
            //      We'll probably be creating lots of identical instances.
            return new SourceLocation(name);
        }

        return null;
    }


    private void displayOrdinal(Appendable out, long ord)
        throws IOException
    {
        if (ord < 0)
        {
            out.append("???");
        }
        else
        {
            FusionUtils.writeFriendlyIndex(out, ord);
        }
    }


    void display(Appendable out)
        throws IOException
    {
        long line   = getLine();
        long column = getColumn();

        if (line < 0)
        {
            out.append("unknown location in ");
            out.append(myName.display());
        }
        else
        {
            displayOrdinal(out, line);
            out.append(" line, ");
            displayOrdinal(out, column);
            out.append(" column");

            if (myName != null)
            {
                out.append(" of ");
                out.append(myName.display());
            }
        }
    }

    @Override
    public String toString()
    {
        StringBuilder out = new StringBuilder();
        try
        {
            display(out);
        }
        catch (IOException e) { /* shouldn't happen */ }
        return out.toString();
    }
}
