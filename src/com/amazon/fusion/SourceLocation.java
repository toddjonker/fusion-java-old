// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.OffsetSpan;
import com.amazon.ion.TextSpan;
import com.amazon.ion.util.Spans;
import java.io.IOException;
import java.util.Objects;


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
     * @return null if the source name isn't known.
     */
    SourceName getSourceName()
    {
        return myName;
    }

    /**
     * Gets the one-based line number.
     * @return zero if the line is unknown.
     */
    long getLine()
    {
        return 0;
    }

    /**
     * Gets the one-based column number.
     * @return zero if the column is unknown.
     */
    long getColumn()
    {
        return 0;
    }


    /**
     * Gets the zero-based starting offset.
     * @return -1 if the offset is unknown.
     */
    long getStartOffset()
    {
        return -1;
    }


    private static final class Shorts
        extends SourceLocation
    {
        private final short myLine;
        private final short myColumn;
        private final short myStartOffset;

        private Shorts(SourceName name, short line, short column,
                       short startOffset)
        {
            super(name);
            myLine = line;
            myColumn = column;
            myStartOffset = startOffset;
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

        @Override
        long getStartOffset()
        {
            return myStartOffset;
        }
    }


    private static final class Ints
        extends SourceLocation
    {
        private final int myLine;
        private final int myColumn;
        private final int myStartOffset;

        private Ints(SourceName name, int line, int column, int startOffset)
        {
            super(name);
            myLine = line;
            myColumn = column;
            myStartOffset = startOffset;
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

        @Override
        long getStartOffset()
        {
            return myStartOffset;
        }
    }


    private static final class Longs
        extends SourceLocation
    {
        private final long myLine;
        private final long myColumn;
        private final long myStartOffset;

        private Longs(SourceName name, long line, long column, long startOffset)
        {
            super(name);
            myLine = line;
            myColumn = column;
            myStartOffset = startOffset;
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

        @Override
        long getStartOffset()
        {
            return myStartOffset;
        }
    }


    /**
     * @param line one-based
     * @param column one-based
     */
    static SourceLocation forLineColumn(SourceName name, long line, long column)
    {
        if (line < 1 && column < 1)
        {
            if (name == null) return null;

            // TODO Can this allocation be eliminated?
            //      We'll probably be creating lots of identical instances.
            return new SourceLocation(name);
        }

        if (line <= Short.MAX_VALUE && column <= Short.MAX_VALUE)
        {
            return new Shorts(name, (short) line, (short) column, (short) -1);
        }

        if (line <= Integer.MAX_VALUE && column <= Integer.MAX_VALUE)
        {
            return new Ints(name, (int) line, (int) column, -1);
        }

        return new Longs(name, line, column, -1);
    }


    /**
     * @param line one-based
     * @param column one-based
     */
    static SourceLocation forLineColumn(long line, long column)
    {
        return forLineColumn(null, line, column);
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
        TextSpan ts   = Spans.currentSpan(TextSpan.class,   source);
        OffsetSpan os = Spans.currentSpan(OffsetSpan.class, source);

        if (ts != null)
        {
            long line   = ts.getStartLine  ();
            long column = ts.getStartColumn();
            long offset = os.getStartOffset();

            if (line   <= Short.MAX_VALUE &&
                column <= Short.MAX_VALUE &&
                offset <= Short.MAX_VALUE)
            {
                return new Shorts(name, (short) line, (short) column,
                                  (short) offset);
            }

            if (line   <= Integer.MAX_VALUE &&
                column <= Integer.MAX_VALUE &&
                offset <= Integer.MAX_VALUE)
            {
                return new Ints(name, (int) line, (int) column, (int) offset);
            }

            return new Longs(name, line, column, offset);
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
        if (ord < 1)
        {
            out.append("???");
        }
        else
        {
            FusionUtils.writeFriendlyOrdinal(out, ord);
        }
    }


    void display(Appendable out)
        throws IOException
    {
        long line   = getLine();
        long column = getColumn();

        if (line < 1)
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


    public boolean equals(SourceLocation that)
    {
        return (this == that
                || (that != null
                    && Objects.equals(this.myName, that.myName)
                    && this.getLine()        == that.getLine()
                    && this.getColumn()      == that.getColumn()
                    && this.getStartOffset() == that.getStartOffset()));
    }

    @Override
    public boolean equals(Object that)
    {
        return that instanceof SourceLocation && equals((SourceLocation) that);
    }


    private static final int HASH_SEED = SourceLocation.class.hashCode();

    @Override
    public int hashCode()
    {
        final int prime = 8191;
        int result = HASH_SEED + Objects.hashCode(myName);
        result ^= (result << 29) ^ (result >> 3);
        result = prime * result + (int) getLine();
        result ^= (result << 29) ^ (result >> 3);
        result = prime * result + (int) getColumn();
        result ^= (result << 29) ^ (result >> 3);
        result = prime * result + (int) getStartOffset();
        result ^= (result << 29) ^ (result >> 3);
        return result;
    }
}
