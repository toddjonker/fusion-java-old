// Copyright (c) 2012-2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.OffsetSpan;
import com.amazon.ion.TextSpan;
import com.amazon.ion.util.Spans;
import java.io.IOException;
import java.util.Objects;


/**
 * A specific location within some Fusion source code.
 * <p>
 * Because Fusion souce code is Ion data, these locations have semantics
 * aligned with {@link com.amazon.ion.TextSpan} and
 * {@link com.amazon.ion.OffsetSpan}.
 */
public class SourceLocation
{
    /** May be null. */
    private final SourceName myName;


    /**
     * @param name may be null
     */
    private SourceLocation(SourceName name)
    {
        myName = name;
    }


    /**
     * Gets the name of the source of this location.
     * @return null if the source name isn't known.
     */
    public SourceName getSourceName()
    {
        return myName;
    }

    /**
     * Gets the one-based line number.
     * @return zero if the line is unknown.
     */
    public long getLine()
    {
        return 0;
    }

    /**
     * Gets the one-based column number.
     * @return zero if the column is unknown.
     */
    public long getColumn()
    {
        return 0;
    }


    // TODO Define what this offset is counting.
    /**
     * Gets the zero-based starting offset.
     * @return -1 if the offset is unknown.
     */
    public long getStartOffset()
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
        public long getLine()
        {
            return myLine;
        }

        @Override
        public long getColumn()
        {
            return myColumn;
        }

        @Override
        public long getStartOffset()
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
        public long getLine()
        {
            return myLine;
        }

        @Override
        public long getColumn()
        {
            return myColumn;
        }

        @Override
        public long getStartOffset()
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
        public long getLine()
        {
            return myLine;
        }

        @Override
        public long getColumn()
        {
            return myColumn;
        }

        @Override
        public long getStartOffset()
        {
            return myStartOffset;
        }
    }


    /**
     * @param name may be null.
     * @param line one-based. -1 indicate that the line is unknown.
     * @param column one-based. -1 indicates that the column is unknown.
     *
     * @return null if no information is known.
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
     * @param line one-based. -1 indicate that the line is unknown.
     * @param column one-based. -1 indicates that the column is unknown.
     *
     * @return null if no information is known.
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


    /**
     * Displays this location in a human-readable form, in terms of line,
     * column, and source name.
     *
     * @param out the stream to write
     */
    public void display(Appendable out)
        throws IOException
    {
        long line   = getLine();
        long column = getColumn();

        if (line < 1)
        {
            out.append("unknown location in ");
            out.append(myName.display());        // FIXME bad output if no name
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

    /**
     * Displays this location in a human-readable form, in terms of line,
     * column, and source name.
     */
    public String display()
    {
        StringBuilder out = new StringBuilder();
        try
        {
            display(out);
        }
        catch (IOException e) { /* shouldn't happen */ }
        return out.toString();
    }

    /**
     * For displaying messages to users, use {@link #display()} instead.
     */
    @Override
    public String toString()
    {
        return display();
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
