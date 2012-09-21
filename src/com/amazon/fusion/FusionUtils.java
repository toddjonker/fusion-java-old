// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.system.IonTextWriterBuilder;
import java.io.IOException;

/**
 *
 */
final class FusionUtils
{
    /** This class is not to be instantiated. */
    private FusionUtils() { }


    static final Object[] EMPTY_OBJECT_ARRAY = new Object[0];
    static final String[] EMPTY_STRING_ARRAY = new String[0];


    static IonValue cloneIfContained(IonValue value)
    {
        if (value.getContainer() != null)
        {
            value = value.clone();
        }
        return value;
    }


    static String friendlySuffix(long i)
    {
        long lastDigit = i % 10;
        if (lastDigit == 1 && i != 11)
        {
            return "st";
        }
        if (lastDigit == 2 && i != 12)
        {
            return "nd";
        }
        if (lastDigit == 3 && i != 13)
        {
            return "rd";
        }
        return "th";
    }


    /**
     * Writes a zero-based index as a one-based position like
     * "1st", "12th, or "23rd".
     *
     * @param out
     * @param i
     * @throws IOException
     */
    static void writeFriendlyIndex(Appendable out, long i)
        throws IOException
    {
        i++;
        out.append(Long.toString(i));
        String suffix = friendlySuffix(i);
        out.append(suffix);
    }


    /**
     * Writes a zero-based index as a one-based position like
     * "1st", "12th, or "23rd".
     *
     * @param out
     * @param i
     * @throws IOException
     */
    static void writeFriendlyIndex(StringBuilder out, long i)
    {
        i++;
        out.append(Long.toString(i));
        String suffix = friendlySuffix(i);
        out.append(suffix);
    }

    static void writeIon(Appendable out, IonValue v)
        throws IOException
    {
        IonWriter writer = IonTextWriterBuilder.standard().build(out);
        v.writeTo(writer);
        writer.flush();
    }
}
