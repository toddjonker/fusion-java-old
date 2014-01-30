// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.system.IonTextWriterBuilder;
import java.io.File;
import java.io.IOException;
import java.io.Reader;

/**
 *
 */
final class FusionUtils
{
    /** This class is not to be instantiated. */
    private FusionUtils() { }


    static final byte[]   EMPTY_BYTE_ARRAY   = new byte[0];
    static final Object[] EMPTY_OBJECT_ARRAY = new Object[0];
    static final String[] EMPTY_STRING_ARRAY = new String[0];


    static boolean safeEquals(Object left, Object right)
    {
        return (left == null ? right == null : left.equals(right));
    }


    /**
     * @param value must not be null.
     */
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
     * @param out must not be null.
     * @param i the zero-based index to display.
     *
     * @throws IOException if thrown by {@code out}.
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
     * @param out must not be null.
     * @param i the zero-based index to display.
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


    /**
     * Resolve a relative path against the {@code currentDirectory} param.
     * If file is absolute it is returned as-is.
     */
    static File resolvePath(Evaluator eval,
                            DynamicParameter currentDirectory,
                            File file)
        throws FusionException
    {
        if (! file.isAbsolute())
        {
            String cdPath = currentDirectory.asString(eval);
            File cdFile = new File(cdPath);
            file = new File(cdFile, file.getPath());
        }
        return file;
    }

    /**
     * Resolve a relative path against the {@code current_directory} param.
     * If the path is absolute it is returned as-is.
     */
    static File resolvePath(Evaluator eval,
                            DynamicParameter currentDirectory,
                            String path)
        throws FusionException
    {
        File file = new File(path);
        return resolvePath(eval, currentDirectory, file);
    }


    public static String loadReader(Reader in)
        throws IOException
    {
        StringBuilder buf = new StringBuilder(2048);

        char[] chars = new char[2048];

        int len;
        while ((len = in.read(chars)) != -1)
        {
            buf.append(chars, 0, len);
        }

        return buf.toString();
    }
}
