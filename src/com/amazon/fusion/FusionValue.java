// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;
import java.io.IOException;
import java.io.StringWriter;

/**
 * The core features of a Fusion run-time value.  Note that the set of Fusion
 * values is a superset of the Ion values, so not all {@link FusionValue}s are
 * Ion data values.
 */
public abstract class FusionValue
{
    /** A zero-length array. */
    public static final FusionValue[] EMPTY_ARRAY = new FusionValue[0];


    public final static FusionValue UNDEF = new FusionValue()
    {
        @Override
        public void write(Appendable out) throws IOException
        {
            out.append("/* undef */");
        }

        @Override
        public String write()
        {
            return("/* undef */");
        }
    };


    //========================================================================


    /**
     * Gets an Ion representation of this value, if available.
     * <p>
     * TODO This should be refactored.
     *
     * @return may be null.
     */
    IonValue getDom()
    {
        return null;
    }


    String getInferredName()
    {
        return null;
    }

    void inferName(String name)
    {
    }


    //========================================================================

    /**
     * Invokes the value as the first position in an S-expression.
     *
     * @param eval the evaluation engine to use for sub-expressions; not null.
     * @param env the lexical environment, containing visible bindings;
     *  not null.
     * @param expr the source expression being invoked; not null.
     *  The first child element is the source for this value.
     *
     * @return the invocation result.
     *
     * @throws FusionException if there's a failure in the fusion code.
     */
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        throw new FusionException("not invokable: " + this);
    }


    //========================================================================

    /**
     * Writes a representation of this value, following Ion syntax where
     * possible, including for strings.
     * The result will be if the value contains any non-Ion data like
     * functions.
     *
     * @param out the output stream; not null.
     *
     * @throws IOException Propagated from the output stream.
     */
    public abstract void write(Appendable out)
        throws IOException;


    /**
     * Returns the output of {@link #write(Appendable)} as a {@link String}.
     *
     * @return not null.
     */
    public String write()
    {
        StringBuilder out = new StringBuilder();
        try
        {
            write(out);
        }
        catch (IOException e) {}
        return out.toString();
    }


    /**
     * Behaves like {@link #write()}
     */
    @Override
    public final String toString()
    {
        return write();
    }


    /**
     * Prints a representation of this value for human consumption, generally
     * translating character/string data to it's content without using Ion
     * quotes or escapes. Non-character data is output as per
     * {@link #write(Appendable)}.
     *
     * @param out the output stream; not null.
     *
     * @throws IOException Propagated from the output stream.
     */
    public void display(Appendable out)
        throws IOException
    {
        write(out);
    }


    /**
     * Returns the output of {@link #display(Appendable)} as a {@link String}
     */
    String display()
    {
        StringWriter out = new StringWriter();
        try
        {
            display(out);
        }
        catch (IOException e) {}
        return out.toString();
    }


    /**
     * Displays the documentation of this value.
     * Implementations should try to ensure that a final newline is printed.
     * <p>
     * TODO This API should be refactored to use a Documentation abstraction
     * that can be output in various ways.
     *
     * @param out the output stream; not null.
     *
     * @throws IOException Propagated from the output stream.
     */
    void displayHelp(Appendable out)
        throws IOException
    {
        out.append("No documentation.\n");
    }


    static void write(Appendable out, FusionValue[] values, String join)
        throws IOException
    {
        for (int i = 0; i < values.length; i++)
        {
            if (i != 0)
            {
                out.append(join);
            }

            values[i].write(out);
        }
    }


    static String write(FusionValue[] values, String join)
    {
        StringBuilder out = new StringBuilder();
        try
        {
            write(out, values, join);
        }
        catch (IOException e) {}
        return out.toString();
    }


    static void display(Appendable out, FusionValue[] values, String join)
        throws IOException
    {
        for (int i = 0; i < values.length; i++)
        {
            if (i != 0)
            {
                out.append(join);
            }

            values[i].display(out);
        }
    }


    static String display(FusionValue[] values, String join)
    {
        StringBuilder out = new StringBuilder();
        try
        {
            display(out, values, join);
        }
        catch (IOException e) {}
        return out.toString();
    }
}
