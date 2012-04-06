// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

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
        void display(Writer out) throws IOException
        {
            out.write("/* undef */");
        }
    };


    boolean isTruthy()
    {
        return false;
    }


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


    /**
     * Prints a representation of this value for human consumption, generally
     * for use by "print" phase of the {@linkplain Repl Read-Eval-Print Loop}.
     *
     * @param out the output stream; not null.
     *
     * @throws IOException Propagated from the output stream.
     */
    abstract void display(Writer out)
        throws IOException;


    /**
     * Prints the documentation of this value.
     * Implementations should try to ensure that a final newline is printed.
     * <p>
     * TODO This API should be refactored to use a Documentation abstraction
     * that can be output in various ways.
     *
     * @param out the output stream; not null.
     *
     * @throws IOException Propagated from the output stream.
     */
    void printHelp(Writer out)
        throws IOException
    {
        out.write("// No documentation.\n");
    }


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
        throw new FusionException("not invokable: " + displayToString(this));
    }


    static String displayToString(FusionValue value)
    {
        StringWriter buf = new StringWriter();
        try
        {
            value.display(buf);
        }
        catch (IOException e)
        {
            // ignore it
        }
        return buf.toString();
    }

    static String displayToString(FusionValue[] values, String join)
    {
        StringBuilder buf = new StringBuilder();
        for (int i = 0; i < values.length; i++)
        {
            if (i != 0)
            {
                buf.append(join);
            }
            buf.append(displayToString(values[i]));
        }
        return buf.toString();
    }
}
