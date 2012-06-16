// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;
import java.io.IOException;
import java.io.StringWriter;

/**
 * The core features of a Fusion run-time value.  Note that the set of Fusion
 * values is a superset of the Ion values, so not all {@link FusionValue}s are
 * Ion data values.
 */
public abstract class FusionValue
{
    private static final class Undef
        extends FusionValue
    {
        @Override
        void write(Appendable out) throws IOException
        {
            out.append("/* undef */");
        }
    }


    /** A zero-length array. */
    public static final FusionValue[] EMPTY_ARRAY = new FusionValue[0];

    /** The singular {@code undef} value. */
    public final static FusionValue UNDEF = new Undef();


    //========================================================================

    /** Not for application use. */
    FusionValue()
    {
    }


    /**
     * Determines whether this value falls within the Ion type system.
     *
     * @return true if this Fusion value is also an Ion value.
     */
    public boolean isIon()
    {
        return false;
    }

    /**
     * Returns the DOM representation of this value, if its type falls within
     * the Ion type system. The {@link IonValue} will use the given factory
     * and will not have a container.
     *
     * @param factory must not be null.
     *
     * @return null if this value's type isn't an Ion type (for example,
     * Fusion procedures).
     */
    public IonValue ionValue(ValueFactory factory)
    {
        return null;
    }

    /**
     * Returns the DOM representation of this value, if its type falls within
     * the Ion type system. The result may have a container!
     * <p>
     * This isn't public because I'm not convinced that the runtime should have
     * a singular IonSystem or ValueFactory.  Different subsystems may have
     * different needs, some using a lazy dom others with full materialization.
     *
     * @return null if this value's type isn't an Ion type.
     */
    IonValue ionValue()
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
     * The result will be invalid if the value contains any non-Ion data like
     * closures.
     *
     * @param out the output stream; not null.
     *
     * @throws IOException Propagated from the output stream.
     */
    abstract void write(Appendable out)
        throws IOException;


    /**
     * Returns the output of {@link #write(Appendable)} as a {@link String}.
     *
     * @return not null.
     */
    String write()
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
     * Returns a representation of this value for debugging and diagnostics.
     * Currently, it behaves like {@link #write()} but the behavior may change
     * at any time.
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
    final String display()
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


    //========================================================================
    // Static write methods


    private static void dispatchWrite(Appendable out, Object value)
        throws IOException
    {
        if (value instanceof FusionValue)
        {
            ((FusionValue) value).write(out);
        }
        else
        {
            out.append("/* ");
            out.append(value.toString());
            out.append(" */");
        }
    }


    /**
     * Writes a representation of this value, following Ion syntax where
     * possible, including for strings.
     * The result will be invalid if the value contains any non-Ion data like
     * closures.
     *
     * @param out the output stream; not null.
     *
     * @throws FusionException if there's an exception thrown by the output
     * stream.
     */
    public static void write(Appendable out, Object value)
        throws FusionException
    {
        try
        {
            dispatchWrite(out, value);
        }
        catch (IOException e)
        {
            throw new FusionException("I/O exception", e);
        }
    }


    /**
     * Writes a representation of this value, following Ion syntax where
     * possible, including for strings.
     * The result will be invalid if the value contains any non-Ion data like
     * closures.
     *
     * @param out the output buffer; not null.
     */
    public static void write(StringBuilder out, Object value)
    {
        try
        {
            dispatchWrite(out, value);
        }
        catch (IOException e)
        {
            // This shouldn't happen
            throw new IllegalStateException("I/O exception", e);
        }
    }


    // TODO (write val) that goes to current_output_stream


    /**
     * Returns the output of {@link #write(StringBuilder,Object)} as a
     * {@link String}.
     *
     * @return not null.
     */
    public static String writeToString(Object value)
    {
        StringBuilder out = new StringBuilder();
        write(out, value);
        return out.toString();
    }


    /**
     * {@linkplain #write(Appendable, Object) Writes} several values,
     * injecting a string between each pair of values.
     *
     * @param out must not be null.
     * @param values must not be null.
     * @param join must not be null.
     */
    public static void writeMany(Appendable out, Object[] values, String join)
        throws FusionException
    {
        try
        {
            for (int i = 0; i < values.length; i++)
            {
                if (i != 0)
                {
                    out.append(join);
                }

                FusionValue.write(out, values[i]);
            }
        }
        catch (IOException e)
        {
            throw new FusionException("I/O exception", e);
        }
    }


    /**
     * {@linkplain #write(StringBuilder, Object) Writes} several values,
     * injecting a string between each pair of values.
     *
     * @param out must not be null.
     * @param values must not be null.
     * @param join must not be null.
     */
    public static void writeMany(StringBuilder out, Object[] values, String join)
    {
        for (int i = 0; i < values.length; i++)
        {
            if (i != 0)
            {
                out.append(join);
            }

            FusionValue.write(out, values[i]);
        }
    }


    public static String writeManyToString(Object[] values, String join)
    {
        StringBuilder out = new StringBuilder();
        writeMany(out, values, join);
        return out.toString();
    }


    //========================================================================
    // Static display methods


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
