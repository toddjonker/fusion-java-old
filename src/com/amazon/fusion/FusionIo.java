// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVoid.voidValue;
import com.amazon.ion.IonException;
import com.amazon.ion.IonText;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.system.IonTextWriterBuilder;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

/**
 * Utilities for input and output of Fusion data.
 */
public final class FusionIo
{
    private FusionIo() {}


    /** The singular {@code eof} value. */
    private final static FusionValue EOF =
        new FusionValue()
        {
            @Override
            void write(Evaluator eval, Appendable out) throws IOException
            {
                out.append("{{{eof}}}");
            }
        };


    static Object eof(Evaluator eval)
    {
        return EOF;
    }


    static boolean isEof(Evaluator eval, Object v)
    {
        assert eval != null;
        return (v == EOF);
    }


    static final class IsEofProc
        extends Procedure1
    {
        IsEofProc()
        {
            //    "                                                                               |
            super("Determines whether a `value` is the Fusion end-of-file value.  This value is\n"
                + "bound to the name `eof`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
        {
            return eval.newBool(arg == EOF);
        }
    }


    //========================================================================
    // "Internal" dispatching procedures that throw exceptions


    static void dispatchIonize(Evaluator eval, IonWriter out, Object value)
        throws IOException, FusionException, IonException
    {
        if (value instanceof FusionValue)
        {
            ((FusionValue) value).ionize(eval, out);
        }
        else if (value instanceof IonValue)
        {
            ((IonValue)value).writeTo(out);
        }
        else
        {
            throw new IonizeFailure(value);
        }
    }


    /**
     *
     * @param eval may be null!
     * @param value may be an {@link IonValue}.
     */
    static void dispatchWrite(Evaluator eval, Appendable out, Object value)
        throws IOException, FusionException
    {
        if (value instanceof FusionValue)
        {
            ((FusionValue) value).write(eval, out);
        }
        else if (value instanceof IonValue)
        {
            FusionUtils.writeIon(out, (IonValue) value);
        }
        else
        {
            out.append(value.getClass().getName());
            out.append("::{{{");
            out.append(value.toString());
            out.append("}}}");
        }
    }


    static void dispatchDisplay(Evaluator eval, Appendable out, Object value)
        throws IOException, FusionException
    {
        if (value instanceof FusionValue)
        {
            ((FusionValue) value).display(eval, out);
        }
        else if (value instanceof IonValue)
        {
            IonValue iv = (IonValue) value;
            if (iv instanceof IonText)
            {
                String text = ((IonText) iv).stringValue();
                out.append(text);
            }
            else
            {
                FusionUtils.writeIon(out, iv);
            }
        }
        else
        {
            out.append(value.getClass().getName());
            out.append("::{{{");
            out.append(value.toString());
            out.append("}}}");
        }
    }


    //========================================================================
    // Basic output procedures


    /**
     * Writes an Ion representation of a value.
     * An exception is thrown if the value contains any non-Ion data
     * like closures.
     *
     * @param eval may be null, in which case output may fall back to default
     * format of some kind.
     * @param out the output stream; not null.
     * @param value must not be null.
     *
     * @throws IonizeFailure if some part of the value cannot be ionized.
     * @throws FusionException if there's an exception thrown by the output
     * stream.
     */
    static void ionize(Evaluator eval, IonWriter out, Object value)
        throws FusionException
    {
        try
        {
            dispatchIonize(eval, out, value);
        }
        catch (IOException e)
        {
            throw new FusionException("I/O exception", e);
        }
        catch (IonException e)
        {
            throw new FusionException(e);
        }
    }


    /**
     * <a href="{@docRoot}/../fusion/io.html#write">Writes</a> a text representation of a Fusion value, following Ion syntax
     * where possible, including for strings.
     * The result will be unreadable (by the Fusion and Ion readers) if the
     * value contains any non-Ionizable data (void, closures, etc.).
     *
     * @param top must not be null.
     * @param value must not be null.
     * @param out the output stream; not null.
     * @throws FusionException if there's an exception thrown by the output
     * stream.
     */
    public static void write(TopLevel top, Object value, Appendable out)
        throws FusionException
    {
        write(((StandardTopLevel) top).getEvaluator(), out, value);
    }


    /**
     * Writes a text representation of a value, following Ion syntax where
     * possible, including for strings.
     * The result will be unreadable (by the Fusion and Ion readers) if the
     * value contains any non-Ionizable data like closures.
     *
     * @param eval may be null, in which case output may fall back to default
     * format of some kind.
     * @param out the output stream; not null.
     * @param value must not be null.
     *
     * @throws FusionException if there's an exception thrown by the output
     * stream.
     */
    static void write(Evaluator eval, Appendable out, Object value)
        throws FusionException
    {
        try
        {
            dispatchWrite(eval, out, value);
        }
        catch (IOException e)
        {
            throw new FusionException("I/O exception", e);
        }
    }


    static void display(Evaluator eval, Appendable out, Object value)
        throws FusionException
    {
        try
        {
            dispatchDisplay(eval, out, value);
        }
        catch (IOException e)
        {
            throw new FusionException("I/O exception", e);
        }
    }


    //========================================================================

    /**
     * Returns the output of {@link #write} as a
     * {@link String}.
     *
     * @return not null.
     */
    static String writeToString(Evaluator eval, Object value)
        throws FusionException
    {
        StringBuilder out = new StringBuilder();
        write(eval, out, value);
        return out.toString();
    }


    //========================================================================

    /**
     * {@linkplain #write Writes}
     * several values, injecting a string between each pair of values.
     *
     * @param out must not be null.
     * @param values must not be null.
     * @param join must not be null.
     */
    static void writeMany(Evaluator eval, Appendable out,
                          Object[] values, String join)
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

                dispatchWrite(eval, out, values[i]);
            }
        }
        catch (IOException e)
        {
            throw new FusionException("I/O exception", e);
        }
    }


    /**
     * {@linkplain #display Displays}
     * several values, injecting a string between each pair of values.
     *
     * @param out must not be null.
     * @param values must not be null.
     * @param join must not be null.
     */
    static void displayMany(Evaluator eval, Appendable out,
                            Object[] values, String join)
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

                dispatchDisplay(eval, out, values[i]);
            }
        }
        catch (IOException e)
        {
            throw new FusionException("I/O exception", e);
        }
    }

    static void displayMany(Evaluator eval,
                            Appendable out,
                            Object[] values,
                            int first)
        throws FusionException
    {
        for (int i = first; i < values.length; i++)
        {
            display(eval, out, values[i]);
        }
    }


    //========================================================================

    /**
     * Returns the output of {@link #writeMany} as a {@link String}.
     *
     * @return not null.
     */
    static String writeManyToString(Evaluator eval, Object[] values,
                                    String join)
        throws FusionException
    {
        StringBuilder out = new StringBuilder();
        writeMany(eval, out, values, join);
        return out.toString();
    }


    /**
     * Returns the output of {@link #displayMany} as a {@link String}.
     *
     * @return not null.
     */
    static String displayManyToString(Evaluator eval, Object[] values,
                                      String join)
        throws FusionException
    {
        StringBuilder out = new StringBuilder();
        displayMany(eval, out, values, join);
        return out.toString();
    }


    static String displayManyToString(Evaluator eval, Object[] values,
                                      int first)
        throws FusionException
    {
        StringBuilder out = new StringBuilder();
        displayMany(eval, out, values, first);
        return out.toString();
    }


    //========================================================================
    // "Safe" (no-throw) variants


    static void safeWrite(Evaluator eval, Appendable out, Object value)
    {
        try
        {
            write(eval, out, value);
        }
        catch (FusionException e)
        {
            displayFailure(out, e);
        }
    }


    static void displayFailure(Appendable out, FusionException e)
    {
        try
        {
            out.append("{{{ FAILURE: ");
            out.append(e.getMessage());
            out.append(" }}}");
        }
        catch (IOException ioe)
        {
            // Give up!
        }
    }


    static void safeDisplay(Evaluator eval, Appendable out, Object value)
    {
        try
        {
            display(eval, out, value);
        }
        catch (FusionException e)
        {
            displayFailure(out, e);
        }
    }


    /**
     * Returns the output of {@link #write} as a {@link String}, handling any
     * exceptions by writing their message into the output.
     *
     * @return not null.
     */
    static String safeWriteToString(Evaluator eval, Object value)
    {
        StringBuilder out = new StringBuilder();
        safeWrite(eval, out, value);
        return out.toString();
    }


    static String safeDisplayManyToString(Evaluator eval, Object[] values,
                                          int first)
    {
        StringBuilder out = new StringBuilder();
        try
        {
            displayMany(eval, out, values, first);
        }
        catch (FusionException e)
        {
            displayFailure(out, e);
        }
        return out.toString();
    }


    //========================================================================


    final static class IonizeProc
        extends Procedure
    {
        IonizeProc()
        {
            //    "                                                                               |
            super("Outputs an Ion text representation of `value`, throwing an exception if the\n" +
                  "value contains any non-Ionizable data like closures.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            IonTextWriterBuilder b = IonTextWriterBuilder.pretty();
            IonWriter writer = b.build((OutputStream) System.out);

            FusionIo.ionize(eval, writer, args[0]);

            try
            {
                writer.flush();
            }
            catch (IOException e)
            {
                throw new FusionException("I/O Exception", e);
            }

            return voidValue(eval);
        }
    }


    final static class WriteProc
        extends Procedure
    {
        WriteProc()
        {
            //    "                                                                               |
            super("Outputs a text representation of `value`, following Ion syntax where possible.\n" +
                  "The result will be unreadable (by the Fusion and Ion readers) if the value\n" +
                  "contains any non-Ionizable data like closures.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            FusionIo.write(eval, System.out, args[0]);

            return voidValue(eval);
        }
    }


    final static class DisplayProc
        extends Procedure
    {
        DisplayProc()
        {
            //    "                                                                               |
            super("Outputs a text representation of the `value`s, writing character data as-is but\n" +
                  "otherwise following Ion syntax where possible.  In general, the result will be\n" +
                  "unreadable by the Fusion and Ion readers.",
                  "value", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            try
            {
                OutputStreamWriter out = new OutputStreamWriter(System.out);
                try
                {
                    for (Object arg : args)
                    {
                        FusionIo.display(eval, out, arg);
                    }
                }
                finally
                {
                    out.flush();
                }
            }
            catch (IOException e)
            {
                throw new FusionException(e);
            }

            return voidValue(eval);
        }
    }
}

