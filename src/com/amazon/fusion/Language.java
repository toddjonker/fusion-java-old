// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.UNDEF;
import com.amazon.ion.IonException;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import com.amazon.ion.system.IonSystemBuilder;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Iterator;

/**
 *
 */
public final class Language
{
    /**
     * Thown to force the exit of an evaluation.
     */
    public static final class ExitException
        extends RuntimeException
    {
        private static final long serialVersionUID = 1L;

        ExitException() { }
    }


    private final IonSystem mySystem = IonSystemBuilder.standard().build();
    private final Environment myEnvironment = new CoreEnvironment(mySystem);
    private final Evaluator myEvaluator = new Evaluator(mySystem);


    /**
     * @param source must not be null.
     * @return not null, but perhaps {@link FusionValue#UNDEF}.
     */
    public FusionValue eval(String source)
        throws ExitException
    {
        Iterator<IonValue> i = mySystem.iterate(source);
        return eval(i);
    }


    /**
     * @param source must not be null.
     * @return not null, but perhaps {@link FusionValue#UNDEF}.
     *
     * @throws ExitException
     */
    public FusionValue eval(Iterator<IonValue> source)
        throws ExitException
    {
        FusionValue result = UNDEF;

        while (source.hasNext())
        {
            IonValue fileExpr = source.next();
            result = myEvaluator.eval(myEnvironment, fileExpr);
        }

        return result;
    }


    /**
     * @param v must not be null.
     * @param out
     */
    public void display(FusionValue v, Writer out)
        throws IOException
    {
        if (v == null) v = UNDEF;
        v.display(out);
        out.write('\n');
    }

    /**
     * @param v must not be null
     */
    public void displayToStdout(FusionValue v)
    {
        try
        {
            OutputStreamWriter out = new OutputStreamWriter(System.out);
            try
            {
                display(v, out);
            }
            finally
            {
                out.flush();
            }
        }
        catch (IOException e)
        {
            throw new IonException(e);
        }
    }
}
