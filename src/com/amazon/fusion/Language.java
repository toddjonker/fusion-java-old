// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import com.amazon.ion.system.IonSystemBuilder;
import java.io.IOException;
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
    public static class ExitException
        extends RuntimeException
    {
        private static final long serialVersionUID = 1L;
    }


    private final IonSystem mySystem = IonSystemBuilder.standard().build();
    private final Environment myEnvironment = new CoreEnvironment(mySystem);
    private final Evaluator myEvaluator = new Evaluator();


    public FusionValue eval(String source)
        throws ExitException
    {
        Iterator<IonValue> i = mySystem.iterate(source);
        return eval(i);
    }

    public FusionValue eval(Iterator<IonValue> source)
        throws ExitException
    {
        FusionValue result = null;

        while (source.hasNext())
        {
            IonValue fileExpr = source.next();
            result = myEvaluator.eval(myEnvironment, fileExpr);
        }

        return result;
    }

    public void write(FusionValue v, Writer out)
        throws IOException
    {
        if (v != null)
        {
            v.print(out);
            out.write('\n');
        }
        else
        {
            out.write("// No value\n");
        }
    }
}
