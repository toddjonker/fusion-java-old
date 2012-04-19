// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
import java.io.Writer;

/**
 * Base class for invocable functions, both built-in and user-defined.
 * This implements the evaluation of arguments and prevents the function from
 * access to the caller's environment.
 */
abstract class FunctionValue
    extends NamedValue
{
    final static String DOTDOTDOT = "...";

    final String[] myParams;
    private final String myDoc;

    FunctionValue(String doc, String... params)
    {
        assert doc == null || ! doc.endsWith("\n");
        assert params != null;
        myParams = params;
        myDoc = doc;
    }


    @Override
    final FusionValue invoke(Evaluator eval, final Environment env, IonSexp expr)
        throws FusionException
    {
        int argCount = expr.size() - 1;

        FusionValue[] args;
        if (argCount == 0)
        {
            args = FusionValue.EMPTY_ARRAY;
        }
        else
        {
            args = new FusionValue[argCount];
            for (int i = 0; i < argCount; i++)
            {
                IonValue argumentExpr = expr.get(i + 1);
                FusionValue argumentValue = eval.eval(env, argumentExpr);
                args[i] = argumentValue;
            }
        }

        return invoke(eval, args);
    }

    @Override
    final void display(Writer out)
        throws IOException
    {
        out.write("/* function");
        String name = getInferredName();
        if (name != null)
        {
            out.write(' ');
            try
            {
                IonTextUtils.printQuotedSymbol(out, name);
            }
            catch (IOException e)
            {
                throw new IllegalStateException("Shouldn't happen", e);
            }
        }
        out.write(" */\n");
    }

    @Override
    final void printHelp(Writer out)
        throws IOException
    {
        out.write("[FUNCTION]  (");
        out.write(getEffectiveName());
        for (String formal : myParams)
        {
            out.write(' ');
            out.write(formal);
        }
        out.write(")\n");

        if (myDoc != null)
        {
            out.write('\n');
            out.write(myDoc);
            out.write('\n');
        }
    }

    /**
     * @param args must not be null, and none of its elements may be null.
     * @return null is a synonym for {@link #UNDEF}.
     */
    abstract FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException;
}
